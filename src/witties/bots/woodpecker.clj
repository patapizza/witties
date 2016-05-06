(ns witties.bots.woodpecker
  (:require [clojure.core.async :refer [<! go]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [chime :refer [chime-at]]
            [plumbing.core :refer [map-vals]]
            [schema.core :as s]
            [taoensso.timbre :refer [debugf infof warn warnf]]
            [witties.request :as req]))

(def threads
  "Example:
   {42 {:reminders [{:reminder \"buy flowers\"
                     :message \"Hey Julien, here's your reminder to buy flowers!\"
                     :at time-ms
                     :cancel f}]
        :contact \"Julien\"}}"
  (atom nil))

(def config-file "woodpecker.clj")
(def Reminder
  {:reminder s/Str
   :message s/Str
   :at s/Int
   (s/optional-key :cancel) (s/pred fn?)})
(def reminder-checker (s/checker Reminder))

;; -----------------------------------------------------------------------------
;; Helpers

(defn- schedule-reminder!
  [fb-page-token thread-id {:keys [at message] :as reminder}]
  (infof "scheduling user=%s reminder=%s" thread-id (pr-str reminder))
  (let [on-finished (fn []
                      (swap! threads update-in [thread-id :reminders]
                             (partial remove #(= reminder (dissoc % :cancel)))))
        f (chime-at [at]
                    (fn [time-ms]
                      ;; TODO reliability
                      (infof "fired user=%s message=%s" thread-id message)
                      (req/fb-message!> fb-page-token thread-id message))
                    {:on-finished on-finished
                     :error-handler (fn [e] (warn e "an error occurred"))})]
    (assoc reminder :cancel f)))

(defn- stop-reminder!
  [{:keys [cancel] :as reminder}]
  (infof "stopping reminder=%s" (pr-str reminder))
  (when cancel (cancel))
  (dissoc reminder :cancel))

(defn- remove-reminder!
  [thread-id about]
  (let [f (fn [r]
            (if (= about (:reminder r))
              (do (stop-reminder! r) nil)
              r))]
    (swap! threads update-in [thread-id :reminders] (comp vec (partial keep f)))))

(def days-of-week
  ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn to-pst
  [dt-or-time-ms]
  (cond-> dt-or-time-ms
    (integer? dt-or-time-ms) c/from-long
    true (t/to-time-zone (t/time-zone-for-id "America/Los_Angeles"))))

(defn pretty-time
  [time-ms]
  {:pre [(number? time-ms)]}
  (let [now (to-pst (t/now))
        in-one-week (t/plus now (t/weeks 1))
        dt (to-pst time-ms)
        [month day hour min] ((juxt t/month t/day t/hour t/minute) dt)]
    (str "on "
         (if (t/before? dt in-one-week)
           (->> dt t/day-of-week dec (get days-of-week))
           (format "%s/%s" month day))
         " at " (format "%s:%s"
                        (if (= 0 hour) 12 (mod hour 12))
                        (cond->> min (> 10 min) (str "0")))
         (if (> 12 hour) "am" "pm"))))

(defn pretty-reminders
  [thread-id]
  (if-let [reminders (seq (get-in @threads [thread-id :reminders]))]
    (->> reminders
         (map (fn [{:keys [at reminder]}]
                (format "%s %s" reminder (pretty-time at))))
         (string/join ", "))
    "none"))

;; -----------------------------------------------------------------------------
;; Wit actions

(defn say!>
  [{:keys [fb-page-token]} thread-id context msg]
  (go (infof "Sending message user=%s msg=%s" thread-id msg)
      (<! (req/fb-message!> fb-page-token thread-id msg))))

(defn merge!>
  [params thread-id context entities msg]
  (go (let [time-ms (some-> (get-in entities [:datetime 0 :value]) c/to-long)
            reminder (get-in entities [:reminder 0 :value])
            intent (get-in entities [:intent 0 :value])]
        (cond-> context
          (= "show_reminders" intent) (assoc :reminders (pretty-reminders thread-id))
          reminder (assoc :reminder reminder)
          time-ms (assoc :time-ms time-ms :time (pretty-time time-ms))))))

(defn error!>
  [{:keys [fb-page-token]} thread-id context msg]
  (go (warnf "Sending error user=%s msg=%s" thread-id msg)
      (<! (req/fb-message!> fb-page-token thread-id msg))))

(defn cancel-reminder!>
  [params thread-id {:keys [reminder] :as context}]
  (go (let [exp (-> (get-in @threads [thread-id :reminders]) count dec)
            reminders (-> (remove-reminder! thread-id reminder)
                          (get-in [thread-id :reminders]))]
        (cond-> context
          (= exp (count reminders)) (assoc :ok-cancel true :reminders exp)))))

(defn clear-context!>
  [params thread-id context]
  (go {}))

(defn set-reminder!>
  [{:keys [fb-page-token]} thread-id {:keys [reminder time-ms] :as context}]
  (go (let [message (format "Here's your reminder to %s!" reminder)
            reminder {:reminder reminder
                      :at time-ms
                      :message message}]
        (if-let [err (reminder-checker reminder)]
          (do (warnf "malformed reminder reminder=%s err=%s" reminder err)
              context)
          (let [reminder (schedule-reminder! fb-page-token thread-id reminder)]
            (swap! threads update-in [thread-id :reminders] conj reminder)
            (assoc context :ok-reminder true))))))

;; -----------------------------------------------------------------------------
;; Core interface

(defn init!
  "Restarts saved reminders, if any."
  [{:keys [fb-page-token]}]
  (when-let [config (some-> config-file io/resource slurp edn/read-string)]
    (debugf "init with config=%s" (pr-str config))
    (->> config
         (map (fn [[thread-id data]]
                [thread-id (update data :reminders (partial mapv (partial schedule-reminder! fb-page-token thread-id)))]))
         (into {})
         (reset! threads))))

(defn stop!>
  "Saves reminders, if any."
  []
  (go (when-let [file (io/resource config-file)]
        (with-open [writer (io/writer file)]
          (let [content (map-vals #(update % :reminders (partial mapv stop-reminder!))
                                  @threads)]
            (debugf "Saving file=%s content=%s" config-file (pr-str content))
            (pprint content writer))))))
