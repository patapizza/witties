(ns witties.bots.woodpecker
  (:require [clojure.core.async :refer [<! go]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as c]
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

;; -----------------------------------------------------------------------------
;; Wit actions

(defn say!>
  [{:keys [fb-page-token]} thread-id context msg]
  (go (infof "Sending message user=%s msg=%s" thread-id msg)
      (<! (req/fb-message!> fb-page-token thread-id msg))))

(defn merge!>
  [params thread-id context entities msg]
  (go (let [dt (get-in entities [:datetime 0 :value])
            reminder (get-in entities [:reminder 0 :value])]
        (cond-> context
          reminder (assoc :reminder reminder)
          dt (assoc :time-ms (c/to-long dt)
                    :time (str "on " dt))))))

(defn error!>
  [{:keys [fb-page-token]} thread-id context msg]
  (go (warnf "Sending error user=%s msg=%s" thread-id msg)
      (<! (req/fb-message!> fb-page-token thread-id msg))))

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
