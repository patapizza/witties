(ns witties.bots.woodpecker
  (:require [clojure.core.async :refer [<! go]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as c]
            [chime :refer [chime-at]]
            [plumbing.core :refer [map-vals]]
            [schema.core :as s]
            [taoensso.timbre :refer [infof warnf]]
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
  (let [on-finished (fn []
                      (swap! threads update-in [thread-id :reminders]
                             (partial remove #(= reminder (dissoc % :cancel)))))
        f (chime-at [at]
                    (fn [time-ms]
                      ;; TODO reliability
                      (req/fb-message!> fb-page-token thread-id message))
                    {:on-finished on-finished})]
    (assoc reminder :cancel f)))

(defn- stop-reminder!
  [{:keys [cancel] :as reminder}]
  (cancel)
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
  [{:keys [fb-page-token]} thread-id {:keys [reminder time-ms]}]
  (go (let [message (format "Here's your reminder to %s!" reminder)
            reminder {:reminder reminder
                      :at time-ms
                      :message message}]
        (if-let [err (reminder-checker reminder)]
          (warnf "malformed reminder reminder=%s err=%s" reminder err)
          (let [reminder (schedule-reminder! fb-page-token thread-id reminder)]
            (infof "scheduled user=%s at=%s message=%s"
                   thread-id time-ms message)
            (swap! threads update-in [thread-id :reminders] conj reminder))))))

;; -----------------------------------------------------------------------------
;; Core interface

(defn init!
  "Restarts saved reminders, if any."
  [{:keys [fb-page-token]}]
  (when-let [config (some-> config-file io/resource slurp edn/read-string)]
    (->> config
         (map (fn [[thread-id {:keys [reminders] :as data}]]
                (mapv (partial schedule-reminder! fb-page-token thread-id) reminders)))
         (into {})
         (reset! threads))))

(defn stop!>
  "Saves reminders, if any."
  []
  (go (with-open [writer (io/writer config-file)]
        (let [content (map-vals (fn [{:keys [reminders] :as data}]
                                  (mapv stop-reminder! reminders))
                                @threads)]
          (pprint content writer)))))
