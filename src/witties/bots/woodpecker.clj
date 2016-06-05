(ns witties.bots.woodpecker
  (:require [clojure.core.async :refer [<! go]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [chime :refer [chime-at]]
            [plumbing.core :refer [<-]]
            [schema.core :as s]
            [taoensso.timbre :refer [debugf infof warnf]]
            [witties.db :as db]
            [witties.request :as req]))

(def threads
  "Example:
   {\"42\" [{:about \"buy flowers\"
             :message \"Hey Julien, here's your reminder to buy flowers!\"
             :at time-ms
             :cancel f}]}"
  (atom nil))

(def db-ref (atom nil))

(def Reminder
  {:about s/Str
   :message s/Str
   :at s/Int
   (s/optional-key :cancel) (s/pred fn?)
   (s/optional-key :fired) s/Bool})
(def reminder-checker (s/checker Reminder))

(def snooze-ms (* 1000 60 10)) ;; 10 minutes
(def snooze-allowed-ms (* 1000 60 5)) ;; 5 minutes

;; -----------------------------------------------------------------------------
;; Helpers

(s/defn schedule-reminder! :- (s/maybe Reminder)
  "Returns the scheduled reminder when successful."
  [fb-page-token :- s/Str
   thread-id :- s/Str
   {:keys [at message] :as reminder} :- Reminder]
  {:pre [(not (:fired reminder))]
   :post [(or (nil? %) (:cancel %))]}
  (if (t/before? (c/from-long at) (t/now))
    (warnf "trying to schedule reminder in the past: %s" (pr-str reminder))
    (let [g (fn [r]
              (cond-> r
                (= message (:message r)) (-> (dissoc :cancel)
                                             (assoc :fired true))))
          f (chime-at [at]
                      (fn [time-ms]
                        ;; TODO reliability
                        (infof "fired user=%s message=%s" thread-id message)
                        (swap! threads update thread-id (partial map g))
                        (req/fb-message!> fb-page-token thread-id message))
                      {:error-handler #(warnf % "an error occurred")})
          scheduled (assoc reminder :cancel f)]
      (infof "scheduling user=%s reminder=%s" thread-id (pr-str reminder))
      (swap! threads update thread-id (comp (partial sort-by :at) conj) scheduled)
      scheduled)))

(s/defn stop-reminder! :- Reminder
  "Returns the updated reminder."
  [{:keys [cancel] :as reminder} :- Reminder]
  {:post [(not (:cancel %))]}
  (infof "stopping reminder=%s" (pr-str reminder))
  (when cancel (cancel))
  (dissoc reminder :cancel))

(s/defn remove-reminder! :- (s/maybe s/Num)
  "Returns the number of scheduled reminders left when successful.
   Removes all reminders whose description matches `about`."
  [thread-id :- s/Str about :- s/Str]
  (let [{to-del true to-keep false} (->> (get @threads thread-id)
                                         (group-by #(= about (:about %))))]
    (doseq [reminder to-del]
      (stop-reminder! reminder))
    (swap! threads assoc thread-id to-keep)
    (when (seq to-del)
      (count (remove :fired to-keep)))))

(s/defn reschedule-reminder! :- (s/maybe Reminder)
  ":at is the new time
   Returns the scheduled reminder when successful."
  [fb-page-token :- s/Str
   thread-id :- s/Str
   {:keys [about] :as reminder} :- Reminder]
  {:pre [(:fired reminder)]
   :post [(or (nil? %) (:cancel %))]}
  (when (remove-reminder! thread-id about)
    (schedule-reminder! fb-page-token thread-id (dissoc reminder :fired))))

(def days-of-week
  ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"])

(defn to-pst
  [dt-or-time-ms]
  (-> (cond-> dt-or-time-ms
        (integer? dt-or-time-ms) c/from-long)
      (t/to-time-zone (t/time-zone-for-id "America/Los_Angeles"))))

(s/defn pretty-time :- s/Str
  [time-ms :- s/Num]
  (let [now (to-pst (t/now))
        in-one-week (t/plus now (t/weeks 1))
        dt (to-pst time-ms)
        [month day hour min] ((juxt t/month t/day t/hour t/minute) dt)]
    (str "on "
         (if (t/before? dt in-one-week)
           (->> dt t/day-of-week dec (get days-of-week))
           (format "%s/%s" month day))
         " at " (format "%s:%s"
                        (if (or (= 0 hour) (= 12 hour)) 12 (mod hour 12))
                        (cond->> min (> 10 min) (str "0")))
         (if (> 12 hour) "am" "pm"))))

(s/defn pretty-reminders :- s/Str
  [thread-id :- s/Str]
  (or (some->> (get @threads thread-id)
               (remove :fired)
               seq
               (map (fn [{:keys [at about]}]
                      (format "%s %s" about (pretty-time at))))
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
            about (get-in entities [:reminder 0 :value])
            intent (get-in entities [:intent 0 :value])]
        (cond-> context
          (= "cancel_reminder" intent) (assoc :cancel true)
          (= "set_reminder" intent) (assoc :set true)
          (= "show_reminders" intent) (assoc :reminders (pretty-reminders thread-id))
          (= "snooze_reminder" intent) (assoc :snooze true)
          about (assoc :about about)
          time-ms (assoc :time-ms time-ms
                         :time (pretty-time time-ms))))))

(defn error!>
  [{:keys [fb-page-token]} thread-id context msg]
  (go (warnf "Sending error user=%s msg=%s" thread-id msg)
      (<! (req/fb-message!> fb-page-token thread-id msg))))

(defn cancel-reminder!>
  [params thread-id {:keys [about] :as context}]
  (go (let [n (remove-reminder! thread-id about)]
        (cond-> context
          n (assoc :ok-cancel true :reminders n)))))

(defn clear-context!>
  [params thread-id context]
  (go {}))

(defn set-reminder!>
  [{:keys [fb-page-token]} thread-id {:keys [about time-ms] :as context}]
  {:pre [(and about time-ms)]}
  (go (let [success? (->> {:about about
                           :at time-ms
                           :message (format "Here's your reminder to %s!" about)}
                          (schedule-reminder! fb-page-token thread-id))]
        (cond-> context
          success? (assoc :ok-reminder true)))))

(s/defn snoozable? :- s/Bool
  "True if reminder has fired off no longer than `snooze-allowed-ms` ago."
  [{:keys [at]} :- Reminder]
  (<= (-> (t/now) c/to-long (- snooze-allowed-ms)) at))

(defn snooze-reminder!>
  [{:keys [fb-page-token] :as params} thread-id context]
  (go (if-let [{:keys [about at]} (some->> (get @threads thread-id)
                                           (filter (every-pred :fired snoozable?))
                                           last
                                           (<- (assoc :at (-> (t/now) c/to-long (+ snooze-ms))))
                                           (reschedule-reminder! fb-page-token thread-id))]
        (cond-> context
          about (assoc :ok-reminder true
                       :about about
                       :time (pretty-time at)
                       :time-ms at))
        context)))

;; -----------------------------------------------------------------------------
;; Core interface

(defn init!
  "Restarts saved reminders, if any."
  [{:keys [db-url fb-page-token]}]
  (when db-url
    (reset! db-ref db-url)
    (doseq [{:keys [thread-id] :as r} (db/q db-url "select * from woodpecker")]
      (schedule-reminder! fb-page-token thread-id (dissoc r :thread-id)))))

(defn stop!>
  "Saves reminders, if any.
   Discards active reminders that can't be snoozed."
  []
  (go (when-let [db-url @db-ref]
        (db/exec! db-url "truncate table woodpecker")
        (->> @threads
             (mapcat (fn [[thread-id reminders]]
                       (->> reminders
                            (remove (every-pred :fired (comp not snoozable?)))
                            (map #(-> (stop-reminder! %)
                                      (dissoc :fired)
                                      (assoc :thread-id thread-id)))
                            doall)))
             (db/insert-rows! db-url :woodpecker)))))
