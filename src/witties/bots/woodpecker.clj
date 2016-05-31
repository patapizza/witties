(ns witties.bots.woodpecker
  (:require [clojure.core.async :refer [<! go]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [chime :refer [chime-at]]
            [plumbing.core :refer [map-vals]]
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

(def config-file "woodpecker.clj")
(def Reminder
  {:about s/Str
   :message s/Str
   :at s/Int
   (s/optional-key :cancel) (s/pred fn?)
   (s/optional-key :fired) s/Bool})
(def reminder-checker (s/checker Reminder))

;; -----------------------------------------------------------------------------
;; Helpers

(defn- schedule-reminder!
  [fb-page-token thread-id {:keys [at message] :as reminder}]
  (if (t/before? (c/from-long at) (t/now))
    (warnf "trying to schedule reminder in the past: %s" (pr-str reminder))
    (do
      (infof "scheduling user=%s reminder=%s" thread-id (pr-str reminder))
      (let [g (fn [r]
                (cond-> r
                  (= message (:message r)) (-> (dissoc :cancel)
                                               (assoc :fired true))))
            f (chime-at [at]
                        (fn [time-ms]
                          ;; TODO reliability
                          (infof "fired user=%s message=%s" thread-id message)
                          (req/fb-message!> fb-page-token thread-id message)
                          (swap! threads update thread-id (partial map g)))
                        {:error-handler #(warnf % "an error occurred")})]
        (assoc reminder :cancel f)))))

(defn- stop-reminder!
  [{:keys [cancel] :as reminder}]
  (infof "stopping reminder=%s" (pr-str reminder))
  (when cancel (cancel))
  (dissoc reminder :cancel))

(defn- remove-reminder!
  "Returns the number of scheduled reminders left when successful"
  [thread-id about]
  (let [exp (-> (get @threads thread-id) count dec)
        f (fn [r]
            (if (= about (:about r))
              (do (stop-reminder! r) nil)
              r))
        res (-> (swap! threads update thread-id (partial keep f))
                (get thread-id))]
    (when (= exp (count res))
      (count (remove :fired res)))))

(defn- reschedule-reminder!
  ":at is the new time"
  [fb-page-token thread-id {:keys [at message about] :as reminder}]
  (when (remove-reminder! thread-id about)
    (schedule-reminder! fb-page-token thread-id (dissoc reminder :fired))))

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
                        (if (or (= 0 hour) (= 12 hour)) 12 (mod hour 12))
                        (cond->> min (> 10 min) (str "0")))
         (if (> 12 hour) "am" "pm"))))

(defn pretty-reminders
  [thread-id]
  (if-let [reminders (->> (get @threads thread-id)
                          (remove :fired)
                          seq)]
    (->> reminders
         (sort-by :at)
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
        (cond
          (= "show_reminders" intent) (assoc context :reminders (pretty-reminders thread-id))
          (= "snooze_reminder" intent) (assoc context :snooze true)
          :else (cond-> context
                  about (assoc :about about)
                  time-ms (assoc :time-ms time-ms
                                 :time (pretty-time time-ms)))))))

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
  (go (let [message (format "Here's your reminder to %s!" about)
            reminder {:about about :at time-ms :message message}]
        (if-let [err (reminder-checker reminder)]
          (do (warnf "malformed reminder reminder=%s err=%s" reminder err)
              context)
          (if-let [reminder (schedule-reminder! fb-page-token thread-id reminder)]
            (do (swap! threads update thread-id conj reminder)
                (assoc context :ok-reminder true))
            context)))))

(defn snooze-reminder!>
  [{:keys [fb-page-token] :as params} thread-id context]
  (go (if-let [reminder (->> (get @threads thread-id)
                             (filter :fired)
                             first)]
        (let [next-at (+ (* 1000 60 10) (:at reminder))
              scheduled (->> (assoc reminder :at next-at)
                             (reschedule-reminder! fb-page-token thread-id))]
          (if scheduled
            (do (swap! threads update thread-id conj scheduled)
                (assoc context
                       :ok-reminder true
                       :about (:about reminder)
                       :time (pretty-time next-at)))
            (do (warnf "couldn't reschedule reminder %s at %s"
                       (pr-str reminder) next-at)
                context)))
        (do (warnf "couldn't find reminder to snooze")
            context))))

;; -----------------------------------------------------------------------------
;; Core interface

(def db-ref (atom nil))

(defn init!
  "Restarts saved reminders, if any.
   Doesn't restore active reminders."
  [{:keys [db-url fb-page-token]}]
  (when db-url
    (reset! db-ref db-url)
    (->> (db/q db-url "select * from woodpecker")
         (keep (fn [{:keys [thread-id] :as r}]
                 (schedule-reminder! fb-page-token thread-id r)))
         (group-by :thread-id)
         (map-vals (partial map #(dissoc % :thread-id)))
         (reset! threads))))

(defn stop!>
  "Saves reminders, if any."
  []
  (go (when-let [db-url @db-ref]
        (db/exec! db-url "truncate table woodpecker")
        (->> @threads
             (mapcat (fn [[thread-id reminders]]
                       (->> reminders
                            (remove :fired)
                            (map (comp #(assoc % :thread-id thread-id)
                                       stop-reminder!)))))
             (db/insert-rows! db-url :woodpecker)))))
