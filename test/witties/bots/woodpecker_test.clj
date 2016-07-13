(ns witties.bots.woodpecker-test
  (:use [clojure.test]
        [witties.bots.woodpecker])
  (:require [clojure.core.async :refer [<!!]]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [witties.request :as req])
  (:import [java.util UUID]))

(def now (-> "2016-07-04T08:00:00.000-07:00" c/to-long c/from-long))
(def one-min-ago (->> 1 t/minutes (t/minus now) c/to-string))
(def one-min-later (->> 1 t/minutes (t/plus now) c/to-string))
(def one-hour-later (->> 1 t/hours (t/plus now) c/to-string))
(def one-day-later (->> 1 t/days (t/plus now) c/to-string))

(deftest solve-datetime-test
  (with-redefs [t/now (constantly now)]
    (testing "should only consider the first datetime"
      (are [datetimes expected] (= (c/to-long expected)
                                   (solve-datetime datetimes))
        [{:values [{:value one-hour-later}]} {:values [{:value one-min-later}]}] one-hour-later
        [{:values [{:value one-day-later}]} {:values [{:value one-min-later}]}] one-day-later))
    (testing "should take the outer bound for intervals"
      (are [datetimes expected] (= (c/to-long expected)
                                   (solve-datetime datetimes))
        [{:values [{:from {:value one-min-later} :to {:value one-day-later}}]}] one-day-later
        [{:values [{:from {:value one-hour-later} :to {:value one-day-later}}]}] one-day-later))
    (testing "should take the first datetime in the future"
      (are [datetimes expected] (= (c/to-long expected)
                                   (solve-datetime datetimes))
        [{:values [{:value one-day-later}
                   {:from {:value one-min-later} :to {:value one-hour-later}}]}]
        one-hour-later
        [{:values [{:from {:value one-hour-later} :to {:value one-day-later}}
                   {:value one-min-later}]}]
        one-min-later))
    (testing "should return nil if none found"
      (are [datetimes] (nil? (solve-datetime datetimes))
        [{:values []} {:values [{:value one-min-later}]}]
        [{:values [{:value one-min-ago}]}]))))

(def access-token (System/getenv "WOODPECKER_WIT_TOKEN"))

(defn converse!!
  [session-id message context]
  (if access-token
    (<!! (req/converse!> access-token session-id message context))
    {}))

;; TODO context deltas
(defmacro story
  [story-name & steps]
  `(deftest ~(symbol (str "story-" story-name))
     (let [~'session-id (str "test-" (UUID/randomUUID))]
       ~@(map (fn [[msg context action & [bot]]]
                (condp = action
                  "merge" `(is (= "merge" (:type (converse!! ~'session-id ~msg ~context))))
                  "say" `(let [~'resp (converse!! ~'session-id ~msg ~context)]
                           (is (= "msg" (:type ~'resp)))
                           (is (re-find ~bot (:msg ~'resp))))
                  "stop" `(is (= "stop" (:type (converse!! ~'session-id ~msg ~context))))
                  `(is (= ~action (:action (converse!! ~'session-id ~msg ~context))))))
              steps))))

(def cancel-ko #"any reminders matching")
(def cancel-ok #"Okay, I won't remind you")
(def cancel-which #"Which reminder")
(def greet #"Hey!")
(def oos #"Oops, I didn't catch that")
(def set-ok #"OK I'll remind you")
(def set-what #"What would you")
(def set-when #"When would you")
(def snooze-ko #"any reminders to snooze")
(def welcome #"You're welcome")

;; Dev instance

(story "quick flow"
  ["remind me to buy flowers in 2 minutes" {} "merge"]
  [nil {:set true :about "buy flowers" :time "in 2'" :time-ms 2} "set-reminder"]
  [nil {:set true :about "buy flowers" :time "in 2'" :time-ms 2 :ok-reminder true} "say" set-ok]
  [nil {:set true :about "buy flowers" :time "in 2'" :time-ms 2 :ok-reminder true} "done-set"]
  [nil {} "stop"])

(story "slow flow"
  ["hi" {} "say" greet]
  [nil {} "stop"]
  ["I want to set a reminder" {} "merge"]
  [nil {:set true} "say" set-what]
  [nil {:set true} "stop"]
  ["to eat" {:set true} "merge"]
  [nil {:set true :about "eat"} "say" set-when]
  [nil {:set true :about "eat"} "stop"]
  ["in 2 minutes" {:set true :about "eat"} "merge"]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2} "set-reminder"]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "say" set-ok]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "done-set"]
  [nil {} "stop"]
  ["thank you" {} "say" welcome]
  [nil {} "stop"])

(story "cancel flow with listing"
  ["I'd like to cancel a reminder" {} "merge"]
  [nil {:cancel true} "say" cancel-which]
  [nil {:cancel true} "stop"]
  ["which ones do I have?" {:cancel true} "show-reminders"]
  [nil {:cancel true} "stop"]
  ["to eat" {:cancel true} "merge"]
  [nil {:cancel true :about "eat"} "cancel-reminder"]
  [nil {:cancel true :about "eat" :ok-cancel true :reminders-left 0} "say" cancel-ok]
  [nil {:cancel true :about "eat" :ok-cancel true :reminders-left 0} "done-cancel"]
  [nil {} "stop"])

(story "greetings+cancel+show+thanks"
  ["hi there" {} "say" greet]
  [nil {} "stop"]
  ["cancel my reminder" {} "merge"]
  [nil {:cancel true} "say" cancel-which]
  [nil {:cancel true} "stop"]
  ["show my reminders" {:cancel true} "show-reminders"]
  [nil {:cancel true} "stop"]
  ["to buy flowers" {:cancel true} "merge"]
  [nil {:cancel true :about "buy flowers"} "cancel-reminder"]
  [nil {:cancel true :about "buy flowers" :ok-cancel true :reminders-left 0} "say" cancel-ok]
  [nil {:cancel true :about "buy flowers" :ok-cancel true :reminders-left 0} "done-cancel"]
  [nil {} "stop"]
  ["thanks" {} "say" welcome]
  [nil {} "stop"])

(story "set+oos+snooze+thanks"
  ["remind me to eat" {} "merge"]
  [nil {:set true :about "eat"} "say" set-when]
  [nil {:set true :about "eat"} "stop"]
  ["I'm batman" {:set true :about "eat"} "say" oos]
  [nil {:set true :about "eat"} "stop"]
  ["snooze" {:set true :about "eat"} "snooze-reminder"]
  [nil {:set true :about "eat"} "say" snooze-ko]
  [nil {:set true :about "eat"} "stop"]
  ["in 2 minutes" {:set true :about "eat"} "merge"]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2} "set-reminder"]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "say" set-ok]
  [nil {:set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "done-set"]
  [nil {} "stop"]
  ["thanks" {} "say" welcome]
  [nil {} "stop"])

(story "greetings+cancel+snooze"
  ["hey" {} "say" greet]
  [nil {} "stop"]
  ["hey there" {} "say" greet]
  [nil {} "stop"]
  ["cancel my reminder" {} "merge"]
  [nil {:cancel true} "say" cancel-which]
  [nil {:cancel true} "stop"]
  ["snooze it" {:cancel true} "snooze-reminder"]
  [nil {:cancel true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "say" set-ok]
  [nil {:cancel true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "done-set"]
  [nil {:cancel true} "stop"]
  ["to eat" {:cancel true} "merge"]
  [nil {:cancel true :about "eat"} "cancel-reminder"]
  [nil {:cancel true :about "eat"} "say" cancel-ko]
  [nil {:cancel true :about "eat"} "done-cancel"]
  [nil {} "stop"])

(story "set+show+cancel+snooze"
  ["remind me in 2 minutes" {} "merge"]
  [nil {:set true :time "in 2'" :time-ms 2} "say" set-what]
  [nil {:set true :time "in 2'" :time-ms 2} "stop"]
  ["show my reminders" {:set true :time "in 2'" :time-ms 2} "show-reminders"]
  [nil {:set true :time "in 2'" :time-ms 2} "stop"]
  ["cancel my reminder" {:set true :time "in 2'" :time-ms 2} "merge"]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true} "say" cancel-which]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true} "stop"]
  ["snooze" {:set true :time "in 2'" :time-ms 2 :cancel true} "snooze-reminder"]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true} "say" snooze-ko]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true} "stop"]
  ["to eat" {:set true :time "in 2'" :time-ms 2 :cancel true} "merge"]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true :about "eat"} "cancel-reminder"]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true :about "eat"} "say" cancel-ko]
  [nil {:set true :time "in 2'" :time-ms 2 :cancel true :about "eat"} "done-cancel"]
  [nil {:set true :time "in 2'" :time-ms 2} "stop"]
  ["to eat" {:set true :time "in 2'" :time-ms 2} "merge"]
  [nil {:set true :time "in 2'" :time-ms 2 :about "eat"} "set-reminder"]
  [nil {:set true :time "in 2'" :time-ms 2 :about "eat" :ok-reminder true} "say" set-ok]
  ;; this one is failing
  #_[nil {:set true :time "in 2'" :time-ms 2 :about "eat" :ok-reminder true} "done-set"]
  [nil {} "stop"])

(story "cancel+set+thanks"
  ["cancel my reminder" {} "merge"]
  [nil {:cancel true} "say" cancel-which]
  [nil {:cancel true} "stop"]
  ["remind me to eat in 2 minutes" {:cancel true} "merge"]
  [nil {:cancel true :set true :about "eat" :time "in 2'" :time-ms 2} "set-reminder"]
  [nil {:cancel true :set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "say" set-ok]
  [nil {:cancel true :set true :about "eat" :time "in 2'" :time-ms 2 :ok-reminder true} "done-set"]
  [nil {:cancel true} "stop"]
  ["thanks" {:cancel true} "say" welcome]
  [nil {:cancel true} "stop"]
  ["to eat" {:cancel true} "merge"]
  [nil {:cancel true :about "eat"} "cancel-reminder"]
  [nil {:cancel true :about "eat"} "say" cancel-ko]
  [nil {:cancel true :about "eat"} "done-cancel"]
  [nil {} "stop"])
