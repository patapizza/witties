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
;; TODO test bot responses
(defmacro story
  [story-name & steps]
  `(deftest ~(symbol (str "story-" story-name))
     (let [~'session-id (str "test-" (UUID/randomUUID))]
       ~@(map (fn [[msg context action]]
                (condp = action
                  "merge" `(is (= "merge" (:type (converse!! ~'session-id ~msg ~context))))
                  "say" `(is (= "msg" (:type (converse!! ~'session-id ~msg ~context))))
                  `(is (= ~action (:action (converse!! ~'session-id ~msg ~context))))))
              (butlast steps))
       ~(let [[msg context] (last steps)]
         `(is (= "stop" (:type (converse!! ~'session-id ~msg ~context))))))))

(story "cancel"
  ["cancel my reminder to buy flowers" {} "merge"]
  [nil {:about "buy flowers" :cancel true} "cancel-reminder"]
  [nil {:about "buy flowers" :cancel true :ok-cancel true :reminders 2} "say"]
  [nil {:about "buy flowers" :cancel true :ok-cancel true :reminders 2} "clear-context"]
  [nil {}])

(story "list"
  ["show me my reminders" {} "merge"]
  [nil {:reminders "reminders"} "say"]
  [nil {:reminders "reminders"} "clear-context"]
  [nil {}])

(story "set"
  ["Remind me to buy flowers tomorrow morning" {} "merge"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42} "set-reminder"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42 :ok-reminder true} "say"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42 :ok-reminder true} "clear-context"]
  [nil {}])

(story "snooze"
  ["snooze" {} "merge"]
  [nil {:snooze true} "snooze-reminder"]
  [nil {:snooze true :time "in 10 minutes" :ok-reminder true :about "buy flowers"} "say"]
  [nil {:snooze true :time "in 10 minutes" :ok-reminder true :about "buy flowers"} "clear-context"]
  [nil {}])

;; Dev instance
#_(story "happy-path"
  ["Remind me to buy flowers tomorrow morning" {} "merge"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42} "set-reminder"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42 :ok-reminder true} "say"]
  [nil {:about "buy flowers" :set true :time "tomorrow morning" :time-ms 42 :ok-reminder true} "done-set"]
  [nil {}])
