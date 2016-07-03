(ns witties.bots.woodpecker-test
  (:use [clojure.test]
        [witties.bots.woodpecker])
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]))

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
