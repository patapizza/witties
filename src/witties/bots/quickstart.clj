(ns witties.bots.quickstart
  (require [clojure.core.async :refer [go]]))

(defn say!>
  [params session-id context msg]
  (go (println (format "Bot says: %s" msg))))

(defn merge!>
  [params session-id context entities msg]
  (go (let [loc (get-in entities [:location 0 :value])]
        (cond-> context
          loc (assoc :loc (cond-> loc (map? loc) (:value loc)))))))

(defn error!>
  [params session-id context msg]
  (go (println (format "Bot error: %s" msg))))

(defn fetch-weather!>
  [params session-id context]
  (go (assoc context :forecast "sunny")))
