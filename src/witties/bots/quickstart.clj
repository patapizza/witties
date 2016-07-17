(ns witties.bots.quickstart
  (require [clojure.core.async :refer [go]]))

(defn say!>
  [params thread-id context msg quickreplies]
  (go (println (format "Bot says: %s" msg))))

(defn merge!>
  [params thread-id context entities msg]
  (go (let [loc (get-in entities [:location 0 :value])]
        (cond-> context
          loc (assoc :loc (cond-> loc (map? loc) (:value loc)))))))

(defn fetch-weather!>
  [params thread-id context]
  (go (assoc context :forecast "sunny")))
