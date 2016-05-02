(ns witties.bots.quickstart)

(defn say
  [session-id context msg]
  (printf "Bot says: %s\n" msg))

(defn merge
  [session-id context entities msg]
  (let [loc (get-in entities [:location 0 :value])]
    (cond-> context
      loc (assoc :loc (cond-> loc (map? loc) (:value loc))))))

(defn error
  [session-id context msg]
  (printf "Bot error: %s" msg))

(defn fetch-weather
  [session-id context]
  (assoc context :forecast "sunny"))
