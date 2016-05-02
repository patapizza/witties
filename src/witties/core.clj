(ns witties.core
  (:require [clojure.core.async :refer [<! alts! go go-loop]]
            [witties.bots.quickstart]
            [witties.wit :as wit])
  (:import [java.util UUID]))

(def bots (atom {:quickstart {:access-token "xx"
                              :sessions nil}}))
(def recipient->bot {"dummy" :quickstart})

(def max-steps 10)

(defn run-actions>
  ([bot access-token session-id user-msg] (run-actions> bot access-token session-id user-msg nil))
  ([bot access-token session-id user-msg context]
   {:pre [(keyword? bot)]}
   (let [ns (symbol (str "witties.bots." (name bot)))
         step> (fn step> [msg context steps]
                 (go (if (>= 0 steps)
                       (do (printf "Max steps reached")
                           context)
                       (let [{:keys [action entities error msg type]}
                             (<! (wit/converse> access-token session-id msg context))]
                         (cond
                           error
                           (printf "error: %s" error)

                           (= "stop" type)
                           context

                           (= "msg" type)
                           (do ((ns-resolve ns 'say) session-id context msg)
                               (<! (step> nil context (dec steps))))

                           (= "merge" type)
                           (let [context' ((ns-resolve ns 'merge) session-id context entities user-msg)]
                             (<! (step> nil context' (dec steps))))

                           (= "action" type)
                           (let [context' ((ns-resolve ns (symbol action)) session-id context)]
                             (<! (step> nil context' (dec steps))))

                           (= "error" type)
                           (do ((ns-resolve ns 'error) session-id context "Oops, I don't know what to do.")
                               (<! (step> nil context (dec steps)))))))))]
     (step> user-msg context max-steps))))

(defn get-or-create-session!
  [bot sender]
  (let [sessions (get-in @bots [bot :sessions])
        new-session! (fn []
                       (let [session-id (str (UUID/randomUUID))
                             session {:thread-id sender}]
                         (swap! bots assoc-in [bot :sessions session-id] session)
                         (assoc session :session-id session-id)))]
    (or (some (fn [[session-id {:keys [thread-id] :as session}]]
                (when (= sender thread-id)
                  (assoc session :session-id session-id)))
              sessions)
        (new-session!))))

(defn init!
  [in ctrl]
  (go-loop []
    (let [[v c] (alts! [in ctrl])]
      (when (= c in)
        (let [{:keys [recipient sender text]} v
              bot (recipient->bot recipient)
              {:keys [session-id context]} (get-or-create-session! bot sender)
              access-token (-> @bots bot :access-token)
              ;; TODO don't block here
              context' (<! (run-actions> bot access-token session-id text context))]
          (when context'
           (swap! bots update-in [bot :sessions session-id :context] context'))
          (recur))))))
