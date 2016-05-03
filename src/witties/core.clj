(ns witties.core
  (:require [clojure.core.async :refer [<! alts! go go-loop]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [schema.core :as s]
            [taoensso.timbre :refer [debugf infof warnf]]
            [witties.bots.quickstart]
            [witties.request :as req])
  (:import [java.util UUID]))

(def bots (atom nil))
(def config-file "bots.clj")
(def max-steps 10)
(def Event
  {:sender s/Int
   :recipient s/Int
   :text s/Str})
(def event-checker (s/checker Event))

;; TODO refactoring
(defn run-actions!>
  ([bot params session-id user-msg] (run-actions!> bot params session-id user-msg nil))
  ([bot {:keys [wit-token] :as params} session-id user-msg context]
   {:pre [(keyword? bot)]}
   (let [ns (symbol (str "witties.bots." (name bot)))
         step!> (fn step!> [msg context steps]
                  (go (if (>= 0 steps)
                        (do (warnf "%s - Max steps reached" bot)
                            context)
                        (let [{:keys [action entities error msg type]}
                              (<! (req/converse!> wit-token session-id msg context))]
                          (cond
                            error
                            (warnf "%s - Converse error=%s" bot error)

                            (= "stop" type)
                            (do (infof "%s - Stopping." bot)
                                context)

                            (= "msg" type)
                            (do (infof "%s - Executing say!> with session-id=%s context=%s msg=%s"
                                       bot session-id (pr-str context) msg)
                                (<! ((ns-resolve ns 'say!>) params session-id context msg))
                                (<! (step!> nil context (dec steps))))

                            (= "merge" type)
                            (let [_ (infof "%s - Executing merge!> with session-id=%s context=%s entities=%s user-msg=%s"
                                           bot session-id (pr-str context) (pr-str entities) user-msg)
                                  context' (<! ((ns-resolve ns 'merge!>) params session-id context entities user-msg))]
                              (<! (step!> nil context' (dec steps))))

                            (= "action" type)
                            (let [action (str action "!>")
                                  _ (infof "%s - Executing %s with session-id=%s context=%s"
                                           bot action session-id (pr-str context))
                                  context' (<! ((ns-resolve ns (symbol action)) params session-id context))]
                              (<! (step!> nil context' (dec steps))))

                            (= "error" type)
                            (do (infof "%s - Executing error!> with session-id=%s context=%s"
                                       bot session-id (pr-str context))
                                (<! ((ns-resolve ns 'error) params session-id context "Oops, I don't know what to do."))
                                (<! (step!> nil context (dec steps)))))))))]
     (step!> user-msg context max-steps))))

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
  (reset! bots (some-> config-file io/resource slurp edn/read-string))
  (go-loop []
    (let [[v c] (alts! [in ctrl])
          {:keys [recipient sender text]} (when (map? v) v)]
      (when (= c in)
        (if-let [err (event-checker v)]
          (warnf "received malformed event event=%s err=%s" v err)
          (if-let [[bot params] (some (fn [[bot params]]
                                        (when (= recipient (:fb-page-id params))
                                          [bot params]))
                                      @bots)]
            (let [{:keys [session-id context]} (get-or-create-session! bot sender)]
              (debugf "Running actions for bot=%s session-id=%s text=%s context=%s"
                      bot session-id text (pr-str context))
              (some->> (run-actions!> bot params session-id text context)
                <! ;; TODO don't block here
                (swap! bots update-in [bot :sessions session-id :context])))
            (warnf "couldn't find bot for recipient=%s" recipient)))
        (recur)))))
