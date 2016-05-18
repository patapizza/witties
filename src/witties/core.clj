(ns witties.core
  (:require [clojure.core.async :refer [<! alts! go go-loop timeout]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [plumbing.core :refer [map-from-keys]]
            [schema.core :as s]
            [taoensso.timbre :refer [debugf infof warnf]]
            [witties.bots.quickstart]
            [witties.bots.woodpecker]
            [witties.request :as req])
  (:import [java.util UUID]))

(def bots
  "Bot config, with sessions.
   Shape:
   {:bot {:wit-token \"\"
          :fb-page-token \"\"
          :fb-page-id \"42\"
          :threads {\"42\" [{:session-id \"\"
                             :context {}}]}}}"
  (atom nil))

(def config-file "bots.clj")
(def max-steps 10)
(def Event
  {:sender s/Str
   :recipient s/Str
   :text s/Str})
(def event-checker (s/checker Event))

(defn ->bot-fn
  [bot f]
  {:pre [(or (string? bot) (keyword? bot))
         (string? f)]}
  (let [qualified-ns (str "witties.bots." (cond-> bot (keyword? bot) name))]
    (ns-resolve (symbol qualified-ns) (symbol f))))

;; TODO refactoring
(defn run-actions!>
  ([bot params thread-id session-id user-msg]
   (run-actions!> bot params thread-id session-id user-msg nil))
  ([bot {:keys [wit-token] :as params} thread-id session-id user-msg context]
   {:pre [(keyword? bot) thread-id wit-token]}
   (let [step!> (fn step!> [msg context steps]
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
                            (do (infof "%s - Executing say!> with thread-id=%s context=%s msg=%s"
                                       bot session-id (pr-str context) msg)
                                (<! ((->bot-fn bot "say!>") params thread-id context msg))
                                (<! (step!> nil context (dec steps))))

                            (= "merge" type)
                            (let [_ (infof "%s - Executing merge!> with thread-id=%s context=%s entities=%s user-msg=%s"
                                           bot thread-id (pr-str context) (pr-str entities) user-msg)
                                  context' (<! ((->bot-fn bot "merge!>") params thread-id context entities user-msg))]
                              (<! (step!> nil context' (dec steps))))

                            (= "action" type)
                            (let [action (str action "!>")
                                  _ (infof "%s - Executing %s with thread-id=%s context=%s"
                                           bot action thread-id (pr-str context))
                                  context' (<! ((->bot-fn bot action) params thread-id context))]
                              (<! (step!> nil context' (dec steps))))

                            (= "error" type)
                            (do (infof "%s - Executing error!> with thread-id=%s context=%s"
                                       bot thread-id (pr-str context))
                                (<! ((->bot-fn bot "error") params thread-id context "Oops, I don't know what to do."))
                                (<! (step!> nil context (dec steps)))))))))]
     (step!> user-msg context max-steps))))

(defn get-or-create-session!
  [bot thread-id]
  (if-let [session (get-in @bots [bot :threads thread-id 0])]
    session
    (let [new-session {:session-id (str (UUID/randomUUID))}]
      (swap! bots update-in [bot :threads thread-id] (comp vec conj) new-session)
      new-session)))

(defn stop-bots!>
  "Gives each bot allowed-ms time to gracefully stop."
  ([bots] (stop-bots!> bots 1000))
  ([bots allowed-ms]
   (let [bot->chan (->> bots
                        (map-from-keys #(when-let [f (->bot-fn % "stop!>")] (f)))
                        (remove (fn [[_ v]] (nil? v)))
                        (into {})
                        (merge {:timeout (timeout allowed-ms)}))]
     (go-loop [chans (vals bot->chan)]
       (let [[_ c] (alts! chans)]
         (when-not (= c (bot->chan :timeout))
           (recur (remove (partial = c) chans))))))))

(defn init!
  [in ctrl]
  (when-let [config (some-> config-file io/resource slurp edn/read-string)]
    (pmap (fn [[bot params]]
            (when-let [f (->bot-fn bot "init!")]
              (f params)))
          config)
    (reset! bots config))
  (go-loop []
    (let [[v c] (alts! [in ctrl])]
      (if (= c ctrl)
        (<! (stop-bots!> (keys @bots)))
        (let [err (event-checker v)
              {:keys [recipient sender text]} v
              [bot params] (some (fn [[bot params]]
                                   (when (= recipient (:fb-page-id params))
                                     [bot (dissoc params :threads)]))
                                 @bots)]
          (cond
            err (warnf "malformed event event=%s err=%s" v err)
            (not bot) (warnf "couldn't find bot for recipient=%s" recipient)
            :else (let [{:keys [session-id context]} (get-or-create-session! bot sender)]
                    (debugf "Running actions for bot=%s thread-id=%s session-id=%s text=%s context=%s"
                            bot sender session-id text (pr-str context))
                    (some->> (run-actions!> bot params sender session-id text context)
                      <! ;; TODO don't block here
                      (swap! bots assoc-in [bot :threads sender 0 :context]))))
          (recur))))))
