(defproject witties "0.1.0-alpha"
  :description "Wit.ai-based bots"
  :url "http://github.com/patapizza/witties"
  :license {:name "The MIT License"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [aleph "0.4.1"]
                 [cheshire "5.6.1"]
                 [compojure "1.4.0"]
                 [http-kit "2.1.19"]
                 [manifold "0.1.4"]
                 [ring/ring-defaults "0.1.5"]]
  :main witties.handler
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}
             :uberjar {:aot :all}})
