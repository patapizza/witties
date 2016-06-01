(defproject witties "0.1.0"
  :description "Wit.ai-based bots"
  :url "http://github.com/patapizza/witties"
  :license {:name "The MIT License"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [aleph "0.4.2-alpha4"]
                 [cheshire "5.6.1"]
                 [clj-time "0.11.0"]
                 [com.taoensso/timbre "4.3.1"]
                 [compojure "1.4.0"]
                 [http-kit "2.1.19"]
                 [jarohen/chime "0.1.9"]
                 [manifold "0.1.4"]
                 [org.postgresql/postgresql "9.4.1208"]
                 [pandect "0.6.0"]
                 [prismatic/plumbing "0.5.3"]
                 [prismatic/schema "1.1.1"]
                 [ring/ring-defaults "0.1.5"]
                 [ring/ring-json "0.4.0"] ]
  :main witties.handler
  :uberjar-name "witties-standalone.jar"
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}
             :uberjar {:aot :all}})
