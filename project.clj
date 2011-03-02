(defproject as2twtr "1.0.0-SNAPSHOT"
 :description "AsakusaSatellite Twitter bridge"
 :dependencies [[org.clojure/clojure "1.2.0"]
                [org.clojure/clojure-contrib "1.2.0"]
                [net.homeip.yusuke/twitter4j-core "2.1.0"]
                [org.clojars.raek/tagsoup "1.2"]
                [clojure-saxon "0.9.0-SNAPSHOT"]]
 :dev-dependencies [[swank-clojure "1.2.0"]]
 :repositories {"central" "http://repo1.maven.org/maven2/"}
 :main as2twtr.core)

