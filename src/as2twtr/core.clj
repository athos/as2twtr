(ns as2twtr.core
  (:gen-class)
  (use [as2twtr.config :only (read-configuration)]
       [clojure.contrib.str-utils :only (re-sub)])
  (require [as2twtr.as-chat :as as]
	   [as2twtr.twitter :as tw]
	   [clojure.contrib.logging :as log]))

(defn config->twitter-cred [config]
  (tw/make-twitter-client
    (config "twitter.consumer-key")
    (config "twitter.consumer-secret")
    (config "twitter.access-token")
    (config "twitter.access-token-secret")))

(defn feed-to-twitter [config id name text]
  (let [content (format "%s: %s" name text)]
    (tw/update-status
      (config->twitter-cred config)
      (if (> (count content) 140)
	(str (subs content 139) \u2026)
	content))))

(defn make-observer [config]
  (fn [type content]
    (let [{id :id, name :name, body :body} content]
      (case type
	:create (feed-to-twitter config id name body)
	:update (feed-to-twitter config id name body)))))

(defn forward-from-twitter [config client since-id]
  (let [cred (config->twitter-cred config)
	username (config "twitter.username")
	mentions (tw/mentions cred since-id)]
    (if (empty? mentions)
      since-id
      (let [follower? (apply hash-set (tw/followers cred))
	    rx (re-pattern (str "^@" (config "twitter.username") "\\s*"))]
	(doseq [[id text user user-id in-reply-to] (reverse mentions)
		:when (follower? user-id)]
	  (log/info  (format "forward from Twitter: %s" text))
	  #_(mixi/feed-to-mixi (re-sub rx "" text) in-reply-to))
	#_(tw/max-status-id mentions)))))

(defn kick-reply-watcher! [client config]
  (let [cred (config->twitter-cred config)]
    (letfn [(iter []
	      (try
		(loop [since-id (tw/max-status-id (tw/mentions cred))]
		  (Thread/sleep 60000)
		  (log/info "watcher polling")
		  (recur (forward-from-twitter config client since-id)))
		(catch Exception e
		  (log/error (format "watcher error: %s" (.getMessage e)))))
	      (Thread/sleep 60000)
	      (recur))]
      (.start (Thread. iter)))))

(defn -main [& args]
  (when (= (count args) 0)
    (throw (Exception. "too few argument")))
  (log/info "starting ...")
  (let [config (read-configuration (first args))
	client (as/connect (config "aschat.websocket-uri")
			   (config "aschat.post-uri")
			   (make-observer config))]
    #_(kick-watcher! client config)
    (while true (.suspend (Thread/currentThread)))))
