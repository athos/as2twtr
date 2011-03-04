(ns as2twtr.config)

(def *configurations* (ref nil))

(defmacro defconfig [name & key-values]
  (let [configs (map (fn [[key value]]
		       [(str name "." key) value])
		     key-values)]
    `(do (alter *configurations*
		(fn [~'x] (into ~'x '~configs)))
	 nil)))

(defn read-configuration [filename]
  (dosync
    (ref-set *configurations* {})
    (load-file filename)
    @*configurations*))
