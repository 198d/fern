(import [re]
        [time]
        [threading [Thread]])


(def *delimeter* "||||FERNSHELL||||")


(defclass Result [object]
  [[--init--
    (fn [self connection]
      (setv self.output "")
      (setv self.retcode nil)
      (setv self.connection connection)
      (.start (apply Thread []
                     {"target" (fn []
                                 (for [line connection]
                                   (if (delimeter? line)
                                     (break)
                                     (setv self.output (+ self.output line))))
                                   (setv self.retcode (parse-result line int)))}))
      None)]

   [success?
    (fn [self]
      (-> (. self retcode) bool not))]

   [ready?
    (fn [self]
      (not (nil? (. self retcode))))]])


(defn wait-results [results &optional [ready-fn (fn [&rest args])]]
  (let [[finished []]]
    (while true
      (for [result results]
        (if (and (.ready? result) (not (in result finished)))
          (do
            (if ready-fn (ready-fn result))
            (.append finished result))))
      (if (= (len finished) (len results))
        (break)
        (.sleep time 0.0001))))
  results)


(defn execute [connection command]
  (.write connection (prepare-command command))
  (.write connection (delimeter-command "$?"))
  (Result connection))


(defn prepare-command [command]
  (.format "/bin/bash -c \"{}\"\n" (re.sub "([\"$`])" "\\\\\\1" command)))


(defn delimeter-command [&optional extra]
  (let [[echo-string (if extra (.format "{} {}" *delimeter* extra) *delimeter*)]]
    (.format "echo \"{}\"\n" echo-string)))


(defn delimeter? [line]
  (.startswith line *delimeter*))


(defn parse-result [line &optional [result-fn identity]]
  (result-fn (first (rest (.split line)))))
