(import [re]
        [time]
        [fern.hosts [connect]])


(defclass Result [object]
  [[--init--
    (fn [self host stdout stderr poll-fn]
      (setv self.host host)
      (setv self.stdout stdout)
      (setv self.stderr stderr)
      (setv self.poll-fn poll-fn)
      None)]

   [success?
    (fn [self]
      (-> (.poll-fn self) bool not))]

   [ready?
    (fn [self]
      (not (nil? (.poll-fn self))))]])


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
  (let [[(, pipes poll-fn) (.execute connection (prepare-command command))]
        [(, stdin stdout stderr) pipes]]
    (Result (. connection host) stdout stderr poll-fn)))


(defn prepare-command [command]
  (.format "/bin/bash -c \"{}\"\n" (re.sub "([\"$`])" "\\\\\\1" command)))
