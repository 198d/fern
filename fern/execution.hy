(import [re]
        [time]
        [shlex]
        [subprocess [Popen STDOUT PIPE]]
        [fern.hosts [connect]])


(defclass Result [object]
  [[--init--
    (fn [self stdout stderr poll-fn &optional host]
      (setv self.stdout stdout)
      (setv self.stderr stderr)
      (setv self.poll-fn poll-fn)
      (setv self.host host)
      None)]

   [--str--
    (fn [self]
      (.read (. self stdout)))]

   [success?
    (fn [self]
      (-> (.poll-fn self) bool not))]

   [ready?
    (fn [self]
      (not (nil? (.poll-fn self))))]])


(defn wait-results [results &optional [ready-fn (fn [&rest args])] [tick-fn (fn [&rest args])]]
  (let [[finished []]]
    (while true
      (tick-fn results)
      (for [result results]
        (if (and (.ready? result) (not (in result finished)))
          (do
            (if ready-fn (ready-fn result))
            (.append finished result))))
      (if (= (len finished) (len results))
        (break)
        (.sleep time 0.0001))))
  (tick-fn results)
  results)


(defn execute [command &optional connection]
  (if connection
    (let [[(, pipes poll-fn) (.execute connection (prepare-command command))]
          [(, stdin stdout stderr) pipes]]
      (Result stdout stderr poll-fn (. connection host)))
    (let [[popen (apply Popen [(.split shlex (prepare-command command))]
                              {"stdout" PIPE "stderr" STDOUT "stdin" PIPE
                               "universal_newlines" true})]]
      (Result (. popen stdout) (. popen stderr) (. popen poll)))))


(defn prepare-command [command]
  (.format "/bin/bash -c \"{}\"" (re.sub "([\"`])" "\\\\\\1" command)))
