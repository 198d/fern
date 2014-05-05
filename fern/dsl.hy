(import [hy.models.keyword [HyKeyword]]
        [hy.models.list [HyList]]
        [hy.models.string [HyString]])


(defmacro defhosts [name &rest host-defs]
  `(def ~name (list-comp [(first host-def)
                          (or (nth host-def 1) [])
                          (or (nth host-def 2) {})] [host-def [~@host-defs]])))


(defmacro run [&rest args]
  (let [[it (iter args)] [exprs []] [leaves []] [hosts 'nil]]
    (for [arg it]
      (if (instance? HyKeyword arg)
        (cond [(= :against arg) (setv hosts (next it))])
        (if (instance? HyList arg)
            (.append exprs arg)
            (.append leaves arg))))
    (if leaves (.append exprs leaves))
    (defn rewrite-body [exprs]
      (let [[-exprs '[]]]
        (for [expr exprs]
          (.append -exprs `(.join " " [~@(list-comp
                                (cond [(instance? HySymbol part)
                                       (HyString (.replace (str part) "_" "-"))]
                                      [(and (instance? HyList part)
                                            (= 'quote (first part)))
                                       (first (rest part))]
                                      [true (HyString part)])
                                [part expr])])))
        -exprs))

    (with-gensyms [exit connect hostname execute wait-results green red bold command host]
      `(do
        (import [sys [exit :as ~exit]]
                [fern.hosts [connect :as ~connect hostname :as ~hostname]]
                [fern.execution [execute :as ~execute wait-results :as ~wait-results]]
                [fern.output [green :as ~green red :as ~red bold :as ~bold]])
        (for [~command [~@(rewrite-body exprs)]]
          (print (.format "> Executing `{}` against {} hosts" (~bold ~command) (~bold (len ~hosts))))
          (~wait-results
            (list-comp (~execute (~connect ~host) ~command) [~host ~hosts])
            (fn [result]
              (if (.success? result)
                (print "  " (~green "\u2713") (~hostname (. result host)))
                (do
                  (print "  " (~red "\u2717") (~hostname (. result host)))
                  (for [line (. result stdout)]
                    (apply print ["  " line] {"end" ""}))
                  (~exit 1))))))))))


(defmacro with-hosts [&rest args]
  (let [[it (iter args)] [filter-hosts-args ['[] '[] '{}]] [body []] [show 'false]]
    (for [arg it]
      (if (instance? HyKeyword arg)
          (cond [(= :in arg) (assoc filter-hosts-args 0 `(+ ~@(next it) []))]
                [(= :providing arg) (assoc filter-hosts-args 1 (next it))]
                [(= :where arg) (assoc filter-hosts-args 2 (next it))]
                [(= :show arg) (setv show (next it))])
          (.append body arg)))
    (with-gensyms [hosts filter-hosts pprint]
      (defn rewrite-body [body]
        (let [[-body '()]]
          (for [part body]
            (.append -body (if (instance? HyList part)
                             (cond [(= 'with-hosts (first part))
                                    `(~@part :in [~hosts])]
                                   [(= 'run (first part)) `(~@part :against ~hosts)]
                                   [true (rewrite-body part)])
                             part)))
          -body))
      `(do
        (import [fern.hosts [filter-hosts :as ~filter-hosts]])
        (let [[~hosts (~filter-hosts ~@filter-hosts-args)]]
          (if ~show
              (do
                (import [pprint [pprint :as ~pprint]])
                (~pprint ~hosts)))
          ~@(rewrite-body body))))))
