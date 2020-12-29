;;; https://repl.it/@begoon/clojure#main.clj

(loop [x 1]
  (if (< x 5)
    (do
      (printf "Hello %d\n" x)
      (recur(inc x)) ) ) )
(System/getenv "PATH")
