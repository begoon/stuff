(def alphabet 
  (str
    "0123456789"
    "abcdefghig"
    "klmnopqrst"
    "uvwxyzABCD"
    "EFGHIGKLMN"
    "OPQRSTVUWX"
    "YZ!@£$%^&*"
    "()_+¡€#¢∞§"
    "¶•ªº–≠~`{“"
    "}:|<≤>≥?÷æ"))

(def board-width 10)
(def board (range (* board-width board-width)))

(defn format-board [board n]
  (do 
    (defn format-line [line]
      (reduce str (map #(format "%3d" %) line)))
    (map format-line (partition n board))))
  
(defn print-board [board n]
  (map println (format-board board n)))

(defn rows [board]
  (partition board-width board))

(defn columns [board]
  (reverse
    (loop [a board n board-width result `()]
      (if (> n 0)
        (recur 
          (drop 1 a) 
          (dec n) 
          (cons (take-nth board-width a) result))
        result))))

(defn diagonal-1 [board]
  (reverse
    (loop [a board n 0 result `()]
      (if (< n board-width)
        (recur 
          (drop 1 (drop-last board-width a)) 
          (inc n) 
          (cons (take-nth (+ board-width 1) a) result))
        result))))

(defn diagonal-2 [board]
  (reverse
    (loop [a board n 0 result `()]
      (if (< n board-width)
        (recur 
          (drop 10 a) 
          (inc n) 
          (cons (take-nth (inc board-width) a) result))
        result))))

(defn diagonal-3 [board]
  (reverse
    (loop [a board n (- board-width 1) result `()]
      (if (>= n 0)
        (recur 
          (drop-last board-width a)
          (dec n) 
          (cons (take-nth (dec board-width) (drop n (drop-last (dec board-width) a))) result))
        result))))

(print-board (range 100) board-width)
(map println (diagonal-1 board))
(map println (diagonal-2 board))
(map println (diagonal-3 board))
