(ns solution (:require [clojure.repl :refer [doc source]]))

(defn my-last [list]
  (if (> 2 (count list)) 
    (first list)
    (recur (rest list))))

(defn my-but-list [list]
  (if (< (count list) 3) 
    list
    (recur (rest list))))

(defn element-at [list i] 
  (if (= i 1) 
    (first list)
    (recur (rest list) (dec i))))

(defn my-count [list]
  (loop [i 0
         l list]
    (if (empty? l)
      i
      (recur (inc i) (rest l)))))

(defn my-reverse [list]
  (loop [res (vec nil)
         l list]
    (if (empty? l)
      res
      (recur (cons (first l) res) (rest l)))))

(defn palindrome? [list]
  (cond
    (< (count list) 2) true
    (not (= (first list) (last list))) false
    :else (recur (rest (take (dec (count list)) list )))))

(defn my-flatten [l]
  (cond 
    (empty? l) '()
    (coll? (first l)) (concat (my-flatten (first l)) (my-flatten (rest l)))
    :else (conj (my-flatten (rest l)) (first l))))

(defn compress [l]
  (let [[f s & r] l]
    (cond
      (and (nil? f) (nil? s)) []
      (and (not (nil? f)) (nil? s)) [f]
      (= f s) (compress (cons s r))
      :else (cons f (compress (cons s r))))))

(defn pack [l]
  (partition-by identity l))

(defn encode [l]
  (map #(list (count %1) (first %1)) (pack l)))

(defn encode-modified [l]
  (map #(if (= 1 (count %1))
          (first %1)
          (list (count %1) (first %1)))
       (pack l)))

(defn decode [l]
  (mapcat #(if (coll? %1)
          (take (first %1) (repeat (second %1)))
          (list %1))
       l))

(my-last '("a" "b" "c" "d"))
(my-but-list '("a" "b" "c" "d"))
(element-at '("a" "b" "c" "d" "e") 3)
(my-count ["a" "b" "c" "d"])
(my-reverse ["a" "b" "c" "d"])
(palindrome? ["a" "b" "c" "d"])
(palindrome? ["a" "b" "b" "a"])
(my-flatten '(1 2 (3 4 (5 6) (7) 5)))
(compress '(a a a a b c c a a d e e e e))
(pack '(a a a a b c c a a d e e e e))
(encode '(a a a a b c c a a d e e e e))
(encode-modified '(a a a a b c c a a d e e e e))
(decode '((4 a) b (2 c) (2 a) d (4 e)))
