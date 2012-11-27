(ns logic-99.core-test
  (:use logic-99.core)
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.logic :refer [run* run fresh]]))

(deftest test-listo
  (is (empty? (run* [q] (listo 'x))))
  (is (empty? (run* [q] (listo 2))))
  (is (not (empty? (run* [q] (listo '[x]))))))

(deftest test-not!
  (is (empty? (run* [q] (not! clojure.core.logic/succeed))))
  (is (not (empty? (run* [q] (not! clojure.core.logic/fail)))))
  (is (empty? (run* [q] (not! (listo [])) )))
  (is (not (empty? (run* [q] (not! (listo 2)) )))))

;################################################################################

(deftest test-lasto
  (testing "fails for empty list"
    (is (empty? (run* [q] (lasto q [])))))
  (testing "gets last"
    (is (= [3] (run* [q] (lasto q [1 2 3]))))))

(deftest test-penultimateo
  (testing "fails for empty list"
    (is (empty? (run* [q] (penultimateo q [])))))
  (testing "fails for singleton list"
    (is (empty? (run* [q] (penultimateo q ['x])))))
  (testing "gets penultimate from 2 item list"
    (is (= ['x] (run* [q] (penultimateo q ['x 'y])))))
  (testing "gets penultimate from 5 item list"
    (is (= ['s] (run* [q] (penultimateo q '[m n o p q r s t]))))))

(deftest test-ntho
  (testing "fails for negative n"
    (is (empty? (run* [q] (ntho q [1 2 3] -1)))))
  (testing "gets the nth element"
    (is (= ['t] (run* [q] (ntho q '[q r s t u] 3))))
    (is (= ['u] (run* [q] (ntho q '[q r s t u] 4))))
    (is (= ['q] (run* [q] (ntho q '[q r s t u] 0)))))
  (testing "gets the index of a value"
    (is (= [2] (run* [q] (ntho :p [1 2 :p 3] q))))))

(deftest test-counto
  (testing "returns 0 for empty list"
    (is (= [0] (run* [q] (counto q [])))))
  (testing "returns 1 for singleton list"
    (is (= [1] (run* [q] (counto q '[x])))))
  (testing "returns length of list"
    (is (= [4] (run* [q] (counto q '[x y z w])))))
  (testing "makes a list for a count"
    (is (= 3 (count (first (run* [q] (counto 3 q))))))))

(deftest test-reverseo
  (testing "reverses an empty list"
    (is (= [] (first (run* [q] (reverseo [] q))))))
  (testing "reverses a singleton list"
    (is (= [1] (first (run* [q] (reverseo [1] q))))))
  (testing "reverses a list"
    (is (= [1 2 3] (first (run* [q] (reverseo [3 2 1] q)))))))

(deftest test-palindromeo
  (testing "succeeds for empty list"
    (is (not (empty? (run* [q] (palindromeo []))))))
  (testing "succeeds for palindrome"
    (is (not (empty? (run* [q] (palindromeo [1 2 3 4 5 4 3 2 1]))))))
  (testing "fails for non-palindrome"
    (is (empty? (run* [q] (palindromeo [1 2 3 4 5]))))))

(deftest test-flatteno
  (testing "flattens an empty list"
    (is (= [[]] (run* [q] (flatteno [] q)))))
  (testing "flattens a nested list structure recursively"
    (is (= [[1 2 3 4 5]]
           (run* [q] (flatteno [1 [2 [3 4] 5]] q))))))

(deftest test-compresso
  (testing "compresses an empty list"
    (is (= [[]] (run* [q] (compresso [] q)))))
  (testing "compresses a list"
    (is (= [[1]] (run* [q] (compresso [1] q))))
    (is (= [[1]] (run* [q] (compresso [1 1] q))))
    (is (= [[2 1]] (run* [q] (compresso [2 1 1] q))))
    (is (= [[2 1]] (run* [q] (compresso [2 2 1 1] q))))
    (is (= ['[a b c a d e]] (run* [q] (compresso '[a a a a b c c a a d e e e e] q))))))

(deftest test-packo
  (testing "packs an empty list"
    (is (= [[]] (run* [q] (packo [] q)))))
  (testing "packs a list"
    (is (= '[[[a]]] (run* [q] (packo '[a] q))))
    (is (= '[[[a a] [b]]] (run* [q] (packo '[a a b] q))))
    (is (= '[[[a a a a] [b] [c c] [a a] [d] [e e e e]]]
           (run* [q] (packo '[a a a a b c c a a d e e e e] q)))))
  ; TODO doesn't run in reverse.
  (comment (testing "unpacks a list"
    (is (= '[[a a a a b c c a a d e e e e]]
           (run* [q] (packo q '[[a a a a] [b] [c c] [a a] [d] [e e e e]])))))))

(deftest test-encodeo
  (testing "encodes an empty list"
    (is (= [[]] (run* [q] (encodeo [] q)))))
  (testing "encodes a list"
    (is (= '[[[2 a] [1 b] [2 c ] [2 a] [1 d] [4 e]]]
           (run* [q] (encodeo '[a a b c c a a d e e e e] q))))))

(deftest test-rangeo
  (testing "generates an empty list for empty range"
    (is (= [[]] (run* [q] (rangeo 2 2 q)))))
  (testing "generates a singleton list for singleton range"
    (is (= [[3]] (run* [q] (rangeo 3 4 q)))))
  (testing "generates a list for a range"
    (is (= [(range 2 7)] (run* [q] (rangeo 2 7 q)))))
  ; TODO doesn't run in reverse
  (comment (testing "is relational"
    (is (= [[2 7]] (run* [q] (fresh [s e]
                               (rangeo s e (range 2 6))
                               (== q [s e]))))))))

(deftest test-duplicate2o
  (testing "generates an empty list for empty input"
    (is (= [[]] (run* [q] (duplicate2o [] q)))))
  (testing "duplicates elemens in input"
    (is (= [[2 2]] (run* [q] (duplicate2o [2] q)))))
  (testing "duplicates elemens in input"
    (is (= [[3 3 2 2]] (run* [q] (duplicate2o [3 2] q))))))

(deftest test-duplicateo
  (testing "generates an empty list for empty input"
    (is (= [[]] (run* [q] (duplicateo 5 [] q)))))
  (testing "duplicates elements in input"
    (is (= [[2 2 2]] (run* [q] (duplicateo 3 [2] q)))))
  (testing "duplicates elemens in input"
    (is (= [[3 3 3 3 2 2 2 2]] (run* [q] (duplicateo 4 [3 2] q))))))

(deftest test-is-treeo
  (testing "nil is a tree"
    (is (not (empty? (run* [q] (is-treeo nil))))))
  (testing "detects a binary tree"
    (is (not (empty? (run* [q] (is-treeo '[a [b nil nil] nil]))))))
  (testing "rejects a non-binary tree"
    (is (empty? (run* [q] (is-treeo '[a [b nil nil]]))))))

