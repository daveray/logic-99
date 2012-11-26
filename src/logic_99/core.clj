(ns logic-99.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [succeed fail
                                        conde conda matche fresh
                                        run* run
                                        != == firsto resto emptyo conso appendo
                                        trace-lvars ]]))

; http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/index.html

;################################################################################

; Presumably there's a less lame way to do this.
(defn successoro
  "Helper to get the successor of a number"
  [v v+1]
  (matche [v]
    ([-1] (== v+1 0))
    ([0]  (== v+1 1))
    ([1]  (== v+1 2))
    ([2]  (== v+1 3))
    ([3]  (== v+1 4))
    ([4]  (== v+1 5))
    ([5]  (== v+1 6))
    ; this should be plenty.
    ([6]  (== v+1 7))))

(defn listo
  "test whether v is a list"
  [v]
  (conde
    [(emptyo v)]
    [(fresh [f] (firsto v f))]))

(defn not!
  "Hack(?) to test for failed goals. Not relational.

  Courtesy of @ambrosbs: https://twitter.com/ambrosebs/status/271877100032647168
  "
  [g]
  (conda [g fail] [succeed succeed]))

;################################################################################

; P01
; Find the last element of a list
(defn lasto [last list]
  (matche [list]
    ([[]]          fail)
    ([[last]]      succeed)
    ([[_ . ?rest]] (lasto last ?rest))))

; P02
; Find second to last element of a list
(defn penultimateo [p list]
  (matche [list]
    ([[]]          fail)
    ([[_]]         fail)
    ([[p _]]       succeed)
    ([[_ . ?rest]] (penultimateo p ?rest))))

; P03
; find kth element in a list
(defn ntho [value list n]
  (fresh [f r n-1]
    (conde
      [(== n -1)        fail]
      [(== 0 n)         (conso value r list)]
      ; Using clojure.core/dec directly works, but then you can't run
      ; ntho backwards, e.g. (ntho "find-me" list q)
      [(conso f r list) (successoro n-1 n) (ntho value r n-1)])))

; P04
; count the elements in a list
(defn counto
  ([n list] (counto n list 0))
  ([n list c]
   (fresh [c+1]
     (successoro c c+1)
     (matche [list]
       ([[]] (== n c))
       ([[_ . ?rest]] (counto n ?rest c+1)) ))))

; P05
; reverse a list
(defn reverseo
  ([a b] (reverseo a b []))
  ([a b acc]
   (matche [a]
    ([[]]   (== b acc))
    ([[?ahead . ?atail]]
      (reverseo ?atail b (cons ?ahead acc))))))

; P06
; is a list a palindrome?
(defn palindromeo
  [list]
  (reverseo list list))

; P07
; flatten a nested list structure
(defn flatteno
  [in out]
  (matche [in]
    ([[]] (== out in))
    ([[?in-head . ?in-tail]]
     (fresh [prefix suffix]
       (flatteno ?in-tail suffix)
       (conde
         ; if head is a list, flatten it and concat to flattened tail
         [(listo ?in-head)         (flatteno ?in-head prefix) (appendo prefix suffix out)]
         ; if head is not a list, cons onto flattened tail. Note that use of
         ; not! here makes flatteno non-relational
         [(not! (listo ?in-head))  (conso ?in-head suffix out)])))))

; P08
; Eliminate consecutive duplicates of list elements
(defn compresso
  [in out]
  (matche [in]
    ([[]] (== out []))
    ([[?in-head . ?in-tail]]
     (fresh [ctail]
       (compresso ?in-tail ctail)
       (matche [ctail]
         ([[]]                (== out in))
         ([[?in-head . _]]    (== out ctail))
         ([[?ctail-head . _]] (!= ?in-head ?ctail-head) (conso ?in-head ctail out)))))))

; P09
; Pack consecutive duplicates of list elements into sublists.
; TODO can this be made to run backward? The transform is 1-to-1.
(defn packo
  [in out]
  (matche [in]
    ([[]] (== out []))
    ([[?in-head . ?in-tail]]
     (fresh [ptail phead]
       (packo ?in-tail ptail)
       (matche [ptail]
         ([[]] (== out [[?in-head]]))
         ([ [[?in-head . _] . _] ]
            (fresh [current rest new-head]
              (firsto ptail current)
              (conso ?in-head current new-head)
              (resto ptail rest)
              (conso new-head rest out)))
         ([ [[phead    . _] . ?ptail-tail] ]
            (!= ?in-head phead)
            (conso [?in-head] ptail out)))))))

; P10
; RLE of a list
(declare encode-packedo)

(defn encodeo
  [in out]
  (fresh [packed]
    (packo in packed)
    (encode-packedo packed out)))

(defn- encode-packedo
  "helper for encodeo above"
  [in out]
  (matche [in]
    ([[]] (== out []))
    ([[?head . ?tail]]
     (fresh [encoded-tail head-value head-count]
       (encode-packedo ?tail encoded-tail)
       (counto head-count ?head)
       (firsto ?head head-value)
       (conso [head-count head-value] encoded-tail out)))))

; .
; .
; .

; P54
; Determine if a tree is binary
(defn is-treeo
  [t]
  (matche [t]
    ([nil] succeed)
    ([[_ ?l ?r]] (is-treeo ?l) (is-treeo ?r))))

