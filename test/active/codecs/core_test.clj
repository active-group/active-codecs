(ns active.codecs.core-test
  (:require [active.codecs.core :as core]
            [clojure.test :refer :all]))

(defn n-chars [n] (core/simple-codec (fn [str n]
                                       (assert (= n (count str)))
                                       (take n (seq str)))
                                     (fn [chars n]
                                       (if (< (count chars) n)
                                         (core/decoder-error "Too few codes")
                                         [(apply str (take n chars)) (drop n chars)]))
                                     n))

(deftest encode-test
  (is (= [\H \e \l \l \o]
         (core/encode (n-chars 5) "Hello"))))

(deftest decode-test
  (is (= ["Hello" [\space \W \o \r \l \d]]
         (core/decode (n-chars 5) [\H \e \l \l \o \space \W \o \r \l \d]))))

(deftest decode-full-test
  (is (= "Hello"
         (core/decode-full (n-chars 5) [\H \e \l \l \o])))
  (is (thrown? Exception
               (core/decode-full (n-chars 5) [\H \e \l \l \o \space \W \o \r \l \d])))
  (is (thrown? Exception
               (core/decode-full (n-chars 5) [\H \e \l \l]))))

(deftest try-decode-test
  (is (= "Hello"
         (core/try-decode (n-chars 5) [\H \e \l \l \o])))
  (is (= ::error
         (core/try-decode (n-chars 5) [\H \e \l \l \o \space \W \o \r \l \d] ::error)))
  (is (= ::error
         (core/try-decode (n-chars 5) [\H \e \l \l] ::error))))

(deftest translate-test
  (let [prefixed (core/translate (n-chars 5)
                                 (fn [s-in] (apply str (drop 2 s-in)))
                                 (fn [s-out] (str "00" s-out)))]
    (is (= "00Hello"
           (core/try-decode prefixed [\H \e \l \l \o])))
    (is (= [\H \e \l \l \o]
           (core/encode prefixed "00Hello")))))

(deftest seq-test
  (let [two-strs (core/seq (n-chars 2) (n-chars 3))]
    (is (= ["He" "llo"]
           (core/try-decode two-strs [\H \e \l \l \o])))
    (is (= [\H \e \l \l \o]
           (core/encode two-strs ["He" "llo"]))))
  (let [three-strs (core/seq (n-chars 2) (n-chars 2) (n-chars 1))]
    (is (= ["He" "ll" "o"]
           (core/try-decode three-strs [\H \e \l \l \o])))
    (is (= [\H \e \l \l \o]
           (core/encode three-strs ["He" "ll" "o"])))))

(deftest conditional-test
  (let [h-w (core/conditional (n-chars 1)
                              (fn [s]
                                (if (= "H" s)
                                  (n-chars 5) ;; "ello "
                                  (n-chars 4) ;; "orld"
                                  )))]
    (is (= ["H" "ello "]
           (core/try-decode h-w [\H \e \l \l \o \space])))
    (is (= [\H \e \l \l \o \space]
           (core/encode h-w ["H" "ello "])))
    (is (= ["W" "orld"]
           (core/try-decode h-w [\W \o \r \l \d])))
    (is (= [\W \o \r \l \d]
           (core/encode h-w ["W" "orld"])))))
