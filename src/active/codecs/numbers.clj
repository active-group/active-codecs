(ns active.codecs.numbers
  "Decode and encode sequences of bytes into numbers."
  (:require [active.codecs.core :as core]))

(def ^{:doc "Codec of 8 bit integer in two's complement form."} int8-2c
  core/singleton)

(defn- to-unsinged [value lim h-lim]
  ;; lim = 256 for 8 bit... 1 << 8
  ;; h-lim = 128 for 8 bit... 1 << 7
  (assert (>= value (- h-lim)) value)
  (assert (< value h-lim) value)
  
  (let [res (if (< value 0)
              (+ value lim)
              value)]
    (assert (>= res 0) res)
    (assert (< res lim) res)
    res))

(defn- to-signed [value lim h-lim]
  ;; lim = 256 for 8 bit... 1 << 8
  ;; h-lim = 128 for 8 bit... 1 << 7
  (assert (>= value 0) value)
  (assert (< value lim) value)
  
  (let [res (if (>= value h-lim)
              (- value lim)
              value)]
    (assert (< res h-lim) res)
    (assert (>= res (- h-lim)) res)
    res))

(defn- signed-2c->unsigned [c bits]
  ;; Note: might not work for 64 bits due to the JVM numbers
  (core/translate c
                  to-unsinged
                  to-signed
                  (bit-shift-left 1 bits)
                  (bit-shift-left 1 (dec bits))))

(defn- unsigned->signed-2c [c bits]
  ;; Note: might not work for 64 bits due to the JVM numbers
  (core/translate c
                  to-signed
                  to-unsinged
                  (bit-shift-left 1 bits)
                  (bit-shift-left 1 (dec bits))))

(def ^{:doc "Codec of an 8 bit unsigned integer."} uint8 (unsigned->signed-2c int8-2c 8))

(defn- split-high-low [value n bits]
  (let [p (- (bit-shift-left 1 (inc bits)) 1)]
    (map (fn [b]
           (bit-and (bit-shift-right value b) p))
         (reverse (range n)))))

(defn- join-high-low [parts n bits]
  (assert (= n (count parts)) (str "Expected " n " codes to decode, but got " (pr-str parts)))
  (reduce + (map (fn [p v]
                   (bit-shift-left v (* p bits)))
                 (range n)
                 (reverse parts))))

(defn- uint-shift [c n bits]
  (core/translate c
                  split-high-low
                  join-high-low
                  n bits))

(def ^{:doc "Codec of a big-endian 16 bit unsigned integer."} uint16-be
  (-> (core/seq uint8 uint8)
      (uint-shift 2 8)))

(def ^{:doc "Codec of a big-endian 24 bit unsigned integer."} uint24-be
  (-> (core/seq uint8 uint8 uint8)
      (uint-shift 3 8)))

(def ^{:doc "Codec of a big-endian 32 bit unsigned integer."} uint32-be
  (-> (core/seq uint8 uint8 uint8 uint8)
      (uint-shift 4 8)))

(def ^{:doc "Codec of a big-endian 16 bit signed integer in two's complement form."} int16-be-2c
  (signed-2c->unsigned uint16-be 16))

(def ^{:doc "Codec of a big-endian 24 bit signed integer in two's complement form."} int24-be-2c
  (signed-2c->unsigned uint24-be 24))

(def ^{:doc "Codec of a big-endian 32 bit signed integer in two's complement form."} int32-be-2c
  (signed-2c->unsigned uint32-be 32))

(def ^{:doc "An alias of [[uint16-be]]."} uint16 uint16-be)
(def ^{:doc "An alias of [[uint24-be]]."} uint24 uint24-be)
(def ^{:doc "An alias of [[uint32-be]]."} uint32 uint32-be)

(def ^{:doc "An alias of [[int8-2c]]."} int8 int8-2c)
(def ^{:doc "An alias of [[int16-be-2c]]."} int16 int16-be-2c)
(def ^{:doc "An alias of [[int24-be-2c]]."} int24 int24-be-2c)
(def ^{:doc "An alias of [[int32-be-2c]]."} int32 int32-be-2c)

(defn i-mul [a b] (long (* a b)))

(defn factor-decimal
  "Returns a codec based on the integer codec `c`, but for decimal
  values based on multiplicatin/division by the given factor."
  [c factor]
  (core/translate c i-mul / factor))
