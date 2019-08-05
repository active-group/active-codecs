(ns active.codecs.numbers-test
  (:require [active.codecs.core :as core]
            [active.codecs.numbers :as numbers]
            [clojure.test :refer :all]))

(def b00000000 (byte 0))
(def b01111111 (byte 127))
(def b11111111 (byte -1))
(def b10000000 (byte -128))

(deftest uint8-test
  (is (= 0
         (core/decode-full numbers/uint8 [b00000000])))
  (is (= 127
         (core/decode-full numbers/uint8 [b01111111])))
  (is (= 128
         (core/decode-full numbers/uint8 [b10000000])))
  (is (= 255
         (core/decode-full numbers/uint8 [b11111111]))))

(deftest uint24-be-test
  (is (= 0
         (core/decode-full numbers/uint24-be [b00000000 b00000000 b00000000])))
  (is (= (- (bit-shift-left 1 23) 1)
         (core/decode-full numbers/uint24-be [b01111111 b11111111 b11111111])))
  (is (= (bit-shift-left 1 23)
         (core/decode-full numbers/uint24-be [b10000000 b00000000 b00000000])))
  (is (= (- (bit-shift-left 1 24) 1)
         (core/decode-full numbers/uint24-be [b11111111 b11111111 b11111111]))))

(deftest int24-be-2c-test
  (is (= 0
         (core/decode-full numbers/int24-be-2c [b00000000 b00000000 b00000000])))
  (is (= (- (bit-shift-left 1 23) 1)
         (core/decode-full numbers/int24-be-2c [b01111111 b11111111 b11111111])))
  (is (= (- (bit-shift-left 1 23))
         (core/decode-full numbers/int24-be-2c [b10000000 b00000000 b00000000])))
  (is (= -1
         (core/decode-full numbers/int24-be-2c [b11111111 b11111111 b11111111]))))
