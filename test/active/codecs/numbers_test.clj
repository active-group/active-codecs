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
         (core/decode-full numbers/uint24-be [b11111111 b11111111 b11111111])))

  ;; 66051
  (is (= [b00000000 b00000000 b00000000]
         (core/encode numbers/uint24-be 0)))
  (is (= [b01111111 b11111111 b11111111]
         (core/encode numbers/uint24-be (- (bit-shift-left 1 23) 1))))
  (is (= [b10000000 b00000000 b00000000]
         (core/encode numbers/uint24-be (bit-shift-left 1 23))))
  (is (= [b11111111 b11111111 b11111111] 
         (core/encode numbers/uint24-be (- (bit-shift-left 1 24) 1))))
  )

(deftest int24-be-2c-test
  (is (= 0
         (core/decode-full numbers/int24-be-2c [b00000000 b00000000 b00000000])))
  (is (= (- (bit-shift-left 1 23) 1)
         (core/decode-full numbers/int24-be-2c [b01111111 b11111111 b11111111])))
  (is (= (- (bit-shift-left 1 23))
         (core/decode-full numbers/int24-be-2c [b10000000 b00000000 b00000000])))
  (is (= -1
         (core/decode-full numbers/int24-be-2c [b11111111 b11111111 b11111111])))

  (is (= [b00000000 b00000000 b00000000]
         (core/encode numbers/int24-be-2c 0)))
  (is (= [b01111111 b11111111 b11111111]
         (core/encode numbers/int24-be-2c (- (bit-shift-left 1 23) 1))))
  (is (= [b10000000 b00000000 b00000000]
         (core/encode numbers/int24-be-2c (- (bit-shift-left 1 23)))))
  (is (= [b11111111 b11111111 b11111111]
         (core/encode numbers/int24-be-2c -1)))
  )

(deftest factor-decimal-test
  (let [c (numbers/factor-decimal numbers/int8 10)]
    (is (= 21/5 (core/decode-full c [42])))
    (is (= [42] (core/encode c 4.2)))))
