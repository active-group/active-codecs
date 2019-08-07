(ns active.codecs.core
  (:require [clojure.set :as set])
  (:refer-clojure :exclude [seq repeat take concat]))

(defprotocol Encoder
  (-encode [this value] "Must return a sequence of codes representing the given value."))

(defn decoder-error [msg & [data]]
  (ex-info msg (or data {})))

(defn decoder-error? [v]
  (instance? Exception v))

(defprotocol Decoder
  (-decode [this codes] "Must return [value rest-codes] or a decoder-error"))

(defn encoder?
  "Returns if `v` is an encoder."
  [v]
  (satisfies? Encoder v))

(defn decoder?
  "Returns if `v` is a decoder."
  [v]
  (satisfies? Decoder v))

(defn codec?
  "Returns if `v` is a codec - both an encoder and a decoder."
  [v]
  (and (decoder? v)
       (encoder? v)))

(defn encode
  "Encode with given `value` according to the given `codec`, returning
  a sequence of codes."
  [codec value]
  (-encode codec value))

(defn decode
  "Decodes the given sequence of `codes` according to the given
  `codec`, returning a [[decoder-error?]] or a tuple of `[value
  rest-codes]`."
  [codec codes]
  (-decode codec codes))

(defn decode-full
  "Decodes the given sequence of `codes` according to the given
  `codec`, returning the decoded value. Throws if there are unread
  codes at the end, or too little codes for the codec, or any other
  problem."
  [codec codes]
  (let [res (-decode codec codes)]
    (if (decoder-error? res)
      (throw res)
      (let [[value rest] res]
        (when-not (empty? rest)
          (throw (decoder-error "Trailing codes after value.")))
        value))))

(defn try-decode
  "Tries to decodes the given sequence of `codes` according to the
  given `codec`, returning the decoded value if successfull. Returns
  `error-return`, which defaults to nil, if there are unread codes at
  the end, or too little codes for the codec, or any other problem."
  [codec codes & [error-return]]
  (let [res (-decode codec codes)]
    (if (decoder-error? res)
      error-return
      (let [[value rest] res]
        (if-not (empty? rest)
          error-return
          value)))))

;; TODO: decode-all decode-first ? encode-all?

(defrecord ^:private SimpleCodec [enc dec args]
  Encoder (-encode [this value] (apply enc value args))
  Decoder (-decode [this codes] (apply dec codes args)))

(defn simple-codec
  "Creates a simple codec based on functions `(apply enc value args)`
  to encode, and `(apply dec codes args)` to decode."
  [enc dec & args]
  (SimpleCodec. enc dec args))

(defrecord ^:private SimpleEncoder [enc args]
  Encoder (-encode [this value] (apply enc value args)))

(defn simple-encoder
  "Creates a simple encoder based on `(apply enc value args)` to
  encode."
  [enc & args]
  (SimpleEncoder. enc args))

(defrecord ^:private SimpleDecoder [dec args]
  Decoder (-decode [this codes] (apply dec codes args)))

(defn simple-decoder
  "Creates a simple decoder based on `(apply dec value args)` to
  encode."
  [dec & args]
  (SimpleDecoder. dec args))

(defn- c-switch [c enc dec & args]
  (cond
    (codec? c) (apply simple-codec enc dec args)
    (encoder? c) (apply simple-encoder enc args)
    (decoder? c) (apply simple-decoder dec args)
    :else (throw (ex-info "Not a codec." {:value c}))))

(defn ^:no-doc dec-bind [v f]
  (if (decoder-error? v)
    v
    (f (first v) (second v))))

(defn- translate-enc [value c f g args]
  (encode c (apply f value args)))

(defn- translate-dec [codes c f g args]
  (dec-bind (decode c codes)
            (fn [v rest]
              [(apply g v args) rest])))

(defn translate
  "Returns a codec like c, but the value encoded by c is `(apply f
  value args)` instead, and the decoded value of the returned value
  is `(apply g value args)` instead."
  [c enc dec & args]
  (assert (codec? c))
  (simple-codec translate-enc translate-dec c enc dec args))

(defn- seq-enc [values cs]
  (assert (= (count values) (count cs)) (str "Expected " (count cs) " values, but got " (count values)))
  (mapcat #(encode %1 %2)
          cs values))

(defn- seq-dec [codes cs]
  (loop [res []
         codes codes
         cs cs]
    (if (empty? cs)
      [res codes]
      (if (empty? codes)
        (decoder-error (str "Premature end of value sequence. Expected " (count cs) " more.") {:codecs cs})
        (let [res1 (decode (first cs) codes)]
          (if (decoder-error? res1)
            res1
            (let [[v rest-codes] res1]
              (recur (conj res v)
                     rest-codes
                     (rest cs)))))))))

(defn seq
  "Returns a codec for a sequence of values, given codecs for each. The codes are concatenated into one."
  [c & cs]
  (let [cs (cons c cs)]
    (cond
      (every? codec? cs) (simple-codec seq-enc seq-dec cs)
      (every? encoder? cs) (simple-encoder seq-enc cs)
      (every? decoder? cs) (simple-decoder seq-dec cs)
      :else (throw (ex-info "Not composable." {:cs cs})))))


(defn- cond-enc [values c f args]
  (let [[v1 v2] values ;; TODO: throw if not a tuple?
        c2 (apply f v1 args)]
    (clojure.core/concat (encode c v1)
                         (encode c2 v2))))

(defn- cond-dec [codes c f args]
  (dec-bind (decode c codes)
            (fn [v1 rest]
              (dec-bind (decode (apply f v1 args) rest)
                        (fn [v2 rest]
                          [(list v1 v2) rest])))))

(defn conditional
  "Returns a codec for a tuple of values, where the codec for the
  first one is `c` and for the second it's `(apply f v args)` where `v` is
  the first value."
  [c f & args]
  (c-switch c cond-enc cond-dec c f args))

#_(defn- repeat-enc [values c]
  
  )

#_(defn- repeat-dec [codes c]
  (loop [codes codes
         res []]
    (if (empty? codes)
      [res codes]
      (dec-bind (decode codes c)
                (fn [v rest]
                  (recur )))
      ))
  )

(defn repeat "Returns a codec that is a [[seq]] of `n` times the same codec repeated."
  #_([c] (c-switch c repeat-enc repeat-dec c))
  ([n c] (apply seq (clojure.core/repeat n c))))

(defn- length-prefix-add-length [coll]
  [(count coll) coll])

(defn- length-prefix-remove-length [[len coll]]
  coll)

(defn length-prefix
  "Returns a codec for a sequence of `coll-item-codec` of any length,
  where the length is encoded using `length-codec` and placed before
  the encoding of the collection items."
  [length-codec coll-item-codec]
  (-> (conditional length-codec repeat coll-item-codec)
      (translate length-prefix-add-length
                 length-prefix-remove-length)))

#_(defn- take-enc [values n]
  (assert (= n (count values)))
  values)

#_(defn- take-dec [codes n]
  [(clojure.core/take n codes) (clojure.core/drop n codes)])

#_(defn take [n]
    (simple-codec take-enc take-dec n))

(defn- singleton-dec [codes]
  [(first codes) (rest codes)])

(def ^{:doc "The codec for the value representing one item of the code
  itself. This is also some kind of 'identity' codec."}
  singleton
  (simple-codec list singleton-dec))

(defn- record-enc [value keys]
  (assert (map? value) (str "Unexpected value to encode as a record, must be a map: " (pr-str value)))
  (assert (every? #(contains? value %) keys) (str "Value to encode misses some record fields: " (pr-str (set/difference (set (clojure.core/keys value)) (set keys)))))
  (map #(get value %) keys))

(defn- record-dec [value keys]
  (assert (= (count value) (count keys)) (str "Unexpected value to decode as a record, must be a sequence of " (count keys) " item: " (pr-str value)))
  (zipmap keys value))

(defn record "Returns a codec for some named elements with different
  codecs. The encoded value should be a map of the given keys, and the
  code is a concatenation of the encodings of each item. E.g. the
  names are not encoded. Example:

  (record :a int8 :b uint32)"
  [key c & more]
  (let [m (partition 2 (cons key (cons c more)))]
    (-> (apply seq (map second m))
        (translate record-enc record-dec (map first m)))))
