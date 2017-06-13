;; http://en.wikipedia.org/wiki/Luhn_algorithm
;; for checking credit card numbers

;; much later blog post, but also covers Luhn
;; https://garajeando.blogspot.com/2017/06/kata-luhn-test-in-clojure.html

;; Good candidate for a spec

;; By the way, credit card numbers are allowed to start with 0 so you can't store them
;; directly as longs.  Also, technically they're allowed to have 19 decimal digits, which
;; can be too big for a long.  The basic type for a credit card 'number' is more really a
;; String.  For processing, it's convenient to convert to a vector of integer digits, which
;; preserves the leading zeroes.



(ns miner.luhn
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]))




;; official definition, just for testing
(defn x2calc [n]
  {:pre [(<= 0 n 9)]}
  (let [n2 (* 2 n)]
    (if (>= n2 10)
      (+ (mod n2 10) (quot n2 10))
      n2)))

;; notice that the (mod n2 10) is always 1, but we can optimize further...

;; easy enough to just look up
(defn x2g [n]
  {:pre [(<= 0 n 9)]}
  (get [0 2 4 6 8 1 3 5 7 9] n))

;; slightly faster, but not as pretty
(defn x2 [n]
  (case (int n)
    0 0
    1 2
    2 4
    3 6
    4 8
    5 1
    6 3
    7 5
    8 7
    9 9))

    
;; using int is slightly faster than long
(defn digit [ch]
  (- (int ch) (int \0)))

;; slightly faster if digits is a vector, but any seq is OK
(defn checksum-digits [digits]
  ;; pad with a leading 0 to get even count
  (let [digits (if (odd? (count digits)) (cons 0 digits) (seq digits))]
    (mod (+ (reduce + (map x2 (take-nth 2 digits)))
            (reduce + (take-nth 2 (rest digits))))
         10)))

;; allows space and hyphens in a string for readability
(defn clean-card-string [s]
  (when (re-matches #"[-0-9 ]+" s)
    (str/replace s #"[- ]" "")))

(defn checksum [numstr]
  (checksum-digits (mapv digit numstr)))

(defn check? [numstr]
  (zero? (checksum numstr)))

;; pads string with leading zeros
;; assumes no more that 20 pad zeroes (pretty safe)
(defn strnum
  ([num] (str num))
  ([num digit-count]
   (let [numstr (str num)
         pad (- digit-count (count numstr))]
     (if (pos? pad)
       (str (subs "00000000000000000000" 0 pad) numstr)
       numstr))))


(defn safe-check? [x]
  (if-let [cc (clean-card-string (str x))]
    (check? cc)
    false))

(defn amex-start? [^String s]
  (and (or (.startsWith s "37") (.startsWith s "34"))
       (== (.length s) 15)))

(defn mastercard-start? [^String s]
  (and (.startsWith s "5") (== (.length s) 16)))

(defn visa-start? [^String s]
  (and (.startsWith s "4")
       (or (= (.length s) 13) (= (.length s) 16))))

;; more on card types and issuers
;; http://en.wikipedia.org/wiki/Bank_card_number
;; http://stackoverflow.com/questions/72768/how-do-you-detect-credit-card-type-based-on-number

;; for now using the CS50x instructions
(defn card-type [num]
  (if-not (check? num)
    :invalid
    (let [s (str num)]
      (cond (amex-start? s) :amex
            (visa-start? s) :visa
            (mastercard-start? s) :mastercard
            :else :valid))))


;; first attempt, but I like the second one better
(defn gen-card1
  ([num-digits] (gen-card1 0 num-digits))
  ([start num-digits]
     (let [bs (if (zero? start) () (map digit (str start)))
           rs (repeatedly (- num-digits (count bs) 1) #(rand-int 10))
           ds (concat bs rs '(0))
           chk (checksum-digits ds)]
       (reduce (fn [acc d] (+ (* 10 acc) d)) 0
               (if (zero? chk) ds (concat bs rs (list (- 10 chk))))))))

;; We originally thought credit cards could not start with a 0, but they actually can.  So
;; we should always use strings as the base type.  It's OK to convert to a vector of digits
;; as long as the leading zeroes are preserved.

;; Credit cards typically are 15 or 16 digits, but they can be 13 to 19 digits.
;; Unfortunately, 10^19 is too big for a long so we restrict to 18 digits to be safe.
;; Maybe this is wrong since we decided to use strings so it should be easy to handle 19
;; digits.



(defn gen-card
  ([] (gen-card nil (+ 13 (rand-int 6))))
  ([num-digits] (gen-card nil num-digits))
  ([start num-digits]
   (let [bv (if start (mapv digit (str start)) [])
         dvx (into bv (repeatedly (- num-digits (inc (count bv))) #(rand-int 10)))
         dv0 (conj dvx 0)
         chk (checksum-digits dv0)]
     (apply str (if (zero? chk) dv0 (conj dvx (- 10 chk)))))))



;; had a bug when chk was 0  found with generative testing
        
;; http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm
;; but there's a typo for 76009244561 entry.  I changed it to 76009244567.
(def test-data
  '{6331101999990016 Switch-Solo-Paymentech,
    38520000023237 Diners-Club,
    378282246310005 American-Express,
    5610591081018250 Australian-BankCard,
    6011000990139424 Discover,
    371449635398431 American-Express,
    30569309025904 Diners-Club,
    378734493671000 American-Express-Corporate,
    4111111111111111 Visa,
    4012888888881881 Visa,
    76009244567 Dankort-PBS,
    5019717010103742 Dankort-PBS,
    5105105105105100 MasterCard,
    5555555555554444 MasterCard,
    6011111111111117 Discover,
    3530111333300000 JCB,
    3566002020360505 JCB,
    4222222222222 Visa})




(defn mask-credit-card [num-or-str]
  (if (check? num-or-str)
    (str/replace (str num-or-str) #"[0-9]" "X")
    num-or-str))


(comment
  (require '[miner.luhn :as lu])

  (every? lu/check? (keys lu/test-data))

  (map #(conj % (lu/card-type (first %))) lu/test-data)
)  


;; https://garajeando.blogspot.com/2017/06/kata-luhn-test-in-clojure.html
;; much slower, but useful for testing

(def sum-digits #(+ (quot % 10) (mod % 10)))

(defn double-when-at-even-position [position num]
  (if (even? (inc position)) (* 2 num) num))

(defn reduce-digits [digits]
  (->> digits
       (reverse)
       (map #(Integer/parseInt (str %)))
       (map-indexed double-when-at-even-position)
       (map sum-digits)
       (apply +)))

(defn valid? [digits]
  (zero? (mod (reduce-digits digits) 10)))

;; to match my check?
(defn gara? [n]
  (valid? (map digit (str n))))


;; refactored from Gara examples
(defn gara-test
  ([] (gara-test gara?))
  ([testfn]
   (and (every? testfn ["00000000000"
                        "00000000505"
                        "00000000018"
                        "00000002030"
                        "00000000091"
                        "49927398716"
                        "79927398713"])
        (not-any? testfn ["00000000001"
                          "49927398712"
                          "79927398715"]))))




;; We want the credit card num to fit in a long, so we don't go more than 18 digits.
;; Typically, they're 15 or 16.  Some foreign cards are 13 to 19.  But 10^19 is beyond Long.

(s/def ::credit-card1 (s/with-gen check? (fn [] (g/fmap #(gen-card %) (g/choose 13 18)))))

;; better gen is more reproducible and shrinkable, needs to combine tc generators
;; Generate num form of N+C
;; N 0-9
;; C checksum of N+0

(def card-generator (gen/let [cnt (g/choose 12 18)
                              dv (g/vector (g/choose 0 9) cnt)]
                      (let [dv0 (conj dv 0)
                            chk (checksum-digits dv0)]
                        (apply str (if (zero? chk) dv0 (conj dv (- 10 chk)))))))

;; About twice as fast but only up to 18 digits because 10^19 is too big.
;; Actually, there's some dangerous imprecision in converting doubles to longs around 2^53
;; or approx 16 decimal digits.  Lowered generation to 13 to 16 digits to be safe.
(def cg2 (gen/let [cnt (g/choose 12 15)
                   num0 (let [high (dec (long (Math/pow 10 cnt)))]
                          (gen/fmap #(* 10 %) (gen/choose 0 high)))]
           (let [numstr (strnum num0 (inc cnt))
                 chk (checksum numstr)]
             (strnum (if (zero? chk) num0 (+ num0 (- 10 chk))) (inc cnt)))))


(s/def ::credit-card
  (s/with-gen check? (fn [] card-generator)))





;; not so useful
;; (s/def ::credit-card-vector (s/and (s/vector (s/choose 1 9) :into []) check-digits?))
