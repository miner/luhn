;; http://en.wikipedia.org/wiki/Luhn_algorithm
;; for checking credit card numbers

;; much later blog post, but also covers Luhn
;; https://garajeando.blogspot.com/2017/06/kata-luhn-test-in-clojure.html

;; See also "Luhny Bin Challenge"
;; [old] http://corner.squareup.com/2011/11/luhny-bin.html
;; [current] https://medium.com/square-corner-blog/coding-challenge-the-luhny-bin-28b43b3942c2

;; Good candidate for a spec.  More below.

;; By the way, credit card numbers are allowed to start with 0 so you can't store them
;; directly as longs.  Also, technically they're allowed to have 19 decimal digits, which
;; can be too big for a long.  The basic type for a credit card 'number' is essentially a
;; String.  For processing, it's convenient to convert to a vector of integer digits, which
;; preserves the leading zeroes.

;; SEM: Had an idea about storing as long... if there are leading zeros, make it negative
;; with prefix -1.  So 001234 becomes -1001234.  Or could store number of digits in high
;; byte.  Probably not worth it, but maybe.


(ns miner.luhn
  (:require [clojure.test.check.generators :as gen]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]))



;; by @clojuregrams on twitter
;; simple but slow
(defn cg-luhn? [cc]
 (let [factors (cycle [1 2])
       numbers (map #(Character/digit % 10) cc)
       sum (->> (map * (reverse numbers) factors)
            (map #(+ (quot % 10) (mod % 10)))
            (reduce +))]
   (zero? (mod sum 10))))


;; much faster with ^Character hint.  Also, mapv/rseq and rem.
(defn cg-luhn2? [cc]
 (let [factors (cycle [1 2])
       numbers (mapv #(Character/digit ^Character % 10) cc)
       sum (->> (map * (rseq numbers) factors)
            (map #(+ (quot % 10) (rem % 10)))
            (reduce +))]
   (zero? (mod sum 10))))



;; NOTE: rem is slightly faster than mod, but beware neg numbers 

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


;; slightly faster than Java Character/digit

;; using int is slightly faster than long
(defn digit [ch]
  (- (int ch) (int \0)))



(defn cdigit [ch]
  (case ch
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9))

(defn cx2 [ch]
  (case ch
    \0 0
    \1 2
    \2 4
    \3 6
    \4 8
    \5 1
    \6 3
    \7 5
    \8 7
    \9 9))



;; fastest but slightly ugly loop
;; digits must be a vector
(defn checksum-digits [digits]
  (let [cnt (count digits)]
    (loop [i (dec cnt) sum (if (odd? cnt) (digits 0) 0)]
      ;; stepping by 2
      (if (pos? i)
        (recur (- i 2)   (long (+ sum (digits i) (x2 (digits (dec i))))))
        (rem sum 10)))))





;; ALMOST
(defn tchk? [numstr]
  (let [len (.length ^String numstr)
        cs (seq numstr)]
    (transduce (map-indexed (fn [i c] (if (even? i) (cx2 c) (cdigit c))))
             (completing + #(zero? (rem % 10)))
             (if (odd? len) (cdigit (first cs)) 0)
             (if (odd? len) (rest cs) cs))))



;; use alternating sign of R to indicate conversion toggle
;; offset by 10 to avoid unsigned 0, mag always increases
;; surprisingly good, but not quite faster than loop

(defn rchk? [numstr]
  (zero? (rem (reduce (fn [r c] (if (neg? r) (- (cx2 c) r)  (- (+ r (cdigit c)))))
                      (if (even? (.length ^String numstr)) -10 10)
                      (seq numstr))
              10)))




;; VERY FASTEST, char-array and ch conversions, skipping digit
(defn fast-check? [numstr]
  (let [len (.length ^String numstr)
        ca (char-array numstr)]
    (loop [i (dec len) sum (if (odd? len) (cdigit (aget ca 0)) 0)]
      ;; stepping by 2
      (if (pos? i)
        (recur (- i 2)   (long (+ sum (cdigit (aget ca i)) (cx2 (aget ca (dec i))))))
        (zero? (rem sum 10))))))


;; only slight slower with vec
(defn vchk1? [numstr]
  (let [cv (vec numstr)
        len (count cv)]
    (loop [i (dec len) sum (if (odd? len) (cdigit (cv 0)) 0)]
      ;; stepping by 2
      (if (pos? i)
        (recur (- i 2)   (long (+ sum (cdigit (cv i)) (cx2 (cv (dec i))))))
        (zero? (mod sum 10))))))


(defn vchecksum [numstr]
  (let [cv (vec numstr)
        len (count cv)]
    (loop [i (dec len) sum (if (odd? len) (cdigit (cv 0)) 0)]
      ;; stepping by 2
      (if (pos? i)
        (recur (- i 2)   (long (+ sum (cdigit (cv i)) (cx2 (cv (dec i))))))
        (mod sum 10)))))

(defn vchk? [numstr]
  (zero? (vchecksum numstr)))

;; https://www.rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Clojure
;; SEM added type hint, but still slow (factor of 10x slower than fast-check?)
(defn rosetta-luhn? [cc]
  (let [factors (cycle [1 2])
        numbers (map #(Character/digit ^Character % 10) cc)
        sum (reduce + (map #(+ (quot % 10) (mod % 10))
                           (map * (reverse numbers) factors)))]
    (zero? (mod sum 10))))


;; 
#_ (defn checksum-digitsGOOD [digits]
  (let [dub? (if (odd? (count digits)) odd? even?)]
    (mod (reduce + (map-indexed (fn [i x] (if (dub? i) (x2 x) x)) digits))
         10)))

;; old version, slower
#_ (defn checksum-digits1 [digits]
  ;; pad with a leading 0 to get even count
  (let [digits (if (odd? (count digits)) (cons 0 digits) (seq digits))]
    (mod (+ (reduce + (map x2 (take-nth 2 digits)))
            (reduce + (take-nth 2 (rest digits))))
         10)))


;; about 800 ms / 10000
;; but not fastest
#_ (defn checksum-digitsFAST [digits]
  (let [sum (if (odd? (count digits))
              (reduce-kv (fn [res i x] (if (odd? i) (+ res (x2 x)) (+ res x))) 0 digits)
              (reduce-kv (fn [res i x] (if (even? i) (+ res (x2 x)) (+ res x))) 0 digits))]
    (mod sum 10)))

(defn kvchk? [numstr]
  (let [cv (vec numstr)
        sum (if (odd? (count cv))
              (reduce-kv (fn [res i x] (if (odd? i) (+ res (cx2 x)) (+ res (cdigit x)))) 0 cv)
              (reduce-kv (fn [res i x] (if (even? i) (+ res (cx2 x)) (+ res (cdigit x)))) 0 cv))]
    (zero? (mod sum 10))))

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



;; Related to "Luhny Bin Challenge".  However, I didn't write a solution in their project format.
;; https://github.com/square/luhnybin
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
