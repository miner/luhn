(ns miner.test-luhn
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [miner.luhn :refer :all]))

(deftest round-trip
  (is (every? check? (repeatedly 1000 gen-card)))
  (is (every? check? (gen/sample card-generator 1000))))


;; wrapper for ARE macro
(defmacro every-is [pred args]
  `(clojure.test/are [x#] (~pred x#) ~@args))


(deftest gara-expand []
  (every-is check? ["00000000000" "00000000505" "00000000018" "00000002030" "00000000091"
                    "49927398716" "79927398713"])
  (every-is gara? ["00000000000" "00000000505" "00000000018" "00000002030" "00000000091"
                   "49927398716" "79927398713"])
  (let [not-check? (complement check?)
        not-gara? (complement gara?)]
    (every-is not-check? ["00000000001" "49927398712" "79927398715"])
    (every-is not-gara? ["00000000001" "49927398712" "79927398715"])))
         
