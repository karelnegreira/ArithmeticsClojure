(require '[clojure.contrib.math :as tower])
(require '[criterium.core :as c])

(defn divisible [n d]
  (zero? (mod n d)))

(divisible 7 2)

;the following function defines if the parameter is prime or if is not. Is a very slow and nonoptimized algorithm.
(defn brute-force-primes? [n]
(not-any? (partial divisible n) (range 2 n)))

;(brute-force-primes 21)

;It ranges from 1 to 1000 and prints all primes. Again is a very slow algorithm
;(time (doall (filter brute-force-primes? (range 1 1000))))


(defn isqrt
  "floor(√n). When incremented, provides an upper bound for factorization."
  ;; Java interop is super fast but not accurate for n > 1E24 (approx) due to
  ;; floating-point rounding. Uses a slightly slower but pinpoint-precise method for n > 1E24.
  [n]
  (if (< n 1E24)
    (-> (Math/sqrt n) bigint)
    ;; https://cs.stackexchange.com/a/30383
    (let [half-bit-length (quot (.bitLength (bigint n)) 2)]
      (loop [a (Math/pow 2 half-bit-length)
             b a
             c (*' a a)]
        (cond
          (zero? b) a
          (> c n)   (recur (-' a b) (quot b 2) (+ c (*' -2 a b) (*' b b)))
          :else     (recur (+' a b) (quot b 2) (+ c (*' 2 a b) (*' b b))))))))



          (defn naive-prime? [n]
  (cond
  (<= n 1)  false
  (= n 2)   true
  (even? n) false ;; Will also weed out non-integers
  :else     (not-any? #(divisible n %) (range 3 (inc (isqrt n)) 2))))

(def prime? (memoize naive-prime?))


(naive-prime? 45565962173)

(
  loop [n 125 primes (filter prime? (range)) factors []]
  (let [k (first primes)]
  (cond (= 1 n) factors 
  (divisible n k) (recur (/ n k) primes (conj factors k))
  :else (recur n (rest primes) factors))))


(defn factors [n]
  (->> (range 1 (inc (isqrt n)))
       (filter #(divisible n %))
       (mapcat (fn [d] [d (/ n d)]))
       (into (sorted-set))))
  
  (defn prime-factorization [n] 
  (when (and (integer? n)(pos? n))
  (loop [n n primes (if (> n 1000000)
  (filter prime? (factors n))
  (filter prime? (range)))
  factors[]]
  (let [k (first primes)]
  (cond 
  (= 1 n) factors
  (divisible n k) (recur (/ n k) primes (conj factors k))
  :else (recur n (rest primes ) factors))))))

  (prime-factorization 125)

(count (prime-factorization 125))


(defn prime-power-integer [n]
(when (and (integer? n)(pos? n))
(if (= n 1) [0]
(let [pf (prime-factorization n)
primes (filter prime? (range (inc (peek pf))))
freqs (frequencies pf)
get-exp(fn [p](if-let[exp (get freqs p)]exp 0))]
(map get-exp primes)))))

(prime-power-integer 168)



(defn prime-powers->num[pp] (let [primes (filter prime? (range))]
(reduce *' (map tower/expt primes pp))))

(prime-powers->num [3 1 0 1])