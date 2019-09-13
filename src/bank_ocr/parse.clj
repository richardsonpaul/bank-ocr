(ns bank-ocr.parse "Parsing for an ocr number")

;; Each entry is 4 lines long, and each line has 27 characters. The first 3
;; lines of each entry contain an account number written using pipes and
;; underscores, and the fourth line is blank. Each account number should have 9
;; digits, all of which should be in the range 0-9.

;; character indices in an ocr number
;; 0 1 2
;; 3 4 5
;; 6 7 8
;; invariants:
;; 0 & 2 should always be blank
;; the edges (3 5 6 9) will be a pipe '|' or blank
;; (1 4 7) will be an underscore '_' or blank

(def zero [1 3 5 6 7 8])
(def one [5 8])
(def two [1 4 5 6 7])
(def three [1 4 5 7 8])
(def four [3 4 5 8])
(def five [1 3 4 7 8])
(def six [1 3 4 6 7 8])
(def seven [1 5 8])
(def eight [1 3 4 5 6 7 8])
(def nine [1 3 4 5 7 8])

(def nums [zero one two three four five six seven eight nine])
(defn- ->name [num]
  (condp = num
    zero :zero
    one :one
    two :two
    three :three
    four :four
    five :five
    six :six
    seven :seven
    eight :eight
    nine :nine))

;; let's do a binary search; I'm going to use `frequencies` to analyze how often each 'pixel'
;;; falls in each number; then I can divide them into even-ish groups
;; (frequencies (flatten nums)) => {1 8, 3 6, 5 8, 6 4, 7 7, 8 9, 4 7}
;;; the best bet is to use the '3' pixel (which six numbers have) or '6' (which four numbers have)

;; using things like this in the repl (clojure.set/difference (set nine) (set four)) => #{7 1}
;;; I have come up with a parse tree that looks like this:
;;         has? 3
;;            /   \
;;       yes /      \ no
;;          /         \
;;         6           4
;;       /   \        /  \
;;      5      5      6    1
;;     / \    / \    / \  / \
;;    4 {6}  1 {5} {2}{3}{7}{1}
;;   / \    /  \
;; {8} {0} {9} {4}

(defn- has-1? [n] (some #{1} n))
(defn- has-3? [n] (some #{3} n))
(defn- has-4? [n] (some #{4} n))
(defn- has-5? [n] (some #{5} n))
(defn- has-6? [n] (some #{6} n))
(defn- !has-1? [n] (not (has-1? n)))
(defn- !has-3? [n] (not (has-3? n)))
(defn- !has-4? [n] (not (has-4? n)))
(defn- !has-5? [n] (not (has-5? n)))
(defn- !has-6? [n] (not (has-6? n)))

;; list of predicate functions describing a path down the tree. The final entry is the
;; number description to check against
(def ^:private pred-0? [has-3? has-6? has-5? !has-4? 0])
(def ^:private pred-1? [!has-3? !has-4? !has-1? 1])
(def ^:private pred-2? [!has-3? has-4? has-6? 2])
(def ^:private pred-3? [!has-3? has-4? !has-6? 3])
(def ^:private pred-4? [has-3? !has-6? has-5? !has-1? 4])
(def ^:private pred-5? [has-3? !has-6? !has-5? 5])
(def ^:private pred-6? [has-3? has-6? !has-5? 6])
(def ^:private pred-7? [!has-3? !has-4? has-1? 7])
(def ^:private pred-8? [has-3? has-6? has-5? has-4? 8])
(def ^:private pred-9? [has-3? !has-6? has-5? has-1? 9])

(def ^:private pred->numeral {pred-0? 0
                              pred-1? 1
                              pred-2? 2
                              pred-3? 3
                              pred-4? 4
                              pred-5? 5
                              pred-6? 6
                              pred-7? 7
                              pred-8? 8
                              pred-9? 9})

(defn- desc->numeral
  "take a description of active 'pixels' and a list of predicates"
  [desc preds]
  (let [groups (group-by first preds)
        next-group? (fn [[pred next-groups]] (when (pred desc) (map rest next-groups)))
        next-pred-list (some next-group? groups)]
    (when (seq next-pred-list)
      (if (= (count next-pred-list) 1)
        (first next-pred-list)
        (desc->numeral desc next-pred-list)))))

(defn ->numeral [desc]
  (let [numeral (desc->numeral desc (keys pred->numeral))]
    (when (seq numeral) (first numeral))))

(defn lines->desc
  "lines should be a 3-line string description of a number in ocr format"
  [lines]
  ;; take 3 chars off each line, and append them all into a string
  ;; then check for the presence of non-blank (remove 0 and 2 in any case as that would be a mis-read of the scanner
  (let [active-pixels (fn [characters pixels i]
                        (if (empty? characters)
                          pixels
                          (if (= \space (first characters))
                            (recur (rest characters) pixels (inc i))
                            (recur (rest characters) (conj pixels i) (inc i)))))]
    (-> (map #(take 3 %) lines) ;; first 3 "columns" describe the next numeral
        flatten
        vec
        (assoc 0 \space 2 \space)
        (active-pixels [] 0))))

(defn lines->acct-number
  "the format should have 3 lines of characters"
  [lines]
  (let [numeral-accum (fn [lines acct-num]
                        (if (-> lines first seq)
                          (let [next-num (-> lines
                                             lines->desc
                                             ->numeral)
                                new-acct-num (+ (* 10 acct-num) next-num)
                                next-lines (map #(drop 3 %) lines)]
                            (recur next-lines new-acct-num))
                          acct-num))]
    (numeral-accum lines 0)))

(defn parse-lines
  "takes a bunch of lines, these *should be* 27 characters each, and parse them into a seq of numerals"
  [lines]
  (let [chars-per-line 27
        acct-num-accum (fn [lines acct-nums]
                         (if (seq lines)
                           (let [[next-acct-num more-acct-nums] (split-at 3 lines)]
                             (->> next-acct-num
                                  (map #(take chars-per-line %))
                                  lines->acct-number
                                  (conj acct-nums)
                                  (recur more-acct-nums)))
                           acct-nums))]
    (acct-num-accum lines [])))
