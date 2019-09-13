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

(def ^:private nums [zero one two three four five six seven eight nine])
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

(def ^:private pred-zero? [has-3? has-6? has-5? !has-4?])
(def ^:private pred-one? [!has-3? !has-4? !has-1?])
(def ^:private pred-two? [!has-3? has-4? has-6?])
(def ^:private pred-three? [!has-3? has-4? !has-6?])
(def ^:private pred-four? [has-3? !has-6? has-5? !has-1?])
(def ^:private pred-five? [has-3? !has-6? !has-5?])
(def ^:private pred-six? [has-3? has-6? !has-5?])
(def ^:private pred-seven? [!has-3? !has-4? has-1?])
(def ^:private pred-eight? [has-3? has-6? has-5? has-4?])
(def ^:private pred-nine? [has-3? !has-6? has-5? has-1?])
