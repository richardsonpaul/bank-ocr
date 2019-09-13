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
