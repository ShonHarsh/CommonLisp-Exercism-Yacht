(defpackage :yacht
  (:use :cl)
  (:export :score))
(in-package :yacht)

;; Ones, Twos, Threes, Fours, Fives, Sixes
;; Takes the quantity of rolled dice and returns digit value * the quantity
(defun single (dice matchedDigit)
  ;; multiply the matched digit value by the count of dice rolled
  (* matchedDigit (count matchedDigit dice))
)

;; Full House
;; My brain has a segmentation fault
(defun fullHouse (sorted)
  (if 
    (and 
      (or 
        (and (= (first sorted) (second sorted))
             (= (third sorted) (fifth sorted))
        )
        (and (= (first sorted) (third sorted))
             (= (fourth sorted) (fifth sorted))
        )
      )
        (not (= (first sorted) (fifth sorted))
        )
    )
    (choice sorted)
    0)
)

;; Four Of A Kind
(defun fourOfAKind (sorted)
  (if 
    (or (= (first sorted) (fourth sorted))
        (= (second sorted) (fifth sorted))
    )
    (* (second sorted) 4)
  0)
)

;; Little Straight, Big Straight
;; Since the scors are the same, the same method can be used for both straights
(defun straight (sorted matchedDigit)
  (if (equal sorted matchedDigit) 30 0)
)

;; Choice
;; reduce function sequence &key key from-end start end initial-value => result
;; Example: (reduce #'* '(1 2 3 4 5)) =>  120
;; http://clhs.lisp.se/Body/f_reduce.htm
(defun choice (dice)
  (reduce #'+ dice)
)

;; Yacht!
(defun yacht (sorted)
  (if (= (first sorted) (fifth sorted)) 50 0)
)

(defun score (dice category)
  "Returns the score of the dice for the given category."

  ;;The case statement to switch based on the category
  (case category
    ;; Ones, Logic: Any combination, Score: 1 * count
    (:ones (single dice 1))

    ;; Twos, Logic: Any combination, Score: 2 * count
    (:twos (single dice 2))

    ;; Threes, Logic: Any combination, Score: 3 * count
    (:threes (single dice 3))

    ;; Fours, Logic: Any combination, Score: 4 * count
    (:fours (single dice 4))

    ;; Fives, Logic: Any combination, Score: 5 * count
    (:fives (single dice 5))

    ;; Sixes, Logic: Any combination, Score: 6 * count
    (:sixes (single dice 6))

    ;; Full House, Logic: Three of one number and two of another, Score: Dice total
    ;; sort sequence predicate &key key => sorted-sequence
    ;; http://clhs.lisp.se/Body/f_sort_.htm
    (:full-house (fullHouse (sort dice #'<)))

    ;; Four Of A Kind, Logic: At leat four of the same digit, Four dice total
    (:four-of-a-kind (fourOfAKind (sort dice #'<)))

    ;; Little Straight, Logic: 1, 2, 3, 4, 5, Score: 30 Points
    (:little-straight (straight (sort dice #'<)'(1 2 3 4 5)))

    ;; Big Straight, Logic: 2, 3, 4, 5, 6, Score: 30 points
    (:big-straight (straight (sort dice #'<)'(2 3 4 5 6)))

    ;; Choice, Logic: Any combination, Score: Dice sum
    (:choice (choice dice))

    ;; Yacht, Logic: All dice the same digit, Score: 50 points
    (:yacht (yacht (sort dice #'<)))
    
    ;; If the dice do not satisfy the requirements of a category, the score is zero
    (otherwise 0)
  )
)
  