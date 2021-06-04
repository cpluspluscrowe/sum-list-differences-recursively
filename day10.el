(setq input (f-read-text "test-input.txt" 'utf-8))
(setq input-sequence (-map #'string-to-number (butlast (split-string input "\n"))))
(setq max-value (+ (-max input-sequence) 3))
(setq adapters (append input-sequence (list 0 max-value)))
(setq sorted-sequence (-sort #'< adapters))

(cl-defstruct difference-sum by-one by-two by-three)
(setq differences (make-difference-sum :by-one 0 :by-two 0 :by-three 0))

(defun create-differences (difference)
  (cond
  ((eq difference 1) (make-difference-sum :by-one 1 :by-two 0 :by-three 0))
  ((eq difference 2) (make-difference-sum :by-one 0 :by-two 1 :by-three 0))
  ((eq difference 3) (make-difference-sum :by-one 0 :by-two 0 :by-three 1))
  )
  )

(defun sum-differences (d1 d2)
  (make-difference-sum
  :by-one 
  (+ (difference-sum-by-one d1) (difference-sum-by-one d2))
  :by-two 
  (+ (difference-sum-by-two d1) (difference-sum-by-two d2))
  :by-three 
  (+ (difference-sum-by-three d1) (difference-sum-by-three d2))
  )
  )

(sum-differences (create-differences 1)(create-differences 2))
; need base case

(defun track-differences (sequence differences)
  ; base case
  (if (eq 1 (length sequence)) differences
  (let* (
        (current (car sequence))
        (next (nth 1 sequence))
        (difference (- next current))
        )
    (progn
      (track-differences (cdr sequence) (sum-differences (create-differences difference) differences))
      )
  )))

(setq result (track-differences sorted-sequence differences))
(setq answer (* (difference-sum-by-one result)
(difference-sum-by-three result)))



