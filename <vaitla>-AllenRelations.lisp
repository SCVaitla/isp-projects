;; 1. Define the 'time-interval' structure to avoid conflict
(defstruct time-interval
  task
  begin
  end)

;; 2. Define Allen's time relations functions using the new 'time-interval'

;; a. 'before' relation: time-interval t1 ends before t2 begins
(defun before (t1 t2)
  (< (time-interval-end t1) (time-interval-begin t2)))

;; b. 'after' relation: time-interval t1 starts after t2 ends
(defun after (t1 t2)
  (> (time-interval-begin t1) (time-interval-end t2)))

;; c. 'during' relation: time-interval t1 is completely inside t2
(defun during (t1 t2)
  (and (> (time-interval-begin t1) (time-interval-begin t2))
       (< (time-interval-end t1) (time-interval-end t2))))

;; d. 'overlaps' relation: time-interval t1 overlaps with t2
(defun overlaps (t1 t2)
  (and (< (time-interval-begin t1) (time-interval-end t2))
       (> (time-interval-end t1) (time-interval-begin t2))))

;; e. 'starts' relation: time-interval t1 starts exactly when t2 starts, but ends before t2
(defun starts (t1 t2)
  (and (= (time-interval-begin t1) (time-interval-begin t2))
       (< (time-interval-end t1) (time-interval-end t2))))

;; f. 'finishes' relation: time-interval t1 ends exactly when t2 ends, but starts after t2
(defun finishes (t1 t2)
  (and (= (time-interval-end t1) (time-interval-end t2))
       (> (time-interval-begin t1) (time-interval-begin t2))))

;; g. 'equals' relation: time-interval t1 starts and ends exactly when t2 does
(defun equals (t1 t2)
  (and (= (time-interval-begin t1) (time-interval-begin t2))
       (= (time-interval-end t1) (time-interval-end t2))))

;; 3. Test cases with the renamed 'time-interval' structure

(defun test-intervals ()
  ;; Define two time intervals
  (let ((t1 (make-time-interval :task "Task1" :begin 5 :end 10))
        (t2 (make-time-interval :task "Task2" :begin 7 :end 13)))
    (format t "Time Interval t1: ~A~%" t1)
    (format t "Time Interval t2: ~A~%" t2)
    
    ;; Test Allen's relations
    (format t "Before: ~A~%" (before t1 t2))   ;; Should return T (t1 ends before t2 starts)
    (format t "After: ~A~%" (after t1 t2))     ;; Should return NIL
    (format t "During: ~A~%" (during t1 t2))   ;; Should return T (t1 is completely inside t2)
    (format t "Overlaps: ~A~%" (overlaps t1 t2)) ;; Should return T (t1 overlaps with t2)
    (format t "Starts: ~A~%" (starts t1 t2))   ;; Should return NIL
    (format t "Finishes: ~A~%" (finishes t1 t2)) ;; Should return NIL
    (format t "Equals: ~A~%" (equals t1 t2))))  ;; Should return NIL
