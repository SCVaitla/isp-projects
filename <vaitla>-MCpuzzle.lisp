;;; Missionaries and Cannibals Problem Solver in Lisp
;;; Using recursion to find the valid sequence of boat rides to solve the puzzle.

(defun missionaries-and-cannibals ()
  "Solves the 3 missionaries and 3 cannibals puzzle."
  (solve-puzzle '(3 3 west 0 0) '(0 0 east 3 3)))

(defun solve-puzzle (initial-state goal-state)
  "Recursively solve the missionaries and cannibals puzzle."
  (recursive-solve initial-state goal-state '() '()))

(defun recursive-solve (state goal-state actions visited-states)
  "Recursive function to explore possible moves and solve the puzzle."
  (cond
    ;; Check if we have reached the goal state
    ((equal state goal-state)
     (reverse actions))  ;; Return the sequence of actions

    ;; Check if we've visited this state before (to avoid loops)
    ((member state visited-states :test #'equal)
     nil)

    ;; Try each possible valid action
    (t
     (let ((next-states (valid-next-states state)))
       ;; Ensure each next state is not NIL and proceed
       (some (lambda (next)
               (when next  ;; Ensure we're not passing NIL as a valid state
                 (recursive-solve next goal-state
                                  (cons (list 'move state next) actions)
                                  (cons state visited-states))))
             next-states)))))

(defun valid-next-states (state)
  "Generate all valid next states for the current state."
  (remove-if-not #'valid-state-p
                 (remove-if #'null (generate-next-states state))))

(defun generate-next-states (state)
  "Generate all possible next states by applying actions."
  (list
   (move-cannibals state 1)
   (move-cannibals state 2)
   (move-missionaries state 1)
   (move-missionaries state 2)
   (move-both state 1 1)))

(defun move-cannibals (state num)
  "Move num cannibals across the river."
  (let* ((west-c (first state))
         (west-m (second state))
         (boat-side (third state))
         (east-c (fourth state))
         (east-m (fifth state)))
    (if (and (>= west-c num) (equal boat-side 'west))
        ;; Move from west to east
        (list (- west-c num) west-m 'east (+ east-c num) east-m)
      ;; Move from east to west
      (if (and (>= east-c num) (equal boat-side 'east))
          (list (+ west-c num) west-m 'west (- east-c num) east-m)
        nil))))

(defun move-missionaries (state num)
  "Move num missionaries across the river."
  (let* ((west-c (first state))
         (west-m (second state))
         (boat-side (third state))
         (east-c (fourth state))
         (east-m (fifth state)))
    (if (and (>= west-m num) (equal boat-side 'west))
        ;; Move from west to east
        (list west-c (- west-m num) 'east east-c (+ east-m num))
      ;; Move from east to west
      (if (and (>= east-m num) (equal boat-side 'east))
          (list west-c (+ west-m num) 'west east-c (- east-m num))
        nil))))

(defun move-both (state c m)
  "Move c cannibals and m missionaries across the river."
  (let* ((west-c (first state))
         (west-m (second state))
         (boat-side (third state))
         (east-c (fourth state))
         (east-m (fifth state)))
    (if (and (>= west-c c) (>= west-m m) (equal boat-side 'west))
        ;; Move from west to east
        (list (- west-c c) (- west-m m) 'east (+ east-c c) (+ east-m m))
      ;; Move from east to west
      (if (and (>= east-c c) (>= east-m m) (equal boat-side 'east))
          (list (+ west-c c) (+ west-m m) 'west (- east-c c) (- east-m m))
        nil))))

(defun valid-state-p (state)
  "Check if the resulting state is safe and valid."
  (let ((west-cannibals (first state))
        (west-missionaries (second state))
        (east-cannibals (fourth state))
        (east-missionaries (fifth state)))
    (and
     ;; Ensure cannibals don’t outnumber missionaries on either bank
     (>= west-cannibals 0)
     (>= west-missionaries 0)
     (>= east-cannibals 0)
     (>= east-missionaries 0)
     (or (<= west-cannibals west-missionaries) (= west-missionaries 0))
     (or (<= east-cannibals east-missionaries) (= east-missionaries 0)))))

(defun test-puzzle ()
  "Test the missionaries and cannibals puzzle solver."
  (let ((solution (missionaries-and-cannibals)))
    (if solution
        (dolist (step solution)
          (format t "~A~%" step))
      (format t "No solution found.~%"))))

(test-puzzle)
