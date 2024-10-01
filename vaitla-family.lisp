;; 1. Declare a global variable *people* as an empty list
(defvar *people* '())

;; 2. Define the structure 'person' with name, age, and siblings
(defstruct person
  (name 'noname)   ;; Default name
  (age 0)          ;; Default age
  (siblings nil))  ;; List of sibling structures

;; 3. Function to add a person to the *people* list
(defun add-member (name age)
  "Add a new person to the *people* list"
  (push (make-person :name name :age age) *people*))

;; 4. Function to find a person by name in the *people* list
(defun find-person (name)
  "Find a person in the *people* list by name"
  (find name *people* :key #'person-name :test #'equal))

;; 5. Function to make two people siblings
(defun make-siblings (name1 name2)
  "Make two people siblings"
  (let ((p1 (find-person name1))
        (p2 (find-person name2)))
    (when (and p1 p2)
      (push p2 (person-siblings p1))
      (push p1 (person-siblings p2)))))

;; 6. Initialize the people and sibling relationships
(defun initialize-people ()
  "Add Bob, Susan, Frank, and Mary to the *people* list and set siblings"
  ;; Adding people
  (add-member "Bob" 21)
  (add-member "Susan" 18)
  (add-member "Frank" 16)
  (add-member "Mary" 14)
  ;; Make Bob, Susan, and Frank siblings
  (make-siblings "Bob" "Susan")
  (make-siblings "Bob" "Frank")
  (make-siblings "Susan" "Frank"))

;; 7. Function to return a list of sibling names
(defun sibling-names (name)
  "Return a list of sibling names for a given person"
  (let ((person (find-person name)))
    (when person
      (mapcar #'person-name (person-siblings person)))))

;; 9. Overriding the print function for person structure
(defstruct (person (:print-function print-person))
  (name 'noname)
  (age 0)
  (siblings nil))

(defun print-person (person stream depth)
  "Custom print function for person structure"
  (format stream "~A ~A ~A"
          (person-name person)
          (person-age person)
          (sibling-names (person-name person))))
