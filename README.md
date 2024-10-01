# vaitla-lisp-projects

This repository contains three distinct Lisp projects that explore different problem-solving techniques and concepts.

## Project Descriptions

### 1. **vaitla-family.lisp**
This project models a family structure using Lisp. The key features include:
- Defining a `person` structure with attributes like name, age, and siblings.
- Functions to add people to the family, find them by name, and establish sibling relationships.
- A custom print function to display each person's details.
  
#### Key Functions:
- `add-member(name, age)` – Adds a new person to the global `*people*` list.
- `find-person(name)` – Searches for a person by name in the `*people*` list.
- `make-siblings(name1, name2)` – Establishes sibling relationships between two people.
- `initialize-people()` – Initializes a predefined set of family members with sibling connections.

### 2. **vaitla-allenRelations.lisp**
This project implements Allen's Interval Algebra, which defines relationships between time intervals. It can be used for temporal reasoning in AI applications.

#### Key Functions:
- `before(t1, t2)` – Checks if interval `t1` ends before `t2` begins.
- `after(t1, t2)` – Checks if `t1` starts after `t2` ends.
- `during(t1, t2)` – Determines if `t1` is completely within `t2`.
- `overlaps(t1, t2)` – Determines if `t1` overlaps with `t2`.
- `starts(t1, t2)` – Checks if `t1` starts at the same time as `t2` but ends earlier.
- `finishes(t1, t2)` – Checks if `t1` finishes at the same time as `t2` but starts later.
- `equals(t1, t2)` – Checks if `t1` and `t2` are equal in both start and end times.

### 3. **vaitla-MCpuzzle.lisp**
This project solves the classic Missionaries and Cannibals problem using recursive techniques in Lisp. The problem involves safely transporting missionaries and cannibals across a river without violating constraints (cannibals must never outnumber missionaries on either side of the river).

#### Key Functions:
- `missionaries-and-cannibals()` – The main function that initiates the puzzle-solving process.
- `solve-puzzle(initial-state, goal-state)` – Recursively solves the puzzle.
- `move-cannibals(state, num)` – Moves a specified number of cannibals across the river.
- `move-missionaries(state, num)` – Moves a specified number of missionaries across the river.
- `valid-state-p(state)` – Validates if the current state of the puzzle is safe.

#### Test Function:
- `test-puzzle()` – Runs test cases to validate the solution.

## How to Run
To run the code, load the `.lisp` files into your Lisp environment and call the respective test functions:
- For **vaitla-family.lisp**: `initialize-people()`
- For **vaitla-allenRelations.lisp**: `test-intervals()`
- For **vaitla-MCpuzzle.lisp**: `test-puzzle()`

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

