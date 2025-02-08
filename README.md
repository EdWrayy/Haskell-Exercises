# Assessed Haskell Exercises

## Overview
This repository contains six Haskell exercises, each accompanied by test cases. The exercises focus on **algorithmic thinking** and **understanding the inner workings of programming languages**, particularly through **the Black Box game** and **lambda calculus**.

## Exercises

### 1. The Blackbox Game
- Given a **triangular grid** containing atoms, we fire **rays** into the grid.
- Rays **reflect** when they hit an atom.
- The goal is to write a program that, given a triangle and atom locations, **computes the entry and exit points** of every possible ray.

### 2. Solving The Blackbox Game
- Given a triangle and a **list of observed interactions**, determine the **locations of all atoms**.
- This requires designing an **optimization algorithm** to deduce atom placements based on ray behaviors.

### 3. Unparse
- Write an **unparser** that converts an **abstract syntax tree (AST)** for a lambda calculus expression into its **string representation**.

### 4. Parse
- Write a **parser** that takes a **string representation of a lambda calculus expression** and converts it into an **AST**.

### 5. Continuation Passing Style (CPS)
- Implement a transformation that converts a **lambda calculus expression** into **Continuation Passing Style (CPS)**.

### 6. Compare Reduction Strategies
- Given a **lambda calculus expression**, compare the number of **reductions** needed using:
  - **Innermost reduction (call-by-value)**
  - **Outermost reduction (call-by-name)**
- If the number of reductions exceeds a given bound **n**, return `Nothing`.

## Installation & Setup

1. **Clone the repository**:
   ```sh
   git clone https://github.com/yourusername/haskell-exercises.git
   cd haskell-exercises
   ```
2. **Ensure you have GHC & Cabal installed**:
   ```sh
   ghc --version
   cabal --version
   ```
3. **Run the test cases**:
   ```sh
   cabal test
   ```

## Running Individual Exercises
Each exercise is implemented as a Haskell module. To run a specific exercise:
```sh
ghci src/Exercise1.hs
```
Or compile and execute:
```sh
ghc -o exercise1 src/Exercise1.hs
./exercise1
```

## Contributing
Contributions are welcome! Please submit issues or pull requests if you find bugs or have improvements.

## License
This project is licensed under the MIT License.

