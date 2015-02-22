After losing most of my original solutions I decided to github if I solve
anything. Gives me an incentive to keep my code clean.

## Content:
# 79.hs
Solution to 79'th problem without the assumption of non-repeats.
Algorithm: with each successful attempt generate all shortest passwords for
each shortest password generated so far.

try:
```shell
cabal run 79 ./data/keylog.txt

Preprocessing executable '79' for projecteuler-0.1.0.0...
73162890
```

# 88.hs

Brute forces decomposition via factorisation and backtracking. Lower-bound
comes from Geometric Mean <= Average. TODO: improve performance by
eliminating duplicates, e.g.
k = 4: 8 = 1 x 1 x 2 x 4, since 4 = 2 x 2 = 2 + 2, k = 5 is solved already.

```shell
time cabal run 88
Preprocessing executable '88' for projecteuler-0.1.0.0...
7587457

real    0m32.811s
user    0m32.100s
sys     0m0.637s
```

# 96.hs

Backtracking depth-first algorithm. At each step pick a square with least
number of guesses.

```shell
cabal run 96 ./data/p096_sudoku.txt

Linking dist/build/96/96 ...
24702
```
