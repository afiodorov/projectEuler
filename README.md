After losing most of my original solutions I decided to github if I solve
anything. Gives me an incentive to keep my code clean.

## Content:

# 7.hs

Haskell's show-off example: "Sieve of Eratosthenes"

```shell
time cabal run 7 10001
Preprocessing executable '7' for projecteuler-0.1.0.0...
[1 of 1] Compiling Main             ( 7.hs, dist/build/7/7-tmp/Main.o )
Linking dist/build/7/7 ...
104743

real    0m5.074s
user    0m4.543s
sys     0m0.503s
```

# 79.hs
Solution to 79'th problem without the assumption of non-repeats.
Algorithm: with each successful attempt generate all shortest passwords for
each shortest password generated so far.

try:
```shell
time run 79 ./data/keylog.txt
Preprocessing executable '79' for projecteuler-0.1.0.0...
73162890

real    0m1.245s
user    0m0.910s
sys     0m0.320s
```

# 88.hs

Brute forces decomposition via factorisation and backtracking.

```shell
time cabal run 88
Preprocessing executable '88' for projecteuler-0.1.0.0...
7587457

real    0m8.768s
user    0m8.363s
sys     0m0.390s
```

# 96.hs

Backtracking depth-first algorithm. At each step pick a square with least
number of guesses.

```shell
time cabal run 96 ./data/p096_sudoku.txt
Preprocessing executable '96' for projecteuler-0.1.0.0...
24702

real    0m2.429s
user    0m2.090s
sys     0m0.323s
```
