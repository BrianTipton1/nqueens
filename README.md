# NQueens

## Genetic Algorithm to solve the nqueens problem

#### Building
```bash
ghc -o nqueens ./app/Main.hs
```

### Defaults
- Initial board configuration -> Diagnonal
- Mutation Rate -> 0.01
- Crossover Rate -> 0.7
- Population Size -> 32
- Board Size -> 8
- Generation Size -> Int Max Bound

When it is running the board that it is being printed to the screen is the current generations best board or board with the least amount of queens in threat.

I was having quicker success with 8x8 boards with 5ish percent mutation rate.

### Common Run configurations

#### Start from random board
```bash
./nqueens -r
```

#### Lower Generation Size
```bash
./nqueens -g=5000
```

####  Random board with larger population size
```bash
./nqueens -r -p=128
```

####  Random board with higher mutation rate 
```bash
./nqueens -r -m=0.05
```

#### 4x4 Board
```bash
./nqueens -s=4
```

#### Average times over 5 trials
```
Average Times for nqueens.cabal
===============================
Average time for n = 4: 152.20ms
Average time for n = 7: 9670.00ms
Average time for n = 8: 150538.00ms
Average time for n = 9: 185746.40ms
```

#### Help Text 
```
N-Queens Solver
=====================

Usage:
    nqueens [-s=SIZE] [-r | -d] [-c=PROB] [-m=PROB] [-p=SIZE] [-g=GEN]

Description:
    This program solves the N-Queens problem, which involves placing N chess queens on an N×N chessboard so that no two queens threaten each other.

Options:
    -s=SIZE, --size=SIZE          Specifies the size of the board. SIZE must be an integer greater than 3. (Default: 8)
    
    -r, --random                  Solves the problem by placing queens at random positions that do not conflict with each other.
    
    -d, --diagonal                Solves the problem by placing queens along the diagonal of the board before attempting to solve.
    
    -c=PROB, --crossover=PROB     Specifies the crossover probability. PROB must be a decimal between 0 and 1.
    
    -m=PROB, --mutation=PROB      Specifies the mutation probability. PROB must be a decimal between 0 and 1.
    
    -p=SIZE, --population=SIZE    Specifies the population size. SIZE must be an integer.
    
    -g=GEN, --generation=GEN      Specifies the number of generations. GEN must be an integer.
  
Note:
    Either the -r/--random flag or the -d/--diagonal flag can be supplied; both cannot be used at the same time.

Examples:
    nqueens -r
    nqueens -s=8 -r
    nqueens --size=10 --diagonal
    nqueens -s=10 -r -c=0.7 -m=0.2 -p=50 -g=100
```



### ChatGPT
- [Help Text Generation](https://chat.openai.com/share/4f6d3efa-1a42-4421-8e5a-9ccdc10233b3)
- [Timing Script Generation](https://chat.openai.com/share/730b07c5-0434-4860-b2e5-1ff32411cac6)