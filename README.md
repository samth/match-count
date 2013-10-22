### Instrumentation for `match` clauses

```
#lang racket

(require match-count)

(for ([i 100])
  (match/count (random 3)
    [0 0]
    [1 1]
    [2 2]))
```

```
$ racket /tmp/mc.rkt 
'((8 . 37) (9 . 36) (7 . 27))
```
