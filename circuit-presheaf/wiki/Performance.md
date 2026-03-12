# Performance

## Requirements

| Configuration | Target | Notes |
|---------------|--------|-------|
| d=3 s<=5 | under 5 seconds | Python takes 60s; expect 10-50x speedup |
| d=3 s<=7 | under 5 minutes | Python can't do this |
| d=4 s<=5 | under 10 minutes | Python can't do this |

## Bottleneck

The enumeration loop: canonical string computation and hash-based dedup.

## Optimization

Use mutable hash maps (`IORef` + `Data.IOArray` or similar) for the inner loop.
