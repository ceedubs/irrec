- `Irrec orig` represents results from irrec was using `foldLeft` instead of `foldM` for matching.
- `Irrec foldM` represents results from when irrec was changed to use `foldM` instead of `foldLeft` for matching.
- `Irrec IndexedSeq Foldable` represents results when irrec was using `foldM` with a custom `Foldable[IndexedSeq]` instance that was meant to be optimized.

# matching `ab(c|d)efg`

## with input`abcefg`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |       11965277.990 |         279283.100 |
| Irrec orig                |        1865748.172 |          64790.817 |
| Irrec IndexedSeq Foldable |        2133946.525 |          89160.571 |
| Irrec foldM               |        1281356.766 |          48570.456 |

# (not) matching `(a|b){20,25}`

## with input `("a" * 10) + ("b" * 13) + ("c" * 20)`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |        1643287.769 |         254558.674 |
| Irrec orig                |         220082.423 |           5246.128 |
| Irrec IndexedSeq Foldable |         238637.438 |          19572.614 |
| Irrec foldM               |         264892.126 |           2025.586 |

# matching `(0*)*A`

## with input `"0" * 20`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |             32.928 |              0.178 |
| Irrec orig                |         585968.064 |          39248.331 |
| Irrec IndexedSeq Foldable |         671214.328 |          34851.945 |
| Irrec foldM               |         439878.964 |           2667.288 |
