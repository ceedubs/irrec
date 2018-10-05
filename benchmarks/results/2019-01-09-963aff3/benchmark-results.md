# matching `ab(c|d)efg`

## with input`abcefg`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |       12912666.823 |         127167.028 |
| Irrec                     |        1350828.751 |          42508.572 |
| Irrec optimized           |        1504600.364 |          12535.381 |

# (not) matching `(a|b){20,25}`

## with input `("a" * 10) + ("b" * 13) + ("c" * 20)`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |        1801165.562 |          13948.995 |
| Irrec                     |         270227.419 |          14478.895 |
| Irrec optimized           |         256191.187 |           3119.406 |

# matching `(0*)*A`

## with input `"0" * 20`

| Implementation            | Throughput (ops/s) | Variance (± ops/s) |
| ------------------------- | ------------------:| ------------------:|
| Java                      |             32.636 |              0.216 |
| Irrec                     |         491247.162 |            817.424 |
| Irrec optimized           |         436371.003 |           4668.516 |
