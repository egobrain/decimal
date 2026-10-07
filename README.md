[![CI](https://github.com/egobrain/decimal/actions/workflows/ci.yml/badge.svg)](https://github.com/egobrain/decimal/actions/workflows/ci.yml)
[![Coverage](https://coveralls.io/repos/github/egobrain/decimal/badge.svg?branch=master)](https://coveralls.io/github/egobrain/decimal?branch=master)
[![GitHub tag](https://img.shields.io/github/tag/egobrain/decimal.svg)](https://github.com/egobrain/decimal)

# decimal
An Erlang decimal arithmetic library.

## Performance

`make bench` runs the benchmark suite in `bench/decimal_bench.erl` with
[erlperf](https://github.com/max-au/erlperf): add, sub, mult, divide, sqrt,
cmp, round, reduce and binary/float/list conversion on small, 40-digit and
1000-digit inputs at precisions from 10 to 1000. Each case reports ops/sec,
ns/op and its spread, plus calls and reductions per op.

```sh
make bench FILTER=sqrt               # only cases whose name contains "sqrt"
make bench OUT=before.bench          # save a run...
make bench OUT=after.bench
make bench-compare BASE=before.bench NEW=after.bench   # ...and diff two runs
```

`rebar3 eunit` also runs `decimal_perf_tests`, a timing-free regression check:
for each benchmark case, the number of calls into `decimal` and `decimal_conv`
per op must stay within 1.1x of `bench/calls.baseline`. The count only changes
when the code does, so it gives the same result on every machine and OTP
release. After an intended change, regenerate the file with
`make bench-baseline`. Time spent inside bignum arithmetic is not counted, so
use `make bench` to check those changes.
