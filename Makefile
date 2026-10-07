REBAR3 ?= rebar3
BENCH_ERL = erl -noshell -pa _build/bench/lib/*/ebin _build/bench/checkouts/*/ebin _build/bench/lib/decimal/bench

.PHONY: bench bench-compare bench-baseline perf-check

# Run the benchmarks. Optional: FILTER=sqrt TIME=500 ROUNDS=3 OUT=file.bench
bench:
	$(REBAR3) as bench compile
	$(BENCH_ERL) -eval 'decimal_bench:main([]), halt().'

# Compare two runs saved with OUT=...
bench-compare:
	$(REBAR3) as bench compile
	$(BENCH_ERL) -eval 'decimal_bench:compare("$(BASE)", "$(NEW)"), halt().'

# Rewrite bench/calls.baseline, checked by decimal_perf_tests.
bench-baseline:
	$(REBAR3) as bench compile
	$(BENCH_ERL) -eval 'decimal_bench:write_baseline(), halt().'

# Only the CI-safe calls check (it also runs as part of `rebar3 eunit`).
perf-check:
	$(REBAR3) eunit --module=decimal_perf_tests
