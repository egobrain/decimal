%% @doc Benchmarks for the decimal library.
%%
%% Timing is done by erlperf (a dependency of the `bench' profile only):
%% each case runs for `rounds' samples of `time' milliseconds after one
%% warmup sample, and the median is reported as ops/sec and ns/op together
%% with the relative standard deviation.
%%
%% Two deterministic counters are reported next to it:
%%   calls/op - calls into decimal and decimal_conv functions per operation,
%%              counted with call_count tracing. It does not depend on the
%%              machine or on how an OTP release accounts reductions, so it
%%              is what `decimal_perf_tests' checks in CI against
%%              `bench/calls.baseline'.
%%   reds/op  - reductions per operation. Informational: OTP releases count
%%              them differently (recent patch releases charge bignum
%%              arithmetic by operand size).
%%
%% Usage (see also the Makefile):
%%   make bench                               % run everything
%%   make bench FILTER=sqrt                   % only cases whose name has "sqrt"
%%   make bench OUT=before.bench              % also save the results
%%   make bench-compare BASE=before.bench NEW=after.bench
%%   make bench-baseline                      % rewrite bench/calls.baseline
-module(decimal_bench).

-export([
         main/1,
         run/0,
         run/1,
         cases/0,
         calls_per_op/2,
         reductions_per_op/2,
         compare/2,
         write_baseline/0,
         write_baseline/1,
         baseline_file/0
        ]).

-type case_name() :: binary().
-type bench_case() :: {case_name(), fun(() -> term())}.
-type result() :: #{name := case_name(),
                    ops_per_sec := float(),
                    ns_per_op := float(),
                    rsd := float(),
                    calls_per_op := float(),
                    reds_per_op := float()}.

-export_type([bench_case/0, result/0]).

-define(DEFAULT_TIME_MS, 500).
-define(DEFAULT_ROUNDS, 3).
-define(COUNT_ITERATIONS, 100).
-define(TRACED_MODULES, [decimal, decimal_conv]).

%% =============================================================================
%%% Entry points
%% =============================================================================

%% @doc Command-line entry point, driven by environment variables so that it
%% can be called from `erl -eval' and from make.
main(_Args) ->
    Opts0 = #{},
    Opts1 = with_env("FILTER", filter, fun list_to_binary/1, Opts0),
    Opts2 = with_env("TIME", time, fun list_to_integer/1, Opts1),
    Opts3 = with_env("ROUNDS", rounds, fun list_to_integer/1, Opts2),
    Opts = with_env("OUT", out, fun(X) -> X end, Opts3),
    _ = run(Opts),
    ok.

-spec run() -> [result()].
run() ->
    run(#{}).

%% @doc Runs the benchmarks and prints a table.
%% Options: `filter' (binary substring of the case name), `time' (ms per
%% sample), `rounds' (samples), `out' (file to save the results to for
%% `compare/2').
-spec run(map()) -> [result()].
run(Opts) ->
    Time = maps:get(time, Opts, ?DEFAULT_TIME_MS),
    Rounds = maps:get(rounds, Opts, ?DEFAULT_ROUNDS),
    Cases = filter(maps:get(filter, Opts, <<>>), cases()),
    io:format("decimal benchmarks, OTP ~s, ~b case(s), ~b x ~bms per case~n~n",
              [erlang:system_info(otp_release), length(Cases), Rounds, Time]),
    print_header(),
    Results = [begin
                   R = measure(Case, Time, Rounds),
                   print_row(R),
                   R
               end || Case <- Cases],
    case maps:find(out, Opts) of
        {ok, File} ->
            ok = save(File, Results),
            io:format("~nsaved to ~s~n", [File]);
        error ->
            ok
    end,
    Results.

%% @doc Prints the difference between two saved runs.
-spec compare(file:filename(), file:filename()) -> ok.
compare(BaseFile, NewFile) ->
    Base = load(BaseFile),
    New = load(NewFile),
    io:format("~-36s ~12s ~12s ~8s ~9s~n",
              ["case", "base ns/op", "new ns/op", "time", "calls/op"]),
    lists:foreach(
      fun(#{name := Name, ns_per_op := NewNs, calls_per_op := NewCalls}) ->
              case [B || B = #{name := N} <- Base, N =:= Name] of
                  [#{ns_per_op := BaseNs, calls_per_op := BaseCalls}] ->
                      io:format("~-36s ~12.1f ~12.1f ~7.1f% ~+9.1f~n",
                                [Name, BaseNs, NewNs,
                                 (NewNs - BaseNs) * 100 / BaseNs,
                                 NewCalls - BaseCalls]);
                  [] ->
                      io:format("~-36s ~12s ~12.1f~n", [Name, "-", NewNs])
              end
      end, New).

%% @doc Rewrites `bench/calls.baseline'.
-spec write_baseline() -> ok.
write_baseline() ->
    write_baseline(baseline_file()).

%% @doc Writes the calls-per-op baseline used by `decimal_perf_tests'.
-spec write_baseline(file:filename()) -> ok.
write_baseline(File) ->
    Lines = [io_lib:format("{~p, ~p}.~n",
                           [Name, round(calls_per_op(Fun, ?COUNT_ITERATIONS))])
             || {Name, Fun} <- cases()],
    Header = "%% Calls into decimal/decimal_conv per op for each decimal_bench case.\n"
             "%% Regenerate with `make bench-baseline` after an intended change.\n",
    ok = file:write_file(File, [Header | Lines]),
    io:format("wrote ~b baselines to ~s~n", [length(Lines), File]).

%% @doc Path of the calls baseline, next to this module's source.
-spec baseline_file() -> file:filename().
baseline_file() ->
    %% rebar3 compiles a copy of bench/ under _build, so fall back to the
    %% path relative to the project root when the source dir lacks the file.
    Src = proplists:get_value(source, module_info(compile)),
    Candidates = [filename:join(filename:dirname(Src), "calls.baseline"),
                  filename:join("bench", "calls.baseline")],
    hd([F || F <- Candidates, filelib:is_regular(F)] ++ Candidates).

%% =============================================================================
%%% Cases
%% =============================================================================

%% @doc All benchmark cases. Inputs are built once, outside the measured fun.
-spec cases() -> [bench_case()].
cases() ->
    Ctx = fun(P) -> #{precision => P, rounding => round_half_up} end,
    Small1 = {12345, -2},            % 123.45
    Small2 = {678, -1},              % 67.8
    Large1 = digits(7, 40, -20),     % 40 significant digits
    Large2 = digits(3, 38, -25),
    Huge1 = digits(7, 1000, -500),   % 1000 significant digits
    Huge2 = digits(3, 990, -480),
    FarExp = {1, 300},               % add/cmp with a large exponent gap
    TrailingZeros = {12345 * pow10(50), -60},
    Arith =
        [
         {<<"add small">>, fun() -> decimal:add(Small1, Small2) end},
         {<<"add large">>, fun() -> decimal:add(Large1, Large2) end},
         {<<"add huge">>, fun() -> decimal:add(Huge1, Huge2) end},
         {<<"add exponent gap 300">>, fun() -> decimal:add(Small1, FarExp) end},
         {<<"sub small">>, fun() -> decimal:sub(Small1, Small2) end},
         {<<"sub large">>, fun() -> decimal:sub(Large1, Large2) end},
         {<<"mult small">>, fun() -> decimal:mult(Small1, Small2) end},
         {<<"mult large">>, fun() -> decimal:mult(Large1, Large2) end},
         {<<"mult huge">>, fun() -> decimal:mult(Huge1, Huge2) end}
        ],
    Divide =
        [
         {<<"divide by 1 p28">>, fun() -> decimal:divide(Small1, {1, 0}, Ctx(28)) end},
         {<<"divide by 2 p28">>, fun() -> decimal:divide(Small1, {2, 0}, Ctx(28)) end},
         {<<"divide small p28">>, fun() -> decimal:divide(Small1, Small2, Ctx(28)) end},
         {<<"divide 1/3 p100">>, fun() -> decimal:divide({1, 0}, {3, 0}, Ctx(100)) end},
         {<<"divide large p28">>, fun() -> decimal:divide(Large1, Large2, Ctx(28)) end},
         {<<"divide large p100">>, fun() -> decimal:divide(Large1, Large2, Ctx(100)) end},
         {<<"divide huge p100">>, fun() -> decimal:divide(Huge1, Huge2, Ctx(100)) end},
         {<<"divide 1/7 p1000">>, fun() -> decimal:divide({1, 0}, {7, 0}, Ctx(1000)) end}
        ],
    Sqrt =
        [
         {<<"sqrt 2 p10">>, fun() -> decimal:sqrt({2, 0}, Ctx(10)) end},
         {<<"sqrt 2 p28">>, fun() -> decimal:sqrt({2, 0}, Ctx(28)) end},
         {<<"sqrt 2 p100">>, fun() -> decimal:sqrt({2, 0}, Ctx(100)) end},
         {<<"sqrt 2 p1000">>, fun() -> decimal:sqrt({2, 0}, Ctx(1000)) end},
         {<<"sqrt exact 144 p28">>, fun() -> decimal:sqrt({144, 0}, Ctx(28)) end},
         {<<"sqrt large p28">>, fun() -> decimal:sqrt(Large1, Ctx(28)) end},
         {<<"sqrt huge p100">>, fun() -> decimal:sqrt(Huge1, Ctx(100)) end}
        ],
    Compare =
        [
         {<<"cmp same exponent">>, fun() -> decimal:cmp(Small1, {12346, -2}, Ctx(28)) end},
         {<<"cmp small p28">>, fun() -> decimal:cmp(Small1, Small2, Ctx(28)) end},
         {<<"cmp large p28">>, fun() -> decimal:cmp(Large1, Large2, Ctx(28)) end},
         {<<"cmp huge p100">>, fun() -> decimal:cmp(Huge1, Huge2, Ctx(100)) end},
         {<<"fast_cmp small">>, fun() -> decimal:fast_cmp(Small1, Small2) end},
         {<<"fast_cmp large">>, fun() -> decimal:fast_cmp(Large1, Large2) end},
         {<<"fast_cmp exponent gap 300">>, fun() -> decimal:fast_cmp(Small1, FarExp) end}
        ],
    Rounding =
        [{iolist_to_binary(["round ", atom_to_list(R), " large p10"]),
          fun() -> decimal:round(R, Large1, 10) end}
         || R <- [round_half_up, round_half_down, round_floor,
                  round_ceiling, round_down]] ++
        [
         {<<"round half_up huge p100">>,
          fun() -> decimal:round(round_half_up, Huge1, 100) end},
         {<<"reduce no zeros">>, fun() -> decimal:reduce(Small1) end},
         {<<"reduce 50 trailing zeros">>,
          fun() -> decimal:reduce(TrailingZeros) end}
        ],
    SmallBin = <<"123.45">>,
    LargeBin = decimal:to_binary(Large1),
    HugeBin = decimal:to_binary(Huge1),
    SciBin = <<"-1.2345678901234567890e-15">>,
    Conv =
        [
         {<<"to_binary small">>, fun() -> decimal:to_binary(Small1) end},
         {<<"to_binary large">>, fun() -> decimal:to_binary(Large1) end},
         {<<"to_binary huge">>, fun() -> decimal:to_binary(Huge1) end},
         {<<"to_binary scientific">>, fun() -> decimal:to_binary({12345, -30}) end},
         {<<"to_decimal binary small p28">>, fun() -> decimal:to_decimal(SmallBin, Ctx(28)) end},
         {<<"to_decimal binary large p28">>, fun() -> decimal:to_decimal(LargeBin, Ctx(28)) end},
         {<<"to_decimal binary huge p100">>, fun() -> decimal:to_decimal(HugeBin, Ctx(100)) end},
         {<<"to_decimal binary scientific p28">>, fun() -> decimal:to_decimal(SciBin, Ctx(28)) end},
         {<<"to_decimal list p28">>, fun() -> decimal:to_decimal("123.45", Ctx(28)) end},
         {<<"to_decimal float p28">>, fun() -> decimal:to_decimal(123.45, Ctx(28)) end},
         {<<"to_decimal integer p28">>, fun() -> decimal:to_decimal(12345, Ctx(28)) end}
        ],
    Arith ++ Divide ++ Sqrt ++ Compare ++ Rounding ++ Conv.

%% =============================================================================
%%% Measurement
%% =============================================================================

%% @doc Average number of calls into the decimal modules per call of `Fun',
%% counted with call_count tracing over `N' calls. Deterministic for a given
%% build of the library. Not safe to run concurrently with other tracing of
%% these modules.
-spec calls_per_op(fun(() -> term()), pos_integer()) -> float().
calls_per_op(Fun, N) ->
    _ = Fun(),
    Patterns = [{M, '_', '_'} || M <- ?TRACED_MODULES],
    try
        [erlang:trace_pattern(P, true, [local, call_count]) || P <- Patterns],
        in_fresh_process(fun() -> loop(Fun, N) end),
        Total = lists:sum([Count || M <- ?TRACED_MODULES,
                                    {F, A} <- M:module_info(functions),
                                    {call_count, Count} <-
                                        [erlang:trace_info({M, F, A}, call_count)],
                                    is_integer(Count)]),
        Total / N
    after
        [erlang:trace_pattern(P, false, [local, call_count]) || P <- Patterns]
    end.

%% @doc Average reductions spent per call of `Fun', measured in a fresh
%% process over `N' calls with the loop overhead subtracted.
-spec reductions_per_op(fun(() -> term()), pos_integer()) -> float().
reductions_per_op(Fun, N) ->
    in_fresh_process(
      fun() ->
              _ = Fun(),
              Loop = loop_reductions(fun noop/0, N),
              Total = loop_reductions(Fun, N),
              max(0.0, (Total - Loop) / N)
      end).

measure({Name, Fun}, TimeMs, Rounds) ->
    #{result := #{median := Median, average := Avg, stddev := StdDev}} =
        erlperf:run(#{runner => Fun},
                    #{samples => Rounds, sample_duration => TimeMs,
                      warmup => 1, report => full}),
    Ops = Median * 1000 / TimeMs,
    #{name => Name,
      ops_per_sec => Ops,
      ns_per_op => 1.0e9 / max(Ops, 1.0e-3),
      rsd => StdDev * 100 / max(Avg, 1),
      calls_per_op => calls_per_op(Fun, ?COUNT_ITERATIONS),
      reds_per_op => reductions_per_op(Fun, ?COUNT_ITERATIONS)}.

loop_reductions(Fun, N) ->
    erlang:garbage_collect(),
    {reductions, R0} = process_info(self(), reductions),
    loop(Fun, N),
    {reductions, R1} = process_info(self(), reductions),
    R1 - R0.

loop(_Fun, 0) -> ok;
loop(Fun, N) ->
    _ = Fun(),
    loop(Fun, N - 1).

noop() -> ok.

in_fresh_process(Fun) ->
    {Pid, Ref} = spawn_monitor(fun() -> exit({ok, Fun()}) end),
    receive
        {'DOWN', Ref, process, Pid, {ok, Result}} -> Result;
        {'DOWN', Ref, process, Pid, Reason} -> erlang:error(Reason)
    end.

%% =============================================================================
%%% Helpers
%% =============================================================================

digits(D, Count, Exp) ->
    {binary_to_integer(binary:copy(<<($0 + D)>>, Count)), Exp}.

pow10(N) ->
    binary_to_integer(<<$1, (binary:copy(<<$0>>, N))/binary>>).

filter(<<>>, Cases) -> Cases;
filter(Pattern, Cases) ->
    [C || C = {Name, _} <- Cases, binary:match(Name, Pattern) =/= nomatch].

with_env(Var, Key, Parse, Opts) ->
    case os:getenv(Var) of
        false -> Opts;
        "" -> Opts;
        Value -> Opts#{Key => Parse(Value)}
    end.

print_header() ->
    io:format("~-36s ~14s ~11s ~6s ~9s ~9s~n",
              ["case", "ops/sec", "ns/op", "rsd", "calls/op", "reds/op"]),
    io:format("~s~n", [lists:duplicate(90, $-)]).

print_row(#{name := Name, ops_per_sec := Ops, ns_per_op := Ns, rsd := Rsd,
            calls_per_op := Calls, reds_per_op := Reds}) ->
    io:format("~-36s ~14s ~11.1f ~5.1f% ~9.1f ~9.1f~n",
              [Name, group(round(Ops)), Ns, Rsd, Calls, Reds]).

group(N) ->
    S = integer_to_list(N),
    lists:reverse(group_(lists:reverse(S))).
group_([A, B, C, D | Rest]) -> [A, B, C, $_ | group_([D | Rest])];
group_(Rest) -> Rest.

save(File, Results) ->
    file:write_file(File, io_lib:format("~p.~n", [Results])).

load(File) ->
    {ok, [Results]} = file:consult(File),
    Results.
