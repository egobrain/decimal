%% @doc Performance regression check that is safe to run in CI.
%%
%% Wall-clock thresholds are flaky on shared runners, and reductions are
%% counted differently by different OTP releases (even patch releases), so
%% this checks the number of calls into decimal and decimal_conv per
%% operation instead. That number only changes when the code does: an extra
%% recursion, a lost fast path, a loop that runs more often. Each
%% `decimal_bench' case must stay within `?FACTOR' x `bench/calls.baseline'.
%%
%% Time spent inside bignum BIFs is not visible here, so run `make bench'
%% for wall-clock numbers when touching arithmetic.
-module(decimal_perf_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(FACTOR, 1.1).
-define(ITERATIONS, 100).

calls_test_() ->
    case cover_compiled() of
        true ->
            %% Cover instrumentation changes what gets counted; skip.
            [];
        false ->
            {ok, Terms} = file:consult(decimal_bench:baseline_file()),
            Baseline = maps:from_list(Terms),
            [{binary_to_list(Name), check(Name, Fun, Baseline)}
             || {Name, Fun} <- decimal_bench:cases()]
    end.

check(Name, Fun, Baseline) ->
    fun() ->
            case maps:find(Name, Baseline) of
                {ok, Base} ->
                    Calls = decimal_bench:calls_per_op(Fun, ?ITERATIONS),
                    Budget = Base * ?FACTOR,
                    Calls =< Budget orelse
                        erlang:error({calls_regression,
                                      [{'case', Name},
                                       {calls_per_op, Calls},
                                       {baseline, Base},
                                       {budget, Budget},
                                       {hint, "if intended, run `make bench-baseline`"}]});
                error ->
                    erlang:error({missing_baseline,
                                  [{'case', Name},
                                   {hint, "run `make bench-baseline`"}]})
            end
    end.

%% Relations that should hold regardless of the baseline: fast paths stay
%% cheaper than the general path.
fast_paths_test_() ->
    case cover_compiled() of
        true -> [];
        false -> fast_paths()
    end.

fast_paths() ->
    Ctx = #{precision => 28, rounding => round_half_up},
    C = fun(F) -> decimal_bench:calls_per_op(F, ?ITERATIONS) end,
    [
     {"divide by 1 and 2 skip long division",
      fun() ->
              Long = C(fun() -> decimal:divide({12345, -2}, {678, -1}, Ctx) end),
              ?assert(C(fun() -> decimal:divide({12345, -2}, {1, 0}, Ctx) end) < Long),
              ?assert(C(fun() -> decimal:divide({12345, -2}, {2, 0}, Ctx) end) < Long)
      end},
     {"cmp with equal exponents skips rounding",
      fun() ->
              ?assert(C(fun() -> decimal:cmp({12345, -2}, {12346, -2}, Ctx) end) <
                      C(fun() -> decimal:cmp({12345, -2}, {678, -1}, Ctx) end))
      end}
    ].

cover_compiled() ->
    %% Ask cover only when its server is running, so we don't start it.
    whereis(cover_server) =/= undefined andalso
        cover:is_compiled(decimal) =/= false.

-endif.
