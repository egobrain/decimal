-module(bench_decimal).

-include_lib("decimal/include/decimal.hrl").

-export([
    add/1,
    bench_add/2
]).

add({input, _}) ->
    [{N, N} || N <- lists:seq(1, 146)].

bench_add(List, _) ->
    lists:foldl(
        fun(D, Sum) ->
            decimal:add(Sum, D)
        end,
        ?d_zero,
        List
    ).
