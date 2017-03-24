-module(decimal_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_test_() ->
    Opts = #{ precision => 5, rounding => round_half_up },
    Tests =
        [
         {"1", {1, 0}},
         {"-1", {-1, 0}}
        ],
    [
     {V, fun() -> R = decimal:to_decimal(V, Opts) end}
     || {V, R} <- Tests
    ].

binary_test_() ->
    Opts = #{ precision => 5, rounding => round_half_up },
    PosTests =
        [
         {<<"1">>, {1, 0}},
         {<<"123">>, {123, 0}},
         {<<"1230">>, {123, 1}},
         {<<"1.23">>, {123, -2}},
         {<<"1.2300">>, {123, -2}},
         {<<"1e0">>, {1, 0}},
         {<<"12e1">>, {12, 1}},
         {<<"12300e3">>, {123, 5}},
         {<<"1.2300e0">>, {123, -2}},
         {<<"1.2300e2">>, {123, 0}},
         {<<"1.2300e-2">>, {123, -4}},
         {<<"123.45600e-2">>, {123456, -5}},
         {<<"1e-0">>, {1, 0}},
         {<<"12e-1">>, {12, -1}},
         {<<"12300e-3">>, {123, -1}},
         {<<"1e+0">>, {1, 0}},
         {<<"12e+1">>, {12, 1}},
         {<<"12300e+3">>, {123, 5}}
        ],
    NegTests = [{<<$-, V/binary>>, {-B, E}} || {V, {B, E}} <- PosTests],
    [
     {V, fun() -> R = decimal:to_decimal(V, Opts) end}
     || {V, R} <- PosTests ++ NegTests
    ].

rounding_test_() ->
    Tests =
        [
         {<<"1">>, 1, round_floor, <<"1.0">>},
         {<<"1.2">>, 1, round_floor, <<"1.2">>},
         {<<"1.0">>, 0, round_floor, <<"1.0">>},
         {<<"1.3">>, 0, round_floor, <<"1.0">>},
         {<<"1.5">>, 0, round_floor, <<"1.0">>},
         {<<"1.9">>, 0, round_floor, <<"1.0">>},
         {<<"-1.0">>, 0, round_floor, <<"-1.0">>},
         {<<"-1.3">>, 0, round_floor, <<"-2.0">>},
         {<<"-1.5">>, 0, round_floor, <<"-2.0">>},
         {<<"-1.9">>, 0, round_floor, <<"-2.0">>},
         {<<"0.002">>, 2, round_floor, <<"0.0">>},
         {<<"-0.002">>, 2, round_floor, <<"-0.01">>},
         {<<"0.00001">>, 0, round_floor, <<"0.0">>},
         {<<"-0.00001">>, 0, round_floor, <<"-1.0">>},

         {<<"1">>, 1, round_cieling, <<"1.0">>},
         {<<"1.2">>, 1, round_cieling, <<"1.2">>},
         {<<"1.0">>, 0, round_cieling, <<"1.0">>},
         {<<"1.3">>, 0, round_cieling, <<"2.0">>},
         {<<"1.5">>, 0, round_cieling, <<"2.0">>},
         {<<"1.9">>, 0, round_cieling, <<"2.0">>},
         {<<"-1.0">>, 0, round_cieling, <<"-1.0">>},
         {<<"-1.3">>, 0, round_cieling, <<"-1.0">>},
         {<<"-1.5">>, 0, round_cieling, <<"-1.0">>},
         {<<"-1.9">>, 0, round_cieling, <<"-1.0">>},
         {<<"0.002">>, 2, round_cieling, <<"0.01">>},
         {<<"-0.002">>, 2, round_cieling, <<"0.0">>},
         {<<"0.00001">>, 0, round_cieling, <<"1.0">>},
         {<<"-0.00001">>, 0, round_cieling, <<"0.0">>},

         {<<"1">>, 1, round_half_up, <<"1.0">>},
         {<<"1.2">>, 1, round_half_up, <<"1.2">>},
         {<<"1.0">>, 0, round_half_up, <<"1.0">>},
         {<<"1.3">>, 0, round_half_up, <<"1.0">>},
         {<<"1.5">>, 0, round_half_up, <<"2.0">>},
         {<<"1.9">>, 0, round_half_up, <<"2.0">>},
         {<<"-1.0">>, 0, round_half_up, <<"-1.0">>},
         {<<"-1.3">>, 0, round_half_up, <<"-1.0">>},
         {<<"-1.5">>, 0, round_half_up, <<"-2.0">>},
         {<<"-1.9">>, 0, round_half_up, <<"-2.0">>},
         {<<"0.002">>, 2, round_half_up, <<"0.0">>},
         {<<"-0.002">>, 2, round_half_up, <<"0.0">>},
         {<<"0.00001">>, 0, round_half_up, <<"0.0">>},
         {<<"-0.00001">>, 0, round_half_up, <<"0.0">>},

         {<<"1">>, 1, round_half_down, <<"1.0">>},
         {<<"1.2">>, 1, round_half_down, <<"1.2">>},
         {<<"1.0">>, 0, round_half_down, <<"1.0">>},
         {<<"1.3">>, 0, round_half_down, <<"1.0">>},
         {<<"1.5">>, 0, round_half_down, <<"1.0">>},
         {<<"1.9">>, 0, round_half_down, <<"2.0">>},
         {<<"-1.0">>, 0, round_half_down, <<"-1.0">>},
         {<<"-1.3">>, 0, round_half_down, <<"-1.0">>},
         {<<"-1.5">>, 0, round_half_down, <<"-1.0">>},
         {<<"-1.9">>, 0, round_half_down, <<"-2.0">>},
         {<<"0.002">>, 2, round_half_down, <<"0.0">>},
         {<<"-0.002">>, 2, round_half_down, <<"0.0">>},
         {<<"0.00001">>, 0, round_half_down, <<"0.0">>},
         {<<"-0.00001">>, 0, round_half_down, <<"0.0">>},

         {<<"1">>, 1, round_down, <<"1.0">>},
         {<<"1.2">>, 1, round_down, <<"1.2">>},
         {<<"1.0">>, 0, round_down, <<"1.0">>},
         {<<"1.3">>, 0, round_down, <<"1.0">>},
         {<<"1.5">>, 0, round_down, <<"1.0">>},
         {<<"1.9">>, 0, round_down, <<"1.0">>},
         {<<"-1.0">>, 0, round_down, <<"-1.0">>},
         {<<"-1.3">>, 0, round_down, <<"-1.0">>},
         {<<"-1.5">>, 0, round_down, <<"-1.0">>},
         {<<"-1.9">>, 0, round_down, <<"-1.0">>},
         {<<"0.002">>, 2, round_down, <<"0.0">>},
         {<<"-0.002">>, 2, round_down, <<"0.0">>},
         {<<"0.00001">>, 0, round_down, <<"0.0">>},
         {<<"-0.00001">>, 0, round_down, <<"0.0">>}
        ],
    [
     {<<V/binary, " ", (list_to_binary(atom_to_list(R)))/binary>>,
      fun() ->
          Opts = #{ precision => P, rounding => R},
          D = decimal:to_decimal(V, Opts),
          Res = decimal:to_binary(D)
      end}
     || {V, P, R, Res} <- Tests
    ].

sum_test_() ->
    Tests =
        [
         {<<"1">>, <<"2">>, <<"3.0">>},
         {<<"1">>, <<"-2">>, <<"-1.0">>},
         {<<"1">>, <<"0.2">>, <<"1.2">>},
         {<<"1e3">>, <<"1e-3">>, <<"1000.001">>},
         {<<"123.456">>, <<"7e-4">>, <<"123.4567">>},
         {<<"1">>, <<"-1">>, <<"0.0">>}
        ],
    Opts = #{ precision => 5, rounding => round_floor},
    [
     {<<A/binary, $+, B/binary>>,
      fun() ->
          A1 = decimal:to_decimal(A, Opts),
          B1 = decimal:to_decimal(B, Opts),
          R = decimal:to_binary(decimal:add(A1, B1))
      end}
     || {A, B, R} <- Tests
    ].

sub_test_() ->
    Tests =
        [
         {<<"1">>, <<"2">>, <<"-1.0">>},
         {<<"1">>, <<"-2">>, <<"3.0">>},
         {<<"1">>, <<"0.2">>, <<"0.8">>},
         {<<"1e3">>, <<"1e-3">>, <<"999.999">>},
         {<<"123.456">>, <<"7e-4">>, <<"123.4553">>},
         {<<"1">>, <<"-1">>, <<"2.0">>}
        ],
    Opts = #{ precision => 5, rounding => round_floor},
    [
     {<<A/binary, $+, B/binary>>,
      fun() ->
          A1 = decimal:to_decimal(A, Opts),
          B1 = decimal:to_decimal(B, Opts),
          R = decimal:to_binary(decimal:sub(A1, B1))
      end}
     || {A, B, R} <- Tests
    ].


mult_test_() ->
    Tests =
        [
         {<<"1">>, <<"2">>, <<"2.0">>},
         {<<"1">>, <<"-2">>, <<"-2.0">>},
         {<<"1">>, <<"0.2">>, <<"0.2">>},
         {<<"1e3">>, <<"1e-3">>, <<"1.0">>},
         {<<"1">>, <<"0">>, <<"0.0">>},
         {<<"3e-1">>, <<"3e-1">>, <<"0.09">>}
        ],
    Opts = #{ precision => 100, rounding => round_floor},
    [
     {<<A/binary, $*, B/binary>>,
      fun() ->
          A1 = decimal:to_decimal(A, Opts),
          B1 = decimal:to_decimal(B, Opts),
          R = decimal:to_binary(decimal:mult(A1, B1))
      end}
     || {A, B, R} <- Tests
    ].

divide_test_() ->
    Tests =
        [
         {<<"1">>, <<"2">>, <<"0.5">>},
         {<<"1">>, <<"-2">>, <<"-0.5">>},
         {<<"1">>, <<"0.2">>, <<"5.0">>},
         {<<"1e3">>, <<"1e-3">>, <<"1.0e6">>},
         {<<"3e-1">>, <<"3e-1">>, <<"1.0">>},
         {<<"1">>, <<"3">>, <<"0.", (repeate($3, 100))/binary>>},
         {<<"2">>, <<"3">>, <<"0.", (repeate($6, 99))/binary, "7">>},
         {<<"1">>, <<"0">>, {error, badarith}}
        ],
    Opts = #{ precision => 100, rounding => round_half_up},
    [
     {<<A/binary, $/, B/binary>>,
      fun() ->
          A1 = decimal:to_decimal(A, Opts),
          B1 = decimal:to_decimal(B, Opts),
          R = try decimal:to_binary(decimal:divide(A1, B1, Opts)) catch Err:Res -> {Err,Res} end
      end}
     || {A, B, R} <- Tests
    ].

repeate(Ch, N) ->
    << <<Ch>> || _ <- lists:seq(1, N)>>.

minus_test() ->
    ?assertEqual({-1, 0}, decimal:minus({1,0})),
    ?assertEqual({1, 0}, decimal:minus({-1,0})).

abs_test() ->
    ?assertEqual({1, 0}, decimal:abs({1,0})),
    ?assertEqual({1, 0}, decimal:abs({-1,0})).

pretty_print_test_() ->
    Tests =
        [
         {{1, 0}, <<"1.0">>},
         {{1, 1}, <<"10.0">>},
         {{1, 2}, <<"100.0">>},
         {{1, 3}, <<"1000.0">>},
         {{1, 4}, <<"10000.0">>},
         {{1, 5}, <<"100000.0">>},
         {{1, 6}, <<"1.0e6">>},

         {{1, -1}, <<"0.1">>},
         {{1, -2}, <<"0.01">>},
         {{1, -3}, <<"0.001">>},
         {{1, -4}, <<"0.0001">>},
         {{1, -5}, <<"0.00001">>},
         {{1, -6}, <<"1.0e-6">>},

         {{123, 0}, <<"123.0">>},
         {{123, 1}, <<"1230.0">>},
         {{123, 2}, <<"12300.0">>},
         {{123, 3}, <<"123000.0">>},
         {{123, 4}, <<"1.23e6">>},

         {{123, -1}, <<"12.3">>},
         {{123, -2}, <<"1.23">>},
         {{123, -3}, <<"0.123">>},
         {{123, -4}, <<"0.0123">>},
         {{123, -5}, <<"0.00123">>},
         {{123, -6}, <<"0.000123">>},
         {{123, -7}, <<"0.0000123">>},
         {{123, -8}, <<"1.23e-6">>}
        ],
    NegTests = [{{-B, E}, <<$-, R/binary>>} || {{B, E}, R} <- Tests],
    [
     {iolist_to_binary(io_lib:format("~pe~p", [B,E])),
      fun() ->
          R = decimal:to_binary(D)
      end}
     || {D={B,E}, R} <- Tests ++ NegTests
    ].

from_float_test_() ->
    Tests =
        [
         {{1,  2}, 100},
         {{1,  1}, 10},
         {{1,  0}, 1.0},
         {{0,  0}, 0.0},
         {{1, -1}, 0.1},
         {{1, -2}, 0.01},
         {{1, -3}, 0.001},
         {{1, -4}, 0.0001},
         {{1, -5}, 0.00001},
         {{1, -6}, 0.000001},

         {{185,  2}, 18500},
         {{185,  1}, 1850},
         {{185,  0}, 185.0},
         {{185, -1}, 18.5},
         {{185, -2}, 1.85},
         {{185, -3}, 0.185},
         {{185, -4}, 0.0185},
         {{185, -5}, 0.00185},
         {{185, -6}, 0.000185}
        ],
    NegTests = [{{-B, E}, -F} || {{B, E}, F} <- Tests],

    Opts = #{precision => 100, rounding => round_half_up},
    [
     {iolist_to_binary(io_lib:format("~p", [F])),
      fun() ->
          D = decimal:to_decimal(F, Opts)
      end}
     || {D, F} <- Tests ++ NegTests
    ].

error_badarg_test_() ->
    Tests =
        [
         <<"bad">>,
         <<"-bad">>,
         <<"-1bad">>,
         <<"-1.bad">>,
         <<"-1.0bad">>,
         <<"-1.0ebad">>,
         <<"-1.0e-bad">>,
         <<"-1.0e-1bad">>,
         <<"-1.0e+bad">>
        ],
    Opts = #{precision => 100, rounding => round_half_up},
    [
     {D,
      fun() ->
          ?assertException(error, badarg, decimal:to_decimal(D, Opts))
      end} || D <- Tests
    ].

cmp_test_() ->
    Tests =
        [
         { {0,0}, {0,0},   0 },
         { {1,0}, {1,0},   0 },
         { {-1,0}, {-1,0}, 0 },
         { {0,0}, {-1,0},  1 },
         { {1,0}, {0,0},   1 },
         { {-1,0}, {0,0}, -1 },
         { {0,0}, {1,0},  -1 },

         { {0, -100}, {0, 100}, 0},
         { {10, 0}, {1, 1}, 0 },
         { {123, -2}, {122, -2}, 1 },
         { {-123, -2}, {-122, -2}, -1 }
        ],
    Opts = #{precision => 100, rounding => round_half_up},
    [
     {iolist_to_binary(
          [
           decimal:to_binary(A),
           case R of
               1 -> $>;
               0 -> $=;
               -1 -> $<
           end,
           decimal:to_binary(B)
          ]),
      fun() ->
          R = decimal:cmp(A, B, Opts)
      end} || {A, B, R} <- Tests
    ].

to_binary_test_() ->
    Tests =
        [
         {true, {1, 0}, <<"1.0">>},
         {true, {10, 0}, <<"10.0">>},
         {true, {1, 6}, <<"1.0e6">>},
         {true, {123, -2}, <<"1.23">>},
         {true, {123456, -10}, <<"0.0000123456">>},
         {true, {123456, -11}, <<"1.23456e-6">>},

         {true, {-1, 0}, <<"-1.0">>},
         {true, {-10, 0}, <<"-10.0">>},
         {true, {-1, 6}, <<"-1.0e6">>},
         {true, {-123, -2}, <<"-1.23">>},
         {true, {-123456, -10}, <<"-0.0000123456">>},
         {true, {-123456, -11}, <<"-1.23456e-6">>},

         {false, {1, 0}, <<"1.0">>},
         {false, {10, 0}, <<"10.0">>},
         {false, {1, 6}, <<"1000000.0">>},
         {false, {123, -2}, <<"1.23">>},
         {false, {123456, -10}, <<"0.0000123456">>},
         {false, {123456, -11}, <<"0.00000123456">>},

         {false, {-1, 0}, <<"-1.0">>},
         {false, {-10, 0}, <<"-10.0">>},
         {false, {-1, 6}, <<"-1000000.0">>},
         {false, {-123, -2}, <<"-1.23">>},
         {false, {-123456, -10}, <<"-0.0000123456">>},
         {false, {-123456, -11}, <<"-0.00000123456">>}
        ],
    [
     {iolist_to_binary(
          [
           case P of
               true -> "(pretty) ";
               false -> ""
           end,
           io_lib:format("~p", [D]),
           $=,
           R
          ]),
      fun() ->
          R = decimal:to_binary(D, #{pretty => P})
      end} || {P, D, R} <- Tests
    ].

to_decimal_test_() ->
    Opts = #{ precision => 5, rounding => round_half_up },
    Tests =
        [
         {<<"from decimal">>, {1,0}, {1, 0}},
         {<<"from deprecated decimal">>, {1,1,0}, {-1, 0}}
        ],
    [
     {N, fun() -> R = decimal:to_decimal(V, Opts) end}
     || {N, V, R} <- Tests
    ] ++ [
       {<<"constructor">>, fun() ->
           ?assertEqual({1,3}, decimal:to_decimal(1,3,Opts))
       end}
    ].

-endif.
