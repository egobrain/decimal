-module(decimal_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
         {<<"-0.002">>, 2, round_floor, <<"-1.0e-2">>},
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
         {<<"0.002">>, 2, round_cieling, <<"1.0e-2">>},
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
         {<<"1e3">>, <<"1e-3">>, <<"1.000001e3">>},
         {<<"123.456">>, <<"7e-4">>, <<"1.234567e2">>},
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

mult_test_() ->
    Tests =
        [
         {<<"1">>, <<"2">>, <<"2.0">>},
         {<<"1">>, <<"-2">>, <<"-2.0">>},
         {<<"1">>, <<"0.2">>, <<"2.0e-1">>},
         {<<"1e3">>, <<"1e-3">>, <<"1.0">>},
         {<<"1">>, <<"0">>, <<"0.0">>},
         {<<"3e-1">>, <<"3e-1">>, <<"9.0e-2">>}
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
         {<<"1">>, <<"2">>, <<"5.0e-1">>},
         {<<"1">>, <<"-2">>, <<"-5.0e-1">>},
         {<<"1">>, <<"0.2">>, <<"5.0">>},
         {<<"1e3">>, <<"1e-3">>, <<"1.0e6">>},
         {<<"3e-1">>, <<"3e-1">>, <<"1.0">>},
         {<<"1">>, <<"3">>, <<"3.", (repeate($3, 99))/binary, "e-1">>},
         {<<"2">>, <<"3">>, <<"6.", (repeate($6, 98))/binary, "7e-1">>}
        ],
    Opts = #{ precision => 100, rounding => round_half_up},
    [
     {<<A/binary, $/, B/binary>>,
      fun() ->
          A1 = decimal:to_decimal(A, Opts),
          B1 = decimal:to_decimal(B, Opts),
          R = decimal:to_binary(decimal:divide(A1, B1, Opts))
      end}
     || {A, B, R} <- Tests
    ].

repeate(Ch, N) ->
    << <<Ch>> || _ <- lists:seq(1, N)>>.

-endif.
