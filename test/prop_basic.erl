-module(prop_basic).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("decimal.hrl").
-import(decimal, [divide/3, mult/2, add/2, sub/2, cmp/2, is_zero/1, to_binary/1, to_decimal/2]).

%% check (R1 + R2) - R2 == R1
prop_add_sub() ->
    ?FORALL({D1, D2, Opts}, {decimal(), decimal(), opts()},
            eq(D1, sub(add(D1, D2), D2), Opts)).

%% check (R1*R2)/R2 == R1
prop_mult_div() ->
    ?FORALL({D1, D2, Opts}, {decimal(), non_zero_decimal(), opts()},
            eq(D1, divide(mult(D1, D2), D2, Opts), Opts)).

%% check ?max(D1,D2) - ?min(D1,D2) >= 0
prop_max_min_ge() ->
    ?FORALL({D1, D2}, {decimal(), decimal()},
            ?ge(?sub(?max(D1,D2), ?min(D1,D2)), ?d_zero)).

prop_to_from_binary() ->
    ?FORALL({D, #{precision := P, rounding := R} = Opts}, {decimal(), opts()},
            eq(decimal:round(R, D, P), to_decimal(to_binary(D) ,Opts), Opts)).

%% ---------------------------------------------------------------
%% Generators
%% ---------------------------------------------------------------
non_zero_decimal() ->
    ?SUCHTHAT(Dec, decimal(), not is_zero(Dec)).

decimal() ->
    {integer(), integer()}.

opts() ->
    ?LET({R,P}, {rounding_algorithm(), pos_integer()}, #{precision => P, rounding => R}).

rounding_algorithm() ->
    oneof([round_floor,round_ceiling,round_half_up,round_half_down,round_down]).

eq(A,B, #{precision := P}) ->
    Diff = decimal:abs(sub(decimal:abs(A),decimal:abs(B))),
    cmp(Diff, {1,-P}) =< 0 orelse io:format("A: ~p~nB: ~p~nDiff: ~p~nPrec: ~p~n", [A, B, Diff, P]) .
