-module(decimal).

-export([
         to_decimal/2
        ]).

-export([
         reduce/1,
         round/2
        ]).

-compile(export_all).

-type decimal() :: {integer(), integer()}.
-type opts() :: #{ precision => non_neg_integer()}.

-spec to_decimal(Value, Opts) -> {ok, decimal()} | {error, Reason} when
      Value :: integer() | float() | binary() | list() | decimal(),
      Opts :: opts(),
      Reason :: any().
to_decimal({Int, E}=Decimal, #{precision := Precision}) when
      is_integer(Int), is_integer(E) ->
    Rounded = round(Decimal, Precision),
    {ok, Rounded};
to_decimal(Int, #{precision := Precision}) when
      is_integer(Int) ->
    Rounded = round({Int, 0}, Precision),
    {ok, Rounded};
to_decimal(Binary, #{precision := Precision}) when is_binary(Binary) ->
    case decimal_conv:from_binary(Binary) of
        {ok, Decimal} ->
            {ok, round(Decimal, Precision)};
        {error, _Reason} = Err -> Err
    end;
to_decimal(Float, Opts) when
      is_float(Float) ->
    Bin = float_to_binary(Float),
    to_decimal(Bin, Opts);
to_decimal(List, Opts) ->
    Bin = list_to_binary(List),
    to_decimal(Bin, Opts).

%% = Arith =====================================================================

-spec add(decimal(), decimal()) -> decimal().
add({Int1, E1}, {Int2, E2}) ->
    Emin = min(E1, E2),
    {Int1 * pow_of_ten(E1 - Emin) +
     Int2 * pow_of_ten(E2 - Emin),
     Emin}.

-spec sub(decimal(), decimal()) -> decimal().
sub(A, B) ->
    add(A, minus(B)).

-spec mult(decimal(), decimal()) -> decimal().
mult({Int1, E1}, {Int2, E2}) ->
    {Int1*Int2, E1+E2}.

-spec divide(decimal(), decimal(), opts()) -> decimal().
divide(A, B, #{ precision := Precision}) ->
    {Int1, E1} = round(A, Precision),
    {Int2, E2} = round(B, Precision),
    case is_zero(B) of
        true -> error(badarith);
        _ ->
            Emin = min(E1, E2),
            Int =
                (Int1*pow_of_ten(E1-Emin+Precision)) div
                (Int2*pow_of_ten(E2-Emin)),
            reduce({Int, -Precision})
    end.

%% = Compare ===================================================================

-spec cmp(decimal(), decimal(), opts()) -> -1 | 0 | 1.
cmp(A, B, #{ precision := Precision }) ->
    {Int1, E1} = round(A, Precision),
    {Int2, E2} = round(B, Precision),

    Emin = min(E1, E2),
    B1 = Int1*pow_of_ten(E1-Emin),
    B2 = Int2*pow_of_ten(E2-Emin),
    if B1 =:= B2 -> 0;
       B1 < B2 -> -1;
       B1 > B2 -> 1
    end.

%% = Utils =====================================================================

is_zero({0, _}) -> true;
is_zero(_) -> false.

-spec minus(decimal()) -> decimal().
minus({Int, E}) ->
    {-Int, E}.

-spec abs(decimal()) -> decimal().
abs({Int, E}) ->
    {erlang:abs(Int), E}.

-spec reduce(decimal()) -> decimal().
reduce({Int, E}) ->
    reduce_(Int, E).
reduce_(0, _E) -> {0, 0};
reduce_(Int, E) ->
    case Int rem 10 of
        0 -> reduce_(Int div 10, E+1);
        _ -> {Int, E}
    end.

-spec round(decimal(), non_neg_integer()) -> decimal().
round(Decimal, Precision) ->
    {Int, E} = reduce(Decimal),
    case -Precision-E of
        Delta when Delta > 0 ->
            P = pow_of_ten(Delta),
            {Int div P, E+Delta};
        _ ->
            Decimal
    end.

%% =============================================================================
%%% Internal functions
%% =============================================================================

-spec pow_of_ten(non_neg_integer()) -> pos_integer().
pow_of_ten(N) ->
    pow_of_ten_(N, 1).
pow_of_ten_(0, Acc) -> Acc;
pow_of_ten_(N, Acc) -> pow_of_ten_(N-1, Acc*10).
