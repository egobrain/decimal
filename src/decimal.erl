-module(decimal).

%% Converters
-export([
         to_decimal/2,
         to_decimal/3,
         to_binary/1,
         to_binary/2
        ]).

%% Arith
-export([
         add/2,
         sub/2,
         mult/2,
         divide/3
        ]).

%% Compare
-export([
         cmp/3
        ]).

%% Utils
-export([
         abs/1,
         minus/1,
         is_zero/1,
         reduce/1,
         round/3
        ]).

-compile(inline).

-type decimal() :: {integer(), integer()}.
-type old_decimal() :: {0|1, non_neg_integer(), integer()}.
-type rounding_algorithm() :: round_floor | round_ceiling |
                              round_half_up | round_half_down |
                              round_down.
-type opts() :: #{
              precision => non_neg_integer(),
              rounding => rounding_algorithm()
           }.

-export_type([
              decimal/0,
              opts/0,
              rounding_algorithm/0
             ]).

%% =============================================================================
%%% API
%% =============================================================================

%% = Converters ================================================================

-spec to_decimal(Value, Opts) -> decimal() when
      Value :: integer() | float() | binary() | list() |
               decimal() | old_decimal(),
      Opts :: opts().
to_decimal({Base, Exp}=Decimal, _Ots) when
      is_integer(Base), is_integer(Exp) ->
    Decimal;
to_decimal(Int, #{precision := Precision, rounding := Rounding}) when
      is_integer(Int) ->
    round(Rounding, {Int, 0}, Precision);
to_decimal(Binary, #{precision := Precision, rounding := Rounding}) when
      is_binary(Binary) ->
    Decimal = decimal_conv:from_binary(Binary),
    round(Rounding, Decimal, Precision);
to_decimal(Float, #{precision := Precision, rounding := Rounding}) when
      is_float(Float) ->
    Decimal = decimal_conv:from_float(Float),
    round(Rounding, Decimal, Precision);
to_decimal(List, #{precision := Precision, rounding := Rounding}) when
      is_list(List) ->
    Decimal = decimal_conv:from_list(List),
    round(Rounding, Decimal, Precision);
%% Old decimal format support
to_decimal({Sign, Base0, Exp}, _Opts) when
      is_integer(Sign), is_integer(Base0), is_integer(Exp) ->
    Base = case Sign of 1 -> -Base0; 0 -> Base0 end,
    {Base, Exp}.

-spec to_decimal(Base, Exp, Opts) -> decimal() when
      Base :: integer(),
      Exp :: integer(),
      Opts :: opts().
to_decimal(Base, Exp, _Opts) ->
    {Base, Exp}.

-spec to_binary(decimal()) -> binary().
to_binary(Decimal) ->
    decimal_conv:to_binary(Decimal, #{pretty => true}).

-spec to_binary(decimal(), Opts) -> binary() when
      Opts :: decimal_conv:binary_opts().
to_binary(Decimal, Opts) ->
    decimal_conv:to_binary(Decimal, Opts).

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
divide(A, B, #{ precision := Precision, rounding := Rounding }) ->
    {Int1, E1} = round(Rounding, A, Precision),
    B2 = round(Rounding, B, Precision),
    case is_zero(B2) of
        true -> error(badarith);
        _ ->
            {Int2, E2} = B2,
            Emin = min(E1, E2),
            Int =
                (Int1*pow_of_ten(E1-Emin+Precision+1)) div
                (Int2*pow_of_ten(E2-Emin)),
            round(Rounding, {Int, -Precision-1}, Precision)
    end.

%% = Compare ===================================================================

-spec cmp(decimal(), decimal(), opts()) -> -1 | 0 | 1.
cmp({0, _}, {0, _}, _Opts) ->
    0;
cmp({Int1, _}, {Int2, _}, _Opts) when Int1 >= 0, Int2 =< 0 ->
    1;
cmp({Int1, _}, {Int2, _}, _Opts) when Int1 =< 0, Int2 >= 0 ->
    -1;
cmp(A, B, #{ precision := Precision, rounding := Rounding }) ->
    {Int1, E1} = round(Rounding, A, Precision),
    {Int2, E2} = round(Rounding, B, Precision),

    Emin = min(E1, E2),
    B1 = Int1*pow_of_ten(E1-Emin),
    B2 = Int2*pow_of_ten(E2-Emin),
    if B1 =:= B2 -> 0;
       B1 < B2 -> -1;
       B1 > B2 -> 1
    end.

%% = Utils =====================================================================

-spec is_zero(decimal()) -> boolean().
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

-spec round(rounding_algorithm(), decimal(), non_neg_integer()) -> decimal().
round(Rounding, {Int, E}=Decimal, Precision) ->
    reduce(
        case -Precision-E of
            Delta when Delta > 0 ->
                round_(Rounding, Int, E, Delta);
            _ ->
                Decimal
        end).

round_(round_down, Int, E, Delta) ->
    P = pow_of_ten(Delta),
    Base = Int div P,
    zero_exp_(Base, E+Delta);
round_(Rounding, Int, E, Delta) when
      Rounding =:= round_ceiling;
      Rounding =:= round_floor ->
    P = pow_of_ten(Delta),
    Base0 = Int div P,
    Diff = Int-Base0*P,
    Base =
        case Rounding of
            round_floor when Diff < 0 ->
                Base0 - 1;
            round_ceiling when Diff > 0 ->
                Base0 + 1;
            _ ->
                Base0
        end,
    zero_exp_(Base, E+Delta);
round_(Rounding, Int, E, Delta) ->
    P = pow_of_ten(Delta-1),
    Data = Int div P,
    Base0 = Data div 10,
    LastDigit = erlang:abs(Data-(Base0*10)),
    Base =
        case Rounding of
            round_half_up when LastDigit >= 5, Data > 0 ->
                Base0 + 1;
            round_half_up when LastDigit >= 5, Data < 0  ->
                Base0 - 1;
            round_half_down when LastDigit > 5, Data > 0 ->
                Base0 + 1;
            round_half_down when LastDigit > 5, Data < 0  ->
                Base0 - 1;
            _ ->
                Base0
        end,
    zero_exp_(Base, E+Delta).

zero_exp_(0, _Exp) -> {0,0};
zero_exp_(Base, Exp) -> {Base, Exp}.

%% =============================================================================
%%% Internal functions
%% =============================================================================

-spec pow_of_ten(non_neg_integer()) -> pos_integer().
pow_of_ten(N) ->
    int_pow(10, N).

int_pow(X, 0) when is_integer(X) ->
    1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).
