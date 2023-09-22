-module(decimal_conv).

-export([
         from_binary/1,
         from_list/1,
         from_float/1,

         to_binary/2
        ]).

-compile(inline).

-type binary_opts() :: #{ pretty => boolean() }.
-export_type([binary_opts/0]).

-spec from_binary(binary()) -> decimal:decimal().
from_binary(Bin) ->
    parse_base(Bin, <<>>).

-spec to_binary(decimal:decimal(), Opts) -> binary() when
      Opts :: binary_opts().
to_binary({Int, 0}, _Opts) ->
    <<(integer_to_binary(Int))/binary, ".0">>;
to_binary({Int, E}, #{pretty := Pretty}) ->
    Sign =
        case Int < 0 of
            true -> <<$->>;
            false -> <<>>
        end,
    Bin = integer_to_binary(abs(Int)),
    Size = byte_size(Bin),
    case Size + E - 1 of
        AE when E < 0 andalso ((not Pretty) orelse (AE > -6)) ->
            case AE < 0 of
                true ->
                    <<Sign/binary, "0.",
                      (binary:copy(<<$0>>, -(AE+1)))/binary, Bin/binary>>;
                false ->
                    Shift = AE+1,
                    <<B:Shift/binary, R/binary>> = Bin,
                    <<Sign/binary, B/binary, $., R/binary>>
            end;
        AE when E >= 0 andalso ((not Pretty) orelse (AE < 6)) ->
            <<Sign/binary, Bin/binary,(binary:copy(<<$0>>, E))/binary, ".0">>;
        AE when Size =:= 1->
            <<Sign/binary, Bin/binary, ".0", (e(AE))/binary>>;
        AE ->
            <<B:1/binary, R/binary>> = Bin,
            <<Sign/binary, B/binary, $., R/binary, (e(AE))/binary>>
    end.

e(0) -> <<>>;
e(E) -> <<$e, (integer_to_binary(E))/binary>>.

%% =============================================================================
%%% Binary string parser
%% =============================================================================

from_list(List) when is_list(List) ->
    from_binary(list_to_binary(List)).

parse_base(<<$-, Rest/binary>>, <<>>) ->
    parse_base(Rest, <<$->>);
parse_base(<<$+, Rest/binary>>, <<>>) ->
    parse_base(Rest, <<>>);
parse_base(<<$., Rest/binary>>, Acc) ->
    parse_fraction(Rest, Acc, 0);
parse_base(<<X, Rest/binary>>, Acc) when X >= $0, X =< $9 ->
    parse_base(Rest, <<Acc/binary, X>>);
parse_base(<<X, Rest/binary>>, Acc) when X =:= $E; X =:= $e ->
    parse_exp(Rest, Acc, 0, <<>>);
parse_base(<<>>, Acc) ->
    {binary_to_integer(Acc),0};
parse_base(_,_) ->
    error(badarg).

parse_fraction(<<X, Rest/binary>>, Acc, E) when X >= $0, X =< $9 ->
    parse_fraction(Rest, <<Acc/binary, X >>, E-1);
parse_fraction(<<X, Rest/binary>>, Acc, E) when X =:= $E; X =:= $e ->
    parse_exp(Rest, Acc, E, <<>>);
parse_fraction(<<>>, Acc, E) ->
    {binary_to_integer(Acc), E};
parse_fraction(_,_,_) ->
    error(badarg).

parse_exp(<<$-, Rest/binary>>, Base, E, <<>>) ->
    parse_exp(Rest, Base, E, <<$->>);
parse_exp(<<$+, Rest/binary>>, Base, E, <<>>) ->
    parse_exp(Rest, Base, E, <<>>);
parse_exp(<<X, Rest/binary>>, Base, E, Acc) when X >= $0, X =< $9 ->
    parse_exp(Rest, Base, E, <<Acc/binary, X>>);
parse_exp(<<>>, Base, E, Acc) ->
    {binary_to_integer(Base), E+binary_to_integer(Acc)};
parse_exp(_,_,_,_) ->
    error(badarg).

%% =============================================================================
%%% From float
%% =============================================================================

from_float(Num) when Num == 0.0 ->
    {0, 0};
from_float(Float) when is_float(Float) ->
    {Frac, Exp} = mantissa_exponent(Float),
    {Place, Digits} = from_float_(Float, Exp, Frac),
    Decimal = {B,E} = to_decimal(Place, [$0 + D || D <- Digits]),
    case Float < 0.0 of
        true -> {-B, E};
        false -> Decimal
    end.

-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).
mantissa_exponent(F) ->
    case <<F:64/float>> of
        <<_S:1, 0:11, M:52>> -> % denormalized
            E = log2floor(M),
            {M bsl (53 - E), E - 52 - 1075};
        <<_S:1, BE:11, M:52>> when BE < 2047 ->
            {M + ?BIG_POW, BE - 1075}
    end.
from_float_(Float, Exp, Frac) ->
    Round = (Frac band 1) =:= 0,
    if
        Exp >= 0 ->
            BExp = 1 bsl Exp,
            if
                Frac =:= ?BIG_POW ->
                    scale(Frac * BExp * 4, 4, BExp * 2, BExp,
                          Round, Round, Float);
                true ->
                    scale(Frac * BExp * 2, 2, BExp, BExp,
                          Round, Round, Float)
            end;
        Exp < ?MIN_EXP ->
            BExp = 1 bsl (?MIN_EXP - Exp),
            scale(Frac * 2, 1 bsl (1 - Exp), BExp, BExp,
                  Round, Round, Float);
        Exp > ?MIN_EXP, Frac =:= ?BIG_POW ->
            scale(Frac * 4, 1 bsl (2 - Exp), 2, 1,
                  Round, Round, Float);
        true ->
            scale(Frac * 2, 1 bsl (1 - Exp), 1, 1,
                  Round, Round, Float)
    end.
scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    %% Note that the scheme implementation uses a 326 element look-up
    %% table for int_pow(10, N) where we do not.
    if
        Est >= 0 ->
            fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
                  LowOk, HighOk);
        true ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
                  LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = if
                 HighOk -> R + MPlus >= S;
                 true -> R + MPlus > S
             end,
    case TooLow of
        true ->
            {K + 1, generate(R, S, MPlus, MMinus, LowOk, HighOk)};
        false ->
            {K, generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)}
    end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    D = R0 div S,
    R = R0 rem S,
    TC1 = if
              LowOk -> R =< MMinus;
              true -> R < MMinus
          end,
    TC2 = if
              HighOk -> R + MPlus >= S;
              true -> R + MPlus > S
          end,
    case {TC1, TC2} of
        {false, false} ->
            [D | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)];
        {false, true} ->
            [D + 1];
        {true, false} ->
            [D];
        {true, true} when R * 2 < S ->
            [D];
        {true, true} ->
            [D + 1]
    end.

to_decimal(Place, S) ->
    {list_to_integer(S), Place - length(S)}.

int_ceil(X) when is_float(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

int_pow(X, 0) when is_integer(X) ->
    1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).
log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).
