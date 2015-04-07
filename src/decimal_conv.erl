-module(decimal_conv).

-export([
         from_binary/1,
         from_list/1,
         from_float/1,

         to_binary/1
        ]).

-spec from_binary(binary()) -> decimal:decimal().
from_binary(Bin) ->
    from_list(binary_to_list(Bin)).

-spec to_binary(decimal:decimal()) -> binary().
to_binary(Decimal) ->
    to_binary_(decimal:reduce(Decimal)).
to_binary_({Int, 0}) ->
    <<(integer_to_binary(Int))/binary, ".0">>;
to_binary_({Int, E}) ->
    Sign =
        case Int < 0 of
            true -> <<$->>;
            false -> <<>>
        end,
    Bin = integer_to_binary(abs(Int)),
    Size = byte_size(Bin),
    case Size + E - 1 of
        AE when E < 0 andalso AE > -6 ->
            case AE < 0 of
                true ->
                    <<Sign/binary, "0.",
                      (binary:copy(<<$0>>, -(AE+1)))/binary, Bin/binary>>;
                false ->
                    Shift = AE+1,
                    <<B:Shift/binary, R/binary>> = Bin,
                    <<Sign/binary, B/binary, $., R/binary>>
            end;
        AE when E >= 0 andalso AE < 6 ->
            <<Sign/binary, Bin/binary, (binary:copy(<<$0>>, E))/binary, ".0">>;
        AE when Size =:= 1->
            <<Sign/binary, Bin/binary, ".0", (e(AE))/binary>>;
        AE ->
            <<B:1/binary, R/binary>> = Bin,
            <<Sign/binary, B/binary, $., R/binary, (e(AE))/binary>>
    end.

e(0) -> <<>>;
e(E) -> <<$e, (integer_to_binary(E))/binary>>.

%% =============================================================================
%%% List parser
%% =============================================================================

from_list(List) ->
    parse_sign(List).

parse_sign([Char = $-|Rest]) ->
    parse_base(Rest, [Char]);
parse_sign(List) ->
    parse_base(List, []).

parse_base([Char|Rest], Base) when Char >= $0, Char =< $9 ->
    parse_base(Rest, [Char|Base]);
parse_base([$.|Rest], Base) ->
    parse_fraction(Rest, Base, 0);
parse_base([Char|Rest], Base) when Char =:= $e; Char =:= $E ->
    parse_exp_sign(Rest, Base, 0);
parse_base([], Base) ->
    {list_to_integer(lists:reverse(Base)), 0}.

parse_fraction([Char|Rest], Base, E) when Char >= $0, Char =< $9 ->
    parse_fraction(Rest, [Char|Base], E-1);
parse_fraction([Char|Rest], Base, E) when Char =:= $e; Char =:= $E ->
    parse_exp_sign(Rest, Base, E);
parse_fraction([], Base, E) ->
    {list_to_integer(lists:reverse(Base)), E}.

parse_exp_sign([Char|Rest], Base, E) when Char =:= $-; Char =:= $+ ->
    parse_exp(Rest, Base, E, [Char]);
parse_exp_sign([], _Base, _E) ->
    error(badarg);
parse_exp_sign(List, Base, E) ->
    parse_exp(List, Base, E, []).

parse_exp([Char|Rest], Base, E, Exp) when Char >= $0, Char =< $9 ->
    parse_exp(Rest, Base, E, [Char|Exp]);
parse_exp([], Base, E, Exp) ->
    {list_to_integer(lists:reverse(Base)),
     list_to_integer(lists:reverse(Exp))+E};
parse_exp(_, _Base, _E, _Exp) ->
    error(badarg).

%% =============================================================================
%%% From float
%% =============================================================================

from_float(0.0) ->
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
            fixup(R, S * decimal_utils:pow_of_ten(Est), MPlus, MMinus, Est,
                  LowOk, HighOk);
        true ->
            Scale = decimal_utils:pow_of_ten(-Est),
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

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).
log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).
