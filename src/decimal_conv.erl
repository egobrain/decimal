-module(decimal_conv).

-export([
         from_binary/1,
         to_binary/1
        ]).

-spec from_binary(binary()) -> {ok, decimal:decimal()} | {error, not_number}.
from_binary(Bin) ->
    parse_sign(Bin).

-spec to_binary(decimal:decimal()) -> binary().
to_binary({Int, E}) ->
    Shift =
        case Int < 0 of
            true -> 1;
            false -> 0
        end,
    Bin = integer_to_binary(Int),
    Size = byte_size(Bin),
    case Size-Shift of
        1 ->
            <<Bin/binary, $e, (integer_to_binary(E))/binary>>;
        _ ->
            S = (Shift+1),
            <<B:S/binary, R/binary>> = Bin,
            <<B/binary, $., R/binary, $e, (integer_to_binary(E+Size-S))/binary>>
    end.

%% =============================================================================
%%% Parser
%% =============================================================================

parse_sign(<<$-, Rest/binary>>) ->
    parse_base(Rest, -1, 0);
parse_sign(Bin) ->
    parse_base(Bin, 1, 0).

parse_base(<<Char, Rest/binary>>, Sign, Base) ->
    case Char of
        _ when Char >= $0, Char =< $9 ->
            parse_base(Rest, Sign, Base*10+Char-$0);
        $. ->
            parse_fraction(Rest, Sign, Base, 0);
        _ when Char =:= $e; Char =:= $E ->
            parse_exp_sign(Rest, Sign*Base, 0)
    end;
parse_base(<<>>, Sign, Base) ->
    {Sign*Base, 0}.

parse_fraction(<<Char, Rest/binary>>, Sign, Base, E) ->
    case Char of
        _ when Char >= $0, Char =< $9 ->
            parse_fraction(Rest, Sign, Base*10+Char-$0, E-1);
        _ when Char =:= $e; Char =:= $E ->
            parse_exp_sign(Rest, Sign*Base, E)
    end;
parse_fraction(<<>>, Sign, Base, E) ->
    {ok, {Sign*Base, E}}.

parse_exp_sign(<<$-, Rest/binary>>, Base, E) ->
    parse_exp(Rest, Base, E, -1, 0);
parse_exp_sign(<<>>, _Base, _E) ->
    {error, bad_number};
parse_exp_sign(Bin, Base, E) ->
    parse_exp(Bin, Base, E, 1, 0).

parse_exp(<<Char, Rest/binary>>, Base, E, ExpSign, Exp) when
      Char >= $0, Char =< $9 ->
    parse_exp(Rest, Base, E, ExpSign, Exp*10+Char-$0);
parse_exp(<<>>, Base, E, ExpSign, Exp) ->
    {ok, {Base, ExpSign*Exp+E}}.
