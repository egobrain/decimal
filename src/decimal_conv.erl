-module(decimal_conv).

-export([
         from_binary/1,
         to_binary/1
        ]).

-spec from_binary(binary()) -> decimal:decimal().
from_binary(Bin) ->
    parse_sign(Bin).

-spec to_binary(decimal:decimal()) -> binary().
to_binary({Int, 0}) ->
    <<(integer_to_binary(Int))/binary, ".0">>;
to_binary({Int, E}) ->
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
    {Sign*Base, E}.

parse_exp_sign(<<$-, Rest/binary>>, Base, E) ->
    parse_exp(Rest, Base, E, -1, 0);
parse_exp_sign(<<$+, Rest/binary>>, Base, E) ->
    parse_exp(Rest, Base, E, 1, 0);
parse_exp_sign(<<>>, _Base, _E) ->
    error(badarg);
parse_exp_sign(Bin, Base, E) ->
    parse_exp(Bin, Base, E, 1, 0).

parse_exp(<<Char, Rest/binary>>, Base, E, ExpSign, Exp) when
      Char >= $0, Char =< $9 ->
    parse_exp(Rest, Base, E, ExpSign, Exp*10+Char-$0);
parse_exp(<<>>, Base, E, ExpSign, Exp) ->
    {Base, ExpSign*Exp+E};
parse_exp(_, _Base, _E, _ExpSign, _Exp) ->
    error(badarg).
