-ifndef(DECIMAL_HRL).
-define(DECIMAL_HRL, true).

%% =============================================================================
%%% Common decimal functions
%% =============================================================================

-ifndef(d_pretty).
-define(d_pretty, true).
-endif.

-ifndef(d_precision).
-define(d_precision, 100).
-endif.

-ifndef(d_rounding).
-define(d_rounding, round_half_up).
-endif.

-define(d_context, #{ precision => ?d_precision, rounding => ?d_rounding}).

-define(is_decimal(D),
        is_tuple(D) andalso
        tuple_size(D) =:= 2 andalso
        is_integer(element(1, D)) andalso
        is_integer(element(2, D))).

-define(to_decimal(X), decimal:to_decimal(X, ?d_context)).
-define(to_decimal(B,E), decimal:to_decimal(B,E, ?d_context)).

-ifdef(d_no_autocast).
-define(d_cast(X), X).
-else.
-define(d_cast(X), ?to_decimal(X)).
-endif.

-define(to_binary(X), decimal:to_binary(?d_cast(X),#{pretty=>?d_pretty})).
-define(to_string(X), binary_to_list(?to_binary(X))).
-define(to_float(X), binary_to_float(?to_binary(X))).

-define(d_zero,{0,0}).
-define(add(X,Y),decimal:add(?d_cast(X),?d_cast(Y))).
-define(sub(X,Y),decimal:sub(?d_cast(X),?d_cast(Y))).
-define(mult(X,Y),decimal:mult(?d_cast(X),?d_cast(Y))).
-define(divide(X,Y),decimal:divide(?d_cast(X),?d_cast(Y),?d_context)).
-define(sqrt(X),decimal:sqrt(?d_cast(X),?d_context)).
-define(abs(X),decimal:abs(?d_cast(X))).
-define(neg(X),decimal:minus(?d_cast(X))).
-define(sign(X),case element(1,?d_cast(X))<0 of true -> -1; _ -> 1 end).
-define(exponent(X),element(2, ?d_cast(X))).
-define(is_zero(X),decimal:is_zero(?d_cast(X))).
-define(is_signed(X),case ?sign(X) of -1 -> true; _ -> false end).
-define(cmp(X,Y),decimal:cmp(?d_cast(X),?d_cast(Y), ?d_context)).
-define(le(X,Y),(?cmp(X,Y) =< 0)).
-define(lt(X,Y),(?cmp(X,Y) < 0)).
-define(ge(X,Y),(?cmp(X,Y) >= 0)).
-define(gt(X,Y),(?cmp(X,Y) > 0)).
-define(max(X,Y),case ?cmp(X,Y) of 1->X;_->Y end).
-define(min(X,Y),case ?cmp(X,Y) of -1->X;_->Y end).
-define(reduce(X),decimal:reduce(?d_cast(X))).

-endif.
