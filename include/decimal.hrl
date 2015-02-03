-ifndef(DECIMAL_HRL).
-define(DECIMAL_HRL, true).

%% =============================================================================
%%% Common decimal functions
%% =============================================================================

-ifndef(d_precision).
-define(d_precision, 100).
-endif.

-ifndef(d_rounding).
-define(d_rounding, round).
-endif.

-define(d_context, #{ precision => ?d_precision, rounding => ?d_rounding}).

-define(is_decimal(D),
        is_tuple(D) andalso
        tuple_size(D) =:= 2 andalso
        is_integer(element(1, D)) andalso
        is_integer(element(2, D))).

-define(to_decimal(X), decimal:to_decimal(X, ?d_context)).
-define(to_binary(X), decimal:to_binary(?to_decimal(X))).
-define(to_string(X), binary_to_list(?to_binary(X))).
-define(to_float(X), binary_to_float(?to_binary(X))).

-define(d_zero,{0,0}).
-define(add(X,Y),decimal:add(?to_decimal(X),?to_decimal(Y))).
-define(sub(X,Y),decimal:sub(?to_decimal(X),?to_decimal(Y))).
-define(mult(X,Y),decimal:mult(?to_decimal(X),?to_decimal(Y))).
-define(divide(X,Y),decimal:divide(?to_decimal(X),?to_decimal(Y),?d_context)).
-define(abs(X),decimal:abs(?to_decimal(X))).
-define(neg(X),decimal:minus(?to_decimal(X))).
-define(sign(X),case element(1,?to_decimal(X))<0 of true -> -1; _ -> 1 end).
-define(exponent(X),element(2, ?to_decimal(X))).
-define(is_zero(X),decimal:is_zero(?to_decimal(X))).
-define(is_signed(X),case ?sign(X) of -1 -> true; _ -> false end).
-define(cmp(X,Y),decimal:cmp(?to_decimal(X),?to_decimal(Y),?d_context)).
-define(le(X,Y),(?cmp(X,Y) =< 0)).
-define(ge(X,Y),(?cmp(X,Y) >= 0)).
-define(max(X,Y),case ?cmp(X,Y) of 1->X;_->Y end).
-define(min(X,Y),case ?cmp(X,Y) of -1->X;_->Y end).
-define(reduce(X),decimal:reduce(?to_decimal(X))).

-endif.
