-module(i).
-export([parse/1, 
         eval/1, 
         to_string/1, 
         to_stack_machine_code/1, run_stack_machine/1,
         simplify/1]).

% ((2+3)-4) => {minus, {plus, {num, 2}, {num, 3}}, {num, 4}}.
% 4 => {num, 4}.
% ~((2*3)+(3*4)) => {minus {plus, {plus, {mult, {num, 2}, {num, 3}}, {mult, {num, 3}, {num, 4}}}}}.

% expr   ::= number | ( expr ) | un_op expr | expr bin_op expr 
% digit  ::= [0-9]
% number ::= digit | digit number
% bin_op ::= * | / | + | -
% un_op  ::= ~

% parse(List) -> parse_impl(List, [], []).
% parse_impl([], Lexems, [])     -> lists:reverse(Lexems);
% parse_impl([], Lexems, Digits) -> lists:reverse([lists:reverse(Digits)|Lexems]);
% parse_impl([Character|Tail], Lexems,  Digits) when Character >= $0, Character =< $9 -> parse_impl(Tail, Lexems, [Character|Digits]);
% parse_impl([_Character|Tail], Lexems, [])     -> parse_impl(Tail, Lexems, []);
% parse_impl([_Character|Tail], Lexems, Digits) -> parse_impl(Tail, [lists:reverse(Digits)|Lexems], []).

% ------------------------------------------------------------------------------------------------------------------------------------
% Lexic
% ------------------------------------------------------------------------------------------------------------------------------------
character2tag($+) -> binary_operation;
character2tag($-) -> binary_operation;
character2tag($*) -> binary_operation;
character2tag($/) -> binary_operation;
character2tag($~) -> unary_operation;
character2tag($() -> bracket_open;
character2tag($)) -> bracket_close.

get_lexemes(String) -> get_lexemes_acc(String, []).

get_lexemes_acc([], LexemeList) -> lists:reverse(LexemeList);

get_lexemes_acc([X|Tail], LexemeList) when X >= $0, X =< $9 -> 
    { RemainingString, NumberAsString } = get_number([X|Tail]), 
    get_lexemes_acc(RemainingString, [{number, NumberAsString} | LexemeList]);   
   
get_lexemes_acc([X|Tail], LexemeList) when X == $+; X == $-; X == $*; X == $/; X == $~ -> 
    get_lexemes_acc(Tail, [{character2tag(X), [X]} | LexemeList]);

get_lexemes_acc([X|Tail], LexemeList) when X == $(; X == $) -> 
    get_lexemes_acc(Tail, [{character2tag(X)} | LexemeList]);
    
% Skip unknown characters.
get_lexemes_acc([_X|Tail], LexemeList) -> get_lexemes_acc(Tail, LexemeList).

get_number(List) -> get_number_acc(List, []).
get_number_acc([X|Tail], NumberAsString) when X >= $0, X =< $9 -> get_number_acc(Tail, [X|NumberAsString]);
get_number_acc(List, NumberAsString) -> { List, lists:reverse(NumberAsString) }.

% ------------------------------------------------------------------------------------------------------------------------------------
% Syntax
% ------------------------------------------------------------------------------------------------------------------------------------
% ? 3+3+2+1+4+5+6+7*8-1-2-4-4/2+1+0

id2action("*") -> multiply;
id2action("/") -> divide;
id2action("+") -> add;
id2action("-") -> subtract;
id2action("~") -> negate.

replace_pattern([Arg, {unary_operation,Id}|Tail])             -> replace_pattern([[id2action(Id), Arg]|Tail]); 
replace_pattern([Arg2, {binary_operation,Id}, Arg1|Tail])     -> replace_pattern([[id2action(Id), Arg1, Arg2]|Tail]); 
replace_pattern([{bracket_close}, Expr, {bracket_open}|Tail]) -> replace_pattern([Expr|Tail]);   
replace_pattern(Expr) -> Expr.

parse(String) -> parse_acc(before_number, get_lexemes(String), []).

parse_acc(before_number, [Lex|LexList], Expr) when element(1, Lex) == bracket_open; 
                                                   element(1, Lex) == unary_operation -> 
    % io:format("~p~n", [[Lex|Expr]]), 
    parse_acc(before_number, LexList, [Lex|Expr]);
    
parse_acc(before_number, [Lex|LexList], Expr) when element(1, Lex) == number -> 
    % io:format("~p~n", [[list_to_integer(element(2, Lex))|Expr]]), 
    parse_acc(after_number,  LexList, replace_pattern([list_to_integer(element(2, Lex))|Expr]));
    
parse_acc(after_number,  [Lex|LexList], Expr) when element(1, Lex) == binary_operation -> 
    % io:format("~p~n", [[Lex|Expr]]), 
    parse_acc(before_number, LexList, [Lex|Expr]);
    
parse_acc(after_number,  [Lex|LexList], Expr) when element(1, Lex) == bracket_close -> 
    % io:format("~p~n", [[Lex|Expr]]), 
    parse_acc(after_number,  LexList, replace_pattern([Lex|Expr]));     

parse_acc(_State, [], [Expr|[]]) -> 
    % io:format("~p~n", [Expr]), 
    Expr.     

% ------------------------------------------------------------------------------------------------------------------------------------
% eval
% ------------------------------------------------------------------------------------------------------------------------------------
eval([negate, Arg]) -> (-eval(Arg));

eval([multiply, Arg1, Arg2]) -> eval(Arg1) * eval(Arg2);
eval([divide,   Arg1, Arg2]) -> eval(Arg1) / eval(Arg2);
eval([add,      Arg1, Arg2]) -> eval(Arg1) + eval(Arg2);
eval([subtract, Arg1, Arg2]) -> eval(Arg1) - eval(Arg2);

eval(Number) -> Number.

% ------------------------------------------------------------------------------------------------------------------------------------
% to_string
% ? lots of lists:flatten()
% ? tail recursion
% ------------------------------------------------------------------------------------------------------------------------------------
to_string([negate, Arg]) -> lists:flatten(["~", to_string(Arg)]);

to_string([multiply, Arg1, Arg2]) -> lists:flatten(["(", to_string(Arg1), "*", to_string(Arg2), ")"]);
to_string([divide,   Arg1, Arg2]) -> lists:flatten(["(", to_string(Arg1), "/", to_string(Arg2), ")"]);
to_string([add,      Arg1, Arg2]) -> lists:flatten(["(", to_string(Arg1), "+", to_string(Arg2), ")"]);
to_string([subtract, Arg1, Arg2]) -> lists:flatten(["(", to_string(Arg1), "-", to_string(Arg2), ")"]);

to_string(Number) -> integer_to_list(Number).

% ------------------------------------------------------------------------------------------------------------------------------------
% to_stack_machine_code
% ? lots of lists:flatten()
% ? tail recursion
% ------------------------------------------------------------------------------------------------------------------------------------
to_stack_machine_code([Action, Arg])        -> lists:flatten([to_stack_machine_code(Arg), Action]);
to_stack_machine_code([Action, Arg1, Arg2]) -> lists:flatten([to_stack_machine_code(Arg1), to_stack_machine_code(Arg2), Action]);
to_stack_machine_code(Number)               -> Number.

% ------------------------------------------------------------------------------------------------------------------------------------
% run_stack_machine
% ------------------------------------------------------------------------------------------------------------------------------------
run_stack_machine(Code) -> run_stack_machine_acc(Code, []).

run_stack_machine_acc([], [Result|[]]) -> Result;

run_stack_machine_acc([negate|Code], [Number|Stack]) -> 
    % io:format("~p ~p~n", [[negate|Code], [Number|Stack]]), 
    run_stack_machine_acc(Code, [-Number|Stack]);

run_stack_machine_acc([multiply|Code], [Number2,Number1|Stack]) -> 
    % io:format("~p ~p~n", [[multiply|Code], [Number2,Number1|Stack]]), 
    run_stack_machine_acc(Code, [(Number1*Number2)|Stack]);
    
run_stack_machine_acc([divide  |Code], [Number2,Number1|Stack]) -> 
    % io:format("~p ~p~n", [[divide|Code], [Number2,Number1|Stack]]), 
    run_stack_machine_acc(Code, [(Number1 div Number2)|Stack]);
    
run_stack_machine_acc([add     |Code], [Number2,Number1|Stack]) -> 
    % io:format("~p ~p~n", [[add|Code], [Number2,Number1|Stack]]), 
    run_stack_machine_acc(Code, [(Number1+Number2)|Stack]);
    
run_stack_machine_acc([subtract|Code], [Number2,Number1|Stack]) -> 
    % io:format("~p ~p~n", [[subtract|Code], [Number2,Number1|Stack]]), 
    run_stack_machine_acc(Code, [(Number1-Number2)|Stack]);

run_stack_machine_acc([Number|Code], Stack) -> 
    % io:format("~p ~p~n", [[Number|Code], [Stack]]), 
    run_stack_machine_acc(Code, [Number|Stack]).

% ------------------------------------------------------------------------------------------------------------------------------------
% simplify
% ? tail recursion
% ? simplification after simplification
% ------------------------------------------------------------------------------------------------------------------------------------
simplify([negate, 0]) -> 0;

simplify([multiply, Arg1, Arg2]) when Arg1 == 0; Arg2 == 0 -> 0;
simplify([multiply, Arg1, Arg2]) when Arg1 == 1 -> simplify(Arg2);
simplify([multiply, Arg1, Arg2]) when Arg2 == 1 -> simplify(Arg1);

simplify([divide, Arg1, Arg2]) when Arg2 == 1 -> simplify(Arg1);

simplify([add, Arg1, Arg2]) when Arg1 == 0 -> simplify(Arg2);
simplify([add, Arg1, Arg2]) when Arg2 == 0 -> simplify(Arg1);

simplify([subtract, Arg1, Arg2]) when Arg2 == 0 -> simplify(Arg1);
simplify([subtract, Arg1, Arg2]) when Arg1 == 0 -> [negate, simplify(Arg2)];

simplify([Action, Arg])        -> [Action, simplify(Arg)];
simplify([Action, Arg1, Arg2]) -> [Action, simplify(Arg1), simplify(Arg2)];
simplify(Number)               -> Number.
