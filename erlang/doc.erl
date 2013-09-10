-module(doc).
-export([process_file_lines/2, print_words_stats/1, format/2, split/2, format_unstructured_text/2]).

% ----------------------------------------------------------------
process_file_lines(Fn, FnArgList, FileName) ->
    {ok, F} = file:open(FileName, [read]),
    process_file_lines_acc(file:read_line(F), Fn, FnArgList, F, []).

process_file_lines_acc({ok, L}, Fn, FnArgList, F, R) when is_function(Fn) ->
    process_file_lines_acc(file:read_line(F), Fn, FnArgList, F, [apply(Fn, [L|FnArgList])|R]);

process_file_lines_acc(_ReadLineResult, _Fn, _FnArgList, F, R) ->
    file:close(F),
    lists:reverse(R).

% ----------------------------------------------------------------
from_line(Line) -> from_line_acc(Line, [], []).

from_line_acc([], [], Words) -> lists:reverse(Words);

from_line_acc([], Word, Words) ->
    from_line_acc([], [], [lists:reverse(Word)|Words]);

from_line_acc([Letter|Line], Word, Words) when $A =< Letter, Letter =< $Z;
					       $a =< Letter, Letter =< $z ->    
    from_line_acc(Line, [Letter|Word], Words);

from_line_acc([_Symbol|Line], [], Words) ->    
    from_line_acc(Line, [], Words);

from_line_acc([_Symbol|Line], Word, Words) ->    
    from_line_acc(Line, [], [lists:reverse(Word)|Words]).

% ----------------------------------------------------------------
% Lines -- result of from_file/1.
% ----------------------------------------------------------------
% flatten(Line) when ... -> flatten([Line], 1, []);

flatten(Lines) -> flatten_acc(Lines, 1, []).
flatten_acc([], _N, R) -> R;
flatten_acc([L|Lines], N, R) -> 
    flatten_acc(Lines, N+1, R ++ lists:zip(L, lists:duplicate(length(L), N))).
    
% ----------------------------------------------------------------    
add_range({N,N}, List) -> [N|List];
add_range({N,M}, List) when ((N+1) == M) -> [M,N|List]; % ? M,N vs N,M
add_range(Range, List) -> [Range|List].

make_ranges([N|Tail]) -> 
    lists:reverse(make_ranges_acc(Tail, {N,N}, [])).    

% finish    
make_ranges_acc([], Range, Result) -> 
    add_range(Range, Result);

% extend range    
make_ranges_acc([N|Tail], {Begin,End}, Result) when (N == (End+1)) -> 
    make_ranges_acc(Tail, {Begin,N}, Result);

% new range    
make_ranges_acc([N|Tail], Range, Result) -> 
    make_ranges_acc(Tail, {N,N}, add_range(Range, Result)).   
    
% ----------------------------------------------------------------
drop_duplicates([H|T])                -> drop_duplicates_acc(T,[H]).   
drop_duplicates_acc([], L)            -> lists:reverse(L);     
drop_duplicates_acc([H|T1], [H|T2])   -> drop_duplicates_acc(T1, [H|T2]);
drop_duplicates_acc([H1|T1], [H2|T2]) -> drop_duplicates_acc(T1, [H1,H2|T2]).
        
% ----------------------------------------------------------------
print([H|[]]) -> print(H);
print([H|T])  -> print(H), io:format(","), print(T);
print({N,M})  -> io:format("~p-~p", [N, M]); 
print(N)      -> io:format("~p", [N]).
print(W, N)   -> 
    io:format("~s\t", [W]),
    print(make_ranges(drop_duplicates(lists:sort(N)))),
    io:format("~n").                    
    
% ----------------------------------------------------------------
% TODO make separate function 'get_words_stats'
print_words_stats(FileName) ->
    [{W,N}|T] = lists:keysort(1,flatten(process_file_lines(fun from_line/1, FileName))),
    print_words_stats_acc(T, W, [N]).
          
% next line number found       
print_words_stats_acc([{W,N1}|T1], W, [N2|T2]) ->
    print_words_stats_acc(T1, W, [N1,N2|T2]);
    
% end of data    
print_words_stats_acc([], W, N) -> 
    print(W, N);

% next word        
print_words_stats_acc([{W1,N1}|T], W2, N2) -> 
    print(W2, N2),
    print_words_stats_acc(T, W1, [N1]).
    
% ----------------------------------------------------------------
% ----------------------------------------------------------------
% ----------------------------------------------------------------
   
% ----------------------------------------------------------------
% io:format("~s", [doc:format(10, "doc.erl")])
% ----------------------------------------------------------------
format(Width, FileName) -> 
    % format_acc(Width, process_file_lines(fun (Line) -> Line end, FileName), []).
    process_file_lines(fun set_line_width/2, [Width], FileName).

% format_acc(_Width, [], Result) ->       
%     lists:reverse(Result);
%     
% % TODO where should be "\n"?    
% format_acc(Width, [Line|Document], Result) when length(Line) > Width ->   
%     {In, Out} = lists:split(Width, Line),
%     format_acc(Width, ["\n",Out|Document], [In|Result]);
% 
% format_acc(Width, [Line|Document], Result) ->   
%     format_acc(Width, Document, [Line|Result]).
    
% ----------------------------------------------------------------   
set_line_width(Width, Line) -> 
    set_line_width_acc(Width, Line, []).

set_line_width_acc(_Width, [], Result) ->       
    Result; % lists:reverse(Result);
    
% TODO where should be "\n"?    
set_line_width_acc(Width, Line, Result) when length(Line) > Width ->   
    {In, Out} = lists:split(Width, Line),
    set_line_width_acc(Width, ["\n"|Out], Result ++ In);

set_line_width_acc(Width, Line, Result) ->   
    set_line_width_acc(Width, [], Result ++ Line).   
  
% ----------------------------------------------------------------
% Format unstructured text.
% ----------------------------------------------------------------
split(Width, Words) -> split_acc(Width, [], Words).

split_acc(_Width, WordsIn, []) ->
    {lists:reverse(WordsIn), []};

split_acc(0, WordsIn, WordsOut) ->
    {lists:reverse(WordsIn), WordsOut};   
    
% If width is so small that even single word can't fit.                                        
split_acc(Width, [], [W|WordsOut]) when Width =< length(W) ->
    split_acc(0, [W], WordsOut);
    
split_acc(Width, WordsIn, [W|WordsOut]) when Width-length(W) >= 0 ->
    split_acc(Width-length(W), [W|WordsIn], WordsOut);

split_acc(_Width, WordsIn, WordsOut) ->
    split_acc(0, WordsIn, WordsOut).
       
% ----------------------------------------------------------------
format_unstructured_text(Width, FileName) -> 
    format_unstructured_text_acc(Width, process_file_lines(fun from_line/1, FileName), []).

format_unstructured_text_acc(_Width, [], Result) ->       
    lists:reverse(Result);
    
% TODO where should be "\n"?    
format_unstructured_text_acc(Width, [[]|Document], Result) ->   
    format_unstructured_text_acc(Width, Document, [[]|Result]);     
  
format_unstructured_text_acc(Width, [Line|Document], Result) ->   
    {In, Out} = split(Width, Line),    
    format_unstructured_text_acc(Width, [Out|Document], [["\n"|In]|Result]). 
    