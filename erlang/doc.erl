-module(doc).
-export([print_words_stats/1, lines_from_file/1, format/2]).

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
% TODO dup with lines_from_file_acc
words_from_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    words_from_file_acc(file:read_line(File), File, []).

words_from_file_acc({ok, Line}, File, Words) ->
    words_from_file_acc(file:read_line(File), File, [from_line(Line)|Words]);

words_from_file_acc(_Result, File, Words) ->
    file:close(File),
    lists:reverse(Words).

% ----------------------------------------------------------------
lines_from_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    lines_from_file_acc(file:read_line(File), File, []).

lines_from_file_acc({ok, Line}, File, Lines) ->
    lines_from_file_acc(file:read_line(File), File, [Line|Lines]);

lines_from_file_acc(_Result, File, Words) ->
    file:close(File),
    lists:reverse(Words).    
    
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
    io:format("~s: ", [W]),
    print(make_ranges(drop_duplicates(lists:sort(N)))),
    io:format("~n").                    
    
% ----------------------------------------------------------------
% TODO make separate function 'get_words_stats'
print_words_stats(FileName) ->
    [{W,N}|T] = lists:keysort(1,flatten(words_from_file(FileName))),
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
% split(Width, Words) -> split_acc(Width, Words, []).

% split_acc(0, WordsOut, WordsIn) ->
  %  {WordsIn, WordsOut};

% split_acc(Width, [], WordsIn) ->
  %   {WordsIn, []};
    
% split_acc(Width, [Word|OtherWords], WordsIn) when Width ->
  %  split_acc(Width-length, [Word|OtherWords], [|WordsIn]);

% split_acc(Width, [Word|OtherWords], WordsIn) ->
  %  split_acc(Width-length, [Word|OtherWords], [|WordsIn]).
    
% ----------------------------------------------------------------
% io:format("~s", [doc:format("doc.erl",10)])
% ----------------------------------------------------------------
format(FileName, Width) -> 
    format_acc(Width, lines_from_file(FileName), []).

format_acc(_Width, [], Result) ->       
    lists:reverse(Result);
    
% TODO where should be "\n"?    
format_acc(Width, [Line|Document], Result) when length(Line) > Width ->   
    {In, Out} = lists:split(Width, Line),
    format_acc(Width, ["\n",Out|Document], [In|Result]);

format_acc(Width, [Line|Document], Result) ->   
    format_acc(Width, Document, [Line|Result]).

    
    