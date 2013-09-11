-module(doc).
-export([print_raw/2, print/2, print_evenly/2, print_words_stats/1]).

% ----------------------------------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------------
process_file_lines(Fn, FnArg, FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Result = process_file_lines_acc(Fn, FnArg, File, []),
    file:close(File),
    Result.

process_file_lines_acc(Fn, FnArg, File, Result) when is_function(Fn) ->
    case file:read_line(File) of
        {ok, Line}  -> process_file_lines_acc(Fn, FnArg, File, [apply(Fn,[FnArg,Line])|Result]);
        _EofOrError -> lists:reverse(Result)
    end.
   
% ----------------------------------------------------------------------------------------------------   
% ----------------------------------------------------------------------------------------------------
insert_newlines(Period, Line) -> 
    insert_newlines_acc(Period, Line, 0, []).
    
insert_newlines_acc(_Period, [], _N, Result) -> 
    lists:reverse(Result);  
    
insert_newlines_acc(Period, [X|Line], Period, Result) -> 
    insert_newlines_acc(Period, Line, 1, [X,$\n|Result]);
    
insert_newlines_acc(Period, [X|Line], N, Result) -> 
    insert_newlines_acc(Period, Line, N+1, [X|Result]).        

% ----------------------------------------------------------------------------------------------------    
print_raw(Width, FileName) -> 
    io:format("~s~n", [process_file_lines(fun insert_newlines/2, Width, FileName)]).    
    
% ----------------------------------------------------------------------------------------------------    
% ----------------------------------------------------------------------------------------------------
classify_symbol(X) when $A =< X, X =< $Z; 
                        $a =< X, X =< $z -> word;     
classify_symbol(_X) -> delimiter.     
        
% ----------------------------------------------------------------------------------------------------
get_chunks([X|Line]) -> get_chunks_acc(Line, [X], []).

get_chunks_acc([], [], Result) ->
    lists:reverse(Result);

get_chunks_acc([], Chunk, Result) ->
    get_chunks_acc([], [], [lists:reverse(Chunk)|Result]);
       
get_chunks_acc([X|Line], [Y|Chunk], Result) -> 
    case classify_symbol(X) == classify_symbol(Y) of
        true  -> get_chunks_acc(Line, [X,Y|Chunk], Result);
        false -> get_chunks_acc(Line, [X], [lists:reverse([Y|Chunk])|Result])
    end.

% ----------------------------------------------------------------------------------------------------
% TODO improve name 
format(Period, Line) ->     
    format_acc(Period, get_chunks(Line), 0, []).
    
format_acc(_Period, [], _N, Result) -> 
    Result;  

format_acc(Period, [X|Chunks], N, Result) when (N+length(X)) =< Period -> 
    format_acc(Period, Chunks, N+length(X), Result ++ X);

format_acc(Period, [X|[]], _N, Result) -> 
    format_acc(Period, [], 0, Result ++ X);        
    
format_acc(Period, [X|Chunks], _N, Result) -> 
    format_acc(Period, Chunks, length(X), Result ++ (X ++ "\n")).        

% ----------------------------------------------------------------------------------------------------    
% TODO improve name 
print(Width, FileName) -> 
    io:format("~s~n", [process_file_lines(fun format/2, Width, FileName)]).        
   
% ----------------------------------------------------------------------------------------------------    
% ----------------------------------------------------------------------------------------------------
fill_evenly(Width, Line) when length(Line) >= Width -> Line;
fill_evenly(Width, Line) -> fill_evenly_acc(Width-length(Line), get_chunks(Line), []).
    
fill_evenly_acc(_N, [], Result) -> 
    Result;  

fill_evenly_acc(0, [X|Chunks], Result) -> 
    fill_evenly_acc(0, Chunks, Result ++ X);    
    
fill_evenly_acc(N, [X|[]], Result) -> 
    fill_evenly_acc(0, [], (Result ++ lists:duplicate(N, $\ )) ++ X);    
    
fill_evenly_acc(N, [X|Chunks], Result) ->  
    M = round(N/length(Chunks)),
    fill_evenly_acc(N-M, Chunks, (Result ++ X) ++ lists:duplicate(M, $\ )).
    
% ----------------------------------------------------------------------------------------------------    
print_evenly(Width, FileName) -> 
    io:format("~s~n", [process_file_lines(fun fill_evenly/2, Width, FileName)]).        

% ----------------------------------------------------------------------------------------------------    
% ----------------------------------------------------------------------------------------------------
% TODO see get_chunks()
from_line(_X, Line) -> from_line_acc(_X, Line, [], []).

from_line_acc(_X, [], [], Words) -> lists:reverse(Words);

from_line_acc(_X, [], Word, Words) ->
    from_line_acc(_X, [], [], [lists:reverse(Word)|Words]);

from_line_acc(_X, [Letter|Line], Word, Words) when $A =< Letter, Letter =< $Z;
					                               $a =< Letter, Letter =< $z ->    
    from_line_acc(_X, Line, [Letter|Word], Words);

from_line_acc(_X, [_Symbol|Line], [], Words) ->    
    from_line_acc(_X, Line, [], Words);

from_line_acc(_X, [_Symbol|Line], Word, Words) ->    
    from_line_acc(_X, Line, [], [lists:reverse(Word)|Words]).

% ----------------------------------------------------------------------------------------------------    
flatten(Lines) -> flatten_acc(Lines, 1, []).
flatten_acc([], _N, R) -> R;
flatten_acc([L|Lines], N, R) -> 
    flatten_acc(Lines, N+1, R ++ lists:zip(L, lists:duplicate(length(L), N))).
    
% ----------------------------------------------------------------------------------------------------    
add_range({N,N}, List) -> [N|List];
add_range({N,M}, List) when ((N+1) == M) -> [M,N|List]; % ? M,N vs N,M
add_range(Range, List) -> [Range|List].

% ----------------------------------------------------------------------------------------------------    
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
    
% ----------------------------------------------------------------------------------------------------    
drop_duplicates([H|T])                -> drop_duplicates_acc(T,[H]).   
drop_duplicates_acc([], L)            -> lists:reverse(L);     
drop_duplicates_acc([H|T1], [H|T2])   -> drop_duplicates_acc(T1, [H|T2]);
drop_duplicates_acc([H1|T1], [H2|T2]) -> drop_duplicates_acc(T1, [H1,H2|T2]).
        
% ----------------------------------------------------------------------------------------------------    
print_word_info([H|[]]) -> print_word_info(H);
print_word_info([H|T])  -> print_word_info(H), io:format(","), print_word_info(T);
print_word_info({N,M})  -> io:format("~p-~p", [N, M]); 
print_word_info(N)      -> io:format("~p", [N]).
print_word_info(W, N)   -> 
    io:format("~s\t", [W]),
    print_word_info(make_ranges(drop_duplicates(lists:sort(N)))),
    io:format("~n").                    
    
% ----------------------------------------------------------------------------------------------------    
% TODO make version of process_file_lines() for fun/1
print_words_stats(FileName) ->
    [{W,N}|T] = lists:keysort(1,flatten(process_file_lines(fun from_line/2, {}, FileName))),
    print_words_stats_acc(T, W, [N]).
          
% next line number found       
print_words_stats_acc([{W,N1}|T1], W, [N2|T2]) ->
    print_words_stats_acc(T1, W, [N1,N2|T2]);
    
% end of data    
print_words_stats_acc([], W, N) -> 
    print_word_info(W, N);

% next word        
print_words_stats_acc([{W1,N1}|T], W2, N2) -> 
    print_word_info(W2, N2),
    print_words_stats_acc(T, W1, [N1]).    
