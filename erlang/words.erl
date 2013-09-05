-module(words).
-export([from_line/1, from_file/1]).

get_words(Line) -> get_words_acc(Line, [], []).

get_words_acc([], [], Words) ->
    Words;

get_words_acc([], Word, Words) ->
    get_words_acc([], [], [Word|Words]);

get_words_acc([Letter|Line], Word, Words) when $A =< Letter, Letter =< $z ->    
    get_words_acc(Line, [Letter|Word], Words);

get_words_acc([Letter|Line], Word, Words) ->    
    get_words_acc(Line, Word, [lists:reverse(Word)|Words]).
 

get_words(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    get_words_acc(file:read_line(File), File, []).

get_words_acc({ok, Line}, File, Words) ->
    read_lines_acc(file:read_line(File), File, [get_words_from_line(Line)|Words]);

get_words_acc(_Result, File, Words) ->
    file:close(File),
    Words.
