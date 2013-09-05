-module(words).
-export([from_line/1, from_file/1]).

% ----------------------------------------------------------------
from_line(Line) -> from_line_acc(Line, [], []).

from_line_acc([], [], Words) -> Words;

from_line_acc([], Word, Words) ->
    from_line_acc([], [], lists:reverse([Word|Words]));

from_line_acc([Letter|Line], Word, Words) when $A =< Letter, Letter =< $Z;
					       $a =< Letter, Letter =< $z ->    
    from_line_acc(Line, [Letter|Word], Words);

from_line_acc([_Symbol|Line], [], Words) ->    
    from_line_acc(Line, [], Words);

from_line_acc([_Symbol|Line], Word, Words) ->    
    from_line_acc(Line, [], [lists:reverse(Word)|Words]).

% ----------------------------------------------------------------
from_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    from_file_acc(file:read_line(File), File, []).

from_file_acc({ok, Line}, File, Words) ->
    from_file_acc(file:read_line(File), File, Words ++ from_line(Line));

from_file_acc(_Result, File, Words) ->
    file:close(File),
    Words.
