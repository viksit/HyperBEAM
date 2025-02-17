%%% @doc Wrapper for encoding and decoding JSON. Supports maps and Jiffy's old 
%%% `ejson' format.
-module(hb_json).
-export([encode/1, decode/1, decode/2]).

encode(Term) ->
    json:encode(Term).

decode(Bin) -> json:decode(Bin).
decode(Bin, _Opts) -> decode(Bin).
