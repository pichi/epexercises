-module(e39).

-compile(export_all).

raw_read_file(F) ->
  {ok, Data} = file:read_file(F),
  rawdoc(binary_to_list(Data)).

read_file(F) ->
  raw2doc(raw_read_file(F)).

rawdoc([]) -> [];
rawdoc(Data) -> rawdoc(Data, []).

rawdoc([], Line) ->
  [lists:reverse(Line)];
rawdoc([$\n|Rest], Line) ->     % handle trailing \n by rawdoc/1
  [lists:reverse(Line)|rawdoc(Rest)];
rawdoc([C|Rest], Line) ->
  rawdoc(Rest, [C|Line]).

raw2doc(RawDoc) ->
  lists:append([string:tokens(Line, " \t") || Line <- RawDoc]).

compact_index([X|Rest]) when is_integer(X) ->
  compact_index([{X, X}|Rest]);
compact_index([{X, Y}, Z|Rest]) when Z =:= Y; Z =:= Y+1 ->
  compact_index([{X, Z}|Rest]);
compact_index([{_,_}=T|Rest]) ->
  [T|compact_index(Rest)];
compact_index([]) -> [].

format_index_entry({E, I}) ->
  lists:flatten([E, $\s|format_index(compact_index(I))]).

format_index(I) ->
  string:join([fi(X) || X<-I], ",").

fi({X,X}) -> io_lib:write(X);
fi({X,Y}) -> [io_lib:write(X), $-, io_lib:write(Y)].

test() ->
  test_rawdoc(),
  test_raw2doc(),
  test_compact_index(),
  test_format(),
  ok.

test_rawdoc() ->
  ["foo bar","","baz"] = rawdoc("foo bar\n\nbaz\n"),
  ["foo bar","","baz"] = rawdoc("foo bar\n\nbaz"),
  ok.

test_raw2doc() ->
  ["foo", "bar", "baz"] = raw2doc(["foo bar","","baz"]),
  ok.

test_compact_index() ->
  [{1,2},{4,6},{98,98},{100,100},{102,102}]
  = compact_index([1,1,2,4,5,6,6,98,100,102,102]),
  ok.

test_format() ->
  "Erlang 1-2,4-6,98,100,102"
  = format_index_entry({"Erlang", [1,1,2,4,5,6,6,98,100,102,102]}),
  ok.
