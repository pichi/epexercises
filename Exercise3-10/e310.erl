-module(e310).

-compile(export_all).

to_words(L) -> string:tokens(L, " \t\n\r").

fill(T, W) ->
  F = fill_words(to_words(T), W),
  string:join([string:join(L, " ") || L <- F], "\n").

fill_words([], _) -> [];
fill_words([H|T], W) ->
  {L, R} = fill_line(T, W - length(H), []),
  [[H|L]|fill_words(R, W)].

fill_line([], _, L) -> {lists:reverse(L), []};
fill_line([H|T], W, L) when length(H) < W ->
  fill_line(T, W - length(H) - 1, [H|L]);
fill_line(R, _, L) -> {lists:reverse(L), R}.

justify(T, W) ->
  F = fill_words(to_words(T), W),
  justify_(F, W).

justify_([], _) -> [];
justify_([L], _) -> string:join(L, " "); % the last line
justify_([L|R], W) -> justify_line(L, W) ++ [$\n | justify_(R, W)].

justify_line([Word], _) -> Word; % one word on line
justify_line(L, W) ->
  Words = length(L),
  Spaces = W - lists:sum([length(Word) || Word <- L]),
  join(L, Words - 1, Spaces, 1, 0).

join([Word], _, _, _, _) -> Word; % the last word on line
join([W|R], D, M, P, T) ->
  Take = (P * M) div D - T,
  W ++ string:chars($\s, Take, join(R, D, M, P + 1, T + Take)).

test() ->
  test_fill(),
  test_justify(),
  ok.

testing_text() ->
"Write a function that will print this in a readable form,
so that duplicates are removed and adjacent numbers are put into a
range. You might like to think of doing this via a function which turns
the earlier list of occurrences into a list like
[{1,2},{4,6},{98,98},{100,100},{102,102}]
through a sequence of transformations.".

test_fill() ->
"Write a function that will print this in
a readable form, so that duplicates are
removed and adjacent numbers are put
into a range. You might like to think of
doing this via a function which turns
the earlier list of occurrences into a
list like
[{1,2},{4,6},{98,98},{100,100},{102,102}]
through a sequence of transformations." = fill(testing_text(), 40),
  ok.

test_justify() ->
"Write a function that will print this in
a readable form, so that duplicates  are
removed and  adjacent  numbers  are  put
into a range. You might like to think of
doing this  via a  function which  turns
the earlier list  of occurrences into  a
list                                like
[{1,2},{4,6},{98,98},{100,100},{102,102}]
through a sequence of transformations." = justify(testing_text(), 40),
  ok.
