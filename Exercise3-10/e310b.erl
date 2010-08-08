% More Erlangish (may be Prologish) and accidentaly faster version of e310
-module(e310b).

-compile(export_all).

to_words(L) -> string:tokens(L, " \t\n\r").

fill(T, W) ->
  F = fill_words(to_words(T), W),
  fill_(F).

fill_([]) -> [];
fill_([LL|F]) -> fill_(F, fill_line(LL, [])).

fill_([], R) -> R;
fill_([L|T], R) ->
  fill_(T, fill_line(L, [$\n|R])).

fill_line([], R) -> R;
fill_line([W|L], R) -> fill_line_(L, W ++ R).

fill_line_([], R) -> R;
fill_line_([W|L], R) -> fill_line_(L, W ++ [$\s|R]).

% it will return lines and words on lines in reversed order
% benefit reveals when result should be build up to result text
fill_words(T, W) -> fill_words(T, W, []).

fill_words([], _, R) -> R;
fill_words([H|T], W, A) ->
  {L, R} = fill_line(T, W - length(H), [H]),
  fill_words(R, W, [L|A]).

fill_line([], _, L) -> {L, []};
fill_line([H|T], W, L) when length(H) < W ->
  fill_line(T, W - length(H) - 1, [H|L]);
fill_line(R, _, L) -> {L, R}.

justify(T, W) ->
  F = fill_words(to_words(T), W),
  justify_(F, W).

justify_([], _) -> [];
justify_([LL|F], W) -> justify_(F, W, fill_line(LL, [])).

justify_([], _, R) -> R;
justify_([L|T], W, R) ->
  justify_(T, W, justify_line(L, W, [$\n|R])).

justify_line([Word], _, R) -> Word ++ R; % one word on line
justify_line([Word|T] = L, W, R) ->
  Words = length(T),
  Spaces = W - lists:sum([length(X) || X <- L]),
  join(T, Words, Spaces, 2, Spaces div Words, Word ++ R).

join([], _, _, _, _, R) -> R;
join([W|L], D, M, P, T, R) ->
  Take = (P * M) div D - T,
  join(L, D, M, P + 1, T + Take, W ++ string:chars($\s, Take, R)).

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
a readable  form, so that duplicates are
removed and  adjacent  numbers  are  put
into a range. You might like to think of
doing this  via a  function which  turns
the earlier  list of occurrences  into a
list                                like
[{1,2},{4,6},{98,98},{100,100},{102,102}]
through a sequence of transformations." = justify(testing_text(), 40),
  ok.

test_speed(0) -> ok;
test_speed(N) ->
  test(),
  test_speed(N-1).
