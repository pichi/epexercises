-module(e35).

-export([filter/2, reverse/1, concat/1, flatten/1]).

filter([H|T], N) when H>N -> filter(T, N);
filter([H|T], N) -> [H|filter(T, N)];
filter([], _) -> [].

reverse(L) -> reverse(L, []).

reverse([H|T], Acc) -> reverse(T, [H|Acc]);
reverse([], Acc) -> Acc.

concat([H | [T]]) -> [H | T]; 
concat([H | T]) -> append(H, concat(T)); 
concat([]) -> [].

append([H | T], L) -> [H | append(T, L)];
append([], L) -> L.

flatten([H|T]) when is_list(H) -> append(flatten(H), flatten(T));
flatten([H|T]) -> [H|flatten(T)];
flatten([]) -> [].
