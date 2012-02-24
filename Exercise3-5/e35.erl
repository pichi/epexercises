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

flatten([   ]  ) -> [];
flatten([_|_]=L) -> flatten(L, []).

flatten([[_|_]=H | T], Tail) -> flatten(H, flatten(T, Tail));
flatten([[   ]   | T], Tail) ->            flatten(T, Tail) ;
flatten([      H    ], Tail) ->       [H |            Tail] ;
flatten([      H | T], Tail) ->       [H | flatten(T, Tail)];
flatten([           ], Tail) ->                       Tail.
