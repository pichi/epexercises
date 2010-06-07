-module(e36).

-export([quicksort/1, mergesort/1]).

quicksort([]) -> [];
quicksort([P|T]) ->
  [L, H] = split(P, T, [], []),
  quicksort(L) ++ [P] ++ quicksort(H).

split(P, [F|T], L, H) when F < P -> split(P, T, [F|L], H);
split(P, [F|T], L, H) -> split(P, T, L, [F|H]);
split(_, [], L, H) -> [L, H].

mergesort([]) -> [];
mergesort(L) ->
  [R] = merge([[X]||X<-L]),
  R.

merge([]) -> [];
merge([A, B|T]) -> merge([merge(A, B)|merge(T)]);
merge([A]) -> [A].

merge([A|TA], [B|_] = LB) when A<B -> [A|merge(TA, LB)];
merge([_|_] = LA, [B|TB]) -> [B|merge(LA, TB)];
merge([], LB) -> LB;
merge(LA, []) -> LA.
