-module(e32).

-export([create/1, reverse_create/1]).

create(N) when is_integer(N) -> create(N, []).

create(N, R) when N > 0 -> create(N - 1, [N | R]);
create(_, R) -> R.

reverse_create(N) when is_integer(N) ->
    reverse_create(N, 0, []).

reverse_create(N, M, R) when M < N ->
    X = M + 1, reverse_create(N, X, [X | R]);
reverse_create(_, _, R) -> R.
