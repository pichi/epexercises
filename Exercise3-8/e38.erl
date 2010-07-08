-module(e38).

-compile(export_all).

parser(L) when is_list(L) -> parser(L, [], []).

parser(['('|R], O, S) -> parser(R, ['('|O], S);
parser([{num, _}=X|R], O, S) -> parser(R, O, [X|S]);
parser([')'|R], O, S) ->
  {O2, S2} = collect(O, S),
  parser(R, O2, S2);
parser([X|R], O, S) ->
  {O2, S2} = collect(X, O, S),
  parser(R, [X|O2], S2);
parser([], O, S) ->
  {[], [R]} = collect('$end', O, S),
  R.

collect(['('|R], S) -> {R, S};
collect(['~'|T], [A|R]) -> collect(T, [{'~', A}|R]);
collect([H|T], [A,B|R]) -> collect(T, [{H, B, A}|R]).

collect(O, ['~'|T], [A|R]) -> collect(O, T, [{'~', A}|R]);
collect(O, [H|T]=Ops, [A,B|R]=S) ->
  case priority(H, O) of
    higher -> collect(O, T, [{H, B, A}|R]);
    _ -> {Ops, S}
  end;
collect(_, O, S) -> {O, S}.

priority(_, '$end') -> higher;
priority('*', _) -> higher;
priority('/', _) -> higher;
priority('+', X) when X=:='+'; X=:='-' -> higher;
priority('-', X) when X=:='+'; X=:='-' -> higher;
priority(_, _) -> lower.

lexer([$(|R]) -> ['('|lexer(R)];
lexer([$)|R]) -> [')'|lexer(R)];
lexer([$~|R]) -> ['~'|lexer(R)];
lexer([$+|R]) -> ['+'|lexer(R)];
lexer([$-|R]) -> ['-'|lexer(R)];
lexer([$*|R]) -> ['*'|lexer(R)];
lexer([$/|R]) -> ['/'|lexer(R)];
lexer([$\s|R]) -> lexer(R);
lexer([X|_] = L) when X =< $9, X >= $0 ->
  {Num, R} = lex_num(L, 0),
  [{num, Num}|lexer(R)];
lexer([]) -> [].

lex_num([X|R], N) when X =< $9, X >= $0 ->
  lex_num(R, 10*N + X - $0);
lex_num([$.|R], N) ->
  {F, R2} = lex_fract(R, 0, 0.1),
  {N + F, R2};
lex_num(R, N) -> {N, R}.

lex_fract([X|R], N, F) when X =< $9, X >= $0 ->
  lex_fract(R, N + F * (X - $0), F/10);
lex_fract(R, N, _) -> {N, R}.

test() ->
  ok = test_lexer().

test_lexer() ->
  ['(','(',{num,2},'+',{num,3},')','-',{num,4},')'] = lexer("((2+3)-4)"),
  ['~','(','(',{num,2},'*',{num,3},')','+','(',{num,3},'*',{num,4},')',')']
    = lexer("~((2*3)+(3*4))"),
  [{num, 4.236}] = lexer("4.236"),
  ok.
