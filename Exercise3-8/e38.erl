-module(e38).

-compile(export_all).

parse(L) -> parser(lexer(L)).

parser(L) when is_list(L) ->
  {T, []} = expression(L),
  T.

expression(['~'|T]) -> {X, R} = expression(T), {{'~', X}, R};
expression(['('|T]) -> {X, [')'|R]} = bin(T), {X, R};
expression([{num, _}=X|T]) -> {X, T}.

bin(L) -> {X, [Op|T]} = expression(L),
  true = lists:member(Op, ['+','-','*', '/']),
  {Y, R} = expression(T),
  {{Op, X, Y}, R}.

eval(L) -> evaluator(parse(L)).

evaluator({num, X}) -> X;
evaluator({'~', X}) -> -evaluator(X);
evaluator({'+', X, Y}) -> evaluator(X)+evaluator(Y);
evaluator({'-', X, Y}) -> evaluator(X)-evaluator(Y);
evaluator({'*', X, Y}) -> evaluator(X)*evaluator(Y);
evaluator({'/', X, Y}) -> evaluator(X)/evaluator(Y).

pretty_print(X) -> lists:flatten(pp(X)).

pp({num, X}) -> io_lib:write(X);
pp({'~', X}) -> [$~|pp(X)];
pp({'+', X, Y}) -> [$(,pp(X)] ++ " + " ++ [pp(Y),$)];
pp({'-', X, Y}) -> [$(,pp(X)] ++ " - " ++ [pp(Y),$)];
pp({'*', X, Y}) -> [$(,pp(X)] ++ " * " ++ [pp(Y),$)];
pp({'/', X, Y}) -> [$(,pp(X)] ++ " / " ++ [pp(Y),$)].

compile(L) -> compiler(parse(L)).

compiler(L) -> lists:reverse(comp(L)).

comp({num, X}) -> [X];
comp({'~', X}) -> ['~'|comp(X)];
comp({Op, X, Y}) -> [Op|comp(Y) ++ comp(X)].

simulator(L) -> simulator(L, []).

simulator([], [X]) -> X;
simulator([X|T], S) when is_number(X) -> simulator(T, [X|S]);
simulator(['~'|T], [X|S]) -> simulator(T, [-X|S]);
simulator(['+'|T], [Y,X|S]) -> simulator(T, [X+Y|S]);
simulator(['-'|T], [Y,X|S]) -> simulator(T, [X-Y|S]);
simulator(['*'|T], [Y,X|S]) -> simulator(T, [X*Y|S]);
simulator(['/'|T], [Y,X|S]) -> simulator(T, [X/Y|S]).

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

test_parser() ->
  {'-',{'+',{num,2},{num,3}},{num,4}} = parser(['(','(',{num,2},'+',{num,3},')','-',{num,4},')']),
  {'~',{'+',{'*',{num,2},{num,3}},{'*',{num,3},{num,4}}}}
    = parser(['~','(','(',{num,2},'*',{num,3},')','+','(',{num,3},'*',{num,4},')',')']),
  {num, 4.236} = parser([{num, 4.236}]),
  ok.
