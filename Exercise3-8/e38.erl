-module(e38).

-compile(export_all).

parse(L) -> parser(lexer(L)).

parser(L) when is_list(L) ->
  {T, []} = expression(L),
  T.

expression(['let',{id,I},'='|T]) ->
  {V, ['in'|R1]} = expression(T),
  {E, R2} = expression(R1),
  {{'let', I, V, E}, R2};
expression(['if'|T]) ->
  {C, ['then'|R1]} = expression(T),
  {X, ['else'|R2]} = expression(R1),
  {Y, R3} = expression(R2),
  {{'if', C, X, Y}, R3};
expression(['~'|T]) -> {X, R} = expression(T), {{'~', X}, R};
expression(['('|T]) -> {X, [')'|R]} = bin(T), {X, R};
expression([{id, _}=X|T]) -> {X, T};
expression([{num, _}=X|T]) -> {X, T}.

bin(L) -> {X, [Op|T]} = expression(L),
  true = lists:member(Op, ['+','-','*', '/']),
  {Y, R} = expression(T),
  {{Op, X, Y}, R}.

get_value(I, D) -> {I, V} = lists:keyfind(I, 1, D), V.

set_value(I, V, D) -> [{I, V}|D].

eval(L) -> evaluator(parse(L)).

evaluator(E) -> evaluator(E, []).

evaluator({'if', C, X, Y}, D) ->
  case evaluator(C, D) == 0 of
    true -> evaluator(X, D);
    false -> evaluator(Y, D)
  end;
evaluator({'let', I, V, E}, D) -> evaluator(E, set_value(I, evaluator(V, D), D));
evaluator({id, I}, D) -> get_value(I, D);
evaluator({num, X}, _D) -> X;
evaluator({'~', X}, D) -> -evaluator(X, D);
evaluator({'+', X, Y}, D) -> evaluator(X, D)+evaluator(Y, D);
evaluator({'-', X, Y}, D) -> evaluator(X, D)-evaluator(Y, D);
evaluator({'*', X, Y}, D) -> evaluator(X, D)*evaluator(Y, D);
evaluator({'/', X, Y}, D) -> evaluator(X, D)/evaluator(Y, D).

pretty_print(X) -> lists:flatten(pp(X)).

pp({'let', I, V, E}) -> "let " ++ [I] ++ " = " ++ [pp(V)] ++ " in " ++ pp(E);
pp({'if', C, X, Y}) -> "if " ++ [pp(C)] ++ " then " ++ [pp(X)] ++ " else " ++ pp(Y);
pp({id, I}) -> I;
pp({num, X}) -> io_lib:write(X);
pp({'~', X}) -> [$~|pp(X)];
pp({'+', X, Y}) -> [$(,pp(X)] ++ " + " ++ [pp(Y),$)];
pp({'-', X, Y}) -> [$(,pp(X)] ++ " - " ++ [pp(Y),$)];
pp({'*', X, Y}) -> [$(,pp(X)] ++ " * " ++ [pp(Y),$)];
pp({'/', X, Y}) -> [$(,pp(X)] ++ " / " ++ [pp(Y),$)].

compile(L) -> compiler(parse(L)).

compiler(L) -> lists:reverse(comp(L)).

comp({id, I}) -> [{get,I}];
comp({num, X}) -> [X];
comp({'~', X}) -> ['~'|comp(X)];
comp({'if', C, X, Y}) -> ['if'|comp(C)++[compiler(X), compiler(Y)]];
comp({'let', I, V, E}) -> comp(E) ++ [{set,I}|comp(V)];
comp({Op, X, Y}) -> [Op|comp(Y) ++ comp(X)].

simulator(L) -> simulator(L, [], []).

simulator([], [X], _D) -> X;
simulator([X|T], S, D) when is_number(X); is_list(X) -> simulator(T, [X|S], D);
simulator(['if'|T], [C,X,_|S], D) when C == 0 -> simulator(X++T, S, D);
simulator(['if'|T], [_,_,Y|S], D) -> simulator(Y++T, S, D);
simulator([{set, I}|T], [V|S], D) -> simulator(T, S, set_value(I, V, D));
simulator([{get, I}|T], S, D) -> simulator(T, [get_value(I, D)|S], D);
simulator(['~'|T], [X|S], D) -> simulator(T, [-X|S], D);
simulator(['+'|T], [Y,X|S], D) -> simulator(T, [X+Y|S], D);
simulator(['-'|T], [Y,X|S], D) -> simulator(T, [X-Y|S], D);
simulator(['*'|T], [Y,X|S], D) -> simulator(T, [X*Y|S], D);
simulator(['/'|T], [Y,X|S], D) -> simulator(T, [X/Y|S], D).

simplify(L) -> simplifier(parse(L)).

simplifier({'/', {num, X}, _}) when X == 0 -> {num, 0};
simplifier({'*', {num, X}, _}) when X == 0 -> {num, 0};
simplifier({'*', _, {num, X}}) when X == 0 -> {num, 0};
simplifier({Op, X, Y}) -> simplifier_({Op, simplifier(X), simplifier(Y)});
simplifier({'~', X}) -> simplifier_({'~', simplifier(X)});
simplifier({'let', I, V, E}) ->
  case simplifier(V) of
    {num, _} = X -> simplifier(subst(I, X, E));
    V2 -> {'let', I, V2, simplifier(E)}
  end;
simplifier({'if', C, X, Y}) ->
  case simplifier(C) of
    {num, N} when N == 0 -> simplifier(X);
    {num, _} -> simplifier(Y);
    C2 -> {'if', C2, simplifier(X), simplifier(Y)}
  end;
simplifier(E) -> E.

simplifier_({'/', {num, X}, _}) when X == 0 -> {num, 0};
simplifier_({'*', {num, X}, _}) when X == 0 -> {num, 0};
simplifier_({'*', _, {num, X}}) when X == 0 -> {num, 0};
simplifier_({'*', {num, X}, E}) when X == 1 -> E;
simplifier_({'*', E, {num, X}}) when X == 1 -> E;
simplifier_({'+', {num, X}, E}) when X == 0 -> E;
simplifier_({'+', E, {num, X}}) when X == 0 -> E;
simplifier_({'-', {num, X}, E}) when X == 0 -> simplifier_({'~', E});
simplifier_({'-', E, {num, X}}) when X == 0 -> E;
simplifier_({'~', {num, X}}) -> {num, -X};
simplifier_(E) -> E.

subst(I, N, {id, I}) -> N;
subst(I, N, {'~', E}) -> {'~', subst(I, N, E)};
subst(I, N, {Op, X, Y}) -> {Op, subst(I, N, X), subst(I, N, Y)};
subst(I, N, {'let', I, V, E}) -> {'let', I, subst(I, N, V), E};
subst(I, N, {'let', I2, V, E}) -> {'let', I2, subst(I, N, V), subst(I, N, E)};
subst(I, N, {'if', C, X, Y}) -> {'if', subst(I, N, C), subst(I, N, X), subst(I, N, Y)};
subst(_, _, E) -> E.

lexer("if"++R) -> ['if'|lexer(R)];
lexer("then"++R) -> ['then'|lexer(R)];
lexer("else"++R) -> ['else'|lexer(R)];
lexer("let"++R) -> ['let'|lexer(R)];
lexer("in"++R) -> ['in'|lexer(R)];
lexer([$=|R]) -> ['='|lexer(R)];
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
lexer([X|R]) when X =< $z, X >= $a; X =< $Z, X >= $A ->
  {I, R2} = lex_id(R, [X]),
  [{id, I}|lexer(R2)];
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

lex_id([X|R], I) when X =< $z, X >= $a; X =< $Z, X >= $A; X =< $9, X >= $0 ->
  lex_id(R, [X|I]);
lex_id(R, I) -> {lists:reverse(I), R}.

test() ->
  ok = test_lexer(),
  ok = test_parser(),
  ok = test_eval(),
  ok = test_pp(),
  ok = test_compile(),
  ok = test_simulator(),
  ok = test_simplify().

test_lexer() ->
  ['(','(',{num,2},'+',{num,3},')','-',{num,4},')'] = lexer("((2+3)-4)"),
  ['~','(','(',{num,2},'*',{num,3},')','+','(',{num,6},'/',{num,3},')',')']
    = lexer("~((2*3)+( 6/3))"),
  [{num, 4.236}] = lexer("4.236"),
  ['if',{num,0},'then',{num,1},'else',{num,0}] = lexer("if 0 then 1 else 0"),
  ['let',{id,"c"},'=','(','(',{num,2},'+',{num,3},')','-',{num,4},')',
    'in','~','(','(',{num,2},'*',{id,"c"},')','+','(',{num,3},'*',{num,4},')',')']
  = lexer("let c = ((2+3)-4) in ~((2*c)+(3*4))"),
  ['let',{id,"foo"},'=',{num,6},'in','(',{num,7},'*',{id,"foo"},')']
  = lexer("letfoo=6in(7*foo)"),
  ok.

test_parser() ->
  {'-',{'+',{num,2},{num,3}},{num,4}} = parser(['(','(',{num,2},'+',{num,3},')','-',{num,4},')']),
  {'~',{'+',{'*',{num,2},{num,3}},{'/',{num,6},{num,3}}}}
  = parser(['~','(','(',{num,2},'*',{num,3},')','+','(',{num,6},'/',{num,3},')',')']),
  {num, 4.236} = parser([{num, 4.236}]),
  {'if', {num, 0}, {num, 1}, {num, 0}} = parser(['if',{num,0},'then',{num,1},'else',{num,0}]),
  {'if', {'if', {num, 0}, {num, 1}, {num, 0}},
    {'if', {num, 1}, {num, 2}, {num, 3}},
    {'if', {num, 4}, {num, 5}, {num, 6}}} = parser(['if','if',{num,0},'then',{num,1},'else',{num,0},
      'then','if',{num,1},'then',{num,2},'else',{num,3},
      'else','if',{num,4},'then',{num,5},'else',{num,6}]),
  {'let',"c",
    {'-',{'+',{num,2},{num,3}},{num,4}},
    {'~',{'+',{'*',{num,2},{id,"c"}},{'*',{num,3},{num,4}}}}}
  = parser(['let',{id,"c"},'=','(','(',{num,2},'+',{num,3},')','-',{num,4},')',
      'in','~','(','(',{num,2},'*',{id,"c"},')','+','(',{num,3},'*',{num,4},')',')']),
  ok.

test_eval() ->
  1 = eval("((2+3)-4)"),
  -8.0 = eval("~((2*3)+(6/3))"),
  4.236 = eval("4.236"),
  42 = eval("if ((5+2)-7) then (6*7) else (3-2)"),
  1 = eval("if ((5+2)-6) then (6*7) else (3-2)"),
  -14 = eval("let c = ((2+3)-4) in ~((2*c)+(3*4))"),
  -5 = eval("let c=3 in let b = let c = (1*c) in (0+c) in if 0 then ((2+c)*~(4-b)) else 42"),
  ok.

test_pp() ->
  "((2 + 3) - 4)" = pretty_print(parse("((2+3)-4)")),
  "~((2 * 3) + (6 / 3))" = pretty_print(parse("~((2*3)+( 6/3))")),
  "4.236" = pretty_print(parse("4.236")),
  "if ((5 + 2) - 7) then (6 * 7) else (3 - 2)"= pretty_print(parse("if((5+2)-7)then (6*7)else(3-2)")),
  "if if 0 then 1 else 0 then if 1 then 2 else 3 else if 4 then 5 else 6"
  = pretty_print({'if', {'if', {num, 0}, {num, 1}, {num, 0}},
      {'if', {num, 1}, {num, 2}, {num, 3}},
      {'if', {num, 4}, {num, 5}, {num, 6}}}),
  "let c = ((2 + 3) - 4) in ~((2 * c) + (3 * 4))"
  = pretty_print(parse("letc=((2+3)-4)in~((2*c)+(3*4))")),
  ok.

test_compile() ->
  [2,3,'+',4,'-'] = compile("((2+3)-4)"),
  [2,3,'*',6,3,'/','+','~'] = compile("~((2*3)+(6/3))"),
  [4.236] = compile("4.236"),
  [[3,2,'-'],[6,7,'*'],5,2,'+',7,'-','if'] = compile("if ((5+2)-7) then (6*7) else (3-2)"),
  [2,3,'+',4,'-',{set,"c"},2,{get,"c"},'*',3,4,'*','+','~']
  = compile("let c = ((2+3)-4) in ~((2*c)+(3*4))"),
  ok.

test_simulator() ->
  1 = simulator([2,3,'+',4,'-']),
  -8.0 = simulator([2,3,'*',6,3,'/','+','~']),
  4.236 = simulator([4.236]),
  42 = simulator([[3,2,'-'],[6,7,'*'],5,2,'+',7,'-','if']),
  1 = simulator([[3,2,'-'],[6,7,'*'],5,2,'+',6,'-','if']),
  -14 = simulator([2,3,'+',4,'-',{set,"c"},2,{get,"c"},'*',3,4,'*','+','~']),
  -5 = simulator(compile("let c=3 in let b = let c = (1*c) in (0+c) in if 0 then ((2+c)*~(4-b)) else 42")),
  ok.

test_simplify() ->
  {'-',{'+',{num,2},{num,3}},{num,4}} = simplify("((2+3)-4)"),
  {'~',{'+',{'*',{num,2},{num,3}},{'/',{num,6},{num,3}}}} = simplify("~((2*3)+(6/3))"),
  {num,4.236} = simplify("4.236"),
  {num,0} = simplify("(0/(3+4))"),
  {num,0} = simplify("(0*(3+4))"),
  {num,0} = simplify("((6/3)*0.0)"),
  {num,0} = simplify("((6/3)*~((~0+0)*6))"),
  {num,0} = simplify("((6/3)*(4*0))"),
  {num,0} = simplify("((6/3)*~((0*8)/4))"),
  {num,0} = simplify("((6/3)*(0+0))"),
  {num,0} = simplify("((6/3)*(0-0))"),
  {num,5} = simplify("(5+0)"),
  {num,5} = simplify("(5-0)"),
  {num,3} = simplify("((0+3)*1)"),
  {num,7} = simplify("(1*(0+7))"),
  {num,0} = simplify("if(0*2)then(3*0)else5"),
  {num,5} = simplify("if(0+2)then(3*0)else(5+0)"),
  {'if',{'+',{num,1},{num,2}},{num,0},{num,5}} = simplify("if(1+2)then(3*0)else(5+0)"),
  {'let',"c",
    {'-',{'+',{num,2},{num,3}},{num,4}},
    {'~',{'+',{'*',{num,2},{id,"c"}},{'*',{num,3},{num,4}}}}}
  = simplify("let c = ((2+3)-4) in ~((2*c)+(3*4))"),
  {'*',{'+',{num,2},{num,3}},{'~',{'-',{num,4},{num,3}}}}
  = simplify("let c=3 in let b = let c = (1*c) in (0+c) in if 0 then ((2+c)*~(4-b)) else 42"),
  ok.
