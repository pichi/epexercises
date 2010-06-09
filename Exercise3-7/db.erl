-module(db).

-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

-export([test/0]).

% db:new() ⇒ Db.
% db:destroy(Db) ⇒ ok.
% db:write(Key, Element, Db) ⇒ NewDb.
% db:delete(Key, Db) ⇒ NewDb.
% db:read(Key, Db) ⇒{ok, Element} | {error, instance}.
% db:match(Element, Db) ⇒ [Key1, ..., KeyN].

new() -> [].

destroy(X) when is_list(X) -> ok.

write(Key, Element, Db) -> [{Key, Element}|lists:keydelete(Key, 1, Db)].

delete(Key, Db) -> lists:keydelete(Key, 1, Db).

read(Key, Db) -> read(lists:keyfind(Key, 1, Db)).

read(false) -> {error, instance};
read({_, Element}) -> {ok, Element}.

match(Element, Db) -> [Key || {Key, X}<-Db, X=:=Element].

test() ->
  [] = Db = db:new(),
  [{francesco,london}] = Db1 = db:write(francesco, london, Db),
  [{lelle,stockholm},{francesco,london}] = Db2 = db:write(lelle, stockholm, Db1),
  {ok,london} = db:read(francesco, Db2),
  [{joern,stockholm},{lelle,stockholm},{francesco,london}] = Db3 = db:write(joern, stockholm, Db2),
  {error,instance}= db:read(ola, Db3),
  [joern,lelle] = db:match(stockholm, Db3),
  [{joern,stockholm},{francesco,london}] = Db4 = db:delete(lelle, Db3),
  [{francesco,prague},{joern,stockholm}] = db:write(francesco, prague, Db4),
  [joern] = db:match(stockholm, Db4),
  ok.
