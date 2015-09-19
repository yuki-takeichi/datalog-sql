-- ancestor(X, Y) :- parent(X, Y).
-- ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

-- grandparent(me: X, him: Y) :- parent(me: X, him: Z), parent(me: Z, him: Y).
-- ancestor(me: X, him: Y) :- parent(me: X, him: Y).
-- ancestor(me: X, him: Y) :- parent(me: X, him: Z), ancestor(me: Z, him: Y).

-- ?(X) :- ancestor(me: yuki, him: X), grandparent(me: masaki, him: X).

with recursive ancestor as (
  select parent.me as me
       , parent.him as him
  from parent
  union all
  select parent.me as me
       , ancestor.him as him
  from parent
     , ancestor
  where parent.him = ancestor.me
)
, grandparent as (
  select parent1.me as me
       , parent2.him as him
  from parent as parent1
     , parent as parent2
  where parent1.him = parent2.me
)
select ancestor.him
from ancestor
   , grandparent
where ancestor.me = 'yuki'
  and grandparent.me = 'masaki'
  and ancestor.him = grandparent.him
;

with recursive grandparent as (
  select parent1.me as me
       , parent2.him as him
  from parent as parent1
     , parent as parent2
  where parent1.him = parent2.me
), ancestor as (
  select parent.me as me
       , parent.him as him
  from parent
  union all
  select parent.me as me
       , ancestor.him as him
  from parent
     , ancestor
  where parent.him = ancestor.me
)
select ancestor.him
from ancestor
   , grandparent
where ancestor.me = 'yuki'
  and grandparent.me = 'masaki'
  and ancestor.him = grandparent.him
;

-- Coooooooool!!
-- hoge :- parent.
-- foo :- hoge.
-- ? :- foo.
-- flattenWithClausesの挙動
with foo as (
  with hoge as (
    select * from parent
  )
  select * from hoge 
)
select * from foo;
-- ↑を↓に変換する
with hoge as (
  select * from parent
)
, foo as (
  select * from hoge
)
select * from foo;

-- forward referenceはダメ
with 
foo as (
  select * from hogerahoge
)
, hogerahoge as (
  select * from parent
)
select * from foo;

-- union allいっぱい書ける？
with ancestor as
(
  union all
  select * from father
  union all
  select * from mother
)
select * from grandparent;

-- 相互再帰はPostgresはダメ...orz
-- ancestor1(me: X, him: Y) :- parent(me: X, him: Y).
-- ancestor1(me: X, him: Y) :- parent(me: X, him: Z), ancestor2(me: Z, him: Y).
-- ancestor2(me: X, him: Y) :- parent(me: X, him: Y).
-- ancestor2(me: X, him: Y) :- parent(me: X, him: Z), ancestor1(me: Z, him: Y).
-- ?- ancestor1(me, yuki, him: Y).
-- ERROR:  mutual recursion between WITH items is not implemented
with recursive ancestor1 as (
  select parent.me as me
       , parent.him as him
  from parent
  union all
  select parent.me as me
       , ancestor2.him as him
  from parent
     , ancestor2
  where parent.him = ancestor2.me
)
, ancestor2 as (
  select parent.me as me
       , parent.him as him
  from parent
  union all
  select parent.me as me
       , ancestor1.him as him
  from parent
     , ancestor1
  where parent.him = ancestor1.me
)
select ancestor1.him
from ancestor1
where ancestor1.me = 'yuki'
;

-- union allの上下を逆にしてみる
-- ERROR:  recursive reference to query "ancestor" must not appear within its non-recursive term
with recursive ancestor as (
  select parent.me as me
       , ancestor.him as him
  from parent
     , ancestor
  where parent.him = ancestor.me
  union all
  select parent.me as me
       , parent.him as him
  from parent
)
select ancestor.him
from ancestor
where ancestor.me = 'yuki'
;

--
with recursive ancestor as (
  select parent.me as me
       , parent.him as him
  from parent
  union all --(この有無で挙動が変わる...OTZ)
  --select parent.me as me
  --     , parent.him as him
  select 'hoge' as me
       , 'foo' as him
  from parent
  union all
  select parent.me as me
       , ancestor.him as him
  from parent
     , ancestor
  where parent.him = ancestor.me
)
select ancestor.me
     , ancestor.him
from ancestor
;
-- ↑と↓は(多分)等価
with recursive ancestor as ( -- なんかだめっぽい...orz
  select parent.me as me
       , parent.him as him
  from parent
  union all
  (
    select 'hoge' as me
         , 'foo' as him
    from parent
    union
    select parent.me as me
         , ancestor.him as him
    from parent
       , ancestor
    where parent.him = ancestor.me
  )
)
select ancestor.me
     , ancestor.him
from ancestor
;


(with foo as (
  with hoge as (
    select * from parent
  )
  select * from hoge 
))
(select * from foo
union
with foo as (
  with hoge as (
    select * from parent
  )
  select * from hoge 
)
select * from foo);

(
  select * from parent limit 1
) union (
  select * from parent limit 1
)
