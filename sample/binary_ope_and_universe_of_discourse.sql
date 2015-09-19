-- 各ユーザーごとの登録から直近3ヶ月までの写真アップロード頻度
-- userRange(objectId: ObjectId, fromTime: FromTime, toTime: ToTime)
--  :- user(objectId: ObjectId, telNumberVerified: true)
--   , ToTime is FromTime + interval '3 months'.
-- userPhoto3Months(interval: Interval)
--  :- userPhoto(ownerId: UserId, createdAt: CreatedAt)
--   , userRange(objectId: UserId, fromTime: FromTime, toTime: ToTime)
--   , FromTime <= CreatedAt -- これを前に持ってくると現行のDatalog実装だと上手くうごかない？ (これはどう解釈すればいいんだろう？関係と二項演算子を同等とみなすことでDatalogはパワーを得るわけだけど、その代償として、SQLに変換するときに色々とやらないといけない？具体的には他のTupleRefで変数が使われていたらそのAttrで置き換える的なことをしないといけないのかな？
--   , CreatedAt < ToTime
--   , Interval is CreatedAt - FromTime

with "UserRange" as (
  select "objectId"
       , "fromTime"
       , "createdAt" + interval '3 months' as "toTime"
  from "User"
  where "telNumberVerified" is true
)
, "UserPhoto3Months" as (
  select "UserPhoto"."createdAt" - "UserRange"."fromTime" as "interval"
  from "UserPhoto"
     , "UserRange"
  where "UserPhoto"."ownerId" = "UserRange"."objectId"
    and "UserRange"."fromTime" <= "UserPhoto"."createdAt"
    and "UserPhoto"."createdAt" < "UserRange"."toTime"
)
select percentile(30, "interval")
from "UserPhoto3Months"
;

-- たとえば、
-- ?(ans: X) :- X = hoge, Y > 10.
-- これを関係論理的に書き直すと、
-- exists X, Y ( X = hoge, Y > 10).
-- これを満たすYは無限に存在する?
-- いや、Yのドメインが決まらないから、真偽が決定できない。
-- この場合は、YがあるTupleのAttrにbindされていないというエラーを出すのかな？
-- そのエラーの根拠となっているのは、多分、このDatalog式はSQLに変換することが出来ないから。
-- ?(ans: X) :- X = hoge.
-- これも多分変換できないんじゃないかな？
-- XのDomain of discourseが与えられていないから。
-- 多分、SQLに変数を入れることの一番の難しさは、Domain of discourseが決められるように
-- reasonableな制約を表現力に加えること。(しかし実用性を失わないように。)
-- ?(ans: X) :- true.
-- ?(ans: X) :- false.
-- こういう述語をサポートするか？
-- これはサポートできない。同様に、
-- ?(ans: X) :- user(objectId: Id), orderHistory(ownerId: Id).
-- これもサポートできない。
-- 前者は0引数の述語をサポートしないということを意味しない。寧ろ、後者と合わせて、
-- DoDの決まらない変数が入っていない場合はクエリをrejectするというルールを意味している。
-- doesYukiHaveAnyOrder :- user(objectId: Id), orderHistory(ownerId: Id).
