drop table if exists father;
drop table if exists mother;

create table father ( of text, me text) ;
create table mother ( of text, me text) ;

insert into father (of, me) values('tara', 'masuo');
insert into mother (of, me) values('tara', 'sazae');
insert into father (of, me) values('sazae', 'fune');
insert into mother (of, me) values('sazae', 'namihei');
