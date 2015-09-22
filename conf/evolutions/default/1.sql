# --- !Ups

create table post (
  id                           bigint auto_increment not null,
  title                        varchar(1024),
  content                  clob,
  create_date            date,
  update_date           date,
  constraint pk_post primary key (id))
;

# --- !Downs

SET REFERENTIAL_INTEGRITY FALSE;

drop table if exists post;

SET REFERENTIAL_INTEGRITY TRUE;
