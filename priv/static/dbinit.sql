grant all privileges on database mymfiles1 to public;
grant all privileges on database mymfiles1 to smithfarm;

create table mfiles (
  id serial NOT NULL,
  created_at timestamp NOT NULL,
  code_id int NOT NULL,
  sern serial NOT NULL,
  keyw varchar(120),
  file_desc varchar(320),
  primary key (id)
);

create table mfilecodes (
  id serial NOT NULL,
  code_str varchar(8) NOT NULL,
  primary key (id)
);

