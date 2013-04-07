grant all privileges on database mymfiles1 to public;
grant all privileges on database mymfiles1 to smithfarm;

create table mfiles (
  id serial NOT NULL,
  created_at timestamp NOT NULL,
  c_ptr int NOT NULL,
  sern serial NOT NULL,
  keyw varchar(120),
  file_desc varchar(320),
  primary key (id)
);

create table mfilecodes (
  id serial NOT NULL,
  c_ptr serial NOT NULL,
  code_str varchar(8),
  primary key (id)
);

