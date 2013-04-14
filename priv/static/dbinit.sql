create table mfiles (
  id serial NOT NULL,
  created_at timestamp NOT NULL,
  code_id int NOT NULL,
  sern int NOT NULL UNIQUE,
  keyw varchar(120),
  file_desc varchar(320),
  primary key (id)
);

create table mfilecodes (
  id serial NOT NULL,
  created_at timestamp NOT NULL,
  code_id int NOT NULL UNIQUE,
  code_str varchar(8) NOT NULL UNIQUE,
  code_desc varchar(120),
  primary key (id)
);

