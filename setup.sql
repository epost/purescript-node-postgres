-- psql --username=testuser test

CREATE TABLE artist (
    name text NOT NULL,
    year int NOT NULL
);

insert into artist values ('Led Zeppelin', 1968);
insert into artist values ('Deep Purple', 1968);
insert into artist values ('Yes', 1968);
