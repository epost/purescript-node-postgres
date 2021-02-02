-- psql --username=testuser test

CREATE TABLE artist (
    name text NOT NULL,
    year int NOT NULL
);

CREATE table types (timestamp_no_tz timestamp without time zone);

ALTER TABLE artist ADD COLUMN isAlive boolean NOT NULL DEFAULT false;
