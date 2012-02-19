PRAGMA journal_mode=WAL;
BEGIN TRANSACTION;
CREATE TABLE author (
    id integer primary key,
    name unique,
    name_lc,
    first,
    middle,
    last,
    nick,
    email
);
CREATE TABLE docauthor (
    document_id integer references document(id),
    author_id integer references author(id)
);
CREATE TABLE genre (
    id integer primary key,
    genre unique
);
CREATE TABLE docgenre (
    document_id integer references document(id),
    genre_id integer references genre(id)
);
CREATE TABLE lang (
    id integer primary key,
    lang unique
);
CREATE TABLE document (
    id integer primary key,
    fulltitle,
    fulltitle_lc,
    title,
    title_lc,
    lang integer references lang (id),
    date
);
COMMIT;
