BEGIN TRANSACTION;

CREATE TABLE document (
    id integer primary key,
    fulltitle,
    fulltitle_lc
);
COMMIT;
