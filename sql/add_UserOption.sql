CREATE TABLE userOption
(
    id           SERIAL                        NOT NULL,
    key          character varying(255)        NOT NULL,
    value        character varying(255)        NOT NULL,
    userId       bigint                        NOT NULL,
    updationTime timestamp                     NOT NULL DEFAULT current_timestamp,
    creationTime timestamp                     NOT NULL DEFAULT CURRENT_TIMESTAMP,
    primary key (id)
);

CREATE INDEX "index_userOption_key_userId_updationTime" ON useroption ("key", "userid", "updationtime");
CREATE INDEX "index_userOption_key_userId_creationTime" ON useroption ("key", "userid", "creationtime");
CREATE UNIQUE INDEX "index_unique_userOption_key_userId" ON useroption ("key", "userid");
ALTER TABLE useroption
    ADD CONSTRAINT fk_userOption_userId FOREIGN KEY (userid) REFERENCES "user" ("id");