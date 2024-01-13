CREATE TABLE "user" (
    id          SERIAL                 NOT NULL,
    login        character varying(255) UNIQUE NOT NULL,
    name         character varying(255) NOT NULL,
    password     character varying(255) NOT NULL,
    creationTime timestamp              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    primary key (id)
);

CREATE INDEX "index_user_id_name_login" ON "user" ("id", "name", "login");
CREATE INDEX "index_user_id_login_password_creationTime" ON "user" ("id", "login", "password", "creationtime");