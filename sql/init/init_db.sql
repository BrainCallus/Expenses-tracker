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

CREATE TABLE expense (
                         id SERIAL NOT NULL,
                         sum DOUBLE PRECISION NOT NULL,
                         expenseType character varying(255) NOT NULL,
                         userId bigint NOT NULL,
                         date DATE NOT NULL,
                         creationTime timestamp NOT NULL,
                         primary key(id)
);

CREATE INDEX "index_Expense_id_sum_expenseType" ON expense ("id", "sum", "expensetype");
CREATE INDEX "index_Expense_sum_expenseType_userId" ON expense ("sum", "expensetype", "userid");
CREATE INDEX "index_Expense_userId_date_expenseType" ON expense ("userid", "date", "expensetype");
CREATE INDEX "index_Expense_sum_date_expenseType" ON expense ("sum", "date", "expensetype");
ALTER TABLE expense
    ADD CONSTRAINT fk_Expense_userId FOREIGN KEY (userid) REFERENCES "user" ("id");

CREATE TABLE scheduledPay
(
    id           SERIAL                 NOT NULL,
    sum          DOUBLE PRECISION       NOT NULL,
    expenseType  character varying(255) NOT NULL,
    userId       bigint                 NOT NULL,
    date         DATE                   NOT NULL,
    status       character varying(255) NOT NULL,
    creationTime timestamp              NOT NULL DEFAULT CURRENT_TIMESTAMP,
    primary key (id)
);

CREATE INDEX "index_scheduledPay_id_sum_expenseType" ON scheduledPay ("id", "sum", "expensetype");
CREATE INDEX "index_scheduledPay_sum_expenseType_userId" ON scheduledPay ("sum", "expensetype", "userid");
CREATE INDEX "index_scheduledPay_userId_status_date_expenseType" ON scheduledPay ("userid", "status", "date", "expensetype");
CREATE INDEX "index_scheduledPay_sum_date_status" ON scheduledPay ("sum", "date", "status");
ALTER TABLE scheduledPay
    ADD CONSTRAINT fk_scheduledPay_userId FOREIGN KEY (userid) REFERENCES "user" ("id");


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