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