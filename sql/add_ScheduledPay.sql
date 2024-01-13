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