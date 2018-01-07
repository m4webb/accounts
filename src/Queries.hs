{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Database.PostgreSQL.Simple.Types

statementSelectQueryFmt = Query "\n\
\SELECT\n\
\    a.sid AS sid,\n\
\    a.date AS date,\n\
\    a.description AS description,\n\
\    a.account AS account,\n\
\    a.counter AS counter,\n\
\    TO_CHAR(a.amount * CASE WHEN a.kind = 'credit' THEN -1 ELSE 1 END, 'MI99990.00') AS amount,\n\
\    TO_CHAR(COALESCE(SUM (\n\
\        CASE\n\
\        WHEN b.kind = 'debit' THEN b.amount\n\
\        ELSE -1*b.amount\n\
\        END\n\
\        ), 0), 'MI99990.00') AS balance\n\
\FROM (\n\
\    SELECT\n\
\        s.sid AS sid,\n\
\        a.aid AS aid,\n\
\        a.name AS account,\n\
\        t.date AS date,\n\
\        s.kind AS kind,\n\
\        s.amount AS amount,\n\
\        COALESCE(STRING_AGG(DISTINCT a2.name, ','), '') AS counter,\n\
\        t.description AS description\n\
\    FROM splits s\n\
\        LEFT JOIN accounts a ON s.aid = a.aid\n\
\        LEFT JOIN transactions t ON s.tid = t.tid\n\
\        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid\n\
\        LEFT JOIN accounts a2 ON s2.aid = a2.aid\n\
\    WHERE\n\
\        a.aid = ?\n\
\        AND t.date >= ?\n\
\        AND t.date < ?\n\
\    GROUP BY\n\
\        s.sid,\n\
\        a.aid,\n\
\        t.date,\n\
\        s.kind,\n\
\        s.amount,\n\
\        t.description\n\
\    ) a\n\
\    LEFT JOIN (\n\
\        SELECT\n\
\            a.aid AS aid,\n\
\            t.date AS date,\n\
\            s.kind AS kind,\n\
\            s.amount AS amount\n\
\        FROM splits s\n\
\            LEFT JOIN accounts a ON s.aid = a.aid\n\
\            LEFT JOIN transactions t ON s.tid = t.tid\n\
\        WHERE\n\
\            t.date >= ?\n\
\            AND t.date < ?\n\
\        ) b ON a.date >= b.date AND a.aid = b.aid\n\
\GROUP BY\n\
\    a.sid,\n\
\    a.date,\n\
\    a.account,\n\
\    a.kind,\n\
\    a.amount,\n\
\    a.counter,\n\
\    a.description\n\
\ORDER BY\n\
\    a.date DESC,\n\
\    a.amount DESC,\n\
\    a.sid DESC\n\
\;"

statementSelectSingleQueryFmt = Query "\n\
\SELECT\n\
\    a.sid AS sid,\n\
\    a.date AS date,\n\
\    a.description AS description,\n\
\    a.account AS account,\n\
\    a.counter AS counter,\n\
\    TO_CHAR(a.amount * CASE WHEN a.kind = 'credit' THEN -1 ELSE 1 END, 'MI99990.00') AS amount,\n\
\    TO_CHAR(COALESCE(SUM (\n\
\        CASE\n\
\        WHEN b.kind = 'debit' THEN b.amount\n\
\        ELSE -1*b.amount\n\
\        END\n\
\        ), 0), 'MI99990.00') AS balance\n\
\FROM (\n\
\    SELECT\n\
\        s.sid AS sid,\n\
\        a.aid AS aid,\n\
\        a.name AS account,\n\
\        t.date AS date,\n\
\        s.kind AS kind,\n\
\        s.amount AS amount,\n\
\        COALESCE(STRING_AGG(DISTINCT a2.name, ','), '') AS counter,\n\
\        t.description AS description\n\
\    FROM splits s\n\
\        LEFT JOIN accounts a ON s.aid = a.aid\n\
\        LEFT JOIN transactions t ON s.tid = t.tid\n\
\        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid\n\
\        LEFT JOIN accounts a2 ON s2.aid = a2.aid\n\
\    WHERE\n\
\        s.sid = ?\n\
\    GROUP BY\n\
\        s.sid,\n\
\        a.aid,\n\
\        t.date,\n\
\        s.kind,\n\
\        s.amount,\n\
\        t.description\n\
\    ) a\n\
\    LEFT JOIN (\n\
\        SELECT\n\
\            a.aid AS aid,\n\
\            t.date AS date,\n\
\            s.kind AS kind,\n\
\            s.amount AS amount\n\
\        FROM splits s\n\
\            LEFT JOIN accounts a ON s.aid = a.aid\n\
\            LEFT JOIN transactions t ON s.tid = t.tid\n\
\        WHERE\n\
\            t.date >= ?\n\
\            AND t.date < ?\n\
\        ) b ON a.date >= b.date AND a.aid = b.aid\n\
\GROUP BY\n\
\    a.sid,\n\
\    a.date,\n\
\    a.account,\n\
\    a.kind,\n\
\    a.amount,\n\
\    a.counter,\n\
\    a.description\n\
\ORDER BY\n\
\    a.date DESC,\n\
\    a.amount DESC,\n\
\    a.sid DESC\n\
\;"

statementSelectCashQueryFmt = Query "\n\
\SELECT\n\
\    a.sid AS sid,\n\
\    a.date AS date,\n\
\    a.description AS description,\n\
\    a.account AS account,\n\
\    a.counter AS counter,\n\
\    TO_CHAR(a.amount * CASE WHEN a.kind = 'credit' THEN -1 ELSE 1 END, 'MI99990.00') AS amount,\n\
\    TO_CHAR(COALESCE(SUM (\n\
\        CASE\n\
\        WHEN b.kind = 'debit' THEN b.amount\n\
\        ELSE -1*b.amount\n\
\        END\n\
\        ), 0), 'MI99990.00') AS balance\n\
\FROM (\n\
\    SELECT\n\
\        s.sid AS sid,\n\
\        a.aid AS aid,\n\
\        a.name AS account,\n\
\        t.date AS date,\n\
\        s.kind AS kind,\n\
\        s.amount AS amount,\n\
\        COALESCE(STRING_AGG(DISTINCT a2.name, ','), '') AS counter,\n\
\        t.description AS description\n\
\    FROM splits s\n\
\        LEFT JOIN accounts a ON s.aid = a.aid\n\
\        LEFT JOIN transactions t ON s.tid = t.tid\n\
\        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid\n\
\        LEFT JOIN accounts a2 ON s2.aid = a2.aid\n\
\    WHERE\n\
\        a.name LIKE 'Cash%'\n\
\        AND t.description LIKE ?\n\
\        AND t.date >= ?\n\
\        AND t.date < ?\n\
\    GROUP BY\n\
\        s.sid,\n\
\        a.aid,\n\
\        t.date,\n\
\        s.kind,\n\
\        s.amount,\n\
\        t.description\n\
\    ) a\n\
\    LEFT JOIN (\n\
\        SELECT\n\
\            a.aid AS aid,\n\
\            t.date AS date,\n\
\            s.kind AS kind,\n\
\            s.amount AS amount\n\
\        FROM splits s\n\
\            LEFT JOIN accounts a ON s.aid = a.aid\n\
\            LEFT JOIN transactions t ON s.tid = t.tid\n\
\        WHERE\n\
\            a.name LIKE 'Cash%'\n\
\            AND t.description LIKE ?\n\
\            AND t.date >= ?\n\
\            AND t.date < ?\n\
\        ) b ON a.date >= b.date\n\
\GROUP BY\n\
\    a.sid,\n\
\    a.date,\n\
\    a.account,\n\
\    a.kind,\n\
\    a.amount,\n\
\    a.counter,\n\
\    a.description\n\
\ORDER BY\n\
\    a.date DESC,\n\
\    a.amount DESC,\n\
\    a.sid DESC\n\
\;"

statementSelectSingleCashQueryFmt = Query "\n\
\SELECT\n\
\    a.sid AS sid,\n\
\    a.date AS date,\n\
\    a.description AS description,\n\
\    a.account AS account,\n\
\    a.counter AS counter,\n\
\    TO_CHAR(a.amount * CASE WHEN a.kind = 'credit' THEN -1 ELSE 1 END, 'MI99990.00') AS amount,\n\
\    TO_CHAR(COALESCE(SUM (\n\
\        CASE\n\
\        WHEN b.kind = 'debit' THEN b.amount\n\
\        ELSE -1*b.amount\n\
\        END\n\
\        ), 0), 'MI99990.00') AS balance\n\
\FROM (\n\
\    SELECT\n\
\        s.sid AS sid,\n\
\        a.aid AS aid,\n\
\        a.name AS account,\n\
\        t.date AS date,\n\
\        s.kind AS kind,\n\
\        s.amount AS amount,\n\
\        COALESCE(STRING_AGG(DISTINCT a2.name, ','), '') AS counter,\n\
\        t.description AS description\n\
\    FROM splits s\n\
\        LEFT JOIN accounts a ON s.aid = a.aid\n\
\        LEFT JOIN transactions t ON s.tid = t.tid\n\
\        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid\n\
\        LEFT JOIN accounts a2 ON s2.aid = a2.aid\n\
\    WHERE\n\
\        s.sid = ?\n\
\    GROUP BY\n\
\        s.sid,\n\
\        a.aid,\n\
\        t.date,\n\
\        s.kind,\n\
\        s.amount,\n\
\        t.description\n\
\    ) a\n\
\    LEFT JOIN (\n\
\        SELECT\n\
\            a.aid AS aid,\n\
\            t.date AS date,\n\
\            s.kind AS kind,\n\
\            s.amount AS amount\n\
\        FROM splits s\n\
\            LEFT JOIN accounts a ON s.aid = a.aid\n\
\            LEFT JOIN transactions t ON s.tid = t.tid\n\
\        WHERE\n\
\            a.name LIKE 'Cash%'\n\
\            AND t.description LIKE ?\n\
\            AND t.date <= ?\n\
\            AND t.date > ?\n\
\        ) b ON a.date >= b.date\n\
\GROUP BY\n\
\    a.sid,\n\
\    a.date,\n\
\    a.account,\n\
\    a.kind,\n\
\    a.amount,\n\
\    a.counter,\n\
\    a.description\n\
\ORDER BY\n\
\    a.date DESC,\n\
\    a.amount DESC,\n\
\    a.sid DESC\n\
\;"
