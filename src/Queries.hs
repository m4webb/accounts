{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Database.PostgreSQL.Simple.Types

statementSelectQueryFmt = Query "\n\
\SELECT\n\
\    a.date AS date,\n\
\    a.kind AS kind,\n\
\    a.amount AS amount,\n\
\    a.counter AS counter,\n\
\    a.description AS description,\n\
\    SUM (\n\
\        CASE\n\
\        WHEN b.kind = 'debit' THEN b.amount\n\
\        ELSE -1*b.amount\n\
\        END\n\
\        ) AS balance\n\
\FROM (\n\
\    SELECT\n\
\        s.sid AS sid,\n\
\        a.aid AS aid,\n\
\        t.date AS date,\n\
\        s.kind AS kind,\n\
\        s.amount AS amount,\n\
\        STRING_AGG(DISTINCT a2.name, ',') AS counter,\n\
\        t.description AS description\n\
\    FROM splits s\n\
\        LEFT JOIN accounts a ON s.aid = a.aid\n\
\        LEFT JOIN transactions t ON s.tid = t.tid\n\
\        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid\n\
\        LEFT JOIN accounts a2 ON s2.aid = a2.aid\n\
\    WHERE\n\
\        a.aid = ?\n\
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
\        ) b ON a.date >= b.date AND a.aid = b.aid\n\
\GROUP BY\n\
\    a.sid,\n\
\    a.date,\n\
\    a.kind,\n\
\    a.amount,\n\
\    a.counter,\n\
\    a.description\n\
\ORDER BY\n\
\    a.date,\n\
\    a.sid\n\
\;"
