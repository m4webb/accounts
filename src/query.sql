SELECT
    a.sid AS sid,
    a.date AS date,
    a.description AS description,
    a.account AS account,
    a.counter AS counter,
    a.kind AS kind,
    a.amount AS amount,
    SUM (
        CASE
        WHEN b.kind = 'debit' THEN b.amount
        ELSE -1*b.amount
        END
        ) AS balance
FROM (
    SELECT
        s.sid AS sid,
        a.aid AS aid,
        a.name AS account,
        t.date AS date,
        s.kind AS kind,
        s.amount AS amount,
        STRING_AGG(DISTINCT a2.name, ',') AS counter,
        t.description AS description
    FROM splits s
        LEFT JOIN accounts a ON s.aid = a.aid
        LEFT JOIN transactions t ON s.tid = t.tid
        LEFT JOIN splits s2 ON s2.kind != s.kind AND s.tid = s2.tid
        LEFT JOIN accounts a2 ON s2.aid = a2.aid
    WHERE
        a.aid = 2
        AND t.date >= '2016-01-01'
        AND t.date < '2017-01-01'
    GROUP BY
        s.sid,
        a.aid,
        t.date,
        s.kind,
        s.amount,
        t.description
    ) a
    LEFT JOIN (
        SELECT
            a.aid AS aid,
            t.date AS date,
            s.kind AS kind,
            s.amount AS amount
        FROM splits s
            LEFT JOIN accounts a ON s.aid = a.aid
            LEFT JOIN transactions t ON s.tid = t.tid
        ) b ON a.date >= b.date AND a.aid = b.aid
GROUP BY
    a.sid,
    a.date,
    a.account,
    a.kind,
    a.amount,
    a.counter,
    a.description
ORDER BY
    a.date DESC,
    a.sid DESC
;