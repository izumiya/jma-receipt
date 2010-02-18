CREATE VIEW view_q002 AS
    SELECT a.hospnum,
           a.ptid,
           a.nyugaikbn,
           a.sryka,
           a.srykbn,
           a.srysyukbn,
           b.sryymd,
           a.srycd1,
           a.srycd2,
           a.srycd3,
           a.srycd4,
           a.srycd5 
      FROM tbl_sryact a,
           tbl_jyurrk b 
     WHERE ((((((a.hospnum)::numeric = b.hospnum) 
       AND ((a.ptid)::numeric = b.ptid)) 
       AND (a.nyugaikbn = b.nyugaikbn)) 
       AND (a.sryka = b.sryka)) 
       AND ((((((((((((((((a.zainum)::numeric = b.zainum1) 
        OR ((a.zainum)::numeric = b.zainum2)) 
        OR ((a.zainum)::numeric = b.zainum3)) 
        OR ((a.zainum)::numeric = b.zainum4)) 
        OR ((a.zainum)::numeric = b.zainum5)) 
        OR ((a.zainum)::numeric = b.zainum6)) 
        OR ((a.zainum)::numeric = b.zainum7)) 
        OR ((a.zainum)::numeric = b.zainum8)) 
        OR ((a.zainum)::numeric = b.zainum9)) 
        OR ((a.zainum)::numeric = b.zainum10)) 
        OR ((a.zainum)::numeric = b.zainum11)) 
        OR ((a.zainum)::numeric = b.zainum12)) 
        OR ((a.zainum)::numeric = b.zainum13)) 
        OR ((a.zainum)::numeric = b.zainum14)) 
        OR ((a.zainum)::numeric = b.zainum15))) 
     UNION ALL SELECT a.hospnum,
           a.ptid,
           '1' AS nyugaikbn,
           b.sryka,
           b.srykbn,
           '   '::bpchar AS srysyukbn,
           a.sryymd,
           b.srycd AS srycd1,
           b.srycd AS srycd2,
           b.srycd AS srycd3,
           b.srycd AS srycd4,
           b.srycd AS srycd5 
      FROM tbl_nrrksrh a,
           tbl_nsrysrh b 
     WHERE (((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.zainum = b.zainum));


COMMENT ON VIEW view_q002 IS '���ԾȲ�ӥ塼2';

