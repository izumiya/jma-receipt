\set ON_ERROR_STOP

CREATE VIEW view_bd001 AS
SELECT a.hospnum,
       a.ptid,
       b.ptnum,
       c.name,
       c.sex,
       c.birthday,
       a.sryka,
       a.denpprtymd,
       a.skymoney,
       a.nyukin_total
FROM tbl_syunou_main a,tbl_ptnum b,tbl_ptinf c
WHERE ((((((((a.hospnum = b.hospnum)
  AND (a.ptid = b.ptid))
  AND (a.hospnum = c.hospnum))
  AND (a.ptid = c.ptid))
  AND (a.denpjtikbn <> '3'))
  AND (a.denpjtikbn <> '7'))
  AND (a.createkbn <> '3'))
  AND (c.tstptnumkbn <> '1'));

SET client_encoding = 'EUC_JP';
COMMENT ON VIEW view_bd001 IS '��Ǽ�ӥ塼';


CREATE VIEW view_bd002 AS
SELECT c.ptnum,a.nyugaikbn,a.ptid,a.denpnum,a.denplastnum,a.sryka,a.sryymd,a.denpjtikbn,a.createkbn,a.denpprtymd,a.skystymd,a.skyedymd,a.hkncombinum,
a.syuhknnum,a.syuhknftnmoney,a.syucompftn,a.syucompftn_entani,a.syucompykzftn,a.syucomptotal,a.syuykzftnkbn,a.koh1hknnum,a.koh1hknftnmoney,a.koh1compftn,
a.koh1compftn_entani,a.koh1compykzftn,a.koh1comptotal,a.koh1ykzftnkbn,a.koh2hknnum,a.koh2hknftnmoney,a.koh2compftn,a.koh2compftn_entani,a.koh2compykzftn,
a.koh2comptotal,a.koh2ykzftnkbn,a.koh3hknnum,a.koh3hknftnmoney,a.koh3compftn,a.koh3compftn_entani,a.koh3compykzftn,a.koh3comptotal,a.koh3ykzftnkbn,
a.koh4hknnum,a.koh4hknftnmoney,a.koh4compftn,a.koh4compftn_entani,a.koh4compykzftn,a.koh4comptotal,a.koh4ykzftnkbn,a.ptftnrate,a.skykukbn,a.tax_taishou,
a.tax_money,a.skygk,a.ssu_hknten,a.ssu_money,a.ssu_tgmoney,a.ssu_tgmoney_tax,a.sdo_hknten,a.sdo_money,a.sdo_tgmoney,a.sdo_tgmoney_tax,a.ztk_hknten,
a.ztk_money,a.ztk_tgmoney,a.ztk_tgmoney_tax,a.tyk_hknten,a.tyk_money,a.tyk_tgmoney,a.tyk_tgmoney_tax,a.csy_hknten,a.csy_money,a.csy_tgmoney,
a.csy_tgmoney_tax,a.syc_hknten,a.syc_money,a.syc_tgmoney,a.syc_tgmoney_tax,a.sjt_hknten,a.sjt_money,a.sjt_tgmoney,a.sjt_tgmoney_tax,a.kns_hknten,
a.kns_money,a.kns_tgmoney,a.kns_tgmoney_tax,a.gzu_hknten,a.gzu_money,a.gzu_tgmoney,a.gzu_tgmoney_tax,a.etc_hknten,a.etc_money,a.etc_tgmoney,
a.etc_tgmoney_tax,a.nyn_hknten,a.nyn_money,a.nyn_tgmoney,a.nyn_tgmoney_tax,a.total_hknten,a.total_money,a.total_tgmoney,a.total_tgmoney_tax,
a.tgmoney_tax_sai,a.jihi_1,a.jihi_1_tax,a.jihi_2,a.jihi_2_tax,a.jihi_3,a.jihi_3_tax,a.jihi_4,a.jihi_4_tax,a.jihi_5,a.jihi_5_tax,a.jihi_6,a.jihi_6_tax,
a.jihi_7,a.jihi_7_tax,a.jihi_8,a.jihi_8_tax,a.jihi_9,a.jihi_9_tax,a.jihi_10,a.jihi_10_tax,a.jihi_total,a.jihi_total_tax,a.jihi_tax,a.rjn_ftn,a.koh_ftn,
a.koh_ftn_entani,a.ykz_ftn,a.rese_ykz_ftn,a.kohtaiykzkbn,a.chosei,a.chosei1,a.chosei2,a.grp_denpnum,a.grp_rennum,a.grp_sgkmoney,a.secmoney,a.hkntekmoney,
a.discount_kbn,a.discount_body,a.discount_ratekbn,a.discount_teiritu,a.discount_rate,a.discount_money,a.rsishoshin_money,a.rsisaishin_money,a.rsisdo_money,
a.rsietc_money,a.rsi_tax_sai,a.rsi_total,a.rsijibai_skymoney,a.skymoney_tax_sai,a.skymoney,a.nyukin_total,a.nyukin_kaisu,a.misyuriyu,a.drcd,a.nynshokaisu,
a.ykzkennum,a.skj_ftngaku1,a.skj_ftnday1,a.skj_ftngaku2,a.skj_ftnday2,a.skj_ftngaku3,a.skj_ftnday3,a.saikeisankbn,a.ingaishohokbn,a.douji_denpnum,a.contkbn,
a.fuku_denpnum,a.fuku_kbn,a.zaitaku,a.kyufugai_shoshin_ten,a.kyufugai_sidou_ten,a.kyufugai_osin1_ten,a.kyufugai_osin2_ten,a.kyufugai_osin3_ten,
a.kyufugai_gokei_ten,a.dayinfflg,a.acct_updkbn,a.jyo_hknftnmoney,a.jyo_compftn,a.jyo_compftn_entani,a.termid,a.opid,a.creymd,a.upymd,a.uphms,a.msi_hknten,
a.msi_money,a.msi_tgmoney,a.msi_tgmoney_tax,a.ssn_hknten,a.ssn_money,a.ssn_tgmoney,a.ssn_tgmoney_tax,a.hou_hknten,a.hou_money,a.hou_tgmoney,
a.hou_tgmoney_tax,a.ryo_hknten,a.ryo_money,a.ryo_tgmoney,a.ryo_tgmoney_tax,a.shohou_sai,a.grp_hakhouflg,a.hospnum,a.byr_hknten,a.byr_money,a.byr_tgmoney,
a.byr_tgmoney_tax,COALESCE(b.roomnum,''::bpchar) AS roomnum,COALESCE(b.byotonum,''::bpchar) AS byotonum,COALESCE(b.rmsagaku,0) AS rmsagaku,
COALESCE(b.rmsagaku_tax_sai,0) AS rmsagaku_tax_sai,COALESCE((b.shokuji1_nissu)::integer,0) AS shokuji1_nissu,COALESCE(b.shokuji1,0) AS shokuji1,
COALESCE((b.shokuji2_nissu)::integer,0) AS shokuji2_nissu,COALESCE(b.shokuji2,0) AS shokuji2,COALESCE((b.shokuji3_nissu)::integer,0) AS shokuji3_nissu,
COALESCE(b.shokuji3,0) AS shokuji3,COALESCE((b.shokuji4_nissu)::integer,0) AS shokuji4_nissu,COALESCE(b.shokuji4,0) AS shokuji4,
COALESCE((b.shokuji5_nissu)::integer,0) AS shokuji5_nissu,COALESCE(b.shokuji5,0) AS shokuji5,COALESCE((b.shokuji6_nissu)::integer,0) AS shokuji6_nissu,
COALESCE(b.shokuji6,0) AS shokuji6,COALESCE((b.shokuji7_nissu)::integer,0) AS shokuji7_nissu,COALESCE(b.shokuji7,0) AS shokuji7,
COALESCE((b.syuskj_nissu)::integer,0) AS syuskj_nissu,COALESCE(b.syuskj_ryoyohi,0) AS syuskj_ryoyohi,COALESCE(b.syuskj_ftn,0) AS syuskj_ftn,
COALESCE(b.syuskj_ftn_rece,0) AS syuskj_ftn_rece,COALESCE(b.syuskj_ftnkbn,''::bpchar) AS syuskj_ftnkbn,
COALESCE((b.koh1skj_nissu)::integer,0) AS koh1skj_nissu,COALESCE(b.koh1skj_ryoyohi,0) AS koh1skj_ryoyohi,COALESCE(b.koh1skj_ftn,0) AS koh1skj_ftn,
COALESCE(b.koh1skj_ftn_rece,0) AS koh1skj_ftn_rece,COALESCE(b.koh1skj_ftnkbn,''::bpchar) AS koh1skj_ftnkbn,
COALESCE((b.koh2skj_nissu)::integer,0) AS koh2skj_nissu,COALESCE(b.koh2skj_ryoyohi,0) AS koh2skj_ryoyohi,COALESCE(b.koh2skj_ftn,0) AS koh2skj_ftn,
COALESCE(b.koh2skj_ftn_rece,0) AS koh2skj_ftn_rece,COALESCE(b.koh2skj_ftnkbn,''::bpchar) AS koh2skj_ftnkbn,
COALESCE((b.koh3skj_nissu)::integer,0) AS koh3skj_nissu,COALESCE(b.koh3skj_ryoyohi,0) AS koh3skj_ryoyohi,COALESCE(b.koh3skj_ftn,0) AS koh3skj_ftn,
COALESCE(b.koh3skj_ftn_rece,0) AS koh3skj_ftn_rece,COALESCE(b.koh3skj_ftnkbn,''::bpchar) AS koh3skj_ftnkbn,
COALESCE((b.koh4skj_nissu)::integer,0) AS koh4skj_nissu,COALESCE(b.koh4skj_ryoyohi,0) AS koh4skj_ryoyohi,COALESCE(b.koh4skj_ftn,0) AS koh4skj_ftn,
COALESCE(b.koh4skj_ftn_rece,0) AS koh4skj_ftn_rece,COALESCE(b.koh4skj_ftnkbn,''::bpchar) AS koh4skj_ftnkbn,
COALESCE(b.ryoyohi_skj,0) AS ryoyohi_skj,COALESCE(b.skymoney_skj,0) AS skymoney_skj,COALESCE(b.skymoney_skj_tax,0) AS skymoney_skj_tax,
COALESCE(b.skymoney_skj_kei,0) AS skymoney_skj_kei,COALESCE(b.ryoyohi_skj_jihi,0) AS ryoyohi_skj_jihi,
COALESCE(b.skymoney_skj_jihi,0) AS skymoney_skj_jihi,COALESCE(b.skymoney_skj_jihi_tax,0) AS skymoney_skj_jihi_tax,
COALESCE(b.skymoney_skj_jihi_kei,0) AS skymoney_skj_jihi_kei,COALESCE(b.day_1,''::bpchar) AS day_1,COALESCE(b.day_2,''::bpchar) AS day_2,
COALESCE(b.day_3,''::bpchar) AS day_3,COALESCE(b.day_4,''::bpchar) AS day_4,COALESCE(b.day_5,''::bpchar) AS day_5,COALESCE(b.day_6,''::bpchar) AS day_6,
COALESCE(b.day_7,''::bpchar) AS day_7,COALESCE(b.day_8,''::bpchar) AS day_8,COALESCE(b.day_9,''::bpchar) AS day_9,COALESCE(b.day_10,''::bpchar) AS day_10,
COALESCE(b.day_11,''::bpchar) AS day_11,COALESCE(b.day_12,''::bpchar) AS day_12,COALESCE(b.day_13,''::bpchar) AS day_13,
COALESCE(b.day_14,''::bpchar) AS day_14,COALESCE(b.day_15,''::bpchar) AS day_15,COALESCE(b.day_16,''::bpchar) AS day_16,
COALESCE(b.day_17,''::bpchar) AS day_17,COALESCE(b.day_18,''::bpchar) AS day_18,COALESCE(b.day_19,''::bpchar) AS day_19,
COALESCE(b.day_20,''::bpchar) AS day_20,COALESCE(b.day_21,''::bpchar) AS day_21,COALESCE(b.day_22,''::bpchar) AS day_22,
COALESCE(b.day_23,''::bpchar) AS day_23,COALESCE(b.day_24,''::bpchar) AS day_24,COALESCE(b.day_25,''::bpchar) AS day_25,
COALESCE(b.day_26,''::bpchar) AS day_26,COALESCE(b.day_27,''::bpchar) AS day_27,COALESCE(b.day_28,''::bpchar) AS day_28,
COALESCE(b.day_29,''::bpchar) AS day_29,COALESCE(b.day_30,''::bpchar) AS day_30,COALESCE(b.day_31,''::bpchar) AS day_31,
COALESCE((b.nyuin_rrknum)::integer,0) AS nyuin_rrknum,COALESCE((b.shokuji8_nissu)::integer,0) AS shokuji8_nissu,COALESCE(b.shokuji8,0) AS shokuji8,
COALESCE((b.shokuji9_nissu)::integer,0) AS shokuji9_nissu,COALESCE(b.shokuji9,0) AS shokuji9,COALESCE((b.shokuji10_nissu)::integer,0) AS shokuji10_nissu,
COALESCE(b.shokuji10,0) AS shokuji10,COALESCE(b.ryoyohi_life,0) AS ryoyohi_life,COALESCE(b.skymoney_life,0) AS skymoney_life,
COALESCE(b.skymoney_life_tax,0) AS skymoney_life_tax,COALESCE(b.skymoney_life_kei,0) AS skymoney_life_kei,
COALESCE(b.ryoyohi_life_jihi,0) AS ryoyohi_life_jihi,COALESCE(b.skymoney_life_jihi,0) AS skymoney_life_jihi,
COALESCE(b.skymoney_life_jihi_tax,0) AS skymoney_life_jihi_tax,COALESCE(b.skymoney_life_jihi_kei,0) AS skymoney_life_jihi_kei
FROM (tbl_syunou_main a LEFT JOIN tbl_syunou_nyuin b USING (hospnum,nyugaikbn,ptid,denpnum)),tbl_ptnum c,tbl_ptinf d
WHERE ((((((((a.hospnum)::numeric = c.hospnum) AND ((a.ptid)::numeric = c.ptid)) AND (a.hospnum = d.hospnum))
AND (a.ptid = d.ptid)) AND (a.denpjtikbn <> '3'::bpchar)) AND (a.createkbn <> '3'::bpchar)) AND (d.tstptnumkbn <> '1'::bpchar));

SET client_encoding = 'EUC_JP';
COMMENT ON VIEW view_bd002 IS '��Ǽ�ӥ塼2';


CREATE VIEW view_i001 AS
SELECT a.hospnum,a.ptid,b.rrknum,b.rrkedanum,e.rrkedanum_max,c.ptnum,a.kananame,a.name,a.sex,a.birthday,a.home_post,a.home_adrs,
a.home_banti,a.deathkbn,b.btunum,b.brmnum,b.nyuinka,b.nyuinymd,b.taiinymd,b.taiincd,b.tennyuymd,b.tenstuymd,a.tstptnumkbn
FROM tbl_ptnyuinrrk b,tbl_ptnyuinrrk d,(SELECT tbl_ptnyuinrrk.hospnum,tbl_ptnyuinrrk.ptid,tbl_ptnyuinrrk.rrknum,
max(tbl_ptnyuinrrk.rrkedanum) AS rrkedanum_max FROM tbl_ptnyuinrrk GROUP BY tbl_ptnyuinrrk.hospnum,tbl_ptnyuinrrk.ptid,
tbl_ptnyuinrrk.rrknum) e,tbl_ptinf a,tbl_ptnum c 
WHERE ((((((((((((((((b.hospnum = a.hospnum) AND (b.ptid = a.ptid)) AND (b.hospnum = c.hospnum)) AND (b.ptid = c.ptid))
AND (b.hospnum = d.hospnum)) AND (b.ptid = d.ptid)) AND (a.rrknum <> (0)::numeric)) AND (a.rrkedanum <> (0)::numeric))
AND (a.rrknum = d.rrknum)) AND (a.rrkedanum = d.rrkedanum)) AND (d.kensaku_dispkbn = '1'::bpchar)) AND (b.jtikbn <> '5'::bpchar))
AND (b.jtikbn <> '6'::bpchar)) AND (b.hospnum = e.hospnum)) AND (b.ptid = e.ptid)) AND (b.rrknum = e.rrknum));

SET client_encoding = 'EUC_JP';
COMMENT ON VIEW view_i001 IS '�������ԾȲ�ӥ塼';
