CREATE TABLE "tbl_syunou_prv" (
	"hospid" character(24) NOT NULL,
	"nyugaikbn" character(1) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"denpnum" numeric(7,0) NOT NULL,
	"denplastnum" numeric(2,0),
	"sryka" character(2),
	"sryymd" character(8),
	"denpjtikbn" character(1),
	"createkbn" character(1),
	"denpprtymd" character(8),
	"skystymd" character(8),
	"skyedymd" character(8),
	"hkncombinum" character(4),
	"syuhknnum" character(3),
	"syuhknftnmoney" numeric(7,0),
	"syucompftn" numeric(7,0),
	"syucompftn_entani" numeric(8,1),
	"syucompykzftn" numeric(7,0),
	"syucomptotal" numeric(7,0),
	"syuykzftnkbn" character(1),
	"koh1hknnum" character(3),
	"koh1hknftnmoney" numeric(7,0),
	"koh1compftn" numeric(7,0),
	"koh1compftn_entani" numeric(8,1),
	"koh1compykzftn" numeric(7,0),
	"koh1comptotal" numeric(7,0),
	"koh1ykzftnkbn" character(1),
	"koh2hknnum" character(3),
	"koh2hknftnmoney" numeric(7,0),
	"koh2compftn" numeric(7,0),
	"koh2compftn_entani" numeric(8,1),
	"koh2compykzftn" numeric(7,0),
	"koh2comptotal" numeric(7,0),
	"koh2ykzftnkbn" character(1),
	"koh3hknnum" character(3),
	"koh3hknftnmoney" numeric(7,0),
	"koh3compftn" numeric(7,0),
	"koh3compftn_entani" numeric(8,1),
	"koh3compykzftn" numeric(7,0),
	"koh3comptotal" numeric(7,0),
	"koh3ykzftnkbn" character(1),
	"koh4hknnum" character(3),
	"koh4hknftnmoney" numeric(7,0),
	"koh4compftn" numeric(7,0),
	"koh4compftn_entani" numeric(8,1),
	"koh4compykzftn" numeric(7,0),
	"koh4comptotal" numeric(7,0),
	"koh4ykzftnkbn" character(1),
	"ptftnrate" numeric(3,0),
	"roomnum" character(8),
	"byotonum" character(2),
	"skykukbn" character(1),
	"tax_taishou" numeric(7,0),
	"tax_money" numeric(7,0),
	"skygk" numeric(7,0),
	"ssu_hknten" numeric(7,0),
	"ssu_money" numeric(7,0),
	"ssu_tgmoney" numeric(7,0),
	"ssu_tgmoney_tax" numeric(7,0),
	"sdo_hknten" numeric(7,0),
	"sdo_money" numeric(7,0),
	"sdo_tgmoney" numeric(7,0),
	"sdo_tgmoney_tax" numeric(7,0),
	"ztk_hknten" numeric(7,0),
	"ztk_money" numeric(7,0),
	"ztk_tgmoney" numeric(7,0),
	"ztk_tgmoney_tax" numeric(7,0),
	"tyk_hknten" numeric(7,0),
	"tyk_money" numeric(7,0),
	"tyk_tgmoney" numeric(7,0),
	"tyk_tgmoney_tax" numeric(7,0),
	"csy_hknten" numeric(7,0),
	"csy_money" numeric(7,0),
	"csy_tgmoney" numeric(7,0),
	"csy_tgmoney_tax" numeric(7,0),
	"syc_hknten" numeric(7,0),
	"syc_money" numeric(7,0),
	"syc_tgmoney" numeric(7,0),
	"syc_tgmoney_tax" numeric(7,0),
	"sjt_hknten" numeric(7,0),
	"sjt_money" numeric(7,0),
	"sjt_tgmoney" numeric(7,0),
	"sjt_tgmoney_tax" numeric(7,0),
	"kns_hknten" numeric(7,0),
	"kns_money" numeric(7,0),
	"kns_tgmoney" numeric(7,0),
	"kns_tgmoney_tax" numeric(7,0),
	"gzu_hknten" numeric(7,0),
	"gzu_money" numeric(7,0),
	"gzu_tgmoney" numeric(7,0),
	"gzu_tgmoney_tax" numeric(7,0),
	"etc_hknten" numeric(7,0),
	"etc_money" numeric(7,0),
	"etc_tgmoney" numeric(7,0),
	"etc_tgmoney_tax" numeric(7,0),
	"nyn_hknten" numeric(7,0),
	"nyn_money" numeric(7,0),
	"nyn_tgmoney" numeric(7,0),
	"nyn_tgmoney_tax" numeric(7,0),
	"total_hknten" numeric(7,0),
	"total_money" numeric(7,0),
	"total_tgmoney" numeric(7,0),
	"total_tgmoney_tax" numeric(7,0),
	"tgmoney_tax_sai" numeric(7,0),
	"jihi_1" numeric(7,0),
	"jihi_1_tax" numeric(7,0),
	"jihi_2" numeric(7,0),
	"jihi_2_tax" numeric(7,0),
	"jihi_3" numeric(7,0),
	"jihi_3_tax" numeric(7,0),
	"jihi_4" numeric(7,0),
	"jihi_4_tax" numeric(7,0),
	"jihi_5" numeric(7,0),
	"jihi_5_tax" numeric(7,0),
	"jihi_6" numeric(7,0),
	"jihi_6_tax" numeric(7,0),
	"jihi_7" numeric(7,0),
	"jihi_7_tax" numeric(7,0),
	"jihi_8" numeric(7,0),
	"jihi_8_tax" numeric(7,0),
	"jihi_9" numeric(7,0),
	"jihi_9_tax" numeric(7,0),
	"jihi_10" numeric(7,0),
	"jihi_10_tax" numeric(7,0),
	"jihi_total" numeric(7,0),
	"jihi_total_tax" numeric(7,0),
	"jihi_tax" numeric(7,0),
	"rjn_ftn" numeric(7,0),
	"koh_ftn" numeric(7,0),
	"koh_ftn_entani" numeric(8,1),
	"ykz_ftn" numeric(7,0),
	"rese_ykz_ftn" numeric(7,0),
	"kohtaiykzkbn" character(1),
	"chosei" numeric(7,0),
	"grp_denpnum" numeric(7,0),
	"grp_rennum" numeric(2,0),
	"grp_sgkmoney" numeric(7,0),
	"secmoney" numeric(7,0),
	"hkntekmoney" numeric(7,0),
	"discount_kbn" character(2),
	"discount_body" character varying(160),
	"discount_ratekbn" character(2),
	"discount_teiritu" character(1),
	"discount_rate" numeric(7,0),
	"discount_money" numeric(7,0),
	"rsishoshin_money" numeric(7,0),
	"rsisaishin_money" numeric(7,0),
	"rsisdo_money" numeric(7,0),
	"rsietc_money" numeric(7,0),
	"rsi_tax_sai" numeric(7,0),
	"rsi_total" numeric(7,0),
	"rsijibai_skymoney" numeric(7,0),
	"skymoney_tax_sai" numeric(7,0),
	"skymoney" numeric(7,0),
	"nyukin_total" numeric(7,0),
	"nyukin_kaisu" numeric(7,0),
	"misyuriyu" character(2),
	"drcd" character(5),
	"nynshokaisu" numeric(2,0),
	"ykzkennum" numeric(5,0),
	"rmsagaku" numeric(7,0),
	"rmsagaku_tax_sai" numeric(7,0),
	"shokuji1_nissu" numeric(2,0),
	"shokuji1" numeric(7,0),
	"shokuji2_nissu" numeric(2,0),
	"shokuji2" numeric(7,0),
	"shokuji3_nissu" numeric(2,0),
	"shokuji3" numeric(7,0),
	"shokuji4_nissu" numeric(2,0),
	"shokuji4" numeric(7,0),
	"shokuji5_nissu" numeric(2,0),
	"shokuji5" numeric(7,0),
	"shokuji6_nissu" numeric(2,0),
	"shokuji6" numeric(7,0),
	"shokuji7_nissu" numeric(2,0),
	"shokuji7" numeric(7,0),
	"syuskj_nissu" numeric(2,0),
	"syuskj_ryoyohi" numeric(7,0),
	"syuskj_ftn" numeric(7,0),
	"syuskj_ftn_rece" numeric(7,0),
	"syuskj_ftnkbn" character(1),
	"koh1skj_nissu" numeric(2,0),
	"koh1skj_ryoyohi" numeric(7,0),
	"koh1skj_ftn" numeric(7,0),
	"koh1skj_ftn_rece" numeric(7,0),
	"koh1skj_ftnkbn" character(1),
	"koh2skj_nissu" numeric(2,0),
	"koh2skj_ryoyohi" numeric(7,0),
	"koh2skj_ftn" numeric(7,0),
	"koh2skj_ftn_rece" numeric(7,0),
	"koh2skj_ftnkbn" character(1),
	"koh3skj_nissu" numeric(2,0),
	"koh3skj_ryoyohi" numeric(7,0),
	"koh3skj_ftn" numeric(7,0),
	"koh3skj_ftn_rece" numeric(7,0),
	"koh3skj_ftnkbn" character(1),
	"koh4skj_nissu" numeric(2,0),
	"koh4skj_ryoyohi" numeric(7,0),
	"koh4skj_ftn" numeric(7,0),
	"koh4skj_ftn_rece" numeric(7,0),
	"koh4skj_ftnkbn" character(1),
	"ryoyohi_skj" numeric(7,0),
	"skymoney_skj" numeric(7,0),
	"skymoney_skj_tax" numeric(7,0),
	"skymoney_skj_kei" numeric(7,0),
	"ryoyohi_skj_jihi" numeric(7,0),
	"skymoney_skj_jihi" numeric(7,0),
	"skymoney_skj_jihi_tax" numeric(7,0),
	"skymoney_skj_jihi_kei" numeric(7,0),
	"skj_ftngaku1" numeric(5,0),
	"skj_ftnday1" numeric(2,0),
	"skj_ftngaku2" numeric(5,0),
	"skj_ftnday2" numeric(2,0),
	"skj_ftngaku3" numeric(5,0),
	"skj_ftnday3" numeric(2,0),
	"saikeisankbn" character(1),
	"ingaishohokbn" character(1),
	"douji_denpnum" numeric(7,0),
	"contkbn" character(1),
	"day_1" character(1),
	"day_2" character(1),
	"day_3" character(1),
	"day_4" character(1),
	"day_5" character(1),
	"day_6" character(1),
	"day_7" character(1),
	"day_8" character(1),
	"day_9" character(1),
	"day_10" character(1),
	"day_11" character(1),
	"day_12" character(1),
	"day_13" character(1),
	"day_14" character(1),
	"day_15" character(1),
	"day_16" character(1),
	"day_17" character(1),
	"day_18" character(1),
	"day_19" character(1),
	"day_20" character(1),
	"day_21" character(1),
	"day_22" character(1),
	"day_23" character(1),
	"day_24" character(1),
	"day_25" character(1),
	"day_26" character(1),
	"day_27" character(1),
	"day_28" character(1),
	"day_29" character(1),
	"day_30" character(1),
	"day_31" character(1),
	"fuku_denpnum" numeric(7,0),
	"fuku_kbn" character(1),
	"zaitaku" character(1),
	"kyufugai_shoshin_ten" numeric(7,0),
	"kyufugai_sidou_ten" numeric(7,0),
	"kyufugai_osin1_ten" numeric(7,0),
	"kyufugai_osin2_ten" numeric(7,0),
	"kyufugai_osin3_ten" numeric(7,0),
	"kyufugai_gokei_ten" numeric(7,0),
	"dayinfflg" character(1),
	"nyuin_rrknum" numeric(3,0),
	"acct_updkbn" character(1),
	"jyo_hknftnmoney" numeric(7,0),
	"jyo_compftn" numeric(7,0),
	"jyo_compftn_entani" numeric(8,1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8) NOT NULL,
	"upymd" character(8),
	"uphms" character(6),
	"msi_hknten" numeric(7,0) DEFAULT 0,
	"msi_money" numeric(7,0) DEFAULT 0,
	"msi_tgmoney" numeric(7,0) DEFAULT 0,
	"msi_tgmoney_tax" numeric(7,0) DEFAULT 0,
	"ssn_hknten" numeric(7,0) DEFAULT 0,
	"ssn_money" numeric(7,0) DEFAULT 0,
	"ssn_tgmoney" numeric(7,0) DEFAULT 0,
	"ssn_tgmoney_tax" numeric(7,0) DEFAULT 0,
	"hou_hknten" numeric(7,0) DEFAULT 0,
	"hou_money" numeric(7,0) DEFAULT 0,
	"hou_tgmoney" numeric(7,0) DEFAULT 0,
	"hou_tgmoney_tax" numeric(7,0) DEFAULT 0,
	"ryo_hknten" numeric(7,0) DEFAULT 0,
	"ryo_money" numeric(7,0) DEFAULT 0,
	"ryo_tgmoney" numeric(7,0) DEFAULT 0,
	"ryo_tgmoney_tax" numeric(7,0) DEFAULT 0,
	"shohou_sai" numeric(7,0) DEFAULT 0,
	Constraint "tbl_syunou_prv_pkey" Primary Key ("hospid", "nyugaikbn", "ptid", "denpnum", "creymd")
);

COMMENT ON TABLE "tbl_syunou_prv" IS '��Ǽ(�ץ�ӥ塼)';

