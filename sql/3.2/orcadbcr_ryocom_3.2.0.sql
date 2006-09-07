create	table	tbl_ryocom	(
	HOSPID	        char(24) not null,
	NYUGAIKBN	char(1) not null,
	PTID	        numeric(10,0) not null, 
	SRYYM	        char(6) not null,
	ZAINUM	        numeric(8,0) not null,
	ZAISKBKBN	char(1),
	MONTH	        numeric(2,0),
	DAY_1	        numeric(2,0),
	DAY_2	        numeric(2,0),
	DAY_3	        numeric(2,0),
	DAY_4	        numeric(2,0),
	DAY_5	        numeric(2,0),
	DAY_6	        numeric(2,0),
	DAY_7	        numeric(2,0),
	DAY_8	        numeric(2,0),
	DAY_9	        numeric(2,0),
	DAY_10	        numeric(2,0),
	DAY_11	        numeric(2,0),
	DAY_12	        numeric(2,0),
	DAY_13	        numeric(2,0),
	DAY_14	        numeric(2,0),
	DAY_15	        numeric(2,0),
	DAY_16	        numeric(2,0),
	DAY_17	        numeric(2,0),
	DAY_18	        numeric(2,0),
	DAY_19	        numeric(2,0),
	DAY_20	        numeric(2,0),
	DAY_21	        numeric(2,0),
	DAY_22	        numeric(2,0),
	DAY_23	        numeric(2,0),
	DAY_24	        numeric(2,0),
	DAY_25	        numeric(2,0),
	DAY_26	        numeric(2,0),
	DAY_27	        numeric(2,0),
	DAY_28	        numeric(2,0),
	DAY_29	        numeric(2,0),
	DAY_30	        numeric(2,0),
	DAY_31	        numeric(2,0),
	TERMID	        varchar(16),
	OPID	        varchar(16),
	CREYMD	        char(8),
	UPYMD	        char(8),
	UPHMS	        char(6),
	constraint tbl_ryocom_primary_key primary key(
		HOSPID,
		NYUGAIKBN,
		PTID,
		SRYYM,
		ZAINUM
	)
);
