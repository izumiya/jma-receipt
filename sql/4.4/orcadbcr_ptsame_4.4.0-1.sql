\set ON_ERROR_STOP
--
-- Ʊ�촵�Լ��̥ơ��֥�
--
-- Create Date : 2009/01/23         --
--
create	table	tbl_ptsame	(
	HOSPNUM	numeric(2,0) NOT NULL,
	PTID		numeric(10,0)  NOT NULL,
	SAME_HOSPNUM	numeric(2,0)  NOT NULL,
	SAME_PTID	numeric(10,0)  NOT NULL,
	TERMID	varchar(16),
	OPID		varchar(16),
	CREYMD	char(8),
	UPYMD		char(8),
	UPHMS		char(6),
	Constraint "tbl_ptsame_key" primary	key(
		HOSPNUM,
		PTID
	)
);
