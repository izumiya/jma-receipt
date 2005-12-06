CREATE TABLE "tbl_nyuksnchk" (
	"nyuinkbn" character(3) NOT NULL,
	"ksnkbn" character(3) NOT NULL,
	"yukostymd" character(8) NOT NULL,
	"yukoedymd" character(8) NOT NULL,
	"chkkbn" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_nyuksnchk_primary_key" Primary Key ("nyuinkbn", "ksnkbn", "yukostymd", "yukoedymd")
);

COMMENT ON TABLE "tbl_nyuksnchk" IS '�����û������å�';

