name	orca36;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGI20";
bind	"I20"		"OpenCOBOL"	"ORCGI20";
bind	"I21"		"OpenCOBOL"	"ORCGI21";
bind	"I22"		"OpenCOBOL"	"ORCGI22";
bind	"I23"		"OpenCOBOL"	"ORCGI23";
bind	"I2ERR"		"OpenCOBOL"	"ORCGI2ERR";
bind	"I2ID1"		"OpenCOBOL"	"ORCGI2ID1";

multiplex_group	orca36;


data	{
	spa	orcaspa;
	window	{
		I20;
		I21;
		I22;
		I23;
		I2ERR;
		I2ID1;
	};
};

db	{
	tbl_syskanri;
	tbl_ptnum;
	tbl_ptinf;
	tbl_jobkanri;
	view_I001;
	tbl_file_info;
	tbl_hkncombi;
	tbl_pthkninf;
	tbl_ptrsiinf;
	tbl_ptkohinf;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_ptnyuinrrk;
	tbl_ptsrh;
	tbl_tnkrrk;
	tbl_tsyrrk;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
