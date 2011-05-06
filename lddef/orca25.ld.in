name	orca25;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGC50";
bind	"C50"		"OpenCOBOL"	"ORCGC50";
bind	"C501"		"OpenCOBOL"	"ORCGC501";
bind	"C50ERR"	"OpenCOBOL"	"ORCGC50ERR";
bind	"C50ID1"	"OpenCOBOL"	"ORCGC50ID1";

multiplex_group	orca25;



data	{
	spa	orcaspa;
	window	{
		C50;
		C501;
                C50ERR;
                C50ID1;
	};
};

db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_jyurrk;
	tbl_ptrsiinf;
	tbl_rececom;
	tbl_example;
	tbl_receupd;
	tbl_ptnyuinrrk;
	tbl_nyuinacct;
	tbl_ptinf;
	tbl_sryacct_main;
	tbl_sryacct_sub;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
