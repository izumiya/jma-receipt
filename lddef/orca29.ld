name	orca29;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGKA01";
bind	"KA01"		"OpenCOBOL"	"ORCGKA01";
bind	"KA02"		"OpenCOBOL"	"ORCGKA02";
bind	"KA021"		"OpenCOBOL"	"ORCGKA021";
bind	"KAERR"		"OpenCOBOL"	"ORCGKAERR";
bind	"KAID1"		"OpenCOBOL"	"ORCGKAID1";
bind	"KAID2"		"OpenCOBOL"	"ORCGKAID2";
bind	"_KADMY01"	"OpenCOBOL"	"ORCGKADMY01";

multiplex_group	orca29;


data	{
	spa	orcaspa;
	window	{
		KA01;
		KA02;
		KA021;
		KAERR;
		KAID1;
		KAID2;
                _KADMY01;
	};
};

db	{
	tbl_syskanri;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_ptcom;
	tbl_ptnum;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_ptinf;
	tbl_sryact;
	tbl_tensu;
        tbl_tensuplus;
	tbl_wksryact;
	tbl_ptrsiinf;
	tbl_yakujyo;
	tbl_monthlynum;
	tbl_jobkanri;
	tbl_byomei;
	tbl_ptbyomei;
	tbl_tsyrrk;
        tbl_tensuplus2;
        tbl_genericname;
	tbl_prgoption;
        tbl_ptnyuinrrk;
        tbl_ptkohftn;
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_dbcatalog;
	tbl_prtctrl;
	tbl_sryacctplus;
        tbl_mnote_h;
        tbl_mnote_b;
        tbl_ptconf;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
