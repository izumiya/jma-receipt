name	orca44;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGT01";
bind	"T01"		"OpenCOBOL"	"ORCGT01";
bind	"T02"		"OpenCOBOL"	"ORCGT02";
bind	"T03"		"OpenCOBOL"	"ORCGT03";
bind	"T04"		"OpenCOBOL"	"ORCGT04";
bind	"T05"		"OpenCOBOL"	"ORCGT05";
bind	"T06"		"OpenCOBOL"	"ORCGT06";
bind	"T07"		"OpenCOBOL"	"ORCGT07";
bind	"T08"		"OpenCOBOL"	"ORCGT08";
bind	"T09"		"OpenCOBOL"	"ORCGT09";
bind	"T91"		"OpenCOBOL"	"ORCGT91";
bind	"T98"		"OpenCOBOL"	"ORCGT98";
bind	"T99"		"OpenCOBOL"	"ORCGT99";
bind	"TERR"		"OpenCOBOL"	"ORCGTERR";
bind	"TID1"		"OpenCOBOL"	"ORCGTID1";
bind	"TID2"		"OpenCOBOL"	"ORCGTID2";
bind	"TID3"		"OpenCOBOL"	"ORCGTID3";

multiplex_group	orca44;


data	{
	spa	orcaspa;
	window	{
		T01;
		T02;
		T03;
		T04;
		T05;
		T06;
		T07;
		T08;
		T09;
		T91;
		T98;
		T99;
		TERR;
		TID1;
		TID2;
		TID3;
	};
};

db	{
	tbl_btpara;
	tbl_csv_info;
	tbl_file_info;
	tbl_henrei_kanri;
	tbl_henrei_body;
	tbl_hknjainf;
	tbl_jobkanri;
        tbl_monthlynum;
	tbl_onrece_kanri;
	tbl_onrece_body;
	tbl_prtdata;
	tbl_prtkanri;
	tbl_prvkanri;
	tbl_ptinf;
        tbl_ptkohinf;
	tbl_ptnum;
	tbl_receden;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_syskanri;
	tbl_syuka;
	tbl_renkei_tmp;
	tbl_efout;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
