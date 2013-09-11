name	orca22;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGC02";
bind	"C02"		"OpenCOBOL"	"ORCGC02";
bind	"C03"		"OpenCOBOL"	"ORCGC03";
bind	"C04"		"OpenCOBOL"	"ORCGC04";
bind	"C05"		"OpenCOBOL"	"ORCGC05";
bind	"C06"		"OpenCOBOL"	"ORCGC06";
bind	"C09"		"OpenCOBOL"	"ORCGC09";
bind	"C10"		"OpenCOBOL"	"ORCGC10";
bind	"C11"		"OpenCOBOL"	"ORCGC11";
bind	"C12"		"OpenCOBOL"	"ORCGC12";
bind	"C99"		"OpenCOBOL"	"ORCGC99";
bind	"CERR"		"OpenCOBOL"	"ORCGCERR";
bind	"CID1"		"OpenCOBOL"	"ORCGCID1";
bind	"CID2"		"OpenCOBOL"	"ORCGCID2";
bind	"CID3"		"OpenCOBOL"	"ORCGCID3";
bind	"CID4"		"OpenCOBOL"	"ORCGCID4";
bind	"CID5"		"OpenCOBOL"	"ORCGCID5";
bind	"CID6"		"OpenCOBOL"	"ORCGCID6";

multiplex_group	orca22;


data	{
	spa	orcaspa;
	window	{
		C02;
		C03;
		C04;
		C05;
		C06;
		C09;
		C10;
		C11;
		C12;
		C99;
		CERR;
		CID1;
		CID2;
		CID3;
		CID4;
		CID5;
		CID6;
	};
};

db	{
	tbl_byomei;
	tbl_byomeiinputset;
	tbl_example;
	tbl_hkncombi;
	tbl_hknnum;
	tbl_jyurrk;
	tbl_jobkanri;
	tbl_ldmap;
	tbl_lock;
	tbl_nyuinacct;
	tbl_opecom;
	tbl_ptbyomei;
	tbl_ptinf;
	tbl_ptnum;
	tbl_ptnyuinrrk;
	tbl_ptrsiinf;
	tbl_pthkninf;
	tbl_ptkohinf;
	tbl_rececom;
	tbl_receupd;
	tbl_sryact;
	tbl_srykarrk;
	tbl_syskanri;
	tbl_tensu;
	tbl_userbyomei;
	tbl_sryacct_main;
	tbl_sryacct_sub;
	tbl_synonym;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
