name	orca901;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGM97";
bind	"M80"		"OpenCOBOL"	"ORCGM80";
bind	"M96"		"OpenCOBOL"	"ORCGM96";
bind	"M97"		"OpenCOBOL"	"ORCGM97";
bind	"MMERR"		"OpenCOBOL"	"ORCGMMERR";
bind	"MMID1"		"OpenCOBOL"	"ORCGMMID1";

multiplex_group	orca901;


data	{
	spa	orcaspa;
	window	{
                M80;
		M96;
                M97;
                MMERR;
                MMID1;
	};
};

db	{
	tbl_jobkanri;
	tbl_sysbase;
	tbl_sysuser;
	tbl_dbkanri;
	tbl_mstkanri;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"
