name    session;

bind	""		"OpenCOBOL"	"ORCGSESEND";
bind	"session_end"	"OpenCOBOL"	"ORCGSESEND";
bindapi	"session_start"	"OpenCOBOL"	"ORCGSESSTART";

data {
 spa orcaspa;
	window {
		session_end;
		session_start;
	};
};

db {
 tbl_lock;
};

#include "orcadbh.inc"
