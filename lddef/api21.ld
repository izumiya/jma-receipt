name	api21;

arraysize	20;
textsize	400;

bindapi	"medicalmod"	"OpenCOBOL"	"ORAPI021S1";
bindapi	"medicalmodv2"	"OpenCOBOL"	"ORAPI021S1V2";
bindapi	"medicalmodv31"	"OpenCOBOL"	"ORAPI021S1V3";
bindapi	"medicalmodv32"	"OpenCOBOL"	"ORAPI021S2V3";
bindapi	"medicalmodv33"	"OpenCOBOL"	"ORAPI021S3V3";

multiplex_group	api21;


data	{
	spa	orcaspa;
	window	{
		medicalmod;
		medicalmodv2;
		medicalmodv31;
		medicalmodv32;
		medicalmodv33;
	};
};
db "xml" {
 xml_medicalreq;
 xml_medicalres;
};
db "xml2" {
 xml_medicalv2req;
 xml_medicalv2res;
 xml_medicalv3req1;
 xml_medicalv3res1;
 xml_medicalv3req2;
 xml_medicalv3res2;
 xml_medicalv3req3;
 xml_medicalv3res3;
};

#include "orcadb.inc"
#include "orcadbh.inc"
