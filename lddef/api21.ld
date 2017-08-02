name	api21;

arraysize	20;
textsize	400;

bindapi	"medicalmod"	"OpenCOBOL"	"ORAPI021S1";
bindapi	"medicalmodv2"	"OpenCOBOL"	"ORAPI021S1V2";
bindapi	"medicalmodv31"	"OpenCOBOL"	"ORAPI021S1V3";
bindapi	"medicalmodv32"	"OpenCOBOL"	"ORAPI021S2V3";
bindapi	"medicalmodv33"	"OpenCOBOL"	"ORAPI021S3V3";
bindapi	"medicalmodv34"	"OpenCOBOL"	"ORAPI021S4V3";
bindapi	"claimreceivev2"	"OpenCOBOL"	"ORAPICLAIM21S1V2";
bindapi	"medicalmodv35"	"OpenCOBOL"	"ORAPI021S5V3";
bindapi	"medicalmodv36"	"OpenCOBOL"	"ORAPI021S6V3";

multiplex_group	api21;


data	{
	spa	orcaspa;
	window	{
		medicalmod;
		medicalmodv2;
		medicalmodv31;
		medicalmodv32;
		medicalmodv33;
		medicalmodv34;
		claimreceivev2;
		medicalmodv35;
		medicalmodv36;
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
 xml_medicalv3req4;
 xml_medicalv3res4;
 xml_claim_receivev2req;
 xml_claim_receivev2res;
 xml_medicalv3req5;
 xml_medicalv3res5;
 xml_medicalv3req6;
 xml_medicalv3res6;
 fm_medicine_notebook;
 fm_prescription;
 fm_medicine_information;
 fm_appointment_form;
 fm_statement;
 fm_invoice_receipt;
};

#include "orcadb.inc"
#include "orcadbh.inc"

db "push" {
  push_print001;
};
