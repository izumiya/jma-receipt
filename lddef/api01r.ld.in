name	api01r;

arraysize	20;
textsize	400;

bindapi	"appointlst"	"OpenCOBOL"	"ORAPI014R1";
bindapi	"acceptlst"	"OpenCOBOL"	"ORAPI011R1";
bindapi	"patientget"	"OpenCOBOL"	"ORAPI012R1";
bindapi	"patientlst1"	"OpenCOBOL"	"ORAPI012R2";
bindapi	"patientlst2"	"OpenCOBOL"	"ORAPI012R3";
bindapi	"patientlst3"	"OpenCOBOL"	"ORAPI012R4";
bindapi	"system01lst"	"OpenCOBOL"	"ORAPI101R1";
bindapi	"medicalget"	"OpenCOBOL"	"ORAPI021R1";
bindapi	"diseaseget"	"OpenCOBOL"	"ORAPI022R1";


multiplex_group	api01r;


data	{
	spa	orcaspa;
	window	{
		appointlst;
		acceptlst;
		patientget;
		patientlst1;
		patientlst2;
		patientlst3;
		system01lst;
		medicalget;
		diseaseget;
	};
};
db "xml" {
 xml_appointlstreq;
 xml_appointlstres;
 xml_acceptlstreq;
 xml_acceptlstres;
 xml_patientinfores;
 xml_patientlst1req;
 xml_patientlst1res;
 xml_patientlst2req;
 xml_patientlst2res;
 xml_patientlst3req;
 xml_system01_managereq;
 xml_physicianres;
 xml_departmentres;
 xml_medicalgetreq;
 xml_medicalget01res;
 xml_medicalget02res;
 xml_medicalget03res;
 xml_disease_infores;
 xml_disease_inforeq;
};

#include "orcadb.inc"
#include "orcadbh.inc"
