name	api01rv2;

arraysize	20;
textsize	400;

bindapi	"patientgetv2"	"OpenCOBOL"	"ORAPI012R1V2";
bindapi	"acceptlstv2"	"OpenCOBOL"	"ORAPI011R1V2";
bindapi	"appointlstv2"	"OpenCOBOL"	"ORAPI014R1V2";
bindapi	"patientlst1v2"	"OpenCOBOL"	"ORAPI012R2V2";
bindapi	"patientlst2v2"	"OpenCOBOL"	"ORAPI012R3V2";
bindapi	"patientlst3v2"	"OpenCOBOL"	"ORAPI012R4V2";
bindapi	"system01lstv2"	"OpenCOBOL"	"ORAPI101R1V2";
bindapi	"medicalgetv2"	"OpenCOBOL"	"ORAPI021R1V2";
bindapi	"diseasegetv2"	"OpenCOBOL"	"ORAPI022R1V2";
bindapi	"appointlst2v2"	"OpenCOBOL"	"ORAPI014R2V2";
bindapi	"acsimulatev2"	"OpenCOBOL"	"ORAPI023R1V2";


multiplex_group	api01rv2;


data	{
	spa	orcaspa;
	window	{
		patientgetv2;
		acceptlstv2;
		appointlstv2;
		patientlst1v2;
		patientlst2v2;
		patientlst3v2;
		system01lstv2;
		medicalgetv2;
		diseasegetv2;
		appointlst2v2;
		acsimulatev2;
	};
};
db "xml2" {
 xml_patientinfov2res;
 xml_acceptlstv2req;
 xml_acceptlstv2res;
 xml_appointlstv2req;
 xml_appointlstv2res;
 xml_patientlst1v2req;
 xml_patientlst1v2res;
 xml_patientlst2v2req;
 xml_patientlst2v2res;
 xml_patientlst3v2req;
 xml_system01_managev2req;
 xml_physicianv2res;
 xml_departmentv2res;
 xml_medicalgetv2req;
 xml_medicalget01v2res;
 xml_medicalget02v2res;
 xml_medicalget03v2res;
 xml_disease_infov2res;
 xml_disease_infov2req;
 xml_appointlst2v2req;
 xml_appointlst2v2res;
 xml_acsimulatev2req;
 xml_acsimulatev2res;
};

#include "orcadb.inc"
#include "orcadbh.inc"
