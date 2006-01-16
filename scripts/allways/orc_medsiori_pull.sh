#!/bin/sh

if test -z "$JMARECEIPT_ENV" ; then
    JMARECEIPT_ENV="./etc/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

# DBSTUB="/usr/lib/panda/sbin/dbstub -nocheck "
# LDDIRECTORY=./directory
MEDSIORISITE="192.168.1.104"
MEDPHOTOSITE="ftp://orca:orca@192.168.1.104/public_html/jma-siori-photo"
MEDPHOTOTEMP="/tmp/medphoto"

# SRYCD="610443074"
# TERMID="toshichan"

#-------------------------------------------#
#    ���޾���ޥ��������ʤ������������ɽ���
#
#	�������
#        $1  SRYCD �����ʥ����ɡʣ����
#        $2  TERMID
#        $3  JOBID 
#        $4  SHELLID
#
#	�Ķ��ѿ����
#        $5  MEDSIORISITE
#        $6  MEDSIORISITE
#        $7  MEDPHOTOTEMP
#-------------------------------------------#

# �����ե������Ǽ�ǥ��쥯�ȥ�κ���
	if ! [ -d   $MEDPHOTOTEMP ];	then
		mkdir $MEDPHOTOTEMP
	fi

# ���󥿡������Ф������������
	MEDSIORIPARAM="$1,$2,$MEDSIORISITE,$MEDPHOTOSITE,$MEDPHOTOTEMP"
	export MEDSIORIPARAM

	$DBSTUB -dir $LDDIRECTORY -bd orcabt OrcSioriPull -parameter ""
	
	$DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE$3$4
	exit 
