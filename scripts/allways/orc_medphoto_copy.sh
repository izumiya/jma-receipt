#!/bin/sh

if test -z "$JMARECEIPT_ENV" ; then
    JMARECEIPT_ENV="./etc/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

MEDPHOTOTEMP="/tmp/medphoto"

#-------------------------------------------#
#    ���޾���ޥ��������ʼ̿�ʣ�̽���
#        
#	�������
#        $1  SRYCD �����ʥ����ɡʣ����
#        $2  ʣ�̸� �̿��ե�����̾
#        $3  ʣ����ե����
#        $4  JOBID 
#        $5  SHELLID
#
#       ʣ�̸��ե�����ξ��ϴĶ��ѿ����
#        $MEDPHOTOTEMP
#-------------------------------------------#
##      ʣ�̸��ե������¸�ߥ����å�
        if  [ -e $MEDPHOTOTEMP/$2 ]; then
	    echo $3/$1-$2
            cp $MEDPHOTOTEMP/$2 $3/$1-$2
        fi

	$DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE$4$5
	
	exit 
