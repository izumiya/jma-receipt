#!/bin/bash
ORCADIR=/usr/local/orca
ORCABT="$ORCADIR"/lib
DBSTUB=/usr/local/panda/bin/dbstub
-------------------------------------------#
#    ̤�����������
#        CPCOMMONSHELL1.INC  
#        $1 SRYYM TERID SYSYMD
#        $2 RECEKBN(���ɽ�μ���)
#           0:����  1:���ݡ�2:����
#        $3 ���顼�ե�����̾ 
#        $4 ���ŵ��أɣ� 
#        $5 ü���ɣġ��ءʣ����� 
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $3
        if  [ -e $3 ]; then
            rm  $3
        fi
        
        cd  $ORCABT

##      ̤�������
     	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBM005 -parameter $1,$4,$2,$5
