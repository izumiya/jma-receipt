#!/bin/bash
ORCADIR=/usr/local/orca
ORCABT="$ORCADIR"/lib
DBSTUB=/usr/local/panda/bin/dbstub
-------------------------------------------#
#    �������ν����
#        CPCOMMONSHELL1.INC  
#        $1 SRYYM TERID SYSYMD
#        $2 RECEKBN(���ɽ�μ���)
#           1:���ݡ�2:����
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

##      �������ν�ʼ��ݡ�
        if  [ $2 = '1' ]; then
       	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBM020 -parameter $1$5
       	fi    

##      �������ν�ʹ��ݡ�
        if  [ $2 = '2' ]; then
       	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBM021 -parameter $1$5
       	fi    
