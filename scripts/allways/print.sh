#!/bin/bash
ORCABT=/usr/local/orca/lib
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------#
#    �����ģ¤���ΰ�������
#        $1-${11} �����ģ��Ѹ������
#        ${12} ���顼�ե�����̾ 
#        ${13} ����֣ɣ�
#        ${14} ������ɣ�
#        ${15} ����������
#        ${16} ������λ��
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${12}
        if  [ -e ${12} ]; then
            rm  ${12}
        fi
        
        cd  $ORCABT

        $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBPRT -parameter $1,$2,$3,$4,$5,$6,$7,$8,$9,${10},${11},${13},${14},${15},${16}
        if  [ -e ${12} ]; then
            exit
        else
	    $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE${13}${14}
	fi

        exit 
