#!/bin/bash
ORCABT=/usr/local/orca/lib
DBSTUB=/usr/local/panda/bin/dbstub
RENNUM=0
#-------------------------------------------#
#    �����ޥ�����������
#        $1-${11} �����ģ��Ѹ������
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${14} �ץ����̾
#        ${15} ��о��(CPHCMLSHELL.INC)
#        ${16} ���顼�ե�����̾ 
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${16}
        if  [ -e ${16} ]; then
            rm  ${16}
        fi
        
        RENNUM=$(expr $RENNUM + 1) 
        $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ${14} -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${15}
        if  [ -e ${16} ]; then
            exit
        fi
#
        $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE${12}${13}

        exit 
