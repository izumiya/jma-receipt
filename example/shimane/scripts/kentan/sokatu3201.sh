#!/bin/sh

. @jma-receipt-env@

RENNUM=0
-------------------------------------------#
#    �������ν�ʼ��ݡ˺���
#        $1-${11}
#              �����ģ�����������(CPORCSRTLNK.INC)
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${14} ���ɽ�μ���
#           0:����  1:���ݡ�2:����
#        ${15} ���ŵ��أɣ� 
#        ${16} ���顼�ե�����̾ 
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${16}
        if  [ -e ${16} ]; then
            rm  ${16}
        fi
        
        cd  $ORCABT

##      �������ν�ʼ��ݡ�
        RENNUM=$(expr $RENNUM + 1) 
        $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane SYUKEI001 -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${15}
        if  [ -e ${16} ]; then
             exit
        fi
        
	$DBSTUB  -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd orcabt ORCBJOB -parameter JBE${12}${13}


