#!/bin/sh

. @jma-receipt-env@

RENNUM=0
-------------------------------------------#
#    �������ν�ʹ��ݡ˺���
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
        

##      �������ν�ʹ��ݡ�
        RENNUM=$(expr $RENNUM + 1) 
        $DBSTUB -dir $LDDEFDIR/directory -bd shimane SYUKEI002 -parameter  $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${15}
        if  [ -e ${16} ]; then
             exit
        fi
        
	$DBSTUB -dir $LDDEFDIR/directory -bd orcabt ORCBJOB -parameter JBE${12}${13}
