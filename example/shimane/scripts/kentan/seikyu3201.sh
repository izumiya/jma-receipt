#!/bin/sh

. @jma-receipt-env@

RENNUM=0
-------------------------------------------#
#    ���ɽ�ѥե��������
#        $1-${11}
#              �����ģ�����������(CPORCSRTLNK.INC)
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${14} ���ŵ��أɣ� 
#        ${15} ���顼�ե�����̾ 
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${15}
        if  [ -e ${15} ]; then
            rm  ${15}
        fi
        
        cd  $ORCABT


##          ���Ļ����������
            RENNUM=$(expr $RENNUM + 1) 
       	    $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDERDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane SEIKYU001 -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${14}
            if  [ -e ${15} ]; then
                exit
            fi

            
  
##          ʡ����������
            RENNUM=$(expr $RENNUM + 1) 
       	    $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $LDDERDIR -db orca  -bd shimane SEIKYU003 -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${14}
            if  [ -e ${15} ]; then
                exit
            fi
 
##          ʡ��Ϸ�Ͱ��������
            RENNUM=$(expr $RENNUM + 1) 
       	    $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDERDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane SEIKYU005 -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${14}
            if  [ -e ${15} ]; then
                exit
            fi
            
	    $DBSTUB  -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd orcabt ORCBJOB -parameter JBE${12}${13}

        exit 
