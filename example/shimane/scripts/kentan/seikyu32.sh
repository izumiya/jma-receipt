#!/bin/sh

. @jma-receipt-env@

-------------------------------------------#
#    ����������������
#        $1 SRYYM TERID SYSYMD
#        $2 ���顼�ե�����̾ 
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $2
        if  [ -e $2 ]; then
            rm  $2
        fi
        
##      ���Ļ����������
        $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane ORCBS321 -parameter $1
        if  [ -e $2 ]; then
            exit
        fi
  
##      ʡ����������
        $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane ORCBS323 -parameter $1
        if  [ -e $2 ]; then
            exit
        fi  
 
##      ʡ��Ϸ�Ͱ��������
        $DBSTUB -host $DBHOST -record $SITERECORDDIR -dir $LDDEFDIR/directory -bddir $SITELDDEFDIR -db orca  -bd shimane ORCBS325 -parameter $1
        if  [ -e $2 ]; then
            exit
        fi  

        exit 
