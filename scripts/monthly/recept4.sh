#!/bin/bash
ORCADIR=/usr/local/orca
ORCABT="$ORCADIR"/lib
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------#
#    �쥻�ťե������������
#        CPCOMMONSHELL1.INC  
#        $1 SRYYM TERID SYSYMD
#        $2 ����ǯ��
#        $3 �����
#        $4 �ե��������
#        $5 �쥻�ťե����������
#        $6 JOBID 
#        $7 SHELLID
#        $8 ���顼�ե�����̾ 
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $8
        if  [ -e $8 ]; then
            rm  $8
        fi
        
     cd  $ORCABT

        $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0200 -parameter $1$6$7
        
        if  [ $4 = '1' ]; then          
#           ����Ĺ�ΤȤ�
	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0210 -parameter $1,$2,$3,$6,$7
        else 
#           �ãӣ֤ΤȤ� 
	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0300 -parameter $1,$2,$3,$6,$7
        fi 
        
        if  [ -e $8 ]; then
            exit 
        fi
        
	$DBSTUB  -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$6$7

        

#       �����������Ѵ��ʣţգä��饷�եȣʣɣӡ�	
        if  [ $4 = '1' ]; then          
#           ����Ĺ�ΤȤ�
	    nkf -s /var/tmp/RECE200.dat > $5RECEIPTJ.UKE
	else
#           �ãӣ֤ΤȤ� 
	    nkf -s /var/tmp/RECE300.dat > $5RECEIPTC.UKE
	fi
	    
        exit 
