#!/bin/bash
ORCABT=/usr/local/orca/lib
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------#
#    ϫ�Ұ����������Ͻ���
#        CPCOMMONSHELL1.INC($3-$6)  
#        $1 JOBID 
#        $2 SHELLID
#        $3 SRYYM TERID SYSYMD
#        $4 �¹ԥץ����
#           0:ORCR0101
#           1:ORCR0102 
#        $5 RECEKBN
#           3:����  4:û����5:����ǯ��
#                   6:���ե�������
#        $6 ���顼�ե�����̾ 
#        $7 �����Ѿ��ѥ�᥿ 
#        $8 �������� 
#        $8 ü���ɣ�X(32)  �ʣϣңãң��������ΤȤ���
#        $9 ������λ
#        $10 Ģɼ�����ֹ�
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $6
        if  [ -e $6 ]; then
            rm  $6
        fi
        
###     cd  $ORCABT
        

        if [ $4 =  0 ]; then
	   $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0490 -parameter $1,$2,$3,$7,$8,$9,${10},
        else 
	   $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0500 -parameter $1,$2,$3,$7,$8
           if  [ -e $6 ]; then
               exit 
           else
	       $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$1$2
           fi
        fi

        exit