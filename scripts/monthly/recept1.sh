#!/bin/bash
ORCABT=/usr/local/orca/lib
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------#
#    ����������ѥե��������
#        CPCOMMONSHELL1.INC  
#        $1 SRYYM TERID SYSYMD
#        $2 SYOKBN
#           1:������  2:���̺���
#        $3 RECEKBN
#           0:����  1:���ݡ�2:����
#        $4 ���顼�ե�����̾ 
#        $5 JOBID 
#        $6 SHELLID
#        $7 ��ñ�ѹ����Խ��ץ����̾
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $4
        if  [ -e $4 ]; then
            rm  $4
        fi
        
        rm  /var/tmp/RECE*
        
     cd  $ORCABT

#        fcu  -P$1$2$3 ORCR0010
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0010 -parameter $1$2$3$5$6
	
        if  [ -e $4 ]; then
            exit  
        else 
#           fcu  -P$1 ORCR0020
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0020 -parameter $1$5$6
        fi 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0025
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0025 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0030
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0030 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0035
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0035 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0040
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0040 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0050
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0050 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
           exit 
        else 
#            fcu  -P$1 ORCR0055
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0055 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0090
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0090 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1 ORCR0091
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0091 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
           if  [ $7 != "NOPG" ]; then
	      $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd recept $7 -parameter $1$5$6
	   fi
        fi

 
        if  [ -e $4 ]; then
            exit 
        else 
#            fcu  -P$1$3 ORCR0100
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCR0100 -parameter $1$3$5$6
        fi
        
        if  [ -e $4 ]; then
            exit 
        else
	$DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$5$6
	fi
	
        exit 
