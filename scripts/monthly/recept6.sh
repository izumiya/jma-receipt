#!/bin/bash
ORCADIR=/usr/local/orca
ORCABT="$ORCADIR"/lib
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------#
#    ϫ��������ѥե��������
#        CPCOMMONSHELL1.INC  
#        $1 SRYYM TERID SYSYMD
#        $2 SYOKBN
#           1:������  2:���̺���
#        $3 RECEKBN
#           3:����  
#        $4 ���顼�ե�����̾ 
#        $5 JOBID 
#        $6 SHELLID
#        $7 ��ñ�ѹ����Խ��ץ����̾�ʰ����ѡ�
#        $8 ����ǯ��
#-------------------------------------------#
##      ���顼�ե�������
	echo $#
	echo "echo " $4
        if  [ -e $4 ]; then
            rm  $4
        fi
        
        rm  /var/tmp/RECE4*
        
     cd  $ORCABT

	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0400 -parameter $1$2$3$5$6
	
	   
        if  [ -e $4 ]; then
            exit  
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0410 -parameter $1$5$6
        fi 

        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0420 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0430 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0435 -parameter $1$5$6
        fi

 
        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0440 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
           exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0450 -parameter $1$5$6
        fi
 
        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0460 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0465 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0466 -parameter $1$5$6
        fi

 
        if  [ -e $4 ]; then
            exit 
        else 
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0470 -parameter $1$5$6
        fi

        if  [ -e $4 ]; then
            exit 
        else 
           $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0480 -parameter $1$3$8$5$6
        fi
        
        if  [ -e $4 ]; then
            exit 
        else 
       	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0481 -parameter $1$3$8$5$6
        fi
            

        if  [ -e $4 ]; then
            exit 
        else 
       	    $DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCR0482 -parameter $1$3$8$5$6
        fi
     
        if  [ -e $4 ]; then
            exit 
        else
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$5$6
	fi
	
        exit 
