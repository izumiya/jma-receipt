#!/bin/bash
ORCADIR=/usr/lib/jma-receipt
ORCABT="$ORCADIR"
DBSTUB=/usr/lib/panda/sbin/dbstub
#-------------------------------------------#
#    �������׻������ʥ�ӥ塼�ѡ�
#-------------------------------------------#
##      ���顼�ե�������

#  �裱���ƥåס�����
        echo "update tbl_jobkanri set shellmsg = '����������н���', stepstarttime = '13013000', stependtime = '        ', stepcnt = 3 where shellid = 'TEIKI1';" | psql orca

        sleep 7
        
#  �裱���ƥåס���λ
        echo "update tbl_jobkanri set shellmsg = '����������н���', stepstarttime = '13013000', stependtime = '13021000', stepcnt = 3 where shellid = 'TEIKI1';" | psql orca

        sleep 1

#  �裲���ƥåס�����
        echo "update tbl_jobkanri set shellmsg = '�����оݴ�����н���', stepstarttime = '13023200', stependtime = '        ', stepcnt = 6 where shellid = 'TEIKI1';" | psql orca

        sleep 7
        
#  �裲���ƥåס���λ
        echo "update tbl_jobkanri set shellmsg = '�����оݴ�����н���', stepstarttime = '13023200', stependtime = '13031000', stepcnt = 6 where shellid = 'TEIKI1';" | psql orca

        sleep 1

#  �裳���ƥåס�����
        echo "update tbl_jobkanri set shellmsg = '���������۷׻�����', stepstarttime = '13031300', stependtime = '        ', stepcnt = 9 where shellid = 'TEIKI1';" | psql orca

        sleep 7
        
#  �裳���ƥåס���λ
        echo "update tbl_jobkanri set shellmsg = '���������۷׻�����', stepstarttime = '13031300', stependtime = '13041000', stepcnt = 9 where shellid = 'TEIKI1';" | psql orca

        sleep 1

#  �裴���ƥåס�����
        echo "update tbl_jobkanri set shellmsg = '��Ǽ�����������', stepstarttime = '13041300', stependtime = '        ', stepcnt = 12 where shellid = 'TEIKI1';" | psql orca

        sleep 7
        
#  �裴���ƥåס���λ
        echo "update tbl_jobkanri set shellmsg = '��Ǽ�����������', stepstarttime = '13041300', stependtime = '13051000', stepcnt = 12 where shellid = 'TEIKI1';" | psql orca

        sleep 1
	
	$DBSTUB -record "$ORCADIR"/record/ -dir "$ORCADIR"/lddef/directory -bddir "$ORCADIR"/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$1$2

       exit
