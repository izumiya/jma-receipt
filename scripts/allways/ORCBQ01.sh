#!/bin/bash
#-------------------------------------------#
#    ���԰�������
#        $1 ������ʬ
#        $2 CSV�ե�����̾
#        $3 �ѥ�᡼���ե�����̾
#        $4 TERMID
#        $5 SYSYMD
#        $6 SYSDATE
#        $7 ����֣ɣ�
#        $8 ������ɣ�
#
#-------------------------------------------#
DBSTUB=/usr/local/panda/bin/dbstub
#-------------------------------------------# 
        $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca -bd orcabt ORCBQ01 -parameter $3,$4,$5,$6

##      �ãӣ֥ե��������
        if  [ $1 -eq "0" ]; then
            if  [ -e $2 ]; then
                awk ' {
                #     ��Ƭ�ζ���������
                      sub(/^ */,"");
                #     �����ζ���������
                      sub(/ *$/,"");
                #     ����ޤθ�ζ���������
                      gsub(/, */,",");
                #     ����ޤ����ζ���������
                      gsub(/ *,/,",");
                      print $0
                }'  $2 > $2.tmp

                mv  $2.tmp $2
            fi
        fi

##      �ѥ�᡼���ե�������
        if  [ -e $3 ]; then
            rm  $3
        fi

       $DBSTUB -record /usr/local/orca/record/ -dir /usr/local/orca/lddef/directory -bddir /usr/local/orca/lddef -db orca  -bd orcabt ORCBJOB -parameter JBE$7$8

        exit
