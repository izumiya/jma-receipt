#! /bin/sh
#
# Project code name "ORCA"
# ����ɸ��쥻�ץȥ��ե�(JMA standard receipt software)
#
# Copyright(C) 2002 JMA(Japan Medical Association)
#
# CREATE: 20020227
#
#
# ���ŵ��أɣĥꥻ�å�
#
#
echo
echo "���ŵ��أɣĤ򿶤�ľ���ޤ�"
echo
#
if [ $# -ne 0 ] ;       then
        HOSPID="$1"
else
        HOSPID="JPN000000000000"
fi
#
while [ "$OKFLG" != "Y" ] && [ "$OKFLG" != "y" ]
do
        echo
        echo -n "����Ǥ褱��Ф��Τޤ� Enter �����򲡤��Ƥ������� " $HOSPID ": "
        read LASTHOSPID
        if [ -z $LASTHOSPID ] ;then
                LASTHOSPID="$HOSPID"
        else
                HOSPID="$LASTHOSPID"
        fi
        echo
        echo    "          ----*----*----* (15��)"
        echo -n "HOSPID = " $LASTHOSPID " [ y/N ]: "
        read OKFLG
done
#
echo
echo "�ϣңã��ģ¥ơ��֥�ΰ��ŵ��أɣĤ򥻥åȤ��ޤ�"
for f in `echo "SELECT pg_class.relname FROM pg_class, pg_attribute WHERE  pg_class.relkind = 'r'  and pg_attribute.attnum > 0 and pg_attribute.attrelid = pg_class.oid and pg_attribute.attname = 'hospid';" | psql  orca | grep 'tbl'` ; do
	echo "$f ------------"
	echo "update $f set hospid = '$LASTHOSPID';" | psql orca
	echo "---------------------"
done
#
echo
echo "�����ƥ�����ơ��֥�ΰ��ŵ��أɣĤ򥻥åȤ��ޤ�"
echo "update tbl_syskanri set kanritbl = substr(kanritbl,1,11) || '$LASTHOSPID' || '         ' || substr(kanritbl,36,465) where kanricd = '1001';" | psql orca
#
echo
echo "vacuum ������Ԥ��ޤ�"
echo "vacuum analyze;" | psql orca
#
# ������λ
#
echo
echo "�����Ϥ��٤ƽ�λ���ޤ���"
exit 0
