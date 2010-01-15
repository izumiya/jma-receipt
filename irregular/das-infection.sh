#!/bin/bash

if test -z "$JMARECEIPT_ENV" ; then
    #JMARECEIPT_ENV="@jma-receipt-env@"
    JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

HOSPNUM=$1
HOSPID=$2

FILE_PREFIX="infection-"
# for head UPLOAD_COMMAND=$SCRIPTSDIR/monthly/das-upload.rb
UPLOAD_COMMAND=$PATCHSCRIPTSDIR/monthly/das-upload.rb
LOG=/var/log/jma-receipt/"$HOSPNUM"datainfection.log
LOGFILE=/var/log/jma-receipt/"$HOSPNUM"datainfection1.log

if [ -e $LOGFILE ]; then
  cat $LOGFILE >> $LOG
fi
rm -f $LOGFILE

export CLIENT_CONFIG_FILE="$SYSCONFDIR/das-upload.d/das-upload${HOSPNUM}.conf"
if [ ! -f $CLIENT_CONFIG_FILE ]; then
  echo "config file not found." | tee -a $LOGFILE
  exit
fi
export DAS_CHECK_HOSPID=$HOSPID

for file in `ls "$DASDIR"/"$HOSPNUM"/"$FILE_PREFIX"*tar.gz 2>/dev/null`
do
  UPTARGEFILE=1
  WRKYMD=`basename $file |sed -e "s/${FILE_PREFIX}\(.*\).tar.gz/\1/"`
  OBJECTDATE=${WRKYMD:4:2}.${WRKYMD:6:2}
  RESULT=`$UPLOAD_COMMAND -i $file 2>&1|head -1`
  if [ "UP $file" = "$RESULT" ]; then
     echo "000" $RESULT | tee -a $LOGFILE
     $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-infection","2","0",$OBJECTDATE,"000","Upload success",$HOSPNUM 2>&1 | tee -a $LOGFILE
     rm $file
  else
     $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-infection","2","1",$OBJECTDATE,"${RESULT:0:3}","${RESULT:4}",$HOSPNUM 2>&1 | tee -a $LOGFILE
     echo "${RESULT:0:3}","${RESULT:4}" | tee -a $LOGFILE
  fi
  sleep 1
done

if [ x$UPTARGEFILE != "x1" ]; then
  RESULT="900 No file"
  echo "${RESULT:0:3}","${RESULT:4}" | tee -a $LOGFILE
fi
