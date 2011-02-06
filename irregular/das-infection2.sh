#!/bin/bash

if test -z "$JMARECEIPT_ENV" ; then
#    JMARECEIPT_ENV="@jma-receipt-env@"
    JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

HOSPNUM=${1}
HOSPID=${2}
KIDONO=${3}

FILE_PREFIX="infection"
#UPLOAD_COMMAND=$SCRIPTSDIR/monthly/das-upload2.rb
UPLOAD_COMMAND=$PATCHSCRIPTSDIR/daily/das-upload2.rb
LOG=/var/log/jma-receipt/"$HOSPNUM"datainfection2.log
LOGFILE=/var/log/jma-receipt/"$HOSPNUM"datainfection2_real.log

#if [ -e $LOGFILE ]; then
#  cat $LOGFILE >> $LOG
#fi
rm -f $LOGFILE

for file in `ls "$DASDIR"/"$HOSPNUM"/"$FILE_PREFIX"*.gz 2>/dev/null`
do
  UPTARGEFILE=1

  TIMESTAMP=`basename $file |sed -e "s/${FILE_PREFIX}.csv.\(.*\).gz/\1/"`
  OBJECTDATE=${TIMESTAMP:8:6}

  RESULT=`$UPLOAD_COMMAND -i $file 2>&1|head -1`
  if [ "UP $file" = "$RESULT" ]; then
     echo "000" $RESULT | tee -a $LOGFILE
     if [ ${KIDONO} -eq 1 ]; then 
       $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-infec2","2","0",$OBJECTDATE,"000","Upload success",$HOSPNUM 2>&1 | tee -a $LOGFILE
     fi
     rm $file
  else
     $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-infec2","2","1",$OBJECTDATE,"${RESULT:0:3}","${RESULT:4}",$HOSPNUM 2>&1 | tee -a $LOGFILE
     echo "${RESULT:0:3}","${RESULT:4}" | tee -a $LOGFILE
  fi
  sleep 1
done

if [ x$UPTARGEFILE != "x1" ]; then
  RESULT="900 No file"
  echo "${RESULT:0:3}","${RESULT:4}" | tee -a $LOGFILE
fi
