#!/bin/bash

if test -z "$JMARECEIPT_ENV" ; then
    JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

HOSPNUM=$1
HOSPID=$2

export CLIENT_CONFIG_FILE="$SYSCONFDIR/das-upload.d/das-upload${HOSPNUM}.conf"
if [ ! -f $CLIENT_CONFIG_FILE ]; then
    exit
fi

export DAS_CHECK_HOSPID=$HOSPID

FILE_PROFILE=$DASDIR/$HOSPNUM/profile.tar.gz
export CSV_PROFILE=/tmp/profile.csv
# for head PROFILE_COMMAND=`dirname $0`/das-profile.rb
PROFILE_COMMAND=$PATCHSCRIPTSDIR/allways/das-profile.rb
# for head UPLOAD_COMMAND=$SCRIPTSDIR/monthly/das-upload.rb
UPLOAD_COMMAND=$PATCHSCRIPTSDIR/monthly/das-upload.rb
LOGFILE=/var/log/jma-receipt/"$HOSPNUM"das-profile.log

DATE=$(date +'%Y%m%d')

confupdate (){
  TEMP=$(mktemp)
  cat $CLIENT_CONFIG_FILE | \
  sed -e '/"SendResult"/c\"SendResult" => "@RESULT@",' | \
  sed -e '/"SendDate"/c\"SendDate" => "@DATE@"' | \
  sed -e "s/@RESULT@/$1/" | \
  sed -e "s/@DATE@/$DATE/" > $TEMP
  mv $TEMP $CLIENT_CONFIG_FILE
}

if ! [ -d $DASDIR/${HOSPNUM} ]; then 
  mkdir $DASDIR/${HOSPNUM}
fi

# delete login check log
$DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-profile","D",,,,,$HOSPNUM 2>&1 | tee $LOGFILE

RESULT=`$PROFILE_COMMAND 2>&1 | tee -a $LOGFILE`
if [ -e $CSV_PROFILE ]; then
  tar cvzf $FILE_PROFILE -C/tmp profile.csv
  rm -f $CSV_PROFILE
else
  echo "$CSV_PROFILE not found" | tee -a $LOGFILE
fi

if [ -e $FILE_PROFILE ]; then
  UPTARGEFILE=1
  RESULT=`$UPLOAD_COMMAND -p $FILE_PROFILE 2>&1|head -1`
  echo ${RESULT} | tee -a $LOGFILE
  if [ "UP $FILE_PROFILE" = "$RESULT" ]; then
    echo "000" $RESULT
    confupdate 000
    $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-profile","2","0","","000","Upload success",$HOSPNUM 2>&1 | tee -a $LOGFILE
  else
    confupdate ${RESULT:0:3}
    $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOBLOG -parameter "das-profile","2","1","","${RESULT:0:3}","${RESULT:4}",$HOSPNUM 2>&1 | tee -a $LOGFILE
    echo "${RESULT:0:3}","${RESULT:4}"
  fi
  rm $FILE_PROFILE
fi

if [ x$UPTARGEFILE != "x1" ]; then
  RESULT="900 No file"
  confupdate ${RESULT:0:3}
  echo "${RESULT:0:3}","${RESULT:4}" | tee -a $LOGFILE
fi
