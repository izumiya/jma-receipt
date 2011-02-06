#!/bin/bash

if test -z "$JMARECEIPT_ENV" ; then
#    JMARECEIPT_ENV="@jma-receipt-env@"
    JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
fi
if ! test -f "$JMARECEIPT_ENV"; then
    exit 0
fi

. $JMARECEIPT_ENV

#-------------------------------------------#
#    感染症データ作成
#        $1  TERMID
#        $2  HOSPNUM
#        $3  HOSPID
#        $4  JOBID
#        $5  SHELLID
#        $6  BEDFLG (0:病床なし、1:病床あり)
#-------------------------------------------#
TERMID=${1}
HOSPNUM=${2}
HOSPID=${3}
JOBID=${4}
SHELLID=${5}
BEDFLG=${6}

INFECFILE=$DASDIR/${HOSPNUM}/infection.csv
ERRFILE=/tmp/${HOSPNUM}infectionerror
INFECREC=$DASDIR/${HOSPNUM}/infection/infection${HOSPNUM}
LOG=/var/log/jma-receipt/${HOSPNUM}infectiondata.log
LOGFILE=/var/log/jma-receipt/${HOSPNUM}infectiondata1.log

# for head DASUPLOAD=$SCRIPTSDIR/daily/das-infection.sh
DASUPLOAD=$PATCHSCRIPTSDIR/daily/das-infection2.sh

DPKGLT=`dpkg -l | grep 'ii  jma-receipt' | grep 'JMA Standard Receipt'`
PKGVER=`echo $DPKGLT | sed 's/\s\{1,\}/ /g' | cut -d' ' -f 3`

syoribi () {
  SYORIBI=""

  ENDYMD=`date --date '1 day ago' +%Y%m%d`

  if [ -e $INFECREC ]; then
    STARTYMD=`head -1 $INFECREC`
    if [ "$STARTYMD" = "" ]; then
      echo "$INFECREC is empty" | tee -a $LOGFILE
      SYORIBI=$ENDYMD
      return
    fi
    if [ `expr length $STARTYMD` -lt 8 ]; then
      echo "YMD in $INFECREC size error `expr length $STARTYMD`" | tee -a $LOGFILE
      SYORIBI=$ENDYMD
      return
    fi
    while [ $STARTYMD -lt $ENDYMD ];
    do
      TARGET=`date --date "$STARTYMD 1 day" +%Y%m%d`
      SYORIBI="$SYORIBI $TARGET"
      STARTYMD=$TARGET
    done
  else
    SYORIBI=$ENDYMD
  fi

  return
}

make_infection_file () {
  SRYYMD=$1
  rm -f $ERRFILE

  #外来分処理
  INFECGFILE=$DASDIR/${HOSPNUM}/infection/infectiong.csv
  rm -f $INFECGFILE
  PARA0="${TERMID},${HOSPNUM},${SRYYMD},2,${JOBID},${SHELLID}"
  PARAM="${PARA0},${INFECGFILE},${ERRFILE},${PKGVER}"
  $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBINF1 -parameter ${PARAM} 2>&1 | tee -a $LOGFILE
  if [ -e $ERRFILE ]; then
    rm -f $INFECGFILE
    return
  fi

  #入院分処理
  INFECNFILE=$DASDIR/${HOSPNUM}/infection/infectionn.csv
  rm -f $INFECNFILE
  if [ $BEDFLG -eq '1' ]; then
    PARA0="${TERMID},${HOSPNUM},${SRYYMD},1,${JOBID},${SHELLID}"
    PARAM="${PARA0},${INFECNFILE},${ERRFILE},${PKGVER}"
    $DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBINF1 -parameter ${PARAM} 2>&1 | tee -a $LOGFILE
    if [ -e $ERRFILE ]; then
      rm -f $INFECNFILE
      return
    fi
  else
    touch $INFECNFILE
  fi

  #アップロードファイル作成
  #外来と入院合体
  if [ -s $INFECGFILE ]; then
    if [ -s $INFECNFILE ]; then
      cp $INFECGFILE $INFECFILE
      grep -v "I," $INFECNFILE >> $INFECFILE
    else
      cp $INFECGFILE $INFECFILE
    fi
  else
    cp $INFECNFILE $INFECFILE
  fi
  rm -f $INFECGFILE
  rm -f $INFECNFILE
  if [ -e $INFECFILE ]; then
    if [ -s $INFECFILE ]; then
      TIMESTAMP=`(date +%Y%m%d%H%M%S)`
      RANDOMIZE=`ruby -e 'print sprintf("%04d",rand(10000))'`
      mv $INFECFILE $INFECFILE.$TIMESTAMP$RANDOMIZE
      gzip $INFECFILE.$TIMESTAMP$RANDOMIZE
    else
      rm -f $INFECFILE
    fi
  fi

  return
}

#main

if ! [ -d $DASDIR/${HOSPNUM} ]; then 
  mkdir $DASDIR/${HOSPNUM}
fi
if ! [ -d $DASDIR/${HOSPNUM}/infection ]; then 
  mkdir $DASDIR/${HOSPNUM}/infection
fi

#if [ -e $LOGFILE ]; then
#  cat $LOGFILE >> $LOG
#fi
rm -f $LOGFILE

#処理日を求める
syoribi
echo "SYORIBI [ $SYORIBI ]" | tee -a $LOGFILE

for sryymd in $SYORIBI; do
  #感染症データ作成
  echo "execute $sryymd" | tee -a $LOGFILE
  make_infection_file $sryymd
done

$DBSTUB -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${JOBID}${SHELLID},${HOSPNUM} 2>&1 | tee -a $LOGFILE

echo "感染症データ送信処理 起動" 2>&1 | tee -a $LOGFILE
$DASUPLOAD ${HOSPNUM} ${HOSPID} &

rm -f $INFECREC
echo $ENDYMD > $INFECREC

exit 0
