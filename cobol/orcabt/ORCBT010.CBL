      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBT010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 月次統計データ用基本情報作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/12    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *  01.00.02    NACL-藤原    04/02/06  包括の対象に保険組合せ９９９９
      *                                     を追加
      *  01.00.03    NACL-藤原    04/02/09  患者自己負担額が公費の上限額
      *                                     に到達しない場合のレセ種別
      *  01.00.04    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.05    NACL-藤原    04/04/05  病名コード９９９９９９９読み飛ばす
      *  01.00.06    NACL-藤原    04/04/06  編集病名のとき疑いフラグからの
      *                                     病名コード追加をやめる
      *  01.00.07    NACL-藤原    04/04/27  ユーザーコメントの追加
      *  01.00.08    NACL-藤原    04/08/17  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *                                     補助区分の追加に伴うレセプト
      *                                     種別の編集を変更
      *  01.00.09    NACL-小豆沢  05/04/06  病名データにセットするレセ電診療科
      *                                     を患者病名テーブルの診療科に変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    月次統計データ（基本情報）
           SELECT  TOUKEI01-FILE   ASSIGN  TOUKEI01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI01-KEY
                                   FILE    STATUS  IS  STS-TOUKEI01.
      *
           SELECT  TOUKEI011-FILE  ASSIGN  TOUKEI01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  TOUKEI011-KEY
                                   FILE    STATUS  IS  STS-TOUKEI01.
      *
      *    月次統計データ（統計情報）
           SELECT  TOUKEI02-FILE   ASSIGN  TOUKEI02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI02-KEY
                                   FILE    STATUS  IS  STS-TOUKEI02.
      *
           SELECT  TOUKEI021-FILE  ASSIGN  TOUKEI02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  TOUKEI021-KEY
                                   FILE    STATUS  IS  STS-TOUKEI02.
      *
      *    月次統計データ（病名情報）
           SELECT  TOUKEI03-FILE   ASSIGN  TOUKEI03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI03-KEY
                                   FILE    STATUS  IS  STS-TOUKEI03.
      *
           SELECT  TOUKEI031-FILE  ASSIGN  TOUKEI03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  TOUKEI031-KEY
                                   FILE    STATUS  IS  STS-TOUKEI03.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  WRK-PARA-ERRFILENM
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    月次統計データ
       FD  TOUKEI01-FILE.
       01  TOUKEI01-REC. 
           COPY    "CPTCF001.INC".
      *
       FD  TOUKEI011-FILE.
       01  TOUKEI011-REC. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //TOUKEI011//.
      *
      *    月次統計データ
       FD  TOUKEI02-FILE.
       01  TOUKEI02-REC. 
           COPY    "CPTCF002.INC".
      *
       FD  TOUKEI021-FILE.
       01  TOUKEI021-REC. 
           COPY    "CPTCF002.INC"  REPLACING  //TOUKEI02//
                                   BY         //TOUKEI021//.
      *
      *    月次統計データ
       FD  TOUKEI03-FILE.
       01  TOUKEI03-REC. 
           COPY    "CPTCF003.INC".
      *
       FD  TOUKEI031-FILE.
       01  TOUKEI031-REC. 
           COPY    "CPTCF003.INC"  REPLACING  //TOUKEI03//
                                   BY         //TOUKEI031//.
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI01//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI02//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI03//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI01                    PIC X(02).
           03  STS-TOUKEI02                    PIC X(02).
           03  STS-TOUKEI03                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01). 
           03  FLG-HKNNUM                      PIC 9(01). 
           03  FLG-TOUKEI01                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-KYUSEIRRK                   PIC 9(01).
           03  FLG-MONTHLYNUM                  PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-PTCOM                       PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
      * 
           03  FLG-KOH                         PIC 9(01).
           03  FLG-CONTKBN                     PIC 9(01).
           03  FLG-SET                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-TOUKEI01                    PIC 9(06).
           03  CNT-TOUKEI02                    PIC 9(06).
           03  CNT-TOUKEI03                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDXX                            PIC 9(03).
      **** 03  SYSKAN-IDX                      PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-HKNCOMBI              PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-HKNCOMBI              PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(16).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-PREFNUM            PIC X(02). 
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-HOUKATUKBN         PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-ERRFILENM          PIC X(100).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
      *
           03  WRK-PATH                    PIC X(64).
      *
      *
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(03).
      * 
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
      *
      *    傷病名編集
           03  WRK-BYOMEI          PIC X(150).
           03  WRK-BYOMEIW         PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *    病名文字数         
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-YM              PIC 9(06).
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
       01  WRK-TOUKEI01-TABLE.
           02  WRK-TOUKEI01-OCC    OCCURS  100.
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-TOUKEI01//.
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO//
                                   BY         //WRK-PTBYO//.
      *
           COPY    "MCPAREA".
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       WRK-TOUKEI01-TABLE
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-PREFNUM 
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOUKATUKBN 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-ERRFILENM
           END-UNSTRING 
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBT010"      TO  JOB-PGID
           MOVE    "月次統計データ作成"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
      *
           MOVE    WRK-SYMD        TO  WRK-STYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD 
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  ORC-DBYMD
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      *
      *    患者登録機能制御情報
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)              
           MOVE    "01"                TO  ORC-DBYMD (7:2)   
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
           ELSE    
               INITIALIZE                      SYS-1017-REC
           END-IF
           IF      SYS-1017-SEIHO-JKYSNUM
                                       =   SPACE
               MOVE    ZERO                TO  SYS-1017-SEIHO-JKYSNUM
           END-IF
      * 
      *    県内扱い保険者情報
           MOVE    SPACE           TO  WRK-1033-TABLE
      *     
           MOVE    SPACE           TO  SYS-1033-REC
           INITIALIZE                  SYS-1033-REC
           MOVE    "1033"          TO  SYS-1033-KANRICD
           MOVE    "*"             TO  SYS-1033-KBNCD
           MOVE    SYS-1033-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY5" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "SYSKANRI-KEY5" TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "SYSKANRI-KEY5" TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY5"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           MOVE    "TOKEI010"          TO  TOUKEI01-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI01-TERMID
      *
           MOVE    "TOKEI020"          TO  TOUKEI02-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI02-TERMID
      *
           MOVE    "TOKEI030"          TO  TOUKEI03-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI03-TERMID
      *
           OPEN    OUTPUT              TOUKEI011-FILE
           CLOSE                       TOUKEI011-FILE
           OPEN    OUTPUT              TOUKEI021-FILE
           CLOSE                       TOUKEI021-FILE
           OPEN    OUTPUT              TOUKEI031-FILE
           CLOSE                       TOUKEI031-FILE
      *
           OPEN    I-O                 TOUKEI01-FILE
                                       TOUKEI02-FILE
                                       TOUKEI03-FILE 
      *
      *    診療会計読み込み（患者ＩＤ、保険組合せ順）
           INITIALIZE                  SRYACCT-REC
           MOVE    WRK-PARA-HOSPID     TO  ACCT-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
           MOVE    "2"                 TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  WRK-TOUKEI01-TABLE

      *    テスト患者のときは読み飛ばす
           MOVE    WRK-PARA-HOSPID         TO  PTINF-HOSPID
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-END         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "SRYACCT-KEY8"          TO  ORC-DBPATH
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-END         =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    WRK-PARA-HOSPID     TO  COMB-HOSPID
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                        ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-END         =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       PERFORM 2001-FILE-SYORI-SEC 
      *
                       IF      WRK-UPDKBN          =   SPACE                   
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-END     =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                               MOVE    "SRYACCT-KEY8"  TO  ORC-DBPATH
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                           MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
      *
      *        基本ファイル出力処理
               PERFORM   2002-KIHON-WRITE-SEC
           END-IF    
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-FILE-SYORI-SEC              SECTION.
      *
           IF      WRK-UPDKBN          =   "1"
      *    保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF    
               END-IF    
           END-IF
      *
      *   主保険情報読み出し
           INITIALIZE                      PTHKNINF-REC
                                           PTRSIINF-REC
           IF      COMB-HKNID          NOT =   ZERO
               EVALUATE    COMB-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       INITIALIZE                  PTRSIINF-REC
                       MOVE    COMB-HOSPID         TO  PTRSI-HOSPID
                       MOVE    COMB-HKNID          TO  PTRSI-HKNID
                       MOVE    COMB-PTID           TO  PTRSI-PTID
                       MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTRSIINF-INV-SEC
                   WHEN    OTHER
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    WRK-PARA-HOSPID     TO  PTHKN-HOSPID
                       MOVE    COMB-PTID           TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       MOVE    COMB-TEKSTYMD       TO  ORC-DBYMD
                       PERFORM 910-PTHKNINF-INV-SEC
               END-EVALUATE 
           END-IF
      *            
      *    ファイル編集処理
           INITIALIZE                      TOUKEI01-REC
           MOVE    WRK-PARA-HOSPID     TO  TOUKEI01-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  TOUKEI01-SRYYM
           MOVE    ACCT-NYUGAIKBN      TO  TOUKEI01-NYUGAIKBN
           MOVE    COMB-PTID           TO  TOUKEI01-PTID
           MOVE    COMB-HKNNUM         TO  TOUKEI01-HKNNUM
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  TOUKEI01-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  TOUKEI01-KOHID
               ELSE
                   MOVE    ZERO                TO  TOUKEI01-KOHID
               END-IF 
           END-IF
           MOVE    PTHKN-HKNJANUM      TO  TOUKEI01-HKNJANUM
           MOVE    PTHKN-HONKZKKBN     TO  TOUKEI01-HONKZKKBN 
           EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                   END-IF
           END-EVALUATE             
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
               IF      WRK-TOUKEI01-HOSPID (IDXX)  =   SPACE   
                   PERFORM 20011-TOUKEI01-WRITE-SEC
                   MOVE    100             TO  IDXX
               ELSE
                   IF      WRK-TOUKEI01-KEY (IDXX) =
                                           TOUKEI01-KEY
                       PERFORM 20011-TOUKEI01-REWRITE-SEC
                       MOVE    100             TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    追加処理
      *****************************************************************
       20011-TOUKEI01-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
           IF      TOUKEI01-HKNNUM NOT =   SPACE
               EVALUATE    TOUKEI01-HKNNUM
                 WHEN    WRK-CONS-ROUSAI-HKNNUM
                 WHEN    WRK-CONS-JIBAI-HKNNUM
      *            労災情報 
                   MOVE   PTRSI-HKNID      TO  TOUKEI01-HKNID
                   MOVE   PTRSI-HKNKBN     TO  TOUKEI01-HKNKBN
                   MOVE   PTRSI-KOFUNUM    TO  TOUKEI01-KOFUNUM
                   MOVE   PTRSI-JIGYOUNAME TO  TOUKEI01-JIGYOUNAME
                   MOVE   PTRSI-PREFNAME   TO  TOUKEI01-PREFNAME
                   MOVE   PTRSI-PREFKBN    TO  TOUKEI01-PREFKBN  
                   MOVE   PTRSI-CITYNAME   TO  TOUKEI01-CITYNAME
                   MOVE   PTRSI-CITYKBN    TO  TOUKEI01-CITYKBN
      *
                   MOVE   PTRSI-SINKEI     TO  TOUKEI01-SINKEI
                   EVALUATE  PTRSI-HKNKBN 
                       WHEN    "1"  
                           IF   ( PTRSI-SINKEI = "1" OR "3" OR "7" )
                           AND  ( PTRSI-RYOSTYMD(1:6) 
                                               <  WRK-PARA-SRYYM )
                               MOVE   "5"      TO  TOUKEI01-SINKEI
                           END-IF
                       WHEN    "2"  
                           IF   ( PTRSI-SINKEI = "1" OR "7"        )
                           AND  ( PTRSI-RYOSTYMD(1:6) 
                                               <  WRK-PARA-SRYYM   )
                               MOVE   "5"      TO  TOUKEI01-SINKEI
                           END-IF
                   END-EVALUATE
      *
                   MOVE   PTRSI-TENKI      TO  TOUKEI01-TENKI
                   MOVE   PTRSI-SHOBYOYMD  TO  TOUKEI01-SHOBYOYMD
                   MOVE   PTRSI-SISIKBN    TO  TOUKEI01-SISIKBN
                   MOVE   PTRSI-RYOSTYMD   TO  TOUKEI01-RYOSTYMD
                   MOVE   PTRSI-RYOEDYMD   TO  TOUKEI01-RYOEDYMD
                   MOVE   PTRSI-SONSHOUKBN TO  TOUKEI01-SONSHOUKBN
                   MOVE   PTRSI-SONSHOUKBN-EDANUM
                                           TO
                                           TOUKEI01-SONSHOUKBN-EDANUM 
                   MOVE   PTRSI-SAIGAIKBN  TO  TOUKEI01-SAIGAIKBN 
                   MOVE   PTRSI-SINSATUYMD TO  TOUKEI01-SINSATUYMD
                   MOVE   PTRSI-SINSATUEND TO  TOUKEI01-SINSATUEND 
                   MOVE   PTRSI-JIBAISEIKBN
                                           TO  TOUKEI01-JIBAISEIKBN
                   MOVE   PTRSI-HIHKNJANAME
                                           TO  TOUKEI01-HIHKNJANAME
                 WHEN    OTHER
      *            主保険情報 
                   MOVE    PTHKN-HKNID     TO  TOUKEI01-HKNID
                                               TOUKEI01-THKNID     (1)
                   MOVE    PTHKN-TEKSTYMD  TO  TOUKEI01-TTEKSTYMD  (1)
                   MOVE    PTHKN-TEKEDYMD  TO  TOUKEI01-TTEKEDYMD  (1)
                   MOVE    PTHKN-SKKGETYMD TO  TOUKEI01-TSKKGETYMD (1)
      ****         MOVE    PTHKN-HKNJANUM  TO  TOUKEI01-THKNJANUM  (1)
                   MOVE    PTHKN-KIGO      TO  TOUKEI01-TKIGO      (1)
                   MOVE    PTHKN-NUM       TO  TOUKEI01-TNUM       (1)
                   MOVE    PTHKN-CONTKBN   TO  TOUKEI01-TCONTKBN   (1)
                   MOVE    PTHKN-HIHKNJANAME
                                           TO  TOUKEI01-THIHKNJANAME(1)
      *            法別番号
                   INITIALIZE                      HKNNUM-REC
                   MOVE    WRK-PARA-HOSPID     TO  HKN-HOSPID
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    ZERO                TO  HKN-PAYKBN   
                   MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       MOVE    HKN-HBTNUM      TO  TOUKEI01-HKNHBTNUM
                       MOVE    HKN-HKNKOHSBTKBN
                                               TO  TOUKEI01-HKNKOHSBTKBN
                   END-IF
                   MOVE    PTHKN-HOJOKBN       TO  TOUKEI01-HKN-HOJOKBN
               END-EVALUATE
           END-IF    
      *
      *    老人保険情報
           IF      TOUKEI01-KOHID  >   ZERO   
               MOVE    COMB-KOH1HKNNUM TO  TOUKEI01-RJNHKNNUM
      *
               MOVE    WRK-PARA-HOSPID TO  PTKOH-HOSPID
               MOVE    ACCT-PTID       TO  PTKOH-PTID
               MOVE    TOUKEI01-KOHID  TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   MOVE    PTKOH-PAYKBN        TO  TOUKEI01-RJNPAYKBN
                   MOVE    PTKOH-FTNJANUM      TO  TOUKEI01-RJNFTNJANUM
                   MOVE    PTKOH-JKYSNUM       TO  TOUKEI01-RJNJKYSNUM
                   MOVE    PTKOH-TEKSTYMD      TO  TOUKEI01-RJNTEKSTYMD
                   MOVE    PTKOH-TEKEDYMD      TO  TOUKEI01-RJNTEKEDYMD
      *            法別番号のセット
                   INITIALIZE                  HKNNUM-REC
                   MOVE    WRK-PARA-HOSPID TO  HKN-HOSPID
                   MOVE    PTKOH-KOHNUM    TO  HKN-HKNNUM
                   MOVE    PTKOH-PAYKBN    TO  HKN-PAYKBN   
                   MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
                   IF      FLG-HKNNUM      =   ZERO
                       MOVE   HKN-HBTNUM       TO TOUKEI01-RJNHBTNUM
                   END-IF
               END-IF
           END-IF
      *
      *    患者番号
           INITIALIZE                  ORCSPTIDAREA
           MOVE    TOUKEI01-HOSPID TO  SPTID-HOSPID
           MOVE    TOUKEI01-PTID   TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-1009-TBL
           MOVE    SPTID-PTNUM     TO  TOUKEI01-PTNUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME   TO  TOUKEI01-KANANAME
           MOVE   PTINF-NAME       TO  TOUKEI01-NAME
           MOVE   PTINF-SEX        TO  TOUKEI01-SEX
           MOVE   PTINF-BIRTHDAY   TO  TOUKEI01-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  TOUKEI01-HOME-POST
           MOVE    PTINF-HOME-ADRS TO  TOUKEI01-HOME-ADRS
           MOVE    PTINF-HOME-TEL1 TO  TOUKEI01-HOME-TEL1
           INITIALIZE                  ADRS-REC
           MOVE    PTINF-HOME-POST TO  ADRS-POST
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           MOVE    ADRS-LPUBCD     TO  TOUKEI01-LPUBCD
      *
      *    旧姓履歴の取得
           INITIALIZE                  KYUSEIRRK-REC
           MOVE    TOUKEI01-HOSPID   TO  KYUSEI-HOSPID
           MOVE    TOUKEI01-PTID     TO  KYUSEI-PTID
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC   TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK  =   ZERO
               MOVE    KYUSEI-NAME     TO  TOUKEI01-NAME
               MOVE    KYUSEI-KANANAME TO  TOUKEI01-KANANAME
           END-IF
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    TOUKEI01-HOSPID   TO  AGECHK-HOSPID
           MOVE    TOUKEI01-PTID     TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM    TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
           IF      AGECHK-RC       =   ZERO
               MOVE  AGECHK-NENREI     TO  TOUKEI01-AGE
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM
                                   TO  TOUKEI01-HKNCOMBI   (1)
      *    
           MOVE    ACCT-SRYKA      TO  TOUKEI01-SRYKA      (1)
      *
           MOVE    WRK-TEISEIKBN   TO  TOUKEI01-TEISEIKBN
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更処理
      *****************************************************************
       20011-TOUKEI01-REWRITE-SEC            SECTION.
      *
           MOVE    WRK-TOUKEI01-OCC (IDXX) TO  TOUKEI01-REC
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
      *    主保険情報 
           IF      TOUKEI01-HKNNUM     NOT =   SPACE
             EVALUATE    TOUKEI01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                 CONTINUE
               WHEN    OTHER
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       10
                   IF      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
                       MOVE    PTHKN-HKNID TO  TOUKEI01-HKNID
                       MOVE    PTHKN-HKNID TO  TOUKEI01-THKNID    (IDX)
                       MOVE    PTHKN-TEKSTYMD
                                           TO  TOUKEI01-TTEKSTYMD (IDX)
                       MOVE    PTHKN-TEKEDYMD
                                           TO  TOUKEI01-TTEKEDYMD (IDX)
                       MOVE    PTHKN-SKKGETYMD
                                           TO  TOUKEI01-TSKKGETYMD(IDX)
                       MOVE    PTHKN-KIGO  TO  TOUKEI01-TKIGO     (IDX)
                       MOVE    PTHKN-NUM   TO  TOUKEI01-TNUM      (IDX)
                       MOVE    PTHKN-CONTKBN
                                           TO  TOUKEI01-TCONTKBN  (IDX)
                       MOVE    PTHKN-HIHKNJANAME
                                           TO
                                           TOUKEI01-THIHKNJANAME  (IDX)
                       MOVE    10          TO  IDX
                   ELSE
                       IF      TOUKEI01-TTEKSTYMD(IDX)
                                           =   PTHKN-TEKSTYMD
                           MOVE    10          TO  IDX
                       END-IF    
                   END-IF    
               END-PERFORM
             END-EVALUATE
           END-IF
      *    
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      TOUKEI01-HKNCOMBI    (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       TOUKEI01-HKNCOMBI   (IDX)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       TOUKEI01-HKNCOMBI   (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *    
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               IF      TOUKEI01-SRYKA    (IDX)       =   SPACE
                   MOVE    ACCT-SRYKA      TO  TOUKEI01-SRYKA (IDX)
                   MOVE    20              TO  IDX
               ELSE
                   IF      TOUKEI01-SRYKA    (IDX)   =   ACCT-SRYKA
                       MOVE    20                  TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  TOUKEI01-TEISEIKBN     
           END-IF          
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       20012-KOHID-CHECK-SEC              SECTION.
      *
      *    老人保険以外の公費情報を編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               IF      COMB-KOHHKNNUM (IDX)    =   "027"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY         >   10
                       IF      COMB-KOHID (IDX)
                                               =   TOUKEI01-TKOHID (IDY)
                           MOVE    10              TO  IDY
                       ELSE  
                           IF      TOUKEI01-TKOHID (IDY)   =   ZERO
                               PERFORM 200121-KOHID-HENSYU-SEC
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20012-KOHID-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号編集処理
      *****************************************************************
       200121-KOHID-HENSYU-SEC                SECTION.
      *
           MOVE    COMB-KOHID (IDX)    TO  TOUKEI01-TKOHID    (IDY)
           MOVE    COMB-KOHHKNNUM(IDX) TO  TOUKEI01-TKOHHKNNUM(IDY)
           MOVE    COMB-KOHPAYKBN(IDX) TO  TOUKEI01-TKOHPAYKBN(IDY)
    *
           MOVE    WRK-PARA-HOSPID     TO  PTKOH-HOSPID
           MOVE    TOUKEI01-PTID       TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           IF       FLG-PTKOHINF       =   ZERO
               MOVE   PTKOH-FTNJANUM   TO  TOUKEI01-TKOHFTNJANUM (IDY)
               MOVE   PTKOH-TEKSTYMD   TO  TOUKEI01-TKOHTEKSTYMD (IDY)
               MOVE   PTKOH-TEKEDYMD   TO  TOUKEI01-TKOHTEKEDYMD (IDY)
      *             
      *        生保のとき月代わり受給者番号マスタより受給者番号をセット 
               IF    ( PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM )
               AND   ( SYS-1017-SEIHO-JKYSNUM
                                       =   ZERO                  )
                   MOVE    WRK-PARA-HOSPID TO  MONTHLYNUM-HOSPID
                   MOVE    TOUKEI01-PTID   TO  MONTHLYNUM-PTID
                   MOVE    WRK-CONS-SEIHO-HKNNUM
                                           TO  MONTHLYNUM-KOHNUM
                   MOVE    WRK-PARA-SRYYM  TO  MONTHLYNUM-SRYYM
                   MOVE    "2"             TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                   PERFORM 910-MONTHLYNUM-READ-SEC
                   IF      FLG-MONTHLYNUM  =   ZERO
                       MOVE    MONTHLYNUM-JKYSNUM 
                                       TO  TOUKEI01-TKOHJKYSNUM  (IDY)
                   END-IF
               ELSE                       
                   MOVE   PTKOH-JKYSNUM
                                       TO  TOUKEI01-TKOHJKYSNUM  (IDY)
               END-IF
      *    
      *        法別番号のセット
               INITIALIZE                 HKNNUM-REC
               MOVE    WRK-PARA-HOSPID TO HKN-HOSPID
               MOVE    PTKOH-KOHNUM    TO HKN-HKNNUM
               MOVE    PTKOH-PAYKBN    TO HKN-PAYKBN   
               MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-INV-SEC
               IF      FLG-HKNNUM      =   ZERO
                    MOVE   HKN-HBTNUM      TO  TOUKEI01-TKOHHBTNUM (IDY)
               END-IF
      *        地方公費のときレセプト請求区分を取得
               IF      COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8"
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( SYS-1001-PREFNUM    =   "40"              )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )    
                       PERFORM 20011-PREFKBN-40-CHECK-SEC
                   ELSE
                       IF      COMB-HKNNUM     =   "060"   OR  "067" 
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      HKN-RECESKYKBN  =   "2" OR  "3"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
                   END-IF
                   IF     TOUKEI01-TRECESKYKBN (IDY)   =   1
      *                地方公費レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPID TO  LNK-FTNJGNCHK-HOSPID
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "2"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       "2"  TO  TOUKEI01-TRECESKYKBN(IDY)
                       END-IF
                   END-IF
               ELSE
      *            全国公費のとき（長期）
                   IF      HKN-HKNKOHSBTKBN    =   "2" 
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（老人原爆）
                   IF    ( TOUKEI01-RJNHKNNUM
                                       NOT =   SPACE                   )
                   AND   ( COMB-KOHHKNNUM (IDX)
                                           =   WRK-CONS-GENBAKU-HKNNUM )
                       IF      TOUKEI01-HKNNUM =   "060"   OR  "067"
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               CONTINUE 
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               CONTINUE 
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（長期・老人原爆以外）
                   MOVE    "1"         TO  TOUKEI01-TRECESKYKBN(IDY)
               END-IF       
           END-IF
           .
       200121-KOHID-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    県外扱いの保険者はレセプトへ記載しない
           IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
           ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
           END-IF
           IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD      
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"     
                           IF      PTHKN-HKNJANUM = 
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF            
               END-PERFORM
           END-IF
      *        
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN         =   SPACE
               MOVE    "2"                 TO  TOUKEI01-TRECESKYKBN(IDY)
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPID         TO  LNK-SKOHPLUS-HOSPID
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    WRK-SYMD            TO  LNK-SKOHPLUS-SRYYMD
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
               IF      LNK-SKOHPLUS-RC     =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    "2"         TO  TOUKEI01-TRECESKYKBN(IDY)
                   ELSE
                       MOVE    "1"         TO  TOUKEI01-TRECESKYKBN(IDY)
                   END-IF
               ELSE
                   MOVE    "1"         TO  TOUKEI01-TRECESKYKBN (IDY)
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       2002-KIHON-WRITE-SEC              SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
                   OR      WRK-TOUKEI01-HOSPID (IDXX)  =   SPACE
               MOVE    WRK-TOUKEI01-OCC (IDXX)     TO  TOUKEI01-REC
      *
      *        レセプト種別
               PERFORM 20021-RECESYUBETU-SET-SEC
      *
               WRITE   TOUKEI01-REC
               ADD     1                    TO  CNT-TOUKEI01
           END-PERFORM
      *
      *    病名ファイル出力処理
           PERFORM 20022-BYOMEI-HENSYU-SEC
           .
       2002-RECESYUBETU-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      TOUKEI01-TEISEIKBN  =   "1"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *     
      *    労災・自賠責のとき
           IF      TOUKEI01-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               EVALUATE    TOUKEI01-HKNKBN
                                       ALSO    TOUKEI01-SAIGAIKBN
                   WHEN    "1"         ALSO    "1"
                       MOVE    7071        TO    TOUKEI01-RECESYUBETU
                   WHEN    "1"         ALSO    "2"
                       MOVE    7072        TO    TOUKEI01-RECESYUBETU
                   WHEN    "2"         ALSO    "1"
                       MOVE    7091        TO    TOUKEI01-RECESYUBETU
                   WHEN    "2"         ALSO    "2"
                       MOVE    7092        TO    TOUKEI01-RECESYUBETU
                   WHEN    "3"         ALSO    ANY   
                       MOVE    7021        TO    TOUKEI01-RECESYUBETU
                   WHEN    "4"         ALSO    ANY                         
                       MOVE    7001        TO    TOUKEI01-RECESYUBETU     
               END-EVALUATE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *     
      *    自費のとき
           IF     TOUKEI01-HKNKOHSBTKBN
                                       =   "8"
               MOVE    8888                TO  TOUKEI01-RECESYUBETU 
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *         
           IF      TOUKEI01-HKNNUM        NOT =   SPACE
               IF      TOUKEI01-HKNNUM    =   "060"   OR   "067"
                   IF   TOUKEI01-HKNNUM   =   "060"
                        MOVE   "1"      TO  WK-KOKUHO
                   ELSE
                        MOVE   "2"      TO  WK-KOKUHO
                   END-IF
               ELSE
                   MOVE    "1"          TO  WK-IHO
               END-IF
           END-IF
      *
           IF      WRK-PARA-SRYYM     <      "200210" 
             IF      TOUKEI01-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
           MOVE    TOUKEI01-HONKZKKBN  TO  WK-HONKZKKBN
      *
           MOVE    ZERO                TO  WK-KOHI-CNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      TOUKEI01-TKOHHKNNUM (IDX)   =   SPACE
               IF      TOUKEI01-TRECESKYKBN (IDX)  =   "1"
                   ADD     1               TO  WK-KOHI-CNT
               END-IF
           END-PERFORM
      *
      *    山形地方公費対応
           IF      WRK-PARA-PREFNUM    =   "06"
               IF  WK-KOKUHO               =     "1"  OR  "2"
                   IF   (TOUKEI01-TKOHHKNNUM(1) (1:1)  =   "1")   OR
                        (TOUKEI01-TKOHHKNNUM(2) (1:1)  =   "1")   OR
                        (TOUKEI01-TKOHHKNNUM(3) (1:1)  =   "1")   OR
                        (TOUKEI01-TKOHHKNNUM(4) (1:1)  =   "1")   OR
                        (TOUKEI01-TKOHHKNNUM(1) (1:1)  =   "2")   OR
                        (TOUKEI01-TKOHHKNNUM(2) (1:1)  =   "2")   OR
                        (TOUKEI01-TKOHHKNNUM(3) (1:1)  =   "2")   OR
                        (TOUKEI01-TKOHHKNNUM(4) (1:1)  =   "2")
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
      *    福岡地方公費対応
           IF      WRK-PARA-PREFNUM    =   "40"
               IF  WK-KOKUHO               =   "1" OR  "2"
                   IF   (TOUKEI01-TKOHHKNNUM(1)(1:1)  =   "1" OR  "2")
                   AND  (TOUKEI01-TKOHHBTNUM(1)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (TOUKEI01-TKOHHKNNUM(2)(1:1)  =   "1" OR  "2")
                   AND  (TOUKEI01-TKOHHBTNUM(2)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (TOUKEI01-TKOHHKNNUM(3)(1:1)  =   "1" OR  "2")
                   AND  (TOUKEI01-TKOHHBTNUM(3)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (TOUKEI01-TKOHHKNNUM(4)(1:1)  =   "1" OR  "2")
                   AND  (TOUKEI01-TKOHHBTNUM(4)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
           IF  TOUKEI01-HKNNUM     NOT =   SPACE
               IF      WRK-PARA-SRYYM      >=  "200210"
      *            ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
                   IF      TOUKEI01-AGE         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
      *
      *            １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *            "９"または"８"をセット
      *            老人保険の場合はＷＫ−ＲＪＮにセットする	
                   EVALUATE    COMB-HKNNUM
                       WHEN    "002" 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                       WHEN    "060" 
                           IF  (TOUKEI01-AGE  >=   70)  AND
                               (TOUKEI01-AGE  <=   74)
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-IF     
                       WHEN    OTHER 
                           EVALUATE   COMB-HOJOKBN
                               WHEN   "9"
                                   MOVE   "9"   TO   WK-HONKZKKBN
                               WHEN   "8"
                                   MOVE   "8"   TO   WK-HONKZKKBN
                           END-EVALUATE
                   END-EVALUATE
                   IF      TOUKEI01-RJNHKNNUM      =    "027"
                       EVALUATE   COMB-KOH1PAYKBN
                           WHEN   "01"
                               MOVE   "9"   TO   WK-RJN                     
                           WHEN   "02"
                               MOVE   "8"   TO   WK-RJN
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )     
           AND   ( TOUKEI01-HONKZKKBN  =   "1"   )     
               IF      TOUKEI01-HKNNUM =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *
      *      DISPLAY "HONKZKKBN : " WK-HONKZKKBN 
      *      DISPLAY "NYUGAIKBN : " TOUKEI01-NYUGAIKBN 
      *      DISPLAY "IHO       : " WK-IHO 
      *      DISPLAY "KOKUHO    : " WK-KOKUHO 
      *      DISPLAY "RJN       : " WK-RJN 
      *      DISPLAY "KOHI-CNT  : " WK-KOHI-CNT 
      *
      *    平成１４年９月まで
           IF      WRK-PARA-SRYYM     <=      "200209"
             EVALUATE  WK-HONKZKKBN ALSO TOUKEI01-NYUGAIKBN   ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1125            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1126            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1132            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1132            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1135            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1136            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1311            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1312            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1321            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1322            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1331            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1332            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1411            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1412            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1415            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1416            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1421            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1422            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1425            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1426            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1431            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1432            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1435            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1436            TO    TOUKEI01-RECESYUBETU
             END-EVALUATE
           END-IF
      *    平成１４年１０月改正対応
           IF      WRK-PARA-SRYYM     >=      "200210"
             EVALUATE  WK-HONKZKKBN ALSO TOUKEI01-NYUGAIKBN   ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
      *保険単独
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    TOUKEI01-RECESYUBETU
      *保険と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    TOUKEI01-RECESYUBETU
      *保険と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    TOUKEI01-RECESYUBETU
      *公費単独
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    TOUKEI01-RECESYUBETU
      *２種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    TOUKEI01-RECESYUBETU
      *３種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    TOUKEI01-RECESYUBETU
      *老人単独
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    TOUKEI01-RECESYUBETU
      *老人と公費１種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    TOUKEI01-RECESYUBETU
      *老人と公費２種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    TOUKEI01-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    TOUKEI01-RECESYUBETU
      *退職者単独
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1409            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410            TO    TOUKEI01-RECESYUBETU
      *退職者と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420            TO    TOUKEI01-RECESYUBETU
      *退職者と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431            TO    TOUKEI01-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433            TO    TOUKEI01-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435            TO    TOUKEI01-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437            TO    TOUKEI01-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439            TO    TOUKEI01-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430            TO    TOUKEI01-RECESYUBETU
             END-EVALUATE
           END-IF
           .
        20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報出力処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC              SECTION.
      *
           IF      WRK-PARA-DATAKBN    =   "1"
               GO  TO  20022-BYOMEI-HENSYU-EXT
           END-IF
      *    
      *    患者病名読み込み
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WRK-PARA-HOSPID     TO  PTBYO-HOSPID
           MOVE    KEY-O-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH 
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI     =   1
               PERFORM 200221-BYOMEI-WRITE-SEC
               MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH 
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH 
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    患者病名出力処理
      *****************************************************************
       200221-BYOMEI-WRITE-SEC         SECTION.
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   WRK-PARA-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   WRK-PARA-SRYYM 
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  WRK-PARA-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF       
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   WRK-PARA-NYUGAIKBN
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    "999999"のコードを読み飛ばす
           INITIALIZE                  WRK-PTBYOMEI-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               IF      PTBYO-BYOMEICD (IDX)    =   ALL "9"
                   CONTINUE
               ELSE
                   ADD     1               TO  WRK-PTBYO-BYOMEICDSU
                                               IDY
                   MOVE    PTBYO-BYOMEICD (IDX)
                                           TO  WRK-PTBYO-BYOMEICD (IDY)
               END-IF
           END-PERFORM
           MOVE    WRK-PTBYO-BYOMEICD-G
                                       TO  PTBYO-BYOMEICD-G
           MOVE    WRK-PTBYO-BYOMEICDSU
                                       TO  PTBYO-BYOMEICDSU    
      *
      *    疑いフラグより病名を編集
           MOVE    SPACE               TO  WRK-HENFLG1
                                           WRK-HENFLG2
      *       
           IF      PTBYO-UTAGAIFLG NOT =   SPACE
               COMPUTE WRK-MOJISU   =   PTBYO-BYOMEIMOJI   *   2
               IF      PTBYO-UTAGAIFLG  =   "1" OR  "3"
                   IF      WRK-MOJISU       <   8
                       MOVE    1                TO  IDZ
                   ELSE    
                       COMPUTE IDZ     =   WRK-MOJISU    -   7
                   END-IF    
                   PERFORM VARYING IDX FROM    IDZ BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:2)      =   "疑"
                           MOVE    "1"              TO  WRK-HENFLG1
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF                
               IF      PTBYO-UTAGAIFLG =   "2" OR  "3"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       WRK-MOJISU
                       IF      PTBYO-BYOMEI (IDX:4)      =   "急性"
                           MOVE    "1"              TO  WRK-HENFLG2
                           MOVE    WRK-MOJISU       TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *     
      *    病名文字数を計算
           COMPUTE WRK-MOJISU          =   PTBYO-BYOMEIMOJI   *   2
           MOVE    PTBYO-BYOMEI      TO  WRK-BYOMEI
      *
      *    疑いフラグがセットされているとき
           IF    ( PTBYO-UTAGAIFLG   =   "1" OR  "3" )
           AND   ( WRK-HENFLG1         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEI
               STRING  PTBYO-BYOMEI        DELIMITED BY  SPACE
                       WRK-CONS-UTAGAI     DELIMITED BY  SIZE
                       INTO                WRK-BYOMEI
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
               IF      PTBYO-BYOMEIHENFLG  =   SPACE
                   IF      PTBYO-BYOMEICD (21)  NOT =   SPACE             
                       MOVE    "1"             TO  PTBYO-BYOMEIHENFLG
                   ELSE
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       21
                           IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                               MOVE    WRK-CONS-BYOMEICD-UTAGAI
                                               TO  PTBYO-BYOMEICD (IDX)
                               MOVE    21           TO  IDX
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF    
           IF    ( PTBYO-UTAGAIFLG   =   "2" OR  "3" )
           AND   ( WRK-HENFLG2         =   SPACE     )                         
               MOVE    SPACE               TO  WRK-BYOMEIW
               STRING  WRK-CONS-KYUSEI     DELIMITED BY  SIZE
                       WRK-BYOMEI          DELIMITED BY  SPACE
                       INTO                WRK-BYOMEIW
               COMPUTE WRK-MOJISU          =      
                                WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
               MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI  
               IF      PTBYO-BYOMEIHENFLG  =   SPACE
                   IF      PTBYO-BYOMEICD (21) NOT =   SPACE             
                       MOVE    "1"             TO  PTBYO-BYOMEIHENFLG
                   ELSE
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       21
                           IF      PTBYO-BYOMEICD (IDX)    =   SPACE
                               MOVE    WRK-CONS-BYOMEICD-KYUSEI
                                               TO  PTBYO-BYOMEICD (IDX)
                               MOVE    21           TO  IDX
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE                      TOUKEI03-REC
           MOVE    TOUKEI01-HOSPID     TO  TOUKEI03-HOSPID
           MOVE    TOUKEI01-SRYYM      TO  TOUKEI03-SRYYM
           MOVE    TOUKEI01-NYUGAIKBN  TO  TOUKEI03-NYUGAIKBN
           MOVE    TOUKEI01-PTID       TO  TOUKEI03-PTID
           MOVE    TOUKEI01-RECEKA     TO  TOUKEI03-RECEKA
           MOVE    PTBYO-SRYKA         TO  TOUKEI03-SRYKA
           MOVE    PTBYO-SRYYMD        TO  TOUKEI03-SRYYMD
           MOVE    PTBYO-RENNUM        TO  TOUKEI03-RENNUM
           MOVE    WRK-BYOMEI          TO  TOUKEI03-BYOMEI
           COMPUTE TOUKEI03-BYOMEIMOJI    =   WRK-MOJISU    /   2
           MOVE    PTBYO-UTAGAIFLG     TO  TOUKEI03-UTAGAIFLG
           MOVE    PTBYO-SYUBYOFLG     TO  TOUKEI03-SYUBYOFLG   
           MOVE    PTBYO-MANSEIKBN     TO  TOUKEI03-MANSEIKBN
           MOVE    PTBYO-HKNCOMBI      TO  TOUKEI03-HKNCOMBI
           MOVE    PTBYO-REZEFLG       TO  TOUKEI03-REZEFLG
           IF      WRK-PARA-SRYYM       <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  TOUKEI03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  TOUKEI03-TENKIYMD   
           END-IF    
           MOVE    PTBYO-HKNBYOMEIFLG  TO  TOUKEI03-HKNBYOMEIFLG
           MOVE    PTBYO-BYOMEIHENFLG  TO  TOUKEI03-BYOMEIHENFLG   
           MOVE    PTBYO-BYOMEICD-G    TO  TOUKEI03-BYOMEICD-G
           MOVE    PTBYO-BYOMEICDSU    TO  TOUKEI03-BYOMEICDSU
      *    レセプト診療科
           IF      PTBYO-SRYKA     NOT =   SPACE    
               INITIALIZE                      SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    PTBYO-SRYKA         TO  SYS-1005-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD(1:6)
               MOVE    "01"                TO  ORC-DBYMD(7:2)
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
               MOVE    SYS-1005-RECESRYKA  TO  TOUKEI03-RECESRYKA
           END-IF
      *
           WRITE   TOUKEI03-REC
           ADD     1                   TO  CNT-TOUKEI03
           .
       200221-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計・診療行為データ編集処理
      *****************************************************************
       2003-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    統計データ（病名）のみのとき
           IF      WRK-PARA-DATAKBN    =   "2"
               GO  TO  2003-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *     
      *    レセプト診療科
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD(1:6)
           MOVE    "01"                TO  ORC-DBYMD(7:2)
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 20031-SRYACT-HENSYU-SEC
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           .
       2003-SRYACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療情報出力処理
      *****************************************************************
       20031-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   5
                   OR      SRY-SRYCD (IDX2)    =   SPACE
      *
               MOVE    SPACE               TO  TOUKEI02-REC
               INITIALIZE                      TOUKEI02-REC
      *
      *        診療会計データ編集
               MOVE    TOUKEI01-KEY        TO  TOUKEI02-KEY
               MOVE    ACCT-ZAINUM         TO  TOUKEI02-ZAINUM
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  TOUKEI02-SEQNO
               MOVE    COMB-HKNID          TO  TOUKEI02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  TOUKEI02-TKOHID(IDX3)
               END-PERFORM
               MOVE    ACCT-SRYKA          TO  TOUKEI02-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  TOUKEI02-RECESRYKA
               MOVE    ACCT-SRYKBN         TO  TOUKEI02-SRYKBN
               MOVE    ACCT-JIHOKBN        TO  TOUKEI02-JIHOKBN
               MOVE    ACCT-ZAITEN         TO  TOUKEI02-ZAITEN
               MOVE    ACCT-ZAIKAISU       TO  TOUKEI02-ZAIKAISU
               MOVE    ACCT-MEISAISU       TO  TOUKEI02-MEISAISU
               MOVE    ACCT-SYUTEN1        TO  TOUKEI02-SYUTEN1
               MOVE    ACCT-SYUTEN2        TO  TOUKEI02-SYUTEN2
               MOVE    ACCT-YKZTEN         TO  TOUKEI02-YKZTEN
               MOVE    ACCT-KIZAITEN       TO  TOUKEI02-KIZAITEN
               MOVE    ACCT-DRCD           TO  TOUKEI02-DRCD
               MOVE    ACCT-HKNCOMBI       TO  TOUKEI02-HKNCOMBI
               MOVE    ACCT-DAY-TBL (1)    TO  TOUKEI02-DAY-AREA
      *
      *        診療行為データ編集
               MOVE    SRY-RENNUM          TO  TOUKEI02-RENNUM
               IF      SRY-SRYSYUKBN       =   SPACE
                   MOVE    ACCT-SRYKBN     TO  TOUKEI02-SRYSYUKBN (1:2)
                   MOVE    "0"             TO  TOUKEI02-SRYSYUKBN (3:1)
               ELSE
                   MOVE    SRY-SRYSYUKBN   TO  TOUKEI02-SRYSYUKBN
               END-IF
               MOVE    SRY-JIHIMONEY(IDX2) TO  TOUKEI02-JIHIMONEY
               MOVE    SRY-SRYCD    (IDX2) TO  TOUKEI02-SRYCD
               MOVE    SRY-SRYSURYO (IDX2) TO  TOUKEI02-SRYSURYO
               MOVE    SRY-SRYKAISU (IDX2) TO  TOUKEI02-SRYKAISU
               MOVE    SRY-INPUTNUM (IDX2) TO  TOUKEI02-INPUTNUM
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SRY-SRYCD(IDX2)     TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANICD          TO  TOUKEI02-TANICD
               MOVE    TNS-TANINAME        TO  TOUKEI02-TANINAME
               MOVE    TNS-FORMALNAME      TO  TOUKEI02-FORMALNAME
               MOVE    TNS-TENSIKIBETU     TO  TOUKEI02-TENSIKIBETU
               MOVE    TNS-TEN             TO  TOUKEI02-TEN
               IF      TNS-SRYCD (1:1)     =   '6'
                   MOVE    TNS-YAKKAKJNCD      TO  TOUKEI02-YAKKAKJNCD
               END-IF
               IF      TNS-TENSIKIBETU     =   1   OR   3
                   IF      ACCT-NYUGAIKBN      =   "1"
                       MOVE  TNS-NYUTENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   ELSE
                       MOVE  TNS-GAITENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   END-IF
               END-IF
      *
               IF (   (SRY-SRYCD    (IDX2)(1:1)
                                               =   "8"  )
                   OR (SRY-SRYCD    (IDX2)(1:3)
                                               =   "008") ) 
               AND (   SRY-INPUTNUM (IDX2) NOT =   ZERO   )
                   MOVE    SRY-HOSPID              TO  PTCOM-HOSPID
                   MOVE    SRY-PTID                TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM              TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD   (IDX2)      TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)      TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT       TO  TOUKEI02-NAME
               ELSE
                   MOVE    TNS-NAME                TO  TOUKEI02-NAME
               END-IF
      *
               WRITE   TOUKEI02-REC
               ADD     1               TO  CNT-TOUKEI02 
           END-PERFORM
      *
           .
       TOUKEI02-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           CLOSE   TOUKEI01-FILE
                   TOUKEI02-FILE
                   TOUKEI03-FILE
      *
           IF      CNT-TOUKEI01           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI01  CNT-TOUKEI01:" CNT-TOUKEI01
           DISPLAY "TOUKEI01  CNT-TOUKEI02:" CNT-TOUKEI02
           DISPLAY "TOUKEI01  CNT-TOUKEI03:" CNT-TOUKEI03
           DISPLAY "*** ORCBT010 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "SRYACCT-KEY8"      TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           IF      FLG-END             =   ZERO
      *        包括を含まないとき
               IF    ( WRK-PARA-HOUKATUKBN =   "1"  )
                   IF    ( ACCT-ZAITEN         =   ZERO   )
                   OR    ( ACCT-HKNCOMBI       =   "9999" )
                       GO  TO  900-SRYACCT-READ-SEC
                   END-IF
               END-IF
      *
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTHKNINF-KEY3"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTKOHINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "KYUSEIRRK-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "KYUSEIRRK-KEY3"    TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "KYUSEIRRK-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "MONTHLYNUM-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"    TO      ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-ADRS-INV-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "ADRS-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "ADRS-KEY2"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               INITIALIZE                  ADRS-REC
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "ADRS-KEY2"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1                   TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE                 PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-TOUKEI01-READ-SEC             SECTION.
      *
           READ    TOUKEI01-FILE
               INVALID
                   MOVE    1           TO  FLG-TOUKEI01
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOUKEI01
           END-READ
           .
       900-TOUKEI01-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       900-DBCLOSE-EXT.
           EXIT.
