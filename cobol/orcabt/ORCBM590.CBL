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
       PROGRAM-ID.             ORCBM590.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 連携用データ作成（個別）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/12/09    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    レセプト電算ファイル  
           SELECT  RECE30-FILE     ASSIGN  RECE30PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECE30.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト電算ファイル  
       FD  RECE30-FILE.
       01  RECE30-R         PIC X(2500).
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
       COPY    "COMMON-SPA".
      *
      *    レセプト電算ファイル 名称領域 
       01  RECE30PARA.
           03  FILLER              PIC X(05)   VALUE 
                                               "/tmp/".
           03  RECE30PARA-HOSPNUM  PIC 9(02).
           03  RECE30PARA-FILENM   PIC X(07)   VALUE 
                                               "RECEDEN".
           03  RECE30PARA-TEISYUTUSAKI
                                   PIC X(01).
           03  RECE30PARA-NYUGAIKBN
                                   PIC X(01).
           03  RECE30PARA-CREYMD   PIC X(08).
           03  RECE30PARA-CREHMS   PIC X(06).
           03  FILLER              PIC X(04)   VALUE   ".txt".
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE30              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ONRECE-B            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RENKEI-TMP             PIC 9(01).
      *
           03  FLG-DATA                PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(05).
           03  IDX                     PIC 9(05). 
           03  IDY                     PIC 9(05). 
           03  IDZ                     PIC 9(05).
           03  IDY1                    PIC 9(05).
           03  IDY2                    PIC 9(05).
           03  IDY3                    PIC 9(05).
           03  IDWW                    PIC 9(05).
           03  IDXX                    PIC 9(05).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
      *
      *    一時領域
      *    パラメタエリア
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-RENNUM     PIC 9(3).
               05  WRK-PARA-FOLDER     PIC X(200).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-DATAKBN    PIC X(01).
               05  WRK-PARA-FIXEDSIZE  PIC 9(07).
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-PRTKBN     PIC X(01).
               05  WRK-PARA-NYUGAIKBN  PIC X(01).
      *
      *    マルチボリューム識別情報     
           03  WRK-VOLNUM              PIC 9(02).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).        
      *    請求年月（和暦）
           03  WRK-SEIYM               PIC 9(05).         
      *    診療年月（和暦）
           03  WRK-SRYYM               PIC 9(05). 
      *
           03  WRK-MOJISU              PIC 9(02).        
      *
      *    データ長
           03  WRK-DATA-LENGTH         PIC 9(07).        
      *
      *     右づめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC1                PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-FILE-RENNUM         PIC 9(02).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"    REPLACING  //RECEDAT1PARA//
                                       BY         //WRK-ERR-FILE-NM//.
      *
      *    医療機関情報ワーク
           COPY    "CPSK1001.INC"      REPLACING  //SYS-1001//
                                       BY         //WRK-SYS-1001//.
           COPY    "CPSK1002.INC"      REPLACING  //SYS-1002//
                                       BY         //WRK-SYS-1002//.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  WRK-HENYMD-FIL      PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
       01  TABLE-AREA.
           03  TABLE-OCC               OCCURS  10000.
               05  TBL-RECEDATA        PIC X(2500).
               05  TBL-LENGTH          PIC 9(04).
           03  TABLE-MAX               PIC 9(05).
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報（基本）
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報（住所）
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE                   DIVISION
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              CNT-AREA
                                   WRK-AREA
                                   FLG-AREA
                                   SPA-AREA
           MOVE    ZERO            TO  WRK-VOLNUM
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM   
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-RENNUM
                                               WRK-PARA-FOLDER
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-FIXEDSIZE
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM590"      TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
                           ALSO    WRK-PARA-NYUGAIKBN   
               WHEN    "1" ALSO    "1"
                   MOVE    "連携ファイル作成（社保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "1" ALSO    "2"
                   MOVE    "連携ファイル作成（社保・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "1"
                   MOVE    "連携ファイル作成（国保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "2"
                   MOVE    "連携ファイル作成（国保・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "3" ALSO    "1"
                   MOVE    "連携ファイル作成（広域・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "3" ALSO    "2"
                   MOVE    "連携ファイル作成（広域・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "4" ALSO    "1"
                   MOVE    "連携ファイル作成（国保、広域・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "4" ALSO    "2"
                   MOVE    "連携ファイル作成（国保、広域・入院外）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 120-SKYYM-HENSYU-SEC
      *
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
      *    医療機関情報編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報（基本）が取得できません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "医療機関情報（住所）が取得できません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SYS-1001-HOSPNAME   TO  WRK-SYS-1001-HOSPNAME
               MOVE    SYS-1002-TEL        TO  WRK-SYS-1002-TEL
      *        医療機関編集情報
               PERFORM 900-1900-READ-SEC
           END-IF    
      *    
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    請求年月編集処理      
      *****************************************************************
       120-SKYYM-HENSYU-SEC      SECTION.
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-SEIYM 
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-SRYYM 
      *    
           .
       120-SKYYM-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-RECENUM
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-ERR     =   1
      *
               INITIALIZE                  TABLE-AREA
      *
      *???
               DISPLAY "PTNUM=" ONRECE-B-PTNUM
                       " SRYYM=" ONRECE-B-SRYYM 
      *???
               PERFORM 900-RENKEI-TMP-SELECT-SEC
               IF      FLG-RENKEI-TMP     =   1
                   STRING
                       "該当患者の連携データが存在しません"
                                               DELIMITED  BY  SIZE
                        "　患者番号＝"         DELIMITED  BY  SIZE
                        ONRECE-B-PTNUM         DELIMITED  BY  SIZE
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               PERFORM     UNTIL   FLG-RENKEI-TMP =   1
                   PERFORM 2001-HENSYU-SEC
               END-PERFORM
      *
               PERFORM 2002-FILE-WRITE-SEC
      *
               MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    "tbl_onrece_body"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *
           ADD     1                   TO  TABLE-MAX
           MOVE    RECEDEN-RECEDATA    TO  TBL-RECEDATA (TABLE-MAX)
      *
           PERFORM 900-RENKEI-TMP-READ-SEC
           .
       2001-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2002-FILE-WRITE-SEC         SECTION.
      *
           IF      TABLE-MAX        >   1
               IF      CNT-OUT         =   ZERO
      *            医療機関情報レコード編集処理
                   PERFORM     20021-HENSYU-SEC
               END-IF
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       TABLE-MAX
                   IF      TBL-RECEDATA (IDX) (1:2)    =   "RE"
      *                レセプト番号の編集
                       MOVE    SPACE               TO  WRK-REC
                       MOVE    "RE,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
                       ADD     1                   TO  WRK-RECENUM
                       MOVE    WRK-RECENUM         TO  WRK-NUM
                       MOVE    6                   TO  WRK-NAGASA
                       PERFORM 5005-NUM-HENSYU-SEC
      *
                       STRING  WRK-REC (1:WRK-REC-LENGTH) 
                                                   DELIMITED   BY  SIZE
                               TBL-RECEDATA (IDX)  (5:)
                                                   DELIMITED   BY  SIZE
                                                   INTO        WRK-REC
                       END-STRING
                       MOVE    WRK-REC         TO  TBL-RECEDATA (IDX)
                   END-IF
      *
                   MOVE    TBL-RECEDATA (IDX)  TO  RECE30-R
                   WRITE   RECE30-R
      *
                   IF      STS-RECE30          =   "00"
                       CONTINUE
                   ELSE
                       CALL   "coblog"
                                   USING   "file write err " RECE30PARA
                       MOVE    SPACE               TO  WRK-RECEERR
                       STRING "ファイル 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                               STS-RECE30          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT
               END-PERFORM
           END-IF
      *
           INITIALIZE              TABLE-AREA
      *
           .
       2002-FILE-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    医療機関情報レコード編集処理
      *****************************************************************
       20021-HENSYU-SEC              SECTION.
      *     
           COMPUTE WRK-FILE-RENNUM =   WRK-VOLNUM  +   1
           MOVE    WRK-PARA-HOSPNUM    TO  RECE30PARA-HOSPNUM
           IF      WRK-PARA-TEISYUTUSAKI
                                   =   "1"
               MOVE    "S"             TO  RECE30PARA-TEISYUTUSAKI
           ELSE
               MOVE    "K"             TO  RECE30PARA-TEISYUTUSAKI
           END-IF
           IF      WRK-PARA-NYUGAIKBN
                                   =   "1"
               MOVE    "2"             TO  RECE30PARA-NYUGAIKBN
           ELSE
               MOVE    "1"             TO  RECE30PARA-NYUGAIKBN
           END-IF
           MOVE    LNK-PRTKANRI-TBL-GROUP (1:8)
                                   TO  RECE30PARA-CREYMD
           MOVE    LNK-PRTKANRI-TBL-GROUP (9:6)
                                   TO  RECE30PARA-CREHMS
      *??
           DISPLAY "PARA=" RECE30PARA "#"
      *??
      *
           OPEN    OUTPUT              RECE30-FILE
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " RECE30PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "ファイル オープンエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-RECE30              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    ZERO                TO  WRK-DATA-LENGTH
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "IR,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *     
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "1"         TO  WRK-DATA
               WHEN    OTHER
                   MOVE    "2"         TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *     
           MOVE    SYS-1001-PREFNUM    TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *     
           MOVE    SYS-1001-TENHYOKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *     
           MOVE    SYS-1001-HOSPCD     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *     
           PERFORM 5004-MOJI-HENSYU-SEC
      *            
           MOVE    WRK-SYS-1001-HOSPNAME
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *            
           MOVE    WRK-SEIYM           TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *            
           MOVE    WRK-VOLNUM          TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *            
           MOVE    WRK-SYS-1002-TEL    TO  WRK-DATA
           MOVE    15                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20021-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "tbl_onrece_body"
                                   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      CNT-OUT         >   ZERO
               CLOSE   RECE30-FILE
           END-IF
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECENUM     TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE300  IN   " CNT-IN  
           DISPLAY "RECE300  OUT  " CNT-OUT
           DISPLAY "RECE300  RE   " WRK-RECENUM
           DISPLAY "RECE300  FILE " WRK-FILE-RENNUM  
           DISPLAY "*** ORCBM590 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *            
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM590 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING        
      *
           MOVE    WRK-REC             TO  RECE30-R         
           WRITE   RECE30-R
      *
           IF      STS-RECE30          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file write err " RECE30PARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING  "ファイル 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                       STS-RECE30              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           ADD     1                   TO  CNT-OUT
      *
           COMPUTE WRK-DATA-LENGTH  =   WRK-DATA-LENGTH
                                    +   WRK-REC-LENGTH
                                    +   2
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    ZERO                TO  WRK-SYMD
           END-IF    
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    右づめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW)           DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW                 TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイル読み込み処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-END
      *     
           INITIALIZE                  ONRECE-B-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ONRECE-B-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  ONRECE-B-SRYYM
           MOVE    WRK-PARA-TEISYUTUSAKI
                                       TO  ONRECE-B-TEISYUTUSAKI
           MOVE    WRK-PARA-RENNUM     TO  ONRECE-B-RENNUM
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-BODY-FETCH-SEC
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-END
           END-IF
           .
       900-ONRECE-BODY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイル読み込み処理
      *****************************************************************
       900-ONRECE-BODY-FETCH-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  ONRECE-B-REC
               MOVE  ZERO                  TO  FLG-ONRECE-B
               ADD   1                     TO  CNT-IN
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-END
           END-IF
           .
       900-ONRECE-BODY-FETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    連携テンポラリ読込
      *****************************************************************
       900-RENKEI-TMP-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RENKEI-TMP
      *     
           INITIALIZE                      RECEDEN-REC
           MOVE    ONRECE-B-HOSPNUM    TO  RECEDEN-HOSPNUM
           MOVE    ONRECE-B-SRYYM-B    TO  RECEDEN-SRYYM
           MOVE    ONRECE-B-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
           MOVE    ONRECE-B-PTID       TO  RECEDEN-PTID
           IF      ONRECE-B-TEISYUTUSAKI-B
                                       =   "1"
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RENKEI-TMP-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RENKEI-TMP
           END-IF
           .
       900-RENKEI-TMP-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    連携テンポラリ読込
      *****************************************************************
       900-RENKEI-TMP-READ-SEC           SECTION.
      *
           MOVE    "tbl_renkei_tmp"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RENKEI-TMP
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
      *
               EVALUATE    RECEDEN-RECEDATA (1:2)
                   WHEN    "RE"
                       MOVE    SPACE           TO  WRK-SRYKBN
                   WHEN    "TO"
                   WHEN    "IY"
                   WHEN    "SI"
                   WHEN    "CO"
                       IF      RECEDEN-RECEDATA (4:1)  = ","
                           CONTINUE
                       ELSE
                           MOVE    RECEDEN-RECEDATA (4:2)
                                                   TO  WRK-SRYKBN
                       END-IF
                   WHEN    OTHER
                       GO  TO  900-RENKEI-TMP-READ-SEC
               END-EVALUATE
      *
               EVALUATE    RECEDEN-RECEDATA (1:2)
                   WHEN    "RE"
                       CONTINUE
                   WHEN    "IY"
                       IF      WRK-SRYKBN   =   "14"    OR  "21"    OR
                                                "22"    OR  "23"    OR
                                                "31"    OR  "32"    OR
                                                "33"
                           CONTINUE
                       ELSE
                           GO  TO  900-RENKEI-TMP-READ-SEC
                       END-IF
                   WHEN    "SI"
                       IF      WRK-SRYKBN   =   "14"    OR  "31"    OR
                                                "32"    OR  "33"    OR
                                                "60"
                           CONTINUE
                       ELSE
                           GO  TO  900-RENKEI-TMP-READ-SEC
                       END-IF
                   WHEN    OTHER
                       GO  TO  900-RENKEI-TMP-READ-SEC
               END-EVALUATE
      *
               ADD     1                   TO  CNT-IN
           ELSE
               MOVE    1                   TO  FLG-RENKEI-TMP
           END-IF
      *
           .
       900-RENKEI-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(19)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-SYS-1001-HOSPNAME
           ELSE
               INITIALIZE                      SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(19)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1902-REC
               IF      SYS-1902-TEL    NOT =   SPACE
                   MOVE    SYS-1902-TEL        TO  WRK-SYS-1002-TEL
               END-IF
           ELSE
               INITIALIZE                      SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
