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
       PROGRAM-ID.             ORCBM520.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : オンライン請求−レセ電個別作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/10/17    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/11/19  レセ電ＣＤ−Ｒ出力対応
      *  04.04.02    NACL-藤原    09/12/10  返戻対応
      *  04.04.03    NACL-藤原    10/03/09  医療機関情報（住所）の取得
      *                                     方法の修正
      *
      *  04.05.01    NACL-藤原    10/03/23  平成２２年４月改正対応
      *                                     請求に係わる記録条件仕様の
      *                                     変更に伴う修正
      *  04.05.02    NACL-藤原    12/06/05  レセ電データ無し時のエラー処理
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
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
           03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  RECE30PARA-HOSPNUM  PIC 9(02).
           03  RECE30PARA-FILENM   PIC X(07)   VALUE   "RECEDEN".
           03  RECE30PARA-RENNUM   PIC 9(02).
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
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-BTPARA              PIC 9(01).
           03  FLG-HENREI-B            PIC 9(01).
      *
           03  FLG-DATA                PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECEDEN             PIC 9(06).
           03  CNT-RECE10              PIC 9(06).
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
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU         PIC 9(06).
           03  SUM-TOTALTEN           PIC 9(10).
      *
      *    一時領域
      *    パラメタエリア
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           05  WRK-PARA-RENNUM         PIC 9(3).
           05  WRK-PARA-FOLDER         PIC X(150).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-DATAKBN        PIC X(01).
           05  WRK-PARA-FIXEDSIZE      PIC 9(07).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
           05  WRK-PARA-PRTKBN         PIC X(01).
      *
           03  WRK-RECEERR             PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
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
           03  WRK-SRYKA               PIC X(02).
           03  WRK-KUGIRI-KAISU        PIC 9(02). 
      *
           03  WRK-INFO1-INDEX         PIC 9(04).        
           03  WRK-INFO2-INDEX         PIC 9(04).        
      *
      *    データ長
           03  WRK-DATA-LENGTH         PIC 9(07).        
           03  WRK-PTNUM-LENGTH        PIC 9(07).   
           03  WRK-LEN                 PIC 9(07). 
      *       
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *       
           03  WRK-MCP-RECEDEN-TABLE   PIC X(64).
           03  WRK-MCP-RECEDEN-PATHNAME
                                       PIC X(64).
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
      *    パラメタＤＢ編集用
           03  WRK-BTPARA-INFO-PARA.
               05  WRK-BTPARA-DATA PIC X(10).
               05  WRK-BTPARA-FILENM
                                   PIC X(20).
               05  WRK-BTPARA-DATAKBN
                                   PIC X(01).
               05  WRK-BTPARA-STNO PIC 9(02).
               05  WRK-BTPARA-EDNO PIC 9(02).
               05  WRK-BTPARA-WRNO PIC 9(02).
      * 
      *    タイトル
           03  WRK-SFILESV-TITLE       PIC X(100).
      *
      *    医療機関情報ワーク
           COPY    "CPSK1001.INC"      REPLACING  //SYS-1001//
                                       BY         //WRK-SYS-1001//.
           COPY    "CPSK1002.INC"      REPLACING  //SYS-1002//
                                       BY         //WRK-SYS-1002//.
      *    更新対象返戻データ
       01  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
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
           03  WRK-CONS-LENGTH         PIC 9(07)   VALUE   1441500.
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
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *    オンライン返戻明細
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
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
           MOVE    "ORCBM520"      TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "レセ電ファイル個別作成（社保）"
                                           TO  JOB-SHELLMSG
               WHEN    "2"
                   MOVE    "レセ電ファイル個別作成（国保）"
                                           TO  JOB-SHELLMSG
               WHEN    "3"
                   MOVE    "レセ電ファイル個別作成（広域）"
                                           TO  JOB-SHELLMSG
               WHEN    "4"
                   MOVE    "レセ電ファイル個別作成（国保、広域）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           IF    ( WRK-PARA-FIXEDSIZE  NUMERIC         )
           AND   ( WRK-PARA-FIXEDSIZE
                               NOT =   WRK-CONS-LENGTH )
               MOVE    WRK-PARA-FIXEDSIZE      TO  WRK-CONS-LENGTH
           END-IF
           DISPLAY "LENGTH=" WRK-CONS-LENGTH
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 120-SKYYM-HENSYU-SEC
      *
           PERFORM 900-SEIKYU-SELECT-SEC
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
                                       SUM-AREA
                                       WRK-LENGTH 
                                       FLG-DATA 
      *     
      *    医療機関情報レコード
           PERFORM     2001-HENSYU-SEC
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-ERR     =   1
      *
               INITIALIZE                  TABLE-AREA
                                           WRK-PTNUM-LENGTH
      *
      *???
               DISPLAY "PTNUM=" ONRECE-B-PTNUM
                       " SRYYM=" ONRECE-B-SRYYM 
                       " RECESYUBETU="  ONRECE-B-RECESYUBETU 
                       " U-HENREI-FLG="  ONRECE-B-U-HENREI-FLG
      *???
               PERFORM 900-RECEDEN-SELECT-SEC
               IF      FLG-RECEDEN     =   1
                   STRING
                       "該当患者のレセ電データが存在しません"
                                               DELIMITED  BY  SIZE
                        "　患者番号＝"         DELIMITED  BY  SIZE
                        ONRECE-B-PTNUM         DELIMITED  BY  SIZE
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               PERFORM     UNTIL   FLG-RECEDEN =   1
                   PERFORM 2002-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    WRK-MCP-RECEDEN-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        返戻再請求時の履歴管理情報
               IF      ONRECE-B-U-HENREI-FLG   =   "1"
                   PERFORM 2009-HENREI-BODY-HENSYU-SEC
               END-IF
      *
               MOVE    "tbl_onrece_body"   TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-ONRECE-BODY-FETCH-SEC
      *
      *        ＦＤ出力のときは複数枚のチェックをする
               IF      WRK-PARA-DATAKBN    =   "1" OR  "3"
                   COMPUTE WRK-LEN =
                           WRK-DATA-LENGTH +   WRK-PTNUM-LENGTH
                   IF      WRK-LEN         >   WRK-CONS-LENGTH
      *???
               DISPLAY "2008 WRITE VOL="  WRK-VOLNUM " LEN=" WRK-LEN 
      *???
      *                合計書レコード
                       PERFORM 2008-HENSYU-SEC
      *???
               DISPLAY "CNT-OUT=" CNT-OUT
      *???
      *                医療機関情報レコード
                       PERFORM 2001-HENSYU-SEC
                   END-IF
               END-IF
      *        データ出力処理
               PERFORM 2003-TABLE-WRITE-SEC
           END-PERFORM
      * 
      *    合計書レコード
           IF      FLG-ERR         =   ZERO
      *???
               DISPLAY "2008 WRITE VOL="  WRK-VOLNUM 
      *???
               MOVE    9               TO  FLG-DATA 
               PERFORM 2008-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    医療機関情報レコード編集処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *     
           COMPUTE WRK-FILE-RENNUM =   WRK-VOLNUM  +   1
           MOVE    WRK-FILE-RENNUM     TO  RECE30PARA-RENNUM
           MOVE    WRK-PARA-HOSPNUM    TO  RECE30PARA-HOSPNUM
           IF      WRK-PARA-PRTKBN     =   "1"
               MOVE    "RECEDEN"           TO  RECE30PARA-FILENM
           ELSE
               MOVE    "TENKENT"           TO  RECE30PARA-FILENM
           END-IF
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
           MOVE    SPACE                TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
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
       2001-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理
      *****************************************************************
       2002-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
      *
           IF      RECEDEN-RECEDATA (1:2)  =   "RE"
      *        総件数・点数の集計
               EVALUATE    RECEDEN-RECESYUBETU (3:1)
                   WHEN    "1"
                       ADD     1               TO  SUM-TOTALKENSU
                   WHEN    "2"
                       ADD     2               TO  SUM-TOTALKENSU
                   WHEN    "3"
                       ADD     3               TO  SUM-TOTALKENSU
                   WHEN    "4"
                       ADD     4               TO  SUM-TOTALKENSU
                   WHEN    "5"
                       ADD     5               TO  SUM-TOTALKENSU
               END-EVALUATE
      *
               ADD     RECEDEN-TOTALTEN    TO  SUM-TOTALTEN
      *  
      *        レセプト番号の編集
               MOVE    "RE,"               TO  WRK-REC
               MOVE    3                   TO  WRK-REC-LENGTH
               ADD     1                   TO  WRK-RECENUM
               MOVE    WRK-RECENUM         TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       RECEDEN-RECEDATA (5:)     DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               MOVE    WRK-REC         TO  RECEDEN-RECEDATA
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
      *
           MOVE    ZERO             TO  WRK-MOJISU
                                        WRK-NAGASA
           EVALUATE    RECEDEN-RECEDATA (1:2)
               WHEN    "RE"
                   INSPECT RECEDEN-RECEDATA   TALLYING    WRK-MOJISU
                                               FOR ALL "," 
                   IF      WRK-MOJISU          =   16  OR  17  OR  25
                       PERFORM VARYING IDX FROM    2500 BY  -1
                               UNTIL   IDX <       1
                           IF      RECEDEN-RECEDATA (IDX:1)
                                                   =   WRK-KAIGYO
                               COMPUTE WRK-NAGASA  =   IDX     -   1
                               MOVE    1               TO  IDX
                           END-IF    
                       END-PERFORM   
                       IF      WRK-MOJISU          =   16
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,,,,,,,,,,,,,,"
                                                   DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                       END-IF
                       IF      WRK-MOJISU          =   17
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,,,,,,,,,,,,,"
                                                   DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                       END-IF
                       IF      WRK-MOJISU          =   25
                           MOVE    ZERO                TO  IDXX
                                                           IDWW
                                                       WRK-KUGIRI-KAISU
                           MOVE    SPACE               TO  WRK-SRYKA
                           PERFORM VARYING IDX FROM    WRK-NAGASA BY  -1
                                   UNTIL   IDX <       1
                               IF      RECEDEN-RECEDATA (IDX:1)
                                                       =   WRK-KUGIRI
                                   COMPUTE WRK-KUGIRI-KAISU    =
                                               WRK-KUGIRI-KAISU    +   1
                                   IF      WRK-KUGIRI-KAISU    =   9
                                       MOVE    IDX     TO  WRK-NAGASA
                                       COMPUTE IDWW    =   IDX +   1
                                       IF      RECEDEN-RECEDATA (IDWW:2)
                                                       NOT =   ",,"
                                           MOVE
                                               RECEDEN-RECEDATA (IDWW:2)
                                                           TO  WRK-SRYKA
                                       END-IF
                                       MOVE    1               TO  IDX
                                   END-IF
                               END-IF        
                           END-PERFORM   
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,"        DELIMITED  BY  SIZE
                                     WRK-SRYKA     DELIMITED  BY  SPACE
                                 ",,,,,,,,,,,,,,"  DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                       END-IF
                       MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                   END-IF     
               WHEN    "TO"
                   INSPECT RECEDEN-RECEDATA    TALLYING    WRK-MOJISU
                                               FOR ALL "," 
                   IF      WRK-MOJISU          =   9   OR  10  OR  16
                       PERFORM VARYING IDX FROM    2500    BY  -1
                               UNTIL   IDX <       1
                           IF      RECEDEN-RECEDATA (IDX:1)
                                                   =   WRK-KAIGYO
                               COMPUTE WRK-NAGASA  =   IDX     -   1
                               MOVE    1               TO  IDX
                           END-IF    
                       END-PERFORM   
                       IF      WRK-MOJISU          =   9
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,,"     DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                           MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                       END-IF
                       IF      WRK-MOJISU          =   10
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,"      DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                           MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                       END-IF
                       IF      WRK-MOJISU          =   16
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
                                                   DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                           MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                       END-IF
                   END-IF     
               WHEN    "SY"
                   IF      RECEDEN-SRYYM       <   200804
                       IF      RECEDEN-RECEDATA (4:7)
                                                   =   "0000998"
                           MOVE    "0000999"   TO
                                               RECEDEN-RECEDATA (4:7)
                       END-IF
                       INSPECT RECEDEN-RECEDATA    TALLYING  WRK-MOJISU
                                                   FOR ALL "," 
                       IF      WRK-MOJISU          =   6
                           PERFORM VARYING IDX FROM    2500 BY  -1
                                   UNTIL   IDX <       1
                               IF      RECEDEN-RECEDATA (IDX:1)
                                                   =   WRK-KAIGYO
                                   COMPUTE WRK-NAGASA  =   IDX     -   1
                                   MOVE    1               TO  IDX
                               END-IF    
                           END-PERFORM   
                           STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ","           DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                           END-STRING
                           MOVE    WRK-REC         TO  RECEDEN-RECEDATA
                       END-IF     
                   END-IF     
               WHEN    "SI"
               WHEN    "IY"
                   IF      RECEDEN-SRYYM       <   201004
                       INSPECT RECEDEN-RECEDATA    TALLYING  WRK-MOJISU
                                                   FOR ALL "," 
                       IF      WRK-MOJISU          =   6   OR  12
                           PERFORM VARYING IDX FROM    2500 BY  -1
                                   UNTIL   IDX <       1
                               IF      RECEDEN-RECEDATA (IDX:1)
                                                       =   WRK-KAIGYO
                                   COMPUTE WRK-NAGASA  =   IDX     -   1
                                   MOVE    1               TO  IDX
                               END-IF    
                           END-PERFORM   
                           IF      WRK-MOJISU          =   6
                               STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,"      DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                               END-STRING
                               MOVE    WRK-REC     TO  RECEDEN-RECEDATA
                           END-IF     
                           IF      WRK-MOJISU          =   12
                               STRING    RECEDEN-RECEDATA (1:WRK-NAGASA)
                                                   DELIMITED  BY  SIZE
                                     ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
                                                   DELIMITED  BY  SIZE
                                     WRK-KAIGYO    DELIMITED  BY  SIZE
                                     INTO          WRK-REC
                               END-STRING
                               MOVE    WRK-REC     TO  RECEDEN-RECEDATA
                           END-IF     
                       END-IF
                   END-IF     
           END-EVALUATE
      *
           ADD     1                   TO  TABLE-MAX
           MOVE    RECEDEN-RECEDATA    TO  TBL-RECEDATA (TABLE-MAX)
      *
           MOVE    ZERO                TO  WRK-REC-LENGTH
           PERFORM VARYING IDX FROM    2500 BY  -1
                   UNTIL   IDX <       1
               IF      RECEDEN-RECEDATA (IDX:1)
                                       =   WRK-KAIGYO
                   MOVE    IDX             TO  WRK-REC-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           COMPUTE TBL-LENGTH (TABLE-MAX)
                                       =   WRK-REC-LENGTH  +   1
      *
           COMPUTE WRK-PTNUM-LENGTH    =   WRK-PTNUM-LENGTH
                                       +   WRK-REC-LENGTH
                                       +   1
      *
           PERFORM 900-RECEDEN-READ-SEC     
           .
       2002-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2003-TABLE-WRITE-SEC         SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       TABLE-MAX
               MOVE    TBL-RECEDATA (IDX)  TO  RECE30-R
               WRITE   RECE30-R
      *
               IF      STS-RECE30          =   "00"
                   CONTINUE
               ELSE
                   CALL "coblog" USING   "file write err " RECE30PARA
                   MOVE    SPACE               TO  WRK-RECEERR
                   STRING "ファイル 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                           STS-RECE30              DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-OUT
           END-PERFORM
      *
           COMPUTE WRK-DATA-LENGTH  =   WRK-DATA-LENGTH
                                    +   WRK-PTNUM-LENGTH
      *
           .
       2003-TABLE-WRITE-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    合計レコード編集処理
      *****************************************************************
       2008-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "GO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *
           IF      FLG-DATA            =   9
               MOVE    SUM-TOTALKENSU      TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *
           IF      FLG-DATA            =   9
               MOVE    SUM-TOTALTEN        TO  WRK-NUM
               MOVE    10                  TO  WRK-NAGASA
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *
           IF      FLG-DATA            =   9
               MOVE    "99"                TO  WRK-DATA
           ELSE
               COMPUTE WRK-VOLNUM  =   WRK-VOLNUM  +   1
               MOVE    WRK-VOLNUM          TO  WRK-DATA
           END-IF    
           MOVE    2                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *
           CLOSE   RECE30-FILE
      *
      *    パラメタＤＢ更新処理
           IF      FLG-DATA            =   ZERO    OR  9
               PERFORM 20081-BTPARA-UPDATE-SEC
           END-IF
      *
           IF      FLG-DATA            =   ZERO 
               MOVE    1                   TO  FLG-DATA
           END-IF
           .
       2008-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    パラメタＤＢ更新処理
      *****************************************************************
       20081-BTPARA-UPDATE-SEC     SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
           MOVE    "recept4.sh"        TO  BTPARA-SCRIPTID
           MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           PERFORM 900-BTPARA-READ-SEC
           IF      FLG-BTPARA          =   ZERO
               MOVE    BTPARA-INFO-PARA    TO  WRK-BTPARA-INFO-PARA
               EVALUATE    FLG-DATA
                   WHEN    ZERO
                       MOVE    WRK-FILE-RENNUM TO  WRK-BTPARA-STNO
                   WHEN    9
                       MOVE    WRK-FILE-RENNUM TO  WRK-BTPARA-EDNO
               END-EVALUATE
               MOVE    WRK-BTPARA-INFO-PARA
                                           TO  BTPARA-INFO-PARA
               MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
               MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "パラメタＤＢの更新処理でエラーとなりました"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           ELSE
               MOVE    "対象のパラメタＤＢが存在しません" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           .
       20081-BTPARA-UPDATE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    返戻再請求時の履歴管理情報編集処理
      *****************************************************************
       2009-HENREI-BODY-HENSYU-SEC             SECTION.
      *    
           INITIALIZE                  WRK-HENREI-B-REC
      *    
           INITIALIZE                  HENREI-B-REC
           MOVE    ONRECE-B-HOSPNUM
                                   TO  HENREI-B-HOSPNUM
           MOVE    ONRECE-B-NYUGAIKBN
                                   TO  HENREI-B-NYUGAIKBN
           MOVE    ONRECE-B-SRYYM-B
                                   TO  HENREI-B-SRYYM
           EVALUATE    ONRECE-B-TEISYUTUSAKI-B
               WHEN    1
                   MOVE    "1"         TO  HENREI-B-TEISYUTUSAKI
                                           HENREI-B-XXTEISYUTUSAKI
               WHEN    2
               WHEN    6
                   MOVE    "2"         TO  HENREI-B-TEISYUTUSAKI
                   MOVE    "6"         TO  HENREI-B-XXTEISYUTUSAKI
           END-EVALUATE
           MOVE    ONRECE-B-PTID   TO  HENREI-B-PTID
           MOVE    HENREI-B-REC    TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_henrei_body"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *           
           PERFORM         UNTIL   FLG-HENREI-B    =   1
               PERFORM VARYING IDZZ    FROM    10  BY  -1
                       UNTIL   IDZZ    <       1
                   IF      HENREI-B-S-TEISYUTUSAKI(IDZZ)
                                       =       SPACE
                       CONTINUE
                   ELSE 
                       IF    ( HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                           =   ONRECE-B-TEISYUTUSAKI-B )
                       AND   ( HENREI-B-S-NYUGAIKBN    (IDZZ)
                                           =   ONRECE-B-NYUGAIKBN    )
                       AND   ( HENREI-B-S-RECEKA       (IDZZ)
                                           =   ONRECE-B-RECEKA       )
                       AND   ( HENREI-B-S-RECESYUBETU  (IDZZ)
                                           =   ONRECE-B-RECESYUBETU  )
                       AND   ( HENREI-B-S-HKNJANUM     (IDZZ)
                                           =   ONRECE-B-HKNJANUM     )
                       AND   ( HENREI-B-S-HOJOKBN-KEY  (IDZZ)
                                           =   ONRECE-B-HOJOKBN      )
                       AND   ( HENREI-B-S-KOHID-KEY  (IDZZ)
                                           =   ONRECE-B-KOHID        )
                       AND   ( HENREI-B-S-TEKSTYMD     (IDZZ)
                                           =   ONRECE-B-TEKSTYMD     )
                           MOVE    HENREI-B-REC    TO  WRK-HENREI-B-REC
                           MOVE    1               TO  IDZZ
                           MOVE    1               TO  FLG-HENREI-B
                       END-IF  
                   END-IF  
               END-PERFORM
      *
               IF      FLG-HENREI-B        =   ZERO
                   MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-HENREI-BODY-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      WRK-HENREI-B-HOSPNUM    >   ZERO
               MOVE    ZERO                TO  FLG-HENREI-B
      *     
               INITIALIZE                      HENREI-B-REC
               MOVE    WRK-HENREI-B-REC    TO  HENREI-B-REC
               MOVE    HENREI-B-REC        TO  MCPDATA-REC
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-HENREI-BODY-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-HENREI-B
               END-IF
      *
               PERFORM     UNTIL   FLG-HENREI-B    =   1
                   PERFORM 20091-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        返戻データ更新処理
               PERFORM 20091-HENREI-B-UPDATE-SEC
      *
      *        レセプト共通レコードへ検索番号をセット
               PERFORM 20092-RE-HENREI-B-SET-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "設定返戻データ存在エラー"
                                               DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
          .
       2009-HENREI-BODY-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理（履歴管理情報）
      *****************************************************************
       20091-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-INFO1-INDEX
                                       WRK-INFO2-INDEX
           MOVE    ZERO            TO  IDZZ
           PERFORM VARYING IDZZ    FROM    20  BY  -1
                   UNTIL   IDZZ   <        1
               IF      HENREI-B-HENREI-INFO1 (IDZZ:1)
                                       NOT =   SPACE
                   MOVE    IDZZ            TO  WRK-INFO1-INDEX
                   MOVE    1               TO  IDZZ
               END-IF
           END-PERFORM
      *
           MOVE    ZERO            TO  IDZZ
           PERFORM VARYING IDZZ    FROM    2200    BY  -1
                   UNTIL   IDZZ   <        1
               IF      HENREI-B-HENREI-INFO2 (IDZZ:1)
                                       NOT =   SPACE
                   MOVE    IDZZ            TO  WRK-INFO2-INDEX
                   MOVE    1               TO  IDZZ
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-REC
           STRING  HENREI-B-HENREI-INFO1 (1:WRK-INFO1-INDEX)
                                           DELIMITED   BY  SIZE
                   HENREI-B-HENREI-INFO2 (1:WRK-INFO2-INDEX)
                                           DELIMITED   BY  SIZE
                   WRK-KAIGYO              DELIMITED   BY  SIZE
                                           INTO        WRK-REC
           END-STRING
      *
           ADD     1                   TO  TABLE-MAX
           MOVE    WRK-REC             TO  TBL-RECEDATA (TABLE-MAX)
      *
           MOVE    ZERO                TO  WRK-REC-LENGTH
           PERFORM VARYING IDX FROM    2500 BY  -1
                   UNTIL   IDX <       1
               IF      WRK-REC (IDX:1) =   WRK-KAIGYO
                   MOVE    IDX             TO  WRK-REC-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           COMPUTE WRK-PTNUM-LENGTH    =   WRK-PTNUM-LENGTH
                                       +   WRK-REC-LENGTH
                                       +   1
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-HENREI-BODY-READ-SEC
           .
       20091-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    返戻データ更新処理
      *****************************************************************
       20091-HENREI-B-UPDATE-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING IDZZ    FROM    10  BY  -1
                   UNTIL   IDZZ    <       1
               IF      WRK-HENREI-B-S-TEISYUTUSAKI(IDZZ)
                                   =       SPACE
                   CONTINUE
               ELSE 
                   IF      ONRECE-B-TEISYUTUSAKI-B =
                                   WRK-HENREI-B-S-TEISYUTUSAKI(IDZZ)
                   AND     ONRECE-B-NYUGAIKBN      =
                                   WRK-HENREI-B-S-NYUGAIKBN   (IDZZ)
                   AND     ONRECE-B-RECEKA         =
                                   WRK-HENREI-B-S-RECEKA      (IDZZ)
                   AND     ONRECE-B-RECESYUBETU    =
                                   WRK-HENREI-B-S-RECESYUBETU (IDZZ)
                   AND     ONRECE-B-HKNJANUM       =
                                   WRK-HENREI-B-S-HKNJANUM    (IDZZ)
                   AND     ONRECE-B-HOJOKBN        =
                                   WRK-HENREI-B-S-HOJOKBN-KEY (IDZZ)
                   AND     ONRECE-B-KOHID          =
                                   WRK-HENREI-B-S-KOHID-KEY   (IDZZ)
                   AND     ONRECE-B-TEKSTYMD       =
                                   WRK-HENREI-B-S-TEKSTYMD    (IDZZ)
                       MOVE    SMCNDATE-YMD    TO
                                       WRK-HENREI-B-S-SYORIYMD (IDZZ)  
                       MOVE    1               TO
                                       WRK-HENREI-B-S-MAKE-FLG (IDZZ)  
      *
                       MOVE    SMCNDATE-YMD    TO  WRK-HENREI-B-UPYMD
                       MOVE    SMCNDATE-HMS    TO  WRK-HENREI-B-UPHMS
      *
                       MOVE    WRK-HENREI-B-REC    TO  MCPDATA-REC
                       MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           CALL    "coblog"
                                   USING "abort update tbl_henrei_body"
      *
                           MOVE    SPACE               TO  WRK-RECEERR
                           MOVE    MCP-RC              TO  WRK-MCP-RC
                           STRING  "返戻明細ＤＢ更新エラー MCP-RC="
                                                   DELIMITED  BY  SIZE
                               WRK-MCP-RC          DELIMITED  BY  SIZE
                                                   INTO    WRK-RECEERR
                           END-STRING
                           PERFORM 500-ERR-HENSYU-SEC
                           PERFORM 500-COBABORT-SEC
                       END-IF
      *
                       MOVE    1                   TO  IDZZ
                   END-IF
               END-IF
           END-PERFORM
          .
       20091-HENREI-B-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト共通レコードへ返戻情報編集処理
      *****************************************************************
       20092-RE-HENREI-B-SET-SEC         SECTION.
      *
           MOVE    TBL-RECEDATA (1)    TO  WRK-REC
           MOVE    SPACE               TO  WRK-REC1
      *
           MOVE    ZERO                TO  IDZZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2500
               IF      WRK-REC (IDX:1) =   ","
                   ADD     1               TO  IDZZ
                   IF      IDZZ            =   18
      *                検索番号
                       COMPUTE IDY     =   IDX +   1
                       STRING   WRK-REC (1:IDX)    DELIMITED  BY  SIZE
                                WRK-HENREI-B-SEARCH-NO
                                                   DELIMITED  BY  SPACE
                                WRK-REC (IDY:)     DELIMITED  BY  SIZE
                                           INTO    WRK-REC1
                       END-STRING
      *************    MOVE    WRK-REC1            TO  WRK-REC
      *************    MOVE    SPACE               TO  WRK-REC1
      *************END-IF
      *************IF      IDZZ            =   19
      *************    記録条件仕様年月情報
      *************    COMPUTE IDY     =   IDX +   1
      *************    STRING   WRK-REC (1:IDX)    DELIMITED  BY  SIZE
      *************             "42204"            DELIMITED  BY  SPACE
      *************             WRK-REC (IDY:)     DELIMITED  BY  SIZE
      *************                        INTO    WRK-REC1
      *************    END-STRING
                       MOVE    2400        TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-REC1            TO  TBL-RECEDATA (1)
      *
           MOVE    ZERO                TO  WRK-REC-LENGTH
           PERFORM VARYING IDX FROM    2500 BY  -1
                   UNTIL   IDX <       1
               IF      WRK-REC1(IDX:1) =   WRK-KAIGYO
                   MOVE    IDX             TO  WRK-REC-LENGTH
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           COMPUTE WRK-PTNUM-LENGTH    =   WRK-PTNUM-LENGTH
                                       -   TBL-LENGTH (1)
                                       +   WRK-REC-LENGTH
                                       +   1
      *
          COMPUTE TBL-LENGTH (1)   =   WRK-REC-LENGTH
                                       +   1
          .
       20092-RE-HENREI-B-SET-EXT.
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
           IF      CNT-RECEDEN     =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ファイル保存情報更新
               IF      WRK-PARA-DATAKBN    =   "5" OR  "6"
                   PERFORM 3001-FILE-INFO-INSERT-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE300  IN   " CNT-IN  
           DISPLAY "RECE300  OUT  " CNT-OUT
           DISPLAY "RECE300  FILE " WRK-FILE-RENNUM  
           DISPLAY "*** ORCBM520 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    "recept4.sh"    TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
           MOVE    WRK-PARA-FOLDER TO  SFILESV-FOR-FOLDER
                                                        (1)
           IF      WRK-PARA-DATAKBN    =   "5"
               IF      WRK-PARA-PRTKBN     =   "1"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "RECEIPTC.UKE"      DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   MOVE    "RECEIPTC.UKE"  TO  SFILESV-TO-DATA  (1)
               ELSE
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "TENKENRC.UKE"      DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   MOVE    "TENKENRC.UKE"  TO  SFILESV-TO-DATA  (1)
               END-IF
           ELSE
               IF      WRK-PARA-PRTKBN     =   "1"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "RECEIPTC.iso"      DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   MOVE    "RECEIPTC.iso"  TO  SFILESV-TO-DATA  (1)
               ELSE
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "TENKENRC.iso"      DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   MOVE    "TENKENRC.iso"  TO  SFILESV-TO-DATA  (1)
               END-IF
           END-IF
           MOVE    CNT-OUT         TO  SFILESV-CNT-ALL  (1)
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "レセ電ファイル作成（社保）"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "2"
                   MOVE    "レセ電データ（国保）"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "3"
                   MOVE    "レセ電データ（広域）"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "4"
                   MOVE    "レセ電データ（国保、広域）"
                                           TO  SFILESV-TITLE    (1)
           END-EVALUATE
           IF      WRK-PARA-PRTKBN =   "2"
               MOVE    SFILESV-TITLE (1)   TO  WRK-SFILESV-TITLE
               MOVE    SPACE               TO  SFILESV-TITLE (1)
               STRING  "【点検用】"        DELIMITED  BY  SIZE
                       WRK-SFILESV-TITLE   DELIMITED  BY  SPACE
                                           INTO    SFILESV-TITLE (1)
               END-STRING
           END-IF       
           MOVE    "1"             TO  SFILESV-DATA-TYPE(1)
      *
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
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
           STRING  "ORCBM520 "         DELIMITED  BY  SIZE
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-END
      *     
           INITIALIZE                  ONRECE-B-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ONRECE-B-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  ONRECE-B-SRYYM
           MOVE    WRK-PARA-TEISYUTUSAKI
                                       TO  ONRECE-B-TEISYUTUSAKI
           MOVE    WRK-PARA-RENNUM     TO  ONRECE-B-RENNUM
      *
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-ONRECE-BODY-SELECT-SEC
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルSELECT処理
      *****************************************************************
       900-ONRECE-BODY-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
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
      *    明細ファイルFETCH処理
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
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN
      *     
           INITIALIZE                      RECEDEN-REC
           MOVE    ONRECE-B-HOSPNUM    TO  RECEDEN-HOSPNUM
           MOVE    ONRECE-B-SRYYM-B    TO  RECEDEN-SRYYM
           MOVE    ONRECE-B-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
           MOVE    ONRECE-B-PTID       TO  RECEDEN-PTID
           MOVE    ONRECE-B-RECEKA     TO  RECEDEN-RECEKA
           MOVE    ONRECE-B-TEISYUTUSAKI-B
                                       TO  RECEDEN-TEISYUTUSAKI
           MOVE    ONRECE-B-RECESYUBETU
                                       TO  RECEDEN-RECESYUBETU
           MOVE    ONRECE-B-HKNJANUM   TO  RECEDEN-HKNJANUM 
           MOVE    ONRECE-B-HOJOKBN    TO  RECEDEN-HOJOKBN-KEY
           MOVE    ONRECE-B-KOHID      TO  RECEDEN-KOHID-KEY
           MOVE    ONRECE-B-TEKSTYMD   TO  RECEDEN-TEKSTYMD
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           IF      WRK-PARA-PRTKBN     =   "1"
               MOVE    "key2"              TO  WRK-MCP-RECEDEN-PATHNAME
           ELSE
               MOVE    "key4"              TO  WRK-MCP-RECEDEN-PATHNAME
           END-IF
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    WRK-MCP-RECEDEN-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-RECEDEN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
           .
       900-RECEDEN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-READ-SEC           SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    WRK-MCP-RECEDEN-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECEDEN
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
               ADD     1                   TO  CNT-RECEDEN
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           .
       900-RECEDEN-READ-EXT.
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
               PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-EXT.
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
