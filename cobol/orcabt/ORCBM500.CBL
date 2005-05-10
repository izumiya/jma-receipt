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
       PROGRAM-ID.             ORCBM500.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト電算ファイル作成（ＣＳＶ形式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/18    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-藤原    04/03/18  対象が無い場合のエラー表示
      *
      *  ２００４年４月仕様変更
      *  01.01.01    NACL-藤原    04/04/02  電話番号（医療機関情報）
      *                                     の追加
      *                                     改正に伴う項目数の変更
      *  01.01.02    NACL-藤原    04/08/10  入院レセ電の追加
      *  01.01.03    NACL-藤原    04/09/17  入外区分によって処理対象を
      *                                     限定する
      *  01.01.04    NACL-藤原    05/05/09  改定通信医１６０３９号
      *                                     「主」の記録についてより
      *                                    「0000998」を「0000999」に変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    レセプト電算ファイル  
           SELECT  RECE30-FILE     ASSIGN  "/var/tmp/RECE300.dat"
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
       01  RECE30-R         PIC X(200).
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *     レセプト電算ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE30//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03                      PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE30              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECEDEN             PIC 9(06).
           03  CNT-RECE10              PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(03). 
           03  IDY                     PIC 9(03). 
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(03).
           03  IDY2                    PIC 9(03).
           03  IDY3                    PIC 9(03).
           03  IDXX                    PIC 9(03).
      *
       01  KEY-AREA                                VALUE   LOW-VALUE.
           03  KEY-NEW.
      *****    05  KEY-N-TEISYUTUSAKI  PIC 9(01).
               05  KEY-N-SRYKA         PIC X(02).
           03  KEY-OLD.
      *****    05  KEY-O-TEISYUTUSAKI  PIC 9(01).
               05  KEY-O-SRYKA         PIC X(02).      
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU         PIC 9(06).
           03  SUM-TOTALTEN         PIC 9(10).
      *
      *    一時領域
      *    パラメタエリア
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI   PIC X(01).
           05  WRK-PARA-NYUGAIKBN      PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
      *
           03  WRK-RECEERR             PIC X(200). 
      *    マルチボリューム識別情報     
           03  WRK-VOLNUM              PIC 9(06).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).        
      *    請求年月（和暦）
           03  WRK-SEIYM               PIC 9(05).         
      *    診療年月（和暦）
           03  WRK-SRYYM               PIC 9(05). 
      *
           03  WRK-MOJISU              PIC 9(02).        
      *       
           03  WRK-PATH.
               05  WRK-MCP-BLOCKS      PIC S9(9)   BINARY.
               05  WRK-MCP-RNAME       PIC S9(9)   BINARY.
               05  WRK-MCP-PNAME       PIC S9(9)   BINARY.
      *     右ずめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-REC                 PIC X(200).
           03  WRK-REC-LENGTH          PIC 9(03).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(80).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(02).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  FILLER              PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
     *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報（基本）
           COPY   "CPSK1001.INC".
      *
      *    医療機関情報（住所）
           COPY   "CPSK1002.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
           COPY    "CPORCMCP.INC".
           COPY    "ORCA-DBPATH".
      *
           COPY    "MCPAREA".
      *
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
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
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
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
           MOVE    "ORCBM500"      TO  JOB-PGID
           EVALUATE    WRK-PARA-TEISYUTUSAKI
                           ALSO    WRK-PARA-NYUGAIKBN   
               WHEN    "1" ALSO    ZERO
                   MOVE    "レセ電ファイル作成（社保）"
                                           TO  JOB-SHELLMSG
               WHEN    "1" ALSO    "1"
                   MOVE    "レセ電ファイル作成（社保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "1" ALSO    "2"
                   MOVE    "レセ電ファイル作成（社保・入院外）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    ZERO
                   MOVE    "レセ電ファイル作成（国保）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "1"
                   MOVE    "レセ電ファイル作成（国保・入院）"
                                           TO  JOB-SHELLMSG
               WHEN    "2" ALSO    "2"
                   MOVE    "レセ電ファイル作成（国保・入院外）"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 120-SKYYM-HENSYU-SEC
      *     
           MOVE    "RECE300"           TO  RECE30PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECE30PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *
           OPEN    OUTPUT             RECE30-FILE  
      *
      *
           EVALUATE    WRK-PARA-TEISYUTUSAKI
                           ALSO    WRK-PARA-NYUGAIKBN   
               WHEN    "1" ALSO    ZERO
                   MOVE    PATH-TBL-SEIKYU-KEY23   TO  WRK-PATH
               WHEN    "1" ALSO    ANY
                   MOVE    PATH-TBL-SEIKYU-KEY25   TO  WRK-PATH
               WHEN    "2" ALSO    ZERO
                   MOVE    PATH-TBL-SEIKYU-KEY24   TO  WRK-PATH
               WHEN    "2" ALSO    ANY
                   MOVE    PATH-TBL-SEIKYU-KEY26   TO  WRK-PATH
           END-EVALUATE
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
           MOVE    LNK-PRTKANRI-SKYYMD TO  ORC-DBYMD
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
           MOVE    LNK-PRTKANRI-SKYYMD TO  ORC-DBYMD
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
           MOVE    ZERO            TO  WRK-VOLNUM
                                       WRK-RECENUM
                                       SUM-AREA
      *     
      *    医療機関情報レコード
           PERFORM     2001-HENSYU-SEC
      *
      *    診療科別処理
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *        
               PERFORM 900-RECEDEN-SELECT-SEC
               PERFORM     UNTIL   FLG-RECEDEN =   1
                   PERFORM 2002-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               MOVE    WRK-PATH            TO  MCP-PATH
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
      *        合計書レコード
          PERFORM     2008-HENSYU-SEC
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    医療機関情報レコード編集処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "IR,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *     
           MOVE    WRK-PARA-TEISYUTUSAKI
                                       TO  WRK-DATA
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
           IF      KEY-N-SRYKA         =   ZERO
              MOVE    SPACE                TO  WRK-DATA
           ELSE    
               MOVE    RECEDEN-RECEKA      TO  WRK-DATA
           END-IF
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *            
           MOVE    SYS-1001-HOSPNAME   TO  WRK-DATA
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
           MOVE    SYS-1002-TEL        TO  WRK-DATA
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
      *****IF      RECEDEN-SRYYM       <=  "200403"
               MOVE    ZERO             TO  WRK-MOJISU
                                            WRK-NAGASA
               EVALUATE    RECEDEN-RECEDATA (1:2)
                   WHEN    "RE"
                       INSPECT RECEDEN-RECEDATA
                                               TALLYING    WRK-MOJISU
                                               FOR ALL "," 
                       IF      WRK-MOJISU          =   16
                           PERFORM VARYING IDX FROM    250 BY  -1
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
                   WHEN    "TO"
                       INSPECT RECEDEN-RECEDATA
                                               TALLYING    WRK-MOJISU
                                               FOR ALL "," 
                       IF      WRK-MOJISU          =   9
                           PERFORM VARYING IDX FROM    250 BY  -1
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
                   WHEN    "SY"
                       IF      RECEDEN-RECEDATA (4:7)
                                                   =   "0000998"
                           MOVE    "0000999"   TO
                                               RECEDEN-RECEDATA (4:7)
                       END-IF
               END-EVALUATE
      *****END-IF
      *          
           MOVE    RECEDEN-RECEDATA    TO  RECE30-R
           WRITE   RECE30-R
      *
           ADD     1                   TO  CNT-OUT
      *
           PERFORM 900-RECEDEN-READ-SEC     
           .
       2002-HENSYU-EXT.
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
           MOVE    SUM-TOTALKENSU      TO  WRK-NUM
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
           MOVE    SUM-TOTALTEN        TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
           MOVE    "99"                TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2008-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC      
      *
           CLOSE   RECE30-FILE
      *
           IF      CNT-RECEDEN         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
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
           DISPLAY "RECE300  CNT " CNT-IN  
           DISPLAY "RECE300  CNT " CNT-OUT  
           DISPLAY "*** ORCBM500 END ***"
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
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
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
           ADD     1                   TO  CNT-OUT
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
      *    右ずめ編集処理
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
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  RECE10-NYUGAIKBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC      
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  MCP-PATH
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC      
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
               ADD     1                   TO  CNT-RECE10
               MOVE    RECE10-RECEKA       TO  KEY-N-SRYKA
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN
      *     
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                  RECEDEN-REC
           MOVE    RECE10-KEY          TO  RECEDEN-KEY
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "RECEDEN-KEY2"      TO  ORC-DBPATH
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
