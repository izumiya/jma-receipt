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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBONPRT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : オンライン印刷管理
      *  コンポーネント名  : オンライン帳票ＤＢの格納
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/25    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  PRTOUTPARA
      *                            ORGANIZATION    IS  LINE
      *                                                SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN              PIC 9(04).
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-OUT             PIC 9(04).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(03). 
      *
      *
       01  PRTOUTPARA.
           03  FILLER                  PIC  X(09) VALUE "/var/tmp/".
           03  PRTOUTPARA-FORM-ID      PIC  X(04).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-UUID         PIC  X(31).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-SYOHMS       PIC  9(08).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  PRTOUTPARA-CNT          PIC  9(06).
           03  FILLER                  PIC  X(04) VALUE ".dat".
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-PATH                PIC X(64). 
      *
           03  WRK-PARA.
               05  WRK-PARA1.  
                   07  WRK-PARA-SYSYMD           PIC X(08).
                   07  WRK-PARA-SRYYMD           PIC X(08).
                   07  WRK-PARA-PRT-UUID         PIC X(31).
                   07  WRK-PARA-PTID             PIC 9(10).
                   07  WRK-PARA-SITE-KBN         PIC X(01).
                   07  WRK-PARA-GYOMU-NAME       PIC X(40).
                   07  WRK-PARA-TITLE0           PIC X(40).
                   07  WRK-PARA-FORMID           PIC X(20).
                   07  WRK-PARA-SYOHMS           PIC X(06).
                   07  WRK-PARA-PARA-CNT         PIC X(05).
                   07  WRK-PARA-DATASETNAME      PIC X(100).
                   07  WRK-PARA-TITLE            PIC X(40).
                   07  WRK-PARA-SRYYM            PIC X(06).
                   07  WRK-PARA-TERMID           PIC X(16).
                   07  WRK-PARA-HOSPNUM          PIC 9(02).
                   07  WRK-PARA-PRTFLG           PIC X(01).
      *
           COPY    "CPORCSPRTLNK.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *    COPY    "ORCA-DBPATH".         
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO  LNK-PRTKANRI-RENNUM
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
                                             WRK-PARA-SRYYMD
                                             LNK-PRTKANRI-TBL-UUID
                                             WRK-PARA-PTID
                                             WRK-PARA-SITE-KBN
                                             LNK-PRTKANRI-GYOUMU-CD
                                             WRK-PARA-TITLE0
                                             WRK-PARA-SYOHMS
                                             WRK-PARA-FORMID
                                             WRK-PARA-PARA-CNT
                                             WRK-PARA-DATASETNAME
                                             WRK-PARA-HOSPNUM
                                             WRK-PARA-PRTFLG
           END-UNSTRING                                  
      *
           MOVE    WRK-PARA-HOSPNUM    TO    SPA-HOSPNUM 
      *
           PERFORM 100-DBOPEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票印刷処理
           MOVE    WRK-PARA-DATASETNAME
                                       TO  PRTOUTPARA
           OPEN    INPUT               PRT-FILE
           MOVE    SPACE               TO  PRT-PRTDATA
           IF        ( STS-PRT         =   ZERO )
               DISPLAY "INPUT FILE OPEN OK"
               READ PRT-FILE
               AT  END
                   DISPLAY "INPUT NO DATA"
               END-READ
           ELSE
               DISPLAY "INPUT FILE OPEN ERR"
           END-IF
      *     
           PERFORM 390-PRINT-OUT-SEC
      *
           CLOSE                       PRT-FILE
           DISPLAY "FILE = "    WRK-PARA-DATASETNAME
           MOVE    WRK-PARA-DATASETNAME
                                       TO  PRTOUTPARA
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *     
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-TBL-UUID
                                       TO  SPRT-TBL-UUID
           MOVE    LNK-PRTKANRI-GYOUMU-CD
                                       TO  SPRT-GYOUMU-CD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    WRK-PARA-SRYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    WRK-PARA-FORMID     TO  SPRT-PRTID
           MOVE    PRT-PRTDATA         TO  SPRT-PRTDATA
           MOVE    WRK-PARA-TITLE0     TO  SPRT-TITLE
      *         
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    WRK-PARA-SITE-KBN   TO  SPRT-SITEKBN
           IF    ( WRK-PARA-PRTFLG     =   "D" )
               MOVE    "D"             TO  SPRT-PRTFLG
           ELSE
               MOVE    "1"             TO  SPRT-PRTFLG
           END-IF
      *    
           MOVE    WRK-PARA-PTID       TO  SPRT-PTID
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
      *    IF      SPRT-RETURN         =   ZERO
      *        CONTINUE
      *    ELSE
      *        MOVE    "印刷ＤＢに更新できませんでした"
      *                                   TO  WRK-RECEERR
      *        PERFORM 500-ERR-HENSYU-SEC                           
      *    END-IF                                                              
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORC IN  "  CNT-IN
           DISPLAY "*** ORC OUT "  CNT-OUT
           DISPLAY "*** ORCBONPRT END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
