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
       PROGRAM-ID.             ORCR0860.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 科別の同一剤のまとめをする（金額摘要欄）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  01.00.02    NACL-門脇    04.07.12  ファイルクリア追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE84-FILE     ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE843-FILE    ASSIGN  RECE843PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE843-KEY
                                   FILE    STATUS  IS  STS-RECE843.
           SELECT  RECE843X-FILE   ASSIGN  RECE843PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE843X-KEY
                                   FILE    STATUS  IS  STS-RECE843.
           SELECT  RECE84X-FILE    ASSIGN  RECE84XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84X-KEY
                                   FILE    STATUS  IS  STS-RECE84X.
           SELECT  RECE84XX-FILE   ASSIGN  RECE84XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE84XX-KEY
                                   FILE    STATUS  IS  STS-RECE84X.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE84-FILE.
       01  RECE84-REC. 
           COPY    "CPRCF084.INC".
      *
       FD  RECE843-FILE.
       01  RECE843-REC. 
           COPY    "CPRCF0842.INC" REPLACING  //RECE842//
                                   BY         //RECE843//.
      *
       FD  RECE843X-FILE.
       01  RECE843X-REC. 
           COPY    "CPRCF0842.INC" REPLACING  //RECE842//
                                   BY         //RECE843X//.
      *
       FD  RECE84X-FILE.
       01  RECE84X-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84X//.
      *
       FD  RECE84XX-FILE.
       01  RECE84XX-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84XX//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE82//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE84//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE843//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE84X//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82                              PIC X(02).
           03  STS-RECE84                              PIC X(02).
           03  STS-RECE843                             PIC X(02).
           03  STS-RECE84X                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE82                              PIC 9(01).
           03  FLG-RECE84                              PIC 9(01).
           03  FLG-RECE843                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-RECE843                             PIC 9(06).
           03  CNT-RECE84X                             PIC 9(06). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-COMRENNUM           PIC 9(05).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
       01  WRK-RECE843-REC.
           COPY    "CPRCF0842.INC" REPLACING  //RECE842//
                                   BY         //WRK-RECE843//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
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
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0860"      TO  JOB-PGID
           MOVE    "同一剤のまとめ２（労災・自賠責）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
      *
           MOVE    "RECE841"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
      *
           MOVE    "RECE843"           TO  RECE843PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE843PARA-TERMID
      *
           MOVE    "RECE84X"           TO  RECE84XPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84XPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE843X-FILE
           CLOSE                       RECE843X-FILE
      *
           OPEN    OUTPUT              RECE84XX-FILE
           CLOSE                       RECE84XX-FILE
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    同一剤のまとめ処理
           OPEN    INPUT               RECE82-FILE
                                       RECE84-FILE
           OPEN    I-O                 RECE843-FILE
           PERFORM 900-RECE82-READ-SEC
      *
           PERFORM 210-RECE843-WRITE-SEC
                                   UNTIL   FLG-RECE82  =   1
           CLOSE   RECE843-FILE
                   RECE84-FILE
                   RECE82-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE843-FILE
                   OUTPUT  RECE84X-FILE
      *
           PERFORM 900-RECE843-READ-SEC 
           PERFORM 220-RECE84X-WRITE-SEC
                                   UNTIL   FLG-RECE843 =   1
           CLOSE   RECE84X-FILE
                   RECE843-FILE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    同一剤のまとめ処理
      *****************************************************************
       210-RECE843-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE84-REC
           MOVE    RECE82-HOSPID       TO  RECE84-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE84-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE84-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE84-PTID
           MOVE    RECE82-RECEKA       TO  RECE84-RECEKA
           MOVE    RECE82-HKNID        TO  RECE84-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE84-TEKSTYMD
           MOVE    ZERO                TO  RECE84-ZAINUM
      *
           START   RECE84-FILE
               KEY IS    >=   RECE84-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE84
               NOT INVALID
                   PERFORM 900-RECE84-READ-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE84           =   1                   )
               OR     ( RECE82-HOSPID    NOT =   RECE84-HOSPID       )
               OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
               IF      RECE84-COMCDFLG     =   "2"
                   MOVE    SPACE               TO  RECE843-REC
                   INITIALIZE                      RECE843-REC
                   MOVE    RECE84-HOSPID       TO  RECE843-HOSPID
                   MOVE    RECE84-SRYYM        TO  RECE843-SRYYM
                   MOVE    RECE84-NYUGAIKBN    TO  RECE843-NYUGAIKBN
                   MOVE    RECE84-PTID         TO  RECE843-PTID
                   MOVE    RECE84-HKNID        TO  RECE843-HKNID
                   MOVE    RECE84-TEKSTYMD     TO  RECE843-TEKSTYMD
                   MOVE    RECE84-SRYKBN       TO  RECE843-SRYKBN
                   MOVE    RECE84-ZAITEN       TO  RECE843-ZAITEN
                   MOVE    RECE84-SRYCDTOTAL   TO  RECE843-SRYCDTOTAL
                   MOVE    RECE84-SURYOUTOTAL  TO  RECE843-SURYOUTOTAL
                   MOVE    RECE84-RUIKEITOTAL  TO  RECE843-RUIKEITOTAL
                   MOVE    RECE84-MEISAISU     TO  RECE843-MEISAISU 
                   ADD     1                   TO  WRK-COMRENNUM
                   MOVE    WRK-COMRENNUM       TO  RECE843-COMRENNUM
                   MOVE    RECE84-ZAIKAISU     TO  RECE843-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           MOVE    RECE84-KAISU (IDX IDY)
                                           TO   RECE843-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
                   MOVE    RECE84-REC          TO  RECE843-REC1
                   WRITE   RECE843-REC
                   ADD     1                   TO  CNT-RECE843
               ELSE
      *
      *        複数診療科受診のとき
      *            既に同一剤があるか
                   MOVE    SPACE               TO  RECE843-REC
                   INITIALIZE                      RECE843-REC
                   MOVE    RECE84-HOSPID       TO  RECE843-HOSPID
                   MOVE    RECE84-SRYYM        TO  RECE843-SRYYM
                   MOVE    RECE84-NYUGAIKBN    TO  RECE843-NYUGAIKBN
                   MOVE    RECE84-PTID         TO  RECE843-PTID
                   MOVE    RECE84-HKNID        TO  RECE843-HKNID
                   MOVE    RECE84-TEKSTYMD     TO  RECE843-TEKSTYMD
                   MOVE    RECE84-SRYKBN       TO  RECE843-SRYKBN
                   MOVE    RECE84-ZAITEN       TO  RECE843-ZAITEN
                   MOVE    RECE84-SRYCDTOTAL   TO  RECE843-SRYCDTOTAL
                   MOVE    RECE84-SURYOUTOTAL  TO  RECE843-SURYOUTOTAL
                   MOVE    RECE84-RUIKEITOTAL  TO  RECE843-RUIKEITOTAL
                   MOVE    RECE84-MEISAISU     TO  RECE843-MEISAISU 
                   MOVE    RECE843-REC         TO  WRK-RECE843-REC
                   PERFORM 900-RECE843-INV-SEC
                   IF      FLG-RECE843         =   1
                       MOVE    WRK-RECE843-REC     TO  RECE843-REC
                       MOVE    RECE84-REC          TO  RECE843-REC1
                   END-IF
      *            剤回数・日数回数テーブルの加算
                   ADD     RECE84-ZAIKAISU     TO  RECE843-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           ADD     RECE84-KAISU (IDX IDY)
                                           TO   RECE843-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
      *     
                   IF      FLG-RECE843         =   1
                       WRITE   RECE843-REC
                       ADD     1                   TO  CNT-RECE843
                   ELSE
                       REWRITE RECE843-REC
                   END-IF
               END-IF
      *
               PERFORM 900-RECE84-READ-SEC
           END-PERFORM
      *
           PERFORM 900-RECE82-READ-SEC
           .
       210-RECE843-WRITE-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       220-RECE84X-WRITE-SEC                SECTION.
      *
           MOVE    RECE843-REC1        TO  RECE84X-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE843-ZAIKAISU    TO  RECE84X-ZAIKAISU
           MOVE    RECE843-DAY-AREA    TO  RECE84X-DAY-AREA
      *
           WRITE   RECE84X-REC
           ADD     1                   TO  CNT-RECE84X
      *
           PERFORM 900-RECE843-READ-SEC
           .
       220-RECE84X-WRITE-EXT.
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
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
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
           DISPLAY "RECE84     :" CNT-IN   
           DISPLAY "RECE843    :" CNT-RECE843
           DISPLAY "RECE84X    :" CNT-RECE84X
           DISPLAY "*** ORCR0860 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ読込処理
      *****************************************************************
       900-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE82
               NOT AT  END
                   ADD     ZERO        TO  FLG-RECE82
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   レセプト０４データ読込処理
      *****************************************************************
       900-RECE84-READ-SEC            SECTION.
      *
           READ    RECE84-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE84
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE84
                   ADD     1             TO    CNT-IN
           END-READ
           .
       900-RECE84-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE843-READ-SEC            SECTION.
      *
           READ    RECE843-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE843
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE843
           END-READ
           .
       900-RECE843-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE843-INV-SEC            SECTION.
      *
           READ    RECE843-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE843
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE843
           END-READ
           .
       900-RECE843-READ-EXT.
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
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
