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
       PROGRAM-ID.             ORCR0425.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 旧総合病院以外のとき科別の同一剤のまとめをする
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/01/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.13  剤点数をキーに追加対応
      *  01.00.02    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.02    NACL-門脇    04.07.12  ファイルクリア追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE442-FILE    ASSIGN  RECE442PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE442-KEY
                                   FILE    STATUS  IS  STS-RECE442.
           SELECT  RECE442X-FILE   ASSIGN  RECE442PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE442X-KEY
                                   FILE    STATUS  IS  STS-RECE442.
           SELECT  RECE44W-FILE    ASSIGN  RECE44WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44W-KEY
                                   FILE    STATUS  IS  STS-RECE44W.
           SELECT  RECE44WX-FILE   ASSIGN  RECE44WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44WX-KEY
                                   FILE    STATUS  IS  STS-RECE44W.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC. 
           COPY    "CPRCF044.INC".
      *
       FD  RECE442-FILE.
       01  RECE442-REC. 
           COPY    "CPRCF0442.INC".
      *
       FD  RECE442X-FILE.
       01  RECE442X-REC. 
           COPY    "CPRCF0442.INC" REPLACING  //RECE442//
                                   BY         //RECE442X//.
      *
       FD  RECE44W-FILE.
       01  RECE44W-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44W//.
      *
       FD  RECE44WX-FILE.
       01  RECE44WX-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE44WX//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE41//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE44//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE442//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE44W//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41                              PIC X(02).
           03  STS-RECE44                              PIC X(02).
           03  STS-RECE442                             PIC X(02).
           03  STS-RECE44W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE41                              PIC 9(01).
           03  FLG-RECE44                              PIC 9(01).
           03  FLG-RECE442                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-RECE442                             PIC 9(06).
           03  CNT-RECE44W                             PIC 9(06). 
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
       01  WRK-RECE442-REC.
           COPY    "CPRCF0442.INC" REPLACING  //RECE442//
                                   BY         //WRK-RECE442//.
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
           MOVE    "ORCR0425"      TO  JOB-PGID
           MOVE    "同一剤のまとめ（労災・自賠責）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
      *
           MOVE    "RECE440"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
      *
           MOVE    "RECE442"           TO  RECE442PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE442PARA-TERMID
      *
           MOVE    "RECE44W"           TO  RECE44WPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44WPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE442X-FILE
           CLOSE                       RECE442X-FILE
      *
           OPEN    OUTPUT              RECE44WX-FILE
           CLOSE                       RECE44WX-FILE
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
           OPEN    INPUT               RECE41-FILE
                                       RECE44-FILE
           OPEN    I-O                 RECE442-FILE
           PERFORM 900-RECE41-READ-SEC
      *
           PERFORM 210-RECE442-WRITE-SEC
                                   UNTIL   FLG-RECE41  =   1
           CLOSE   RECE442-FILE
                   RECE44-FILE
                   RECE41-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE442-FILE
                   OUTPUT  RECE44W-FILE
      *
           PERFORM 900-RECE442-READ-SEC 
           PERFORM 220-RECE44W-WRITE-SEC
                                   UNTIL   FLG-RECE442 =   1
           CLOSE   RECE44W-FILE
                   RECE442-FILE
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
       210-RECE442-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPID       TO  RECE44-HOSPID
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 900-RECE44-READ-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPID    NOT =   RECE44-HOSPID       )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
      *        複数診療科受診でないとき
      ***      IF      RECE41-SRYKA (2)    =   SPACE
      ***      OR      RECE44-COMCDFLG     =   "1"
               IF      RECE44-COMCDFLG     =   "1"
                   MOVE    SPACE               TO  RECE442-REC
                   INITIALIZE                      RECE442-REC
                   MOVE    RECE44-HOSPID       TO  RECE442-HOSPID
                   MOVE    RECE44-SRYYM        TO  RECE442-SRYYM
                   MOVE    RECE44-NYUGAIKBN    TO  RECE442-NYUGAIKBN
                   MOVE    RECE44-PTID         TO  RECE442-PTID
                   MOVE    RECE44-HKNID        TO  RECE442-HKNID
                   MOVE    RECE44-TEKSTYMD     TO  RECE442-TEKSTYMD
                   MOVE    RECE44-SRYKBN       TO  RECE442-SRYKBN
                   MOVE    RECE44-ZAITEN       TO  RECE442-ZAITEN
                   MOVE    RECE44-SRYCDTOTAL   TO  RECE442-SRYCDTOTAL
                   MOVE    RECE44-SURYOUTOTAL  TO  RECE442-SURYOUTOTAL
                   MOVE    RECE44-RUIKEITOTAL  TO  RECE442-RUIKEITOTAL
                   MOVE    RECE44-MEISAISU     TO  RECE442-MEISAISU 
                   IF   RECE44-COMCDFLG     =   "1"
                        ADD       1            TO  WRK-COMRENNUM
                        MOVE    WRK-COMRENNUM  TO  RECE442-COMRENNUM
                   END-IF
                   MOVE    RECE44-FLG-HOUTEN-COMMENT
                                      TO  RECE442-FLG-HOUTEN-COMMENT
                   MOVE    RECE44-ZAIKAISU     TO  RECE442-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           MOVE    RECE44-KAISU (IDX IDY)
                                           TO   RECE442-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
                   MOVE    RECE44-REC          TO  RECE442-REC1
                   WRITE   RECE442-REC
                   ADD     1                   TO  CNT-RECE442
               ELSE
      *
      *        複数診療科受診のとき
      *            既に同一剤があるか
                   MOVE    SPACE               TO  RECE442-REC
                   INITIALIZE                      RECE442-REC
                   MOVE    RECE44-HOSPID       TO  RECE442-HOSPID
                   MOVE    RECE44-SRYYM        TO  RECE442-SRYYM
                   MOVE    RECE44-NYUGAIKBN    TO  RECE442-NYUGAIKBN
                   MOVE    RECE44-PTID         TO  RECE442-PTID
                   MOVE    RECE44-HKNID        TO  RECE442-HKNID
                   MOVE    RECE44-TEKSTYMD     TO  RECE442-TEKSTYMD
                   MOVE    RECE44-SRYKBN       TO  RECE442-SRYKBN
                   MOVE    RECE44-ZAITEN       TO  RECE442-ZAITEN
                   MOVE    RECE44-SRYCDTOTAL   TO  RECE442-SRYCDTOTAL
                   MOVE    RECE44-SURYOUTOTAL  TO  RECE442-SURYOUTOTAL
                   MOVE    RECE44-RUIKEITOTAL  TO  RECE442-RUIKEITOTAL
                   MOVE    RECE44-MEISAISU     TO  RECE442-MEISAISU 
                   MOVE    RECE44-FLG-HOUTEN-COMMENT
                                      TO  RECE442-FLG-HOUTEN-COMMENT
                   MOVE    RECE442-REC         TO  WRK-RECE442-REC
                   PERFORM 900-RECE442-INV-SEC
                   IF      FLG-RECE442         =   1
                       MOVE    WRK-RECE442-REC     TO  RECE442-REC
                       MOVE    RECE44-REC          TO  RECE442-REC1
                   END-IF
      *            剤回数・日数回数テーブルの加算
                   ADD     RECE44-ZAIKAISU     TO  RECE442-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           ADD     RECE44-KAISU (IDX IDY)
                                           TO   RECE442-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
      *     
                   IF      FLG-RECE442         =   1
                       WRITE   RECE442-REC
                       ADD     1                   TO  CNT-RECE442
                   ELSE
                       REWRITE RECE442-REC
                   END-IF
               END-IF
      *
               PERFORM     900-RECE44-READ-SEC
           END-PERFORM
      *
           PERFORM 900-RECE41-READ-SEC
           .
       210-RECE442-WRITE-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       220-RECE44W-WRITE-SEC                SECTION.
      *
           MOVE    RECE442-REC1        TO  RECE44W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE442-ZAIKAISU     TO  RECE44W-ZAIKAISU
           MOVE    RECE442-DAY-AREA     TO  RECE44W-DAY-AREA
      *
           WRITE   RECE44W-REC
           ADD     1                   TO  CNT-RECE44W
      *
           PERFORM     900-RECE442-READ-SEC
           .
       220-RECE44W-WRITE-EXT.
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
           DISPLAY "RECE44     :" CNT-IN   
           DISPLAY "RECE442    :" CNT-RECE442
           DISPLAY "RECE44W    :" CNT-RECE44W
           DISPLAY "*** ORCR0425 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ読込処理
      *****************************************************************
       900-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE41
               NOT AT  END
                   ADD     ZERO        TO  FLG-RECE41
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
           .
       900-RECE41-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   レセプト０４データ読込処理
      *****************************************************************
       900-RECE44-READ-SEC            SECTION.
      *
           READ    RECE44-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE44
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE44
                   ADD     1             TO    CNT-IN
           END-READ
           .
       900-RECE44-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE442-READ-SEC            SECTION.
      *
           READ    RECE442-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE442
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE442
           END-READ
           .
       900-RECE442-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE442-INV-SEC            SECTION.
      *
           READ    RECE442-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE442
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE442
           END-READ
           .
       900-RECE442-READ-EXT.
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
