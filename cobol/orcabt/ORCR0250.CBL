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
       PROGRAM-ID.             ORCR0250.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプト電算ファイル作成
      *                      レセプト明細書の並び替え処理 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/28    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    05/01/07  差し込みコメントを対象とする
      *  02.07.02    NACL-藤原    05/12/02  診療区分９９は０１に変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
      *
           SELECT  RECE64-FILE     ASSIGN  RECE64PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE64-KEY
                                   FILE    STATUS  IS  STS-RECE64.
      *
           SELECT  RECE641-FILE    ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE641-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *
           SELECT  RECE64W-FILE     ASSIGN  RECE641PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE64W-KEY
                                   FILE    STATUS  IS  STS-RECE641.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE64-FILE.
       01  RECE64-REC. 
           COPY    "CPRCF064.INC".
      *
       FD  RECE641-FILE.
       01  RECE641-REC. 
           COPY    "CPRCF0641.INC".
      *
       FD  RECE64W-FILE.
       01  RECE64W-REC. 
           COPY    "CPRCF0641.INC"  REPLACING  //RECE641//
                                   BY          //RECE64W//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE64//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE641//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03                      PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE641                             PIC X(02).
           03  STS-RECE64                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-READ                                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(06).
           03  IDY                     PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
      *
           03  WRK-RECEERR             PIC X(200). 
           03  WRK-KOHBUNRI            PIC 9(01).
           03  WRK-HKNCOMBI            PIC 9(04).
           03  OLD-KEY                 PIC X(65).
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
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
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
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
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
           MOVE    "レセ電用データ並び変え"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCR0250"      TO  JOB-PGID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE641"           TO  RECE64PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE64PARA-TERMID
      *
           MOVE    "RECE649"           TO  RECE641PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE641PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE64W-FILE
           CLOSE                       RECE64W-FILE
      *
           OPEN    INPUT               RECE64-FILE
      *
           OPEN    I-O                 RECE641-FILE
      *
           PERFORM 900-RECE64-READ-SEC
           MOVE    RECE64-KEY(1:65)    TO  OLD-KEY
           MOVE  9                     TO  WRK-KOHBUNRI
           MOVE  ZERO                  TO  WRK-HKNCOMBI
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      RECE641-REC
           MOVE    RECE64-HOSPID       TO  RECE641-HOSPID
           MOVE    RECE64-SRYYM        TO  RECE641-SRYYM
           MOVE    RECE64-NYUGAIKBN    TO  RECE641-NYUGAIKBN
           MOVE    RECE64-PTID         TO  RECE641-PTID
           MOVE    RECE64-RECEKA       TO  RECE641-RECEKA
           MOVE    RECE64-TEISYUTUSAKI TO  RECE641-TEISYUTUSAKI
           MOVE    RECE64-RECESYUBETU  TO  RECE641-RECESYUBETU
           MOVE    RECE64-HKNJANUM     TO  RECE641-HKNJANUM
           MOVE    RECE64-HOJOKBN      TO  RECE641-HOJOKBN
           MOVE    RECE64-TEKSTYMD     TO  RECE641-TEKSTYMD
           MOVE    RECE64-ZAINUM       TO  RECE641-ZAINUM
           MOVE    RECE64-KOHBUNRI     TO  RECE641-KOHBUNRI
           IF      RECE64-KEY(1:65)    NOT =   OLD-KEY
                MOVE  9                TO  WRK-KOHBUNRI
                MOVE  ZERO             TO  WRK-HKNCOMBI
                MOVE  RECE64-KEY(1:65) TO  OLD-KEY
           END-IF
           IF      RECE64-KOHBUNRI     <   WRK-KOHBUNRI
                MOVE   RECE64-KOHBUNRI TO  WRK-KOHBUNRI
                MOVE   RECE64-HKNCOMBI TO  WRK-HKNCOMBI
           END-IF
           IF      RECE64-SRYKBN   =   "99"
               MOVE    "01"            TO  RECE641-SRYKBN
               IF  RECE64-HKNCOMBI     =   ZERO
                   MOVE    WRK-HKNCOMBI    TO  RECE64-HKNCOMBI
               END-IF
           ELSE
               MOVE    RECE64-SRYKBN       TO  RECE641-SRYKBN
               IF      RECE64-SRYKBN   >=  "90"
                   IF  RECE64-HKNCOMBI     =   ZERO
                       MOVE    WRK-HKNCOMBI    TO  RECE64-HKNCOMBI
                   END-IF
               END-IF
           END-IF
           MOVE    RECE64-PTNUM        TO  RECE641-PTNUM
           MOVE    RECE64-SRYKA        TO  RECE641-SRYKA
           MOVE    RECE64-HKNCOMBI     TO  RECE641-HKNCOMBI
           MOVE    RECE64-JIHOKBN      TO  RECE641-JIHOKBN
           MOVE    RECE64-ZAITEN       TO  RECE641-ZAITEN
           MOVE    RECE64-SRYCDTOTAL   TO  RECE641-SRYCDTOTAL
           MOVE    RECE64-SURYOUTOTAL  TO  RECE641-SURYOUTOTAL
           MOVE    RECE64-MEISAISU     TO  RECE641-MEISAISU
           MOVE    RECE64-ZAIKAISU     TO  RECE641-ZAIKAISU
           MOVE    RECE64-SYUTEN1      TO  RECE641-SYUTEN1
           MOVE    RECE64-SYUTEN2      TO  RECE641-SYUTEN2
           MOVE    RECE64-YKZTEN       TO  RECE641-YKZTEN
           MOVE    RECE64-KIZAITEN     TO  RECE641-KIZAITEN
           MOVE    RECE64-DRCD         TO  RECE641-DRCD
           MOVE    RECE64-ZAIREQFLG    TO  RECE641-ZAIREQFLG
           MOVE    RECE64-TEIGENRATE   TO  RECE641-TEIGENRATE
           MOVE    RECE64-DAY-AREA     TO  RECE641-DAY-AREA
      *
           WRITE   RECE641-REC
           ADD     1                   TO  CNT-OUT
      *
      *
           PERFORM 900-RECE64-READ-SEC      
           .
       200-MAIN-EXT.
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
           CLOSE   RECE64-FILE
           CLOSE   RECE641-FILE
      *
           DISPLAY "RECE64   IN  " CNT-IN
           DISPLAY "RECE641  CNT " CNT-OUT
           DISPLAY "*** ORCR0250 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書６２読込
      *****************************************************************
       900-RECE64-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1            
               READ    RECE64-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                   NOT AT  END
                       IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
                                                   OR  "1" )   AND
                             ( RECE64-TEISYUTUSAKI
                                                   =   "1"  )      )
                       OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
                                                   OR  "1"  )  AND
                             ( RECE64-TEISYUTUSAKI
                                                   =   "2"  )      )
                       OR  (   RECE64-RECESYUBETU  =   ZERO   OR 
                                                       8888        )
      ****             OR  ( RECE64-SRYKBN         >=  "90"   AND 
      ****                                         <   "99"        )
                       OR  ( RECE64-SRYKBN         >=  "95"   AND 
                                                   <   "99"        )
                       OR  ( ( RECE64-SRYKBN = "90" OR "92" )  AND 
                             ( RECE64-ZAITEN  NOT =  ZERO   )      )
                               CONTINUE
                       ELSE
                           MOVE    1           TO  FLG-READ
                       END-IF      
      *
                       IF      FLG-READ            =   1         
                           ADD     1                   TO  CNT-IN
                           MOVE    ZERO                TO  FLG-END
                       END-IF
               END-READ
           END-PERFORM
           .
       900-RECE64-READ-EXT.
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
