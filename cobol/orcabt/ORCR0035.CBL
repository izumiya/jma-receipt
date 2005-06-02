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
       PROGRAM-ID.             ORCR0035.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.01.02    MCC-藤原     02.02.13  保険組合せを３から１０に変更
      *  01.00.03    NACL-藤原    03.03.12  補助区分の追加
      *  01.00.04    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.05    NACL-門脇    04.03.17  ＜愛媛県地方公費対応＞
      *                                     合計点数情報（ぶん点・公費分離
      *                                     区分編集用）を使用するよう修正
      *  01.00.06    NACL-門脇    04.08.04  同一保険番号対応
      *  01.00.07    NACL-門脇    05.04.20  内服薬剤逓減編集対応
      *  01.00.08    NACL-門脇    05/06/02  公費分離区分編集修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE04-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
       FD  RECE04-FILE.
       01  RECE04-REC. 
           COPY    "CPRCF004.INC".
      *
       FD  RECE05-FILE.
       01  RECE05-REC. 
           COPY    "CPRCF005.INC".
      * 
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE04//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE05//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02                              PIC X(02).
           03  STS-RECE04                              PIC X(02).
           03  STS-RECE05                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE04                              PIC 9(01).
           03  FLG-RECE05                              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  BUN-IDX                                 PIC 9(03).
      *
      *    剤番号の退避
       01  WK-ZAINUM                                   PIC 9(08).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-RECEERR             PIC X(200). 
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
           MOVE    "ORCR0035"      TO  JOB-PGID
           MOVE    "公費分点数判定"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE020"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
      *
           MOVE    "RECE040"           TO  RECE04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
      *
           MOVE    "RECE050"           TO  RECE05PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE05PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    INPUT               RECE02-FILE
           OPEN    I-O                 RECE04-FILE
                                       RECE05-FILE
      *
           PERFORM    RECE02-READ-SEC
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
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     RECE02-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           IF (RECE02-HENTOTALTEN(2)       =      ZERO)    AND
              (RECE02-HENTOTALTEN(3)       =      ZERO)
               CONTINUE
           ELSE
               IF (RECE02-HENTOTALTEN(2)   NOT =  ZERO)              AND
                  (RECE02-HENTOTALTEN(1)   NOT =  RECE02-HENTOTALTEN(2))
                   MOVE      1          TO     BUN-IDX
                   PERFORM   RECE04-HENSYU-SEC
               END-IF
               IF (RECE02-HENTOTALTEN(3)   NOT =  ZERO)              AND
                  (RECE02-HENTOTALTEN(1)   NOT =  RECE02-HENTOTALTEN(3))
                   MOVE      2          TO     BUN-IDX
                   PERFORM   RECE04-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００４）
      *****************************************************************
       RECE04-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE04-REC
           INITIALIZE                           RECE04-REC
           MOVE    RECE02-HOSPID        TO      RECE04-HOSPID
           MOVE    RECE02-SRYYM         TO      RECE04-SRYYM
           MOVE    RECE02-NYUGAIKBN     TO      RECE04-NYUGAIKBN
           MOVE    RECE02-PTID          TO      RECE04-PTID
           MOVE    RECE02-RECEKA        TO      RECE04-RECEKA
           MOVE    RECE02-TEISYUTUSAKI  TO      RECE04-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU   TO      RECE04-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY  TO      RECE04-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY   TO      RECE04-HOJOKBN
           MOVE    RECE02-TEKSTYMD      TO      RECE04-TEKSTYMD
           START   RECE04-FILE   KEY  IS  >=    RECE04-KEY
                   INVALID
                       MOVE   1         TO      FLG-RECE04
                   NOT  INVALID
                       MOVE   ZERO      TO      FLG-RECE04
                       PERFORM   RECE04-NEXT-SEC
           END-START
           PERFORM   UNTIL
                    (RECE02-HOSPID    NOT = RECE04-HOSPID)    OR
                    (RECE02-NYUGAIKBN NOT = RECE04-NYUGAIKBN) OR
                    (RECE02-PTID      NOT = RECE04-PTID)      OR
                    (RECE02-RECEKA    NOT = RECE04-RECEKA)    OR
                    (RECE02-TEISYUTUSAKI NOT = RECE04-TEISYUTUSAKI) OR
                    (RECE02-RECESYUBETU  NOT = RECE04-RECESYUBETU)  OR
                    (RECE02-HKNJANUM-KEY   NOT = RECE04-HKNJANUM)  OR
                    (RECE02-HOJOKBN-KEY    NOT = RECE04-HOJOKBN )  OR
                    (RECE02-TEKSTYMD  NOT = RECE04-TEKSTYMD)  OR
                    (FLG-RECE04       =    1)
                     MOVE   RECE04-ZAINUM    TO   WK-ZAINUM
                     PERFORM      KOHBUNRI-SET-SEC
                     PERFORM      RECE04-REWRITE-SEC
                     PERFORM      RECE05-HENSYU-SEC
                     PERFORM      RECE04-NEXT-SEC
           END-PERFORM
      *
           .
       RECE04-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費分離区分の算出
      *****************************************************************
       KOHBUNRI-SET-SEC               SECTION.
      *
      *****EVALUATE  TRUE
      *        WHEN  (RECE04-HKNCOMBI   =    RECE02-HKNCOMBI(1))
      *              MOVE     1         TO   HC-IDX
      *        WHEN  (RECE04-HKNCOMBI   =    RECE02-HKNCOMBI(2))
      *              MOVE     2         TO   HC-IDX
      *        WHEN  (RECE04-HKNCOMBI   =    RECE02-HKNCOMBI(3))
      *              MOVE     3         TO   HC-IDX
      *        WHEN  OTHER
      *              MOVE     ZERO      TO   HC-IDX
      *****END-EVALUATE
           MOVE    ZERO                TO  HC-IDX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE02-HKNCOMBI (IDX)   =   ZERO 
               IF      RECE04-HKNCOMBI     =   RECE02-HKNCOMBI (IDX)
                     MOVE     IDX              TO  HC-IDX
                     MOVE    10                TO  IDX
               END-IF
           END-PERFORM          
      *
           MOVE       ZERO              TO   KH-IDX
           IF ((RECE02-KOHNUMHC(HC-IDX 1) = RECE02-KOHNUM(BUN-IDX)) AND
               (RECE02-KOHIDHC(HC-IDX 1)  = RECE02-KOHID(BUN-IDX) ))
           OR ((RECE02-KOHNUMHC(HC-IDX 2) = RECE02-KOHNUM(BUN-IDX)) AND
               (RECE02-KOHIDHC(HC-IDX 2)  = RECE02-KOHID(BUN-IDX) ))
           OR ((RECE02-KOHNUMHC(HC-IDX 3) = RECE02-KOHNUM(BUN-IDX)) AND
               (RECE02-KOHIDHC(HC-IDX 3)  = RECE02-KOHID(BUN-IDX) ))
           OR ((RECE02-KOHNUMHC(HC-IDX 4) = RECE02-KOHNUM(BUN-IDX)) AND
               (RECE02-KOHIDHC(HC-IDX 4)  = RECE02-KOHID(BUN-IDX) ))
                MOVE    BUN-IDX     TO        KH-IDX
           END-IF
      **     DISPLAY   "RECE04-PTID     : " RECE04-PTID
      **     DISPLAY   "RECE04-HKNCOMNI : " RECE04-HKNCOMBI
      **     DISPLAY   "KH-IDX          : " KH-IDX
      *
           IF    RECE02-HENTOTALTEN(1)   NOT  =  ZERO
              IF    KH-IDX      NOT =   ZERO
                     MOVE    KH-IDX      TO     RECE04-KOHBUNRI
              END-IF
           ELSE
              IF    KH-IDX      NOT =   ZERO
                IF    RECE04-KOHBUNRI     =      ZERO
                     MOVE    KH-IDX      TO     RECE04-KOHBUNRI
                ELSE
                  IF  RECE02-HENTOTALTEN(2)  >  RECE02-HENTOTALTEN(3)
                     MOVE    KH-IDX      TO     RECE04-KOHBUNRI
                  END-IF
                END-IF
              END-IF
           END-IF
      *     ACCEPT  WRK-IN FROM CONSOLE 
           .
       KOHBUNRI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００５）
      *****************************************************************
       RECE05-HENSYU-SEC               SECTION.
      *
           MOVE    SPACE                TO      RECE05-REC
           INITIALIZE                           RECE05-REC
           MOVE    RECE02-HOSPID        TO      RECE05-HOSPID
           MOVE    RECE02-SRYYM         TO      RECE05-SRYYM
           MOVE    RECE02-NYUGAIKBN     TO      RECE05-NYUGAIKBN
           MOVE    RECE02-PTID          TO      RECE05-PTID
           MOVE    RECE02-RECEKA        TO      RECE05-RECEKA
           MOVE    RECE02-TEISYUTUSAKI  TO      RECE05-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU   TO      RECE05-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY  TO      RECE05-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY   TO      RECE05-HOJOKBN
           MOVE    RECE02-TEKSTYMD      TO      RECE05-TEKSTYMD
           MOVE    WK-ZAINUM            TO      RECE05-ZAINUM
           START   RECE05-FILE   KEY  IS  >=    RECE05-KEY
                   INVALID
                       MOVE    1        TO      FLG-RECE05
                   NOT  INVALID
                       MOVE    ZERO     TO      FLG-RECE05
                       PERFORM   RECE05-NEXT-SEC
           END-START
           PERFORM   UNTIL
                  (RECE02-HOSPID        NOT =  RECE05-HOSPID)    OR
                  (RECE02-NYUGAIKBN     NOT =  RECE05-NYUGAIKBN) OR
                  (RECE02-PTID          NOT =  RECE05-PTID)      OR
                  (RECE02-RECEKA        NOT =  RECE05-RECEKA)    OR
                  (RECE02-TEISYUTUSAKI  NOT =  RECE05-TEISYUTUSAKI) OR
                  (RECE02-RECESYUBETU   NOT =  RECE05-RECESYUBETU)  OR
                  (RECE02-HKNJANUM-KEY  NOT =  RECE05-HKNJANUM)  OR
                  (RECE02-HOJOKBN-KEY   NOT =  RECE05-HOJOKBN )  OR
                  (RECE02-TEKSTYMD      NOT =  RECE05-TEKSTYMD)  OR
                  (WK-ZAINUM            NOT =  RECE05-ZAINUM)    OR
                  (FLG-RECE05           =      1)
      *
                   IF    RECE02-HENTOTALTEN(1)   NOT  =  ZERO
                     IF    KH-IDX      NOT =   ZERO
                            MOVE   KH-IDX        TO     RECE05-KOHBUNRI
                            PERFORM      RECE05-REWRITE-SEC
                     END-IF
                   ELSE
                     IF    KH-IDX      NOT =   ZERO
                       IF    RECE05-KOHBUNRI     =      ZERO
                            MOVE   KH-IDX        TO     RECE05-KOHBUNRI
                            PERFORM      RECE05-REWRITE-SEC
                       ELSE
                         IF  RECE02-HENTOTALTEN(2)  >
                                                  RECE02-HENTOTALTEN(3)
                            MOVE   KH-IDX        TO     RECE05-KOHBUNRI
                            PERFORM      RECE05-REWRITE-SEC
                         END-IF
                       END-IF
                     END-IF
                   END-IF
      *
                   PERFORM      RECE05-NEXT-SEC
           END-PERFORM
      *
           .
       RECE05-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００４）
      *****************************************************************
       RECE04-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE04-REC
      *
           ADD     1                   TO  CNT-OUT1
      *
           .
       RECE04-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE05-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE05-REC
      *
           ADD     1                   TO  CNT-OUT2
      *
           .
       RECE05-REWRITE-EXT.
           EXIT.
057400*
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
           CLOSE   RECE02-FILE
                   RECE04-FILE
                   RECE05-FILE
      *
           DISPLAY "RECE02  CNT-IN  :" CNT-IN 
           DISPLAY "RECE04  CNT-MOD :" CNT-OUT1  
           DISPLAY "RECE05  CNT-MOD :" CNT-OUT2 
           DISPLAY "*** ORCR0035 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００２）
      *****************************************************************
       RECE02-READ-SEC            SECTION.
      *
           READ    RECE02-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
      *
                   DISPLAY "RECE02  PTNUM=" RECE02-PTNUM
           END-READ
           .
       RECE02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       RECE04-NEXT-SEC            SECTION.
      *
           MOVE    ZERO          TO    FLG-RECE04
           READ    RECE04-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE04
           END-READ
           .
       RECE04-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００５）
      *****************************************************************
       RECE05-NEXT-SEC            SECTION.
      *
           MOVE    ZERO          TO    FLG-RECE05
           READ    RECE05-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE05
           END-READ
           .
       RECE05-READ-EXT.
           EXIT.
      *
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
