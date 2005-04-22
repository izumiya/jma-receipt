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
       PROGRAM-ID.             ORCR0038.
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
      *  01.00.01    NACL-藤原    03.03.12  補助区分の追加
      *  01.00.02    NACL-門脇    03.03.13  剤点数をキーに追加対応
      *  01.00.03    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.02    NACL-門脇    04.07.12  ファイルクリア追加
      *  01.01.03    NACL-門脇    04.08.17  レセプト科の編集
      *  01.01.04    NACL-門脇    05.04.20  内服薬剤逓減編集対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE04-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
           SELECT  RECE042-FILE    ASSIGN  RECE042PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE042-KEY
                                   FILE    STATUS  IS  STS-RECE042.
           SELECT  RECE042X-FILE   ASSIGN  RECE042PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE042X-KEY
                                   FILE    STATUS  IS  STS-RECE042.
           SELECT  RECE04W-FILE    ASSIGN  RECE04WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04W-KEY
                                   FILE    STATUS  IS  STS-RECE04W.
           SELECT  RECE04WX-FILE   ASSIGN  RECE04WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE04WX-KEY
                                   FILE    STATUS  IS  STS-RECE04W.
      *    エラーファイル
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
       FD  RECE042-FILE.
       01  RECE042-REC. 
           COPY    "CPRCF0042.INC".
      *
       FD  RECE042X-FILE.
       01  RECE042X-REC. 
           COPY    "CPRCF0042.INC" REPLACING  //RECE042//
                                   BY         //RECE042X//.
      *
       FD  RECE04W-FILE.
       01  RECE04W-REC. 
           COPY    "CPRCF004.INC"  REPLACING  //RECE04//
                                   BY         //RECE04W//.
      *
       FD  RECE04WX-FILE.
       01  RECE04WX-REC. 
           COPY    "CPRCF004.INC"  REPLACING  //RECE04//
                                   BY         //RECE04WX//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE02//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE04//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE042//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE04W//.
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
           03  STS-RECE042                             PIC X(02).
           03  STS-RECE04W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE02                              PIC 9(01).
           03  FLG-RECE04                              PIC 9(01).
           03  FLG-RECE042                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-RECE042                             PIC 9(06).
           03  CNT-RECE04W                             PIC 9(06). 
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
           03  WRK-KOHBUNRI-MIN        PIC 9(01).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
       01  WRK-RECE042-REC.
           COPY    "CPRCF0042.INC" REPLACING  //RECE042//
                                   BY         //WRK-RECE042//.
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
           MOVE    "ORCR0038"      TO  JOB-PGID
           MOVE    "同一剤のまとめ"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECE020"           TO  RECE02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE02PARA-TERMID
      *
           MOVE    "RECE040"           TO  RECE04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
      *
           MOVE    "RECE042"           TO  RECE042PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE042PARA-TERMID
      *
           MOVE    "RECE04W"           TO  RECE04WPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04WPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    OUTPUT              RECE042X-FILE
           CLOSE                       RECE042X-FILE
      *
           OPEN    OUTPUT              RECE04WX-FILE
           CLOSE                       RECE04WX-FILE
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
           OPEN    INPUT               RECE04-FILE
           OPEN    I-O                 RECE02-FILE
                                       RECE042-FILE
           PERFORM 900-RECE02-READ-SEC
      *
           PERFORM 210-RECE042-WRITE-SEC
                                   UNTIL   FLG-RECE02  =   1
           CLOSE   RECE042-FILE
                   RECE04-FILE
                   RECE02-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE042-FILE
                   OUTPUT  RECE04W-FILE
      *
           PERFORM 900-RECE042-READ-SEC 
           PERFORM 220-RECE04W-WRITE-SEC
                                   UNTIL   FLG-RECE042 =   1
           CLOSE   RECE04W-FILE
                   RECE042-FILE
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
       210-RECE042-WRITE-SEC                SECTION.
      *
           MOVE       9                TO  WRK-KOHBUNRI-MIN
      *
           MOVE    ZERO                TO  WRK-COMRENNUM
      *
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-HOSPID       TO  RECE04-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE04-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE04-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE04-PTID
           MOVE    RECE02-RECEKA       TO  RECE04-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE04-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE04-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE04-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE04-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE04-TEKSTYMD
           MOVE    ZERO                TO  RECE04-ZAINUM
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 900-RECE04-READ-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE04           =   1                   )
               OR     ( RECE02-HOSPID    NOT =   RECE04-HOSPID       )
               OR     ( RECE02-SRYYM     NOT =   RECE04-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE04-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE04-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE04-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE04-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE04-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY  NOT =  RECE04-HKNJANUM    )
               OR     ( RECE02-HOJOKBN-KEY   NOT =  RECE04-HOJOKBN     )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE04-TEKSTYMD     )
      *
      *        複数診療科受診でないとき
      ***      IF      RECE02-SRYKA (2)    =   SPACE
      ***      OR      RECE04-COMCDFLG     =   "1"
               IF      RECE04-COMCDFLG     =   "1" OR "2"
                   MOVE    SPACE               TO  RECE042-REC
                   INITIALIZE                      RECE042-REC
                   MOVE    RECE04-HOSPID       TO  RECE042-HOSPID
                   MOVE    RECE04-SRYYM        TO  RECE042-SRYYM
                   MOVE    RECE04-NYUGAIKBN    TO  RECE042-NYUGAIKBN
                   MOVE    RECE04-PTID         TO  RECE042-PTID
                   MOVE    RECE04-RECEKA       TO  RECE042-RECEKA
                   MOVE    RECE04-TEISYUTUSAKI TO  RECE042-TEISYUTUSAKI
                   MOVE    RECE04-RECESYUBETU  TO  RECE042-RECESYUBETU
                   MOVE    RECE04-HKNJANUM     TO  RECE042-HKNJANUM
                   MOVE    RECE04-HOJOKBN      TO  RECE042-HOJOKBN
                   MOVE    RECE04-TEKSTYMD     TO  RECE042-TEKSTYMD
                   MOVE    RECE04-KOHBUNRI     TO  RECE042-KOHBUNRI
                   MOVE    RECE04-SRYKBN       TO  RECE042-SRYKBN
                   MOVE    RECE04-ZAITEN       TO  RECE042-ZAITEN
                   MOVE    RECE04-SRYCDTOTAL   TO  RECE042-SRYCDTOTAL
                   MOVE    RECE04-SURYOUTOTAL  TO  RECE042-SURYOUTOTAL
                   MOVE    RECE04-RUIKEITOTAL  TO  RECE042-RUIKEITOTAL
                   MOVE    RECE04-MEISAISU     TO  RECE042-MEISAISU 
                   IF   RECE04-COMCDFLG     =   "1" OR "2"
                        ADD       1            TO  WRK-COMRENNUM
                        MOVE    WRK-COMRENNUM  TO  RECE042-COMRENNUM
                   END-IF
                   MOVE    RECE04-FLG-HOUTEN-COMMENT
                                      TO  RECE042-FLG-HOUTEN-COMMENT
                   MOVE    RECE04-ZAIKAISU     TO  RECE042-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           MOVE    RECE04-KAISU (IDX IDY)
                                           TO   RECE042-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
                   MOVE    RECE04-REC          TO  RECE042-REC1
                   WRITE   RECE042-REC
                   ADD     1                   TO  CNT-RECE042
               ELSE
      *
      *        複数診療科受診のとき
      *            既に同一剤があるか
                   MOVE    SPACE               TO  RECE042-REC
                   INITIALIZE                      RECE042-REC
                   MOVE    RECE04-HOSPID       TO  RECE042-HOSPID
                   MOVE    RECE04-SRYYM        TO  RECE042-SRYYM
                   MOVE    RECE04-NYUGAIKBN    TO  RECE042-NYUGAIKBN
                   MOVE    RECE04-PTID         TO  RECE042-PTID
                   MOVE    RECE04-RECEKA       TO  RECE042-RECEKA
                   MOVE    RECE04-TEISYUTUSAKI TO  RECE042-TEISYUTUSAKI
                   MOVE    RECE04-RECESYUBETU  TO  RECE042-RECESYUBETU
                   MOVE    RECE04-HKNJANUM     TO  RECE042-HKNJANUM
                   MOVE    RECE04-HOJOKBN      TO  RECE042-HOJOKBN
                   MOVE    RECE04-TEKSTYMD     TO  RECE042-TEKSTYMD
                   MOVE    RECE04-KOHBUNRI     TO  RECE042-KOHBUNRI
                   MOVE    RECE04-SRYKBN       TO  RECE042-SRYKBN
                   MOVE    RECE04-ZAITEN       TO  RECE042-ZAITEN
                   MOVE    RECE04-SRYCDTOTAL   TO  RECE042-SRYCDTOTAL
                   MOVE    RECE04-SURYOUTOTAL  TO  RECE042-SURYOUTOTAL
                   MOVE    RECE04-RUIKEITOTAL  TO  RECE042-RUIKEITOTAL
                   MOVE    RECE04-MEISAISU     TO  RECE042-MEISAISU 
                   MOVE    RECE04-FLG-HOUTEN-COMMENT
                                      TO  RECE042-FLG-HOUTEN-COMMENT
                   MOVE    RECE042-REC         TO  WRK-RECE042-REC
                   PERFORM 900-RECE042-INV-SEC
                   IF      FLG-RECE042         =   1
                       MOVE    WRK-RECE042-REC     TO  RECE042-REC
                       MOVE    RECE04-REC          TO  RECE042-REC1
                   END-IF
      *            剤回数・日数回数テーブルの加算
                   ADD     RECE04-ZAIKAISU     TO  RECE042-ZAIKAISU
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       31
                           ADD     RECE04-KAISU (IDX IDY)
                                           TO   RECE042-KAISU (IDX IDY)
                       END-PERFORM
                   END-PERFORM
      *     
                   IF      FLG-RECE042         =   1
                       WRITE   RECE042-REC
                       ADD     1                   TO  CNT-RECE042
                   ELSE
                       REWRITE RECE042-REC
                   END-IF
               END-IF
      *
               IF   WRK-KOHBUNRI-MIN  >  RECE04-KOHBUNRI
                   MOVE   RECE04-KOHBUNRI  TO  WRK-KOHBUNRI-MIN
               END-IF
      *
               PERFORM     900-RECE04-READ-SEC
           END-PERFORM
      *
           MOVE      WRK-KOHBUNRI-MIN   TO   RECE02-KOHBUNRI-MIN
           REWRITE   RECE02-REC
      *
      *
           PERFORM 900-RECE02-READ-SEC
           .
       210-RECE042-WRITE-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       220-RECE04W-WRITE-SEC                SECTION.
      *
           MOVE    RECE042-REC1        TO  RECE04W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE042-ZAIKAISU     TO  RECE04W-ZAIKAISU
           MOVE    RECE042-DAY-AREA     TO  RECE04W-DAY-AREA
      *
           WRITE   RECE04W-REC
           ADD     1                   TO  CNT-RECE04W
      *
           PERFORM     900-RECE042-READ-SEC
           .
       220-RECE04W-WRITE-EXT.
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
           DISPLAY "RECE04  CNT-IN  :" CNT-IN
           DISPLAY "RECE042 CNT-OUT :" CNT-RECE042
           DISPLAY "RECE04W CNT-OUT :" CNT-RECE04W
           DISPLAY "*** ORCR0038 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ読込処理
      *****************************************************************
       900-RECE02-READ-SEC           SECTION.
      *
           READ    RECE02-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE02
               NOT AT  END
                   ADD     ZERO        TO  FLG-RECE02
      *
                   DISPLAY "RECE02  PTNUM=" RECE02-PTNUM
           END-READ
           .
       900-RECE02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   レセプト０４データ読込処理
      *****************************************************************
       900-RECE04-READ-SEC            SECTION.
      *
           READ    RECE04-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE04
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE04
                   ADD     1             TO    CNT-IN
           END-READ
           .
       900-RECE04-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE042-READ-SEC            SECTION.
      *
           READ    RECE042-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE042
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE042
           END-READ
           .
       900-RECE042-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE042-INV-SEC            SECTION.
      *
           READ    RECE042-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE042
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE042
           END-READ
           .
       900-RECE042-READ-EXT.
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
