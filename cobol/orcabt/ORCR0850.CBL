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
       PROGRAM-ID.             ORCR0850.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 同一剤のまとめ（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  01.00.02    NACL-門脇    04.07.12  ファイルクリア追加
      *  01.00.03    NACL-門脇    04.08.17  レセプト科の編集
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    05/07/05  検査は診療種別区分毎でまとめる
      *  01.01.03    NACL-門脇    05/11/11  コメントを含む場合の編集修正
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
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE842-FILE    ASSIGN  RECE842PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE842-KEY
                                   FILE    STATUS  IS  STS-RECE842.
           SELECT  RECE842X-FILE   ASSIGN  RECE842PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE842X-KEY
                                   FILE    STATUS  IS  STS-RECE842.
           SELECT  RECE84W-FILE    ASSIGN  RECE84WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84W-KEY
                                   FILE    STATUS  IS  STS-RECE84W.
           SELECT  RECE84WX-FILE   ASSIGN  RECE84WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE84WX-KEY
                                   FILE    STATUS  IS  STS-RECE84W.
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
       FD  RECE85-FILE.
       01  RECE85-REC. 
           COPY    "CPRCF085.INC".
      *
       FD  RECE842-FILE.
       01  RECE842-REC. 
           COPY    "CPRCF0842.INC".
      *
       FD  RECE842X-FILE.
       01  RECE842X-REC. 
           COPY    "CPRCF0842.INC" REPLACING  //RECE842//
                                   BY         //RECE842X//.
      *
       FD  RECE84W-FILE.
       01  RECE84W-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84W//.
      *
       FD  RECE84WX-FILE.
       01  RECE84WX-REC. 
           COPY    "CPRCF084.INC"  REPLACING  //RECE84//
                                   BY         //RECE84WX//.
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
                                   BY         //RECE85//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE842//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE84W//.
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
           03  STS-RECE85                              PIC X(02).
           03  STS-RECE842                             PIC X(02).
           03  STS-RECE84W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE82                              PIC 9(01).
           03  FLG-RECE84                              PIC 9(01).
           03  FLG-RECE85                              PIC 9(01).
           03  FLG-RECE842                             PIC 9(01).
           03  FLG-NASHI                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-MATOME                              PIC 9(01).
           03  FLG-NG                                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-RECE842                             PIC 9(06).
           03  CNT-RECE84W                             PIC 9(06). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDZ2                                    PIC 9(03).
           03  IDZ3                                    PIC 9(03).
           03  IDZ4                                    PIC 9(03).
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
      *    剤番号の退避
           03  WRK-ZAINUM              PIC 9(08).
      *    剤番号の退避
           03  WRK-SRYSYUKBN           PIC X(03).
      *    コメント退避
           03  WRK-COM-AREA.
               05  WRK-COM             PIC X(200)    OCCURS  10.
      *    インデックス退避
           03  WRK-INDEX-AREA.
               05  WRK-INDEX           PIC 9(003)    OCCURS  10.
      *
           03  WRK-RECEERR             PIC X(200). 
      *
       01  WRK-RECE842-REC.
           COPY    "CPRCF0842.INC" REPLACING  //RECE842//
                                   BY         //WRK-RECE842//.
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
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
      *
           MOVE    "RECE840"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
      *
           MOVE    "RECE850"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
      *
           MOVE    "RECE842"           TO  RECE842PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE842PARA-TERMID
      *
           MOVE    "RECE84W"           TO  RECE84WPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84WPARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0850"      TO  JOB-PGID
           MOVE    "同一剤のまとめ（労災・自賠責）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           OPEN    OUTPUT              RECE842X-FILE
           CLOSE                       RECE842X-FILE
      *
           OPEN    OUTPUT              RECE84WX-FILE
           CLOSE                       RECE84WX-FILE
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
                                       RECE85-FILE
           OPEN    I-O                 RECE842-FILE
           PERFORM 900-RECE82-READ-SEC
      *
           PERFORM 210-RECE842-WRITE-SEC
                                   UNTIL   FLG-RECE82  =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE842-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE82-FILE
      *
      *    元のレイアウトに出力処理
           OPEN    INPUT   RECE842-FILE
                   OUTPUT  RECE84W-FILE
      *
           PERFORM 900-RECE842-READ-SEC 
           PERFORM 220-RECE84W-WRITE-SEC
                                   UNTIL   FLG-RECE842 =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE84W-FILE
                   RECE842-FILE
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
       210-RECE842-WRITE-SEC                SECTION.
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
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE84           =   1                   )
               OR     ( RECE82-HOSPID    NOT =   RECE84-HOSPID       )
               OR     ( RECE82-SRYYM     NOT =   RECE84-SRYYM        )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE84-NYUGAIKBN    )
               OR     ( RECE82-PTID      NOT =   RECE84-PTID         )
               OR     ( RECE82-RECEKA    NOT =   RECE84-RECEKA       )
               OR     ( RECE82-HKNID     NOT =   RECE84-HKNID        )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE84-TEKSTYMD     )
      *
               MOVE    RECE84-ZAINUM     TO      WRK-ZAINUM
               MOVE    SPACE             TO      WRK-SRYSYUKBN
               IF       RECE84-SRYKBN  =  "60"
                   PERFORM   2101-RECE85-SRYSYUKBN-HENSYU-SEC
               END-IF
      *
               INITIALIZE          WRK-COM-AREA
               IF       RECE84-COMCDFLG   =   "1"
                   PERFORM   2101-RECE85-COM-HENSYU-SEC
               END-IF
      *
               MOVE    SPACE               TO  RECE842-REC
               INITIALIZE                      RECE842-REC
               MOVE    RECE84-HOSPID       TO  RECE842-HOSPID
               MOVE    RECE84-SRYYM        TO  RECE842-SRYYM
               MOVE    RECE84-NYUGAIKBN    TO  RECE842-NYUGAIKBN
               MOVE    RECE84-PTID         TO  RECE842-PTID
               MOVE    RECE84-RECEKA       TO  RECE842-RECEKA
               MOVE    RECE84-HKNID        TO  RECE842-HKNID
               MOVE    RECE84-TEKSTYMD     TO  RECE842-TEKSTYMD
               MOVE    RECE84-SRYKBN       TO  RECE842-SRYKBN
               MOVE    WRK-SRYSYUKBN       TO  RECE842-SRYSYUKBN
               MOVE    RECE84-ZAITEN       TO  RECE842-ZAITEN
               MOVE    RECE84-SRYCDTOTAL   TO  RECE842-SRYCDTOTAL
               MOVE    RECE84-SURYOUTOTAL  TO  RECE842-SURYOUTOTAL
               MOVE    RECE84-RUIKEITOTAL  TO  RECE842-RUIKEITOTAL
               MOVE    RECE84-MEISAISU     TO  RECE842-MEISAISU 
               IF      RECE84-COMCDFLG   =   "1"
                 ADD     1                 TO  WRK-COMRENNUM
                 MOVE    WRK-COMRENNUM     TO  RECE842-COMRENNUM
      *
                 MOVE    RECE842-REC         TO  WRK-RECE842-REC
                 PERFORM 900-RECE842-INV2-SEC
                 IF      FLG-MATOME    =   ZERO
                     MOVE    WRK-RECE842-REC     TO  RECE842-REC
                     MOVE    RECE84-REC          TO  RECE842-REC1
                     MOVE    WRK-COM-AREA        TO  RECE842-COM-AREA
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE84-ZAIKAISU     TO  RECE842-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                            UNTIL   IDY >       31
                         ADD     RECE84-KAISU (IDX IDY)
                                         TO   RECE842-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
                 IF      FLG-MATOME    =   ZERO
                     WRITE   RECE842-REC
                     ADD     1                   TO  CNT-RECE842
                 ELSE
                     REWRITE RECE842-REC
                 END-IF
               ELSE
                 MOVE    RECE842-REC         TO  WRK-RECE842-REC
                 PERFORM 900-RECE842-INV-SEC
                 IF      FLG-RECE842         =   1
                     MOVE    WRK-RECE842-REC     TO  RECE842-REC
                     MOVE    RECE84-REC          TO  RECE842-REC1
                 END-IF
      *          剤回数・日数回数テーブルの加算
                 ADD     RECE84-ZAIKAISU     TO  RECE842-ZAIKAISU
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       4
                     PERFORM VARYING IDY FROM    1   BY  1
                            UNTIL   IDY >       31
                         ADD     RECE84-KAISU (IDX IDY)
                                         TO   RECE842-KAISU (IDX IDY)
                     END-PERFORM
                 END-PERFORM
                 IF      FLG-RECE842         =   1
                     WRITE   RECE842-REC
                     ADD     1                   TO  CNT-RECE842
                 ELSE
                     REWRITE RECE842-REC
                 END-IF
               END-IF
      *
               PERFORM     900-RECE84-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM 900-RECE82-READ-SEC
           .
       210-RECE842-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分取得
      *****************************************************************
       2101-RECE85-SRYSYUKBN-HENSYU-SEC         SECTION.
      *
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           START   RECE85-FILE
               KEY IS    >=   RECE85-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE85
               NOT INVALID
                   PERFORM  900-RECE85-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE82-HOSPID        NOT =  RECE85-HOSPID)    OR
                  (RECE82-SRYYM         NOT =  RECE85-SRYYM)     OR
                  (RECE82-NYUGAIKBN     NOT =  RECE85-NYUGAIKBN) OR
                  (RECE82-PTID          NOT =  RECE85-PTID)      OR
                  (RECE82-RECEKA        NOT =  RECE85-RECEKA)    OR
                  (RECE82-HKNID         NOT =  RECE85-HKNID)     OR
                  (RECE82-TEKSTYMD      NOT =  RECE85-TEKSTYMD)  OR
                  (WRK-ZAINUM           NOT =  RECE85-ZAINUM)    OR
                  (FLG-RECE85               =      1        )
      *
                   MOVE   RECE85-SRYSYUKBN   TO   WRK-SRYSYUKBN
      *
                   MOVE   1                  TO   FLG-RECE85
      *
           END-PERFORM
      *
           .
       2101-RECE85-SRYSYUKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集
      *****************************************************************
       2101-RECE85-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                 TO      IDZ2
      *
           INITIALIZE                           RECE85-REC
           MOVE    RECE82-HOSPID        TO      RECE85-HOSPID
           MOVE    RECE82-SRYYM         TO      RECE85-SRYYM
           MOVE    RECE82-NYUGAIKBN     TO      RECE85-NYUGAIKBN
           MOVE    RECE82-PTID          TO      RECE85-PTID
           MOVE    RECE82-RECEKA        TO      RECE85-RECEKA
           MOVE    RECE82-HKNID         TO      RECE85-HKNID
           MOVE    RECE82-TEKSTYMD      TO      RECE85-TEKSTYMD
           MOVE    WRK-ZAINUM           TO      RECE85-ZAINUM
           START   RECE85-FILE
               KEY IS    >=   RECE85-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE85
               NOT INVALID
                   PERFORM  900-RECE85-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE82-HOSPID        NOT =  RECE85-HOSPID)    OR
                  (RECE82-SRYYM         NOT =  RECE85-SRYYM)     OR
                  (RECE82-NYUGAIKBN     NOT =  RECE85-NYUGAIKBN) OR
                  (RECE82-PTID          NOT =  RECE85-PTID)      OR
                  (RECE82-RECEKA        NOT =  RECE85-RECEKA)    OR
                  (RECE82-HKNID         NOT =  RECE85-HKNID)     OR
                  (RECE82-TEKSTYMD      NOT =  RECE85-TEKSTYMD)  OR
                  (WRK-ZAINUM           NOT =  RECE85-ZAINUM)    OR
                  (FLG-RECE85               =      1        )
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >       5)  OR
                                    (RECE85-SRYCD(IDZ)  =   SPACE)
      *
                     IF  (RECE85-SRYCD(IDZ)(1:1)  =   "8"  )  OR
                         (RECE85-SRYCD(IDZ)(1:3)  =   "008")
                         ADD       1                 TO  IDZ2
                         IF   IDZ2  <=  10
                            MOVE   RECE85-NAME(IDZ)  TO  WRK-COM(IDZ2)
                         END-IF
                     END-IF
      *
                  END-PERFORM
      *
                  PERFORM  900-RECE85-READ-SEC
      *
           END-PERFORM
      *
           .
       2101-RECE85-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       220-RECE84W-WRITE-SEC                SECTION.
      *
           MOVE    RECE842-REC1        TO  RECE84W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE842-ZAIKAISU     TO  RECE84W-ZAIKAISU
           MOVE    RECE842-DAY-AREA     TO  RECE84W-DAY-AREA
      *
           WRITE   RECE84W-REC
           ADD     1                   TO  CNT-RECE84W
      *
           PERFORM     900-RECE842-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       220-RECE84W-WRITE-EXT.
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
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO )
           AND   ( JOB-STOPFLG     =   1    )
               MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *            ジョブ管理終了処理
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
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
           DISPLAY "RECE842    :" CNT-RECE842
           DISPLAY "RECE84W    :" CNT-RECE84W
           DISPLAY "*** ORCR0850 END ***"
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
                                               CNT-IN-CANCEL
           END-READ
           .
       900-RECE84-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５データ読込処理
      *****************************************************************
       900-RECE85-READ-SEC            SECTION.
      *
           READ    RECE85-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE85
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE85
           END-READ
           .
       900-RECE85-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE842-READ-SEC            SECTION.
      *
           READ    RECE842-FILE   NEXT
               AT  END
                   MOVE    1             TO    FLG-RECE842
               NOT AT END    
                   MOVE    ZERO          TO    FLG-RECE842
                   ADD     1             TO    CNT-IN-CANCEL
           END-READ
           .
       900-RECE842-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE842-INV-SEC            SECTION.
      *
           READ    RECE842-FILE
               INVALID
                   MOVE    1             TO    FLG-RECE842
               NOT INVALID
                   MOVE    ZERO          TO    FLG-RECE842
           END-READ
           .
       900-RECE842-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE842-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE842-REC
           MOVE    RECE84-HOSPID       TO  RECE842-HOSPID
           MOVE    RECE84-SRYYM        TO  RECE842-SRYYM
           MOVE    RECE84-NYUGAIKBN    TO  RECE842-NYUGAIKBN
           MOVE    RECE84-PTID         TO  RECE842-PTID
           MOVE    RECE84-RECEKA       TO  RECE842-RECEKA
           MOVE    RECE84-HKNID        TO  RECE842-HKNID
           MOVE    RECE84-TEKSTYMD     TO  RECE842-TEKSTYMD
           MOVE    RECE84-SRYKBN       TO  RECE842-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE842-SRYSYUKBN
           MOVE    RECE84-ZAITEN       TO  RECE842-ZAITEN
           MOVE    RECE84-SRYCDTOTAL   TO  RECE842-SRYCDTOTAL
           MOVE    RECE84-SURYOUTOTAL  TO  RECE842-SURYOUTOTAL
           MOVE    RECE84-RUIKEITOTAL  TO  RECE842-RUIKEITOTAL
           MOVE    RECE84-MEISAISU     TO  RECE842-MEISAISU
           START   RECE842-FILE
               KEY IS    >=   RECE842-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE842
               NOT INVALID
                   PERFORM  900-RECE842-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE84-HOSPID       NOT =  RECE842-HOSPID)       OR
                  (RECE84-SRYYM        NOT =  RECE842-SRYYM)        OR
                  (RECE84-NYUGAIKBN    NOT =  RECE842-NYUGAIKBN)    OR
                  (RECE84-PTID         NOT =  RECE842-PTID)         OR
                  (RECE84-RECEKA       NOT =  RECE842-RECEKA)       OR
                  (RECE84-HKNID        NOT =  RECE842-HKNID)        OR
                  (RECE84-TEKSTYMD     NOT =  RECE842-TEKSTYMD)     OR
                  (RECE84-SRYKBN       NOT =  RECE842-SRYKBN)       OR
                  (WRK-SRYSYUKBN       NOT =  RECE842-SRYSYUKBN)    OR
                  (RECE84-ZAITEN       NOT =  RECE842-ZAITEN)       OR
                  (RECE84-SRYCDTOTAL   NOT =  RECE842-SRYCDTOTAL)   OR
                  (RECE84-SURYOUTOTAL  NOT =  RECE842-SURYOUTOTAL)  OR
                  (RECE84-RUIKEITOTAL  NOT =  RECE842-RUIKEITOTAL)  OR
                  (RECE84-MEISAISU     NOT =  RECE842-MEISAISU)     OR
                  (FLG-RECE842             =  1)
      *
                  MOVE     ZERO     TO     FLG-NASHI
                  MOVE     ZERO     TO     IDZ3
                  INITIALIZE               WRK-INDEX-AREA
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >      10)  OR
                                    (WRK-COM(IDZ)       =   SPACE)
      *
                     MOVE     ZERO     TO    FLG-ARI
                     PERFORM VARYING IDZ2  FROM    1    BY   1
                              UNTIL (IDZ2               >      10)  OR
                                    (RECE842-COM(IDZ2)  =   SPACE)
                        IF  RECE842-COM(IDZ2)  =  WRK-COM(IDZ)
                            MOVE   ZERO   TO   FLG-NG
                            PERFORM VARYING IDZ4  FROM  1  BY  1
                               UNTIL (IDZ4             >  10 ) OR
                                     (WRK-INDEX(IDZ4)  = ZERO)
                               IF  WRK-INDEX(IDZ4)  =  IDZ2
                                   MOVE   1   TO   FLG-NG
                                   MOVE   10  TO   IDZ4
                               END-IF
                            END-PERFORM
                            IF   FLG-NG  =   1
                               CONTINUE
                            ELSE
                               MOVE   1       TO   FLG-ARI
                               ADD    1       TO   IDZ3
                               MOVE   IDZ2    TO   WRK-INDEX(IDZ3)
                               MOVE   10      TO   IDZ2
                            END-IF
                        END-IF
                     END-PERFORM
                     IF   FLG-ARI  =  ZERO
                         MOVE    10    TO    IDZ
                         MOVE     1    TO    FLG-NASHI
                     END-IF
      *
                  END-PERFORM
      *
                  IF   FLG-NASHI   =   1
                     PERFORM  900-RECE842-READ-SEC
                  ELSE
                     MOVE    1    TO   FLG-RECE842
                     MOVE    1    TO   FLG-MATOME
                  END-IF
      *
           END-PERFORM
      *
           .
       900-RECE842-INV2-EXT.
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
