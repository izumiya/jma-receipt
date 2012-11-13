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
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/09/28  中止処理の追加
      *
      *  02.06.01    NACL-門脇    05/04/20  内服薬剤逓減編集対応
      *  02.06.02    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.03    NACL-門脇    05/07/05  検査は診療種別区分毎でまとめる
      *  02.06.04    NACL-門脇    05/11/11  コメントを含む場合の編集修正
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    12/04/11  平成２４年４月改正
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
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
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
           SELECT  RECE042-FILE    ASSIGN  RECE042PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE042-KEY
                                   FILE    STATUS  IS  STS-RECE042.
           SELECT  RECE04W-FILE    ASSIGN  RECE04WPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04W-KEY
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
       FD  RECE05-FILE.
       01  RECE05-REC. 
           COPY    "CPRCF005.INC".
      *
       FD  RECE042-FILE.
       01  RECE042-REC. 
           COPY    "CPRCF0042.INC".
      *
       FD  RECE04W-FILE.
       01  RECE04W-REC. 
           COPY    "CPRCF004.INC"  REPLACING  //RECE04//
                                   BY         //RECE04W//.
      * 
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE02//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE04//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE05//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE042//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE04W//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02                              PIC X(02).
           03  STS-RECE04                              PIC X(02).
           03  STS-RECE05                              PIC X(02).
           03  STS-RECE042                             PIC X(02).
           03  STS-RECE04W                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE02                              PIC 9(01).
           03  FLG-RECE04                              PIC 9(01).
           03  FLG-RECE05                              PIC 9(01).
           03  FLG-RECE042                             PIC 9(01).
           03  FLG-NASHI                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-MATOME                              PIC 9(01).
           03  FLG-NG                                  PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-RECE042                             PIC 9(06).
           03  CNT-RECE04W                             PIC 9(06). 
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
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-COMRENNUM           PIC 9(05).
      *
           03  WRK-KOHBUNRI-MIN        PIC 9(01).
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE020"           TO  RECE02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE02PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE02PARA-HOSPNUM
      *
           MOVE    "RECE040"           TO  RECE04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE04PARA-HOSPNUM
      *
           MOVE    "RECE050"           TO  RECE05PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE05PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE05PARA-HOSPNUM
      *
           MOVE    "RECE042"           TO  RECE042PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE042PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE042PARA-HOSPNUM
      *
           MOVE    "RECE04W"           TO  RECE04WPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04WPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE04WPARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0038"      TO  JOB-PGID
               MOVE    "同一剤のまとめ"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE042-FILE
           CLOSE                       RECE042-FILE
      *
           OPEN    OUTPUT              RECE04W-FILE
           CLOSE                       RECE04W-FILE
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
                                       RECE05-FILE
           OPEN    I-O                 RECE02-FILE
                                       RECE042-FILE
           PERFORM 900-RECE02-READ-SEC
      *
           PERFORM 210-RECE042-WRITE-SEC
                                   UNTIL   FLG-RECE02  =   1
                                   OR      FLG-END     =   1
           CLOSE   RECE042-FILE
                   RECE04-FILE
                   RECE05-FILE
                   RECE02-FILE
      *
      *    元のレイアウトに出力処理
           IF      FLG-END             =   ZERO
               OPEN    INPUT   RECE042-FILE
                       OUTPUT  RECE04W-FILE
               PERFORM 900-RECE042-READ-SEC 
      *
               PERFORM 220-RECE04W-WRITE-SEC
                                   UNTIL   FLG-RECE042 =   1
                                   OR      FLG-END     =   1
               CLOSE   RECE04W-FILE
                       RECE042-FILE
           END-IF
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
           MOVE    RECE02-KEY          TO  RECE04-KEY1
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
               UNTIL  ( FLG-END              =   1           )
               OR     ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
      *
               MOVE    RECE04-ZAINUM     TO      WRK-ZAINUM
               MOVE    SPACE             TO      WRK-SRYSYUKBN
               IF       RECE04-SRYKBN  =  "60" OR "64" OR "99"
                   PERFORM   2101-RECE05-SRYSYUKBN-HENSYU-SEC
               END-IF
      *
               INITIALIZE          WRK-COM-AREA
               IF       RECE04-COMCDFLG   =   "1"
                   PERFORM   2101-RECE05-COM-HENSYU-SEC
               END-IF
      *
               IF      RECE04-COMCDFLG     =   "2"
                   MOVE    SPACE               TO  RECE042-REC
                   INITIALIZE                      RECE042-REC
                   MOVE    RECE04-KEY1         TO  RECE042-KEY1
                   MOVE    RECE04-KOHBUNRI     TO  RECE042-KOHBUNRI
                   MOVE    RECE04-SRYKBN       TO  RECE042-SRYKBN
                   MOVE    SPACE               TO  RECE042-SRYSYUKBN
                   MOVE    RECE04-ZAITEN       TO  RECE042-ZAITEN
                   MOVE    RECE04-SRYCDTOTAL   TO  RECE042-SRYCDTOTAL
                   MOVE    RECE04-SURYOUTOTAL  TO  RECE042-SURYOUTOTAL
                   MOVE    RECE04-RUIKEITOTAL  TO  RECE042-RUIKEITOTAL
                   MOVE    RECE04-MEISAISU     TO  RECE042-MEISAISU 
                   MOVE    RECE04-FLG-HOUTEN-COMMENT
                                      TO  RECE042-FLG-HOUTEN-COMMENT
                   ADD     1                   TO  WRK-COMRENNUM
                   MOVE    WRK-COMRENNUM       TO  RECE042-COMRENNUM
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
                   MOVE    SPACE               TO  RECE042-REC
                   INITIALIZE                      RECE042-REC
                   MOVE    RECE04-KEY1         TO  RECE042-KEY1
                   MOVE    RECE04-KOHBUNRI     TO  RECE042-KOHBUNRI
                   MOVE    RECE04-SRYKBN       TO  RECE042-SRYKBN
                   MOVE    WRK-SRYSYUKBN       TO  RECE042-SRYSYUKBN
                   MOVE    RECE04-ZAITEN       TO  RECE042-ZAITEN
                   MOVE    RECE04-SRYCDTOTAL   TO  RECE042-SRYCDTOTAL
                   MOVE    RECE04-SURYOUTOTAL  TO  RECE042-SURYOUTOTAL
                   MOVE    RECE04-RUIKEITOTAL  TO  RECE042-RUIKEITOTAL
                   MOVE    RECE04-MEISAISU     TO  RECE042-MEISAISU 
                   MOVE    RECE04-FLG-HOUTEN-COMMENT
                                      TO  RECE042-FLG-HOUTEN-COMMENT
                   IF      RECE04-COMCDFLG   =   "1"
                     ADD     1                 TO  WRK-COMRENNUM
                     MOVE    WRK-COMRENNUM     TO  RECE042-COMRENNUM
      *
                     MOVE    RECE042-REC       TO  WRK-RECE042-REC
                     PERFORM 900-RECE042-INV2-SEC
                     IF      FLG-MATOME    =   ZERO
                         MOVE    WRK-RECE042-REC   TO  RECE042-REC
                         MOVE    RECE04-REC        TO  RECE042-REC1
                         MOVE    WRK-COM-AREA      TO  RECE042-COM-AREA
                     END-IF
      *              剤回数・日数回数テーブルの加算
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
                     IF  RECE04-SYODOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE042-SYODOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE04-SYODOU-SRYKA(1)
                                     =  RECE042-SYODOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE04-SYODOU-SRYKA(1)
                                        TO   RECE042-SYODOU-SRYKA(IDX)
                       END-IF
                     END-IF
                     IF  RECE04-SAIDOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE042-SAIDOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE04-SAIDOU-SRYKA(1)
                                     =  RECE042-SAIDOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE04-SAIDOU-SRYKA(1)
                                        TO   RECE042-SAIDOU-SRYKA(IDX)
                       END-IF
                     END-IF
      *
                     IF      FLG-MATOME    =   ZERO
                         WRITE   RECE042-REC
                         ADD     1                   TO  CNT-RECE042
                     ELSE
                         REWRITE RECE042-REC
                     END-IF
                   ELSE
                     MOVE    RECE042-REC         TO  WRK-RECE042-REC
                     PERFORM 900-RECE042-INV-SEC
                     IF      FLG-RECE042         =   1
                         MOVE    WRK-RECE042-REC     TO  RECE042-REC
                         MOVE    RECE04-REC          TO  RECE042-REC1
                     END-IF
      *              剤回数・日数回数テーブルの加算
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
                     IF  RECE04-SYODOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE042-SYODOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE04-SYODOU-SRYKA(1)
                                     =  RECE042-SYODOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE04-SYODOU-SRYKA(1)
                                        TO   RECE042-SYODOU-SRYKA(IDX)
                       END-IF
                     END-IF
                     IF  RECE04-SAIDOU-SRYKA(1)  NOT =  SPACE
                       MOVE     ZERO     TO    FLG-ARI
                       PERFORM  VARYING  IDX   FROM    1   BY  1
                                 UNTIL IDX                       > 10
                                  OR   RECE042-SAIDOU-SRYKA(IDX) = SPACE
                                  OR   FLG-ARI                   = 1
                          IF  RECE04-SAIDOU-SRYKA(1)
                                     =  RECE042-SAIDOU-SRYKA(IDX)
                              MOVE  1   TO   FLG-ARI
                          END-IF
                       END-PERFORM
                       IF  FLG-ARI  =  ZERO
                       AND IDX     <=  10
                              MOVE   RECE04-SAIDOU-SRYKA(1)
                                        TO   RECE042-SAIDOU-SRYKA(IDX)
                       END-IF
                     END-IF
      *
                     IF      FLG-RECE042         =   1
                         WRITE   RECE042-REC
                         ADD     1                   TO  CNT-RECE042
                     ELSE
                         REWRITE RECE042-REC
                     END-IF
                   END-IF
               END-IF
      *
               IF   WRK-KOHBUNRI-MIN  >  RECE04-KOHBUNRI
                   MOVE   RECE04-KOHBUNRI  TO  WRK-KOHBUNRI-MIN
               END-IF
      *
               PERFORM     900-RECE04-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
               MOVE      WRK-KOHBUNRI-MIN   TO   RECE02-KOHBUNRI-MIN
               REWRITE   RECE02-REC
      *
      *
               PERFORM 900-RECE02-READ-SEC
           END-IF
           .
       210-RECE042-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別区分取得
      *****************************************************************
       2101-RECE05-SRYSYUKBN-HENSYU-SEC         SECTION.
      *
           INITIALIZE                           RECE05-REC
           MOVE    RECE02-KEY           TO      RECE05-KEY1
           MOVE    WRK-ZAINUM           TO      RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE05
               NOT INVALID
                   PERFORM  900-RECE05-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE02-KEY           NOT =  RECE05-KEY1  )    OR
                  (WRK-ZAINUM           NOT =  RECE05-ZAINUM)    OR
                  (FLG-RECE05               =      1        )
      *
                   MOVE   RECE05-SRYSYUKBN   TO   WRK-SRYSYUKBN
      *
                   MOVE   1                  TO   FLG-RECE05
      *
           END-PERFORM
      *
           .
       2101-RECE05-SRYSYUKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集
      *****************************************************************
       2101-RECE05-COM-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                 TO      IDZ2
      *
           INITIALIZE                           RECE05-REC
           MOVE    RECE02-KEY           TO      RECE05-KEY1
           MOVE    WRK-ZAINUM           TO      RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1            TO      FLG-RECE05
               NOT INVALID
                   PERFORM  900-RECE05-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE02-KEY           NOT =  RECE05-KEY1  )    OR
                  (WRK-ZAINUM           NOT =  RECE05-ZAINUM)    OR
                  (FLG-RECE05               =      1        )
      *
                  PERFORM  VARYING   IDZ   FROM    1    BY   1
                            UNTIL   (IDZ                >       5)  OR
                                    (RECE05-SRYCD(IDZ)  =   SPACE)
      *
                     IF  (RECE05-SRYCD(IDZ)(1:1)  =   "8"  )  OR
                         (RECE05-SRYCD(IDZ)(1:3)  =   "008")
                         ADD       1                 TO  IDZ2
                         IF   IDZ2  <=  10
                            MOVE   RECE05-NAME(IDZ)  TO  WRK-COM(IDZ2)
                         END-IF
                     END-IF
      *
                  END-PERFORM
      *
                  PERFORM  900-RECE05-READ-SEC
      *
           END-PERFORM
      *
           .
       2101-RECE05-COM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    元のレイアウトに出力処理
      *****************************************************************
       220-RECE04W-WRITE-SEC                SECTION.
      *
           MOVE    RECE042-REC1        TO  RECE04W-REC
      *
      *   剤回数・日数回数テーブルの加算したものを編集
           MOVE    RECE042-ZAIKAISU     TO  RECE04W-ZAIKAISU
           MOVE    RECE042-DAY-AREA     TO  RECE04W-DAY-AREA
      *
           MOVE    RECE042-SYODOU-SRYKA-AREA
                                        TO  RECE04W-SYODOU-SRYKA-AREA
           MOVE    RECE042-SAIDOU-SRYKA-AREA
                                        TO  RECE04W-SAIDOU-SRYKA-AREA
      *
           WRITE   RECE04W-REC
           ADD     1                   TO  CNT-RECE04W
      *
           PERFORM     900-RECE042-READ-SEC
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
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
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                       MOVE    WRK-PARA-SHELLID
                                               TO  JOB-SHELLID
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
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
                   MOVE    ZERO        TO  FLG-RECE02
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
                   ADD     1             TO    CNT-IN-CANCEL
           END-READ
           .
       900-RECE04-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５データ読込処理
      *****************************************************************
       900-RECE05-READ-SEC            SECTION.
      *
           READ    RECE05-FILE   NEXT
               AT  END
                   MOVE    1      TO    FLG-RECE05
               NOT AT  END
                   MOVE    ZERO   TO    FLG-RECE05
           END-READ
           .
       900-RECE05-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE042-READ-SEC            SECTION.
      *
           READ    RECE042-FILE   NEXT
               AT  END
                   MOVE    1             TO  FLG-RECE042
               NOT AT END    
                   MOVE    ZERO          TO  FLG-RECE042
                   ADD     1             TO  CNT-IN-CANCEL
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
       900-RECE042-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理（Ｆ００４）
      *****************************************************************
       900-RECE042-INV2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MATOME
      *
           INITIALIZE                      RECE042-REC
           MOVE    RECE04-KEY1         TO  RECE042-KEY1
           MOVE    RECE04-KOHBUNRI     TO  RECE042-KOHBUNRI
           MOVE    RECE04-SRYKBN       TO  RECE042-SRYKBN
           MOVE    WRK-SRYSYUKBN       TO  RECE042-SRYSYUKBN
           MOVE    RECE04-ZAITEN       TO  RECE042-ZAITEN
           MOVE    RECE04-SRYCDTOTAL   TO  RECE042-SRYCDTOTAL
           MOVE    RECE04-SURYOUTOTAL  TO  RECE042-SURYOUTOTAL
           MOVE    RECE04-RUIKEITOTAL  TO  RECE042-RUIKEITOTAL
           MOVE    RECE04-MEISAISU     TO  RECE042-MEISAISU
           MOVE    RECE04-FLG-HOUTEN-COMMENT
                                       TO  RECE042-FLG-HOUTEN-COMMENT
           START   RECE042-FILE
               KEY IS    >=   RECE042-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE042
               NOT INVALID
                   PERFORM  900-RECE042-READ-SEC
           END-START
           PERFORM   UNTIL
                  (RECE04-KEY1         NOT =  RECE042-KEY1    )     OR
                  (RECE04-KOHBUNRI     NOT =  RECE042-KOHBUNRI)     OR
                  (RECE04-SRYKBN       NOT =  RECE042-SRYKBN)       OR
                  (WRK-SRYSYUKBN       NOT =  RECE042-SRYSYUKBN)    OR
                  (RECE04-ZAITEN       NOT =  RECE042-ZAITEN)       OR
                  (RECE04-SRYCDTOTAL   NOT =  RECE042-SRYCDTOTAL)   OR
                  (RECE04-SURYOUTOTAL  NOT =  RECE042-SURYOUTOTAL)  OR
                  (RECE04-RUIKEITOTAL  NOT =  RECE042-RUIKEITOTAL)  OR
                  (RECE04-MEISAISU     NOT =  RECE042-MEISAISU)     OR
                  (RECE04-FLG-HOUTEN-COMMENT
                                NOT =  RECE042-FLG-HOUTEN-COMMENT)  OR
                  (FLG-RECE042             =  1)
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
                                    (RECE042-COM(IDZ2)  =   SPACE)
                        IF  RECE042-COM(IDZ2)  =  WRK-COM(IDZ)
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
                     PERFORM  900-RECE042-READ-SEC
                  ELSE
                     MOVE    1    TO   FLG-RECE042
                     MOVE    1    TO   FLG-MATOME
                  END-IF
      *
           END-PERFORM
      *
           .
       900-RECE042-INV2-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
