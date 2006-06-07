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
       PROGRAM-ID.             ORCRRECEMAIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 月次業務　明細書主プロ
      *                    : プレビュー用
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    NACL−竹田　　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.08.02    NACL-藤原    06/03/28  山形県のレセプト様式変更対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
       01  LNK-TIMEOFDAY       PIC X(100).
      *
      *    カウント領域
       01  FLG-AREA.
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYUMEI                              PIC 9(01).
           03  FLG-SYUTTL                              PIC 9(01).
           03  FLG-JYURRK                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SANTEI                              PIC 9(01).
           03  FLG-SRYKARRK                            PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-PRVKANRI                            PIC 9(01).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-A1.
                   07  WRK-PARA-A1-1                   PIC  X(06).
                   07  WRK-PARA-A1-2                   PIC  X(16).
                   07  WRK-PARA-A1-3                   PIC  X(08).
               05  WRK-PARA-A2                         PIC  X(01).
               05  WRK-PARA-A3                         PIC  X(01).
               05  WRK-PARA-A4                         PIC  X(100).
               05  WRK-PARA-A5                         PIC  9(07).
               05  WRK-PARA-A6                         PIC  X(08).
               05  WRK-PARA-A7                         PIC  X(15).
               05  WRK-PARA-A8                         PIC  X(06).
               05  WRK-PARA-A9                         PIC  X(08).
               05  WRK-PARA-A10                        PIC  X(08).
               05  WRK-PARA-A11                        PIC  X(02).
      *
               05  WRK-PARA-A12                        PIC  X(02).
               05  WRK-PARA-A13                        PIC  X(02).
               05  WRK-PARA-A14                        PIC  X(02).
      *
               05  WRK-PARA-A15                        PIC  X(16).
               05  WRK-PARA-A16                        PIC  X(16).
               05  WRK-PARA-A17                        PIC  9(04).
               05  WRK-PARA-A18                        PIC  X(08).
               05  WRK-PARA-A19                        PIC  X(14).
               05  WRK-PARA-A20                        PIC  9(04).
               05  WRK-PARA-A21                        PIC  X(06).
               05  WRK-PARA-A22                        PIC  X(08).
               05  WRK-PARA-A23                        PIC  X(20).
               05  WRK-PARA-A24                        PIC  9(04).
               05  WRK-PARA-A25                        PIC  X(64).
               05  WRK-PARA-A26                        PIC  X(16).
               05  WRK-PARA-A27                        PIC  X(16).
               05  WRK-PARA-A28                        PIC  X(16).
               05  WRK-PARA-A29                        PIC  X(16).
      *
               05  WRK-PARA-PTID                       PIC  9(10).
               05  WRK-PARA-NYUGAIKBN                  PIC  X(01).
               05  WRK-PARA-HKNKBN                     PIC  X(01).
               05  WRK-PARA-SHORI-KBN                  PIC  X(01).
               05  WRK-PARA-HOSPID                     PIC  X(24).
      *
           03  WRK-PARA0.
               05  WRK-PARA0-A1.
                   07  WRK-PARA0-A1-1                  PIC  X(06).
                   07  WRK-PARA0-A1-2                  PIC  X(16).
                   07  WRK-PARA0-A1-3                  PIC  X(08).
               05  WRK-PARA0-A2                        PIC  X(01).
               05  WRK-PARA0-A3                        PIC  X(01).
               05  WRK-PARA0-A5                        PIC  9(07).
               05  WRK-PARA0-A6                        PIC  X(08).
               05  WRK-PARA0-A7                        PIC  9(10).
               05  WRK-PARA0-A8                        PIC  X(06).
      *
           03  WRK-PARA1.
               05  WRK-PARA1-A1.
                   07  WRK-PARA1-A1-1                  PIC  X(06).
                   07  WRK-PARA1-A1-2                  PIC  X(16).
                   07  WRK-PARA1-A1-3                  PIC  X(08).
               05  WRK-PARA1-A3                        PIC  X(01).
               05  WRK-PARA1-A5                        PIC  9(07).
               05  WRK-PARA1-A6                        PIC  X(08).
           03  WRK-PARA2.
               05  WRK-PARA2-A1.
                   07  WRK-PARA2-A1-1                  PIC  X(06).
                   07  WRK-PARA2-A1-2                  PIC  X(16).
                   07  WRK-PARA2-A1-3                  PIC  X(08).
               05  WRK-PARA2-A5                        PIC  9(07).
               05  WRK-PARA2-A6                        PIC  X(08).
      *
           03  WRK-PARA3.
               05  WRK-PARA3-A9                        PIC  X(08).
               05  WRK-PARA3-A10                       PIC  X(08).
               05  WRK-PARA3-A1.
                   07  WRK-PARA3-A1-1                  PIC  X(06).
                   07  WRK-PARA3-A1-2                  PIC  X(16).
                   07  WRK-PARA3-A1-3                  PIC  X(08).
               05  WRK-PARA3-A2                        PIC  X(01).
               05  WRK-PARA3-A3                        PIC  X(01).
               05  WRK-PARA3-A8                        PIC  X(06).
               05  WRK-PARA3-A5                        PIC  9(07).
               05  WRK-PARA3-A6                        PIC  X(08).
      *
           03  WRK-ARG                 PIC X(512).
      *
           03  WRK-PTID-X.
               05  WRK-PTID            PIC 9(10).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *  
           03  WRK-ZAINUM              PIC 9(08).
      *
        01  WRK-CONS-AREA.
      *    帳票出力用パラメタ  
           03  WRK-CONS-ARG.       
               05  WRK-CONS-ARG-PRTID  PIC X(04)   VALUE   "HC06".
               05  WRK-CONS-ARG-SRYYM  PIC X(06)   VALUE   "999999".
               05  WRK-CONS-ARG-CREYMD PIC X(08).
               05  WRK-CONS-ARG-CREHMS PIC X(06).
               05  WRK-CONS-ARG-PREFKBN
                                       PIC X(01)   VALUE   "0".
               05  WRK-CONS-ARG-TEISYUTUSAKI
                                       PIC X(01)   VALUE   "0".
               05  WRK-CONS-ARG-RECESYUBETU
                                       PIC X(04)   VALUE   "0000".
               05  WRK-CONS-ARG-NYUGAIKBN
                                       PIC X(01).
               05  WRK-CONS-ARG-PRTJYUN
                                       PIC X(02)   VALUE   "05".
      *
           COPY    "MCPAREA".
      *
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    医療機関情報
           COPY  "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計マスタ
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    会計付加情報マスタ
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定マスタ
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療科履歴マスタ
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者コメントマスタ
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    プレビュー管理マスタ
           COPY    "CPPRVKANRI.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           PERFORM 200-MAIN-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED BY ","
                                   INTO  WRK-PARA-A1
                                         WRK-PARA-A2
                                         WRK-PARA-A3
                                         WRK-PARA-A5
                                         WRK-PARA-A6
                                         WRK-PARA-A7
                                         WRK-PARA-A8
                                         WRK-PARA-A9
                                         WRK-PARA-A10
                                         WRK-PARA-A11
                                         WRK-PARA-A15
                                         WRK-PARA-A16
                                         WRK-PARA-A17
                                         WRK-PARA-A18
                                         WRK-PARA-A19
                                         WRK-PARA-A20
                                         WRK-PARA-A21
                                         WRK-PARA-A22
                                         WRK-PARA-A23
                                         WRK-PARA-A24
                                         WRK-PARA-A25
                                         WRK-PARA-A26
                                         WRK-PARA-A27
                                         WRK-PARA-A28
                                         WRK-PARA-A29
                                         WRK-PARA-PTID
                                         WRK-PARA-NYUGAIKBN
                                         WRK-PARA-HKNKBN
                                         WRK-PARA-SHORI-KBN
                                         WRK-PARA-HOSPID
           END-UNSTRING
      *
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA0-A1
           MOVE     WRK-PARA-A2    TO  WRK-PARA0-A2
           MOVE     WRK-PARA-A3    TO  WRK-PARA0-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA0-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA0-A6
           MOVE     WRK-PARA-PTID  TO  WRK-PARA0-A7
           MOVE     WRK-PARA-A8    TO  WRK-PARA0-A8
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA1-A1
           MOVE     WRK-PARA-A3    TO  WRK-PARA1-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA1-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA1-A6
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA2-A1
           MOVE     WRK-PARA-A5    TO  WRK-PARA2-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA2-A6
      *
           MOVE     WRK-PARA-A1    TO  WRK-PARA3-A1
           MOVE     WRK-PARA-A2    TO  WRK-PARA0-A2
           MOVE     WRK-PARA-A3    TO  WRK-PARA3-A3
           MOVE     WRK-PARA-A5    TO  WRK-PARA3-A5
           MOVE     WRK-PARA-A6    TO  WRK-PARA3-A6
           MOVE     WRK-PARA-A8    TO  WRK-PARA3-A8
           MOVE     WRK-PARA-A9    TO  WRK-PARA3-A9
           MOVE     WRK-PARA-A10   TO  WRK-PARA3-A10
      *
      *    作成日・時間の設定
           MOVE    WRK-PARA-A9     TO  WRK-CONS-ARG-CREYMD
           MOVE    WRK-PARA-A10    TO  WRK-CONS-ARG-CREHMS
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  WRK-CONS-ARG-NYUGAIKBN 
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
           IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
               PERFORM 20011-REAL-DB-MAKE-SEC
           END-IF
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "1"
                   PERFORM 2002-IHO-NYUIN-SEC
               WHEN    "2"
                   PERFORM 2001-IHO-GAIRAI-SEC
           END-EVALUATE
      *
           IF      WRK-PARA-SHORI-KBN  NOT  =  ZERO
      *    ＤＢオープン処理(もう少し整理)
                   PERFORM 100-DBOPEN-SEC
                   PERFORM 20011-TMPDB-DELETE-SEC
                   PERFORM 900-DBCOMMIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医保処理
      *****************************************************************
       2001-IHO-GAIRAI-SEC                SECTION.
      *
       DISPLAY "orcr0010 start = [" WRK-PARA0 "]"
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0010 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0010"          USING
                                   WRK-PARA0
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
       DISPLAY "orcr0020 start = [" WRK-PARA1 "]"
      **   DISPLAY "orcr0020 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0020"          USING
                                   WRK-PARA1
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0030 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0030"          USING
                                   WRK-PARA2
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0035 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0035"          USING
                                   WRK-PARA2
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0038 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0038"          USING
                                   WRK-PARA2
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0040 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0040"          USING
                                   WRK-PARA2
      *
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0050 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0050"          USING
                                   WRK-PARA2
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0055 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0055"          USING
                                   WRK-PARA2
      *
      *    PERFORM 900-DBDISCONNECT-SEC
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0090 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0090"          USING
                                   WRK-PARA2
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0091 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0091"          USING
                                   WRK-PARA2
      *
      ***  IF      WRK-PARA-A7 NOT =   "NOPG"
      ***      CALL    WRK-PARA-A7     USING
      ***                              WRK-PARA2
      ***  END-IF
      *
           PERFORM 20010-PRVDATA-DEL-SEC
           PERFORM 900-DBDISCONNECT-SEC
      *    前回分削除(存在すれば)
           PERFORM 200101-PRVDATA-DEL-SEC
      *
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0104 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0104"          USING
                                   WRK-PARA3
      *
           IF    ( WRK-PARA-A11    =   "06"     )
           AND   ( WRK-PARA-A1-1   <   "200601" )
               CALL    "ORCRCP060"     USING
                                       WRK-PARA3
           END-IF
      *
           IF      WRK-PARA-A11    =   "40"
               CALL    "ORCRCP400"     USING
                                       WRK-PARA3
           END-IF
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   "1,"        DELIMITED   BY  SIZE
                    "1,"        DELIMITED   BY  SIZE
                    "%%,"       DELIMITED   BY  SIZE
                    WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,01,"     DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0101 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0101"          USING
                                   WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A28  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A29  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **     DISPLAY "orcr0102 START"
      **             " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0102"          USING
                                   WRK-ARG
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **     DISPLAY "orcr0102 END  "
      **             " "  LNK-TIMEOFDAY(1:20).
           .
       2001-IHO-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医保処理
      *****************************************************************
       2002-IHO-NYUIN-SEC          SECTION.
      *
           DISPLAY "take nyuuin st  "
           DISPLAY "orcr0600 start = [" WRK-PARA0 "]"
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0600 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0600"          USING
                                       WRK-PARA0
      *
      **   DISPLAY "orcr0610 start = [" WRK-PARA0 "]"
           CALL    "ORCR0610"          USING
                                   WRK-PARA0
      *
           DISPLAY "orcr0620 start = [" WRK-PARA1 "]"
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0620 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0620"          USING
                                   WRK-PARA1
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0630 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0630"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0640 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0640"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0650 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0650"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0655 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0655"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0660 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0660"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0670 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0670"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0680 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0680"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0690 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0690"          USING
                                   WRK-PARA2
      *
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0700 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0700"          USING
                                   WRK-PARA2
      *
      ***  IF      WRK-PARA-A7 NOT =   "NOPG"
      ***      CALL    WRK-PARA-A7     USING
      ***                              WRK-PARA2
      ***  END-IF
      *
           PERFORM 20010-PRVDATA-DEL-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           PERFORM 200101-PRVDATA-DEL-SEC
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0710 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0710"          USING
                                   WRK-PARA3
      *
      *
           IF      WRK-PARA-A11    =   "40"
               CALL    "ORCRCP401"     USING
                                       WRK-PARA3
           END-IF
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   "1,"        DELIMITED   BY  SIZE
                    "1,"        DELIMITED   BY  SIZE
                    "%%,"       DELIMITED   BY  SIZE
                    WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "0,"        DELIMITED   BY  SIZE
                    "Z,01,"     DELIMITED   BY  SIZE
                                INTO    WRK-ARG
           END-STRING
      **   CALL "timeofday" USING LNK-TIMEOFDAY.
      **   DISPLAY "orcr0101 START"
      **           " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0101"          USING
                                   WRK-ARG
      *
      *
           MOVE     SPACE       TO WRK-ARG
           STRING   WRK-PARA-A5 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A6 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A1 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-CONS-ARG
                                DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "2"         DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A15  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A16  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A17 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A18 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A19 DELIMITED   BY  SIZE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A20 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A21 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A22 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A23 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A24 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A25 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A26 DELIMITED   BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A27  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A28  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    WRK-PARA-A29  DELIMITED BY  SPACE
                    ","         DELIMITED   BY  SIZE
                    "3"         DELIMITED   BY  SIZE
                                INTO    WRK-ARG
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **     DISPLAY "orcr0102 START"
      **             " "  LNK-TIMEOFDAY(1:20).
           CALL    "ORCR0102"          USING
                                   WRK-ARG
      **     CALL "timeofday" USING LNK-TIMEOFDAY.
      **     DISPLAY "orcr0102 END  "
      **             " "  LNK-TIMEOFDAY(1:20).
      *
           .
       2002-IHO-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回分削除(存在すれば)
      *****************************************************************
       20010-PRVDATA-DEL-SEC         SECTION.
      *
           INITIALIZE                  PRVKANRI-REC
           MOVE    "RECEPTX"           TO  PRVKANRI-TBL-KEY
           MOVE    WRK-PARA-PTID       TO  WRK-PTID-X
           MOVE    WRK-PTID            TO  PRVKANRI-TBL-GROUP
           MOVE    PRVKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    PATH-TBL-PRVKANRI-KEY8
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC             =   ZERO
               MOVE    PATH-TBL-PRVKANRI-KEY8
                                       TO  MCP-PATH
               PERFORM 900-PRVKANRI-FET-SEC
               MOVE    PATH-TBL-PRVKANRI-KEY8
                                       TO  MCP-PATH
               PERFORM 900-DBCLOSE-SEC
           ELSE
               MOVE    1               TO  FLG-PRVKANRI
           END-IF
      *
           .
       20010-PRVDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回分削除(存在すれば)
      *****************************************************************
       200101-PRVDATA-DEL-SEC        SECTION.
      *
           IF      FLG-PRVKANRI         =   ZERO
               DISPLAY "RECEMAIN PRV EXIST !!"
               DISPLAY "PAGE = [" PRVKANRI-PAGE "]"
      *        MOVE    PRVKANRI-PAGE   TO  MAX-PAGE-CNT
               PERFORM 100-DBOPEN-SEC
               MOVE    WRK-PARA-A8     TO  WRK-SRYYM
      *
               CALL    "ORCSPRVPSDEL"   USING
                                        WRK-PARA-HOSPID
                                        WRK-PTID
                                        WRK-SRYYM
      *
      *
               PERFORM 900-DBCOMMIT-SEC
           ELSE
               DISPLAY "RECEMAIM PRV NO DATA !!"
           END-IF
      *
           .
       20010-PRVDATA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納)
      *****************************************************************
       20011-SYUNOU-DB-MAKE-SEC      SECTION.
           EVALUATE    SYU-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUNOU-KEY
                                        TO      MCP-PATH
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUNOU-KEY
                                        TO      MCP-PATH
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUNOU-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUNOU-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納明細)
      *****************************************************************
       20011-SYUMEI-DB-MAKE-SEC      SECTION.
           EVALUATE    SMEI-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUMEI-KEY
                                        TO      MCP-PATH
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUMEI-KEY
                                        TO      MCP-PATH
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUMEI-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUMEI-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(収納トータル)
      *****************************************************************
       20011-SYUTOTAL-DB-MAKE-SEC      SECTION.
           EVALUATE    SYUTTL-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUTOTAL-KEY
                                        TO      MCP-PATH
      *
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUTOTAL-KEY
                                        TO      MCP-PATH
      *
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SYUTOTAL-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SYUTOTAL-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(会計)
      *****************************************************************
       20011-SRYACCT-DB-MAKE-SEC      SECTION.
           EVALUATE    ACCT-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SRYACCT-KEY
                                        TO      MCP-PATH
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
      *---> test   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SRYACCT-KEY
                                        TO      MCP-PATH
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SRYACCT-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-SRYACCT-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(受診履歴)
      *****************************************************************
       20011-JYURRK-DB-MAKE-SEC      SECTION.
           EVALUATE    JYURRK-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-JYURRK-KEY
                                        TO      MCP-PATH
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-JYURRK-KEY
                                        TO      MCP-PATH
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-JYURRK-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-JYURRK-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理(患者コメント)
      *****************************************************************
       20011-PTCOM-DB-MAKE-SEC      SECTION.
           EVALUATE    PTCOM-CREYMD
               WHEN    "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-PTCOM-KEY
                                        TO      MCP-PATH
      *    
               WHEN    "999del11"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-PTCOM-DEL11
                                        TO      MCP-PATH
      *    
               WHEN    "88888888"
                   MOVE    "DBUPDATE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-PTCOM-KEY
                                        TO      MCP-PATH
      *    
               WHEN      OTHER
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-PTCOM-KEY
                                        TO      MCP-PATH
           END-EVALUATE
           PERFORM  900-REALDB-INS-SEC
           .
       20011-PTCOM-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       20011-REAL-DB-DELETE-SEC      SECTION.
      *受診履歴削除
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db(jyurrk) delete st  "
                   " "  LNK-TIMEOFDAY(1:20)
           MOVE    WRK-PARA-HOSPID      TO      JYURRK-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    WRK-PARA-A8          TO      WRK-SRYYM
           MOVE    "% "                 TO      WRK-SRYDD
           MOVE    WRK-SRYYMD           TO      JYURRK-SRYYMD
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-JYURRK-DEL2
                                        TO      MCP-PATH
           DISPLAY "take jyurrk  del "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db(jyurrk) delete ed  "
                   " "  LNK-TIMEOFDAY(1:20)
      *診療会計削除
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db(sryacct) delete st  "
                   " "  LNK-TIMEOFDAY(1:20)
           MOVE    WRK-PARA-HOSPID      TO      ACCT-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    WRK-PARA-A8          TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACCT-DEL3
                                        TO      MCP-PATH
           DISPLAY "take acct  del "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db(sryacct) delete ed  "
                   " "  LNK-TIMEOFDAY(1:20)
           PERFORM 200111-REAL-SRYACY-DB-DELETE2-SEC
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db(sryact) del ed  "
                   " "  LNK-TIMEOFDAY(1:20)
      *算定履歴削除
           DISPLAY "real db(santei) del ed  "
           MOVE    WRK-PARA-HOSPID      TO      SANTEI-HOSPID
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    WRK-PARA-A8          TO      SANTEI-SRYYM
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           MOVE    "tbl_santei"         TO  MCP-TABLE
           MOVE    "del2"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"           TO      MCP-FUNC
           CALL    "MONFUNC"            USING
                                        MCPAREA
                                        MCPDATA-REC
           .
       20011-REAL-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
      *診療行為削除(この削除は、時間がかかりすぎ 2に移行)
       200111-REAL-SRYACY-DB-DELETE1-SEC   SECTION.
           MOVE    WRK-PARA-HOSPID      TO      SRY-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    WRK-PARA-A8          TO      SRY-SRYYM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACT-DEL12
                                        TO      MCP-PATH
           DISPLAY "take act  del "
                                WRK-PARA-HOSPID WRK-PTID SRY-SRYYM
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           .
       200111-REAL-SRYACY-DB-DELETE1-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       200111-REAL-SRYACY-DB-DELETE2-SEC   SECTION.
           MOVE    WRK-PARA-HOSPID      TO      SRY-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    WRK-PARA-A8          TO      SRY-SRYYM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACT-KEY4
                                        TO      MCP-PATH
           DISPLAY "take real act  select "
                                WRK-PARA-HOSPID WRK-PTID SRY-SRYYM
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
           DISPLAY "take act select ok   "
               MOVE    PATH-TBL-SRYACT-KEY4
                                        TO      MCP-PATH
               PERFORM  900-ACTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACT         =    1)
           DISPLAY "take act delete loop   "
                   MOVE    SRYACT-REC  TO      MCPDATA-REC
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SRYACT-KEY
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   MOVE    PATH-TBL-SRYACT-KEY4
                                        TO      MCP-PATH
                   PERFORM  900-ACTPRV-READ-SEC
               END-PERFORM
           END-IF 
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-SRYACT-KEY4
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       200111-REAL-SRYACY-DB-DELETE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    実ＤＢ更新処理
      *****************************************************************
       20011-REAL-DB-MAKE-SEC        SECTION.
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db make st  "
                   " "  LNK-TIMEOFDAY(1:20)
           PERFORM 20011-REAL-DB-DELETE-SEC
      *収納マスタ関係は、負担金を捨てるということで一時コメントアウト
      *※ 但し、収納金額を見て負担者番号の出し入れをしているのも無視
      *   される。(どうする)
      *収納マスタ
      **   MOVE    WRK-PARA-HOSPID      TO      SYU-HOSPID
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SYU-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SYU-PTID
      **   MOVE    SYUNOU-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syunou read "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUNOUPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUNOU             =    1)
      **           MOVE    SYUNOU-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUNOU-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUNOU-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUNOUPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syunou read err  "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *収納明細マスタ
      **   MOVE    WRK-PARA-HOSPID      TO      SMEI-HOSPID
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SMEI-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SMEI-PTID
      **   MOVE    SYUMEI-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syumei read  "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUMEIPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUMEI             =    1)
      **           MOVE    SYUMEI-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUMEI-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUMEI-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUMEIPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syumei read err  "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *--------------------------------------------------
      *収納トータルマスタ
      **   MOVE    WRK-PARA-HOSPID      TO      SYUTTL-HOSPID
      **   MOVE    WRK-PARA-NYUGAIKBN   TO      SYUTTL-NYUGAIKBN
      **   MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
      **   MOVE    WRK-PTID             TO      SYUTTL-PTID
      **   MOVE    SYUTTL-REC           TO      MCPDATA-REC
      **   MOVE    "DBSELECT"           TO      MCP-FUNC
      **   MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **   DISPLAY "take syuttl read  "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   CALL    "MCPSUB"             USING
      **                                MCPAREA
      **                                MCPDATA-REC
      **   IF      MCP-RC               =       ZERO
      **       MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **       PERFORM  900-SYUTTLPRV-READ-SEC
      **       PERFORM  UNTIL
      **                (FLG-SYUTTL             =    1)
      **           MOVE    SYUTTL-REC   TO      MCPDATA-REC
      **           PERFORM 20011-SYUMEI-DB-MAKE-SEC
      **           MOVE    PATH-TBL-SYUTOTAL-PRV-KEY2
      **                                TO      MCP-PATH
      **           PERFORM  900-SYUTTLPRV-READ-SEC
      **       END-PERFORM
      **   ELSE
      **                DISPLAY "take syuttl read err  "
      **                        WRK-PARA-HOSPID WRK-PTID
      **   END-IF
      *
      *--------------------------------------------------
      *会計マスタ
           MOVE    WRK-PARA-HOSPID      TO      ACCT-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACCT-PRV-KEY40
                                        TO      MCP-PATH
           DISPLAY "take acct read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-SRYACCT-PRV-KEY40
                                        TO      MCP-PATH
               PERFORM  900-ACCTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACCT         =    1)
           DISPLAY "take acct insert   "
                   MOVE    SRYACCT-REC  TO      MCPDATA-REC
                   PERFORM 20011-SRYACCT-DB-MAKE-SEC
                   MOVE    PATH-TBL-SRYACCT-PRV-KEY40
                                        TO      MCP-PATH
                   PERFORM  900-ACCTPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take acct read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-SRYACCT-PRV-KEY40
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *診療行為マスタ
           MOVE    WRK-PARA-HOSPID      TO      SRY-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACT-PRV-KEY12
                                        TO      MCP-PATH
           DISPLAY "take act read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-SRYACT-PRV-KEY12
                                        TO      MCP-PATH
               PERFORM  900-ACTPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYACT         =    1)
           DISPLAY "take act insert   "
                   MOVE    SRYACT-REC  TO      MCPDATA-REC
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SRYACT-KEY
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   MOVE    PATH-TBL-SRYACT-PRV-KEY12
                                        TO      MCP-PATH
                   PERFORM  900-ACTPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take act read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-SRYACT-PRV-KEY12
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *算定履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      SANTEI-HOSPID
      *    MOVE    WRK-PARA-NYUGAIKBN   TO      SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO      SANTEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID        TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-SANTEI-PRV-KEY2
                                        TO      MCP-PATH
           DISPLAY "take santei read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-SANTEI-PRV-KEY2
                                        TO      MCP-PATH
               PERFORM  900-SANTEIPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SANTEI         =    1)
                   MOVE    SANTEI-REC   TO      MCPDATA-REC
                DISPLAY "take para8 = "       WRK-PARA-A8
                DISPLAY "take santei  " SANTEI-SRYYM 
                IF SANTEI-SRYYM         <       WRK-PARA-A8
                   CONTINUE
                ELSE
                   MOVE    "DBINSERT"   TO      MCP-FUNC
                   MOVE    PATH-TBL-SANTEI-KEY
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   DISPLAY "take santei insert "
                END-IF
                   MOVE    PATH-TBL-SANTEI-PRV-KEY2
                                        TO      MCP-PATH
                   PERFORM  900-SANTEIPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take santei read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-SANTEI-PRV-KEY2
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *受診履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      JYURRK-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-JYURRK-PRV-KEY30
                                        TO      MCP-PATH
           DISPLAY "take jyurrk read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-JYURRK-PRV-KEY30
                                        TO      MCP-PATH
               PERFORM  900-JYURRKPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-JYURRK         =    1)
           DISPLAY "take jyurrk insert   "
                   MOVE    JYURRK-REC   TO      MCPDATA-REC
                   PERFORM  20011-JYURRK-DB-MAKE-SEC
                   MOVE    PATH-TBL-JYURRK-PRV-KEY30
                                        TO      MCP-PATH
                   PERFORM  900-JYURRKPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take jyurrk read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-JYURRK-PRV-KEY30
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *診療科履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      KARRK-HOSPID
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      KARRK-PTID
           MOVE    SRYKARRK-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYKARRK-PRV-KEY2
                                        TO      MCP-PATH
           DISPLAY "take srykarrk read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-SRYKARRK-PRV-KEY2
                                        TO      MCP-PATH
               PERFORM  900-SRYKARRKPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-SRYKARRK      =    1)
                   MOVE    SRYKARRK-REC TO      MCPDATA-REC
               IF  KARRK-CREYMD         =       "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
               ELSE
                   IF  KARRK-CREYMD     =       "88888888"
                       MOVE    "DBUPDATE"   TO      MCP-FUNC
                   ELSE
                       MOVE    "DBINSERT"   TO      MCP-FUNC
                   END-IF
               END-IF
                   MOVE    PATH-TBL-SRYKARRK-KEY
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   MOVE    PATH-TBL-SRYKARRK-PRV-KEY2
                                        TO      MCP-PATH
                   PERFORM  900-SRYKARRKPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take srykarrk read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-SRYKARRK-PRV-KEY2
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *患者コメントマスタ
           MOVE    WRK-PARA-HOSPID      TO      PTCOM-HOSPID
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTCOM-PTID
           MOVE    PTCOM-REC            TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    PATH-TBL-PTCOM-PRV-KEY3
                                        TO      MCP-PATH
           DISPLAY "take ptcom read   "
                                WRK-PARA-HOSPID WRK-PTID
           CALL    "MCPSUB"             USING
                                        MCPAREA
                                        MCPDATA-REC
      *
           MOVE    ZERO                 TO      WRK-ZAINUM
      *
           IF      MCP-RC               =       ZERO
               MOVE    PATH-TBL-PTCOM-PRV-KEY3
                                        TO      MCP-PATH
               PERFORM  900-PTCOMPRV-READ-SEC
               PERFORM  UNTIL
                        (FLG-PTCOM      =    1)
               IF  WRK-ZAINUM       NOT =       PTCOM-ZAINUM
                   DISPLAY "take ptcom org del start = "
                                                PTCOM-ZAINUM
                   MOVE    PTCOM-ZAINUM TO      WRK-ZAINUM
                   MOVE    PTCOM-REC    TO      MCPDATA-REC
                   MOVE    "DBDELETE"   TO      MCP-FUNC
                   MOVE    PATH-TBL-PTCOM-DEL11
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   IF      MCP-RC       NOT  =  ZERO
                       DISPLAY "take ptcom del er " PTCOM-REC(1:100)
                   END-IF
               END-IF
                   MOVE    PTCOM-REC    TO      MCPDATA-REC
               IF  PTCOM-CREYMD         =       "99999999"
                   MOVE    "DBDELETE"   TO      MCP-FUNC
               ELSE
                   IF  PTCOM-CREYMD     =       "88888888"
                       MOVE    "DBUPDATE"   TO      MCP-FUNC
                   ELSE
                       MOVE    "DBINSERT"   TO      MCP-FUNC
                   END-IF
               END-IF
                   MOVE    PATH-TBL-PTCOM-KEY
                                        TO      MCP-PATH
                   PERFORM  900-REALDB-INS-SEC
                   IF      MCP-RC       NOT  =  ZERO
                       DISPLAY "take ptcom ins er " PTCOM-REC(1:100)
                   END-IF
                   MOVE    PATH-TBL-PTCOM-PRV-KEY3
                                        TO      MCP-PATH
                   PERFORM  900-PTCOMPRV-READ-SEC
               END-PERFORM
           ELSE
                        DISPLAY "take ptcom read err  "
                                WRK-PARA-HOSPID WRK-PTID
           END-IF
      *--------------------------------------------------
      * ★ DBCLOSECURSOR
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    PATH-TBL-PTCOM-PRV-KEY3
                                       TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *--------------------------------------------------
      *
           CALL "timeofday" USING LNK-TIMEOFDAY
           DISPLAY "real db make ed  "
                   " "  LNK-TIMEOFDAY(1:20)
           .
       20011-REAL-DB-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOUPRV-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOUPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細読込
      *****************************************************************
       900-SYUMEIPRV-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
               MOVE    ZERO            TO  FLG-SYUMEI
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEIPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納トータル読込
      *****************************************************************
       900-SYUTTLPRV-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUTTL-REC
               MOVE    ZERO            TO  FLG-SYUTTL
           ELSE
               INITIALIZE                  SYUTTL-REC
               MOVE    1               TO  FLG-SYUTTL
           END-IF
      *
           .
       900-SYUTTLPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計読込
      *****************************************************************
       900-ACCTPRV-READ-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-ACCTPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-ACTPRV-READ-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-ACTPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRKPRV-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRKPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴読込
      *****************************************************************
       900-SANTEIPRV-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEIPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴読込
      *****************************************************************
       900-SRYKARRKPRV-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYKARRK-REC
               MOVE    ZERO            TO  FLG-SRYKARRK
           ELSE
               INITIALIZE                  SRYKARRK-REC
               MOVE    1               TO  FLG-SRYKARRK
           END-IF
      *
           .
       900-SRYKARRKPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       900-PTCOMPRV-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTCOM-REC
               MOVE    ZERO            TO  FLG-PTCOM 
           ELSE
               INITIALIZE                  PTCOM-REC
               MOVE    1               TO  FLG-PTCOM
           END-IF
      *
           .
       900-PTCOMPRV-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    本体DB更新処理
      *****************************************************************
       900-REALDB-INS-SEC            SECTION.
      *
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC         NOT  =   ZERO
               INITIALIZE                  MCPDATA-REC
               DISPLAY "INS ERR = " MCP-RC
           END-IF
      *
           .
       900-REALDB-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    テンポラリDB削除処理
      *****************************************************************
       20011-TMPDB-DELETE-SEC        SECTION.
           DISPLAY "take tmp db delete st"
      *収納マスタ
           MOVE    WRK-PARA-HOSPID      TO      SYU-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SYU-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SYU-PTID
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           DISPLAY "take tmp db delete syunou st = " SYU-PTID "," 
                                           SYU-HOSPID ","
                                           SYU-NYUGAIKBN
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SYUNOU-PRV-DELPRV
                                        TO      MCP-PATH
           PERFORM  900-REALDB-INS-SEC
      *
      *収納明細マスタ
           MOVE    WRK-PARA-HOSPID      TO      SMEI-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SMEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SMEI-PTID
           MOVE    SYUMEI-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SYUMEI-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete syumei st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *収納トータルマスタ
           MOVE    WRK-PARA-HOSPID      TO      SYUTTL-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SYUTTL-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SYUTTL-PTID
           MOVE    SYUTTL-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           DISPLAY "take tmp db delete syutotal st = " SYU-PTID "," 
           MOVE    PATH-TBL-SYUTOTAL-PRV-DELPRV
                                        TO      MCP-PATH
           PERFORM  900-REALDB-INS-SEC
      *
      *会計マスタ
           MOVE    WRK-PARA-HOSPID      TO      ACCT-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCT-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCT-PTID
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACCT-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete acct st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *診療行為マスタ
           MOVE    WRK-PARA-HOSPID      TO      SRY-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      SRY-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SRY-PTID
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACT-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete act st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *算定履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      SANTEI-HOSPID
      *    MOVE    WRK-PARA-NYUGAIKBN   TO      SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO      SANTEI-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      SANTEI-PTID
           MOVE    SANTEI-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SANTEI-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete santei st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *受診履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      JYURRK-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      JYURRK-PTID
           MOVE    JYURRK-REC           TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-JYURRK-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete jyurr st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *診療科履歴マスタ
           MOVE    WRK-PARA-HOSPID      TO      KARRK-HOSPID
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      KARRK-PTID
           MOVE    SRYKARRK-REC         TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYKARRK-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete srykarr st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *患者コメントマスタ
           MOVE    WRK-PARA-HOSPID      TO      PTCOM-HOSPID
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTCOM-PTID
           MOVE    PTCOM-REC            TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-PTCOM-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete ptcom st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
      *患者マスタ
           MOVE    WRK-PARA-HOSPID      TO      PTINF-HOSPID
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      PTINF-PTID
           MOVE    PTINF-REC            TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-PTINF-PRV-DELPRV
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete ptinf st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *診療行為付加情報マスタ
           MOVE    WRK-PARA-HOSPID      TO      ACCTP-HOSPID
           MOVE    WRK-PARA-NYUGAIKBN   TO      ACCTP-NYUGAIKBN
           MOVE    WRK-PARA-PTID         TO      WRK-PTID-X
           MOVE    WRK-PTID             TO      ACCTP-PTID
           MOVE    SRYACCTPLUS-REC      TO      MCPDATA-REC
           MOVE    "DBDELETE"           TO      MCP-FUNC
           MOVE    PATH-TBL-SRYACCTPLUS-PRV-DEL1
                                        TO      MCP-PATH
           DISPLAY "take tmp db delete ptinf st = " SYU-PTID "," 
           PERFORM  900-REALDB-INS-SEC
      *
           .
       20011-TMPDB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRVKANRI-FET-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC

           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRVKANRI-REC
               MOVE    ZERO                TO  FLG-PRVKANRI
           ELSE
               MOVE    1                   TO  FLG-PRVKANRI
           END-IF
           .
       900-PRVKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-PATH.
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
           DISPLAY "DBOPEN RC= " MCP-RC
      *
           MOVE    LOW-VALUE           TO  MCP-PATH.
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
           DISPLAY "DBSTART RC= " MCP-RC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
       
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *    MOVE    "ROLLBACK"          TO  MCP-FUNC.
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC.
      *    DISPLAY "rollback mcp-rc = " MCP-RC
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       900-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                           MCPAREA
                                           MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
