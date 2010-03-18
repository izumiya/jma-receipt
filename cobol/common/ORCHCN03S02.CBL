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
       PROGRAM-ID.             ORCHCN03S02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院請求書兼領収書　まとめサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/07/03    NACL−太田    新規作成
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
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-TARGET          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX-SYU             PIC 9(05).
           03  IDX-DENPNUM         PIC 9(05).
           03  IDX-SRYKBN          PIC 9(05).
           03  IDX-JIHI            PIC 9(05).
      *
       01  KEY-AREA.
           03  KEY-PTID            PIC 9(10).
           03  KEY-DENPNUM         PIC 9(07).
           03  KEY-RRKNUM          PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SYU             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-COMP-KEY            OCCURS  2.
               05  WRK-COMP-SKYEDYMD   PIC X(08).
               05  WRK-COMP-DENPNUM    PIC 9(07).
           03  WRK-DENPNUM             PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-SRYKBN-MAX        PIC 9(03)   VALUE 17.
           03  CONST-GRP-MAX           PIC 9(03)   VALUE 10.
      *
       01  MYSYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //MYSYU-//.
      *
      *    定期請求履歴
       01  MYTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC"  REPLACING   //PTTEIKIRRK-//
                                       BY          //MYTEIKIRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCHCN03.INC".
           COPY    "CPORCHCN03S02.INC".
       01  LNKSYU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //LNKSYU-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           ORCHCN03S02AREA
           LNKSYU-REC
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
                                       IDX-AREA
                                       CNT-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ORCHCN03-PTID (ORCHCN03S02-IDX)
                                   TO  KEY-PTID
           MOVE    ORCHCN03-DENPNUM (ORCHCN03S02-IDX)
                                   TO  KEY-DENPNUM
           MOVE    LNKSYU-REC      TO  MYSYU-REC
      *
           INITIALIZE                  MYTEIKIRRK-REC
           IF    ( MYSYU-CREATEKBN =   "2" )
               PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
               MOVE    PTTEIKIRRK-REC
                                   TO  MYTEIKIRRK-REC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( MYSYU-DENPJTIKBN    =   "3" )
            OR   ( MYSYU-SYUHKNNUM     =   "971" OR "973" OR "975" )
            OR   ( MYSYU-GRP-HAKHOUFLG =   0 )
            OR   ( ORCHCN03-SKYPRTFLG  =   1 )
               MOVE    MYSYU-REC       TO  SYUNOU-REC
               PERFORM 2001-SUM-SYUNOU-SEC
           ELSE
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU     >   ORCHCN03-CNT )
                        OR   ( FLG-END     =   1 )
      *
                   MOVE    ORCHCN03-PTID (IDX-SYU)
                                               TO  KEY-PTID
                   MOVE    ORCHCN03-DENPNUM (IDX-SYU)
                                               TO  KEY-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   INITIALIZE                      PTTEIKIRRK-REC
                   IF    ( SYU-CREATEKBN       =   "2" )
                       PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
                   END-IF
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       IF    ( IDX-SYU     <   ORCHCN03S02-IDX )
                           MOVE    1           TO  FLG-END
                       ELSE
                           MOVE    ORCHCN03-PTID (IDX-SYU)
                                       TO  KEY-PTID
                           MOVE    ORCHCN03-DENPNUM (IDX-SYU)
                                       TO  KEY-DENPNUM
                           PERFORM 2001-SUM-SYUNOU-SEC
      *
                           IF    ( IDX-SYU =    ORCHCN03S02-IDX)
                            OR   ( ORCHCN03-HAKKOFLG (ORCHCN03S02-IDX)
                                           =   1 )

                               CONTINUE
                           ELSE
                             COMPUTE ORCHCN03-SEIKYU-TAX-SAI
                                                 (ORCHCN03S02-IDX)
                                 =   ORCHCN03-SEIKYU-TAX-SAI
                                                 (ORCHCN03S02-IDX)
                                 +   ORCHCN03-SEIKYU-TAX-SAI (IDX-SYU)
      *
                             COMPUTE ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                 =   ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                                 +   ORCHCN03-SEIKYU (IDX-SYU)
      *
                             COMPUTE ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                                 =   ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                                 +   ORCHCN03-NYUKIN (IDX-SYU)
      *
                             COMPUTE ORCHCN03-KONMISYU (ORCHCN03S02-IDX)
                                 =   ORCHCN03-KONMISYU (ORCHCN03S02-IDX)
                                 +   ORCHCN03-KONMISYU (IDX-SYU)
                           END-IF
                       END-IF
      *
                   END-IF
               END-PERFORM
      *
               EVALUATE    MYSYU-CREATEKBN
               WHEN    "1"
                   PERFORM 2001-CHK-TAIIN-SEC
               WHEN    "2"
                   PERFORM 2001-CHK-TEIKI-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象チェック処理
      *****************************************************************
       2001-CHK-TARGET-SEC             SECTION.
      *
           MOVE    ZERO        TO  FLG-TARGET
      *
           EVALUATE    MYSYU-GRP-HAKHOUFLG
           WHEN    1
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                AND  ( SYU-SRYYMD(1:6) =   MYSYU-SRYYMD (1:6))
                   MOVE    1   TO  FLG-TARGET
      *
               END-IF
           WHEN    2
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                AND  ( SYU-HKNCOMBINUM =   MYSYU-HKNCOMBINUM )
                   MOVE    1   TO  FLG-TARGET
               END-IF
           WHEN    3
               IF    ( SYU-PTID        =   MYSYU-PTID )
                AND  ( SYU-CREATEKBN   =   MYSYU-CREATEKBN )
                   MOVE    1   TO  FLG-TARGET
               END-IF
           END-EVALUATE
      *
           IF    ( MYSYU-CREATEKBN =   "2" )
               IF    ( PTTEIKIRRK-KBT  =   MYTEIKIRRK-KBT )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-TARGET
               END-IF
           END-IF
      *
           IF    ( SYU-SYUHKNNUM       =   "971" OR "973" OR "975" )
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( SYU-NYUIN-RRKNUM    =   MYSYU-NYUIN-RRKNUM )
               CONTINUE
           ELSE
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( SYU-GRP-HAKHOUFLG   =   MYSYU-GRP-HAKHOUFLG )
               CONTINUE
           ELSE
                   MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           .
       2001-CHK-TARGET-EXT.
           EXIT.
      *****************************************************************
      *    退院請求チェック処理
      *****************************************************************
       2001-CHK-TAIIN-SEC            SECTION.
      *
           PERFORM 900-SYUNOU-KEY71-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    ZERO            TO IDX-DENPNUM
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   ORCHCN03-CNT )
                        OR   ( IDX-DENPNUM NOT =   ZERO )
      *
                   IF    ( SYU-PTID    =   ORCHCN03-PTID   (IDX-SYU))
                    AND  ( SYU-DENPNUM =   ORCHCN03-DENPNUM(IDX-SYU))
                       MOVE    IDX-SYU TO  IDX-DENPNUM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-DENPNUM     =   ZERO )
                   INITIALIZE                  PTTEIKIRRK-REC
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       PERFORM 2001-SUM-SYUNOU-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-KEY71-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key71"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-CHK-TAIIN-EXT.
           EXIT.
      *****************************************************************
      *    定期請求チェック処理
      *****************************************************************
       2001-CHK-TEIKI-SEC            SECTION.
      *
           PERFORM 900-PTTEIKIRRK-KEY12-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK NOT =   ZERO )
      *
               MOVE    ZERO            TO IDX-DENPNUM
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   ORCHCN03-CNT )
                        OR   ( IDX-DENPNUM NOT =   ZERO )
      *
                   IF    ( PTTEIKIRRK-PTID
                                       =   ORCHCN03-PTID   (IDX-SYU))
                    AND  ( PTTEIKIRRK-DENPNUM
                                       =   ORCHCN03-DENPNUM(IDX-SYU))
                       MOVE    IDX-SYU TO  IDX-DENPNUM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-DENPNUM     =   ZERO )
                   MOVE    PTTEIKIRRK-PTID         TO  KEY-PTID
                   MOVE    PTTEIKIRRK-DENPNUM      TO  KEY-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   PERFORM 2001-CHK-TARGET-SEC
                   IF    ( FLG-TARGET  =   1 )
                       PERFORM 2001-SUM-SYUNOU-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-PTTEIKIRRK-KEY12-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-CHK-TEIKI-EXT.
           EXIT.
      *****************************************************************
      *    収納集計処理
      *****************************************************************
       2001-SUM-SYUNOU-SEC             SECTION.
      *
           COMPUTE CNT-SYU =   CNT-SYU +   1
      *
           IF    ( CNT-SYU =   1 )
               MOVE    SYUNOU-REC      TO  LNKSYU-REC
               MOVE    LNKSYU-SKYEDYMD TO  WRK-COMP-SKYEDYMD (1)
               MOVE    LNKSYU-DENPNUM  TO  WRK-COMP-DENPNUM  (1)
      *
           ELSE
      *
               PERFORM VARYING IDX-SRYKBN  FROM    1   BY  1
                       UNTIL ( IDX-SRYKBN  >   CONST-SRYKBN-MAX )
      *
                   COMPUTE LNKSYU-HKNTEN (IDX-SRYKBN)
                       =   LNKSYU-HKNTEN (IDX-SRYKBN)
                       +   SYU-HKNTEN    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-MONEY (IDX-SRYKBN)
                       =   LNKSYU-MONEY (IDX-SRYKBN)
                       +   SYU-MONEY    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-TGMONEY (IDX-SRYKBN)
                       =   LNKSYU-TGMONEY (IDX-SRYKBN)
                       +   SYU-TGMONEY    (IDX-SRYKBN)
      *
                   COMPUTE LNKSYU-TGMONEY-TAX (IDX-SRYKBN)
                       =   LNKSYU-TGMONEY-TAX (IDX-SRYKBN)
                       +   SYU-TGMONEY-TAX (IDX-SRYKBN)
      *
               END-PERFORM
      *
               COMPUTE LNKSYU-SHOHOU-SAI
                   =   LNKSYU-SHOHOU-SAI
                   +   SYU-SHOHOU-SAI
      *
               COMPUTE LNKSYU-TGMONEY-TAX-SAI
                   =   LNKSYU-TGMONEY-TAX-SAI
                   +   SYU-TGMONEY-TAX-SAI
      *
               PERFORM VARYING IDX-JIHI    FROM   1    BY  1
                       UNTIL( IDX-JIHI    >   10 )
                   COMPUTE LNKSYU-JIHI-TAXNASI(IDX-JIHI)
                        =  LNKSYU-JIHI-TAXNASI(IDX-JIHI)
                        +  SYU-JIHI-TAXNASI   (IDX-JIHI)
      *
                   COMPUTE LNKSYU-JIHI-TAXARI(IDX-JIHI)
                        =  LNKSYU-JIHI-TAXARI(IDX-JIHI)
                        +  SYU-JIHI-TAXARI   (IDX-JIHI)
               END-PERFORM
      *
               COMPUTE LNKSYU-JIHI-TOTAL
                   =   LNKSYU-JIHI-TOTAL
                   +   SYU-JIHI-TOTAL
      *
               COMPUTE LNKSYU-JIHI-TOTAL-TAX
                   =   LNKSYU-JIHI-TOTAL-TAX
                   +   SYU-JIHI-TOTAL-TAX
      *
               COMPUTE LNKSYU-JIHI-TAX
                   =   LNKSYU-JIHI-TAX
                   +   SYU-JIHI-TAX
      *
               COMPUTE LNKSYU-RJN-FTN
                   =   LNKSYU-RJN-FTN
                   +   SYU-RJN-FTN
      *
               COMPUTE LNKSYU-KOH-FTN
                   =   LNKSYU-KOH-FTN
                   +   SYU-KOH-FTN
      *
               COMPUTE LNKSYU-YKZ-FTN
                   =   LNKSYU-YKZ-FTN
                   +   SYU-YKZ-FTN
      *
               COMPUTE LNKSYU-CHOSEI
                   =   LNKSYU-CHOSEI
                   +   SYU-CHOSEI
      *
               COMPUTE LNKSYU-HKNTEKMONEY
                   =   LNKSYU-HKNTEKMONEY
                   +   SYU-HKNTEKMONEY
      *
               COMPUTE LNKSYU-DISCOUNT-MONEY
                   =   LNKSYU-DISCOUNT-MONEY
                   +   SYU-DISCOUNT-MONEY
      *
               COMPUTE LNKSYU-RSISHOSHIN-MONEY
                   =   LNKSYU-RSISHOSHIN-MONEY
                   +   SYU-RSISHOSHIN-MONEY
      *
               COMPUTE LNKSYU-RSISAISHIN-MONEY
                   =   LNKSYU-RSISAISHIN-MONEY
                   +   SYU-RSISAISHIN-MONEY
      *
               COMPUTE LNKSYU-RSISDO-MONEY
                   =   LNKSYU-RSISDO-MONEY
                   +   SYU-RSISDO-MONEY
      *
               COMPUTE LNKSYU-RSIETC-MONEY
                   =   LNKSYU-RSIETC-MONEY
                   +   SYU-RSIETC-MONEY
      *
               COMPUTE LNKSYU-RSI-TAX-SAI
                   =   LNKSYU-RSI-TAX-SAI
                   +   SYU-RSI-TAX-SAI
      *
               COMPUTE LNKSYU-RSI-TOTAL
                   =   LNKSYU-RSI-TOTAL
                   +   SYU-RSI-TOTAL
      *
               COMPUTE LNKSYU-RSIJIBAI-SKYMONEY
                   =   LNKSYU-RSIJIBAI-SKYMONEY
                   +   SYU-RSIJIBAI-SKYMONEY
      *
               COMPUTE LNKSYU-SKYMONEY-TAX-SAI
                   =   LNKSYU-SKYMONEY-TAX-SAI
                   +   SYU-SKYMONEY-TAX-SAI
      *
               COMPUTE LNKSYU-SKYMONEY
                   =   LNKSYU-SKYMONEY
                   +   SYU-SKYMONEY
      *
               COMPUTE LNKSYU-RMSAGAKU
                   =   LNKSYU-RMSAGAKU
                   +   SYU-RMSAGAKU
      *
               COMPUTE LNKSYU-RMSAGAKU-TAX-SAI
                   =   LNKSYU-RMSAGAKU-TAX-SAI
                   +   SYU-RMSAGAKU-TAX-SAI
      *
               COMPUTE LNKSYU-RYOYOHI-SKJ
                   =   LNKSYU-RYOYOHI-SKJ
                   +   SYU-RYOYOHI-SKJ
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ
                   =   LNKSYU-SKYMONEY-SKJ
                   +   SYU-SKYMONEY-SKJ
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-TAX
                   =   LNKSYU-SKYMONEY-SKJ-TAX
                   +   SYU-SKYMONEY-SKJ-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-KEI
                   =   LNKSYU-SKYMONEY-SKJ-KEI
                   +   SYU-SKYMONEY-SKJ-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-SKJ-JIHI
                   =   LNKSYU-RYOYOHI-SKJ-JIHI
                   +   SYU-RYOYOHI-SKJ-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI
                   =   LNKSYU-SKYMONEY-SKJ-JIHI
                   +   SYU-SKYMONEY-SKJ-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI-TAX
                   =   LNKSYU-SKYMONEY-SKJ-JIHI-TAX
                   +   SYU-SKYMONEY-SKJ-JIHI-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-SKJ-JIHI-KEI
                   =   LNKSYU-SKYMONEY-SKJ-JIHI-KEI
                   +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-LIFE
                   =   LNKSYU-RYOYOHI-LIFE
                   +   SYU-RYOYOHI-LIFE
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE
                   =   LNKSYU-SKYMONEY-LIFE
                   +   SYU-SKYMONEY-LIFE
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-TAX
                   =   LNKSYU-SKYMONEY-LIFE-TAX
                   +   SYU-SKYMONEY-LIFE-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-KEI
                   =   LNKSYU-SKYMONEY-LIFE-KEI
                   +   SYU-SKYMONEY-LIFE-KEI
      *
               COMPUTE LNKSYU-RYOYOHI-LIFE-JIHI
                   =   LNKSYU-RYOYOHI-LIFE-JIHI
                   +   SYU-RYOYOHI-LIFE-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI
                   =   LNKSYU-SKYMONEY-LIFE-JIHI
                   +   SYU-SKYMONEY-LIFE-JIHI
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI-TAX
                   =   LNKSYU-SKYMONEY-LIFE-JIHI-TAX
                   +   SYU-SKYMONEY-LIFE-JIHI-TAX
      *
               COMPUTE LNKSYU-SKYMONEY-LIFE-JIHI-KEI
                   =   LNKSYU-SKYMONEY-LIFE-JIHI-KEI
                   +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
           END-IF
      *
           IF    ( LNKSYU-HKNCOMBINUM  =   SYU-HKNCOMBINUM )
               CONTINUE
           ELSE
               MOVE    1               TO  ORCHCN03S02-FUKUSU-COMBI
           END-IF
      *
           IF    ( LNKSYU-DENPNUM      =   SYU-DENPNUM )
               CONTINUE
           ELSE
               IF    ( ORCHCN03-HAKKOFLG (ORCHCN03S02-IDX)  =   1 )
                   COMPUTE ORCHCN03-SEIKYU-TAX-SAI (ORCHCN03S02-IDX)
                       =   ORCHCN03-SEIKYU-TAX-SAI (ORCHCN03S02-IDX)
                       +   SYU-SKYMONEY-TAX-SAI
      *
                   COMPUTE ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                       =   ORCHCN03-SEIKYU (ORCHCN03S02-IDX)
                       +   SYU-SKYMONEY
      *
                   COMPUTE ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                       =   ORCHCN03-NYUKIN (ORCHCN03S02-IDX)
                       +   SYU-NYUKIN-TOTAL
               END-IF
      *
           END-IF
      *
           IF    ( ORCHCN03S02-GRP-CNT >=  CONST-GRP-MAX )
               CONTINUE
           ELSE
               COMPUTE ORCHCN03S02-GRP-CNT
                   =   ORCHCN03S02-GRP-CNT +   1
               MOVE    SYU-DENPNUM TO  ORCHCN03S02-GRP-DENPNUM
                                              (ORCHCN03S02-GRP-CNT)
           END-IF
      *
           IF    ( WRK-DENPNUM         =   ZERO )
            OR   ( WRK-DENPNUM         >   SYU-DENPNUM )
               MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
           END-IF
      *
           IF    ( LNKSYU-SKYSTYMD     >   SYU-SKYSTYMD )
               MOVE    SYU-SKYSTYMD    TO  LNKSYU-SKYSTYMD
           END-IF
      *
           IF    ( LNKSYU-SKYEDYMD     <   SYU-SKYEDYMD )
               MOVE    SYU-SKYEDYMD    TO  LNKSYU-SKYEDYMD
           END-IF
      *
           COMPUTE ORCHCN03S02-MYMISYU
               =   ORCHCN03S02-MYMISYU
               +   SYU-SKYMONEY
               -   SYU-NYUKIN-TOTAL
      *
           MOVE    SYU-SKYEDYMD    TO  WRK-COMP-SKYEDYMD(2)
           MOVE    SYU-DENPNUM     TO  WRK-COMP-DENPNUM (2)
      *
           IF    ( WRK-COMP-KEY (1)    <   WRK-COMP-KEY(2))
               MOVE    WRK-COMP-KEY(2) TO  WRK-COMP-KEY(1)
               MOVE    SYU-ROOMNUM     TO  LNKSYU-ROOMNUM
               MOVE    SYU-BYOTONUM    TO  LNKSYU-BYOTONUM
               MOVE    SYU-SRYKA       TO  LNKSYU-SRYKA
               IF    ( SYU-DISCOUNT-TEIRITU   =   "1" OR "2" )
                   MOVE    SYU-DISCOUNT-KBN
                                       TO  LNKSYU-DISCOUNT-KBN
                   MOVE    SYU-DISCOUNT-BODY
                                       TO  LNKSYU-DISCOUNT-BODY
                   MOVE    SYU-DISCOUNT-RATEKBN
                                       TO  LNKSYU-DISCOUNT-RATEKBN
                   MOVE    SYU-DISCOUNT-TEIRITU
                                       TO  LNKSYU-DISCOUNT-TEIRITU
                   MOVE    SYU-DISCOUNT-RATE
                                       TO  LNKSYU-DISCOUNT-RATE
               END-IF
           END-IF
      *
           .
       2001-SUM-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           IF    ( CNT-SYU         >   ZERO )
               MOVE    1           TO  ORCHCN03S02-PRTFLG
               MOVE    WRK-DENPNUM TO  LNKSYU-DENPNUM
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "1"             TO  SYU-NYUGAIKBN
           MOVE    KEY-PTID        TO  SYU-PTID
           MOVE    KEY-DENPNUM     TO  SYU-DENPNUM
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY71-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    MYSYU-REC       TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key71"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY71-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタFETCH処理
      *****************************************************************
       900-SYUNOU-KEY71-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key71"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY71-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    KEY-PTID        TO  PTTEIKIRRK-PTID
           MOVE    KEY-DENPNUM     TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY12-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    MYSYU-PTID      TO  PTTEIKIRRK-PTID
           MOVE    MYSYU-DENPNUM   TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY12-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY12-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY12-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
