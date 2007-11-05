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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHCM35.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト件数枚数印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/08/20    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-森脇    06/01/20  編綴分対応
      *  03.04.01    NACL-森脇    06/11/29  パラメタ変更
      *  03.05.00    NACL-森脇    07/11/05  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE            ASSIGN  HC01PARA
                                       FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCMC001.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-TPAGE               PIC 9(10).
           03  CNT-TKENSU              PIC 9(10).
           03  CNT-GPAGE               PIC 9(10).
           03  CNT-GKENSU              PIC 9(10).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
      *----(00.00.01)--ADD-START---
           03  IDY                     PIC 9(04).
      *----(00.00.01)--ADD-END-----
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z10                 PIC ZZZZZZZZZ9.
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       01  PARA-SYAPRTNM               PIC X(16).
      *----(03.04.01)--UPD-START---
       COPY    "R02SCR-SPA".
      * 01  PARA-FILEKBN                PIC X(40).
      * 01  PARA-TBL1.
      *----(00.00.01)--UPD-START---
      *     03  PARA-TBL-OCC            OCCURS   21.
      *----(00.00.01)--UPD-END-----
      *         05  PARA-TBL-G          OCCURS   36.
      *             07  PARA-TPAGE      PIC 9(04).
      *             07  PARA-TKENSU     PIC 9(04).
      *             07  PARA-TSYURUINAME
      *                                 PIC X(40).
      *----(03.04.01)--UPD-END-----
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       SPA-AREA
                                       PARA-SYAPRTNM
      *----(03.04.01)--UPD-START---
                                       SPA-R02.
      *                                 PARA-FILEKBN
      *                                 PARA-TBL1.
      *----(03.04.01)--UPD-END-----
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    0                   TO  CNT-GPAGE
           MOVE    0                   TO  CNT-GKENSU
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
      *
      *    帳票見出編集処理
           PERFORM 220-PRT-PAGE-SEC
      *
      *    社保
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   24
               ADD     1                   TO  CNT-LINE
               MOVE    SPA-GMN-TSYURUINAME(1 IDX)
                                           TO  HCMC001-NAME(CNT-LINE)
               MOVE    SPA-GMN-TKENSU(1 IDX)   TO  WRK-Z10
               MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
               MOVE    SPA-GMN-TPAGE(1 IDX)    TO  WRK-Z10
               MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
               ADD     SPA-GMN-TPAGE(1 IDX)    TO  CNT-TPAGE
               ADD     SPA-GMN-TKENSU(1 IDX)   TO  CNT-TKENSU
           END-PERFORM
      *
           ADD     2                   TO  CNT-LINE
           MOVE    "社保　小計"        TO  HCMC001-NAME(CNT-LINE)
           MOVE    CNT-TKENSU          TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    CNT-TPAGE           TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
      *
           MOVE    0                   TO  CNT-TPAGE
           MOVE    0                   TO  CNT-TKENSU
           ADD     1                   TO  CNT-LINE
      *
      *    国保
      *----(00.00.01)--UPD-START---
           PERFORM VARYING IDY     FROM    2   BY  1
                   UNTIL   IDY     >   21
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   36
                   OR      SPA-GMN-TSYURUINAME(IDY IDX) =  SPACE
               IF      CNT-LINE    >=  35
      *            印刷処理
                   PERFORM 400-PRINT-OUT-SEC
      *            帳票見出編集処理
                   PERFORM 220-PRT-PAGE-SEC
               END-IF
               ADD     1                     TO  CNT-LINE
               MOVE    SPA-GMN-TSYURUINAME(IDY IDX)
                                             TO  HCMC001-NAME(CNT-LINE)
               MOVE    SPA-GMN-TKENSU(IDY IDX)   TO  WRK-Z10
               MOVE    WRK-Z10           TO  HCMC001-NAIYO(CNT-LINE 1)
               MOVE    SPA-GMN-TPAGE(IDY IDX)    TO  WRK-Z10
               MOVE    WRK-Z10           TO  HCMC001-NAIYO(CNT-LINE 2)
               ADD     SPA-GMN-TPAGE(IDY IDX)    TO  CNT-TPAGE
               ADD     SPA-GMN-TKENSU(IDY IDX)   TO  CNT-TKENSU
           END-PERFORM
           END-PERFORM
      *
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    "国保　小計"        TO  HCMC001-NAME(CNT-LINE)
           MOVE    CNT-TKENSU          TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    CNT-TPAGE           TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
      *
           ADD     CNT-TPAGE           TO  CNT-GPAGE
           ADD     CNT-TKENSU          TO  CNT-GKENSU
      *
      *    特別療養費
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    SPA-GMN-TSYURUINAME(1 34)
                                       TO  HCMC001-NAME(CNT-LINE)
           MOVE    SPA-GMN-TKENSU(1 34)    TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    SPA-GMN-TPAGE(1 34)     TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
           ADD     SPA-GMN-TPAGE(1 34)     TO  CNT-GPAGE
           ADD     SPA-GMN-TKENSU(1 34)    TO  CNT-GKENSU
      *
      *    自費
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    SPA-GMN-TSYURUINAME(1 35)
                                       TO  HCMC001-NAME(CNT-LINE)
           MOVE    SPA-GMN-TKENSU(1 35)    TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    SPA-GMN-TPAGE(1 35)     TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
           ADD     SPA-GMN-TPAGE(1 35)     TO  CNT-GPAGE
           ADD     SPA-GMN-TKENSU(1 35)    TO  CNT-GKENSU
      *
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
      *    不明
           ADD     1                   TO  CNT-LINE
           MOVE    SPA-GMN-TSYURUINAME(1 36)
                                       TO  HCMC001-NAME(CNT-LINE)
           MOVE    SPA-GMN-TKENSU(1 36)    TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    SPA-GMN-TPAGE(1 36)     TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
           ADD     SPA-GMN-TPAGE(1 36)     TO  CNT-GPAGE
           ADD     SPA-GMN-TKENSU(1 36)    TO  CNT-GKENSU
      *
      *    合計
           ADD     1                   TO  CNT-LINE
           IF      CNT-LINE    >=  35
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
      *        帳票見出編集処理
               PERFORM 220-PRT-PAGE-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    "合　　計"          TO  HCMC001-NAME(CNT-LINE)
           MOVE    CNT-GKENSU          TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 1)
           MOVE    CNT-GPAGE           TO  WRK-Z10
           MOVE    WRK-Z10             TO  HCMC001-NAIYO(CNT-LINE 2)
      *
      *----(00.00.01)--UPD-END-----
      *    印刷処理
           PERFORM 400-PRINT-OUT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-END-----
      *****************************************************************
      *    帳票<見出>編集処理
      *****************************************************************
       220-PRT-PAGE-SEC            SECTION.
      *
           INITIALIZE                  HCMC001
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-LINE
      *
      *    タイトル
      *----(03.04.01)--UPD-START---
           MOVE    SPA-GMN-FILEKBN     TO  HCMC001-DAI
      *     MOVE    PARA-FILEKBN        TO  HCMC001-DAI
      *
      *    印刷日付
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HCMC001-YMD
      *
      *    ページ数
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCMC001-PAGE
      *
      *    見出し
           MOVE    "レセプト種別"      TO  HCMC001-TAI-NAME
           MOVE    "患者数"            TO  HCMC001-TAI-NAIYO(1)
           MOVE    "レセプト枚数"      TO  HCMC001-TAI-NAIYO(2)
      *
           .
       220-PRT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
      *    印刷用ファイル作成
      *----(03.05.00)--ADD-START---
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
      *----(03.05.00)--ADD-END-----
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE            TO  HC01PARA-CNT
           OPEN    OUTPUT              PRT-FILE
           WRITE   HCMC001
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           MOVE    "HCMC001.red"       TO  MKPRT-DIA
           MOVE    SPACE               TO  MKPRT-DEF
           MOVE    HC01PARA            TO  MKPRT-DAT
           MOVE    PARA-SYAPRTNM       TO  MKPRT-PRTNM
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCHCM35 END ***"
           .
       300-END-EXT.
           EXIT.
      *
