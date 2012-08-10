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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGLFIL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : ファイルセレクション
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/06/28    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       DATA                DIVISION.
       WORKING-STORAGE         SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "L01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "ORCA-BLOB".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ＤＢ検索
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "L01.INC".
           COPY    "L02.INC".
           COPY    "L03.INC".
           COPY    "L04.INC".
           COPY    "L07.INC".
           COPY    "L99.INC".
           COPY    "L100.INC".
           COPY    "LERR.INC".
           COPY    "LID1.INC".
           COPY    "LID2.INC".
           COPY    "LFIL.INC".
           COPY    "G01.INC".
           COPY    "G02.INC".
           COPY    "G03.INC".
           COPY    "G05.INC".
           COPY    "G07.INC".
           COPY    "G99.INC".
           COPY    "G100.INC".
           COPY    "GERR.INC".
           COPY    "GID1.INC".
           COPY    "GID2.INC".
           COPY    "GFIL.INC".
      ******************************************************************
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-L01KYOUTU
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
             WHEN     "LINK"           ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
             WHEN     "PUTG"           ALSO   "CLICKED"
               PERFORM 290-OTHER
           END-EVALUATE
           MOVE    SPA-L01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
      *
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       290-OTHER                SECTION.
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *     クリア
               WHEN    OTHER
                   MOVE    SPACE       TO  MCP-PUTTYPE
                   MOVE   "LFIL"       TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
           END-EVALUATE
           .
       290-OTHER-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                 SECTION.
      *
           MOVE    "L99"               TO  SPA-SAKIPG
           MOVE    "LFIL"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
      *    MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW          SECTION.
           MOVE   "PUTWINDOW"  TO  MCP-FUNC
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
