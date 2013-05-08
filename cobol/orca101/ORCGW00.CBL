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
       PROGRAM-ID.             ORCGW00.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
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
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W00.
           03  SPA-W00-AREA.
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-LIST-G.
                       09  SPA-GMN-LIST    OCCURS  200.
                           11  SPA-GMN-LIST-KANRICD     PIC X(04).
                           11  SPA-GMN-LIST-KBNCD       PIC X(08).
                           11  SPA-GMN-LIST-ITEMCD      PIC 9(05).
                           11  SPA-GMN-LIST-ITEMLENGTH  PIC 9(03).
                           11  SPA-GMN-LIST-ITEMTYPE    PIC 9(01).
                           11  SPA-GMN-LIST-NO          PIC X(02).
                           11  SPA-GMN-LIST-NAME        PIC X(40).
                           11  SPA-GMN-LIST-ATAI        PIC X(40).
                           11  SPA-GMN-LIST-YUKOSTYMD   PIC X(10).
                           11  SPA-GMN-LIST-YUKOEDYMD   PIC X(10).
                           11  SPA-NAI-LIST-YUKOSTYMD   PIC X(08).
                           11  SPA-NAI-LIST-YUKOEDYMD   PIC X(08).
                   07  SPA-GMN-LIST-MAX    PIC 9(05).
      *
               05  SPA-GMN-SELNUM      PIC 9(02).
      *
               05  SPA-GMN-NAIYO.
                   07  SPA-GMN-KANRICD     PIC X(04).
                   07  SPA-GMN-KBNCD       PIC X(08).
                   07  SPA-GMN-ITEMCD      PIC 9(05).
                   07  SPA-GMN-NAME        PIC X(40).
                   07  SPA-GMN-YUKOSTYMD   PIC X(10).
                   07  SPA-GMN-YUKOEDYMD   PIC X(10).
                   07  SPA-NAI-YUKOSTYMD   PIC X(08).
                   07  SPA-NAI-YUKOEDYMD   PIC X(08).
                   07  SPA-GMN-ITEMLENGTH  PIC 9(03).
                   07  SPA-GMN-ITEMTYPE    PIC 9(01).
      *
                   07  SPA-GMN-ATAI-G.
                       09  SPA-GMN-ATAI    PIC X(40).
                   07  SPA-GMN-ATAILIST    OCCURS  50.
                       09  SPA-GMN-ATAIL   PIC X(40).
                   07  SPA-GMN-ATAI-MAX    PIC 9(02).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-KANRICD     PIC X(04).
                   07  SPA-NAI-FLG         PIC 9(01).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYS-ITEM        PIC 9(01).
           03  FLG-SYS-VALUE       PIC 9(01).
           03  FLG-SYS-COMBO       PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
           03  IDY                 PIC 9(02).
           03  PAGE-MAX            PIC 9(01).
           03  PAGE-IDX            PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-PATHNAME        PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENFLG          PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理項目
           COPY  "CPSYSKANRIITEM.INC".
      *
      *    システム管理項目値
           COPY  "CPSYSKANRIVALUE.INC".
      *
      *    システム管理項目値選択肢
           COPY  "CPSYSKANRICOMBO.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "W01.INC".
           COPY    "W02.INC".
           COPY    "W03.INC".
           COPY    "W04.INC".
           COPY    "W05.INC".
           COPY    "W06.INC".
           COPY    "W07.INC". 
           COPY    "W08.INC". 
           COPY    "W09.INC".
           COPY    "W10.INC".
           COPY    "W11.INC".
           COPY    "W111.INC".
           COPY    "W12.INC".
           COPY    "W121.INC".
           COPY    "W122.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W201.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W32.INC".
           COPY    "W321.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W36.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W68.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
           COPY    "W00.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W00
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W00         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "W00"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-W00
      *
      *    リスト初期表示
           INITIALIZE                  SYS-ITEM-REC
           MOVE    SYS-ITEM-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-FETCH-SEC
           ELSE
               INITIALIZE                  SYS-ITEM-REC
               MOVE    1               TO  FLG-SYS-ITEM
           END-IF
      *
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL  (IDX             >=  50)
                   OR     (FLG-SYS-ITEM    =   1)
               MOVE    IDX             TO  SPA-GMN-LIST-NO(IDX)
               MOVE    SYS-ITEM-KANRICD
                                       TO  SPA-GMN-LIST-KANRICD(IDX)
      *
               STRING  SYS-ITEM-KANRICD    DELIMITED BY SIZE
                       " "                 DELIMITED BY SIZE
                       SYS-ITEM-ITEMNAME   DELIMITED BY SPACE
                                   INTO    SPA-GMN-LIST-NAME(IDX)
               END-STRING
      *
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-FETCH-SEC
           END-PERFORM
      *
           MOVE    IDX             TO  SPA-GMN-LIST-MAX
      *
           MOVE    1               TO  PAGE-IDX
           MOVE    1               TO  PAGE-MAX
           IF      IDX > 30
               COMPUTE PAGE-MAX    = ( IDX  /   30 )   +   1
           END-IF
      *
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           . 
       31O-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG        =   "OK"
               EVALUATE    SPA-WIDCD
                   WHEN    "1001"
                       PERFORM 490-KOUSHIN-SEC
                   WHEN    "1002"
                       PERFORM 490-SAKUJYO-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   IF      SPA-NAI-KANRICD     =   SPACE
                       PERFORM 210-BACK
                   ELSE
                       PERFORM 310-SPASET-SEC
                   END-IF
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   MOVE    "1002"      TO  SPA-WIDCD
      *        期間切替
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 400-KIRIKAE-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 400-MAEPAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 400-ATOPAGE-SEC
      *        更新
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 400-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY         ALSO    "LIST"
                   PERFORM 400-LIST-SEC
      *        番号選択
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 400-SELNUM-SEC
      *        管理コード
               WHEN    "ACTIVATE"  ALSO    "KANRICD"
                   MOVE    W00-KANRICD     TO  SPA-GMN-KANRICD
      *        区分コード
               WHEN    "ACTIVATE"  ALSO    "KBNCD"
                   MOVE    W00-KBNCD       TO  SPA-GMN-KBNCD
      *        値コード
               WHEN    "ACTIVATE"  ALSO    "ITEMCD"
                   MOVE    W00-ITEMCD      TO  SPA-GMN-ITEMCD
      *        値
               WHEN    "ACTIVATE"  ALSO    "ATAI"
                   PERFORM 400-ATAICHECK-SEC
      *        有効開始日付
               WHEN    "ACTIVATE"  ALSO    "YUKOSTYMD"
                   PERFORM 400-YUKOSTYMD-SEC
      *        有効終了日付
               WHEN    "ACTIVATE"  ALSO    "YUKOEDYMD"
                   PERFORM 400-YUKOEDYMD-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W00"           TO  SPA-MOTOPG
      *
           INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    切替処理
      *****************************************************************
       400-KIRIKAE-SEC             SECTION.
      *
           EVALUATE    SPA-NAI-FLG
               WHEN    ZERO
                   MOVE    1           TO  SPA-NAI-FLG
               WHEN    1
                   MOVE    ZERO        TO  SPA-NAI-FLG
           END-EVALUATE
      *
           PERFORM 400-LISTSET-SEC
      *
           .
       400-KIRIKAE-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       400-MAEPAGE-SEC             SECTION.
      *
           IF      PAGE-IDX    >   1
               COMPUTE PAGE-IDX    =   PAGE-IDX    -   1
           END-IF
      *
           INITIALIZE                  SPA-GMN-NAIYO
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       400-MAEPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       400-ATOPAGE-SEC             SECTION.
      *
           IF      PAGE-IDX    <   PAGE-MAX
               COMPUTE PAGE-IDX    =   PAGE-IDX    +   1
           END-IF
      *
           INITIALIZE                  SPA-GMN-NAIYO
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       400-ATOPAGE-EXT.
           EXIT.
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       400-LIST-SEC            SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >   SPA-GMN-LIST-MAX)
               IF      W00-LIST-SELECT(IDX)    =   "T"
                   COMPUTE IDY = ( PAGE-IDX - 1 ) *  30 + IDX
                   MOVE    IDY             TO  W00-SELNUM
               END-IF
           END-PERFORM
      *
          PERFORM 400-SELNUM-SEC
      *
           .
       400-LIST-EXT.
           EXIT.
      *****************************************************************
      *    番号選択処理
      *****************************************************************
       400-SELNUM-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-NAIYO
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    W00-SELNUM  TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO        TO  FLG-OK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >   SPA-GMN-LIST-MAX)
                   OR     (FLG-OK  =   1)
               IF      SPA-GMN-SELNUM  =   IDX
                   MOVE    1               TO  FLG-OK
                   COMPUTE IDY = ( IDX / 30 )
                   IF      IDX NOT = ( IDY * 30 )
                       COMPUTE IDY = ( IDX / 30 ) + 1
                       MOVE    IDY             TO  PAGE-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-KANRICD     =   SPACE
                   MOVE    SPA-GMN-LIST-KANRICD(SPA-GMN-SELNUM)
                                           TO  SPA-NAI-KANRICD
                   MOVE    0               TO  SPA-NAI-FLG
                   PERFORM 400-LISTSET-SEC
               ELSE
                   PERFORM 400-GMNSET-SEC
               END-IF
           END-IF
      *
           .
       400-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    リストセット処理
      *****************************************************************
       400-LISTSET-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-NAIYO
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           MOVE    1               TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-LIST-G
           MOVE    ZERO            TO  SPA-GMN-LIST-MAX
      *
           INITIALIZE                  SYS-ITEM-REC
           MOVE    SPA-NAI-KANRICD TO  SYS-ITEM-KANRICD
           MOVE    SYS-ITEM-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-FETCH-SEC
           ELSE
               INITIALIZE                  SYS-ITEM-REC
               MOVE    1               TO  FLG-SYS-ITEM
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL  (IDX             >   200)
                   OR     (FLG-SYS-ITEM    =   1)
               COMPUTE IDX = IDX + 1
               MOVE    IDX             TO  SPA-GMN-LIST-NO(IDX)
               MOVE    SYS-ITEM-KANRICD
                                       TO  SPA-GMN-LIST-KANRICD(IDX)
               MOVE    SYS-ITEM-KBNCD  TO  SPA-GMN-LIST-KBNCD(IDX)
               MOVE    SYS-ITEM-ITEMCD TO  SPA-GMN-LIST-ITEMCD(IDX)
               MOVE    SYS-ITEM-ITEMLENGTH
                                       TO  SPA-GMN-LIST-ITEMLENGTH(IDX)
               MOVE    SYS-ITEM-ITEMTYPE
                                       TO  SPA-GMN-LIST-ITEMTYPE(IDX)
               MOVE    SYS-ITEM-ITEMNAME
                                       TO  SPA-GMN-LIST-NAME(IDX)
               MOVE    "00000000"      TO  SPA-NAI-LIST-YUKOSTYMD(IDX)
                                           SPA-GMN-LIST-YUKOSTYMD(IDX)
               MOVE    "99999999"      TO  SPA-NAI-LIST-YUKOEDYMD(IDX)
                                           SPA-GMN-LIST-YUKOEDYMD(IDX)
               PERFORM 400-LISTATAI-SEC
      *
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-FETCH-SEC
           END-PERFORM
      *
           MOVE    IDX             TO  SPA-GMN-LIST-MAX
      *
           MOVE    1               TO  PAGE-IDX
           MOVE    1               TO  PAGE-MAX
           IF      IDX > 30
               COMPUTE PAGE-MAX    = ( IDX  /   30 )   +   1
           END-IF
      *
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       400-LISTSET-EXT.
           EXIT.
      *****************************************************************
      *    項目値セット
      *****************************************************************
       400-LISTATAI-SEC            SECTION.
      *
           IF      SPA-NAI-FLG     =   ZERO
               MOVE    "key1"          TO  WRK-PATHNAME
           ELSE
               MOVE    "key2"          TO  WRK-PATHNAME
           END-IF
      *
           INITIALIZE                  SYS-VALUE-REC
           MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
           MOVE    SYS-ITEM-KANRICD
                                   TO  SYS-VALUE-KANRICD
           MOVE    SYS-ITEM-KBNCD  TO  SYS-VALUE-KBNCD
           MOVE    SYS-ITEM-ITEMCD TO  SYS-VALUE-ITEMCD
           MOVE    SPA-SYSYMD      TO  SYS-VALUE-YUKOSTYMD
                                       SYS-VALUE-YUKOEDYMD
      *
           MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
           MOVE    "tbl_syskanri_value"
                                   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           ELSE
               INITIALIZE                  SYS-VALUE-REC
               MOVE    1               TO  FLG-SYS-VALUE
           END-IF
      *
           IF      FLG-SYS-VALUE   =   ZERO
               PERFORM 400-LISTCOMBO-SEC
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           END-IF
      *
           PERFORM UNTIL  (IDX             >   200)
                   OR     (FLG-SYS-VALUE   =   1)
               COMPUTE IDX = IDX + 1
               MOVE    IDX             TO  SPA-GMN-LIST-NO(IDX)
               MOVE    SYS-ITEM-KANRICD
                                       TO  SPA-GMN-LIST-KANRICD(IDX)
               MOVE    SYS-ITEM-KBNCD  TO  SPA-GMN-LIST-KBNCD(IDX)
               MOVE    SYS-ITEM-ITEMCD TO  SPA-GMN-LIST-ITEMCD(IDX)
               MOVE    SYS-ITEM-ITEMLENGTH
                                       TO  SPA-GMN-LIST-ITEMLENGTH(IDX)
               MOVE    SYS-ITEM-ITEMTYPE
                                       TO  SPA-GMN-LIST-ITEMTYPE(IDX)
               MOVE    SYS-ITEM-ITEMNAME
                                       TO  SPA-GMN-LIST-NAME(IDX)
               PERFORM 400-LISTCOMBO-SEC
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri_value"
                                   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       400-LISTATAI-EXT.
           EXIT.
      *****************************************************************
      *    項目値内容セット処理
      *****************************************************************
       400-LISTCOMBO-SEC           SECTION.
      *
           MOVE    SYS-VALUE-YUKOSTYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD       =   SPACE
               MOVE    WRK-YMD     TO  SPA-NAI-LIST-YUKOSTYMD(IDX)
               MOVE    WRK-HENYMDG TO  SPA-GMN-LIST-YUKOSTYMD(IDX)
           END-IF
      *
           MOVE    SYS-VALUE-YUKOEDYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD       =   SPACE
               MOVE    WRK-YMD     TO  SPA-NAI-LIST-YUKOEDYMD(IDX)
               MOVE    WRK-HENYMDG TO  SPA-GMN-LIST-YUKOEDYMD(IDX)
           END-IF
      *
           INITIALIZE                  SYS-COMBO-REC
           MOVE    SYS-VALUE-KANRICD
                                   TO  SYS-COMBO-KANRICD
           MOVE    SYS-VALUE-KBNCD TO  SYS-COMBO-KBNCD
           MOVE    SYS-VALUE-ITEMCD
                                   TO  SYS-COMBO-ITEMCD
           MOVE    SYS-VALUE-VALUE TO  SYS-COMBO-VALUE
           MOVE    SYS-COMBO-REC   TO  MCPDATA-REC
           PERFORM 900-SYS-COMBO-READ-SEC
      *
           STRING  SYS-VALUE-VALUE     DELIMITED BY SPACE
                   " "                 DELIMITED BY SIZE
                   SYS-COMBO-VALUENAME DELIMITED BY SPACE
                               INTO    SPA-GMN-LIST-ATAI(IDX)
           END-STRING
      *
           .
       400-LISTCOMBO-EXT.
           EXIT.
      *****************************************************************
      *    画面セット処理
      *****************************************************************
       400-GMNSET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-NAIYO
           MOVE    2               TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-LIST-KANRICD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KANRICD
           MOVE    SPA-GMN-LIST-KBNCD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KBNCD
           MOVE    SPA-GMN-LIST-ITEMCD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-ITEMCD
           MOVE    SPA-GMN-LIST-ITEMLENGTH(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-ITEMLENGTH
           MOVE    SPA-GMN-LIST-ITEMTYPE(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-ITEMTYPE
           MOVE    SPA-GMN-LIST-NAME(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-NAME
           MOVE    SPA-GMN-LIST-ATAI(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-ATAI
           MOVE    SPA-GMN-LIST-YUKOSTYMD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-YUKOSTYMD
           MOVE    SPA-GMN-LIST-YUKOEDYMD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-YUKOEDYMD
           MOVE    SPA-NAI-LIST-YUKOSTYMD(SPA-GMN-SELNUM)
                                   TO  SPA-NAI-YUKOSTYMD
           MOVE    SPA-NAI-LIST-YUKOEDYMD(SPA-GMN-SELNUM)
                                   TO  SPA-NAI-YUKOEDYMD
      *
           INITIALIZE                  SYS-COMBO-REC
           MOVE    SPA-GMN-KANRICD TO  SYS-COMBO-KANRICD
           MOVE    SPA-GMN-KBNCD   TO  SYS-COMBO-KBNCD
           MOVE    SPA-GMN-ITEMCD  TO  SYS-COMBO-ITEMCD
           MOVE    SYS-COMBO-REC   TO  MCPDATA-REC
           MOVE    "tbl_syskanri_combo"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_combo"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-COMBO-FETCH-SEC
           ELSE
               INITIALIZE                  SYS-COMBO-REC
               MOVE    1               TO  FLG-SYS-COMBO
           END-IF
      *
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL  (IDX             >=  50)
                   OR     (FLG-SYS-COMBO   =   1)
               STRING  SYS-COMBO-VALUE     DELIMITED BY SPACE
                       " "                 DELIMITED BY SIZE
                       SYS-COMBO-VALUENAME DELIMITED BY SPACE
                                   INTO    SPA-GMN-ATAIL(IDX)
               END-STRING
               MOVE    IDX             TO  SPA-GMN-ATAI-MAX
      *
               MOVE    "tbl_syskanri_combo"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-COMBO-FETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri_combo"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       400-GMNSET-EXT.
           EXIT.
      *****************************************************************
      *    値チェック処理
      *****************************************************************
       400-ATAICHECK-SEC           SECTION.
      *
           MOVE    W00-ATAI        TO  SPA-GMN-ATAI
      *
           IF      SPA-GMN-ATAI    =   SPACE
               GO  TO  400-ATAICHECK-EXT
           END-IF
      *
      *    コンボでない場合は文字チェック
           IF      SPA-GMN-ATAI-MAX    =   ZERO
               PERFORM 400-MOJICHECK-SEC
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL  (IDX     >       SPA-GMN-ATAI-MAX )
                       OR     (FLG-OK  =   1)
                   IF      SPA-GMN-ATAI(1:SPA-GMN-ITEMLENGTH)
                           =   SPA-GMN-ATAIL(IDX)(1:SPA-GMN-ITEMLENGTH)
                       MOVE    SPA-GMN-ATAIL(IDX)
                                               TO  SPA-GMN-ATAI-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK          =   ZERO
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-GMN-ATAI(3:)
               END-IF
           END-IF
      *
           .
       400-ATAICHECK-EXT.
           EXIT.
      *****************************************************************
      *    値文字チェック処理
      *****************************************************************
       400-MOJICHECK-SEC           SECTION.
      *
      *    カナチェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-GMN-ATAI    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  SPA-GMN-ATAI
      *
      *    文字サイズオーバー
           IF      KANACHK-MAX     >   SPA-GMN-ITEMLENGTH
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  400-MOJICHECK-EXT
           END-IF
      *
      *    混在以外の時文字タイプチェック
           IF      SPA-GMN-ITEMTYPE    =   5
               GO  TO  400-MOJICHECK-EXT
           END-IF
      *
      *    混在の場合エラー
           IF      KANACHK-RC  NOT =   ZERO
               MOVE    "0011"          TO  SPA-ERRCD
           ELSE
      *        全角
               IF    SPA-GMN-ITEMTYPE    =   4
      *            半角の場合エラー
                   IF      KANACHK-SYUBETU =   1
                       MOVE    "0012"          TO  SPA-ERRCD
                   END-IF
               ELSE
      *            全角の場合エラー
                   IF      KANACHK-SYUBETU =   2
                       MOVE    "0013"          TO  SPA-ERRCD
                   ELSE
      *                英数字の場合エラー
                       IF    ( SPA-GMN-ITEMTYPE    NOT =   3 ) AND
                             ( SPA-GMN-ATAI(1:KANACHK-MAX)
                                                   NOT NUMERIC )
                           MOVE    "0014"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
      *
           .
       400-MOJICHECK-EXT.
           EXIT.
      *****************************************************************
      *    有効開始日付チェック処理
      *****************************************************************
       400-YUKOSTYMD-SEC           SECTION.
      *
           MOVE    W00-YUKOSTYMD   TO  SPA-GMN-YUKOSTYMD
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "00000000"      TO  SPA-NAI-YUKOSTYMD
                                           SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD            =   SPACE
                   MOVE    WRK-SYMD             TO  SPA-NAI-YUKOSTYMD
                   MOVE    WRK-HENYMDG          TO  SPA-GMN-YUKOSTYMD
               ELSE
                   MOVE    6               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >   SPA-NAI-YUKOEDYMD
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *
           .
       400-YUKOSTYMD-EXT.
           EXIT.
      *****************************************************************
      *    有効終了日付チェック処理
      *****************************************************************
       400-YUKOEDYMD-SEC           SECTION.
      *
           MOVE    W00-YUKOEDYMD   TO  SPA-GMN-YUKOEDYMD
      *
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "99999999"      TO  SPA-NAI-YUKOEDYMD
                                           SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      SPA-ERRCD            =   SPACE
                   MOVE    WRK-SYMD             TO  SPA-NAI-YUKOEDYMD 
                   MOVE    WRK-HENYMDG          TO  SPA-GMN-YUKOEDYMD
               ELSE
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >   SPA-NAI-YUKOEDYMD
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
           END-IF
      *
           .
       400-YUKOEDYMD-EXT.
           EXIT.
      *****************************************************************
      *    確認処理
      *****************************************************************
       400-KAKUNIN-SEC             SECTION.
      *
      *    値チェック
           PERFORM 400-ATAICHECK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  400-KAKUNIN-EXT
           END-IF
      *
      *    有効開始日チェック
           PERFORM 400-YUKOSTYMD-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  400-KAKUNIN-EXT
           END-IF
      *
      *    有効終了日チェック
           PERFORM 400-YUKOEDYMD-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  400-KAKUNIN-EXT
           END-IF
      *
      *    開始期間が変更していなければチェックを行わない
           IF      SPA-NAI-LIST-YUKOSTYMD(SPA-GMN-SELNUM)
                                   =   SPA-NAI-YUKOSTYMD
               MOVE    "1001"          TO  SPA-WIDCD
               GO  TO  400-KAKUNIN-EXT
           END-IF
      *
      *    重複チェック
           INITIALIZE                  SYS-VALUE-REC
           MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
           MOVE    SPA-GMN-KANRICD TO  SYS-VALUE-KANRICD
           MOVE    SPA-GMN-KBNCD   TO  SYS-VALUE-KBNCD
           MOVE    SPA-GMN-ITEMCD  TO  SYS-VALUE-ITEMCD
      *
           MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
           MOVE    "tbl_syskanri_value"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           ELSE
               INITIALIZE                  SYS-VALUE-REC
               MOVE    1               TO  FLG-SYS-VALUE
           END-IF
      *
           MOVE    ZERO            TO  IDY
           PERFORM UNTIL   FLG-SYS-VALUE   =   1
               IF    ( SYS-VALUE-YUKOEDYMD <   SPA-NAI-YUKOSTYMD ) 
                 OR  ( SYS-VALUE-YUKOSTYMD >   SPA-NAI-YUKOEDYMD )
                   CONTINUE
               ELSE
                   COMPUTE IDY = IDY + 1
               END-IF
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           END-PERFORM
      *
           IF      IDY      >=   1
               MOVE    "0006"          TO  SPA-ERRCD
           ELSE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       400-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       490-KOUSHIN-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYS-VALUE-REC
           MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
           MOVE    SPA-GMN-KANRICD TO  SYS-VALUE-KANRICD
           MOVE    SPA-GMN-KBNCD   TO  SYS-VALUE-KBNCD
           MOVE    SPA-GMN-ITEMCD  TO  SYS-VALUE-ITEMCD
           MOVE    SPA-NAI-YUKOSTYMD
                                   TO  SYS-VALUE-YUKOSTYMD
      *     MOVE    SPA-NAI-YUKOEDYMD
           MOVE    SPA-NAI-YUKOSTYMD
                                   TO  SYS-VALUE-YUKOEDYMD
           MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
           PERFORM 900-SYS-VALUE-READ-SEC
      *
           IF      FLG-SYS-VALUE   =   ZERO
               MOVE    SYS-VALUE-HOSPNUM
                                       TO  SYSUP-VALUE-HOSPNUM
               MOVE    SYS-VALUE-KANRICD
                                       TO  SYSUP-VALUE-KANRICD
               MOVE    SYS-VALUE-KBNCD TO  SYSUP-VALUE-KBNCD
               MOVE    SYS-VALUE-ITEMCD
                                       TO  SYSUP-VALUE-ITEMCD
               MOVE    SYS-VALUE-YUKOSTYMD
                                       TO  SYSUP-VALUE-YUKOSTYMD
               MOVE    SYS-VALUE-YUKOEDYMD
                                       TO  SYSUP-VALUE-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
               MOVE    SPA-GMN-KANRICD TO  SYS-VALUE-KANRICD
               MOVE    SPA-GMN-KBNCD   TO  SYS-VALUE-KBNCD
               MOVE    SPA-GMN-ITEMCD  TO  SYS-VALUE-ITEMCD
               MOVE    SPA-NAI-YUKOSTYMD
                                       TO  SYS-VALUE-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD
                                       TO  SYS-VALUE-YUKOEDYMD
               MOVE    SPA-GMN-ATAI(1:SPA-GMN-ITEMLENGTH)
                                       TO  SYS-VALUE-VALUE
               MOVE    SPA-OPID        TO  SYS-VALUE-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-VALUE-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-VALUE-UPHMS
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
               MOVE    SPA-GMN-KANRICD TO  SYS-VALUE-KANRICD
               MOVE    SPA-GMN-KBNCD   TO  SYS-VALUE-KBNCD
               MOVE    SPA-GMN-ITEMCD  TO  SYS-VALUE-ITEMCD
               MOVE    SPA-NAI-YUKOSTYMD
                                       TO  SYS-VALUE-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD
                                       TO  SYS-VALUE-YUKOEDYMD
               MOVE    SPA-GMN-ATAI(1:SPA-GMN-ITEMLENGTH)
                                       TO  SYS-VALUE-VALUE
               MOVE    SPA-OPID        TO  SYS-VALUE-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-VALUE-CREYMD
               MOVE    SMCNDATE-YMD    TO  SYS-VALUE-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-VALUE-UPHMS
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
      *
           MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           INITIALIZE                  SPA-GMN-NAIYO
      *
           PERFORM 400-LISTSET-SEC
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       490-SAKUJYO-SEC             SECTION.
      *
           INITIALIZE                  SYS-VALUE-REC
           MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
           MOVE    SPA-GMN-KANRICD TO  SYS-VALUE-KANRICD
           MOVE    SPA-GMN-KBNCD   TO  SYS-VALUE-KBNCD
           MOVE    SPA-GMN-ITEMCD  TO  SYS-VALUE-ITEMCD
           MOVE    SPA-NAI-LIST-YUKOSTYMD(SPA-GMN-SELNUM)
                                   TO  SYS-VALUE-YUKOSTYMD
           MOVE    SPA-NAI-LIST-YUKOEDYMD(SPA-GMN-SELNUM)
                                   TO  SYS-VALUE-YUKOEDYMD
           MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
           PERFORM 900-SYS-VALUE-READ-SEC
      *
           IF      FLG-SYS-VALUE   =   ZERO
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
      *
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1002"          TO  SPA-ERRCD
                   GO  TO  490-SAKUJYO-EXT
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           INITIALIZE                  SPA-GMN-NAIYO
      *
           PERFORM 400-LISTSET-SEC
      *
           .
       490-SAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "W00"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
           INITIALIZE                  W00
      *
           MOVE    ZERO            TO  IDX
           MOVE    ZERO            TO  IDY
      *
           IF      SPA-NAI-KANRICD     NOT =   SPACE
               COMPUTE IDX     = ( PAGE-IDX    -   1 ) *  30
           END-IF
      *
           PERFORM UNTIL  (IDX     =   SPA-GMN-LIST-MAX)
                   OR     (IDY     =   30)
               COMPUTE IDX = IDX + 1
               COMPUTE IDY = IDY + 1
               MOVE    SPA-GMN-LIST-NO(IDX)
                                       TO  W00-LIST-NO(IDY)
               MOVE    SPA-GMN-LIST-NAME(IDX)
                                       TO  W00-LIST-NAME(IDY)
               MOVE    SPA-GMN-LIST-ATAI(IDX)
                                       TO  W00-LIST-ATAI(IDY)
               IF      SPA-GMN-LIST-YUKOSTYMD(IDX) NOT =   "00000000"
                   MOVE    SPA-GMN-LIST-YUKOSTYMD(IDX)
                                           TO  W00-LIST-YUKOSTYMD(IDY)
               END-IF
               IF      SPA-GMN-LIST-YUKOEDYMD(IDX) NOT =   "99999999"
                   MOVE    SPA-GMN-LIST-YUKOEDYMD(IDX)
                                           TO  W00-LIST-YUKOEDYMD(IDY)
               END-IF
               IF      SPA-GMN-SELNUM  =   IDX
                   MOVE    "T"             TO  W00-LIST-SELECT(IDY)
               ELSE
                   MOVE    "F"             TO  W00-LIST-SELECT(IDY)
               END-IF
           END-PERFORM
      *
           MOVE    IDY                 TO  W00-LIST-COUNT
      *
           MOVE    WIDGET-NORMAL       TO  W00-B06-STATE
           MOVE    WIDGET-NORMAL       TO  W00-B07-STATE
      *
           IF      PAGE-IDX    =   1
               MOVE    WIDGET-INSENSITIVE  TO  W00-B06-STATE
           END-IF
           IF      PAGE-IDX    =   PAGE-MAX
               MOVE    WIDGET-INSENSITIVE  TO  W00-B07-STATE
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  W00-SELNUM
      *
           MOVE    SPA-GMN-KANRICD     TO  W00-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  W00-KBNCD
           MOVE    SPA-GMN-ITEMCD      TO  W00-ITEMCD
           MOVE    SPA-GMN-NAME        TO  W00-NAME
           MOVE    SPA-GMN-YUKOSTYMD   TO  W00-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD   TO  W00-YUKOEDYMD
           MOVE    SPA-GMN-ATAI        TO  W00-ATAI
      *
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL ( IDX     >   SPA-GMN-ATAI-MAX )
                   OR    ( IDX     >   50 )
               MOVE    SPA-GMN-ATAILIST(IDX)
                                       TO  W00-ATAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-ATAI-MAX    TO  W00-ATAI-COUNT
      *
           MOVE    WIDGET-NORMAL       TO  W00-B03-STATE
           MOVE    WIDGET-NORMAL       TO  W00-B04-STATE
           MOVE    WIDGET-NORMAL       TO  W00-B12-STATE
      *
           IF    ( SPA-NAI-KANRICD     =   SPACE ) OR
                 ( SPA-GMN-SELNUM      =   ZERO )  OR
                 ( SPA-GMN-SELNUM      >   SPA-GMN-LIST-MAX )
               MOVE    WIDGET-INSENSITIVE  TO  W00-B03-STATE
               MOVE    WIDGET-INSENSITIVE  TO  W00-B12-STATE
           END-IF
      *
           IF      SPA-NAI-KANRICD     =   SPACE
               MOVE    WIDGET-INSENSITIVE  TO  W00-B04-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号が正しくありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "値が正しくありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "値の桁が大きくなっています。"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "日付が正しくありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効期間が正しくありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "有効期間重複しています。"
                                           TO  SPA-ERRMSG
              WHEN    "0011"
                   MOVE    "文字が混在しています。"
                                           TO  SPA-ERRMSG
              WHEN    "0012"
                   MOVE    "全角で入力してください。"
                                           TO  SPA-ERRMSG
              WHEN    "0013"
                   MOVE    "半角で入力してください。"
                                           TO  SPA-ERRMSG
              WHEN    "0014"
                   MOVE    "数字で入力してください。"
                                           TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新できませんでした。"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除できませんでした。"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W00"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "更新します。よろしいですか？"
                                           TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除します。よろしいですか？"
                                           TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "W00"           TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    2
                   MOVE    "KANRICD"       TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KBNCD"         TO  MCP-WIDGET
               WHEN    4
                   MOVE    "ITEMCD"        TO  MCP-WIDGET
               WHEN    5
                   MOVE    "ATAI"          TO  MCP-WIDGET
               WHEN    6
                   MOVE    "YUKOSTYMD"     TO  MCP-WIDGET
               WHEN    7
                   MOVE    "YUKOEDYMD"     TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "KANRICD"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "KBNCD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "ITEMCD"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "ATAI"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "YUKOSTYMD"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)    TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)    TO  WRK-HENYMDG
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                  ORCSGDAYAREA
                   MOVE    WRK-YMD         TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
      *
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理項目読み込み処理
      *****************************************************************
       900-SYS-ITEM-FETCH-SEC      SECTION.
      *
           PERFORM 920-DBFETCH-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS-ITEM
               MOVE    MCPDATA-REC     TO  SYS-ITEM-REC
           ELSE
               MOVE    1               TO  FLG-SYS-ITEM
               INITIALIZE                  SYS-ITEM-REC
           END-IF
      *
           .
       900-SYS-ITEM-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理項目値読み込み処理
      *****************************************************************
       900-SYS-VALUE-READ-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYS-VALUE
      *
           MOVE    "tbl_syskanri_value"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-SYS-VALUE-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYS-VALUE
               INITIALIZE                  SYS-VALUE-REC
           END-IF
      *
           MOVE    "tbl_syskanri_value"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYS-VALUE-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理項目値読み込み処理
      *****************************************************************
       900-SYS-VALUE-FETCH-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS-VALUE
               MOVE    MCPDATA-REC     TO  SYS-VALUE-REC
           ELSE
               MOVE    1               TO  FLG-SYS-VALUE
               INITIALIZE                  SYS-VALUE-REC
           END-IF
      *
           .
       900-SYS-VALUE-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理項目値選択肢読み込み処理
      *****************************************************************
       900-SYS-COMBO-READ-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYS-COMBO
      *
           MOVE    "tbl_syskanri_combo"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri_combo"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-SYS-COMBO-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYS-COMBO
               INITIALIZE                  SYS-COMBO-REC
           END-IF
      *
           MOVE    "tbl_syskanri_combo"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYS-COMBO-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理項目値選択肢読み込み処理
      *****************************************************************
       900-SYS-COMBO-FETCH-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYS-COMBO
               MOVE    MCPDATA-REC     TO  SYS-COMBO-REC
           ELSE
               MOVE    1               TO  FLG-SYS-COMBO
               INITIALIZE                  SYS-COMBO-REC
           END-IF
      *
           .
       900-SYS-COMBO-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
