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
                           11  SPA-GMN-LIST-NO          PIC X(02).
                           11  SPA-GMN-LIST-NAME        PIC X(30).
                           11  SPA-GMN-LIST-ATAI        PIC X(40).
                           11  SPA-GMN-LIST-YUKOSTYMD   PIC X(08).
                           11  SPA-GMN-LIST-YUKOEDYMD   PIC X(08).
                   07  SPA-GMN-LIST-MAX    PIC 9(05).
      *
               05  SPA-GMN-SELNUM      PIC 9(02).
      *
               05  SPA-GMN-NAIYO.
                   07  SPA-GMN-KANRICD     PIC X(04).
                   07  SPA-GMN-KBNCD       PIC X(08).
                   07  SPA-GMN-ITEMCD      PIC 9(05).
                   07  SPA-GMN-NAME        PIC X(30).
                   07  SPA-GMN-YUKOSTYMD   PIC X(08).
                   07  SPA-GMN-YUKOEDYMD   PIC X(08).
      *
                   07  SPA-GMN-ATAI        PIC X(40).
                   07  SPA-GMN-ATAILST     OCCURS  50.
                       09  SPA-GMN-ATAIL   PIC X(40).
                   07  SPA-GMN-ATAI-MAX    PIC 9(02).
      *
               05  SPA-NAI-KANRICD     PIC X(04).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYS-ITEM        PIC 9(01).
           03  FLG-SYS-VALUE       PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
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
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
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
           EXIT    PROGRAM
           .
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
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
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
           PERFORM 400-LISTSET1-SEC
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
           IF      SPA-WID1-FLG    =   "OK"
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
                   MOVE   "CHANGE"     TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   MOVE    "1002"      TO  SPA-WIDCD
      *        更新
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    "1001"      TO  SPA-WIDCD
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
                   MOVE    W00-ATAI        TO  SPA-GMN-ATAI
      *        有効開始日付
               WHEN    "ACTIVATE"  ALSO    "YUKOSTYMD"
                   MOVE    W00-YUKOSTYMD   TO  SPA-GMN-YUKOSTYMD
      *        有効終了日付
               WHEN    "ACTIVATE"  ALSO    "YUKOEDYMD"
                   MOVE    W00-YUKOEDYMD   TO  SPA-GMN-YUKOEDYMD
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
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    リスト選択処理
      *****************************************************************
       400-LIST-SEC            SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    ZERO        TO  FLG-OK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >   SPA-GMN-LIST-MAX)
                   OR     (FLG-OK  =   1)
               IF      W00-LIST-SELECT(IDX)    =   "T"
                   MOVE    IDX         TO  SPA-GMN-SELNUM
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-KANRICD     =   SPACE
                   MOVE    SPA-GMN-LIST-KANRICD(SPA-GMN-SELNUM)
                                           TO  SPA-NAI-KANRICD
                   MOVE    ZERO            TO  SPA-GMN-SELNUM
                   PERFORM 400-LISTSET2-SEC
               ELSE
                   PERFORM 400-GMNSET-SEC
               END-IF
           END-IF
      *
           .
       400-LIST-EXT.
           EXIT.
      *****************************************************************
      *    番号選択処理
      *****************************************************************
       400-SELNUM-SEC            SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    W00-SELNUM  TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO        TO  FLG-OK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >   SPA-GMN-LIST-MAX)
                   OR     (FLG-OK  =   1)
               IF      SPA-GMN-SELNUM  =   IDX
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      SPA-NAI-KANRICD     =   SPACE
                   MOVE    SPA-GMN-LIST-KANRICD(SPA-GMN-SELNUM)
                                           TO  SPA-NAI-KANRICD
                   MOVE    ZERO            TO  SPA-GMN-SELNUM
                   PERFORM 400-LISTSET2-SEC
               ELSE
                   PERFORM 400-GMNSET-SEC
               END-IF
           END-IF
      *
           .
       400-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    画面セット処理
      *****************************************************************
       400-GMNSET-SEC              SECTION.
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-LIST-KANRICD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-KANRICD
           MOVE    SPA-GMN-LIST-KBNCD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-KBNCD
           MOVE    SPA-GMN-LIST-ITEMCD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-ITEMCD
           MOVE    SPA-GMN-LIST-NAME(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-NAME
           MOVE    SPA-GMN-LIST-YUKOSTYMD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-YUKOSTYMD
           MOVE    SPA-GMN-LIST-YUKOEDYMD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-YUKOEDYMD
           MOVE    SPA-GMN-LIST-ATAI(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-ATAI
      *     MOVE    SPA-GMN-LIST-ATAI(SPA-GMN-SELNUM)
      *                                 TO  SPA-GMN-ATAIL(1)
           MOVE    0                   TO  SPA-GMN-ATAI-MAX
      *
           .
       400-GMNSET-EXT.
           EXIT.
      *****************************************************************
      *    リストセット１処理
      *****************************************************************
       400-LISTSET1-SEC            SECTION.
      *
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
               PERFORM 900-SYS-ITEM-READ-SEC
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
               MOVE    SYS-ITEM-YUKOSTYMD
                                       TO  SPA-GMN-LIST-YUKOSTYMD(IDX)
               MOVE    SYS-ITEM-YUKOEDYMD
                                       TO  SPA-GMN-LIST-YUKOEDYMD(IDX)
               MOVE    IDX             TO  SPA-GMN-LIST-MAX
      *
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       400-LISTSET1-EXT.
           EXIT.
      *****************************************************************
      *    リストセット２処理
      *****************************************************************
       400-LISTSET2-SEC            SECTION.
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
               PERFORM 900-SYS-ITEM-READ-SEC
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
               MOVE    SYS-ITEM-KBNCD  TO  SPA-GMN-LIST-KBNCD(IDX)
               MOVE    SYS-ITEM-ITEMCD TO  SPA-GMN-LIST-ITEMCD(IDX)
               MOVE    SYS-ITEM-ITEMNAME
                                       TO  SPA-GMN-LIST-NAME(IDX)
               MOVE    SYS-ITEM-YUKOSTYMD
                                       TO  SPA-GMN-LIST-YUKOSTYMD(IDX)
               MOVE    SYS-ITEM-YUKOEDYMD
                                       TO  SPA-GMN-LIST-YUKOEDYMD(IDX)
      *
               INITIALIZE                  SYS-VALUE-REC
               MOVE    SPA-HOSPNUM     TO  SYS-VALUE-HOSPNUM
               MOVE    SYS-ITEM-KANRICD
                                       TO  SYS-VALUE-KANRICD
               MOVE    SYS-ITEM-KBNCD  TO  SYS-VALUE-KBNCD
               MOVE    SYS-ITEM-ITEMCD TO  SYS-VALUE-ITEMCD
               MOVE    SYS-ITEM-YUKOSTYMD
                                       TO  SYS-VALUE-YUKOSTYMD
               MOVE    SYS-ITEM-YUKOEDYMD
                                       TO  SYS-VALUE-YUKOEDYMD
               MOVE    SYS-VALUE-REC   TO  MCPDATA-REC
               PERFORM 900-SYS-VALUE-READ-SEC
      *
               IF      FLG-SYS-VALUE   =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-VALUE-REC
                   MOVE    SYS-VALUE-VALUE TO  SPA-GMN-LIST-ATAI(IDX)
               END-IF
      *
               MOVE    IDX             TO  SPA-GMN-LIST-MAX
      *
               MOVE    "tbl_syskanri_item"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYS-ITEM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri_item"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       400-LISTSET2-EXT.
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
           MOVE    SPA-GMN-YUKOSTYMD
                                   TO  SYS-VALUE-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD
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
      *
               MOVE    SPA-GMN-ATAI    TO  SYS-VALUE-VALUE
               MOVE    SPA-OPID        TO  SYS-VALUE-OPID
               MOVE    SMCNDATE-YMD    TO  SYS-VALUE-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-VALUE-UPHMS
      *
               MOVE    "tbl_syskanri_value"
                                       TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    SPA-GMN-ATAI    TO  SYS-VALUE-VALUE
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
           PERFORM 310-SPASET-SEC
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
           MOVE    SPA-GMN-YUKOSTYMD
                                   TO  SYS-VALUE-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD
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
           PERFORM 310-SPASET-SEC
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
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL  (IDX     >   SPA-GMN-LIST-MAX)
               MOVE    SPA-GMN-LIST-NO(IDX)
                                       TO  W00-LIST-NO(IDX)
               MOVE    SPA-GMN-LIST-NAME(IDX)
                                       TO  W00-LIST-NAME(IDX)
               MOVE    SPA-GMN-LIST-ATAI(IDX)
                                       TO  W00-LIST-ATAI(IDX)
               MOVE    SPA-GMN-LIST-YUKOSTYMD(IDX)
                                       TO  W00-LIST-YUKOSTYMD(IDX)
               MOVE    SPA-GMN-LIST-YUKOEDYMD(IDX)
                                       TO  W00-LIST-YUKOEDYMD(IDX)
               IF      SPA-GMN-SELNUM  =   IDX
                   MOVE    "T"             TO  W00-LIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  W00-LIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-LIST-MAX    TO  W00-LIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  W00-SELNUM
      *
           MOVE    SPA-GMN-KANRICD     TO  W00-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  W00-KBNCD
           MOVE    SPA-GMN-ITEMCD      TO  W00-ITEMCD
           MOVE    SPA-GMN-NAME        TO  W00-NAME
           MOVE    SPA-GMN-YUKOSTYMD   TO  W00-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD   TO  W00-YUKOEDYMD
      *
           MOVE    SPA-GMN-ATAI        TO  W00-ATAI
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX >       SPA-GMN-ATAI-MAX
               MOVE    SPA-GMN-ATAILST(IDX)
                                       TO  W00-ATAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-ATAI-MAX    TO  W00-ATAI-COUNT
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
           DISPLAY "SPA-GMN-CUR=" SPA-GMN-CUR
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
      *    システム管理項目読み込み処理
      *****************************************************************
       900-SYS-ITEM-READ-SEC       SECTION.
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
       900-SYS-ITEM-READ-EXT.
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
               PERFORM 920-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYS-VALUE
               ELSE
                   MOVE    1               TO  FLG-SYS-VALUE
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYS-VALUE
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
