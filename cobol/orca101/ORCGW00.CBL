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
                   07  SPA-GMN-KBN    PIC X(24).
      *
                   07  SPA-GMN-LIST    OCCURS  31 TIMES.
                       09  SPA-GMN-BANGO       PIC X(6).
                       09  SPA-GMN-MEISYO      PIC X(30).
                       09  SPA-GMN-ATAI        PIC X(40).
                       09  SPA-GMN-YUKOSTYMD   PIC X(9).
                       09  SPA-GMN-YUKOEDYMD   PIC X(9).
                   07  SPA-GMN-LIST-MAX        PIC 9(02).
      *
                   07  SPA-GMN-HOSPKBN-G.
                       09  SPA-GMN-HOSPKBN     PIC X(01).
                       09  SPA-GMN-F1          PIC X(01).
                       09  SPA-GMN-HOSPKBNMEI  PIC X(08).
                   07  SPA-GMN-HOSPKBNLIST     OCCURS  2.
                       09  SPA-GMN-HOSPKBNL    PIC X(01).
                       09  SPA-GMN-F1L         PIC X(01).
                       09  SPA-GMN-HOSPKBNMEIL PIC X(08).
                   07  SPA-GMN-HOSPKBN-MAX     PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
      *
      *    一時領域
      *
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
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    印刷データ削除設定機能情報
           COPY  "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
               MOVE    SPACE               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  LINKAREA
      * 
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W00"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
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
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
      *     IF      SPA-WID1-FLG    =   "OK"
      *         PERFORM 490-KOUSHIN-SEC
      *     END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-W00
      *
           MOVE    "000001"        TO  SPA-GMN-BANGO(1)
           MOVE    "都道府県番号"  TO  SPA-GMN-MEISYO(1)
           MOVE    "13"            TO  SPA-GMN-ATAI(1)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(1)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(1)
      *
           MOVE    "000002"        TO  SPA-GMN-BANGO(2)
           MOVE    "点数表"        TO  SPA-GMN-MEISYO(2)
           MOVE    "1"             TO  SPA-GMN-ATAI(2)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(2)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(2)
      *
           MOVE    "000003"        TO  SPA-GMN-BANGO(3)
           MOVE    "医療機関コード"    TO  SPA-GMN-MEISYO(3)
           MOVE    "1234567"       TO  SPA-GMN-ATAI(3)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(3)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(3)
      *
           MOVE    "000004"        TO  SPA-GMN-BANGO(4)
           MOVE    "医療機関種別"  TO  SPA-GMN-MEISYO(4)
           MOVE    "2"             TO  SPA-GMN-ATAI(4)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(4)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(4)
      *
           MOVE    "000005"        TO  SPA-GMN-BANGO(5)
           MOVE    "医療機関ＩＤ"  TO  SPA-GMN-MEISYO(5)
           MOVE    "JPN000000000000"   TO  SPA-GMN-ATAI(5)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(5)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(5)
      *
           MOVE    "000006"        TO  SPA-GMN-BANGO(6)
           MOVE    "医療機関名称"  TO  SPA-GMN-MEISYO(6)
           MOVE    "医療法人　オルカ医院"  TO  SPA-GMN-ATAI(6)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(6)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(6)
      *
           MOVE    "000007"        TO  SPA-GMN-BANGO(7)
           MOVE    "短縮医療機関名称"  TO  SPA-GMN-MEISYO(7)
           MOVE    "オルカ医院"    TO  SPA-GMN-ATAI(7)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(7)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(7)
      *
           MOVE    "000008"        TO  SPA-GMN-BANGO(8)
           MOVE    "開設者名"      TO  SPA-GMN-MEISYO(8)
           MOVE    "オルカ"        TO  SPA-GMN-ATAI(8)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(8)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(8)
      *
           MOVE    "000009"        TO  SPA-GMN-BANGO(9)
           MOVE    "管理者氏名（院長）"    TO  SPA-GMN-MEISYO(9)
           MOVE    "オルカ　シャチ"    TO  SPA-GMN-ATAI(9)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(9)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(9)
      *
           MOVE    "000010"        TO  SPA-GMN-BANGO(10)
           MOVE    "病床数（許可）"    TO  SPA-GMN-MEISYO(10)
           MOVE    "0"             TO  SPA-GMN-ATAI(10)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(10)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(10)
      *
           MOVE    "000011"        TO  SPA-GMN-BANGO(11)
           MOVE    "病床数（一般）"    TO  SPA-GMN-MEISYO(11)
           MOVE    "0"             TO  SPA-GMN-ATAI(11)
           MOVE    "00000000"      TO  SPA-GMN-YUKOSTYMD(11)
           MOVE    "99999999"      TO  SPA-GMN-YUKOEDYMD(11)
      *
           MOVE    11              TO  SPA-GMN-LIST-MAX
      *
           MOVE    "1 病院"        TO  SPA-GMN-HOSPKBNLIST(1)
           MOVE    "2 診療所"      TO  SPA-GMN-HOSPKBNLIST(2)
           MOVE    2               TO  SPA-GMN-HOSPKBN-MAX
      * 
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "KBN"
               MOVE    W00-KBN        TO  SPA-GMN-KBN
           WHEN    "ACTIVATE"      ALSO    "HOSPKBN"
               MOVE    W00-HOSPKBN        TO  SPA-GMN-HOSPKBN-G
               MOVE    ZERO            TO  WRK-HENFLG
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL  (IDX     >   SPA-GMN-HOSPKBN-MAX)
                       OR     (WRK-HENFLG      =   1)
                   IF       SPA-GMN-HOSPKBNL(IDX)   =  SPA-GMN-HOSPKBN
                           MOVE    SPA-GMN-HOSPKBNLIST(IDX)
                                                TO  SPA-GMN-HOSPKBN-G
                       MOVE    1           TO  WRK-HENFLG
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               ELSE
                   MOVE    99          TO  SPA-GMN-CUR
               END-IF
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W00"               TO  SPA-MOTOPG
      *
           INITIALIZE                      SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W00"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      * 
           INITIALIZE                  W00
           MOVE    SPA-GMN-KBN         TO  W00-KBN
      *
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL   IDX     >   SPA-GMN-LIST-MAX
               MOVE    SPA-GMN-BANGO(IDX)      TO  W00-BANGO(IDX)
               MOVE    SPA-GMN-MEISYO(IDX)     TO  W00-MEISYO(IDX)
               MOVE    SPA-GMN-ATAI(IDX)       TO  W00-ATAI(IDX)
               MOVE    SPA-GMN-YUKOSTYMD(IDX)  TO  W00-YUKOSTYMD(IDX)
               MOVE    SPA-GMN-YUKOEDYMD(IDX)  TO  W00-YUKOEDYMD(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-HOSPKBN-G     TO  W00-HOSPKBN
           PERFORM VARYING IDX     FROM    1       BY  1
                   UNTIL   IDX     >   SPA-GMN-HOSPKBN-MAX
               MOVE    SPA-GMN-HOSPKBNLIST(IDX)
                                       TO  W00-HOSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HOSPKBN-MAX   TO  W00-HOSPKBN-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    ""
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W00"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    ""
                                       TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W00"               TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
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
                    MOVE  "KBN"          TO  MCP-WIDGET
                WHEN    2
                    MOVE  "HOSPKBN"        TO  MCP-WIDGET
                WHEN    99
                    MOVE  "B12"            TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "KBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "HOSPKBN"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"           TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
