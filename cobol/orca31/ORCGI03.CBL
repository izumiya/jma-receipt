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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 他院歴作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.25    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.02.05  自院歴作成機能追加
      *  01.00.02    NACL-太田    03.05.08  入院中区分追加
      *  01.00.03    NACL-太田    03.06.05  日付以外の修正を可とする
      *                                     区分追加
      *  01.00.04    NACL-太田    03.11.14  更新日付編集対応
      *  01.00.05    NACL-小豆沢  05.02.04  他院歴作成については
      *                                     「入院期間≧通算対象日数となっていること」
      *                                     のチェックを外す
      *  03.05.00    NACL-太田    07.05.15  グループ診療対応
      *  04.01.00    NACL-太田    07.12.13  特別な関係にある医療機関の考慮を追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    入退院登録共通領域
           COPY    "I0COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "I0SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TAINFUKA                PIC 9(01).
           03  FLG-LOOPEND                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-I0IDMSG                 PIC X(70). 
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-YMD                     PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-KANACHK-OUT-INPUT       PIC X(400).
      *    パス名領域
           03  WRK-CMB-CD                  PIC X(05).
           03  WRK-CMB-G.
               05  WRK-CMB-IDX             PIC 9(03).
               05  WRK-CMB-ITM             PIC X(100).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU1                  PIC 9(06).
           03  WRK-NISSU2                  PIC 9(06).
           03  WRK-UPDKBN                  PIC X(3).
           03  WRK-TAINFUKA.
               05  WRK-TAINFUKA-OCC        OCCURS 25.
                   07  WRK-TAINFUKA-KBN    PIC 9(02).
                   07  WRK-TAINFUKA-NAIYO  PIC X(80).
           03  WRK-CREYMD                  PIC X(08).
           03  WRK-SHONUM                  PIC 9(03).
           03  WRK-DAY6-KEISAN             PIC X(08).
      *
       01  WRK-BAK-AREA.
           03  WRK-BAK-SHOKAI.
               05  WRK-BAK-SHOKAI-CD       PIC X(01).
               05                          PIC X(01).
               05  WRK-BAK-SHOKAI-NM       PIC X(06).
           03  WRK-BAK-SHONUM              PIC S9(3).
      *
       01  CONST-AREA.
           03  CONST-ZIIN-LBL              PIC X(06)
               VALUE   "自院歴".
           03  CONST-TAIN-LBL              PIC X(06)
               VALUE   "他院歴".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    他院歴付加情報
       01  PTTAINFUKA-REC.
           COPY    "CPPTTAINFUKA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数値号変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "I01.INC".
           COPY    "I02.INC".
           COPY    "I03.INC".
           COPY    "I04.INC".
           COPY    "I05.INC".
           COPY    "I06.INC".
           COPY    "I0ERR.INC".
           COPY    "I0ID1.INC".
           COPY    "I0ID2.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I0FREE.
           MOVE    SPA-WORK        TO  SPA-I0KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF  FLG-END   =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I0KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-I0FREE      TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I0ERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
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
           MOVE    "I03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "I0ID1"
               PERFORM 330-I0ID1-SET-SEC
               MOVE    SPACE           TO  SPA-I0ID1-FLG
               MOVE    SPACE           TO  SPA-I0IDCD
      *
               GO  TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              I03
                                   SPA-I03
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
           IF    ( SPA-I01-I03-SYORIKBN    =   "2" OR "3" )
               PERFORM 4101-INPUT-AREA-EDIT-SEC
           ELSE
               IF    ( SPA-I01-I03-SHONUM  NOT =   ZERO )
                   MOVE    SPA-GMN-I03-SHOKAI-ITM (02)
                                   TO    SPA-GMN-I03-SHOKAI
                   MOVE    SPA-I01-I03-SHONUM
                                   TO    SPA-GMN-I03-SHONUM
               ELSE
                   MOVE    SPA-GMN-I03-SHOKAI-ITM (01)
                                   TO    SPA-GMN-I03-SHOKAI
               END-IF
      *
               MOVE    SPA-GMN-I03-RRKKBN-ITM (02)
                                   TO    SPA-GMN-I03-RRKKBN
      *
      *        MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (01)
      *                            TO    SPA-GMN-I03-NYUINCHUKBN
               MOVE    SPACE
                                   TO    SPA-GMN-I03-NYUINCHUKBN
      *
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
           INITIALIZE              SPA-GMN-I03-CMB-DAT-AREA
      *
      *    初回コンボ
           PERFORM 310-CMB-SHOKAI-SET-SEC
      *
      *    自院／他院コンボ
           PERFORM 310-CMB-RRKKBN-SET-SEC
      *
      *    入院中コンボ
           PERFORM 310-CMB-NYUINCHUKBN-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    初回コンボ初期処理
      *****************************************************************
       310-CMB-SHOKAI-SET-SEC          SECTION.
      *
           MOVE    2                   TO  SPA-GMN-I03-SHOKAI-CNT
      *
           MOVE    "1"                 TO  SPA-GMN-I03-SHOKAIL    (01)
           MOVE    "初回"              TO  SPA-GMN-I03-SHOKAIMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-I03-SHOKAIL    (02)
           MOVE    "継続"              TO  SPA-GMN-I03-SHOKAIMEIL (02)
      *
           .
       310-CMB-SHOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    自院／他院コンボ初期処理
      *****************************************************************
       310-CMB-RRKKBN-SET-SEC          SECTION.
      *
           MOVE    3                   TO  SPA-GMN-I03-RRKKBN-CNT
      *
           MOVE    "1"                 TO  SPA-GMN-I03-RRKKBNL    (01)
           MOVE    "自院歴"            TO  SPA-GMN-I03-RRKKBNMEIL (01)
      *
           MOVE    "2"                 TO  SPA-GMN-I03-RRKKBNL    (02)
           MOVE    "他院歴"            TO  SPA-GMN-I03-RRKKBNMEIL (02)
      *
           MOVE    "3"                 TO  SPA-GMN-I03-RRKKBNL    (03)
           MOVE    "他院歴（特別な関係にある医療機関）"
                                       TO  SPA-GMN-I03-RRKKBNMEIL (03)
      *
           .
       310-CMB-RRKKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院中コンボ初期処理
      *****************************************************************
       310-CMB-NYUINCHUKBN-SET-SEC          SECTION.
      *
           MOVE    2               TO  SPA-GMN-I03-NYUINCHUKBN-CNT
      *
           MOVE    "0"             TO  SPA-GMN-I03-NYUINCHUKBNL    (01)
           MOVE    "退院済"        TO  SPA-GMN-I03-NYUINCHUKBNMEIL (01)
      *
           MOVE    "1"             TO  SPA-GMN-I03-NYUINCHUKBNL    (02)
           MOVE    "入院中"        TO  SPA-GMN-I03-NYUINCHUKBNMEIL (02)
           
      *
           .
       310-CMB-NYUINCHUKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（Ｉ０ＩＤ１）からの戻り値設定処理
      *****************************************************************
       330-I0ID1-SET-SEC               SECTION.
      *
           IF    ( SPA-I0ID1-FLG   =   "OK" )
               MOVE    SPACE       TO  WRK-UPDKBN
               EVALUATE    SPA-I0IDCD
      *        他院歴削除
               WHEN "3001"
                   MOVE    "DEL"   TO  WRK-UPDKBN
                   PERFORM 4801-PTNYUINRRK-UPD-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *
                   PERFORM 4801-PTTAINFUKA-DEL-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  330-I0ID1-SET-EXT
                   END-IF
      *
                   PERFORM 210-BACK
                   MOVE    "B04"   TO  SPA-I01-I03-BTN
      *
      *
               END-EVALUATE
      *
           END-IF
      *
           .
       330-I0ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 420-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 4102-RRKKBN-CHK-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-DEL-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    自院／退院チェック処理
           WHEN    "ACTIVATE"      ALSO    "RRKKBN"
               PERFORM 4101-RRKKBN-CHK-SEC
      *    入院中チェック処理
           WHEN    "ACTIVATE"      ALSO    "NYUINCHUKBN"
               PERFORM 4101-NYUINCHUKBN-CHK-SEC
           WHEN    OTHER
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    END-EVALUATE
      *
           MOVE    ZERO            TO      SPA-GMN-I03-CUR
      *
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    初回
           MOVE    I03-SHOKAI      TO  SPA-GMN-I03-SHOKAI
      *    初回番号
           MOVE    I03-SHONUM      TO  SPA-GMN-I03-SHONUM
      *    コメント
           MOVE    I03-COMMENT     TO  SPA-GMN-I03-COMMENT
      *    入院日
           MOVE    I03-NYUINYMD    TO  SPA-GMN-I03-NYUINYMD
      *    退院日
           MOVE    I03-TAIINYMD    TO  SPA-GMN-I03-TAIINYMD
      *    通算対象日数
           MOVE    I03-TAISYONISSU TO  SPA-GMN-I03-TAISYONISSU
      *    病名
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   5 )
               MOVE    I03-BYOMEI (IDX1)
                                   TO  SPA-GMN-I03-BYOMEI (IDX1)
           END-PERFORM
      *    入院基本料
           MOVE    I03-KHNRYO      TO  SPA-GMN-I03-KHNRYO
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    初回チェック処理
           PERFORM 4101-SHOKAI-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    コメントチェック処理
           PERFORM 4101-COMMENT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院日チェック処理
           IF    ( SPA-GMN-I03-NYUINYMD
                                   NOT =   SPA-NAI-I03-NYUINYMD-MAE )
               PERFORM 4101-NYUINYMD-CHK-SEC
               MOVE    SPA-GMN-I03-NYUINYMD
                                   TO  SPA-NAI-I03-NYUINYMD-MAE
               MOVE    SPA-ERRCD   TO  SPA-NAI-I03-NYUINYMD-ERRCD
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I03-NYUINYMD-ERRCD
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I03-NYUINYMD-ERRCD
                                   TO  SPA-ERRCD
                   MOVE    3       TO  SPA-GMN-I03-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    退院日チェック処理
           IF    ( SPA-GMN-I03-TAIINYMD
                                   NOT =   SPA-NAI-I03-TAIINYMD-MAE )
               PERFORM 4101-TAIINYMD-CHK-SEC
               MOVE    SPA-GMN-I03-TAIINYMD
                                   TO  SPA-NAI-I03-TAIINYMD-MAE
               MOVE    SPA-ERRCD   TO  SPA-NAI-I03-TAIINYMD-ERRCD
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           ELSE
               IF    ( SPA-NAI-I03-TAIINYMD-ERRCD
                                   NOT =   SPACE )
                   MOVE    SPA-NAI-I03-TAIINYMD-ERRCD
                                   TO  SPA-ERRCD
                   MOVE    4       TO  SPA-GMN-I03-CUR
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    通算対象日数チェック処理
           PERFORM 4101-TAISYONISSU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    病名チェック処理
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   5 )
      *
               PERFORM 4101-BYOMEI-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
           END-PERFORM
      *
      *    入院基本料チェック処理
           PERFORM 4101-KHNRYO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4101-INPUT-AREA-EDIT-SEC        SECTION.
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               GO  TO  4101-INPUT-AREA-EDIT-EXT
           END-IF
      *
      *    履歴区分／入院中区分
           IF    ( PTNYUINRRK-JTIKBN   =   "5" )
               IF    ( PTNYUINRRK-TAINRELKBN  =   "1" )
                   MOVE    SPA-GMN-I03-RRKKBN-ITM (03)
                                   TO    SPA-GMN-I03-RRKKBN
               ELSE
                   MOVE    SPA-GMN-I03-RRKKBN-ITM (02)
                                   TO    SPA-GMN-I03-RRKKBN
               END-IF
      *        MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (01)
      *                            TO    SPA-GMN-I03-NYUINCHUKBN
               MOVE    SPACE
                                   TO    SPA-GMN-I03-NYUINCHUKBN
           ELSE
               MOVE    SPA-GMN-I03-RRKKBN-ITM (01)
                                   TO    SPA-GMN-I03-RRKKBN
      *
               IF    ( PTNYUINRRK-NYUINCHUKBN    = "0" )
                   MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (01)
                                   TO    SPA-GMN-I03-NYUINCHUKBN
               ELSE
                   MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (02)
                                   TO    SPA-GMN-I03-NYUINCHUKBN
               END-IF
      *
           END-IF
      *
      *    初回継続区分
           MOVE  PTNYUINRRK-SHOKAIKBN
                                   TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-I03-SHOKAI
      *
      *    初回番号
           MOVE    PTNYUINRRK-SHONUM
                                   TO  SPA-GMN-I03-SHONUM
      *
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SPA-GMN-I03-COMMENT
      *
      *    入院日
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I03-NYUINYMD
                                       SPA-NAI-I03-NYUINYMD-MAE
           MOVE    WRK-SYMD        TO  SPA-NAI-I03-NYUINYMD
      *
      *    退院日
           MOVE    PTNYUINRRK-TAIINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I03-TAIINYMD
                                       SPA-NAI-I03-TAIINYMD-MAE
           MOVE    WRK-SYMD        TO  SPA-NAI-I03-TAIINYMD
      *
      *    通算対象日数
           MOVE    PTNYUINRRK-TAISYONISSU
                                   TO  SPA-GMN-I03-TAISYONISSU
      *
           PERFORM 900-PTTAINFUKA-KEY-SEL-SEC
           IF    ( FLG-TAINFUKA    NOT =   ZERO )
               GO  TO  4101-INPUT-AREA-EDIT-EXT
           END-IF
      *
           MOVE    MCPDATA-REC TO  PTTAINFUKA-REC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
      *
      *        病名
               MOVE    PTTAINFUKA-BYOMEI (IDX1)
                                   TO  SPA-GMN-I03-BYOMEI (IDX1)
      *
           END-PERFORM
      *
      *    入院基本料
           MOVE    PTTAINFUKA-KHNRYO
                                   TO  SPA-GMN-I03-KHNRYO
      *
       .
      *
       4101-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    初回チェック処理
      *****************************************************************
       4101-SHOKAI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-I03-SHOKAI  =   SPACE )
               MOVE    ZERO        TO  SPA-GMN-I03-SHONUM
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I03-SHOKAI-CD
                                       TO  WRK-CMB-CD
           PERFORM 4103-CMB-SHOKAI-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM TO  SPA-GMN-I03-SHOKAI
           ELSE
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-I03-CUR
               GO  TO  4101-SHOKAI-CHK-EXT
           END-IF
      *
      *
           IF    ( SPA-I01-I03-SYORIKBN    =   "1" )
               IF    ( SPA-GMN-I03-SHOKAI-CD   =   "2" )
                   MOVE    SPA-I01-I03-SHONUM
                                   TO  SPA-GMN-I03-SHONUM
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-I03-SHONUM
               END-IF
           END-IF
      *
           .
      *
       4101-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       4101-COMMENT-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-I03-COMMENT =   SPACE )
               GO  TO  4101-COMMENT-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-I03-COMMENT
                                   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-I03-COMMENT
           IF  ( FLG-ENTRYCHK          =   1 )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-I03-CUR
               GO  TO  4101-COMMENT-CHK-EXT
           END-IF
      *
           .
      *
       4101-COMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       4101-NYUINYMD-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I03-NYUINYMD
      *
           IF    ( SPA-GMN-I03-NYUINYMD    =   SPACE )
               GO  TO  4101-NYUINYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I03-NYUINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    3           TO  SPA-GMN-I03-CUR
               GO  TO  4101-NYUINYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I03-NYUINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I03-NYUINYMD
      *
           IF      ( SPA-GMN-I03-TAIINYMD  =   SPACE )
               MOVE    SPA-GMN-I03-NYUINYMD
                                   TO  SPA-GMN-I03-TAIINYMD
               MOVE    SPA-NAI-I03-NYUINYMD
                                   TO  SPA-NAI-I03-TAIINYMD
           END-IF
      *
      *    通算日数計算処理
           PERFORM 4101-TUSAN-CALC-SEC
      *
           .
      *
       4101-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通算日数計算処理
      *****************************************************************
       4101-TUSAN-CALC-SEC         SECTION.
      *
           IF    ( SPA-NAI-I03-NYUINYMD  NOT =   SPACE )
            AND  ( SPA-NAI-I03-TAIINYMD  NOT =   SPACE )
               MOVE    SPA-NAI-I03-NYUINYMD
                                   TO  WRK-STYMD
               MOVE    SPA-NAI-I03-TAIINYMD
                                   TO  WRK-EDYMD
               PERFORM 5002-KIKAN-CALC-SEC
               IF    ( WRK-NISSU2  <   100000 )
                   MOVE    WRK-NISSU2  TO  SPA-GMN-I03-TAISYONISSU
               ELSE
                   MOVE    99999       TO  SPA-GMN-I03-TAISYONISSU
               END-IF
           END-IF
      *
           .
      *
       4101-TUSAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    退院日チェック処理
      *****************************************************************
       4101-TAIINYMD-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-I03-TAIINYMD
      *
           IF    ( SPA-GMN-I03-TAIINYMD    =   SPACE )
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-I03-TAIINYMD
                                   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    4           TO  SPA-GMN-I03-CUR
               GO  TO  4101-TAIINYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-I03-TAIINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-I03-TAIINYMD
      *
      *    通算日数計算処理
           PERFORM 4101-TUSAN-CALC-SEC
      *
           .
      *
       4101-TAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通算対象日数チェック処理
      *****************************************************************
       4101-TAISYONISSU-CHK-SEC    SECTION.
      *
      *
           .
      *
       4101-TAISYONISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4101-BYOMEI-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-I03-BYOMEI (IDX1)   =   SPACE )
               GO  TO  4101-BYOMEI-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-I03-BYOMEI (IDX1)
                                   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-I03-BYOMEI (IDX1)
           IF  ( FLG-ENTRYCHK          =   1 )
               MOVE    "0002"          TO  SPA-ERRCD
               COMPUTE SPA-GMN-I03-CUR =   IDX1    +   6
               GO  TO  4101-BYOMEI-CHK-EXT
           END-IF
      *
           .
      *
       4101-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料チェック処理
      *****************************************************************
       4101-KHNRYO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-I03-KHNRYO  =   SPACE )
               GO  TO  4101-KHNRYO-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-I03-KHNRYO
                                   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM     800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-I03-KHNRYO
           IF  ( FLG-ENTRYCHK          =   1 )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    12              TO  SPA-GMN-I03-CUR
               GO  TO  4101-KHNRYO-CHK-EXT
           END-IF
      *
           .
      *
       4101-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴区分チェック処理
      *****************************************************************
       4101-RRKKBN-CHK-SEC             SECTION.
      *
           MOVE    I03-RRKKBN      TO  SPA-GMN-I03-RRKKBN
      *
           EVALUATE    SPA-GMN-I03-RRKKBN-CD
           WHEN    "1"
               MOVE    SPA-GMN-I03-RRKKBN-ITM (1)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (1)
                                   TO  SPA-GMN-I03-NYUINCHUKBN
           WHEN    "3"
               MOVE    SPA-GMN-I03-RRKKBN-ITM (3)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPACE       TO  SPA-GMN-I03-NYUINCHUKBN
           WHEN    OTHER
               MOVE    SPA-GMN-I03-RRKKBN-ITM (2)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPACE       TO  SPA-GMN-I03-NYUINCHUKBN
           END-EVALUATE
      *
           .
      *
       4101-RRKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴区分チェック処理
      *****************************************************************
       4102-RRKKBN-CHK-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-I03-RRKKBN-CD
           WHEN    "1"
               MOVE    SPA-GMN-I03-RRKKBN-ITM (2)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPACE       TO  SPA-GMN-I03-NYUINCHUKBN
           WHEN    "3"
               MOVE    SPA-GMN-I03-RRKKBN-ITM (1)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (1)
                                   TO  SPA-GMN-I03-NYUINCHUKBN
           WHEN    OTHER
               MOVE    SPA-GMN-I03-RRKKBN-ITM (3)
                                   TO  SPA-GMN-I03-RRKKBN
               MOVE    SPACE       TO  SPA-GMN-I03-NYUINCHUKBN
           END-EVALUATE
      *
           .
      *
       4102-RRKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院中区分チェック処理
      *****************************************************************
       4101-NYUINCHUKBN-CHK-SEC        SECTION.
      *
           MOVE    6               TO  SPA-GMN-I03-CUR
      *
           MOVE    I03-NYUINCHUKBN TO  SPA-GMN-I03-NYUINCHUKBN
      *
           IF    ( SPA-GMN-I03-NYUINCHUKBN-CD  =   "1" )
               MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (2)
                                   TO  SPA-GMN-I03-NYUINCHUKBN
           ELSE
               MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (1)
                                   TO  SPA-GMN-I03-NYUINCHUKBN
           END-IF
      *
           MOVE    7               TO  SPA-GMN-I03-CUR
      *
           .
      *
       4101-NYUINCHUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回サーチ処理
      *****************************************************************
       4103-CMB-SHOKAI-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I03-SHOKAI-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-I03-SHOKAIL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I03-SHOKAI-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4103-CMB-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自院／他院サーチ処理
      *****************************************************************
       4103-CMB-RRKKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I03-RRKKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
               IF    ( SPA-GMN-I03-RRKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I03-RRKKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-RRKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院中区分サーチ処理
      *****************************************************************
       4103-CMB-NYUINCHUKBN-CHK-SEC     SECTION.
      *
           INITIALIZE              WRK-CMB-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-I03-NYUINCHUKBN-CNT )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
               IF    ( SPA-GMN-I03-NYUINCHUKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    IDX-CMB TO  WRK-CMB-IDX
                   MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
           END-PERFORM
      *
           .
      *
       4103-CMB-NYUINCHUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "B01"               TO  SPA-I01-I03-BTN
      *
           MOVE    "I01"               TO  SPA-SAKIPG
           MOVE    "I03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
      *
           MOVE    SPA-GMN-I03-SHOKAI  TO  WRK-BAK-SHOKAI
           MOVE    SPA-GMN-I03-SHONUM
                                       TO  WRK-BAK-SHONUM
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    WRK-BAK-SHOKAI      TO  SPA-GMN-I03-SHOKAI
           MOVE    WRK-BAK-SHONUM      TO  SPA-GMN-I03-SHONUM
      *
           MOVE    LOW-VALUE           TO  SPA-NAI-I03-NYUINYMD-MAE
                                           SPA-NAI-I03-TAIINYMD-MAE
      *
           MOVE    1                   TO  SPA-GMN-I03-CUR
      *
           .
      *
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-I03-INPUT-AREA
                                   SPA-NAI-I03-INPUT-AREA
      *
           MOVE    SPA-GMN-I03-SHOKAI-ITM (01)
                                       TO  SPA-GMN-I03-SHOKAI
           MOVE    SPA-GMN-I03-RRKKBN-ITM (02)
                                       TO  SPA-GMN-I03-RRKKBN
      *    MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (1)
      *                                TO  SPA-GMN-I03-NYUINCHUKBN
           MOVE    SPACE
                                       TO  SPA-GMN-I03-NYUINCHUKBN
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    他院歴削除処理
      *****************************************************************
       480-DEL-SEC                     SECTION.
      *
           MOVE    "3001"          TO  SPA-I0IDCD
      *
           .
      *
       480-DEL-EXT.
           EXIT.
      *****************************************************************
      *    他院歴登録処理
      *****************************************************************
       480-TOUROKU-SEC                     SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4801-KANREN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
      *    既に歴データが存在するかチェックする
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               IF    ( SPA-I01-I03-SYORIKBN    =   "1" )
                   MOVE    "2001"  TO  SPA-ERRCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
               IF    ( PTNYUINRRK-JTIKBN       =   "5" OR  "6" )
                   CONTINUE
               ELSE
                   MOVE    "2005"  TO  SPA-ERRCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
      *
      *        更新用に作成年月日を退避
               MOVE    PTNYUINRRK-CREYMD
                                   TO  WRK-CREYMD
               MOVE    "UPD"       TO  WRK-UPDKBN
           ELSE
               IF    ( SPA-I01-I03-SYORIKBN    =   "2" OR "3" )
                   MOVE    "2001"  TO  SPA-ERRCD
                   GO  TO  480-TOUROKU-EXT
               END-IF
               MOVE    "INS"       TO  WRK-UPDKBN
           END-IF
      *
      *    入院履歴マスタ更新処理
           PERFORM 4801-PTNYUINRRK-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
      *    他院歴付加情報更新処理
           PERFORM 4801-PTTAINFUKA-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-I03-CUR
           PERFORM 210-BACK
           MOVE    "B12"           TO  SPA-I01-I03-BTN
      *
           .
      *
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスタ更新処理
      *****************************************************************
       4801-PTNYUINRRK-UPD-SEC     SECTION.
      *
      *    入院履歴データ編集
           PERFORM 4801-PTNYUINRRK-EDIT-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           EVALUATE WRK-UPDKBN
           WHEN    "INS"
      *        入院履歴マスタ追加
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBINSERT-SEC
           WHEN    "UPD"
      *        入院履歴マスタ更新
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBUPDATE-SEC
           WHEN    "DEL"
      *        入院履歴マスタ削除
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               PERFORM 910-DBDELETE-SEC
           END-EVALUATE
      *
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "2002"      TO  SPA-ERRCD
               GO  TO  4801-PTNYUINRRK-UPD-EXT
           END-IF
      *
           .
      *
       4801-PTNYUINRRK-UPD-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴データ編集
      *****************************************************************
       4801-PTNYUINRRK-EDIT-SEC    SECTION.
      *
      *    入院履歴データ編集前処理（初回番号を求める）
           PERFORM 4801-PTNYUINRRK-EDIT-MAE-SEC
      *
           INITIALIZE              PTNYUINRRK-REC
      *
      *    医療機関ＩＤ
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
      *
      *    患者ＩＤ
           MOVE    SPA-I01-I03-PTID
                                   TO  PTNYUINRRK-PTID
      *
      *    入院履歴番号
           MOVE    SPA-I01-I03-PTRRKNUM
                                   TO  PTNYUINRRK-RRKNUM
      *
           IF    ( WRK-UPDKBN      =   "DEL" )
               GO  TO  4801-PTNYUINRRK-EDIT-EXT
           END-IF
      *
      *    入院履歴枝番号
           MOVE    1
                                   TO  PTNYUINRRK-RRKEDANUM
      *
      *    病棟名称
           MOVE    SPA-GMN-I03-COMMENT
                                   TO  PTNYUINRRK-BTUNAME
      *
      *    入院日
           MOVE    SPA-NAI-I03-NYUINYMD
                                   TO  PTNYUINRRK-NYUINYMD
      *    退院日
           MOVE    SPA-NAI-I03-TAIINYMD
                                   TO  PTNYUINRRK-TAIINYMD
      *
      *    初回継続区分
           MOVE    SPA-GMN-I03-SHOKAI-CD
                                   TO  PTNYUINRRK-SHOKAIKBN
      *
      *    初回番号
           MOVE    WRK-SHONUM      TO  PTNYUINRRK-SHONUM
      *
      *    状態区分
           IF    ( SPA-GMN-I03-RRKKBN-CD   =   "2" OR "3" )
               MOVE    "5"             TO  PTNYUINRRK-JTIKBN
           ELSE
               MOVE    "6"             TO  PTNYUINRRK-JTIKBN
           END-IF
      *    転入日
           MOVE    SPA-NAI-I03-NYUINYMD
                                   TO  PTNYUINRRK-TENNYUYMD
      *    転出日
           MOVE    SPA-NAI-I03-TAIINYMD
                                   TO  PTNYUINRRK-TENSTUYMD
      *    通算対象日数
           MOVE    SPA-GMN-I03-TAISYONISSU
                                   TO  PTNYUINRRK-TAISYONISSU
      *
      *    入院中区分
           IF    ( SPA-GMN-I03-NYUINCHUKBN-CD  =   "1" )
               MOVE    "1"         TO  PTNYUINRRK-NYUINCHUKBN
           ELSE
               MOVE    "0"         TO  PTNYUINRRK-NYUINCHUKBN
           END-IF
      *
           MOVE    1               TO  PTNYUINRRK-MAXEDANUM
      *
      *    他院区分
           IF    ( SPA-GMN-I03-RRKKBN-CD   =   "3" )
               MOVE    "1"         TO  PTNYUINRRK-TAINRELKBN
           END-IF
      *
           IF    ( WRK-UPDKBN      =   "UPD" )
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    WRK-CREYMD      TO  PTNYUINRRK-CREYMD
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTNYUINRRK-CREYMD
                                           PTNYUINRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTNYUINRRK-UPHMS
           END-IF
      *
           .
      *
       4801-PTNYUINRRK-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴データ編集前処理
      *****************************************************************
       4801-PTNYUINRRK-EDIT-MAE-SEC    SECTION.
      *
           MOVE    ZERO                    TO  WRK-SHONUM
      *
      *    初回登録場合、設定する初回番号を求める
           IF    ( WRK-UPDKBN              =   "INS" )
            AND  ( SPA-GMN-I03-SHOKAI-CD   =   "1"  )
      *        入院履歴マスタ検索
               MOVE    ZERO                TO  FLG-PTNYUINRRK
               INITIALIZE  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-I01-I03-PTID    TO  PTNYUINRRK-PTID
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC      =   ZERO  )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
                   COMPUTE WRK-SHONUM  =   1
               ELSE
                   COMPUTE WRK-SHONUM  =   PTNYUINRRK-SHONUM   +   1
               END-IF
           ELSE
               COMPUTE WRK-SHONUM      =   SPA-GMN-I03-SHONUM
           END-IF
      *
           .
      *
       4801-PTNYUINRRK-EDIT-MAE-EXT.
           EXIT.
      *****************************************************************
      *    他院歴付加情報更新処理
      *****************************************************************
       4801-PTTAINFUKA-UPD-SEC     SECTION.
      *
           PERFORM 900-PTTAINFUKA-KEY-SEL-SEC
           IF    ( FLG-TAINFUKA    =   ZERO )
               MOVE    "UPD"       TO  WRK-UPDKBN
               MOVE    PTTAINFUKA-CREYMD
                                   TO  WRK-CREYMD
           ELSE
               MOVE    "INS"       TO  WRK-UPDKBN
           END-IF
      *
      *    他院付加情報編集処理
           PERFORM 4801-PTTAINFUKA-EDIT-SEC
      *
           MOVE    "tbl_pttainfuka"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PTTAINFUKA-REC  TO  MCPDATA-REC
      *
           IF   ( WRK-UPDKBN   =   "UPD" )
                   PERFORM 910-DBUPDATE-SEC
           ELSE
                   PERFORM 910-DBINSERT-SEC
           END-IF
      *
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "2003"      TO  SPA-ERRCD
               GO  TO  4801-PTTAINFUKA-UPD-EXT
           END-IF
      *
           .
      *
       4801-PTTAINFUKA-UPD-EXT.
           EXIT.
      *****************************************************************
      *    他院付加情報編集処理
      *****************************************************************
       4801-PTTAINFUKA-EDIT-SEC    SECTION.
      *
           INITIALIZE              PTTAINFUKA-REC
      *
      *    医療機関ＩＤ
           MOVE    SPA-HOSPNUM      TO  PTTAINFUKA-HOSPNUM
      *    患者ＩＤ
           MOVE    SPA-I01-I03-PTID
                                   TO  PTTAINFUKA-PTID
      *    入院履歴番号
           MOVE    SPA-I01-I03-PTRRKNUM
                                   TO  PTTAINFUKA-RRKNUM
      *    病名
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
               MOVE    SPA-GMN-I03-BYOMEI (IDX1)
                                   TO  PTTAINFUKA-BYOMEI (IDX1)
           END-PERFORM
      *
      *    入院基本料
           MOVE    SPA-GMN-I03-KHNRYO
                                   TO  PTTAINFUKA-KHNRYO
      *
           IF    ( WRK-UPDKBN      =   "UPD" )
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    WRK-CREYMD      TO  PTTAINFUKA-CREYMD
               MOVE    SMCNDATE-YMD    TO  PTTAINFUKA-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTTAINFUKA-UPHMS
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  PTTAINFUKA-CREYMD
                                           PTTAINFUKA-UPYMD
               MOVE    SMCNDATE-HMS    TO  PTTAINFUKA-UPHMS
           END-IF
      *
           .
      *
       4801-PTTAINFUKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    他院歴付加情報削除処理
      *****************************************************************
       4801-PTTAINFUKA-DEL-SEC     SECTION.
      *
           INITIALIZE  PTTAINFUKA-REC
           MOVE    SPA-HOSPNUM      TO  PTTAINFUKA-HOSPNUM
           MOVE    SPA-I01-I03-PTID
                                   TO  PTTAINFUKA-PTID
           MOVE    SPA-I01-I03-PTRRKNUM
                                   TO  PTTAINFUKA-RRKNUM
           MOVE    "tbl_pttainfuka"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "2004"      TO  SPA-ERRCD
               GO  TO  4801-PTTAINFUKA-DEL-EXT
           END-IF
      *
           .
      *
       4801-PTTAINFUKA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4801-KANREN-CHK-SEC         SECTION.
      *
      *    必須入力チェック
      *    初回
           IF    ( SPA-GMN-I03-SHOKAI      =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF


      *    コメント
           IF    ( SPA-GMN-I03-COMMENT     =   SPACE )
               MOVE    "1002"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院日
           IF    ( SPA-GMN-I03-NYUINYMD    =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    退院日
           IF    ( SPA-GMN-I03-TAIINYMD    =   SPACE )
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    4           TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    自院／他院区分
           IF    ( SPA-GMN-I03-RRKKBN      =   SPACE )
               MOVE    "1009"      TO  SPA-ERRCD
               MOVE    99          TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院中区分
           IF    ( SPA-GMN-I03-RRKKBN-CD   =   "1" )
               IF    ( SPA-GMN-I03-NYUINCHUKBN =   SPACE )
                   MOVE    "1010"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-I03-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    継続の場合、
      *    前回退院日＜入院日となっていること
           IF    ( SPA-GMN-I03-SHOKAI-CD   =   "2" )
               IF    ( SPA-I01-I03-TAIINYMD
                                   <   SPA-NAI-I03-NYUINYMD )
                   CONTINUE
               ELSE
                   MOVE    "1007"      TO  SPA-ERRCD
                   MOVE    3           TO  SPA-GMN-I03-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    入院期間重複チェック
           PERFORM 4801-NYUKIKAN-DABURI-CHK
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院日≦退院日となっていること
           IF    ( SPA-NAI-I03-NYUINYMD
                                  <=   SPA-NAI-I03-TAIINYMD )
               CONTINUE
           ELSE
               MOVE    "1005"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-I03-CUR
               GO  TO  4801-KANREN-CHK-EXT
           END-IF
      *
      *    入院期間≧通算対象日数となっていること
      *    （自院歴のみチェックを行う）
           MOVE    SPA-NAI-I03-NYUINYMD
                                   TO  WRK-STYMD
           MOVE    SPA-NAI-I03-TAIINYMD
                                   TO  WRK-EDYMD
           PERFORM 5002-KIKAN-CALC-SEC
           IF    SPA-GMN-I03-RRKKBN-CD      =    "1"
               IF    ( WRK-NISSU2     >=   SPA-GMN-I03-TAISYONISSU )
                   CONTINUE
               ELSE
                   MOVE    "1006"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-I03-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    継続の場合、
      *    継続元が自院歴で、「入院中」でないこと
           IF    ( SPA-GMN-I03-SHOKAI-CD   =   "2" )
      *
               PERFORM 4801-ZIIN-NYUINCHU-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    「入院中」を選択した場合、
      *    既に継続先のデータが存在する場合は、
      *    継続先のデータが入院登録で作成したデータで、
      *    退院日の翌日が入院日となっていること
           IF      ( SPA-GMN-I03-NYUINCHUKBN-CD    =   "1" )
               PERFORM 4801-KEIZOKU-NYUIN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   MOVE    6       TO  SPA-GMN-I03-CUR
                   GO  TO  4801-KANREN-CHK-EXT
               END-IF
           END-IF
      *
           .
       4801-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院期間重複チェック
      *****************************************************************
       4801-NYUKIKAN-DABURI-CHK        SECTION.
      *
           INITIALIZE  PTNYUINRRK-REC
      *    入院履歴マスタ検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I03-PTID
                                       TO  PTNYUINRRK-PTID
           MOVE    SPA-I01-I03-PTRRKNUM
                                       TO  PTNYUINRRK-RRKNUM
           MOVE    SPA-NAI-I03-NYUINYMD
                                       TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-NAI-I03-TAIINYMD
                                       TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key7"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key7"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               MOVE    "1008"          TO  SPA-ERRCD
               GO  TO  4801-NYUKIKAN-DABURI-EXT
           END-IF
      *
           .
       4801-NYUKIKAN-DABURI-EXT.
           EXIT.
      *****************************************************************
      *    自院入院中チェック
      *****************************************************************
       4801-ZIIN-NYUINCHU-CHK-SEC          SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY16-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( PTNYUINRRK-RRKNUM
                                           NOT =   SPA-I01-I03-PTRRKNUM)
      *
               PERFORM 900-PTNYUINRRK-KEY16-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               IF    ( PTNYUINRRK-JTIKBN       =   "6" )
                AND  ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                   MOVE    "1011"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4801-ZIIN-NYUINCHU-EXT.
           EXIT.
      *****************************************************************
      *    継続入院チェック処理
      *****************************************************************
       4801-KEIZOKU-NYUIN-CHK-SEC  SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY20-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( PTNYUINRRK-RRKNUM
                                           NOT =   SPA-I01-I03-PTRRKNUM)
      *
               PERFORM 900-PTNYUINRRK-KEY20-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTNYUINRRK      =   ZERO )
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-I03-TAIINYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    1               TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO  WRK-DAY6-KEISAN
      *
               EVALUATE PTNYUINRRK-JTIKBN
               WHEN    "5" 
                   MOVE    "1013"      TO  SPA-ERRCD
               WHEN    "6"
                   MOVE    "1014"      TO  SPA-ERRCD
               WHEN    OTHER
                   IF    ( PTNYUINRRK-NYUINYMD NOT =   WRK-DAY6-KEISAN )
                       MOVE    "1012"  TO  SPA-ERRCD
                       GO  TO  4801-KEIZOKU-NYUIN-CHK-EXT
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
       4801-KEIZOKU-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-I0IDCD      NOT =   SPACE )
               PERFORM 520-I0IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *    初回継続区分
           MOVE    SPA-GMN-I03-SHOKAI
                                   TO  I03-SHOKAI
           MOVE    SPA-GMN-I03-SHOKAI-CNT
                                   TO  I03-SHOKAI-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I03-SHOKAI-CNT )
               MOVE    SPA-GMN-I03-SHOKAI-ITM (IDX1)
                                   TO  I03-SHOKAI-ITM (IDX1)
           END-PERFORM
      *
      *    初回番号
           MOVE    SPA-GMN-I03-SHONUM
                                   TO  I03-SHONUM
      *    コメント
           MOVE    SPA-GMN-I03-COMMENT
                                   TO  I03-COMMENT
      *    入院日
           MOVE    SPA-GMN-I03-NYUINYMD
                                   TO  I03-NYUINYMD
      *    退院日
           MOVE    SPA-GMN-I03-TAIINYMD
                                   TO  I03-TAIINYMD
      *    通算対象日数
           MOVE    SPA-GMN-I03-TAISYONISSU
                                   TO  I03-TAISYONISSU
      *
      *    病名
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   5 )
               MOVE    SPA-GMN-I03-BYOMEI (IDX1)
                                   TO  I03-BYOMEI (IDX1)
           END-PERFORM
      *
      *    入院基本料
           MOVE    SPA-GMN-I03-KHNRYO
                                   TO  I03-KHNRYO
      *
      *    履歴区分
           MOVE    SPA-GMN-I03-RRKKBN
                                   TO  I03-RRKKBN
           MOVE    SPA-GMN-I03-RRKKBN-CNT
                                   TO  I03-RRKKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I03-RRKKBN-CNT )
               MOVE    SPA-GMN-I03-RRKKBN-ITM (IDX1)
                                   TO  I03-RRKKBN-ITM (IDX1)
           END-PERFORM
      *
      *    入院中区分
           MOVE    SPA-GMN-I03-NYUINCHUKBN
                                   TO  I03-NYUINCHUKBN
           MOVE    SPA-GMN-I03-NYUINCHUKBN-CNT
                                   TO  I03-NYUINCHUKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-I03-NYUINCHUKBN-CNT )
               MOVE    SPA-GMN-I03-NYUINCHUKBN-ITM (IDX1)
                                   TO  I03-NYUINCHUKBN-ITM (IDX1)
           END-PERFORM
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL           TO  I03-B02-STT
                                               I03-B03-STT
                                               I03-B04-STT
                                               I03-SHOKAI-STT
                                               I03-SHOKAI-ITM-STT
                                               I03-SHONUM-STT
                                               I03-NYUINYMD-STT
                                               I03-TAIINYMD-STT
                                               I03-TAISYONISSU-STT
                                               I03-RRKKBN-STT
                                               I03-RRKKBN-ITM-STT
                                               I03-NYUINCHUKBN-STT
                                               I03-NYUINCHUKBN-ITM-STT
      *
           EVALUATE    SPA-I01-I03-SYORIKBN
      *    入院歴作成
           WHEN    "1"
               MOVE    WIDGET-INSENSITIVE  TO  I03-B04-STT
      *
      *    入院歴修正
           WHEN    "2"
               MOVE    WIDGET-INSENSITIVE  TO  I03-SHOKAI-STT
                                               I03-SHOKAI-ITM-STT
                                               I03-SHONUM-STT
      *
      *    入院歴修正（日付項目変更不可）
           WHEN    "3"
               MOVE    WIDGET-INSENSITIVE  TO  I03-B02-STT
                                               I03-B03-STT
                                               I03-B04-STT
                                               I03-SHOKAI-STT
                                               I03-SHOKAI-ITM-STT
                                               I03-SHONUM-STT
                                               I03-NYUINYMD-STT
                                               I03-TAIINYMD-STT
                                               I03-TAISYONISSU-STT
                                               I03-RRKKBN-STT
                                               I03-RRKKBN-ITM-STT
           END-EVALUATE
      *
           IF    ( SPA-GMN-I03-RRKKBN-CD   =   "1" )
               MOVE    WIDGET-NORMAL       TO  I03-NYUINCHUKBN-STT
                                               I03-NYUINCHUKBN-ITM-STT
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  I03-NYUINCHUKBN-STT
                                               I03-NYUINCHUKBN-ITM-STT
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC              SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-I03-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-CUR-SKIP-SEC
      *
           EVALUATE SPA-GMN-I03-CUR
           WHEN    1
               MOVE    "SHOKAI"        TO  MCP-WIDGET
           WHEN    2
               MOVE    "COMMENT"       TO  MCP-WIDGET
           WHEN    3   
               MOVE    "NYUINYMD"      TO  MCP-WIDGET
           WHEN    4
               MOVE    "TAIINYMD"      TO  MCP-WIDGET
           WHEN    5
               MOVE    "TAISYONISSU"   TO  MCP-WIDGET
           WHEN    6
               MOVE    "NYUINCHUKBN"   TO  MCP-WIDGET
           WHEN    7
               MOVE    "BYOMEI1"       TO  MCP-WIDGET
           WHEN    8
               MOVE    "BYOMEI2"       TO  MCP-WIDGET
           WHEN    9
               MOVE    "BYOMEI3"       TO  MCP-WIDGET
           WHEN    10
               MOVE    "BYOMEI4"       TO  MCP-WIDGET
           WHEN    11
               MOVE    "BYOMEI5"       TO  MCP-WIDGET
           WHEN    12
               MOVE    "KHNRYO"        TO  MCP-WIDGET
           WHEN    99
               MOVE    "RRKKBN"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SHOKAI"
               MOVE    001             TO  SPA-GMN-I03-CUR
           WHEN    "COMMENT"
               MOVE    002             TO  SPA-GMN-I03-CUR
           WHEN    "NYUINYMD"
               MOVE    003             TO  SPA-GMN-I03-CUR
           WHEN    "TAIINYMD"
               MOVE    004             TO  SPA-GMN-I03-CUR
           WHEN    "TAISYONISSU"
               MOVE    005             TO  SPA-GMN-I03-CUR
           WHEN    "NYUINCHUKBN"
               MOVE    006             TO  SPA-GMN-I03-CUR
           WHEN    "BYOMEI1"
               MOVE    007             TO  SPA-GMN-I03-CUR
           WHEN    "BYOMEI2"
               MOVE    008             TO  SPA-GMN-I03-CUR
           WHEN    "BYOMEI3"
               MOVE    009             TO  SPA-GMN-I03-CUR
           WHEN    "BYOMEI4"
               MOVE    010             TO  SPA-GMN-I03-CUR
           WHEN    "BYOMEI5"
               MOVE    011             TO  SPA-GMN-I03-CUR
           WHEN    "KHNRYO"
               MOVE    012             TO  SPA-GMN-I03-CUR
           END-EVALUATE
      *
      *    次カーソル位置を設定
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "KHNRYO"
               MOVE    212             TO  SPA-GMN-I03-CUR
           WHEN    OTHER
               COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR
                                       +   1
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-CUR-SKIP-SEC           SECTION.
      *
           IF    ( SPA-GMN-I03-CUR =   1   )
               IF    ( I03-SHOKAI-STT      =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR +   1
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I03-CUR =   3   )
               IF    ( I03-NYUINYMD-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR +   1
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I03-CUR =   4   )
               IF    ( I03-TAIINYMD-STT    =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR +   1
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I03-CUR =   5   )
               IF    ( I03-TAISYONISSU-STT =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR +   1
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-I03-CUR =   6  )
               IF    ( I03-NYUINCHUKBN-STT =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-I03-CUR =   SPA-GMN-I03-CUR +   1
               END-IF
           END-IF
      *
           .
      *
       5012-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"    TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "全角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "初回を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "コメントを入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "入院日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "退院日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "退院日＜入院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "入院期間＜通算対象日数となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "入院日≦前回退院日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "入院期間が他の入院歴と重複しています"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "自院／他院を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "退院済／入院中を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "継続元の歴が「自院歴−入院中」の場合は、"
                                       TO  SPA-ERRMSG (1:40)
               MOVE    "登録処理は行えません。"
                                       TO  SPA-ERRMSG (41:)
           WHEN    "1012"
               MOVE    "継続先の入院と歴日がつながっていないため"
                                       TO  SPA-ERRMSG (1:40)
               MOVE    "、「退院済」として登録してください"
                                       TO  SPA-ERRMSG (41:)
           WHEN    "1013"
               MOVE    "直後の継続入院の歴が他院歴の場合は、「入"
                                       TO  SPA-ERRMSG (1:40)
               MOVE    "院中」として登録できません"
                                       TO  SPA-ERRMSG (41:)
           WHEN    "1014"
               MOVE    "直後の継続入院の歴が自院歴の場合は、「入"
                                       TO  SPA-ERRMSG (1:40)
               MOVE    "院中」として登録できません"
                                       TO  SPA-ERRMSG (41:)
           WHEN    "2001"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "2002"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "2003"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "2004"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "2005"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I0ERR
           MOVE    SPA-ERRCD           TO  I0ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I0ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I03"               TO  SPA-MOTOPG
           MOVE    "I0ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I0ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I0IDSET-SEC             SECTION.
      *
           EVALUATE    SPA-I0IDCD
           WHEN    "3001"
               MOVE    "入院歴情報を削除します"
                                        TO  WRK-I0IDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  I0ID1
           MOVE    SPA-I0IDCD           TO  I0ID1-ID1CODE
           MOVE    WRK-I0IDMSG          TO  I0ID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "I03"                TO  SPA-MOTOPG
           MOVE    "I0ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "I0ID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-I0IDSET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-SYMD
      *
           IF    ( WRK-YMD         =   SPACE )
               GO  TO  5002-HIZUKE-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       5002-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       5002-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-KANACHK-OUT-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
      *    全角でない場合はエラーとする。
           IF      ( KANACHK-SYUBETU   NOT =   2 )
               MOVE    1           TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY16-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I03-PTID    TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I03-NYUINYMD
                                       TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-GMN-I03-SHONUM  TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY16-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY16-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY20-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I03-PTID
                                       TO  PTNYUINRRK-PTID
           MOVE    SPA-NAI-I03-TAIINYMD
                                       TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-GMN-I03-SHONUM  TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY20-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO  )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-I01-I03-PTID
                                   TO  PTNYUINRRK-PTID
           MOVE    SPA-I01-I03-PTRRKNUM
                                   TO  PTNYUINRRK-RRKNUM
           MOVE    1               TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者他院付加情報検索処理
      *****************************************************************
       900-PTTAINFUKA-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TAINFUKA
      *
           INITIALIZE  PTTAINFUKA-REC
           MOVE    SPA-HOSPNUM         TO  PTTAINFUKA-HOSPNUM
           MOVE    SPA-I01-I03-PTID    TO  PTTAINFUKA-PTID
           MOVE    SPA-I01-I03-PTRRKNUM
                                       TO  PTTAINFUKA-RRKNUM
           MOVE    PTTAINFUKA-REC      TO  MCPDATA-REC
           MOVE    "tbl_pttainfuka"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTAINFUKA-REC
           ELSE
               MOVE    1               TO  FLG-TAINFUKA
           END-IF
      *
           MOVE    "tbl_pttainfuka"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTAINFUKA-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

