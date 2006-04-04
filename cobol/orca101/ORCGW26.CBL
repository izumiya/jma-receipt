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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW26.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 病棟入院基本料加算情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/22    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    04/01/07  更新日付編集対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "W26SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-KANJI                           PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-DRKNGTEK                        PIC 9(01).
           03  FLG-KSNCD-G.
               05  FLG-190101770                   PIC 9(01).
               05  FLG-190101970                   PIC 9(01).
               05  FLG-190110170                   PIC 9(01).
               05  FLG-190106970                   PIC 9(01).
               05  FLG-190102070                   PIC 9(01).
               05  FLG-190102170                   PIC 9(01).
               05  FLG-190102270                   PIC 9(01).
               05  FLG-190102370                   PIC 9(01).
               05  FLG-190102470                   PIC 9(01).
               05  FLG-190109670                   PIC 9(01).
               05  FLG-190117070                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX-LST                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
           03  IDX-SYS                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-MAE-INPUT               PIC X(400).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(100).
           03  WRK-KANRICD                         PIC X(04).
           03  WRK-KBNCD                           PIC X(08).
           03  WRK-IDX                             PIC 9(05).
           03  WRK-SEL-CNT                         PIC 9(05).
           03  WRK-KJNYMD                          PIC X(08).
           03  WRK-NYUKSNKBN                       PIC X(03).
           03  WRK-BTU-NYUKSNKBN                   PIC X(03).
           03  WRK-KSN-NYUKSNKBN                   PIC X(03).
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-STYUKYMD                        PIC X(08).
           03  WRK-BTUNAME                         PIC X(20).
           03  WRK-SELNUM                          PIC 9(03).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-KJNYMD                      PIC X(08).
           03  WRK-KSNKJNYMD                       PIC X(08).
           03  WRK-BASE-CUR                        PIC 9(05).
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
      *
       01  SSTKJN-AREA.
           03  SSTKJN-VAL.
               05  SSTKJN-VAL1                     PIC X(500).
               05  SSTKJN-VAL2                     PIC X(500).
           03  SSTKJN-R    REDEFINES       SSTKJN-VAL.
               05  SSTKJN-CD                       PIC 9(01)
                                                   OCCURS  1000.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
           03  CONST-BTUNAME-MAX           PIC 9(03) VALUE   50.
           03  CONST-SYSKSN-MAX            PIC 9(03) VALUE   14.
           03  CONST-KSN-AREA.
               05  CONST-KSN-MAX           PIC 9(03) VALUE   9.
               05  CONST-KSN-H1804-G.
                   07  CONST-KSN-H1804-VAL.
                       09                  PIC X(09) VALUE "190101770".
                       09                  PIC X(09) VALUE "190101970".
                       09                  PIC X(09) VALUE "190110170".
                       09                  PIC X(09) VALUE "190106970".
                       09                  PIC X(09) VALUE "190102070".
                       09                  PIC X(09) VALUE "190109670".
                       09                  PIC X(09) VALUE "190117070".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                   07  CONST-KSN-H1804-R
                                       REDEFINES   CONST-KSN-H1804-VAL.
                       09  CONST-KSN-H1804 PIC X(09) OCCURS  9.
               05  CONST-KSN-G.
                   07  CONST-KSN-VAL.
                       09                  PIC X(09) VALUE "190101770".
                       09                  PIC X(09) VALUE "190101970".
                       09                  PIC X(09) VALUE "190110170".
                       09                  PIC X(09) VALUE "190106970".
                       09                  PIC X(09) VALUE "190102070".
                       09                  PIC X(09) VALUE "190102170".
                       09                  PIC X(09) VALUE "190102270".
                       09                  PIC X(09) VALUE "190102370".
                       09                  PIC X(09) VALUE "190102470".
                   07  CONST-KSN-R         REDEFINES   CONST-KSN-VAL.
                       09  CONST-KSN-SRYCD PIC X(09) OCCURS  9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    施設基準コードフラグ情報
           COPY  "CPSK1006.INC".
      *    病棟情報
           COPY  "CPSK5001.INC".
      *    病棟種別
           COPY  "CPSK5101.INC".
      *    病棟特定入院料
           COPY  "CPSK5110.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
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
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W26.INC".
           COPY    "W27.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W98.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W26FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W26FREE     TO  SPA-FREE
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
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
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
           MOVE    "W26    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               MOVE    SPACE           TO  SPA-WID1-FLG
               MOVE    SPACE           TO  SPA-WIDCD
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
           INITIALIZE                  SPA-W26
                                           W26
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
      *
      *    画面初期化処理
           PERFORM 420-CLEAR-SEC
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
           INITIALIZE              SPA-GMN-W26-CMB-AREA
      *
      *    病棟コンボ
           PERFORM 310-BTUNAMELST-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟コンボ設定処理
      *****************************************************************
       310-BTUNAMELST-SET-SEC          SECTION.
      *
      *    病棟情報検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                     OR  ( IDX-CMB             >   CONST-BTUNAME-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB     TO  SPA-GMN-W26-BTUNUM-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *        病棟名
               MOVE    SYS-5001-KBNCD  TO  SPA-GMN-W26-BTUNUML
                                                       (IDX-CMB)
               MOVE    SYS-5001-BTU-NAME
                                       TO  SPA-GMN-W26-BTUNUMMEIL
                                                       (IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       310-BTUNAMELST-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 481-TOUROKU-SEC
               END-EVALUATE
           END-IF
      *
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    選択番号
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    100             TO  SPA-GMN-W26-CUR
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
      *    入院加算情報
           WHEN    "CLICKED"       ALSO    "CHK_KSN01"
                                   THRU    "CHK_KSN09"
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    病棟名称
           WHEN    "ACTIVATE"      ALSO    "BTU_NAME"
               PERFORM 4100-BTUNUM-CHK-SEC
      *
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
      *
      *    有効期間リスト
           WHEN        ANY         ALSO    "YUKOYMDLST"
               PERFORM 4101-YUKOYMDLST-SEL-SEC
      *
           WHEN    OTHER
      *
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
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W26-CUR
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
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-KSN-MAX )
      *
               MOVE    W26-CHK-KSN (IDX0)
                                   TO  SPA-GMN-W26-CHK-KSN (IDX0)
      *
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効期間リスト選択処理
      *****************************************************************
       4101-YUKOYMDLST-SEL-SEC       SECTION.
      *
           PERFORM VARYING     IDX-LST FROM    1   BY  1
                   UNTIL     ( IDX-LST >   SPA-GMN-W26-YUKOYMDLST-MAX )
      *
               IF    ( W26-YUKOYMDLST-SELECT (IDX-LST) =   "T"  )
                   MOVE    IDX-LST     TO  SPA-GMN-W26-YUKOYMDLST-SEL
                   MOVE    SPA-NAI-W26-TSTYUKYMD   (IDX-LST)
                                       TO  WRK-STYUKYMD
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
                   MOVE    SPA-GMN-W26-TSELNUM (IDX-LST)
                                       TO  SPA-GMN-W26-SELNUM
                   MOVE    SPA-GMN-W26-YUKOYMDLST-MAX
                                       TO  IDX-LST
               END-IF
      *
           END-PERFORM
           .
      *
       4101-YUKOYMDLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    病棟名称チェック処理
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選択番号チェック
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    施設基準チェック処理
           PERFORM 4100-SSTKIJUN-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W26"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "W01"               TO  MCP-WINDOW
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           INITIALIZE  SPA-GMN-W26-HEAD-AREA
                       SPA-GMN-W26-INPUT-AREA
                       SPA-NAI-W26-INPUT-AREA
                       SPA-GMN-W26-YUKOYMDLST
                       SPA-NAI-W26-YUKOYMDLST
      *
           MOVE    ALL  "F"    TO  SPA-GMN-W26-CHK-KSN-G
      *
           MOVE    SPA-SYSYMD      TO  WRK-KSNKJNYMD
           PERFORM 42011-KSN-SYOKI-SEC
      *
           MOVE    1           TO  SPA-GMN-W26-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    入力個所のチェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       481-TOUROKU-SEC                 SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    SPA-GMN-W26-BTUNUM  TO  SYS-KBNCD
           MOVE    SPA-NAI-W26-STYUKYMD TO SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
               GO  TO  481-TOUROKU-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           PERFORM 4811-BTU-KSN-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-5001-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-5001-UPHMS
      *
           MOVE    SYS-5001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "1202"      TO  SPA-ERRCD
           ELSE
      *        画面初期化
               PERFORM 420-CLEAR-SEC
           END-IF
      *
           .
       481-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    病棟加算編集処理
      *****************************************************************
       4811-BTU-KSN-EDIT-SEC           SECTION.
      *
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-KSN-MAX )
      *
               EVALUATE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
               WHEN    "190101770"
      *            難病患者等特別入院診療加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-NANBYO-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-NANBYO-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-NANBYO-KBN
                   END-IF
               WHEN    "190101970"
      *            特殊疾患入院施設管理加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-TOKUSYU-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-TOKUSYU-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-TOKUSYU-KBN
                   END-IF
               WHEN    "190110170"
      *            児童・思春期精神科入院医療管理加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-JIDO-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-JIDO-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-JIDO-KBN
                   END-IF
               WHEN    "190106970"
      *            精神科応急入院施設管理加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-SEISIN-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-SEISIN-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-SEISIN-KBN
                   END-IF
               WHEN    "190102070"
      *            看護配置加算（入院基本料３）
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KANGO-CD (1)
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KANGO-KBN(1)
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KANGO-KBN(1)
                   END-IF
               WHEN    "190102170"
      *            看護配置加算（入院基本料４）
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KANGO-CD (2)
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KANGO-KBN(2)
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KANGO-KBN(2)
                   END-IF
               WHEN    "190102270"
      *            看護配置加算（入院基本料５）
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KANGO-CD (3)
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KANGO-KBN(3)
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KANGO-KBN(3)
                   END-IF
               WHEN    "190102370"
      *            看護配置加算（入院基本料６）
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KANGO-CD (4)
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KANGO-KBN(4)
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KANGO-KBN(4)
                   END-IF
               WHEN    "190102470"
      *            看護配置加算（入院基本料７）
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KANGO-CD (5)
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KANGO-KBN(5)
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KANGO-KBN(5)
                   END-IF
               WHEN    "190109670"
      *            看護配置適合加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-KNGTEK-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-KNGTEK-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-KNGTEK-KBN
                   END-IF
               WHEN    "190117070"
      *            複数医師及び看護配置適合加算
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5001-DRTEK-CD
                   IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5001-DRTEK-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5001-DRTEK-KBN
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
      *
           .
      *
       4811-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称チェック処理
      *****************************************************************
       4100-BTUNUM-CHK-SEC           SECTION.
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    W26-BTU-NAME    TO  SPA-GMN-W26-BTUNUM-G
      *
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1           TO  SPA-GMN-W26-CUR
               GO  TO  4100-BTUNUM-CHK-EXT
           END-IF
      *
      *    入力領域編集処理
           MOVE    SPA-SYSYMD              TO  WRK-STYUKYMD
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
      *    有効期間リスト編集処理
           PERFORM 4100-YUKOYMDLST-EDIT-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-W26-CUR
      *
           .
      *
       4100-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号チェック処理
      *****************************************************************
       4101-BTUNUM-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-W26-BTUNUM  TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           IF   (  WRK-CMB-ITM     =   SPACE )
               MOVE    1           TO  SPA-GMN-W26-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W26-BTUNUM-G
           END-IF
      *
           .
      *
       4101-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    W26-SELNUM      TO  SPA-GMN-W26-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W26-SELNUM
                                   TO  SPA-GMN-W26-YUKOYMDLST-SEL
           MOVE    SPA-NAI-W26-TSTYUKYMD   (SPA-GMN-W26-SELNUM)
                                   TO  WRK-STYUKYMD
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           MOVE    W26-SELNUM      TO  SPA-GMN-W26-SELNUM
      *
           MOVE    2               TO  SPA-GMN-W26-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W26-SELNUM  <=   ZERO )
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-W26-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W26-SELNUM  >   SPA-GMN-W26-YUKOYMDLST-MAX )
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-W26-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間リスト編集処理
      *****************************************************************
       4100-YUKOYMDLST-EDIT-SEC        SECTION.
      *
           INITIALIZE      SPA-GMN-W26-YUKOYMDLST
                           SPA-NAI-W26-YUKOYMDLST
      *
           PERFORM 900-SYSKANRI-KEY4-SEL-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( IDX-LST >   100 )
      *
               MOVE    IDX-LST     TO  SPA-GMN-W26-YUKOYMDLST-MAX
      *
      *        選択番号
               MOVE    IDX-LST     TO  SPA-GMN-W26-TSELNUM (IDX-LST)
      *
      *        有効開始年月日
               MOVE    SYS-5001-STYUKYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W26-TSTYUKYMD   (IDX-LST)
               MOVE    WRK-SYMD    TO  SPA-NAI-W26-TSTYUKYMD   (IDX-LST)
      *
      *        有効終了年月日
               MOVE    SYS-5001-EDYUKYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W26-TEDYUKYMD   (IDX-LST)
               MOVE    WRK-SYMD    TO  SPA-NAI-W26-TEDYUKYMD   (IDX-LST)
      *
               IF    ( SPA-SYSYMD  >=  SYS-5001-STYUKYMD )
                AND  ( SPA-SYSYMD  <=  SYS-5001-EDYUKYMD )
                   MOVE    IDX-LST TO  SPA-GMN-W26-YUKOYMDLST-SEL
                                       SPA-GMN-W26-SELNUM
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4100-YUKOYMDLST-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           INITIALIZE                  SYSKANRI-REC
      *
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W26-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    WRK-STYUKYMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "0004"      TO  SPA-ERRCD
               GO  TO  4100-INPUT-AREA-EDIT-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           MOVE    SYS-5001-STYUKYMD   TO  WRK-KSNKJNYMD
           PERFORM 42011-KSN-SYOKI-SEC
      *
      *    病棟種別
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5101"              TO  SYS-KANRICD
           MOVE    SYS-5001-BTU-SBT    TO  SYS-KBNCD
           MOVE    WRK-STYUKYMD        TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5101-REC
           MOVE    SYS-5101-BTU-SBT-NM TO  SPA-GMN-W26-SBT
      *
           MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  SPA-NAI-W26-KHNSRYCD
      *
      *    入院料
           IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                       =   SPACE )
      *        点数マスタを検索し、入院基本料名を設定する
               MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  WRK-TNS-SRYCD
               MOVE    WRK-STYUKYMD        TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME            TO  SPA-GMN-W26-KHNRYO
               MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SPA-NAI-W26-KHNRYO
               MOVE    TNS-NYUKHNKSNKBN    TO  SPA-NAI-W26-KSNKBN
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    "5110"              TO  SYS-KANRICD
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                           TO  SYS-KBNCD
               MOVE    WRK-STYUKYMD        TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5110-REC
               MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                           TO  SPA-GMN-W26-KHNRYO
               MOVE    SYS-5110-SRYCD      TO  SPA-NAI-W26-KHNRYO
               MOVE    SYS-5110-SRYCD      TO  WRK-TNS-SRYCD
               MOVE    WRK-STYUKYMD        TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN    TO  SPA-NAI-W26-KSNKBN
           END-IF
      *
      *    有効期間開始日
           MOVE    SYS-5001-STYUKYMD
                                   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W26-STYUKYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-W26-STYUKYMD  
      *
      *
      *    有効期間終了日
           MOVE    SYS-5001-EDYUKYMD
                                   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
      *
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W26-EDYUKYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-W26-EDYUKYMD
      *
           PERFORM VARYING IDX-KSN    FROM    1   BY  1
                   UNTIL ( IDX-KSN    >   CONST-KSN-MAX )
      *
               MOVE    "F"     TO  SPA-GMN-W26-CHK-KSN (IDX-KSN)
      *
               EVALUATE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   PERFORM VARYING IDX-SYS FROM    1   BY  1
                           UNTIL ( IDX-SYS >   CONST-SYSKSN-MAX )
      *
                       IF    ( SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           =   SYS-5001-KSNCD (IDX-SYS))
                           IF    ( SYS-5001-KSNKBN (IDX-SYS)
                                           =   "2" )
                               MOVE    "T"
                                   TO   SPA-GMN-W26-CHK-KSN (IDX-KSN)
                           END-IF
                       END-IF
      *
      *
                   END-PERFORM
      *
               END-EVALUATE
      *
           END-PERFORM
      *
           .
      *
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       4100-SSTKIJUN-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-W26-TSTYUKYMD   (SPA-GMN-W26-SELNUM)
                                   TO  WRK-STYUKYMD
      *
      *    施設基準検索処理
           PERFORM 4100-SYS-1006-SEL-SEC
      *
           MOVE    1               TO  WRK-BASE-CUR
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN         >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "T" )
                   PERFORM 41011-SSTKIJUN-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       4100-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
      *
           EVALUATE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
           WHEN    "190101770"
      *        難病患者等入院診療加算
               CONTINUE
           WHEN    "190101970"
      *        特殊疾患入院施設管理加算
               IF    ( SSTKJN-CD (009) =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190110170"
      *        児童・思春期精神科入院医療管理加算
               IF    ( SSTKJN-CD (225) =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106970"
      *        精神科応急入院施設管理加算
               IF    ( SSTKJN-CD (031) =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190102070"
           WHEN    "190102170"
           WHEN    "190102270"
           WHEN    "190102370"
           WHEN    "190102470"
      *        看護配置加算（入院基本料３）
      *        看護配置加算（入院基本料４）
      *        看護配置加算（入院基本料５）
      *        看護配置加算（入院基本料６）
      *        看護配置加算（入院基本料７）
      *
               CONTINUE
           WHEN    "190109670"
      *        看護配置適合加算
               IF    ( SSTKJN-CD (399) =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190117070"
      *        複数医師及び看護配置適合加算
               IF    ( SSTKJN-CD (397) =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
      *
           END-EVALUATE
      *
           IF    ( FLG-SSTKIJUNERR     =   1 )
               COMPUTE SPA-GMN-W26-CUR
                   =   WRK-BASE-CUR    +   IDX-KSN
               MOVE    "0003"  TO  SPA-ERRCD
               MOVE    "F"     TO  SPA-GMN-W26-CHK-KSN (IDX-KSN)
           ELSE
               IF    ( SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                           =   "190109670"
                                            OR "190117070" )
                   IF    ( SPA-NAI-W26-KHNSRYCD
                                       NOT =   "099999950" )
                       MOVE    "0005"  TO  SPA-ERRCD
                       MOVE    "F"     TO  SPA-GMN-W26-CHK-KSN (IDX-KSN)
                   END-IF
               ELSE
                   MOVE    SPA-NAI-W26-KSNKBN
                               TO  WRK-BTU-NYUKSNKBN
                   MOVE    SPA-NAI-W26-CHK-KSN-KSNKBN (IDX-KSN)
                               TO  WRK-KSN-NYUKSNKBN
                   PERFORM 900-NYUKSNCHK-KEY-SEL-SEC
                   IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       COMPUTE SPA-GMN-W26-CUR
                           =   WRK-BASE-CUR    +   IDX-KSN
                       MOVE    "0005"  TO  SPA-ERRCD
                       MOVE    "F"     TO  SPA-GMN-W26-CHK-KSN (IDX-KSN)
                   END-IF
               END-IF
           END-IF
      *
           .
       41011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準検索処理
      *****************************************************************
       4100-SYS-1006-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  SSTKJN-AREA
      *
      *    施設基準情報検索
           INITIALIZE  SYSKANRI-REC
           MOVE    "1006"          TO  SYS-KANRICD
           MOVE    WRK-STYUKYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1006-REC
      *
               EVALUATE    SYS-1006-KBNCD
               WHEN    "1"
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL1
               WHEN    "2"
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL2
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-SYS-1006-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W26-INPUT-AREA
                       SPA-NAI-W26-INPUT-AREA
      *
           MOVE    SPA-SYSYMD      TO  WRK-KSNKJNYMD
           PERFORM 42011-KSN-SYOKI-SEC
      *
           MOVE    ALL  "F"    TO  SPA-GMN-W26-CHK-KSN-G
      *
           MOVE    2           TO  SPA-GMN-W26-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    加算情報初期化処理
      *****************************************************************
       42011-KSN-SYOKI-SEC             SECTION.
      *
           INITIALIZE      SPA-GMN-W26-KSNNAME-G
                           SPA-NAI-W26-CHK-KSN-G
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( WRK-KSNKJNYMD   >=   CONST-H180401 )
                   MOVE    CONST-KSN-H1804 (IDX-KSN)
                                       TO  SPA-NAI-W26-CHK-KSN-SRYCD
                                                           (IDX-KSN)
               ELSE
                   MOVE    CONST-KSN-SRYCD (IDX-KSN)
                                       TO  SPA-NAI-W26-CHK-KSN-SRYCD
                                                           (IDX-KSN)
               END-IF
      *
               IF    ( SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                       NOT =   SPACE )
                   MOVE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  WRK-TNS-SRYCD
                   MOVE    WRK-KSNKJNYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME    TO  SPA-GMN-W26-KSNNAME (IDX-KSN)
                   MOVE    TNS-NYUKHNKSNKBN
                                       TO  SPA-NAI-W26-CHK-KSN-KSNKBN
                                                               (IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
      *
       42011-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号コンボチェック処理処理
      *****************************************************************
       4200-BTUNUMLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W26-BTUNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W26-BTUNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W26-BTUNUMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-BTUNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           INITIALIZE       FLG-KSNCD-G
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( SPA-GMN-W26-CHK-KSN (IDX-KSN)  =   "T" )
                   EVALUATE    SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                   WHEN    "190101770"
                       MOVE    1       TO  FLG-190101770
                   WHEN    "190101970"
                       MOVE    1       TO  FLG-190101970
                   WHEN    "190110170"
                       MOVE    1       TO  FLG-190110170
                   WHEN    "190106970"
                       MOVE    1       TO  FLG-190106970
                   WHEN    "190102070"
                       MOVE    1       TO  FLG-190102070
                   WHEN    "190102170"
                       MOVE    1       TO  FLG-190102170
                   WHEN    "190102270"
                       MOVE    1       TO  FLG-190102270
                   WHEN    "190102370"
                       MOVE    1       TO  FLG-190102370
                   WHEN    "190102470"
                       MOVE    1       TO  FLG-190102470
                   WHEN    "190109670"
                       MOVE    1       TO  FLG-190109670
                   WHEN    "190117070"
                       MOVE    1       TO  FLG-190117070
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
      *    難病患者等特別入院診療加算と特殊疾患入院施設管理加算
      *    が同時に算定されていないこと
           IF    ( FLG-190101770       =   1 )
            AND  ( FLG-190101970       =   1 )
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    看護配置加算の１２３４５のいずれかのみ
      *    選択されていること
           INITIALIZE  WRK-SEL-CNT
           IF    ( FLG-190102070       =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
           IF    ( FLG-190102170       =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
           IF    ( FLG-190102270       =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
           IF    ( FLG-190102370       =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
           IF    ( FLG-190102470       =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
      *
           IF    ( WRK-SEL-CNT     >   1 )
               MOVE    "1003"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W26    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                          W26
      *
      *    有効開始日
           MOVE    SPA-GMN-W26-STYUKYMD    TO  W26-STYUKYMD
      *    有効終了日
           MOVE    SPA-GMN-W26-EDYUKYMD    TO  W26-EDYUKYMD
      *    病棟名称
           MOVE    SPA-GMN-W26-BTUNUM-G    TO  W26-BTU-NAME
           MOVE    SPA-GMN-W26-BTUNUM-MAX  TO  W26-BTU-NAME-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W26-BTUNUM-MAX )
               MOVE    SPA-GMN-W26-BTUNUMLST  (IDX1)
                                           TO  W26-BTU-NAME-ITM (IDX1)
           END-PERFORM
      *
      *    病棟種別
           MOVE     SPA-GMN-W26-SBT        TO  W26-BTU-SBT
      *    入院基本料
           MOVE     SPA-GMN-W26-KHNRYO     TO  W26-BTU-KHNRYO
      *
      *    入院料加算
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-KSN-MAX )
               MOVE    SPA-GMN-W26-KSNNAME (IDX1)
                                           TO  W26-CHK-KSN-LBL (IDX1)
               MOVE    SPA-GMN-W26-CHK-KSN (IDX1)
                                           TO  W26-CHK-KSN (IDX1)
           END-PERFORM
      *
      *    選択番号
           MOVE    SPA-GMN-W26-SELNUM      TO  W26-SELNUM
      *
      *    有効期間リスト
           MOVE    SPA-GMN-W26-YUKOYMDLST-MAX
                                           TO  W26-YUKOYMDLST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W26-YUKOYMDLST-MAX )
      *
      *        選択番号
               MOVE    SPA-GMN-W26-TSELNUM (IDX1)
                                   TO  W26-TSELNUM (IDX1)
      *        有効期間開始日
               MOVE    SPA-GMN-W26-TSTYUKYMD (IDX1)
                                   TO  W26-TSTYUKYMD (IDX1)
      *        有効期間終了日
               MOVE    SPA-GMN-W26-TEDYUKYMD (IDX1)
                                   TO  W26-TEDYUKYMD (IDX1)
      *
               IF    ( IDX1    =   SPA-GMN-W26-YUKOYMDLST-SEL )
                   MOVE    "T"     TO  W26-YUKOYMDLST-SELECT (IDX1)
               ELSE
                   MOVE    "F"     TO  W26-YUKOYMDLST-SELECT (IDX1)
               END-IF
      *
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
           IF    ( SPA-NAI-W26-EDYUKYMD   =    SPACE )
            OR   ( SPA-NAI-W26-EDYUKYMD  >=    SPA-SYSYMD )
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                       UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                   MOVE    WIDGET-NORMAL
                                   TO  W26-CHK-KSN-STATE (IDX-KSN)
               END-PERFORM
           ELSE
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                       UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                   MOVE    WIDGET-INSENSITIVE
                                   TO  W26-CHK-KSN-STATE (IDX-KSN)
               END-PERFORM
           END-IF
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W26-CHK-KSN-SRYCD (IDX-KSN)
                                   =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                   TO  W26-CHK-KSN-STATE (IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-W26-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W26-CUR
           WHEN    001
               MOVE    "BTU_NAME"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "CHK_KSN01"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "CHK_KSN02"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "CHK_KSN03"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "CHK_KSN04"     TO  MCP-WIDGET
           WHEN    006
               MOVE    "CHK_KSN05"     TO  MCP-WIDGET
           WHEN    007
               MOVE    "CHK_KSN06"     TO  MCP-WIDGET
           WHEN    008
               MOVE    "CHK_KSN07"     TO  MCP-WIDGET
           WHEN    009
               MOVE    "CHK_KSN08"     TO  MCP-WIDGET
           WHEN    010
               MOVE    "CHK_KSN09"     TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-NAI-W26-EDYUKYMD  >=   SPA-SYSYMD )
               CONTINUE
           ELSE
               MOVE    100             TO  SPA-GMN-W26-CUR
           END-IF
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               COMPUTE WRK-BASE-CUR    =   IDX-KSN +   1
      *
               IF    ( SPA-GMN-W26-CUR =   WRK-BASE-CUR )
                   IF    ( W26-CHK-KSN-STATE (IDX-KSN)
                                       =   WIDGET-INSENSITIVE )
                       IF    ( IDX-KSN =   CONST-KSN-MAX )
                           COMPUTE SPA-GMN-W26-CUR
                               =   212
                       ELSE
                           COMPUTE SPA-GMN-W26-CUR
                               =   SPA-GMN-W26-CUR +   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BTU_NAME"
               MOVE    002             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN01"
               MOVE    003             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN02"
               MOVE    004             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN03"
               MOVE    005             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN04"
               MOVE    006             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN05"
               MOVE    007             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN06"
               MOVE    008             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN07"
               MOVE    009             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN08"
               MOVE    010             TO  SPA-GMN-W26-CUR
           WHEN    "CHK_KSN09"
               MOVE    212             TO  SPA-GMN-W26-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
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
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "病棟名称入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "加算に必要な施設基準が設定されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "病棟加算情報の取得に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "当該入院料において、この加算は算定できません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "難病患者等特別入院と特殊疾患入院は同時に"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "加算できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1002"
               MOVE    "療養病棟療養環境加算はいずれか１つのみ選"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "択してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1003"
               MOVE    "看護配置加算はいずれか１つのみ選択してく"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "ださい"        TO  SPA-ERRMSG (41 : )
           WHEN    "1201"
           WHEN    "1202"
               MOVE    "更新できませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W26"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-WIDCD
           WHEN    "3001"
               MOVE    "登録処理をします"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W26"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
               GO  TO  800-SEIWA-HEN-EXT
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
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W26-BTUNUM  TO  SYS-5001-KBNCD
           MOVE    "%"                 TO  SYS-5001-KBNCD (3:1)
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *    点数マスタ検索
           INITIALIZE  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD      TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算チェックテーブル検索処理(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-BTU-NYUKSNKBN   TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSN-NYUKSNKBN   TO  NYUKSNCHK-KSNKBN
           MOVE    WRK-STYUKYMD        TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUKSNCHK-REC
           ELSE
               INITIALIZE                  NYUKSNCHK-REC
               MOVE    1               TO  FLG-NYUKSNCHK
           END-IF
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "MONFUNC"     USING
                                MCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

