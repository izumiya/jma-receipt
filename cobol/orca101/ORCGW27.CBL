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
       PROGRAM-ID.                               ORCGW27.
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
      *  00.00.01    NACL-太田    03/05/01  更新時に処理がループする
      *                                     不具合を修正
      *  00.00.02    NACL-太田    03/06/10  重症者等療養環境特別加算、
      *                                     無菌治療室管理加算を追加
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
           03  FLG-SYS5002                         PIC 9(01).
           03  FLG-KANJI                           PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-YUKO                            PIC 9(01).
           03  FLG-TOKNYUIN-CD                     PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-SYS5001                         PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-KSNCD-G.
               05  FLG-190109770                   PIC 9(01).
               05  FLG-190106370                   PIC 9(01).
               05  FLG-190106470                   PIC 9(01).
               05  FLG-190106670                   PIC 9(01).
               05  FLG-190105570                   PIC 9(01).
               05  FLG-190120210                   PIC 9(01).
               05  FLG-190106070                   PIC 9(01).
               05  FLG-190106170                   PIC 9(01).
               05  FLG-190106270                   PIC 9(01).
               05  FLG-190105870                   PIC 9(01).
               05  FLG-190105970                   PIC 9(01).
               05  FLG-190106570                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDX7                                PIC 9(05).
           03  IDXI                                PIC 9(05).
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
               05  WRK-CMB-ITM2                    PIC X(100).
               05  WRK-CMB-ITM3                    PIC X(100).
               05  WRK-CMB-ITM4                    PIC X(100).
           03  WRK-KANRICD                         PIC X(04).
           03  WRK-KBNCD                           PIC X(08).
           03  WRK-IDX                             PIC 9(05).
           03  WRK-KJNYMD                          PIC X(08).
           03  WRK-KJNYMD-BAK                      PIC X(08).
           03  WRK-DSPSELNUM-G.
               05 WRK-DSPMARK                      PIC X.
               05 WRK-DSPSELNUM                    PIC ZZ9.
           03  WRK-NUM-EDIT.
               05  WRK-99                  PIC 9(02).
               05  WRK-99999               PIC 9(05).
               05  WRK-999999              PIC 9(06).
               05  WRK-ZZ9                 PIC ZZ9.
               05  WRK-ZZZ9                PIC ZZZ9.
               05  WRK-ZZZZ9               PIC ZZZZ9.
               05  WRK-ZZZZZ9              PIC ZZZZZ9.
               05  WRK-ZZZZZZZZZ9          PIC ZZZZZZZZZ9.
           03  WRK-STSELNUM                PIC 9(05).
           03  WRK-EDSELNUM                PIC 9(03).
           03  WRK-EDBRMNUM                PIC X(06).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-TOKNYUIN-CD             PIC X(09).
           03  WRK-NYUKSNKBN               PIC X(03).
           03  WRK-BTU-NYUKSNKBN           PIC X(03).
           03  WRK-BRM-NYUKSNKBN           PIC X(03).
           03  WRK-KSN-NYUKSNKBN           PIC X(03).
           03  WRK-BTU-NAME                PIC X(20).
           03  WRK-SELNUM                  PIC 9(03).
           03  WRK-SRYCD                   PIC X(09).
           03  WRK-SEL-CNT                 PIC 9(05).
           03  WRK-SEL-IDX                 PIC 9(05).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-KSNKJNYMD               PIC X(08).
           03  WRK-BASE-CUR                PIC 9(05).
      *    入力項目名
           03  WRK-MCP-WIDGET              PIC X(64).
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
           03  CONST-H180401               PIC X(08) VALUE "20060401".
           03  CONST-H180701               PIC X(08) VALUE "20060701".
           03  CONST-KSN-MAX               PIC 9(03) VALUE  15.
           03  CONST-SYSKSN-MAX            PIC 9(03) VALUE  12.
           03  CONST-BTUNAME-MAX           PIC 9(03) VALUE  50.
           03  CONST-KSN-AREA.
               05  CONST-KSN-H1804-G.
                   07  CONST-KSN-H1804-VAL.
                       09                  PIC X(09) VALUE "190109770".
                       09                  PIC X(09) VALUE "190106370".
                       09                  PIC X(09) VALUE "190106470".
                       09                  PIC X(09) VALUE "190106670".
                       09                  PIC X(09) VALUE "190105570".
                       09                  PIC X(09) VALUE "190120210".
                       09                  PIC X(09) VALUE "190106070".
                       09                  PIC X(09) VALUE "190106170".
                       09                  PIC X(09) VALUE "190106270".
                       09                  PIC X(09) VALUE "190105870".
                       09                  PIC X(09) VALUE "190105970".
                       09                  PIC X(09) VALUE "190106570".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                   07  CONST-KSN-H1804-R
                                       REDEFINES   CONST-KSN-H1804-VAL.
                       09  CONST-KSN-H1804 PIC X(09) OCCURS 15.
               05  CONST-KSN-G.
                   07  CONST-KSN-VAL.
                       09                  PIC X(09) VALUE "190109770".
                       09                  PIC X(09) VALUE "190106370".
                       09                  PIC X(09) VALUE "190106470".
                       09                  PIC X(09) VALUE "190106670".
                       09                  PIC X(09) VALUE "190105570".
                       09                  PIC X(09) VALUE "190106070".
                       09                  PIC X(09) VALUE "190106170".
                       09                  PIC X(09) VALUE "190106270".
                       09                  PIC X(09) VALUE "190105870".
                       09                  PIC X(09) VALUE "190105970".
                       09                  PIC X(09) VALUE "190106570".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                       09                  PIC X(09) VALUE "         ".
                   07  CONST-KSN-R     REDEFINES   CONST-KSN-VAL.
                       09  CONST-KSN-SRYCD PIC X(09) OCCURS 15.
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
      *    病室情報
           COPY  "CPSK5002.INC".
      *    病棟種別
           COPY  "CPSK5101.INC".
      *    病室種別
           COPY  "CPSK5108.INC".
      *    病棟特定入院料
           COPY  "CPSK5110.INC".
      *    特定入院料（病室）
           COPY  "CPSK5111.INC".
      *    室料差額
           COPY  "CPSK5113.INC".
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
           COPY    "CPORCMCP.INC".
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
           COPY    "W31.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
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
           MOVE    "W27    "           TO  MCP-WINDOW.
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
           INITIALIZE                  SPA-W27
                                           W27
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W27-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W27-KJNYMD
      *
      *    画面初期化処理
           PERFORM 420-CLEAR-SEC
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
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
           INITIALIZE              SPA-GMN-W27-CMB-AREA
                                   SPA-NAI-W27-CMB-AREA
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
           MOVE    ZERO            TO  FLG-SYS5001
      *
      *    病棟情報検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W27-KJNYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    1           TO  FLG-SYS5001
           END-IF
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYS5001 NOT =   ZERO )
                     OR  ( IDX-CMB     >   CONST-BTUNAME-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB     TO  SPA-GMN-W27-BTUNUM-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *        病棟名
               MOVE    SYS-5001-KBNCD  TO  SPA-GMN-W27-BTUNUML
                                                       (IDX-CMB)
               MOVE    SYS-5001-BTU-NAME
                                       TO  SPA-GMN-W27-BTUNUMMEIL
                                                       (IDX-CMB)
               MOVE    SYS-5001-BTU-SBT
                                       TO  SPA-NAI-W27-SBTL
                                                       (IDX-CMB)
               MOVE    SYS-5001-STYUKYMD
                                       TO  SPA-NAI-W27-STYUKYMDL
                                                       (IDX-CMB)
      *
      *        入院料
               IF    ( SYS-5001-BTU-TOKTEI-NYUIN
                                       =   SPACE )
                   MOVE    SYS-5001-BTU-KHNSRYCD
                                           TO  SPA-NAI-W27-KIHONRYOL
                                                       (IDX-CMB)
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "5110"          TO  SYS-KANRICD
                   MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-W27-KJNYMD
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-5110-REC
                   MOVE    SYS-5110-SRYCD  TO  SPA-NAI-W27-KIHONRYOL
                                                       (IDX-CMB)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               IF    ( FLG-SYSKANRI    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYS5001
               END-IF
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
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    100                 TO  SPA-GMN-W27-CUR
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM 480-TOUROKU-SEC
      *    入院加算情報
           WHEN    "CLICKED"       ALSO    "CHK_KSN01"
                                   THRU    "CHK_KSN15"
      *
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
      *    基準日
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *
      *    開始選択番号
           WHEN    "ACTIVATE"      ALSO    "STSELNUM"
               PERFORM 4100-STSELNUM-CHK-SEC
      *
      *    病室リスト
           WHEN    ANY             ALSO    "BRMLST"
               PERFORM 4100-BRMLST-CHK-SEC
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
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-W27-CUR
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
      *    終了選択番号
           MOVE    W27-EDSELNUM    TO  SPA-GMN-W27-EDSELNUM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-KSN-MAX )
      *
               MOVE    W27-CHK-KSN (IDX0)
                                   TO  SPA-GMN-W27-CHK-KSN (IDX0)
      *
           END-PERFORM
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
           MOVE    SPACE           TO  WRK-TOKNYUIN-CD
      *
      *    病棟名称チェック処理
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    開始選択番号
           PERFORM 4101-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4102-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    終了選択番号
           PERFORM 4100-EDSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           IF    ( SPA-NAI-W27-STYUKYMD  >= CONST-H180701 )
            AND  ( SPA-NAI-W27-SBT        =    "04" OR "09" )
               CONTINUE
           ELSE
      *        施設基準チェック処理
               PERFORM 4100-SSTKIJUN-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
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
           MOVE    "W27"               TO  SPA-MOTOPG
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
           INITIALIZE  SPA-GMN-W27-HEAD-AREA
                       SPA-NAI-W27-HEAD-AREA
                       SPA-GMN-W27-BRMLST-AREA
                       SPA-NAI-W27-BRMLST-AREA
                       SPA-GMN-W27-INPUT-AREA
                       SPA-NAI-W27-INPUT-AREA
      *
           MOVE    SPA-NAI-W27-KJNYMD
                               TO   SPA-NAI-W27-BRMSTYUKYMD
      *
           MOVE    ALL  "F"    TO  SPA-GMN-W27-CHK-KSN-G
           MOVE    SPA-NAI-W27-KJNYMD  TO  WRK-KSNKJNYMD
           PERFORM 42011-KSN-SYOKI-SEC
      *
           MOVE    1               TO  SPA-GMN-W27-CUR
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
           MOVE    SPA-GMN-W27-STSELNUM
                                   TO  WRK-STIDX
           IF    ( SPA-GMN-W27-EDSELNUM
                                   =   ZERO )
               MOVE    SPA-GMN-W27-STSELNUM
                                   TO  WRK-EDIDX
           ELSE
               MOVE    SPA-GMN-W27-EDSELNUM
                                   TO  WRK-EDIDX
           END-IF
      *
           PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                   UNTIL ( IDX1    >       WRK-EDIDX )
      *
               IF    ( SPA-GMN-W27-TYUKO-FLG (IDX1)
                                   =       1 )
      *
      *            更新対象のデータが存在するかチェック
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "5002"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-W27-TBRMNUM (IDX1)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-W27-TSTYUKYMD (IDX1)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    NOT =   ZERO )
                       MOVE    "1201"      TO  SPA-ERRCD
                       GO  TO  481-TOUROKU-EXT
                   END-IF
      *
                   MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
                   PERFORM 4811-BRM-KSN-EDIT-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYS-5002-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5002-UPHMS
      *
                   MOVE    SYS-5002-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "1201"      TO  SPA-ERRCD
                       GO  TO  481-TOUROKU-EXT
                   END-IF
               END-IF
           END-PERFORM
      *
      *    画面処理化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           .
       481-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    病室加算編集処理
      *****************************************************************
       4811-BRM-KSN-EDIT-SEC           SECTION.
      *
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-KSN-MAX )
      *
               EVALUATE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
               WHEN    "190109770"
      *            新生児入院医療管理加算
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5002-SINSEIJI-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-SINSEIJI-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5002-SINSEIJI-KBN
                   END-IF
               WHEN    "190106370"
      *            診療所療養病床療養環境加算１
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5002-SNR-RYOYO-CD (1)
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-SNR-RYOYO-KBN(1)
                   ELSE
                       MOVE    "2"         TO  SYS-5002-SNR-RYOYO-KBN(1)
                   END-IF
               WHEN    "190106470"
      *            診療所療養病床療養環境加算２
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5002-SNR-RYOYO-CD (2)
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-SNR-RYOYO-KBN(2)
                   ELSE
                       MOVE    "2"         TO  SYS-5002-SNR-RYOYO-KBN(2)
                   END-IF
               WHEN    "190106670"
      *            放射線治療病室管理加算
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5002-HOSYASEN-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-HOSYASEN-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5002-HOSYASEN-KBN
                   END-IF
               WHEN    "190105570"
      *            療養環境加算
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           TO  SYS-5002-RYOYO-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-RYOYO-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5002-RYOYO-KBN
                   END-IF
               WHEN    "190120210"
      *            療養病棟療養環境加算１
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-BTU-RYOYO-H18-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-BTU-RYOYO-H18-KBN
                   ELSE
                       MOVE    "2"     TO  SYS-5002-BTU-RYOYO-H18-KBN
                   END-IF
               WHEN    "190106070"
      *            療養病棟療養環境加算２
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-BTU-RYOYO-CD (1)
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-BTU-RYOYO-KBN (1)
                   ELSE
                       MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (1)
                   END-IF
               WHEN    "190106170"
      *            療養病棟療養環境加算３
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-BTU-RYOYO-CD (2)
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-BTU-RYOYO-KBN (2)
                   ELSE
                       MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (2)
                   END-IF
               WHEN    "190106270"
      *            療養病棟療養環境加算４
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-BTU-RYOYO-CD (3)
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-BTU-RYOYO-KBN (3)
                   ELSE
                       MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (3)
                   END-IF
               WHEN    "190105870"
      *            重症者等療養環境特別加算（個室）
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-JYUTOK1-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-JYUTOK1-KBN
                   ELSE
                       MOVE    "2"     TO  SYS-5002-JYUTOK1-KBN
                   END-IF
               WHEN    "190105970"
      *            重症者等療養環境特別加算（２人部屋）
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-JYUTOK2-CD
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"     TO  SYS-5002-JYUTOK2-KBN
                   ELSE
                       MOVE    "2"     TO  SYS-5002-JYUTOK2-KBN
                   END-IF
               WHEN    "190106570"
      *            無菌治療室管理加算
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  SYS-5002-MUKIN-KBN
                   IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "F" )
                       MOVE    "1"         TO  SYS-5002-MUKIN-KBN
                   ELSE
                       MOVE    "2"         TO  SYS-5002-MUKIN-KBN
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
      *
           .
      *
       4811-BRM-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称チェック処理
      *****************************************************************
       4100-BTUNUM-CHK-SEC           SECTION.
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    W27-BTU-NAME    TO  SPA-GMN-W27-BTUNUM-G
      *
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-BTUNUM-CHK-EXT
           END-IF
      *
      *    病室リスト編集処理
           PERFORM 4100-BRMLST-EDIT-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-W27-CUR
      *
           .
      *
       4100-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称チェック処理
      *****************************************************************
       4101-BTUNUM-CHK-SEC           SECTION.
      *
           MOVE    SPA-GMN-W27-BTUNUM  TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           IF   (  WRK-CMB-ITM     =   SPACE )
               MOVE    1           TO  SPA-GMN-W27-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W27-BTUNUM-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W27-SBT
               MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W27-KIHONRYO
               MOVE    WRK-CMB-ITM4    TO  SPA-NAI-W27-STYUKYMD
           END-IF
      *
           .
      *
       4101-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-W27-KJNYMD  TO  WRK-KJNYMD-BAK
      *
           DISPLAY "WRK-KJNYMD-BAK =" WRK-KJNYMD-BAK
      *
           MOVE    SPACE               TO  SPA-NAI-W27-KJNYMD
      *
           MOVE    W27-KJNYMD          TO  SPA-GMN-W27-KJNYMD
      *
           IF    ( SPA-GMN-W27-KJNYMD  =   SPACE )
               MOVE    SPA-SYSYMD      TO  SPA-NAI-W27-KJNYMD
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W27-KJNYMD
               MOVE    ZERO            TO  SPA-GMN-W27-CUR
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 310-CMB-SYOKI-SEC
               MOVE    SPA-GMN-W27-BTUNUM  TO  WRK-CMB-CD
               PERFORM 4200-BTUNUMLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W27-BTUNUM-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W27-SBT
               MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W27-KIHONRYO
               MOVE    WRK-CMB-ITM4    TO  SPA-NAI-W27-STYUKYMD
               PERFORM 4100-BRMLST-EDIT-SEC
               GO  TO  4100-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W27-KJNYMD  TO   WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR   NOT =   ZERO )
               MOVE    WRK-KJNYMD-BAK  TO   WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-W27-KJNYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-W27-KJNYMD
               MOVE    100             TO  SPA-GMN-W27-CUR
               GO  TO  4100-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W27-KJNYMD
           MOVE    WRK-SYMD            TO  SPA-NAI-W27-KJNYMD
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 310-CMB-SYOKI-SEC
           MOVE    SPA-GMN-W27-BTUNUM  TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W27-BTUNUM-G
           MOVE    WRK-CMB-ITM2        TO  SPA-NAI-W27-SBT
           MOVE    WRK-CMB-ITM3        TO  SPA-NAI-W27-KIHONRYO
           MOVE    WRK-CMB-ITM4        TO  SPA-NAI-W27-STYUKYMD
           PERFORM 4100-BRMLST-EDIT-SEC
      *
           MOVE    ZERO                TO   SPA-GMN-W27-CUR
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始選択番号チェック処理
      *****************************************************************
       4100-STSELNUM-CHK-SEC       SECTION.
      *
           MOVE   SPACE        TO  SPA-GMN-W27-STBRMNUM
           MOVE   SPACE        TO  SPA-GMN-W27-STBRMNUM2
           MOVE   ZERO         TO  SPA-NAI-W27-STSELNUM
                                   SPA-NAI-W27-EDSELNUM
      *
           MOVE   W27-STSELNUM TO  SPA-GMN-W27-STSELNUM
      *
           PERFORM 4101-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-STSELNUM-CHK-EXT
           END-IF
      *
      *    入力領域編集処理
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           MOVE    SPA-GMN-W27-STSELNUM
                                   TO  SPA-GMN-W27-BRMLST-SEL
      *
           PERFORM 4102-STSELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-STSELNUM-CHK-EXT
           END-IF
      *
           MOVE    ZERO        TO   SPA-GMN-W27-CUR
      *
           .
       4100-STSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始選択番号チェック処理
      *****************************************************************
       4101-STSELNUM-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W27-STSELNUM    =   ZERO )
               MOVE    2      TO   SPA-GMN-W27-CUR
               MOVE    "1001" TO   SPA-ERRCD
               GO  TO  4101-STSELNUM-CHK-EXT
           END-IF
      *
      *    範囲内の選択番号が入力されていること
           IF    ( SPA-GMN-W27-STSELNUM   <=  SPA-GMN-W27-BRMLST-MAX )
               CONTINUE
           ELSE
               MOVE    2      TO   SPA-GMN-W27-CUR
               MOVE    "0006"      TO  SPA-ERRCD
               GO  TO  4101-STSELNUM-CHK-EXT
           END-IF
      *
           .
       4101-STSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始選択番号チェック処理
      *****************************************************************
       4102-STSELNUM-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W27-TYUKO-FLG (SPA-GMN-W27-STSELNUM)
                                           NOT =  1 )
               MOVE   2            TO   SPA-GMN-W27-CUR
               MOVE    "0009"      TO  SPA-ERRCD
           END-IF
      *
           .
       4102-STSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了選択番号チェック処理
      *****************************************************************
       4100-EDSELNUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-W27-EDBRMNUM
           MOVE    ZERO        TO  SPA-NAI-W27-STSELNUM
                                   SPA-NAI-W27-EDSELNUM
      *
           IF    ( SPA-GMN-W27-EDSELNUM    =   ZERO )
               MOVE    SPA-NAI-W27-TTOKNYUIN (SPA-GMN-W27-STSELNUM)
                              TO   WRK-TOKNYUIN-CD
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
      *    範囲内の選択番号が入力されていること
           IF    ( SPA-GMN-W27-EDSELNUM   <=  SPA-GMN-W27-BRMLST-MAX )
               CONTINUE
           ELSE
               MOVE    "0006"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-W27-CUR
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
      *    終了選択番号が入力されてる場合、
      *    開始選択番号≦終了選択番号であること
           IF    ( SPA-GMN-W27-STSELNUM
                                       <=  SPA-GMN-W27-EDSELNUM )
               CONTINUE
           ELSE
               MOVE    "1003"  TO  SPA-ERRCD
               MOVE    3       TO  SPA-GMN-W27-CUR
               GO  TO  4100-EDSELNUM-CHK-EXT
           END-IF
      *
           MOVE   SPA-GMN-W27-EDSELNUM
                                   TO  IDX1
           MOVE   SPA-GMN-W27-TBRMNUM2 (IDX1)
                                   TO  SPA-GMN-W27-EDBRMNUM
      *
           MOVE    ZERO            TO  FLG-TOKNYUIN-CD
           PERFORM VARYING IDX2    FROM    SPA-GMN-W27-STSELNUM    BY  1
                   UNTIL ( IDX2    >       SPA-GMN-W27-EDSELNUM )
      *
               IF    (SPA-GMN-W27-TYUKO-FLG (IDX2) =   "1" )
                   IF    ( FLG-TOKNYUIN-CD =   ZERO )
                       MOVE    1   TO  FLG-TOKNYUIN-CD
                       MOVE    SPA-NAI-W27-TTOKNYUIN (IDX2)
                                   TO  WRK-TOKNYUIN-CD
                   ELSE
                       IF    ( WRK-TOKNYUIN-CD
                               NOT =   SPA-NAI-W27-TTOKNYUIN (IDX2) )
                           MOVE    "0007"  TO  SPA-ERRCD
                           MOVE    3       TO  SPA-GMN-W27-CUR
                           GO  TO  4100-EDSELNUM-CHK-EXT
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-W27-STSELNUM    TO  SPA-NAI-W27-STSELNUM
           MOVE    SPA-GMN-W27-EDSELNUM    TO  SPA-NAI-W27-EDSELNUM
      *
           .
       4100-EDSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC        SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    SPA-GMN-W27-TBRMNUM  (SPA-GMN-W27-STSELNUM)
                                       TO  SYS-KBNCD
           MOVE    SPA-NAI-W27-TSTYUKYMD  (SPA-GMN-W27-STSELNUM)
                                       TO  SPA-NAI-W27-BRMSTYUKYMD
           MOVE    SPA-NAI-W27-BRMSTYUKYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "0004"      TO  SPA-ERRCD
               GO  TO  4100-INPUT-AREA-EDIT-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           MOVE    SPA-GMN-W27-STSELNUM
                                   TO  WRK-STSELNUM
      *
           PERFORM 4201-INPUT-AREA-CLEAR2-SEC
      *
           MOVE    WRK-STSELNUM    TO  IDXI
      *
      *    開始選択番号
           MOVE    WRK-STSELNUM    TO  SPA-GMN-W27-STSELNUM
      *
      *    開始病室番号
           MOVE    SPA-GMN-W27-TBRMNUM  (IDXI)
                                   TO  SPA-GMN-W27-STBRMNUM
           MOVE    SPA-GMN-W27-TBRMNUM2 (IDXI)
                                   TO  SPA-GMN-W27-STBRMNUM2
      *
      *
           PERFORM VARYING IDX-KSN    FROM    1   BY  1
                   UNTIL ( IDX-KSN    >   CONST-KSN-MAX )
      *
               MOVE    "F"     TO  SPA-GMN-W27-CHK-KSN (IDX-KSN)
      *
               EVALUATE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   PERFORM VARYING IDX-SYS FROM    1   BY  1
                           UNTIL ( IDX-SYS >   CONST-SYSKSN-MAX )
      *
                       IF    ( SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                           =   SYS-5002-KSNCD (IDX-SYS))
                           IF    ( SYS-5002-KSNKBN (IDX-SYS)
                                           =   "2" )
                               MOVE    "T"
                                   TO   SPA-GMN-W27-CHK-KSN (IDX-KSN)
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
           MOVE    SPA-GMN-W27-STSELNUM    TO  SPA-NAI-W27-STSELNUM
           MOVE    SPA-GMN-W27-EDSELNUM    TO  SPA-NAI-W27-EDSELNUM
      *
           .
      *
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室リストチェック処理
      *****************************************************************
       4100-BRMLST-CHK-SEC             SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W27-BRMLST-MAX )
               IF    ( W27-BRMLST-SELECT (IDX1)    =   "T" )
      *
      *
                   MOVE    IDX1        TO  SPA-GMN-W27-STSELNUM
      *
      *            入力領域編集処理
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
                   MOVE    SPA-GMN-W27-STSELNUM
                                       TO  SPA-GMN-W27-BRMLST-SEL
      *
      *            現在有効な病室が選択されていること
                   IF    ( SPA-GMN-W27-TYUKO-FLG (IDX1)    NOT =  1 )
                       MOVE    "0009"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-W27-CUR
                       GO  TO  4100-BRMLST-CHK-EXT
                   END-IF
      *
                   MOVE    3           TO  SPA-GMN-W27-CUR
                   MOVE    SPA-GMN-W27-BRMLST-MAX
                                       TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           .
       4100-BRMLST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病室リスト編集処理
      *****************************************************************
       4100-BRMLST-EDIT-SEC            SECTION.
      *
           INITIALIZE  SPA-GMN-W27-BRMLST-AREA
                       SPA-NAI-W27-BRMLST-AREA
      *
           PERFORM 41001-BTUINF-SET-SEC
      *
      *    病室情報検索
           PERFORM 900-SYSKANRI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYS5002     NOT =   ZERO )
                    OR   ( SPA-GMN-W27-BRMLST-MAX >=   400 )
      *
               COMPUTE SPA-GMN-W27-BRMLST-MAX
                   =   SPA-GMN-W27-BRMLST-MAX  +   1
               MOVE    SPA-GMN-W27-BRMLST-MAX
                                   TO  IDX1
      *
      *        有効
               IF    ( SYS-5002-STYUKYMD    <= SPA-NAI-W27-KJNYMD )
                AND  ( SYS-5002-EDYUKYMD    >= SPA-NAI-W27-KJNYMD )
                   MOVE    1       TO  SPA-GMN-W27-TYUKO-FLG (IDX1)
               ELSE
                   MOVE    ZERO    TO  SPA-GMN-W27-TYUKO-FLG (IDX1)
               END-IF
      *
      *        番号
               MOVE    IDX1        TO  SPA-GMN-W27-TSELNUM (IDX1)
      *
      *        病室番号
               MOVE    SYS-5002-KBNCD
                                       TO  SPA-GMN-W27-TBRMNUM (IDX1)
               MOVE    SYS-5002-BRM-NUM
                                       TO  SPA-GMN-W27-TBRMNUM2 (IDX1)
      *
      *        特定入院料
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                       TO  SYS-KBNCD
               MOVE    SYS-5002-STYUKYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
               MOVE    SYS-5111-SRYCD  TO  SPA-NAI-W27-TTOKNYUIN (IDX1)
               MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM2
                                       TO  SPA-GMN-W27-TTOKNYUIN (IDX1)
      *
      *        病室種別
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5108"          TO  SYS-KANRICD
               MOVE    SYS-5002-BRM-SBT    TO  SYS-KBNCD
               MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5108-REC
               MOVE    SYS-5108-BRM-SBT-NM TO  SPA-GMN-W27-TBRMSBT(IDX1)
      *
      *        室料差額
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5113"          TO  SYS-KANRICD
               MOVE    SYS-5002-BRM-SAGAKU
                                       TO  SYS-KBNCD
               MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5113-REC
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  SPA-GMN-W27-TSAGAKU (IDX1)
      *
      *        開始有効年月日
               MOVE    SYS-5002-STYUKYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-W27-TSTYUKYMD (IDX1)
               MOVE    WRK-SYMD        TO  SPA-NAI-W27-TSTYUKYMD (IDX1)
      *
      *        終了有効年月日
               MOVE    SYS-5002-EDYUKYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-W27-TEDYUKYMD (IDX1)
               MOVE    WRK-SYMD        TO  SPA-NAI-W27-TEDYUKYMD (IDX1)
      *
      *        病室情報読み込み
               PERFORM 900-SYSKANRI-KEY4-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-BRMLST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報設定処理
      *****************************************************************
       41001-BTUINF-SET-SEC            SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5101"              TO  SYS-KANRICD
           MOVE    SPA-NAI-W27-SBT     TO  SYS-KBNCD
           MOVE    SPA-NAI-W27-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5101-REC
           MOVE    SYS-5101-BTU-SBT-NM TO  SPA-GMN-W27-SBT
      *
           MOVE    SPA-NAI-W27-KIHONRYO    TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W27-STYUKYMD
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-NAME                TO  SPA-GMN-W27-KHNRYO
           MOVE    TNS-NYUKHNKSNKBN        TO  SPA-NAI-W27-KSNKBN
      *
           EVALUATE    SPA-NAI-W27-KIHONRYO
           WHEN    "099999950"
               MOVE    "190097010"     TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W27-STYUKYMD
                                       TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN TO SPA-NAI-W27-KSNKBN
           WHEN    "099999951"
               MOVE    "190097210"     TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W27-STYUKYMD
                                       TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN TO SPA-NAI-W27-KSNKBN
           WHEN    OTHER
               CONTINUE
           END-EVALUATE
      *
           .
       41001-BTUINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       4100-SSTKIJUN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-BTU-NYUKSNKBN
                                       WRK-BRM-NYUKSNKBN
                                       WRK-KSN-NYUKSNKBN
      *
      *    病室入院料加算情報取得処理
           PERFORM 4100-BRM-NYUKSN-GET-SEC
      *
      *    施設基準検索処理
           PERFORM 4100-SYS-1006-SEL-SEC
      *
           MOVE    3               TO  WRK-BASE-CUR
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN         >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "T" )
                   PERFORM 41011-SSTKIJUN-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       4100-SSTKIJUN-CHK-EXT.
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
           MOVE    SPA-NAI-W27-KJNYMD
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
      *    施設基準チェック処理
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
      *
           EVALUATE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
           WHEN    "190109770"
      *        新生児入院医療管理加算
               IF  ( SSTKJN-CD (223)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106370"
      *        診療所療養病床療養環境加算１
               IF  ( SSTKJN-CD (149)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106470"
      *        診療所療養病床療養環境加算２
               IF  ( SSTKJN-CD (150)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106670"
      *        放射線治療病室管理加算
               CONTINUE
           WHEN    "190105570"
      *        療養環境加算
               IF  ( SSTKJN-CD (057)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190120210"
      *        療養病棟療養環境加算１
               IF  ( SSTKJN-CD (90)    =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106070"
      *        療養病棟療養環境加算２
               IF  ( SSTKJN-CD (91)    =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106170"
      *        療養病棟療養環境加算３
               IF  ( SSTKJN-CD (110)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106270"
      *        療養病棟療養環境加算４
               IF  ( SSTKJN-CD (602)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190105870"
           WHEN    "190105970"
      *        重症者等療養環境特別加算（個室）
      *        重症者等療養環境特別加算（２人部屋）
               IF  ( SSTKJN-CD (019)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190106570"
      *        無菌治療室管理加算
               CONTINUE
      *
           END-EVALUATE
      *
           IF    ( FLG-SSTKIJUNERR     =   1 )
               COMPUTE SPA-GMN-W27-CUR
                   =   WRK-BASE-CUR    +   IDX-KSN
               MOVE    "0003"  TO  SPA-ERRCD
               MOVE    "F"     TO  SPA-GMN-W27-CHK-KSN (IDX-KSN)
           ELSE
               MOVE    SPA-NAI-W27-KSNKBN
                               TO  WRK-BTU-NYUKSNKBN
               MOVE    SPA-NAI-W27-CHK-KSN-KSNKBN (IDX-KSN)
                               TO  WRK-KSN-NYUKSNKBN
               PERFORM 900-NYUKSNCHK-KEY-SEL-SEC
               IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                   CONTINUE
               ELSE
                   COMPUTE SPA-GMN-W27-CUR
                       =   WRK-BASE-CUR    +   IDX-KSN
                   MOVE    "0008"  TO  SPA-ERRCD
                   MOVE    "F"     TO  SPA-GMN-W27-CHK-KSN (IDX-KSN)
               END-IF
           END-IF
      *
           .
       41011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟入院料加算情報取得処理
      *****************************************************************
       4100-BRM-NYUKSN-GET-SEC         SECTION.
      *
      *    点数マスタ検索
           IF    ( WRK-TOKNYUIN-CD     NOT =   SPACE )
               MOVE    WRK-TOKNYUIN-CD         TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W27-KJNYMD      TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN        TO  WRK-BRM-NYUKSNKBN
           END-IF
      *
           .
      *
       4100-BRM-NYUKSN-GET-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W27-INPUT-AREA
                       SPA-NAI-W27-INPUT-AREA
      *
           MOVE    SPA-NAI-W27-KJNYMD
                               TO  SPA-NAI-W27-BRMSTYUKYMD
      *
           MOVE    ALL  "F"    TO  SPA-GMN-W27-CHK-KSN-G
           MOVE    SPA-NAI-W27-BRMSTYUKYMD
                               TO  WRK-KSNKJNYMD
           PERFORM 42011-KSN-SYOKI-SEC
      *
           MOVE   ZERO         TO  SPA-GMN-W27-BRMLST-SEL
           MOVE    2           TO  SPA-GMN-W27-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理２（終了選択番号は初期化しない）
      *****************************************************************
       4201-INPUT-AREA-CLEAR2-SEC      SECTION.
      *
           MOVE    SPA-GMN-W27-EDSELNUM
                                   TO  WRK-EDSELNUM
           MOVE    SPA-GMN-W27-EDBRMNUM
                                   TO  WRK-EDBRMNUM
      *
           MOVE    SPA-NAI-W27-KJNYMD
                                   TO  WRK-KJNYMD-BAK
           MOVE    SPA-NAI-W27-BRMSTYUKYMD
                                   TO  SPA-NAI-W27-KJNYMD
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           MOVE    WRK-KJNYMD-BAK  TO  SPA-NAI-W27-KJNYMD
      *
           MOVE    WRK-EDSELNUM    TO  SPA-GMN-W27-EDSELNUM
           MOVE    WRK-EDBRMNUM    TO  SPA-GMN-W27-EDBRMNUM
      *
           .
      *
       4201-INPUT-AREA-CLEAR2-EXT.
           EXIT.
      *****************************************************************
      *    加算情報初期化処理
      *****************************************************************
       42011-KSN-SYOKI-SEC             SECTION.
      *
           INITIALIZE      SPA-GMN-W27-KSNNAME-G
                           SPA-NAI-W27-CHK-KSN-G
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( WRK-KSNKJNYMD   >=   CONST-H180401 )
                   MOVE    CONST-KSN-H1804 (IDX-KSN)
                                       TO  SPA-NAI-W27-CHK-KSN-SRYCD
                                                           (IDX-KSN)
               ELSE
                   MOVE    CONST-KSN-SRYCD (IDX-KSN)
                                       TO  SPA-NAI-W27-CHK-KSN-SRYCD
                                                           (IDX-KSN)
               END-IF
      *
               IF    ( SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       NOT =   SPACE )
                   MOVE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                       TO  WRK-TNS-SRYCD
                   MOVE    WRK-KSNKJNYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME    TO  SPA-GMN-W27-KSNNAME (IDX-KSN)
                   MOVE    TNS-NYUKHNKSNKBN
                                       TO  SPA-NAI-W27-CHK-KSN-KSNKBN
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
                   UNTIL ( IDX-CMB     >   SPA-GMN-W27-BTUNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W27-BTUNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W27-BTUNUMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W27-SBTL (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    SPA-NAI-W27-KIHONRYOL (IDX-CMB)
                                       TO  WRK-CMB-ITM3
                   MOVE    SPA-NAI-W27-STYUKYMDL (IDX-CMB)
                                       TO  WRK-CMB-ITM4
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
      *    基準日が入力されていること
           IF    ( SPA-GMN-W27-KJNYMD  =   SPACE )
               MOVE    "0010"      TO  SPA-ERRCD
               MOVE    100         TO  SPA-GMN-W27-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
           
      *    開始選択番号が入力されていること
           IF    ( SPA-GMN-W27-STSELNUM    =   ZERO )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W27-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    終了選択番号が入力されていない場合、
      *    開始選択番号の対象データが有効なデータであること
           IF    ( SPA-GMN-W27-EDSELNUM    =   ZERO )
               MOVE    SPA-GMN-W27-STSELNUM
                                   TO  IDX1
               IF    ( SPA-GMN-W27-TYUKO-FLG (IDX1)
                                           =   1 )
                   CONTINUE
               ELSE
                   MOVE    "1002"  TO  SPA-ERRCD
                   MOVE    2       TO  SPA-GMN-W27-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    終了選択番号が入力されてる場合、
      *    選択範囲に有効な病室が存在していること
           IF    ( SPA-GMN-W27-EDSELNUM    >   ZERO )
               MOVE    SPA-GMN-W27-STSELNUM    TO  WRK-STIDX
               MOVE    SPA-GMN-W27-EDSELNUM    TO  WRK-EDIDX
               MOVE    ZERO                    TO  FLG-YUKO
               PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX1    >       WRK-EDIDX )
                   IF    ( SPA-GMN-W27-TYUKO-FLG (IDX1)    =   1 )
                       MOVE    1               TO  FLG-YUKO
                       MOVE    WRK-EDIDX       TO  IDX1
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-YUKO    =   ZERO )
                   MOVE    "1004"  TO  SPA-ERRCD
                   MOVE    2       TO  SPA-GMN-W27-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
      *
           END-IF
      *
      *
           INITIALIZE       FLG-KSNCD-G
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( SPA-GMN-W27-CHK-KSN (IDX-KSN)  =   "T" )
                   EVALUATE    SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                   WHEN    "190109770"
                       MOVE    1       TO  FLG-190109770
                   WHEN    "190106370"
                       MOVE    1       TO  FLG-190106370
                   WHEN    "190106470"
                       MOVE    1       TO  FLG-190106470
                   WHEN    "190106670"
                       MOVE    1       TO  FLG-190106670
                   WHEN    "190105570"
                       MOVE    1       TO  FLG-190105570
                   WHEN    "190120210"
                       MOVE    1       TO  FLG-190120210
                   WHEN    "190106070"
                       MOVE    1       TO  FLG-190106070
                   WHEN    "190106170"
                       MOVE    1       TO  FLG-190106170
                   WHEN    "190106270"
                       MOVE    1       TO  FLG-190106270
                   WHEN    "190105870"
                       MOVE    1       TO  FLG-190105870
                   WHEN    "190105970"
                       MOVE    1       TO  FLG-190105970
                   WHEN    "190106570"
                       MOVE    1       TO  FLG-190106570
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
      *
      *    診療所療養病床療養環境加算１と２が同時に算定されていないこと
           IF    ( FLG-190106370   =   1 )
            AND  ( FLG-190106470   =   1 )
      *
               MOVE    "1005"  TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
      *
           END-IF
      *
      *
      *    療養病棟療養環境加算の１２３４のいずれかのみ
      *    選択されていること
           INITIALIZE  WRK-SEL-CNT
           IF    ( FLG-190120210   =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
      *
           IF    ( FLG-190106070   =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
      *
           IF    ( FLG-190106170   =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
      *
           IF    ( FLG-190106270   =   1 )
               COMPUTE WRK-SEL-CNT =   WRK-SEL-CNT +   1
           END-IF
      *
           IF    ( WRK-SEL-CNT     >   1 )
               COMPUTE SPA-GMN-W27-CUR =   WRK-SEL-IDX +   8
               MOVE    "1006"          TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    重症者等療養環境特別加算の個室と２人部屋が同時に算定されて
      *    いないこと
           IF    ( FLG-190105870   =   1 )
            AND  ( FLG-190105970   =   1 )
      *
               MOVE    "1007"  TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
      *
           END-IF
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
           MOVE    "W27    "           TO  MCP-WINDOW.
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
           INITIALIZE                          W27
      *
      *    病棟名称
           MOVE    SPA-GMN-W27-BTUNUM-G    TO  W27-BTU-NAME
           MOVE    SPA-GMN-W27-BTUNUM-MAX
                                           TO  W27-BTU-NAME-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W27-BTUNUM-MAX )
               MOVE    SPA-GMN-W27-BTUNUMLST (IDX1)
                                   TO  W27-BTU-NAME-ITM (IDX1)
           END-PERFORM
      *    病棟種別
           MOVE    SPA-GMN-W27-SBT         TO  W27-BTU-SBT
      *    入院基本料
           MOVE    SPA-GMN-W27-KHNRYO      TO  W27-BTU-KHNRYO
      *    開始選択番号
           MOVE    SPA-GMN-W27-STSELNUM    TO  W27-STSELNUM
      *    終了選択番号
           MOVE    SPA-GMN-W27-EDSELNUM    TO  W27-EDSELNUM
      *    開始病室番号
           MOVE    SPA-GMN-W27-STBRMNUM2   TO  W27-STBRMNUM
      *    終了病室番号
           MOVE    SPA-GMN-W27-EDBRMNUM    TO  W27-EDBRMNUM
      *    基準日
           MOVE    SPA-GMN-W27-KJNYMD      TO  W27-KJNYMD
      *
      *    入院料加算
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-KSN-MAX )
               MOVE    SPA-GMN-W27-KSNNAME (IDX1)
                                           TO  W27-CHK-KSN-LBL (IDX1)
               MOVE    SPA-GMN-W27-CHK-KSN (IDX1)
                                           TO  W27-CHK-KSN (IDX1)
           END-PERFORM
      *
      *    病室情報リスト領域
           MOVE    SPA-GMN-W27-BRMLST-MAX  TO  W27-BRMLST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W27-BRMLST-MAX )
      *
      *        有効マーク
               IF    ( SPA-GMN-W27-TYUKO-FLG (IDX1)
                                           =   1 )
                   MOVE    "  ◎"          TO  W27-TYUKO (IDX1)
               ELSE
                   MOVE    SPACE           TO  W27-TYUKO (IDX1)
               END-IF
      *
      *        選択番号
               INITIALIZE                  WRK-DSPSELNUM-G
               MOVE    SPA-GMN-W27-TSELNUM (IDX1)
                                           TO  WRK-DSPSELNUM
               IF    ( SPA-NAI-W27-STSELNUM    >   ZERO )
                AND  ( SPA-NAI-W27-EDSELNUM    >   ZERO )
                AND  ( SPA-NAI-W27-EDSELNUM    >   SPA-NAI-W27-STSELNUM)
                   IF    ( SPA-GMN-W27-TSELNUM (IDX1)
                                               >=  SPA-NAI-W27-STSELNUM)
                    AND  ( SPA-GMN-W27-TSELNUM (IDX1)
                                               <=  SPA-NAI-W27-EDSELNUM)
                    AND  ( SPA-GMN-W27-TYUKO-FLG (IDX1)
                                           =   1 )
                       MOVE    "*"         TO  WRK-DSPMARK
                   END-IF
               END-IF
               MOVE    WRK-DSPSELNUM-G     TO  W27-TSELNUM (IDX1)
      *
      *        病室番号
               IF    ( SPA-GMN-W27-TBRMNUM (IDX1) (3 : )
                               IS  NUMERIC )
                   MOVE    SPA-GMN-W27-TBRMNUM (IDX1) (3 : )
                               TO   WRK-999999
                   MOVE    WRK-999999
                               TO   WRK-ZZZZZZZZZ9
                   MOVE    WRK-ZZZZZZZZZ9
                               TO  W27-TBRMNUM (IDX1)
               ELSE
                   MOVE    SPA-GMN-W27-TBRMNUM2 (IDX1)
                               TO  W27-TBRMNUM (IDX1)
               END-IF
      *
      *        特定入院料
               MOVE    SPA-GMN-W27-TTOKNYUIN (IDX1)
                               TO  W27-TTOKNYUIN (IDX1)
      *
      *        病室種別
               MOVE    SPA-GMN-W27-TBRMSBT (IDX1)
                               TO  W27-TBRMSBT (IDX1)
      *        室料差額
               MOVE    SPA-GMN-W27-TSAGAKU (IDX1)
                               TO  WRK-ZZZZZZZZZ9
               MOVE    WRK-ZZZZZZZZZ9
                               TO  W27-TSAGAKU (IDX1)
      *
      *        開始有効年月日
               MOVE    SPA-GMN-W27-TSTYUKYMD (IDX1)
                               TO  W27-TSTYUKYMD (IDX1)
      *
      *        終了有効年月日
               MOVE    SPA-GMN-W27-TEDYUKYMD (IDX1)
                               TO  W27-TEDYUKYMD (IDX1)
      *
               IF    ( SPA-GMN-W27-BRMLST-SEL  =   IDX1 )
                   MOVE    "T"             TO  W27-BRMLST-SELECT (IDX1)
               ELSE
                   MOVE    "F"             TO  W27-BRMLST-SELECT (IDX1)
               END-IF
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
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W27-CHK-KSN-SRYCD (IDX-KSN)
                                    =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                    TO  W27-CHK-KSN-STATE (IDX-KSN)
               ELSE
                   MOVE    WIDGET-NORMAL
                                    TO  W27-CHK-KSN-STATE (IDX-KSN)
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
                  (SPA-GMN-W27-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W27-CUR
           WHEN    001
               MOVE    "BTU_NAME"          TO  MCP-WIDGET
           WHEN    002
               MOVE    "STSELNUM"          TO  MCP-WIDGET
           WHEN    003
               MOVE    "EDSELNUM"          TO  MCP-WIDGET
           WHEN    004
               MOVE    "CHK_KSN01"         TO  MCP-WIDGET
           WHEN    005
               MOVE    "CHK_KSN02"         TO  MCP-WIDGET
           WHEN    006
               MOVE    "CHK_KSN03"         TO  MCP-WIDGET
           WHEN    007
               MOVE    "CHK_KSN04"         TO  MCP-WIDGET
           WHEN    008
               MOVE    "CHK_KSN05"         TO  MCP-WIDGET
           WHEN    009
               MOVE    "CHK_KSN06"         TO  MCP-WIDGET
           WHEN    010
               MOVE    "CHK_KSN07"         TO  MCP-WIDGET
           WHEN    011
               MOVE    "CHK_KSN08"         TO  MCP-WIDGET
           WHEN    012
               MOVE    "CHK_KSN09"         TO  MCP-WIDGET
           WHEN    013
               MOVE    "CHK_KSN10"         TO  MCP-WIDGET
           WHEN    014
               MOVE    "CHK_KSN11"         TO  MCP-WIDGET
           WHEN    015
               MOVE    "CHK_KSN12"         TO  MCP-WIDGET
           WHEN    016
               MOVE    "CHK_KSN13"         TO  MCP-WIDGET
           WHEN    017
               MOVE    "CHK_KSN14"         TO  MCP-WIDGET
           WHEN    018
               MOVE    "CHK_KSN15"         TO  MCP-WIDGET
           WHEN    100
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"               TO  MCP-WIDGET
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
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               COMPUTE WRK-BASE-CUR    =   IDX-KSN +   3
      *
               IF    ( SPA-GMN-W27-CUR =   WRK-BASE-CUR )
                   IF    ( W27-CHK-KSN-STATE (IDX-KSN)
                                       =   WIDGET-INSENSITIVE )
                       IF    ( IDX-KSN =   CONST-KSN-MAX )
                           COMPUTE SPA-GMN-W27-CUR
                               =   212
                       ELSE
                           COMPUTE SPA-GMN-W27-CUR
                               =   SPA-GMN-W27-CUR +   1
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
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BTU_NAME"
               MOVE    002             TO  SPA-GMN-W27-CUR
           WHEN    "STSELNUM"
               MOVE    003             TO  SPA-GMN-W27-CUR
           WHEN    "EDSELNUM"
               MOVE    004             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN01"
               MOVE    005             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN02"
               MOVE    006             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN03"
               MOVE    007             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN04"
               MOVE    008             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN05"
               MOVE    009             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN06"
               MOVE    010             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN07"
               MOVE    011             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN08"
               MOVE    012             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN09"
               MOVE    013             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN10"
               MOVE    014             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN11"
               MOVE    015             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN12"
               MOVE    016             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN13"
               MOVE    017             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN14"
               MOVE    018             TO  SPA-GMN-W27-CUR
           WHEN    "CHK_KSN15"
               MOVE    212             TO  SPA-GMN-W27-CUR
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
               MOVE    "病室加算情報の取得に失敗しました"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "入力番号に該当する病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "選択範囲に複数の入院料が存在する場合は一"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "括入力できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "0008"
               MOVE    "当該入院料において、この加算は算定できません"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "選択された病室は現在有効な病室ではありません"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "基準日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "選択された病室は現在有効な病室でないため"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "更新できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1003"
               MOVE    "範囲選択の指定に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "選択範囲に現在有効な病室がありません"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "診療所療養病床療養環境加算１と２は同時に"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "加算できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1006"
               MOVE    "療養病棟療養環境加算はいずれか１つのみ選"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "択してください"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1007"
               MOVE    "重症者等療養環境特別加算の個室と２人部屋"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "は同時に加算できません"
                                       TO  SPA-ERRMSG (41 : )
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
           MOVE    "W27"               TO  SPA-MOTOPG
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
               MOVE    "選択範囲内の有効な病室について登録処理を"
                                        TO  WRK-WIDMSG (1 : 40)
               MOVE    "します"
                                        TO  WRK-WIDMSG (41 : )
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W27"                TO  SPA-MOTOPG
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
      *    システム管理検索処理(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYS5002
      *
           INITIALIZE                  SYS-5002-REC
           MOVE    "5002"          TO  SYS-5002-KANRICD
           MOVE    SPA-GMN-W27-BTUNUM
                                   TO  SYS-5002-KBNCD
           MOVE    "%"             TO  SYS-5002-KBNCD (3:1)
           MOVE    SYS-5002-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYS5002
               INITIALIZE              SYS-5002-REC
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
           MOVE    ZERO            TO  FLG-SYS5002
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYS5002
               INITIALIZE              SYS-5002-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-FET-EXT.
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
           IF    ( WRK-BRM-NYUKSNKBN   NOT =   SPACE )
               MOVE    WRK-BRM-NYUKSNKBN   TO  NYUKSNCHK-NYUINKBN
           ELSE
               MOVE    WRK-BTU-NYUKSNKBN   TO  NYUKSNCHK-NYUINKBN
           END-IF
           MOVE    WRK-KSN-NYUKSNKBN   TO  NYUKSNCHK-KSNKBN
           MOVE    SPA-NAI-W27-BRMSTYUKYMD
                                       TO  NYUKSNCHK-YUKOSTYMD
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

