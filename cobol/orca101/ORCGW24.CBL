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
       PROGRAM-ID.                               ORCGW24.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 医療機関入院基本情報設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-太田    03.01.24  入院診療計画未実施減算を
      *                                     画面から削除
      *  00.00.02    NACL-太田    03.03.05  入院歴表示区分を追加
      *  00.00.03    NACL-太田    03.03.07  請求書発行区分を追加
      *                                     退院証明書発行区分を追加
      *  00.00.04    NACL-太田    03.03.24  選定療養費情報を追加
      *  00.00.05    NACL-太田    03.04.10  請求書未収額印刷区分を追加
      *  00.00.06    NACL-太田    03.06.25  点数マスタ有効期間の指定を修正
      *  00.00.07    NACL-太田    04.01.07  更新日付編集対応
      *  00.00.08    NACL-太田    04.01.15  室料差額消費税区分追加
      *  01.00.01    NACL-太田    04.03.04  臨床研修病院入院診療加算
      *                                     離島加算追加
      *  01.00.02    NACL-太田    04.08.23  入院会計室料差額区分追加
      *  01.00.03    NACL-小豆沢  06.01.19  外泊登録時の実更新区分追加
      *  02.08.00    NACL-太田    06.02.27  減算情報の編集場所変更
      *  03.02.01    NACL-小豆沢  06.08.09  標欠対応
      *  03.02.02    NACL-小豆沢  06.09.12  定数超過対応
      *  03.04.00    NACL-多々納  06/10/26  検索画面連絡情報追加 
      *  03.04.00    NACL-太田    06/11/29  退院再計算区分追加
      *  03.05.00    NACL-太田    07.05.18  グループ診療対応
      *  03.05.00    NACL-小豆沢  07.07.27  療養病棟（病床）入院会計
      *                                     表示区分対応
      *  04.03.00    NACL-多々納  08.08.28  退院時院外処方せん区分追加
      *  04.05.00    NACL-太田    10.03.01  改正対応
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
           COPY    "W24SCR-SPA".
      *    画面用ＳＰＡ
           COPY    "W25SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-KANJI                           PIC 9(01).
           03  FLG-KENSAKU                         PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-NYUKSNCD-G.
               05  FLG-190098870                   PIC 9(01).
               05  FLG-190098970                   PIC 9(01).
               05  FLG-190099170                   PIC 9(01).
               05  FLG-190099270                   PIC 9(01).
               05  FLG-190099570                   PIC 9(01).
               05  FLG-190099870                   PIC 9(01).
               05  FLG-190099970                   PIC 9(01).
               05  FLG-190100370                   PIC 9(01).
               05  FLG-190100470                   PIC 9(01).
               05  FLG-190100670                   PIC 9(01).
               05  FLG-190100770                   PIC 9(01).
               05  FLG-190100970                   PIC 9(01).
               05  FLG-190107170                   PIC 9(01).
               05  FLG-190117170                   PIC 9(01).
               05  FLG-190117270                   PIC 9(01).
               05  FLG-190119910                   PIC 9(01).
               05  FLG-190120410                   PIC 9(01).
               05  FLG-190120510                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDXC                                PIC 9(05).
           03  IDXY                                PIC 9(05).
           03  IDXZ                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-SYS                             PIC 9(05).
           03  IDX-SKJ                             PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
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
           03  WRK-CMB-CD                          PIC X(05).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX                     PIC 9(03).
               05  WRK-CMB-ITM                     PIC X(100).
               05  WRK-CMB-ITM2                    PIC X(100).
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-INPUTCD                         PIC X(09).
           03  WRK-CD-MCNT                         PIC 9(02).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-NAME                        PIC X(100).
           03  WRK-SELNUM                          PIC 9(05).
           03  WRK-STYUKYMD                        PIC X(08).
           03  WRK-EDYUKYMD                        PIC X(08).
           03  WRK-SEL-STYUKYMD                    PIC X(08).
           03  WRK-SEL-EDYUKYMD                    PIC X(08).
           03  WRK-CREYMD                          PIC X(08).
           03  WRK-MAX                             PIC 9(05).
           03  WRK-BASE-CUR                        PIC 9(05).
           03  WRK-CHIIKIKSN-G.
               05  WRK-CHIIKIKSN                   PIC 9(01).
               05  WRK-CHIIKIKSNFL                 PIC X(27).
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
      *
           03  CONST-H220401           PIC X(08)   VALUE "20100401".
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
           03  CONST-H160401           PIC X(08)   VALUE "20040401".
           03  CONST-SRYACT-MAX        PIC 9(03)   VALUE 10.
      *
      *    選定療養費−点数単価
           03  CONST-SENTEI-TANKA      PIC 9(03)   VALUE 10.
      *    入院時食事療養
           03  CONST-SHOKUJIRYOYOCD-VAL.
               05                      PIC X(09)   VALUE   "197000110".
               05                      PIC X(09)   VALUE   "197000710".
           03  CONST-SHOKUJIRYOYOCD-R
                       REDEFINES   CONST-SHOKUJIRYOYOCD-VAL.
               05  CONST-SHOKUJIRYOYOCD-OCC        OCCURS  2.
                   07  CONST-SHOKUJIRYOYOCD
                                       PIC X(09).
      *    食事情報
           03  CONST-SKJSRY-AREA.
               05  CONST-SKJSRY-MAX    PIC 9(05)   VALUE   2.
               05  CONST-SKJSRY-H1804-G.
                   07  CONST-SKJSRY-H1804-VAL.
                       09              PIC X(09)   VALUE   "197000570".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-SKJSRY-H1804-R
                               REDEFINES   CONST-SKJSRY-H1804-VAL.
                       09  CONST-SKJSRY-H1804-OCC  OCCURS  2.
                           11  CONST-SKJSRY-H1804  PIC X(09).
               05  CONST-SKJSRY-G.
                   07  CONST-SKJSRY-VAL.
                       09              PIC X(09)   VALUE   "197000270".
                       09              PIC X(09)   VALUE   "197000570".
                   07  CONST-SKJSRY-R  REDEFINES   CONST-SKJSRY-VAL.
                       09  CONST-SKJSRY-OCC        OCCURS  2.
                           11  CONST-SKJSRY-SRYCD  PIC X(09).
      *
      *    医療機関減算情報
           03  CONST-GEN-AREA.
               05  CONST-GEN-MAX       PIC 9(05)   VALUE   4.
               05  CONST-GEN-VAL.
                   07                  PIC X(09)   VALUE   "190080470".
                   07                  PIC X(09)   VALUE   "190080570".
                   07                  PIC X(09)   VALUE   "190111690".
                   07                  PIC X(09)   VALUE   "190111790".
               05  CONST-GEN-R REDEFINES   CONST-GEN-VAL.
                   07  CONST-GEN-OCC               OCCURS  4.
                       09  CONST-GENCD PIC X(09).
      *
      *    入院加算情報
           03  CONST-NYUKSN-AREA.
               05  CONST-NYUKSN-MAX    PIC 9(05)   VALUE   20.
               05  CONST-NYUKSN-H2204-G.
                   07  CONST-NYUKSN-H2204-VAL.
                       09              PIC X(09)   VALUE   "190098870".
                       09              PIC X(09)   VALUE   "190099870".
                       09              PIC X(09)   VALUE   "190100370".
                       09              PIC X(09)   VALUE   "190100470".
                       09              PIC X(09)   VALUE   "190100770".
                       09              PIC X(09)   VALUE   "190100670".
                       09              PIC X(09)   VALUE   "190100970".
                       09              PIC X(09)   VALUE   "190107170".
                       09              PIC X(09)   VALUE   "190117170".
                       09              PIC X(09)   VALUE   "190119910".
                       09              PIC X(09)   VALUE   "190117270".
                       09              PIC X(09)   VALUE   "190120510".
                       09              PIC X(09)   VALUE   "190136910".
                       09              PIC X(09)   VALUE   "190137070".
                       09              PIC X(09)   VALUE   "190120410".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-NYUKSN-H2204-R
                               REDEFINES   CONST-NYUKSN-H2204-VAL.
                       09  CONST-NYUKSN-H2204
                                       PIC X(09)   OCCURS   20.
               05  CONST-NYUKSN-H1804-G.
                   07  CONST-NYUKSN-H1804-VAL.
                       09              PIC X(09)   VALUE   "190098870".
                       09              PIC X(09)   VALUE   "190099870".
                       09              PIC X(09)   VALUE   "190100370".
                       09              PIC X(09)   VALUE   "190100470".
                       09              PIC X(09)   VALUE   "190100770".
                       09              PIC X(09)   VALUE   "190100670".
                       09              PIC X(09)   VALUE   "190100970".
                       09              PIC X(09)   VALUE   "190107170".
                       09              PIC X(09)   VALUE   "190117170".
                       09              PIC X(09)   VALUE   "190119910".
                       09              PIC X(09)   VALUE   "190117270".
                       09              PIC X(09)   VALUE   "190120510".
                       09              PIC X(09)   VALUE   "190120410".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-NYUKSN-H1804-R
                               REDEFINES   CONST-NYUKSN-H1804-VAL.
                       09  CONST-NYUKSN-H1804
                                       PIC X(09)   OCCURS   20.
               05  CONST-NYUKSN-H1604-G.
                   07  CONST-NYUKSN-H1604-VAL.
                       09              PIC X(09)   VALUE   "190098870".
                       09              PIC X(09)   VALUE   "190098970".
                       09              PIC X(09)   VALUE   "190099170".
                       09              PIC X(09)   VALUE   "190099270".
                       09              PIC X(09)   VALUE   "190099570".
                       09              PIC X(09)   VALUE   "190099870".
                       09              PIC X(09)   VALUE   "190099970".
                       09              PIC X(09)   VALUE   "190100370".
                       09              PIC X(09)   VALUE   "190100470".
                       09              PIC X(09)   VALUE   "190100770".
                       09              PIC X(09)   VALUE   "190100670".
                       09              PIC X(09)   VALUE   "190100970".
                       09              PIC X(09)   VALUE   "190107170".
                       09              PIC X(09)   VALUE   "190117170".
                       09              PIC X(09)   VALUE   "190117270".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-NYUKSN-H1604-R
                               REDEFINES   CONST-NYUKSN-H1604-VAL.
                       09  CONST-NYUKSN-H1604
                                       PIC X(09)   OCCURS   20.
               05  CONST-NYUKSN-SRYCD-G.
                   07  CONST-NYUKSN-SRYCD-VAL.
                       09              PIC X(09)   VALUE   "190098870".
                       09              PIC X(09)   VALUE   "190098970".
                       09              PIC X(09)   VALUE   "190099170".
                       09              PIC X(09)   VALUE   "190099270".
                       09              PIC X(09)   VALUE   "190099570".
                       09              PIC X(09)   VALUE   "190099870".
                       09              PIC X(09)   VALUE   "190099970".
                       09              PIC X(09)   VALUE   "190100370".
                       09              PIC X(09)   VALUE   "190100470".
                       09              PIC X(09)   VALUE   "190100770".
                       09              PIC X(09)   VALUE   "190100670".
                       09              PIC X(09)   VALUE   "190100970".
                       09              PIC X(09)   VALUE   "190107170".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-NYUKSN-SRYCD-R
                               REDEFINES   CONST-NYUKSN-SRYCD-VAL.
                       09  CONST-NYUKSN-SRYCD
                                       PIC X(09)   OCCURS   20.
      *
      *    地域加算情報
           03  CONST-CHIIKIKSN-SRYCD-AREA.
               05  CONST-CHIIKIKSN-H1804-G.
                   07  CONST-CHIIKIKSN-H1804-VAL.
                       09              PIC X(09)   VALUE   "190077070".
                       09              PIC X(09)   VALUE   "190077170".
                       09              PIC X(09)   VALUE   "190120010".
                       09              PIC X(09)   VALUE   "190077270".
                       09              PIC X(09)   VALUE   "190120110".
                       09              PIC X(09)   VALUE   "190077370".
                   07  CONST-CHIIKIKSN-H1804-R
                           REDEFINES   CONST-CHIIKIKSN-H1804-VAL.
                       09  CONST-CHIIKIKSN-H1804
                                       PIC X(09)   OCCURS  6.
               05  CONST-CHIIKIKSN-SRYCD-G.
                   07  CONST-CHIIKIKSN-SRYCD-VAL.
                       09              PIC X(09)   VALUE   "190077070".
                       09              PIC X(09)   VALUE   "190077170".
                       09              PIC X(09)   VALUE   "190077270".
                       09              PIC X(09)   VALUE   "190077370".
                       09              PIC X(09)   VALUE   "         ".
                       09              PIC X(09)   VALUE   "         ".
                   07  CONST-CHIIKIKSN-SRYCD-R
                           REDEFINES   CONST-CHIIKIKSN-SRYCD-VAL.
                       09  CONST-CHIIKIKSN-SRYCD
                                       PIC X(09)   OCCURS  6.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    施設基準コードフラグ情報
           COPY  "CPSK1006.INC".
      *    医療機関入院基本情報
           COPY  "CPSK5000.INC".
      *    点数マスタ
           COPY  "CPTENSU.INC".
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
           COPY    "W121.INC".
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
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W32.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W24FREE
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-W24FREE     TO  SPA-FREE
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
           MOVE    "W24    "           TO  MCP-WINDOW.
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
           WHEN    "W25"
      *        診療行為セット処理
               PERFORM 320-W25SPASET-SEC
               GO  TO  300-SCREEN-EXT
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
           PERFORM 800-H1804-KAISEI-CHK-SEC
      *
           PERFORM 3101-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-SPASET-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-W24
           INITIALIZE                  SPA-W24
                                           W24
      *
      *    コンボボックス初期化
           PERFORM 310-CMB-SYOKI-SEC
      *
      *    入力情報領域初期化処理
           PERFORM 4201-INPUT-AREA-INIT-SEC
      *
      *    入力情報編集処理
           PERFORM 310-INPUT-AREA-EDIT-SEC
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       310-CMB-SYOKI-SEC               SECTION.
      *
           INITIALIZE              SPA-GMN-W24-CMB-DAT-AREA
      *
      *    入院時食事療養コンボ
           PERFORM 310-SKJRYOYOLST-SET-SEC
      *
      *    食事診療行為コンボ
           PERFORM 310-SKJSRYLST-SET-SEC
      *
      *    地域加算コンボ
           PERFORM 310-CHIIKIKSNLST-SET-SEC
      *
      *    選定療養費−負担区分コンボ
           PERFORM 310-SENTEI-FTNKBNLST-SET-SEC
      *
      *    選定療養費−消費税コンボ
           PERFORM 310-SENTEI-TAXLST-SET-SEC
      *
      *    入院歴表示コンボ
           PERFORM 310-REKI-DISPKBNLST-SET-SEC
      *
      *    退院再計算区分コンボ
           PERFORM 310-SAIKEISANKBNLST-SET-SEC
      *
      *    入金の取扱いコンボ
           PERFORM 310-NYKNKNRKBNLST-SET-SEC
      *
      *    請求書発行区分コンボ
           PERFORM 310-SKYPRTKBNLST-SET-SEC
      *
      *    請求書未収額印刷区分コンボ
           PERFORM 310-MISYUPRTKBNLST-SET-SEC
      *
      *    診療費明細書発行区分コンボ
           PERFORM 310-SRYPRTKBNLST-SET-SEC
      *
      *    退院証明書発行区分コンボ
           PERFORM 310-TAIPRTKBNLST-SET-SEC
      *
      *    退院証明書記載区分コンボ
           PERFORM 310-TAIKSIKBNLST-SET-SEC
      *
      *    室料差額消費税区分コンボ
           PERFORM 310-BRMSGK-TAXLST-SET-SEC
      *
      *    入院会計室料差額区分コンボ
           PERFORM 310-NACCT-BRMSGKLST-SET-SEC
      *
      *    入院会計外泊区分コンボ
           PERFORM 310-NACCT-GAIHAKULST-SET-SEC
      *
      *    標欠減額区分コンボ
           IF    ( SPA-W01-STYUKYMD    >=  CONST-H180401 )
               PERFORM 310-HYOUKETULST-SET-SEC
           END-IF
      *
      *    定数超過区分コンボ
           IF    ( SPA-W01-STYUKYMD    >=  CONST-H180401 )
               PERFORM 310-TEISULST-SET-SEC
           END-IF
      *
      *    ＡＤＬ入力区分コンボ
           PERFORM 310-ADLINPUTLST-SET-SEC
      *
      *    定期請求差額初期表示区分コンボ
           PERFORM 310-TEIKISAGAKUACTLST-SET-SEC
      *
      *    療養入院表示区分コンボ
           PERFORM 310-RYOYODSPLST-SET-SEC
      *!!
      *    退院時院外処方せん発行区分コンボ
           PERFORM 310-SHOHOUKBNLST-SET-SEC
      *
           .
       310-CMB-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養コンボ
      *****************************************************************
       310-SKJRYOYOLST-SET-SEC         SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SKJRYOYO-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-SKJRYOYOL   (1)
           MOVE    "入院時食事療養（１）"
                                   TO  SPA-GMN-W24-SKJRYOYOMEIL(1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-SKJRYOYOL   (2)
           MOVE    "入院時食事療養（２）"
                                   TO  SPA-GMN-W24-SKJRYOYOMEIL(2)
      *
           .
       310-SKJRYOYOLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事診療行為コンボ
      *****************************************************************
       310-SKJSRYLST-SET-SEC           SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SKJSRY-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-SKJSRYL    (1)
           MOVE    "算定しない"    TO  SPA-GMN-W24-SKJSRYMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-SKJSRYL    (2)
           MOVE    "算定する"      TO  SPA-GMN-W24-SKJSRYMEIL (2)
      *
           .
       310-SKJSRYLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    地域加算コンボ
      *****************************************************************
       310-CHIIKIKSNLST-SET-SEC        SECTION.
      *
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               MOVE    6               TO  SPA-GMN-W24-CHIIKIKSN-MAX
      *
               MOVE    "1"             TO  SPA-GMN-W24-CHIIKIKSNL(1)
               MOVE    "１級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W24-CHIIKIKSNL    (2)
               MOVE    "２級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W24-CHIIKIKSNL    (3)
               MOVE    "３級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W24-CHIIKIKSNL    (4)
               MOVE    "４級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W24-CHIIKIKSNL    (5)
               MOVE    "５級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(5)
               MOVE    "6"             TO  SPA-GMN-W24-CHIIKIKSNL    (6)
               MOVE    "６級地地域加算" TO  SPA-GMN-W24-CHIIKIKSNMEIL(6)
           ELSE
               MOVE    4               TO  SPA-GMN-W24-CHIIKIKSN-MAX
      *
               MOVE    "1"             TO  SPA-GMN-W24-CHIIKIKSNL    (1)
               MOVE    "１種地域加算"  TO  SPA-GMN-W24-CHIIKIKSNMEIL (1)
               MOVE    "2"             TO  SPA-GMN-W24-CHIIKIKSNL    (2)
               MOVE    "２種地域加算"  TO  SPA-GMN-W24-CHIIKIKSNMEIL (2)
               MOVE    "3"             TO  SPA-GMN-W24-CHIIKIKSNL    (3)
               MOVE    "３種地域加算"  TO  SPA-GMN-W24-CHIIKIKSNMEIL (3)
               MOVE    "4"             TO  SPA-GMN-W24-CHIIKIKSNL    (4)
               MOVE    "４種地域加算"  TO  SPA-GMN-W24-CHIIKIKSNMEIL (4)
           END-IF
      *
           .
       310-CHIIKIKSNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−負担区分コンボ
      *****************************************************************
       310-SENTEI-FTNKBNLST-SET-SEC    SECTION.
      *
           MOVE    3               TO  SPA-GMN-W24-SENTEI-FTNKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-SENTEI-FTNKBNL    (1)
           MOVE    "医療機関定率設定"
                                   TO  SPA-GMN-W24-SENTEI-FTNKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-SENTEI-FTNKBNL    (2)
           MOVE    "医療機関定額設定"
                                   TO  SPA-GMN-W24-SENTEI-FTNKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-W24-SENTEI-FTNKBNL    (3)
           MOVE    "病棟別に設定"
                                   TO  SPA-GMN-W24-SENTEI-FTNKBNMEIL (3)
      *
           .
       310-SENTEI-FTNKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−消費税コンボ
      *****************************************************************
       310-SENTEI-TAXLST-SET-SEC       SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SENTEI-TAX-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-SENTEI-TAXL    (1)
           MOVE    "なし"          TO  SPA-GMN-W24-SENTEI-TAXMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-SENTEI-TAXL    (2)
           MOVE    "あり"          TO  SPA-GMN-W24-SENTEI-TAXMEIL (2)
      *
           .
       310-SENTEI-TAXLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院歴表示コンボ
      *****************************************************************
       310-REKI-DISPKBNLST-SET-SEC     SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-REKI-DISPKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-REKI-DISPKBNL    (1)
           MOVE    "入院時の歴を表示"
                                   TO  SPA-GMN-W24-REKI-DISPKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-REKI-DISPKBNL    (2)
           MOVE    "最新の歴を表示"
                                   TO  SPA-GMN-W24-REKI-DISPKBNMEIL (2)
      *
           .
       310-REKI-DISPKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    再計算区分コンボ
      *****************************************************************
       310-SAIKEISANKBNLST-SET-SEC     SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SAIKEISANKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-SAIKEISANKBNL    (1)
           MOVE    "収納再作成"
                                   TO  SPA-GMN-W24-SAIKEISANKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-SAIKEISANKBNL    (2)
           MOVE    "収納訂正"
                                   TO  SPA-GMN-W24-SAIKEISANKBNMEIL (2)
      *
           .
       310-SAIKEISANKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱いコンボ
      *****************************************************************
       310-NYKNKNRKBNLST-SET-SEC     SECTION.
      *
           MOVE    5               TO  SPA-GMN-W24-NYKNKNRKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-NYKNKNRKBNL (1)
           MOVE    "今回請求分のみ入金"
                                   TO  SPA-GMN-W24-NYKNKNRKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-NYKNKNRKBNL (2)
           MOVE    "今回分・伝票の古い未収順"
                                   TO  SPA-GMN-W24-NYKNKNRKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-W24-NYKNKNRKBNL (3)
           MOVE    "今回分・伝票の新しい未収順"
                                   TO  SPA-GMN-W24-NYKNKNRKBNMEIL (3)
      *
           MOVE    "4"             TO  SPA-GMN-W24-NYKNKNRKBNL (4)
           MOVE    "伝票の古い未収順"
                                   TO  SPA-GMN-W24-NYKNKNRKBNMEIL (4)
      *
           MOVE    "5"             TO  SPA-GMN-W24-NYKNKNRKBNL (5)
           MOVE    "伝票の新しい未収順"
                                   TO  SPA-GMN-W24-NYKNKNRKBNMEIL (5)
      *
           .
       310-NYKNKNRKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボ
      *****************************************************************
       310-SKYPRTKBNLST-SET-SEC    SECTION.
      *
           MOVE    3               TO  SPA-GMN-W24-SKYPRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-SKYPRTKBNL   (1)
           MOVE    "発行しない"    TO  SPA-GMN-W24-SKYPRTKBNMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-SKYPRTKBNL   (2)
           MOVE    "発行する"      TO  SPA-GMN-W24-SKYPRTKBNMEIL(2)
      *
           MOVE    "2"             TO  SPA-GMN-W24-SKYPRTKBNL   (3)
           MOVE    "発行する（請求あり）"
                                   TO  SPA-GMN-W24-SKYPRTKBNMEIL(3)
      *
           .
       310-SKYPRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求書未収額印刷区分コンボ
      *****************************************************************
       310-MISYUPRTKBNLST-SET-SEC      SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-MISYUPRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-MISYUPRTKBNL    (1)
           MOVE    "印字しない"    TO  SPA-GMN-W24-MISYUPRTKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-MISYUPRTKBNL    (2)
           MOVE    "印字する"      TO  SPA-GMN-W24-MISYUPRTKBNMEIL (2)
      *
           .
       310-MISYUPRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分コンボ
      *****************************************************************
       310-SRYPRTKBNLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SRYPRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-SRYPRTKBNL   (1)
           MOVE    "発行しない"    TO  SPA-GMN-W24-SRYPRTKBNMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-SRYPRTKBNL   (2)
           MOVE    "発行する"      TO  SPA-GMN-W24-SRYPRTKBNMEIL(2)
      *
      *
           .
       310-SRYPRTKBNLST-SET-EXT.
           EXIT.
      *!
      *****************************************************************
      *    退院時院外処方せん発行区分コンボ
      *****************************************************************
       310-SHOHOUKBNLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-SHOHOUKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-SHOHOUKBNL   (1)
           MOVE    "発行しない"    TO  SPA-GMN-W24-SHOHOUKBNMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-SHOHOUKBNL   (2)
           MOVE    "発行する"      TO  SPA-GMN-W24-SHOHOUKBNMEIL(2)
      *
      *
           .
       310-SHOHOUKBNLST-SET-EXT.
           EXIT.
      *!
      *****************************************************************
      *    退院証明書発行区分コンボ
      *****************************************************************
       310-TAIPRTKBNLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-TAIPRTKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-TAIPRTKBNL    (1)
           MOVE    "発行しない"    TO  SPA-GMN-W24-TAIPRTKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-TAIPRTKBNL    (2)
           MOVE    "発行する"      TO  SPA-GMN-W24-TAIPRTKBNMEIL (2)
      *
           .
       310-TAIPRTKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書記載区分コンボ
      *****************************************************************
       310-TAIKSIKBNLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-TAIKSIKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-TAIKSIKBNL    (1)
           MOVE    "２と３を記載しない"
                                   TO  SPA-GMN-W24-TAIKSIKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-TAIKSIKBNL    (2)
           MOVE    "２と３を記載する"
                                   TO  SPA-GMN-W24-TAIKSIKBNMEIL (2)
      *
           .
       310-TAIKSIKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額消費税区分コンボ
      *****************************************************************
       310-BRMSGK-TAXLST-SET-SEC   SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-BRMSGK-TAX-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-BRMSGK-TAXL    (1)
           MOVE    "なし"          TO  SPA-GMN-W24-BRMSGK-TAXMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-BRMSGK-TAXL    (2)
           MOVE    "あり"          TO  SPA-GMN-W24-BRMSGK-TAXMEIL (2)
      *
           .
       310-BRMSGK-TAXLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計室料差額区分コンボ
      *****************************************************************
       310-NACCT-BRMSGKLST-SET-SEC     SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-NACCT-BRMSGK-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-NACCT-BRMSGKL    (1)
           MOVE    "算定しない"    TO  SPA-GMN-W24-NACCT-BRMSGKMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-NACCT-BRMSGKL    (2)
           MOVE    "算定する"      TO  SPA-GMN-W24-NACCT-BRMSGKMEIL (2)
      *
           .
       310-NACCT-BRMSGKLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院会計外泊区分コンボ
      *****************************************************************
       310-NACCT-GAIHAKULST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-NACCT-GAIHAKU-MAX
      *
           MOVE    "1"             TO  SPA-GMN-W24-NACCT-GAIHAKUL    (1)
           MOVE    "実更新なし"    TO  SPA-GMN-W24-NACCT-GAIHAKUMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W24-NACCT-GAIHAKUL    (2)
           MOVE    "実更新あり"    TO  SPA-GMN-W24-NACCT-GAIHAKUMEIL (2)
      *
           .
       310-NACCT-GAIHAKULST-SET-EXT.
           EXIT.
      *****************************************************************
      *    標欠減額区分コンボ
      *****************************************************************
       310-HYOUKETULST-SET-SEC    SECTION.
      *
           MOVE    5               TO  SPA-GMN-W24-HYOUKETU-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-HYOUKETUL    (1)
           MOVE    "減額なし"
                                   TO  SPA-GMN-W24-HYOUKETUMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-HYOUKETUL    (2)
           MOVE    "１００分の１０減額"
                                   TO  SPA-GMN-W24-HYOUKETUMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-W24-HYOUKETUL    (3)
           MOVE    "１００分の１５減額"
                                   TO  SPA-GMN-W24-HYOUKETUMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-W24-HYOUKETUL    (4)
           MOVE    "１００分の２減額"
                                   TO  SPA-GMN-W24-HYOUKETUMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-W24-HYOUKETUL    (5)
           MOVE    "１００分の３減額"
                                   TO  SPA-GMN-W24-HYOUKETUMEIL (5)
      *
           .
       310-HYOUKETULST-SET-EXT.
           EXIT.
      *****************************************************************
      *    定数超過区分コンボ
      *****************************************************************
       310-TEISULST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-TEISU-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-TEISUL    (1)
           MOVE    "定数超過なし"
                                   TO  SPA-GMN-W24-TEISUMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-TEISUL    (2)
           MOVE    "定数超過あり"
                                   TO  SPA-GMN-W24-TEISUMEIL (2)
      *
           .
       310-TEISULST-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ入力区分コンボ
      *****************************************************************
       310-ADLINPUTLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-ADLINPUT-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-ADLINPUTL    (1)
           MOVE    "しない"
                                   TO  SPA-GMN-W24-ADLINPUTMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-ADLINPUTL    (2)
           MOVE    "する"
                                   TO  SPA-GMN-W24-ADLINPUTMEIL (2)
      *
           .
       310-ADLINPUTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求差額初期表示区分コンボ
      *****************************************************************
       310-TEIKISAGAKUACTLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-TEIKISAGAKUACT-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-TEIKISAGAKUACTL   (1)
           MOVE    "初期表示しない"
                                   TO  SPA-GMN-W24-TEIKISAGAKUACTMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-TEIKISAGAKUACTL   (2)
           MOVE    "初期表示する"
                                   TO  SPA-GMN-W24-TEIKISAGAKUACTMEIL(2)
      *
           .
       310-TEIKISAGAKUACTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養入院表示区分コンボ
      *****************************************************************
       310-RYOYODSPLST-SET-SEC    SECTION.
      *
           MOVE    2               TO  SPA-GMN-W24-RYOYODSP-MAX
      *
           MOVE    "0"             TO  SPA-GMN-W24-RYOYODSPL    (1)
           MOVE    "算定入院料のみ表示"
                                   TO  SPA-GMN-W24-RYOYODSPMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-W24-RYOYODSPL    (2)
      *
           IF    ( SPA-W01-STYUKYMD    >=  CONST-H220401 )
            AND  ( SPA-HOSPSBT          =  1 )
               MOVE    "入院料Ａ〜Ｉを表示"
                                   TO  SPA-GMN-W24-RYOYODSPMEIL (2)
           ELSE
               MOVE    "入院料Ａ〜Ｅを表示"
                                   TO  SPA-GMN-W24-RYOYODSPMEIL (2)
           END-IF
      *
           .
       310-RYOYODSPLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    入力情報編集処理
      *****************************************************************
       310-INPUT-AREA-EDIT-SEC     SECTION.
      *
      *    医療機関入院基本情報検索処理
           PERFORM 310-SYS-5000-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               CONTINUE
           ELSE
      *
      *        医療機関入院基本情報検索処理（期間切り替えチェック）
               PERFORM 311-SYS-5000-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   CONTINUE
               ELSE
                   GO  TO  310-INPUT-AREA-EDIT-EXT
               END-IF
      *
           END-IF
      *
      *    食事療養費
           IF    ( SYS-5000-SHOKUJIRYOYOCD NOT =   SPACE )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   2 )
                   IF    ( SYS-5000-SHOKUJIRYOYOCD
                                   =   CONST-SHOKUJIRYOYOCD (IDX0) )
                       MOVE    SPA-GMN-W24-SKJRYOYOLST (IDX0)
                                       TO  SPA-GMN-W24-SKJRYOYO-G
                   END-IF
               END-PERFORM
           END-IF
      *
      *    特別管理加算
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               CONTINUE
           ELSE
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-SKJSRY-MAX )
                        OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX0)
                                   =   SPACE )
                   IF    ( SYS-5000-SHOKUCD (1)
                                   =   SPA-NAI-W24-SKJSRY-SRYCD (IDX0))
                       MOVE    SYS-5000-SHOKUKBN (1)
                                       TO  WRK-CMB-CD
                       PERFORM 4200-SKJSRYLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-SKJSRY-OCC
                                                                (IDX0)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    食堂加算
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-SKJSRY-MAX )
                    OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX0)
                                   =   SPACE )
               IF    ( SYS-5000-SHOKUCD (2)
                                   =   SPA-NAI-W24-SKJSRY-SRYCD (IDX0))
                   MOVE    SYS-5000-SHOKUKBN (2)
                                       TO  WRK-CMB-CD
                   PERFORM 4200-SKJSRYLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-SKJSRY-OCC
                                                                (IDX0)
               END-IF
           END-PERFORM
      *
      *    室料差額消費税区分
           MOVE    SYS-5000-BRMSGK-TAXKBN  TO  WRK-CMB-CD
           PERFORM 4200-BRMSGK-TAXLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-BRMSGK-TAX-G
           ELSE
               MOVE    SPA-GMN-W24-BRMSGK-TAXLST (1)
                                           TO  SPA-GMN-W24-BRMSGK-TAX-G
           END-IF
      *
      *    入院会計室料差額区分
           MOVE    SYS-5000-NACCT-BRMSGKKBN  TO  WRK-CMB-CD
           PERFORM 4200-NACCT-BRMSGKLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NACCT-BRMSGK-G
           ELSE
               MOVE    SPA-GMN-W24-NACCT-BRMSGKLST (1)
                                       TO  SPA-GMN-W24-NACCT-BRMSGK-G
           END-IF
      *
      *    入院会計外泊区分
           MOVE    SYS-5000-NACCT-GAIHAKUKBN  TO  WRK-CMB-CD
           PERFORM 4200-NACCT-GAIHAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NACCT-GAIHAKU-G
           ELSE
               MOVE    SPA-GMN-W24-NACCT-GAIHAKULST (1)
                                       TO  SPA-GMN-W24-NACCT-GAIHAKU-G
           END-IF
      *
      *    標欠減額区分
           MOVE    SYS-5000-HYOUKETUKBN  TO  WRK-CMB-CD
           PERFORM 4200-HYOUKETULST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-HYOUKETU-G
           ELSE
               MOVE    SPA-GMN-W24-HYOUKETULST (1)
                                       TO  SPA-GMN-W24-HYOUKETU-G
           END-IF
      *
      *    定数超過区分
           MOVE    SYS-5000-TEISUKBN  TO  WRK-CMB-CD
           PERFORM 4200-TEISULST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TEISU-G
           ELSE
               MOVE    SPA-GMN-W24-TEISULST (1)
                                       TO  SPA-GMN-W24-TEISU-G
           END-IF
      *
      *    ＡＤＬ入力区分
           MOVE    SYS-5000-ADLINPUTKBN  TO  WRK-CMB-CD
           PERFORM 4200-ADLINPUTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-ADLINPUT-G
           ELSE
               MOVE    SPA-GMN-W24-ADLINPUTLST (2)
                                       TO  SPA-GMN-W24-ADLINPUT-G
           END-IF
      *
      *    定期請求差額初期表示
           MOVE    SYS-5000-TEIKISAGAKUACTKBN  TO  WRK-CMB-CD
           PERFORM 4200-TEIKISAGAKUACTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TEIKISAGAKUACT-G
           ELSE
               MOVE    SPA-GMN-W24-TEIKISAGAKUACTLST (1)
                                       TO  SPA-GMN-W24-TEIKISAGAKUACT-G
           END-IF
      *
      *    療養入院表示区分
           MOVE    SYS-5000-RYOYODSPKBN  TO  WRK-CMB-CD
           PERFORM 4200-RYOYODSPLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-RYOYODSP-G
           ELSE
               MOVE    SPA-GMN-W24-RYOYODSPLST (1)
                                       TO  SPA-GMN-W24-RYOYODSP-G
           END-IF
      *
      *    入院料加算情報
           PERFORM 3101-INPUT-AREA-KSN-SET-SEC
      *
      *    地域加算情報
           IF       (( SYS-5000-STYUKYMD   <   CONST-H180401 )
            AND      ( SPA-W01-EDYUKYMD    >=  CONST-H180401 ))
               MOVE    SPACE       TO  SPA-GMN-W24-CHIIKIKSN-G
           ELSE
               IF    ( SYS-5000-CHIIKIKSN  NOT =   SPACE )
                   PERFORM VARYING IDX0 FROM    1   BY  1
                           UNTIL ( IDX0 >   SPA-GMN-W24-CHIIKIKSN-MAX )
      *
                       IF    ( SYS-5000-STYUKYMD   >=  CONST-H180401 )
                           IF    ( CONST-CHIIKIKSN-H1804 (IDX0)
                                           =   SYS-5000-CHIIKIKSN )
                               MOVE    SPA-GMN-W24-CHIIKIKSNLST (IDX0)
                                   TO  SPA-GMN-W24-CHIIKIKSN-G
                           END-IF
                       ELSE
                           IF    ( CONST-CHIIKIKSN-SRYCD (IDX0)
                                           =   SYS-5000-CHIIKIKSN )
                               MOVE    SPA-GMN-W24-CHIIKIKSNLST (IDX0)
                                   TO  SPA-GMN-W24-CHIIKIKSN-G
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-IF
      *
      *    入院会計テーブル表示情報
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-SRYACT-MAX )
               MOVE    SYS-5000-DSPCD (IDX0)
                                       TO  SPA-GMN-W24-SRYCD  (IDX0)
                                       WRK-SRYCD
               IF    ( WRK-SRYCD   NOT =   SPACE )
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME    TO  SPA-GMN-W24-SRYACT (IDX0)
               END-IF
      *
           END-PERFORM
      *
      *    選定療養費−負担区分
           MOVE    SYS-5000-SENTEI-FTNKBN  TO  WRK-CMB-CD
           PERFORM 4200-SENTEI-FTNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SENTEI-FTNKBN-G
           ELSE
               MOVE    SPA-GMN-W24-SENTEI-FTNKBNLST (1)
                                       TO  SPA-GMN-W24-SENTEI-FTNKBN-G
           END-IF
      *
      *    選定療養費−消費税区分
           MOVE    SYS-5000-SENTEI-TAXKBN  TO  WRK-CMB-CD
           PERFORM 4200-SENTEI-TAXLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SENTEI-TAX-G
           ELSE
               MOVE    SPA-GMN-W24-SENTEI-TAXLST (1)
                                       TO  SPA-GMN-W24-SENTEI-TAX-G
           END-IF
      *
      *    選定療養費−点数単価（金額）
           EVALUATE    SYS-5000-SENTEI-FTNKBN
           WHEN    SPACE
               MOVE    CONST-SENTEI-TANKA
                                       TO  SPA-GMN-W24-SENTEI-TANKA
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKARJ
           WHEN    "1"
               IF    ( SYS-5000-SENTEI-TANKA-X     IS  NUMERIC )
                   MOVE    SYS-5000-SENTEI-TANKA
                                       TO  SPA-GMN-W24-SENTEI-TANKA
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W24-SENTEI-TANKA
               END-IF
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKARJ
           WHEN    "2"
               IF    ( SYS-5000-SENTEI-KINGAKU-X   IS  NUMERIC )
                   MOVE    SYS-5000-SENTEI-KINGAKU
                                       TO  SPA-GMN-W24-SENTEI-TANKA
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W24-SENTEI-TANKA
               END-IF
               IF    ( SYS-5000-SENTEI-IPN-KINGAKU-X   IS  NUMERIC )
                   MOVE    SYS-5000-SENTEI-IPN-KINGAKU
                                       TO  SPA-GMN-W24-SENTEI-TANKAIP
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W24-SENTEI-TANKAIP
               END-IF
               IF    ( SYS-5000-SENTEI-RJN-KINGAKU-X   IS  NUMERIC )
                   MOVE    SYS-5000-SENTEI-RJN-KINGAKU
                                       TO  SPA-GMN-W24-SENTEI-TANKARJ
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-W24-SENTEI-TANKARJ
               END-IF
           WHEN    OTHER
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKA
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKARJ
               MOVE    SPACE           TO  SPA-GMN-W24-SENTEI-TAX-G
           END-EVALUATE
      *
      *    入院歴表示区分
           MOVE    SYS-5000-REKI-DISPKBN   TO  WRK-CMB-CD
           PERFORM 4200-REKI-DISPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-REKI-DISPKBN-G
           ELSE
               MOVE    SPA-GMN-W24-REKI-DISPKBNLST (1)
                                       TO  SPA-GMN-W24-REKI-DISPKBN-G
           END-IF
      *
      *    退院再計算区分
           MOVE    SYS-5000-SAIKEISANKBN   TO  WRK-CMB-CD
           PERFORM 4200-SAIKEISANKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SAIKEISANKBN-G
           ELSE
               MOVE    SPA-GMN-W24-SAIKEISANKBNLST (1)
                                       TO  SPA-GMN-W24-SAIKEISANKBN-G
           END-IF
      *
      *    入金の取扱い
           MOVE    SYS-5000-NYKNKNRKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYKNKNRKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NYKNKNRKBN-G
           ELSE
               MOVE    SPA-GMN-W24-NYKNKNRKBNLST (1)
                                       TO  SPA-GMN-W24-NYKNKNRKBN-G
           END-IF
      *
      *    請求書発行発行区分
           MOVE    SYS-5000-SKYPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-SKYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-W24-SKYPRTKBNLST (2)
                                           TO  SPA-GMN-W24-SKYPRTKBN-G
           END-IF
      *
      *    請求書未収額印刷区分
           MOVE    SYS-5000-SKYMISYUPRTKBN TO  WRK-CMB-CD
           PERFORM 4200-MISYUPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-MISYUPRTKBN-G
           ELSE
               MOVE    SPA-GMN-W24-MISYUPRTKBNLST (1)
                                       TO  SPA-GMN-W24-MISYUPRTKBN-G
           END-IF
      *
      *    診療費明細書発行区分
           MOVE    SYS-5000-SRYPRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-SRYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-SRYPRTKBN-G
           ELSE
               MOVE    SPA-GMN-W24-SRYPRTKBNLST (1)
                                           TO  SPA-GMN-W24-SRYPRTKBN-G
           END-IF
      *
      *    退院証明書発行区分
           MOVE    SYS-5000-TAIPRTKBN  TO  WRK-CMB-CD
           PERFORM 4200-TAIPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-TAIPRTKBN-G
           ELSE
               MOVE    SPA-GMN-W24-TAIPRTKBNLST (2)
                                           TO  SPA-GMN-W24-TAIPRTKBN-G
           END-IF
      *
      *    退院証明書記載区分
           MOVE    SYS-5000-TAIKSIKBN  TO  WRK-CMB-CD
           PERFORM 4200-TAIKSIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-TAIKSIKBN-G
           ELSE
               MOVE    SPA-GMN-W24-TAIKSIKBNLST (1)
                                           TO  SPA-GMN-W24-TAIKSIKBN-G
           END-IF
      *!!
      *    退院時院外処方せん発行区分
           MOVE    SYS-5000-SHOHOUKBN   TO  WRK-CMB-CD
           PERFORM 4200-SHOHOUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-W24-SHOHOUKBN-G
           ELSE
               MOVE    SPA-GMN-W24-SHOHOUKBNLST (1)
                                           TO  SPA-GMN-W24-SHOHOUKBN-G
           END-IF
      *!!
      *
      *    減算情報
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               CONTINUE
           ELSE
               PERFORM 3101-GENZAN-EDIT-SEC
           END-IF
           .
       310-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    加算設定処理
      *****************************************************************
       3101-INPUT-AREA-KSN-SET-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-NYUKSN-MAX )
      *
               MOVE    "F"     TO  SPA-GMN-W24-CHK-NYUKSN (IDX0)
      *
               EVALUATE    SPA-NAI-W24-NYUKSN-SRYCD (IDX0)
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   PERFORM VARYING IDX-SYS FROM    1   BY  1
                           UNTIL ( IDX-SYS >   CONST-NYUKSN-MAX )
      *
                       IF    ( SPA-NAI-W24-NYUKSN-SRYCD (IDX0)
                                           =   SYS-5000-KSNCD (IDX-SYS))
                           IF    ( SYS-5000-KSNKBN (IDX-SYS)
                                           =   "2" )
                               MOVE    "T"
                                   TO   SPA-GMN-W24-CHK-NYUKSN (IDX0)
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
       3101-INPUT-AREA-KSN-SET-EXT.
           EXIT.
      *****************************************************************
      *    減算情報編集処理
      *****************************************************************
       3101-GENZAN-EDIT-SEC            SECTION.
      *
           IF    ( SPA-W01-EDYUKYMD   >=   CONST-H160401 )
               MOVE    16          TO  IDX0
           ELSE
               MOVE    14          TO  IDX0
           END-IF
      *
           MOVE    2               TO  IDX1
      *
           PERFORM UNTIL ( IDX1    >   4 )
      *
               IF    ( SYS-5000-GENKBN (IDX1)  =   "2" )
                   MOVE    "T"     TO  SPA-GMN-W24-CHK-NYUKSN (IDX0)
               END-IF
      *
               COMPUTE IDX0    =   IDX0    +   1
               COMPUTE IDX1    =   IDX1    +   1
      *
           END-PERFORM
      *
           .
       3101-GENZAN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関入院基本情報検索処理
      *****************************************************************
       310-SYS-5000-SEL-SEC        SECTION.
      *
      *    医療機関入院基本情報検索
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    MCPDATA-REC         TO  SYS-5000-REC
      *
           .
      *
       310-SYS-5000-SEL-EXT.
           EXIT.
      *****************************************************************
      *    医療機関入院基本情報検索処理（期間切り替え時）
      *****************************************************************
       311-SYS-5000-SEL-SEC        SECTION.
      *
      *    医療機関入院基本情報検索
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           MOVE    MCPDATA-REC         TO  SYS-5000-REC
           .
      *
       311-SYS-5000-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-W25SPASET-SEC           SECTION.
      *
           IF    ( SPA-W25-SRYCD   NOT =   SPACE )
               MOVE    SPA-GMN-W24-MOTOIDX
                                       TO  IDXZ
               MOVE    SPA-W25-SRYCD   TO  SPA-GMN-W24-SRYCD (IDXZ)
               PERFORM 4101-SRYCD-CHK-SEC
           ELSE
               COMPUTE SPA-GMN-W24-CUR =   SPA-GMN-W24-MOTOIDX +   28
           END-IF 
           .
       320-W25SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
                   PERFORM 481-TOUROKU-SEC
               WHEN "3002"
                   PERFORM 431-DEL-SEC
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
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-DEL-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
      *    入院加算情報
           WHEN    "CLICKED"       ALSO    "CHK_NYUKSN01"
                                   THRU    "CHK_NYUKSN20"
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
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
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
           MOVE    ZERO        TO      SPA-GMN-W24-CUR
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
      *    入院時食事療養
           MOVE    W24-SKJRYOYO        TO  SPA-GMN-W24-SKJRYOYO-G
      *
      *    食事診療行為
           MOVE    W24-SYOKUJI (1)     TO  SPA-GMN-W24-SKJSRY-OCC (1)
      *
      *    食事診療行為
           MOVE    W24-SYOKUJI (2)     TO  SPA-GMN-W24-SKJSRY-OCC (2)
      *
      *    室料差額消費税区分
           MOVE    W24-BRMSGK-TAX      TO  SPA-GMN-W24-BRMSGK-TAX-G
      *    入院会計室料差額区分
           MOVE    W24-NACCT-BRMSGK    TO  SPA-GMN-W24-NACCT-BRMSGK-G
      *    入院会計外泊区分
           MOVE    W24-NACCT-GAIHAKU   TO  SPA-GMN-W24-NACCT-GAIHAKU-G
      *
      *    入院加算情報
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   20 )
               MOVE    W24-CHK-NYUKSN (IDX1)
                                   TO  SPA-GMN-W24-CHK-NYUKSN (IDX1)
           END-PERFORM
      *
      *    入院会計表示情報
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL     ( IDX1    >   10 )
               MOVE    W24-SRYCD (IDX1)
                                   TO  SPA-GMN-W24-SRYCD (IDX1)
           END-PERFORM
      *
      *    地域加算
           MOVE    W24-CHIIKIKSN       TO  SPA-GMN-W24-CHIIKIKSN-G
      *
      *    選定療養費−負担区分
           MOVE    W24-SENTEI-FTNKBN   TO  SPA-GMN-W24-SENTEI-FTNKBN-G
      *
      *    選定療養費−点数単価
           MOVE    W24-SENTEI-TANKA    TO  SPA-GMN-W24-SENTEI-TANKA
      *
      *    選定療養費−点数単価（一般）
           MOVE    W24-SENTEI-TANKAIP  TO  SPA-GMN-W24-SENTEI-TANKAIP
      *
      *    選定療養費−点数単価（老人）
           MOVE    W24-SENTEI-TANKARJ  TO  SPA-GMN-W24-SENTEI-TANKARJ
      *
      *    選定療養費−消費税
           MOVE    W24-SENTEI-TAX      TO  SPA-GMN-W24-SENTEI-TAX
      *
      *    入院歴表示区分
           MOVE    W24-REKI-DISPKBN    TO  SPA-GMN-W24-REKI-DISPKBN-G
      *
      *    退院再計算区分
           MOVE    W24-SAIKEISANKBN    TO  SPA-GMN-W24-SAIKEISANKBN-G
      *
      *    入金の取扱い
           MOVE    W24-NYKNKNRKBN      TO  SPA-GMN-W24-NYKNKNRKBN-G
      *
      *    請求書発行区分
           MOVE    W24-SKYPRTKBN       TO  SPA-GMN-W24-SKYPRTKBN-G
      *
      *    請求書未収額印刷区分
           MOVE    W24-SKYMISYUPRTKBN  TO  SPA-GMN-W24-MISYUPRTKBN-G
      *
      *    診療費明細書発行区分
           MOVE    W24-SRYPRTKBN       TO  SPA-GMN-W24-SRYPRTKBN-G
      *
      *    退院証明書発行区分
           MOVE    W24-TAIPRTKBN       TO  SPA-GMN-W24-TAIPRTKBN-G
      *
      *    退院証明書記載区分
           MOVE    W24-TAIKSIKBN       TO  SPA-GMN-W24-TAIKSIKBN-G
      *
      *    標欠減額区分
           MOVE    W24-HYOUKETU        TO  SPA-GMN-W24-HYOUKETU-G
      *
      *    定数超過区分
           MOVE    W24-TEISU           TO  SPA-GMN-W24-TEISU-G
      *
      *    ＡＤＬ区分
           MOVE    W24-ADLINPUT        TO  SPA-GMN-W24-ADLINPUT-G
      *
      *    定期請求差額初期表示区分
           MOVE    W24-TEIKISAGAKUACT  TO  SPA-GMN-W24-TEIKISAGAKUACT-G
      *
      *    療養入院表示区分
           MOVE    W24-RYOYODSP        TO  SPA-GMN-W24-RYOYODSP-G
      *
      *    退院時院外処方せん区分
           MOVE    W24-SHOHOUKBN       TO  SPA-GMN-W24-SHOHOUKBN-G
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
      *    入院時食事療養チェック処理
           PERFORM 4101-SKJRYOYO-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    食事診療行為チェック処理
           PERFORM VARYING IDX-SKJ     FROM    1   BY  1
                   UNTIL ( IDX-SKJ     >   CONST-SKJSRY-MAX )
                    OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ) )
               PERFORM 4101-SKJSRY-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    室料差額消費税区分
           PERFORM 4101-BRMSGK-TAX-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院会計室料差額区分
           PERFORM 4101-NACCT-BRMSGK-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院会計外泊区分
           PERFORM 4101-NACCT-GAIHAKU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院加算情報チェック処理
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   20 )
               PERFORM 4101-CHK-NYUKSN-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    入院加算情報チェック処理-施設基準チェック処理
           PERFORM 4101-SSTKIJUN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    地域加算チェック処理
           PERFORM 4101-CHIIKIKSN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院会計表示情報チェック処理
           PERFORM VARYING IDXZ    FROM    1   BY  1
                   UNTIL ( IDXZ    >   10 )
               PERFORM 4101-SRYCD-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                 OR  ( FLG-END     NOT =   ZERO  )
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    選定療養費−負担区分
           PERFORM 4101-SENTEI-FTNKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価
           PERFORM 4101-SENTEI-TANKA-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価（一般）
           PERFORM 4101-SENTEI-TANKAIP-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−点数単価（老人）
           PERFORM 4101-SENTEI-TANKARJ-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    選定療養費−消費税
           PERFORM 4101-SENTEI-TAX-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入院歴表示区分チェック処理
           PERFORM 4101-REKI-DISPKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院再計算区分チェック処理
           PERFORM 4101-SAIKEISANKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    入金の取扱いチェック処理
           PERFORM 4101-NYKNKNRKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    請求書発行区分チェック処理
           PERFORM 4101-SKYPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    請求書未収額印刷区分チェック処理
           PERFORM 4101-MISYUPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    診療費明細書発行区分チェック処理
           PERFORM 4101-SRYPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院証明書発行区分チェック処理
           PERFORM 4100-TAIPRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    退院証明書記載区分チェック処理
           PERFORM 4100-TAIKSIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    標欠減額区分チェック処理
           PERFORM 4100-HYOUKETU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    定数超過区分チェック処理
           PERFORM 4100-TEISU-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    ＡＤＬ入力区分チェック処理
           PERFORM 4100-ADLINPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    定期請求差額初期表示区分チェック処理
           PERFORM 4100-TEIKISAGAKUACT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *!!
      *    退院時院外処方せん発行チェック処理
           PERFORM 4101-SHOHOUKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *!!
      *
      *    療養入院表示区分チェック処理
           PERFORM 4100-RYOYODSP-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
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
           MOVE    "W24"               TO  SPA-MOTOPG
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
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
           MOVE    IDXZ                TO  SPA-GMN-W24-MOTOIDX
           PERFORM 5011-INPUT-CUR-SEC
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-W25-AREA
           MOVE    "2"                 TO  SPA-W25-TYPE
      *
           INITIALIZE                  SPA-W25GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        診療行為で検索（オリジナルのＺ９８は内服）
               MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *        MOVE    "3"                 TO  SPA-W25-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-W25-INPUTCD
               MOVE    "1"                 TO  SPA-W25-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-W25-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-W25-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-W25-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-W25-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-W25-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-W25-SET-SEC
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-W25-GMN-MAX     =   ZERO )  AND
                      (SPA-W25-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-W25GMN-AREA
                   MOVE    "8"                 TO  SPA-W25-GMN-SYORI
      *            画面編集
                   PERFORM 41011-W25-SET-SEC
               END-IF
           ELSE
      *        画面編集
               PERFORM 41011-W25-SET-SEC
           END-IF
      *
           MOVE    "W24"               TO  SPA-MOTOPG
           MOVE    "W25"               TO  SPA-SAKIPG
           MOVE    "W24"               TO  SPA-W25-MOTOPG
      *
           IF      SPA-W25-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "W25    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-W25-SET-SEC                           SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-W25-GMN-AREA    TO  SPA-W25-KYOUTU
           MOVE    SPA-W24FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW25"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W24FREE
           MOVE    SPA-W25-KYOUTU      TO  SPA-W25-GMN-AREA
      *
           .
       41011-W25-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 4201-INPUT-AREA-INIT-SEC
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DEL-SEC                 SECTION.
      *
           MOVE    "3002"          TO  SPA-WIDCD
      *
           .
       430-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       431-DEL-SEC                 SECTION.
      *
           INITIALIZE                      SYS-5000-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
           MOVE    "5000"              TO  SYS-5000-KANRICD
           MOVE    "*"                 TO  SYS-5000-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5000-EDYUKYMD
           MOVE    SYS-5000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           ELSE
               MOVE    "1202"          TO  SPA-ERRCD
           END-IF
      *
           .
       431-DEL-EXT.
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
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           IF    ( SPA-W01-SELNUM      >   ZERO )
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYS-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYS-EDYUKYMD
           ELSE
               MOVE    SPA-W01-STYUKYMD        TO  SYS-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-EDYUKYMD
           END-IF
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
      *
               MOVE    SYSKANRI-REC        TO  SYS-5000-REC
      *
               MOVE    SYS-5000-STYUKYMD   TO  WRK-STYUKYMD
               MOVE    SYS-5000-EDYUKYMD   TO  WRK-EDYUKYMD
               MOVE    SYS-5000-CREYMD     TO  WRK-CREYMD
               PERFORM 4811-HOSP-NYUINKHN-EDIT-SEC
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    WRK-CREYMD          TO  SYS-5000-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-5000-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-5000-UPHMS
      *
               MOVE    SYS-5000-REC        TO  SYSKANRI-REC
               MOVE    SYS-5000-HOSPNUM    TO  SYSUP-HOSPNUM
               MOVE    SYS-5000-KANRICD    TO  SYSUP-KANRICD
               MOVE    SYS-5000-KBNCD      TO  SYSUP-KBNCD
               MOVE    WRK-STYUKYMD        TO  SYSUP-STYUKYMD
               MOVE    WRK-EDYUKYMD        TO  SYSUP-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
           ELSE
               PERFORM 4811-HOSP-NYUINKHN-EDIT-SEC
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYS-5000-CREYMD
                                               SYS-5000-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-5000-UPHMS
      *
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
           END-IF
      *
           IF    ( MCP-RC          =   ZERO )
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               PERFORM 210-BACK
           ELSE
               MOVE    "1201"          TO  SPA-ERRCD
           END-IF
      *
      *
           .
       481-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    医療機関入院基本情報編集処理
      *****************************************************************
       4811-HOSP-NYUINKHN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  SYS-5000-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5000-HOSPNUM
      *
      *    管理コード
           MOVE    "5000"              TO  SYS-5000-KANRICD
      *    区分コード
           MOVE    "*"                 TO  SYS-5000-KBNCD
      *    有効開始年月日
           MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-STYUKYMD
      *    有効終了年月日
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-5000-EDYUKYMD
      *    食事療養費
           EVALUATE    SPA-GMN-W24-SKJRYOYO
           WHEN    "1"
               MOVE    CONST-SHOKUJIRYOYOCD (1)
                                       TO  SYS-5000-SHOKUJIRYOYOCD
           WHEN    "2"
               MOVE    CONST-SHOKUJIRYOYOCD (2)
                                       TO  SYS-5000-SHOKUJIRYOYOCD
           END-EVALUATE
      *
      *    特別管理加算
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               CONTINUE
           ELSE
               PERFORM VARYING IDX-SKJ FROM    1   BY  1
                       UNTIL ( IDX-SKJ >   CONST-SKJSRY-MAX )
                        OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                                       =   SPACE )
      *
                   IF    ( SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                                   =   "197000270" )
                       MOVE    SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                                       TO  SYS-5000-SHOKUCD  (1)
                       MOVE    SPA-GMN-W24-SKJSRY (IDX-SKJ)
                                       TO  SYS-5000-SHOKUKBN (1)
                   END-IF
               END-PERFORM
           END-IF
      *
      *    食堂加算
           PERFORM VARYING IDX-SKJ FROM    1   BY  1
                   UNTIL ( IDX-SKJ >   CONST-SKJSRY-MAX )
                    OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                                       =   SPACE )
      *
               IF    ( SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                               =   "197000570" )
                   MOVE    SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
                                   TO  SYS-5000-SHOKUCD  (2)
                   MOVE    SPA-GMN-W24-SKJSRY (IDX-SKJ)
                                   TO  SYS-5000-SHOKUKBN (2)
               END-IF
           END-PERFORM
      *
      *    室料差額消費税区分
           MOVE    SPA-GMN-W24-BRMSGK-TAX
                                   TO  SYS-5000-BRMSGK-TAXKBN
      *
      *    入院会計室料差額区分
           MOVE    SPA-GMN-W24-NACCT-BRMSGK
                                   TO  SYS-5000-NACCT-BRMSGKKBN
      *
      *    入院会計外泊区分
           MOVE    SPA-GMN-W24-NACCT-GAIHAKU
                                   TO  SYS-5000-NACCT-GAIHAKUKBN
      *
           EVALUATE    TRUE
           WHEN  ( SPA-W01-EDYUKYMD   >=   CONST-H220401 )
               MOVE    15          TO  WRK-MAX
           WHEN  ( SPA-W01-EDYUKYMD   >=   CONST-H180401 )
               MOVE    13          TO  WRK-MAX
           WHEN  ( SPA-W01-EDYUKYMD   >=   CONST-H160401 )
               MOVE    15          TO  WRK-MAX
           WHEN    OTHER
               MOVE    13          TO  WRK-MAX
           END-EVALUATE
      *
      *    入院料加算情報
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WRK-MAX )
               MOVE    SPA-NAI-W24-NYUKSN-SRYCD (IDX1)
                                   TO  SYS-5000-KSNCD (IDX1)
               IF    ( SPA-GMN-W24-CHK-NYUKSN (IDX1)
                                   =   "F" )
                   MOVE    "1"     TO  SYS-5000-KSNKBN (IDX1)
               ELSE
                   MOVE    "2"     TO  SYS-5000-KSNKBN (IDX1)
               END-IF
           END-PERFORM
      *
      *    地域加算情報
           IF    ( SPA-GMN-W24-CHIIKIKSN   >=  "1")
            AND  ( SPA-GMN-W24-CHIIKIKSN   <=  "6" )
               MOVE    SPA-GMN-W24-CHIIKIKSN   TO  WRK-CHIIKIKSN-G
               IF    ( SYS-5000-STYUKYMD   >=  CONST-H180401 )
                   MOVE    CONST-CHIIKIKSN-H1804 (WRK-CHIIKIKSN)
                                   TO  SYS-5000-CHIIKIKSN
               ELSE
                   MOVE    CONST-CHIIKIKSN-SRYCD (WRK-CHIIKIKSN)
                                   TO  SYS-5000-CHIIKIKSN
               END-IF
           END-IF
      *
      *    入院会計テーブル表示情報
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    10 )
               MOVE    SPA-GMN-W24-SRYCD (IDX1)
                                   TO  SYS-5000-DSPCD (IDX1)
           END-PERFORM
      *
      *    選定入院負担区分
           MOVE    SPA-GMN-W24-SENTEI-FTNKBN
                                   TO  SYS-5000-SENTEI-FTNKBN
      *
      *    選定入院消費税区分
           MOVE    SPA-GMN-W24-SENTEI-TAX
                                   TO  SYS-5000-SENTEI-TAXKBN
      *    選定入院単価（金額）
           EVALUATE    SPA-GMN-W24-SENTEI-FTNKBN
           WHEN    "1"
               MOVE    SPA-GMN-W24-SENTEI-TANKA
                                   TO  SYS-5000-SENTEI-TANKA
               MOVE    ZERO        TO  SYS-5000-SENTEI-KINGAKU
           WHEN    "2"
               MOVE    ZERO        TO  SYS-5000-SENTEI-TANKA
               MOVE    SPA-GMN-W24-SENTEI-TANKA
                                   TO  SYS-5000-SENTEI-KINGAKU
               MOVE    SPA-GMN-W24-SENTEI-TANKAIP
                                   TO  SYS-5000-SENTEI-IPN-KINGAKU
               MOVE    SPA-GMN-W24-SENTEI-TANKARJ
                                   TO  SYS-5000-SENTEI-RJN-KINGAKU
           WHEN    OTHER
               MOVE    ZERO        TO  SYS-5000-SENTEI-KINGAKU
               MOVE    ZERO        TO  SYS-5000-SENTEI-IPN-KINGAKU
               MOVE    ZERO        TO  SYS-5000-SENTEI-RJN-KINGAKU
           END-EVALUATE
      *
      *    入院歴表示区分
           MOVE    SPA-GMN-W24-REKI-DISPKBN
                                   TO  SYS-5000-REKI-DISPKBN
      *    退院再計算区分
           MOVE    SPA-GMN-W24-SAIKEISANKBN
                                   TO  SYS-5000-SAIKEISANKBN
      *    入金の取扱い
           MOVE    SPA-GMN-W24-NYKNKNRKBN
                                   TO  SYS-5000-NYKNKNRKBN
      *
      *    請求書発行区分
           MOVE    SPA-GMN-W24-SKYPRTKBN
                                   TO  SYS-5000-SKYPRTKBN
      *
      *    請求書未収額印刷区分
           MOVE    SPA-GMN-W24-MISYUPRTKBN
                                   TO  SYS-5000-SKYMISYUPRTKBN
      *
      *    診療費明細書発行区分
           MOVE    SPA-GMN-W24-SRYPRTKBN
                                   TO  SYS-5000-SRYPRTKBN
      *
      *    退院証明書発行区分
           MOVE    SPA-GMN-W24-TAIPRTKBN
                                   TO  SYS-5000-TAIPRTKBN
      *
      *    退院証明書記載区分
           MOVE    SPA-GMN-W24-TAIKSIKBN
                                   TO  SYS-5000-TAIKSIKBN
      *
      *    標欠減額区分
           MOVE    SPA-GMN-W24-HYOUKETU
                                   TO  SYS-5000-HYOUKETUKBN
      *
      *    定数超過区分
           MOVE    SPA-GMN-W24-TEISU
                                   TO  SYS-5000-TEISUKBN
      *
      *    ＡＤＬ入力区分
           MOVE    SPA-GMN-W24-ADLINPUT
                                   TO  SYS-5000-ADLINPUTKBN
      *
      *    定期請求差額初期表示区分
           MOVE    SPA-GMN-W24-TEIKISAGAKUACT
                                   TO  SYS-5000-TEIKISAGAKUACTKBN
      *
      *    療養入院表示区分
           MOVE    SPA-GMN-W24-RYOYODSP
                                   TO  SYS-5000-RYOYODSPKBN
      *
      *    減算情報
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               CONTINUE
           ELSE
               PERFORM 48111-GENZAN-EDIT-SEC
           END-IF
      *
      *    退院時院外処方せん発行区分
           MOVE    SPA-GMN-W24-SHOHOUKBN
                                   TO  SYS-5000-SHOHOUKBN
      *
           .
      *
       4811-HOSP-NYUINKHN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    減算情報編集処理
      *****************************************************************
       48111-GENZAN-EDIT-SEC           SECTION.
      *
           IF    ( SPA-W01-EDYUKYMD   >=   CONST-H160401 )
               MOVE    16          TO  IDX0
           ELSE
               MOVE    14          TO  IDX0
           END-IF
      *
           MOVE    2               TO  IDX1
      *
           PERFORM UNTIL ( IDX1    >   CONST-GEN-MAX )
      *
               MOVE    CONST-GENCD (IDX1)
                                   TO  SYS-5000-GENCD (IDX1)
      *
               IF    ( SPA-GMN-W24-CHK-NYUKSN (IDX0)   =   "T" )
                   MOVE    "2"     TO  SYS-5000-GENKBN (IDX1)
               ELSE
                   MOVE    "1"     TO  SYS-5000-GENKBN (IDX1)
               END-IF
      *
               COMPUTE IDX0    =   IDX0    +   1
               COMPUTE IDX1    =   IDX1    +   1
      *
           END-PERFORM
      *
           .
      *
       48111-GENZAN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養チェック処理
      *****************************************************************
       4101-SKJRYOYO-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W24-SKJRYOYO-G  =   SPACE )
               GO  TO  4101-SKJRYOYO-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-SKJRYOYO
                                   TO  WRK-CMB-CD
           PERFORM 4200-SKJRYOYOLST-CHK-SEC
           IF   (  WRK-CMB-ITM     =   SPACE )
               MOVE    1           TO  SPA-GMN-W24-CUR
               MOVE    "0005"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-SKJRYOYO
           END-IF
      *
           .
      *
       4101-SKJRYOYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事診療行為チェック処理
      *****************************************************************
       4101-SKJSRY-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W24-SKJSRY-OCC (IDX-SKJ)
                                       =   SPACE )
               MOVE   "1"          TO  WRK-CMB-CD
               PERFORM 4200-SKJSRYLST-CHK-SEC
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-SKJSRY-OCC (IDX-SKJ)
               GO  TO  4101-SKJSRY-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-SKJSRY (IDX-SKJ)
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKJSRYLST-CHK-SEC
           IF   (  WRK-CMB-ITM     =   SPACE )
      *
               COMPUTE SPA-GMN-W24-CUR =   IDX-SKJ +   1
      *
               EVALUATE    SPA-NAI-W24-SKJSRY-SRYCD (IDX-SKJ)
               WHEN    "197000270"
                   MOVE    "0006"      TO  SPA-ERRCD
               WHEN    "197000570"
                   MOVE    "0007"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "0001"      TO  SPA-ERRCD
               END-EVALUATE
      *
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-SKJSRY-OCC (IDX-SKJ)
           END-IF
      *
      *
           .
      *
       4101-SKJSRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額消費税区分
      *****************************************************************
       4101-BRMSGK-TAX-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-W24-BRMSGK-TAX
                                       TO  WRK-CMB-CD
           PERFORM 4200-BRMSGK-TAXLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-BRMSGK-TAXLST (1)
                                       TO  SPA-GMN-W24-BRMSGK-TAX-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-BRMSGK-TAX-G
           END-IF
      *
           .
       4101-BRMSGK-TAX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計室料差額区分
      *****************************************************************
       4101-NACCT-BRMSGK-CHK-SEC         SECTION.
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-NACCT-BRMSGK
                                       TO  WRK-CMB-CD
           PERFORM 4200-NACCT-BRMSGKLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-NACCT-BRMSGKLST (1)
                                       TO  SPA-GMN-W24-NACCT-BRMSGK-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NACCT-BRMSGK-G
           END-IF
      *
           .
       4101-NACCT-BRMSGK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計外泊区分
      *****************************************************************
       4101-NACCT-GAIHAKU-CHK-SEC         SECTION.
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-NACCT-GAIHAKU
                                       TO  WRK-CMB-CD
           PERFORM 4200-NACCT-GAIHAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-NACCT-GAIHAKULST (1)
                                       TO  SPA-GMN-W24-NACCT-GAIHAKU-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NACCT-GAIHAKU-G
           END-IF
      *
           .
       4101-NACCT-GAIHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算情報チェック処理
      *****************************************************************
       4101-CHK-NYUKSN-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W24-NYUKSNMEI (IDX1) =  SPACE )
               MOVE    "F"         TO  SPA-GMN-W24-CHK-NYUKSN (IDX1)
           END-IF
      *
           .
      *
       4101-CHK-NYUKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       4101-SSTKIJUN-CHK-SEC           SECTION.
      *
      *    施設基準検索処理
           PERFORM 4101-SYS-1006-SEL-SEC
      *
           MOVE    7               TO  WRK-BASE-CUR
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN         >   CONST-NYUKSN-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-CHK-NYUKSN (IDX-KSN)  =   "T" )
                   PERFORM 41011-SSTKIJUN-CHK-SEC
               END-IF
           END-PERFORM
      *
           .
       4101-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
      *
           EVALUATE    SPA-NAI-W24-NYUKSN-SRYCD (IDX-KSN)
           WHEN    "190098870"
      *        入院時医学管理料チェック
               IF  ( SSTKJN-CD (201)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190098970"
      *        紹介外来加算（特定機能病院以外）
               IF    ( SSTKJN-CD (202) =   1 )
                AND (( SSTKJN-CD (147) =   1 )
                 OR  ( SSTKJN-CD (148) =   1 )
                 OR  ( SSTKJN-CD (107) =   1 )
                 OR  ( SSTKJN-CD (108) =   1 ))
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190099170"
      *        紹介外来特別加算
               IF    ( SSTKJN-CD (202) =   1 )
                AND  ( SSTKJN-CD (203) =   1 )
                AND (( SSTKJN-CD (147) =   1 )
                 OR  ( SSTKJN-CD (148) =   1 )
                 OR  ( SSTKJN-CD (107) =   1 )
                 OR  ( SSTKJN-CD (108) =   1 ))
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190099270"
      *        急性期入院加算
               IF  ( SSTKJN-CD (323)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190099570"
      *        急性期特定入院加算
               IF  ( SSTKJN-CD (324)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
      *        地域医療支援病院入院診療加算
           WHEN    "190099870"
               IF  ( SSTKJN-CD (146)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190099970"
      *        地域医療支援病院入院診療加算２
               IF    ( SSTKJN-CD (146)   =   1 )
                AND  ( SSTKJN-CD (147)   =   1 )
                AND  ( SSTKJN-CD (156)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190117170"
           WHEN    "190119910"
      *        単独型及び管理型臨床研修病院入院診療加算（入院初日）
      *        協力型臨床研修病院入院診療加算（入院初日）
               IF    ( SSTKJN-CD (206)   =   1 )
                AND  ( SSTKJN-CD (364)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190100370"
      *        診療録管理体制加算
               IF    ( SSTKJN-CD (206)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190100470"
           WHEN    "190100770"
           WHEN    "190100670"
           WHEN    "190100970"
      *        乳幼児加算（病院）
      *        幼児加算（病院）
      *        乳幼児加算（診療所）
      *        幼児加算（診療所）
               CONTINUE
           WHEN    "190107170"
      *        精神病棟入院時医学管理加算
               IF    ( SSTKJN-CD (207)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190117270"
      *        離島加算
                   CONTINUE
           WHEN    "190120510"
      *        医療安全対策加算（入院初日）
               IF    ( SSTKJN-CD (605)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           WHEN    "190120410"
      *        栄養管理実施加算
               IF    ( SSTKJN-CD (604)   =   1 )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-SSTKIJUNERR
               END-IF
           END-EVALUATE
      *
           IF    ( FLG-SSTKIJUNERR     =   1 )
               COMPUTE SPA-GMN-W24-CUR
                   =   WRK-BASE-CUR    +   IDX-KSN
               MOVE    "0004"  TO  SPA-ERRCD
               MOVE    "F"     TO  SPA-GMN-W24-CHK-NYUKSN (IDX-KSN)
           END-IF
      *
           .
       41011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準検索処理
      *****************************************************************
       4101-SYS-1006-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  SSTKJN-AREA
      *
      *    施設基準情報検索
           INITIALIZE  SYSKANRI-REC
           MOVE    "1006"          TO  SYS-KANRICD
           MOVE    SPA-W01-EDYUKYMD
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
       4101-SYS-1006-SEL-EXT.
           EXIT.
      *****************************************************************
      *    地域加算チェック処理
      *****************************************************************
       4101-CHIIKIKSN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-W24-CHIIKIKSN-G =   SPACE )
               GO  TO  4101-CHIIKIKSN-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-CHIIKIKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-CHIIKIKSNLST-CHK-SEC
           IF   (  WRK-CMB-ITM     =   SPACE )
               MOVE    28          TO  SPA-GMN-W24-CUR
               MOVE    "0008"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W24-CHIIKIKSN-G
           END-IF
      *
           .
      *
       4101-CHIIKIKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計表示情報チェック処理
      *****************************************************************
       4101-SRYCD-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-W24-SRYCD (IDXZ)    =   SPACE )
               MOVE    SPACE       TO  SPA-GMN-W24-SRYACT (IDXZ)
               GO  TO  4101-SRYCD-CHK-EXT
           END-IF
      *
      *    "/" 入力の時、検索画面へ
           IF    ( SPA-GMN-W24-SRYCD (IDXZ)(1 : 1)   =  "/" )
               MOVE    SPACE           TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
               GO  TO  4101-SRYCD-CHK-EXT
           END-IF
      *
      *    入力コードチェック
           PERFORM 4010-SRYCD-CHK-SEC
           IF    ( FLG-KENSAKU         =   1 )
               GO  TO  4101-SRYCD-CHK-EXT
           END-IF
      *
           EVALUATE  SPA-GMN-W24-SRYCD (IDXZ) (1 : 1)
           WHEN  "0"
               IF  ( SPA-GMN-W24-SRYCD (IDXZ)(2 : 2) = "00" )
                   MOVE    "0003"    TO  SPA-ERRCD
                   GO  TO  4101-SRYCD-CHK-EXT
               END-IF
           WHEN  "1"
           WHEN  "6"
           WHEN  "7"
           WHEN  "8"
               CONTINUE
           WHEN  OTHER
               MOVE    "0003"      TO  SPA-ERRCD
               GO  TO  4101-SRYCD-CHK-EXT
           END-EVALUATE
      *
      *    点数マスタ該当データ読み出し
           MOVE    SPA-GMN-W24-SRYCD (IDXZ)
                                   TO  WRK-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               COMPUTE SPA-GMN-W24-CUR =   IDXZ    +   28
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           MOVE    TNS-NAME        TO  SPA-GMN-W24-SRYACT (IDXZ)
      *
           .
      *
       4101-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *  入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                        SECTION.
      *
           MOVE    ZERO               TO  FLG-KENSAKU
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-W24-SRYCD (IDXZ)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO
                                       SPA-GMN-W24-SRYCD (IDXZ)
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   MOVE    SPACE   TO   SPA-GMN-W24-SRYCD (IDXZ)(9:)
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9  )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (SPA-GMN-W24-SRYCD (IDXZ) (1 : 9 )
                                       IS  NUMERIC  )
               GO      TO    4010-SRYCD-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-KENSAKU
           MOVE    SPA-GMN-W24-SRYCD (IDXZ)
                                       TO  WRK-INPUTCD
           PERFORM 409-KENSAKU-SEC
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−負担区分
      *****************************************************************
       4101-SENTEI-FTNKBN-CHK-SEC         SECTION.
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-SENTEI-FTNKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SENTEI-FTNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SENTEI-FTNKBNLST (1)
                                       TO  SPA-GMN-W24-SENTEI-FTNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SENTEI-FTNKBN-G
           END-IF
      *
           EVALUATE    SPA-GMN-W24-SENTEI-FTNKBN
           WHEN    "1"
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKARJ
           WHEN    "2"
               CONTINUE
           WHEN    OTHER
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKA
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKAIP
               MOVE    ZERO            TO  SPA-GMN-W24-SENTEI-TANKARJ
               MOVE    SPACE           TO  SPA-GMN-W24-SENTEI-TAX-G
           END-EVALUATE
      *
           .
       4101-SENTEI-FTNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価
      *****************************************************************
       4101-SENTEI-TANKA-CHK-SEC       SECTION.
      *
      *
           .
       4101-SENTEI-TANKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価（一般）
      *****************************************************************
       4101-SENTEI-TANKAIP-CHK-SEC     SECTION.
      *
      *
           .
       4101-SENTEI-TANKAIP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−点数単価（老人）
      *****************************************************************
       4101-SENTEI-TANKARJ-CHK-SEC     SECTION.
      *
      *
           .
       4101-SENTEI-TANKARJ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−消費税
      *****************************************************************
       4101-SENTEI-TAX-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W24-SENTEI-TAX-G =   SPACE )
               EVALUATE    SPA-GMN-W24-SENTEI-FTNKBN
               WHEN    "1"
               WHEN    "2"
                   MOVE    SPA-GMN-W24-SENTEI-TAXLST (1)
                                       TO  SPA-GMN-W24-SENTEI-TAX-G
               WHEN    OTHER
                   CONTINUE
               END-EVALUATE
               GO  TO  4101-SENTEI-TAX-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W24-SENTEI-TAX
                                       TO  WRK-CMB-CD
           PERFORM 4200-SENTEI-TAXLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SENTEI-TAXLST (1)
                                       TO  SPA-GMN-W24-SENTEI-TAX-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SENTEI-TAX-G
           END-IF
      *
           .
       4101-SENTEI-TAX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院歴表示チェック処理
      *****************************************************************
       4101-REKI-DISPKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-REKI-DISPKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-REKI-DISPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-REKI-DISPKBNLST (1)
                                       TO  SPA-GMN-W24-REKI-DISPKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-REKI-DISPKBN-G
           END-IF
      *
           .
      *
       4101-REKI-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院再計算区分チェック処理
      *****************************************************************
       4101-SAIKEISANKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-SAIKEISANKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SAIKEISANKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SAIKEISANKBNLST (1)
                                       TO  SPA-GMN-W24-SAIKEISANKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SAIKEISANKBN-G
           END-IF
      *
           .
      *
       4101-SAIKEISANKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱いチェック処理
      *****************************************************************
       4101-NYKNKNRKBN-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-W24-NYKNKNRKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYKNKNRKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-NYKNKNRKBNLST (1)
                                       TO  SPA-GMN-W24-NYKNKNRKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-NYKNKNRKBN-G
           END-IF
      *
           .
      *
       4101-NYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分チェック処理
      *****************************************************************
       4101-SKYPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-SKYPRTKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SKYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SKYPRTKBNLST (2)
                                       TO  SPA-GMN-W24-SKYPRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SKYPRTKBN-G
           END-IF
      *
           .
      *
       4101-SKYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分チェック処理
      *****************************************************************
       4101-SRYPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-SRYPRTKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SRYPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SRYPRTKBNLST (1)
                                       TO  SPA-GMN-W24-SRYPRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SRYPRTKBN-G
           END-IF
      *
           .
      *
       4101-SRYPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院時院外処方せん発行区分チェック処理
      *****************************************************************
       4101-SHOHOUKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-SHOHOUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SHOHOUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-SHOHOUKBNLST (1)
                                       TO  SPA-GMN-W24-SHOHOUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-SHOHOUKBN-G
           END-IF
      *
           .
      *
       4101-SHOHOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書未収額印刷区分チェック処理
      *****************************************************************
       4101-MISYUPRTKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W24-MISYUPRTKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-MISYUPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-MISYUPRTKBNLST (1)
                                       TO  SPA-GMN-W24-MISYUPRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-MISYUPRTKBN-G
           END-IF
      *
           .
      *
       4101-MISYUPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書印刷区分チェック処理
      *****************************************************************
       4100-TAIPRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-TAIPRTKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TAIPRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-TAIPRTKBNLST (2)
                                       TO  SPA-GMN-W24-TAIPRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TAIPRTKBN-G
           END-IF
      *
           .
      *
       4100-TAIPRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書記載区分チェック処理
      *****************************************************************
       4100-TAIKSIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-TAIKSIKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TAIKSIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-TAIKSIKBNLST (1)
                                       TO  SPA-GMN-W24-TAIKSIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TAIKSIKBN-G
           END-IF
      *
           .
      *
       4100-TAIKSIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    標欠減額区分チェック処理
      *****************************************************************
       4100-HYOUKETU-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-HYOUKETU
                                       TO  WRK-CMB-CD
           PERFORM 4200-HYOUKETULST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-HYOUKETULST (1)
                                       TO  SPA-GMN-W24-HYOUKETU-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-HYOUKETU-G
           END-IF
      *
           .
      *
       4100-HYOUKETU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定数超過区分チェック処理
      *****************************************************************
       4100-TEISU-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-TEISU
                                       TO  WRK-CMB-CD
           PERFORM 4200-TEISULST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-TEISULST (1)
                                       TO  SPA-GMN-W24-TEISU-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TEISU-G
           END-IF
      *
           .
      *
       4100-TEISU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ入力区分チェック処理
      *****************************************************************
       4100-ADLINPUT-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-ADLINPUT
                                       TO  WRK-CMB-CD
           PERFORM 4200-ADLINPUTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-ADLINPUTLST (2)
                                       TO  SPA-GMN-W24-ADLINPUT-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-ADLINPUT-G
           END-IF
      *
           .
      *
       4100-ADLINPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求差額初期表示区分チェック処理
      *****************************************************************
       4100-TEIKISAGAKUACT-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-TEIKISAGAKUACT
                                       TO  WRK-CMB-CD
           PERFORM 4200-TEIKISAGAKUACTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-TEIKISAGAKUACTLST (1)
                                       TO  SPA-GMN-W24-TEIKISAGAKUACT-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-TEIKISAGAKUACT-G
           END-IF
      *
           .
      *
       4100-TEIKISAGAKUACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養入院表示区分チェック処理
      *****************************************************************
       4100-RYOYODSP-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W24-RYOYODSP
                                       TO  WRK-CMB-CD
           PERFORM 4200-RYOYODSPLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W24-RYOYODSPLST (1)
                                       TO  SPA-GMN-W24-RYOYODSP-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W24-RYOYODSP-G
           END-IF
      *
           IF   SPA-GMN-W24-RYOYODSP    =   "1"
               IF  (SPA-GMN-W24-HYOUKETU = "1" OR "2" OR "3" OR "4") OR
                   (SPA-GMN-W24-TEISU    = "1")      
                   MOVE    59          TO  SPA-GMN-W24-CUR
                   MOVE    "1010"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4100-RYOYODSP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時食事療養コンボチェック処理
      *****************************************************************
       4200-SKJRYOYOLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SKJRYOYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SKJRYOYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SKJRYOYOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SKJRYOYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食事診療行為コンボチェック処理
      *****************************************************************
       4200-SKJSRYLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SKJSRY-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SKJSRYL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SKJSRYLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SKJSRYLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地域加算コンボチェック処理
      *****************************************************************
       4200-CHIIKIKSNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-CHIIKIKSN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-CHIIKIKSNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-CHIIKIKSNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-CHIIKIKSNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院歴表示区分コンボチェック処理
      *****************************************************************
       4200-REKI-DISPKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-REKI-DISPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-REKI-DISPKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-REKI-DISPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-REKI-DISPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院再計算区分コンボチェック処理
      *****************************************************************
       4200-SAIKEISANKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-SAIKEISANKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SAIKEISANKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SAIKEISANKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SAIKEISANKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱いコンボチェック処理
      *****************************************************************
       4200-NYKNKNRKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-NYKNKNRKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-NYKNKNRKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-NYKNKNRKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-NYKNKNRKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボチェック処理
      *****************************************************************
       4200-SKYPRTKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SKYPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SKYPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SKYPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SKYPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書未収額印刷区分コンボチェック処理
      *****************************************************************
       4200-MISYUPRTKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-MISYUPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-MISYUPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-MISYUPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-MISYUPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分コンボチェック処理
      *****************************************************************
       4200-SRYPRTKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SRYPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SRYPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SRYPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-SRYPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院時院外処方せん発行区分コンボチェック処理
      *****************************************************************
       4200-SHOHOUKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SHOHOUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SHOHOUKBNL   (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SHOHOUKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
           END-PERFORM
           .
       4200-SHOHOUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書発行区分サーチ処理
      *****************************************************************
       4200-TAIPRTKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-TAIPRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-TAIPRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-TAIPRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TAIPRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    退院証明書記載区分サーチ処理
      *****************************************************************
       4200-TAIKSIKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-TAIKSIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-TAIKSIKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-TAIKSIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4200-TAIKSIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−負担区分サーチ処理
      *****************************************************************
       4200-SENTEI-FTNKBNLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-SENTEI-FTNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SENTEI-FTNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SENTEI-FTNKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SENTEI-FTNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定療養費−消費税サーチ処理
      *****************************************************************
       4200-SENTEI-TAXLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-SENTEI-TAX-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-SENTEI-TAXL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-SENTEI-TAXLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SENTEI-TAXLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額消費税サーチ処理
      *****************************************************************
       4200-BRMSGK-TAXLST-CHK-SEC  SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-W24-BRMSGK-TAX-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-BRMSGK-TAXL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-BRMSGK-TAXLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BRMSGK-TAXLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計室料差額コンボチェック処理
      *****************************************************************
       4200-NACCT-BRMSGKLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-NACCT-BRMSGK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-NACCT-BRMSGKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-NACCT-BRMSGKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NACCT-BRMSGKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計外泊区分コンボチェック処理
      *****************************************************************
       4200-NACCT-GAIHAKULST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-NACCT-GAIHAKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-NACCT-GAIHAKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-NACCT-GAIHAKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NACCT-GAIHAKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    標欠減額区分コンボチェック処理
      *****************************************************************
       4200-HYOUKETULST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-HYOUKETU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-HYOUKETUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-HYOUKETULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HYOUKETULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定数超過区分コンボチェック処理
      *****************************************************************
       4200-TEISULST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-TEISU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-TEISUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-TEISULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEISULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ入力区分コンボチェック処理
      *****************************************************************
       4200-ADLINPUTLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-ADLINPUT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-ADLINPUTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-ADLINPUTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ADLINPUTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求差額初期表示区分コンボチェック処理
      *****************************************************************
       4200-TEIKISAGAKUACTLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-TEIKISAGAKUACT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-TEIKISAGAKUACTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-TEIKISAGAKUACTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TEIKISAGAKUACTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養入院表示区分コンボチェック処理
      *****************************************************************
       4200-RYOYODSPLST-CHK-SEC   SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W24-RYOYODSP-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W24-RYOYODSPL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W24-RYOYODSPLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       4200-RYOYODSPLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-INIT-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-W24-INPUT-AREA
                       SPA-NAI-W24-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-SKJSRY-MAX )
      *
               IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
                   MOVE    CONST-SKJSRY-H1804 (IDX0)
                                   TO  SPA-NAI-W24-SKJSRY-SRYCD  (IDX0)
               ELSE
                   MOVE    CONST-SKJSRY-SRYCD (IDX0)
                                   TO  SPA-NAI-W24-SKJSRY-SRYCD  (IDX0)
               END-IF
      *
               IF    ( SPA-NAI-W24-SKJSRY-SRYCD  (IDX0)
                                           NOT =   SPACE )
                   MOVE    SPA-NAI-W24-SKJSRY-SRYCD (IDX0)
                                       TO  WRK-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF  SPA-NAI-W24-SKJSRY-SRYCD (IDX0)  =  "197000570"
                       MOVE  "食堂加算"     TO    TNS-NAME
                   END-IF
                   MOVE    TNS-NAME    TO  SPA-GMN-W24-LBLSKJSRY (IDX0)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-NYUKSN-MAX )
      *
               EVALUATE    TRUE
               WHEN  ( SPA-W01-EDYUKYMD    >=  CONST-H220401 )
                   MOVE    CONST-NYUKSN-H2204 (IDX0)
                                   TO  SPA-NAI-W24-NYUKSN-SRYCD  (IDX0)
               WHEN  ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
                   MOVE    CONST-NYUKSN-H1804 (IDX0)
                                   TO  SPA-NAI-W24-NYUKSN-SRYCD  (IDX0)
               WHEN  ( SPA-W01-EDYUKYMD    >=  CONST-H160401 )
                   MOVE    CONST-NYUKSN-H1604 (IDX0)
                                   TO  SPA-NAI-W24-NYUKSN-SRYCD  (IDX0)
               WHEN    OTHER
                   MOVE    CONST-NYUKSN-SRYCD (IDX0)
                                   TO  SPA-NAI-W24-NYUKSN-SRYCD  (IDX0)
               END-EVALUATE
      *
               MOVE    "F"         TO  SPA-GMN-W24-CHK-NYUKSN (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-W01-STYUKYMDH
                                   TO  SPA-GMN-W24-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH
                                   TO  SPA-GMN-W24-EDYUKYMD
      *
           MOVE    SPA-GMN-W24-SKJSRYLST (1)
                                   TO  SPA-GMN-W24-SKJSRY-OCC (1)
                                       SPA-GMN-W24-SKJSRY-OCC (2)
      *
           MOVE    SPA-GMN-W24-BRMSGK-TAXLST (1)
                                   TO  SPA-GMN-W24-BRMSGK-TAX-G
      *
           MOVE    SPA-GMN-W24-NACCT-BRMSGKLST (1)
                                   TO  SPA-GMN-W24-NACCT-BRMSGK-G
      *
           MOVE    SPA-GMN-W24-NACCT-GAIHAKULST (1)
                                   TO  SPA-GMN-W24-NACCT-GAIHAKU-G
      *
           MOVE    SPA-GMN-W24-SENTEI-FTNKBNLST (1)
                                   TO  SPA-GMN-W24-SENTEI-FTNKBN-G
      *
           MOVE    CONST-SENTEI-TANKA
                                   TO  SPA-GMN-W24-SENTEI-TANKA
      *
           MOVE    SPA-GMN-W24-SENTEI-TAXLST (1)
                                   TO  SPA-GMN-W24-SENTEI-TAX-G
      *
           MOVE    SPA-GMN-W24-REKI-DISPKBNLST (1)
                                   TO  SPA-GMN-W24-REKI-DISPKBN-G
           MOVE    SPA-GMN-W24-SAIKEISANKBNLST (1)
                                   TO  SPA-GMN-W24-SAIKEISANKBN-G
           MOVE    SPA-GMN-W24-NYKNKNRKBNLST (1)
                                   TO  SPA-GMN-W24-NYKNKNRKBN-G
           MOVE    SPA-GMN-W24-SKYPRTKBNLST    (2)
                                   TO  SPA-GMN-W24-SKYPRTKBN-G
           MOVE    SPA-GMN-W24-MISYUPRTKBNLST  (1)
                                   TO  SPA-GMN-W24-MISYUPRTKBN-G
           MOVE    SPA-GMN-W24-SRYPRTKBNLST    (1)
                                   TO  SPA-GMN-W24-SRYPRTKBN-G
           MOVE    SPA-GMN-W24-TAIPRTKBNLST (2)
                                   TO  SPA-GMN-W24-TAIPRTKBN-G
           MOVE    SPA-GMN-W24-TAIKSIKBNLST (1)
                                   TO  SPA-GMN-W24-TAIKSIKBN-G
           MOVE    SPA-GMN-W24-HYOUKETULST (1)
                                   TO  SPA-GMN-W24-HYOUKETU-G
           MOVE    SPA-GMN-W24-TEISULST (1)
                                   TO  SPA-GMN-W24-TEISU-G
           MOVE    SPA-GMN-W24-ADLINPUTLST (2)
                                   TO  SPA-GMN-W24-ADLINPUT-G
           MOVE    SPA-GMN-W24-TEIKISAGAKUACTLST (1)
                                   TO  SPA-GMN-W24-TEIKISAGAKUACT-G
           MOVE    SPA-GMN-W24-RYOYODSPLST (1)
                                   TO  SPA-GMN-W24-RYOYODSP-G
      *
           MOVE    SPA-GMN-W24-SHOHOUKBNLST    (1)
                                   TO  SPA-GMN-W24-SHOHOUKBN-G
      *
      *    入院加算名称編集処理
           PERFORM 4201-NYUKSNMEI-EDIT-SEC
      *
      *    入院減算名称編集処理
           IF    ( SPA-W01-EDYUKYMD   >=   CONST-H180401 )
               CONTINUE
           ELSE
               PERFORM 4201-GENZANMEI-EDIT-SEC
           END-IF
      *
           MOVE    1               TO  SPA-GMN-W24-CUR
      *
           .
      *
       4201-INPUT-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    入院加算名称編集処理
      *****************************************************************
       4201-NYUKSNMEI-EDIT-SEC         SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
               UNTIL     ( IDX0    >   CONST-NYUKSN-MAX )
                    OR   ( SPA-NAI-W24-NYUKSN-SRYCD (IDX0) =   SPACE )
               MOVE    SPA-NAI-W24-NYUKSN-SRYCD (IDX0)
                                   TO  WRK-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   MOVE    TNS-NAME
                                   TO  SPA-GMN-W24-NYUKSNMEI (IDX0)
               END-IF
           END-PERFORM
      *
      *
           .
       4201-NYUKSNMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院減算名称編集処理
      *****************************************************************
       4201-GENZANMEI-EDIT-SEC         SECTION.
      *
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H160401 )
               MOVE    16          TO  IDX0
           ELSE
               MOVE    14          TO  IDX0
           END-IF
      *
           MOVE    2               TO  IDX1
      *
           PERFORM UNTIL ( IDX1    >   CONST-GEN-MAX )
      *
                MOVE    CONST-GENCD (IDX1)  TO  WRK-SRYCD
                PERFORM 900-TENSU-KEY-SEL-SEC
                MOVE    TNS-NAME   TO  SPA-GMN-W24-NYUKSNMEI  (IDX0)
      *
               COMPUTE IDX0    =   IDX0    +   1
               COMPUTE IDX1    =   IDX1    +   1
      *
           END-PERFORM
      *
           .
       4201-GENZANMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報検索処理
      *****************************************************************
       4201-SYS-1001-SEL-SEC           SECTION.
      *
      *    医療機関情報検索
           INITIALIZE  SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *
           .
       4201-SYS-1001-SEL-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           INITIALIZE      FLG-NYUKSNCD-G
           PERFORM VARYING  IDX-KSN FROM    1   BY  1
                   UNTIL (  IDX-KSN >   CONST-NYUKSN-MAX )
      *
               IF    ( SPA-GMN-W24-CHK-NYUKSN (IDX-KSN)  =   "T" )
                   EVALUATE    SPA-NAI-W24-NYUKSN-SRYCD (IDX-KSN)
                   WHEN    "190098870"
                       MOVE    1           TO  FLG-190098870
                   WHEN    "190098970"
                       MOVE    1           TO  FLG-190098970
                   WHEN    "190099170"
                       MOVE    1           TO  FLG-190099170
                   WHEN    "190099270"
                       MOVE    1           TO  FLG-190099270
                   WHEN    "190099570"
                       MOVE    1           TO  FLG-190099570
                   WHEN    "190099870"
                       MOVE    1           TO  FLG-190099870
                   WHEN    "190099970"
                       MOVE    1           TO  FLG-190099970
                   WHEN    "190100370"
                       MOVE    1           TO  FLG-190100370
                   WHEN    "190100470"
                       MOVE    1           TO  FLG-190100470
                   WHEN    "190100670"
                       MOVE    1           TO  FLG-190100670
                   WHEN    "190100770"
                       MOVE    1           TO  FLG-190100770
                   WHEN    "190100970"
                       MOVE    1           TO  FLG-190100970
                   WHEN    "190107170"
                       MOVE    1           TO  FLG-190107170
                   WHEN    "190117170"
                       MOVE    1           TO  FLG-190117170
                   WHEN    "190117270"
                       MOVE    1           TO  FLG-190117270
                   WHEN    "190119910"
                       MOVE    1           TO  FLG-190119910
                   WHEN    "190120410"
                       MOVE    1           TO  FLG-190120410
                   WHEN    "190120510"
                       MOVE    1           TO  FLG-190120510
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
      *    急性期病院加算と急性期特定病院加算が
      *    同時に算定されていないこと
           IF    ( SPA-W01-EDYUKYMD      >=    CONST-H180401 )
               CONTINUE
           ELSE
               IF    ( FLG-190099270           =   1 )
                AND  ( FLG-190099570           =   1 )
                   MOVE    "1001"      TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    地域医療支援病院入院診療加算１と２が
      *    同時に算定されていないこと
           IF    ( SPA-W01-EDYUKYMD      >=    CONST-H180401 )
               CONTINUE
           ELSE
               IF    ( FLG-190099870           =   1 )
                AND  ( FLG-190099970           =   1 )
                   MOVE   "1002"       TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    医療機関情報より病院か診療所を判断し、
      *    乳幼児加算のチェックが妥当であること
           PERFORM 4201-SYS-1001-SEL-SEC
           EVALUATE    SYS-1001-HOSPSBT
           WHEN    "1"
               IF    ( FLG-190100670     =   1 )
                OR   ( FLG-190100970     =   1 )
                   MOVE    "1003"        TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
      *
           WHEN    "2"
               IF    ( FLG-190100470     =   1 )
                OR   ( FLG-190100770     =   1 )
                   MOVE    "1004"        TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
      *
           END-EVALUATE
      *
      *    重複して診療行為コードが設定されていないこと
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   9 )
               IF    ( SPA-GMN-W24-SRYCD (IDX1)    NOT =   SPACE )
                   COMPUTE IDX3    =   IDX1    +   1
                   PERFORM VARYING IDX2    FROM    10      BY  -1
                           UNTIL ( IDX2    <   IDX3 )
                       IF    ( SPA-GMN-W24-SRYCD (IDX1)
                                           = SPA-GMN-W24-SRYCD (IDX2))
                           COMPUTE SPA-GMN-W24-CUR =   IDX2    +   28
                           MOVE    "1005"  TO  SPA-ERRCD
                           GO  TO  4900-KANRENCHK-EXT
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    選定入院料−負担区分が入力されていること
           IF    ( SPA-GMN-W24-SENTEI-FTNKBN   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    39            TO  SPA-GMN-W24-CUR
               MOVE    "1006"        TO  SPA-ERRCD
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    選定入院料−負担区分が定率設定の時、
      *    点数単価が９９点以下であること
           IF    ( SPA-GMN-W24-SENTEI-FTNKBN      =   "1" )
               IF    ( SPA-GMN-W24-SENTEI-TANKA  <=   99  )
                   CONTINUE
               ELSE
                   MOVE    40            TO  SPA-GMN-W24-CUR
                   MOVE    "1007"        TO  SPA-ERRCD
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    選定入院料−負担区分が定額設定の時、
      *    一般／老人と一般１日・老人１日と一緒に設定されていないこと
           IF    ( SPA-GMN-W24-SENTEI-FTNKBN      =   "2" )
               IF    ( SPA-GMN-W24-SENTEI-TANKA   >   ZERO  )
                   IF    ( SPA-GMN-W24-SENTEI-TANKAIP =    ZERO )
                    AND  ( SPA-GMN-W24-SENTEI-TANKARJ =    ZERO )
                       CONTINUE
                   ELSE
                       MOVE    40      TO  SPA-GMN-W24-CUR
                       MOVE    "1008"  TO  SPA-ERRCD
                       GO  TO  4900-KANRENCHK-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    標欠と定数超過は同時設定不可とする
           IF    ( SPA-GMN-W24-HYOUKETU    NOT = SPACE AND "0") AND
                 ( SPA-GMN-W24-TEISU       NOT = SPACE AND "0")
                       MOVE    53      TO  SPA-GMN-W24-CUR
                       MOVE    "1009"  TO  SPA-ERRCD
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
           MOVE    "W24    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           INITIALIZE                          W24
      *
      *    有効開始日
           MOVE    SPA-GMN-W24-STYUKYMD    TO  W24-STYUKYMD
      *    有効終了日
           MOVE    SPA-GMN-W24-EDYUKYMD    TO  W24-EDYUKYMD
      *    入院時食事療養
           MOVE    SPA-GMN-W24-SKJRYOYO-G  TO  W24-SKJRYOYO
           MOVE    SPA-GMN-W24-SKJRYOYO-MAX
                                           TO  W24-SKJRYOYO-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-W24-SKJRYOYO-MAX )
      *
               MOVE    SPA-GMN-W24-SKJRYOYOLST (IDX0)
                                           TO  W24-SKJRYOYO-ITM (IDX0)
      *
           END-PERFORM
      *
      *    食事診療行為
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-SKJSRY-MAX )
                    OR   ( SPA-NAI-W24-SKJSRY-SRYCD (IDX0) =   SPACE )
      *
               MOVE    SPA-GMN-W24-LBLSKJSRY (IDX0)
                                       TO  W24-LBLSYOKUJI (IDX0)
               MOVE    SPA-GMN-W24-SKJSRY-OCC (IDX0)
                                       TO  W24-SYOKUJI (IDX0)
               MOVE    SPA-GMN-W24-SKJSRY-MAX
                                       TO  W24-SYOKUJI-CNT (IDX0)
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   SPA-GMN-W24-SKJSRY-MAX )
      *
                   MOVE    SPA-GMN-W24-SKJSRYLST (IDX1)
                                       TO  W24-SYOKUJI-ITM (IDX0 IDX1)
      *
               END-PERFORM
      *
           END-PERFORM
      *
      *
      *    室料差額消費税区分
           MOVE    SPA-GMN-W24-BRMSGK-TAX-G TO W24-BRMSGK-TAX
           MOVE    SPA-GMN-W24-BRMSGK-TAX-MAX
                                           TO  W24-BRMSGK-TAX-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-BRMSGK-TAX-CNT )
      *
               MOVE    SPA-GMN-W24-BRMSGK-TAXLST (IDX0)
                                       TO  W24-BRMSGK-TAX-ITM (IDX0)
      *
           END-PERFORM
      *
      *    入院会計室料差額区分
           MOVE    SPA-GMN-W24-NACCT-BRMSGK-G
                                           TO  W24-NACCT-BRMSGK
           MOVE    SPA-GMN-W24-NACCT-BRMSGK-MAX
                                           TO  W24-NACCT-BRMSGK-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-NACCT-BRMSGK-CNT )
      *
               MOVE    SPA-GMN-W24-NACCT-BRMSGKLST (IDX0)
                                       TO  W24-NACCT-BRMSGK-ITM(IDX0)
      *
           END-PERFORM
      *
      *    入院会計外泊区分
           MOVE    SPA-GMN-W24-NACCT-GAIHAKU-G
                                           TO  W24-NACCT-GAIHAKU
           MOVE    SPA-GMN-W24-NACCT-GAIHAKU-MAX
                                           TO  W24-NACCT-GAIHAKU-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-NACCT-GAIHAKU-CNT )
      *
               MOVE    SPA-GMN-W24-NACCT-GAIHAKULST (IDX0)
                                       TO  W24-NACCT-GAIHAKU-ITM (IDX0)
      *
           END-PERFORM
      *
      *    入院加算情報
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-NYUKSN-MAX )
               MOVE    SPA-GMN-W24-NYUKSNMEI (IDX0)
                                           TO  W24-NYUKSNMEI (IDX0)
               MOVE    SPA-GMN-W24-CHK-NYUKSN (IDX0)
                                           TO  W24-CHK-NYUKSN(IDX0)
           END-PERFORM
      *
      *    入院会計表示情報
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-SRYACT-MAX )
               MOVE    SPA-GMN-W24-SRYCD (IDX0)
                                           TO  W24-SRYCD (IDX0)
               MOVE    SPA-GMN-W24-SRYACT (IDX0)
                                           TO  W24-SRYACT (IDX0)
           END-PERFORM
      *
      *    地域加算
           MOVE    SPA-GMN-W24-CHIIKIKSN-G TO  W24-CHIIKIKSN
           MOVE    SPA-GMN-W24-CHIIKIKSN-MAX
                                           TO  W24-CHIIKIKSN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-CHIIKIKSN-CNT )
      *
               MOVE    SPA-GMN-W24-CHIIKIKSNLST (IDX0)
                                           TO  W24-CHIIKIKSN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    選定療養費−負担区分
           MOVE    SPA-GMN-W24-SENTEI-FTNKBN-G
                                           TO  W24-SENTEI-FTNKBN
           MOVE    SPA-GMN-W24-SENTEI-FTNKBN-MAX
                                           TO  W24-SENTEI-FTNKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-SENTEI-FTNKBN-CNT )
      *
               MOVE    SPA-GMN-W24-SENTEI-FTNKBNLST (IDX0)
                                       TO  W24-SENTEI-FTNKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    選定療養費−点数単価
           MOVE    SPA-GMN-W24-SENTEI-TANKA
                                       TO  W24-SENTEI-TANKA
      *
      *    選定療養費−点数単価（一般）
           MOVE    SPA-GMN-W24-SENTEI-TANKAIP
                                       TO  W24-SENTEI-TANKAIP
      *
      *    選定療養費−点数単価（老人）
           MOVE    SPA-GMN-W24-SENTEI-TANKARJ
                                       TO  W24-SENTEI-TANKARJ
      *
      *    選定療養費−ラベル
           EVALUATE    SPA-GMN-W24-SENTEI-FTNKBN
           WHEN    "1"
               MOVE    "点数単価"      TO  W24-LBL-SENTEI-TANKA
               MOVE    SPACE           TO  W24-LBL-SENTEI-EN
           WHEN    "2"
               MOVE    "一般／老人"    TO  W24-LBL-SENTEI-TANKA
               MOVE    "円"            TO  W24-LBL-SENTEI-EN
               MOVE    "一般１日"      TO  W24-LBL-SENTEI-TANKAIP
               MOVE    "円"            TO  W24-LBL-SENTEI-ENIP
               MOVE    "老人１日"      TO  W24-LBL-SENTEI-TANKARJ
               MOVE    "円"            TO  W24-LBL-SENTEI-ENRJ
           WHEN    OTHER
               MOVE    SPACE           TO  W24-LBL-SENTEI-TANKA
               MOVE    SPACE           TO  W24-LBL-SENTEI-EN
           END-EVALUATE
      *
      *    選定療養費−消費税
           MOVE    SPA-GMN-W24-SENTEI-TAX-G
                                       TO  W24-SENTEI-TAX
           MOVE    SPA-GMN-W24-SENTEI-TAX-MAX
                                       TO  W24-SENTEI-TAX-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-SENTEI-TAX-CNT )
      *
               MOVE    SPA-GMN-W24-SENTEI-TAXLST (IDX0)
                                       TO  W24-SENTEI-TAX-ITM (IDX0)
      *
           END-PERFORM
      *
      *    入院歴表示区分
           MOVE    SPA-GMN-W24-REKI-DISPKBN-G
                                       TO  W24-REKI-DISPKBN
           MOVE    SPA-GMN-W24-REKI-DISPKBN-MAX
                                       TO  W24-REKI-DISPKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-REKI-DISPKBN-CNT )
      *
               MOVE    SPA-GMN-W24-REKI-DISPKBNLST (IDX0)
                                       TO  W24-REKI-DISPKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    退院再計算区分
           MOVE    SPA-GMN-W24-SAIKEISANKBN-G
                                       TO  W24-SAIKEISANKBN
           MOVE    SPA-GMN-W24-SAIKEISANKBN-MAX
                                       TO  W24-SAIKEISANKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-SAIKEISANKBN-CNT )
      *
               MOVE    SPA-GMN-W24-SAIKEISANKBNLST (IDX0)
                                       TO  W24-SAIKEISANKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    入金の取扱い
           MOVE    SPA-GMN-W24-NYKNKNRKBN-G
                                       TO  W24-NYKNKNRKBN
           MOVE    SPA-GMN-W24-NYKNKNRKBN-MAX
                                       TO  W24-NYKNKNRKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-NYKNKNRKBN-CNT )
      *
               MOVE    SPA-GMN-W24-NYKNKNRKBNLST (IDX0)
                                       TO  W24-NYKNKNRKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    請求書発行区分
           MOVE    SPA-GMN-W24-SKYPRTKBN-G
                                       TO  W24-SKYPRTKBN
           MOVE    SPA-GMN-W24-SKYPRTKBN-MAX
                                       TO  W24-SKYPRTKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   W24-SKYPRTKBN-CNT )
      *
               MOVE    SPA-GMN-W24-SKYPRTKBNLST (IDX0)
                                       TO  W24-SKYPRTKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    請求書未収額印刷区分
           MOVE    SPA-GMN-W24-MISYUPRTKBN-G
                                       TO  W24-SKYMISYUPRTKBN
           MOVE    SPA-GMN-W24-MISYUPRTKBN-MAX
                                       TO  W24-SKYMISYUPRTKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-SKYMISYUPRTKBN-CNT )
      *
               MOVE    SPA-GMN-W24-MISYUPRTKBNLST (IDX0)
                                       TO  W24-SKYMISYUPRTKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    診療費明細書発行区分
           MOVE    SPA-GMN-W24-SRYPRTKBN-G
                                       TO  W24-SRYPRTKBN
           MOVE    SPA-GMN-W24-SRYPRTKBN-MAX
                                       TO  W24-SRYPRTKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   W24-SRYPRTKBN-CNT )
      *
               MOVE    SPA-GMN-W24-SRYPRTKBNLST (IDX0)
                                       TO  W24-SRYPRTKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    退院証明書発行区分
           MOVE    SPA-GMN-W24-TAIPRTKBN-G
                                       TO  W24-TAIPRTKBN
           MOVE    SPA-GMN-W24-TAIPRTKBN-MAX
                                       TO  W24-TAIPRTKBN-CNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-TAIPRTKBN-CNT )
      *
               MOVE    SPA-GMN-W24-TAIPRTKBNLST (IDX0)
                                       TO  W24-TAIPRTKBN-ITM (IDX0)
      *
           END-PERFORM
      *
      *    退院証明書記載区分
           MOVE    SPA-GMN-W24-TAIKSIKBN-G
                                       TO  W24-TAIKSIKBN
           MOVE    SPA-GMN-W24-TAIKSIKBN-MAX
                                       TO  W24-TAIKSIKBN-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-TAIKSIKBN-CNT )
      *
      *
               MOVE    SPA-GMN-W24-TAIKSIKBNLST (IDX0)
                                       TO  W24-TAIKSIKBN-ITM (IDX0)
      *
           END-PERFORM
      *!!
      *    退院時院外処方せん発行区分
           MOVE    SPA-GMN-W24-SHOHOUKBN-G
                                       TO  W24-SHOHOUKBN
           MOVE    SPA-GMN-W24-SHOHOUKBN-MAX
                                       TO  W24-SHOHOUKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-W24-SHOHOUKBN-MAX )
      *
               MOVE    SPA-GMN-W24-SHOHOUKBNLST (IDX0)
                                       TO  W24-SHOHOUKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
      *    標欠減額区分
           MOVE    SPA-GMN-W24-HYOUKETU-G
                                       TO  W24-HYOUKETU
           MOVE    SPA-GMN-W24-HYOUKETU-MAX
                                       TO  W24-HYOUKETU-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-HYOUKETU-CNT )
      *
      *
               MOVE    SPA-GMN-W24-HYOUKETULST (IDX0)
                                       TO  W24-HYOUKETU-ITM (IDX0)
      *
           END-PERFORM
      *
      *    定数超過区分
           MOVE    SPA-GMN-W24-TEISU-G
                                       TO  W24-TEISU
           MOVE    SPA-GMN-W24-TEISU-MAX
                                       TO  W24-TEISU-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-TEISU-CNT )
      *
      *
               MOVE    SPA-GMN-W24-TEISULST (IDX0)
                                       TO  W24-TEISU-ITM (IDX0)
      *
           END-PERFORM
      *
      *
      *    ＡＤＬ入力区分
           MOVE    SPA-GMN-W24-ADLINPUT-G
                                       TO  W24-ADLINPUT
           MOVE    SPA-GMN-W24-ADLINPUT-MAX
                                       TO  W24-ADLINPUT-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-ADLINPUT-CNT )
      *
      *
               MOVE    SPA-GMN-W24-ADLINPUTLST (IDX0)
                                       TO  W24-ADLINPUT-ITM (IDX0)
      *
           END-PERFORM
      *
      *    定期請求差額初期表示区分
           MOVE    SPA-GMN-W24-TEIKISAGAKUACT-G
                                       TO  W24-TEIKISAGAKUACT
           MOVE    SPA-GMN-W24-TEIKISAGAKUACT-MAX
                                       TO  W24-TEIKISAGAKUACT-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-TEIKISAGAKUACT-CNT )
      *
      *
               MOVE    SPA-GMN-W24-TEIKISAGAKUACTLST (IDX0)
                                       TO  W24-TEIKISAGAKUACT-ITM (IDX0)
      *
           END-PERFORM
      *
      *    療養入院表示区分
           MOVE    SPA-GMN-W24-RYOYODSP-G
                                       TO  W24-RYOYODSP
           MOVE    SPA-GMN-W24-RYOYODSP-MAX
                                       TO  W24-RYOYODSP-CNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   W24-RYOYODSP-CNT )
      *
      *
               MOVE    SPA-GMN-W24-RYOYODSPLST (IDX0)
                                       TO  W24-RYOYODSP-ITM (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-W01-EDYUKYMD    >=  CONST-H180401 )
               MOVE    "入院加算情報"          TO  W24-FRAME3-LABEL
           ELSE
               MOVE    "入院加算・減算情報"    TO  W24-FRAME3-LABEL
           END-IF
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
           MOVE    WIDGET-NORMAL       TO  W24-B03-STATE
           MOVE    WIDGET-NORMAL       TO  W24-SENTEI-TANKAIP-STATE
           MOVE    WIDGET-NORMAL       TO  W24-SENTEI-TANKARJ-STATE
           MOVE    WIDGET-NORMAL       TO  W24-B03-STATE
           MOVE    WIDGET-NORMAL       TO  W24-SYOKUJI-STATE (1)
           MOVE    WIDGET-NORMAL       TO  W24-SYOKUJI-STATE (2)
           MOVE    WIDGET-NORMAL       TO  W24-SYOKUJI-ITM-STATE (1)
           MOVE    WIDGET-NORMAL       TO  W24-SYOKUJI-ITM-STATE (2)
           MOVE    WIDGET-NORMAL       TO  W24-HYOUKETU-STATE
           MOVE    WIDGET-NORMAL       TO  W24-TEISU-STATE
           MOVE    WIDGET-NORMAL       TO  W24-HYOUKETU-ITM-STATE
      *
           IF    ( SPA-W01-STYUKYMD    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-B03-STATE
           END-IF
      *
           IF    ( SPA-GMN-W24-SENTEI-FTNKBN   =   "1" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-SENTEI-TANKAIP-STATE
                                           W24-SENTEI-TANKARJ-STATE
           END-IF
      *
           IF    ( SPA-NAI-W24-SKJSRY-SRYCD (1)   =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-SYOKUJI-STATE (1)
                                           W24-SYOKUJI-ITM-STATE (1)
           END-IF
      *
           IF    ( SPA-NAI-W24-SKJSRY-SRYCD (2)   =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-SYOKUJI-STATE (2)
                                           W24-SYOKUJI-ITM-STATE (2)
           END-IF
      *
           IF    ( SPA-W01-STYUKYMD    <   CONST-H180401 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-HYOUKETU-STATE
                                           W24-HYOUKETU-ITM-STATE
           END-IF
      *
           IF    ( SPA-W01-STYUKYMD    <   CONST-H180401 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W24-TEISU-STATE
                                           W24-TEISU-ITM-STATE
           END-IF
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
                  (SPA-GMN-W24-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W24-CUR
           WHEN    001
               MOVE    "SKJRYOYO"      TO  MCP-WIDGET
           WHEN    002
               MOVE    "SYOKUJI01"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "SYOKUJI02"     TO  MCP-WIDGET
           WHEN    008
               MOVE    "CHK_NYUKSN01"  TO  MCP-WIDGET
           WHEN    009
               MOVE    "CHK_NYUKSN02"  TO  MCP-WIDGET
           WHEN    010
               MOVE    "CHK_NYUKSN03"  TO  MCP-WIDGET
           WHEN    011
               MOVE    "CHK_NYUKSN04"  TO  MCP-WIDGET
           WHEN    012
               MOVE    "CHK_NYUKSN05"  TO  MCP-WIDGET
           WHEN    013
               MOVE    "CHK_NYUKSN06"  TO  MCP-WIDGET
           WHEN    014
               MOVE    "CHK_NYUKSN07"  TO  MCP-WIDGET
           WHEN    015
               MOVE    "CHK_NYUKSN08"  TO  MCP-WIDGET
           WHEN    016
               MOVE    "CHK_NYUKSN09"  TO  MCP-WIDGET
           WHEN    017
               MOVE    "CHK_NYUKSN10"  TO  MCP-WIDGET
           WHEN    018
               MOVE    "CHK_NYUKSN11"  TO  MCP-WIDGET
           WHEN    019
               MOVE    "CHK_NYUKSN12"  TO  MCP-WIDGET
           WHEN    020
               MOVE    "CHK_NYUKSN13"  TO  MCP-WIDGET
           WHEN    021
               MOVE    "CHK_NYUKSN14"  TO  MCP-WIDGET
           WHEN    022
               MOVE    "CHK_NYUKSN15"  TO  MCP-WIDGET
           WHEN    023
               MOVE    "CHK_NYUKSN16"  TO  MCP-WIDGET
           WHEN    024
               MOVE    "CHK_NYUKSN17"  TO  MCP-WIDGET
           WHEN    025
               MOVE    "CHK_NYUKSN18"  TO  MCP-WIDGET
           WHEN    026
               MOVE    "CHK_NYUKSN19"  TO  MCP-WIDGET
           WHEN    027
               MOVE    "CHK_NYUKSN20"  TO  MCP-WIDGET
           WHEN    028
               MOVE    "CHIIKIKSN"     TO  MCP-WIDGET
           WHEN    029
               MOVE    "SRYCD01"       TO  MCP-WIDGET
           WHEN    030
               MOVE    "SRYCD02"       TO  MCP-WIDGET
           WHEN    031
               MOVE    "SRYCD03"       TO  MCP-WIDGET
           WHEN    032
               MOVE    "SRYCD04"       TO  MCP-WIDGET
           WHEN    033
               MOVE    "SRYCD05"       TO  MCP-WIDGET
           WHEN    034
               MOVE    "SRYCD06"       TO  MCP-WIDGET
           WHEN    035
               MOVE    "SRYCD07"       TO  MCP-WIDGET
           WHEN    036
               MOVE    "SRYCD08"       TO  MCP-WIDGET
           WHEN    037
               MOVE    "SRYCD09"       TO  MCP-WIDGET
           WHEN    038
               MOVE    "SRYCD10"       TO  MCP-WIDGET
           WHEN    039
               MOVE    "SENTEI_FTNKBN"    TO  MCP-WIDGET
           WHEN    040
               MOVE    "SENTEI_TANKA"  TO  MCP-WIDGET
           WHEN    041
               MOVE    "SENTEI_TAX"    TO  MCP-WIDGET
           WHEN    042
               MOVE    "REKI_DISPKBN"  TO  MCP-WIDGET
           WHEN    056
               MOVE    "SAIKEISANKBN"  TO  MCP-WIDGET
           WHEN    058
               MOVE    "NYKNKNRKBN"    TO  MCP-WIDGET
           WHEN    043
               MOVE    "SKYPRTKBN"     TO  MCP-WIDGET
           WHEN    044
               MOVE    "SKYMISYUPRTKBN"
                                       TO  MCP-WIDGET
           WHEN    050
               MOVE    "SRYPRTKBN"     TO  MCP-WIDGET
           WHEN    045
               MOVE    "TAIPRTKBN"     TO  MCP-WIDGET
           WHEN    052
               MOVE    "TAIKSIKBN"     TO  MCP-WIDGET
           WHEN    046
               MOVE    "BRMSGK_TAX"    TO  MCP-WIDGET
           WHEN    047
               MOVE    "NACCT_BRMSGK"  TO  MCP-WIDGET
           WHEN    051
               MOVE    "NACCT_GAIHAKU" TO  MCP-WIDGET
           WHEN    053
               MOVE    "HYOUKETU"      TO  MCP-WIDGET
           WHEN    054
               MOVE    "TEISU"         TO  MCP-WIDGET
           WHEN    055
               MOVE    "ADLINPUT"      TO  MCP-WIDGET
           WHEN    048
               MOVE    "SENTEI_TANKAIP"
                                       TO  MCP-WIDGET
           WHEN    049
               MOVE    "SENTEI_TANKARJ"
                                       TO  MCP-WIDGET
           WHEN    057
               MOVE    "TEIKISAGAKUACT" TO MCP-WIDGET
           WHEN    059
               MOVE    "RYOYODSP"      TO MCP-WIDGET
      *    退院時院外処方せん
           WHEN    060
               MOVE    "SHOHOUKBN"     TO MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    MCP-WIDGET
           WHEN    "SKJRYOYO"
               MOVE    002             TO  SPA-GMN-W24-CUR
           WHEN    "SYOKUJI01"
               MOVE    003             TO  SPA-GMN-W24-CUR
           WHEN    "SYOKUJI02"
               MOVE    046             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN01"
               MOVE    009             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN02"
               MOVE    010             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN03"
               MOVE    011             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN04"
               MOVE    012             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN05"
               MOVE    013             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN06"
               MOVE    014             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN07"
               MOVE    015             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN08"
               MOVE    016             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN09"
               MOVE    017             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN10"
               MOVE    018             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN11"
               MOVE    019             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN12"
               MOVE    020             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN13"
               MOVE    021             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN14"
               MOVE    022             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN15"
               MOVE    023             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN16"
               MOVE    024             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN17"
               MOVE    025             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN18"
               MOVE    026             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN19"
               MOVE    027             TO  SPA-GMN-W24-CUR
           WHEN    "CHK_NYUKSN20"
               MOVE    028             TO  SPA-GMN-W24-CUR
           WHEN    "CHIIKIKSN"
               MOVE    029             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD01"
               MOVE    030             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD02"
               MOVE    031             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD03"
               MOVE    032             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD04"
               MOVE    033             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD05"
               MOVE    034             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD06"
               MOVE    035             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD07"
               MOVE    036             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD08"
               MOVE    037             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD09"
               MOVE    038             TO  SPA-GMN-W24-CUR
           WHEN    "SRYCD10"
               MOVE    039             TO  SPA-GMN-W24-CUR
           WHEN    "SENTEI_FTNKBN"
               EVALUATE    SPA-GMN-W24-SENTEI-FTNKBN
               WHEN    "1"
               WHEN    "2"
                   MOVE    040         TO  SPA-GMN-W24-CUR
               WHEN    OTHER
                   MOVE    042         TO  SPA-GMN-W24-CUR
               END-EVALUATE
           WHEN    "SENTEI_TANKA"
               MOVE    048             TO  SPA-GMN-W24-CUR
           WHEN    "SENTEI_TANKAIP"
               MOVE    049             TO  SPA-GMN-W24-CUR
           WHEN    "SENTEI_TANKARJ"
               MOVE    041             TO  SPA-GMN-W24-CUR
           WHEN    "SENTEI_TAX"
               MOVE    042         TO  SPA-GMN-W24-CUR
           WHEN    "REKI_DISPKBN"
               MOVE    056             TO  SPA-GMN-W24-CUR
           WHEN    "SAIKEISANKBN"
               MOVE    058             TO  SPA-GMN-W24-CUR
           WHEN    "NYKNKNRKBN"
               MOVE    043             TO  SPA-GMN-W24-CUR
           WHEN    "SKYPRTKBN"
               MOVE    044             TO  SPA-GMN-W24-CUR
           WHEN    "SKYMISYUPRTKBN"
               MOVE    050             TO  SPA-GMN-W24-CUR
           WHEN    "SRYPRTKBN"
               MOVE    045             TO  SPA-GMN-W24-CUR
           WHEN    "TAIPRTKBN"
               MOVE    052             TO  SPA-GMN-W24-CUR
           WHEN    "TAIKSIKBN"
               MOVE    057             TO  SPA-GMN-W24-CUR
           WHEN    "BRMSGK_TAX"
               MOVE    047             TO  SPA-GMN-W24-CUR
           WHEN    "NACCT_BRMSGK"
               MOVE    051             TO  SPA-GMN-W24-CUR
           WHEN    "NACCT_GAIHAKU"
               MOVE    053             TO  SPA-GMN-W24-CUR
           WHEN    "HYOUKETU"
               MOVE    054             TO  SPA-GMN-W24-CUR
           WHEN    "TEISU"
               MOVE    055             TO  SPA-GMN-W24-CUR
           WHEN    "ADLINPUT"
               MOVE    059             TO  SPA-GMN-W24-CUR
           WHEN    "TEIKISAGAKUACT"
      ******** MOVE    212             TO  SPA-GMN-W24-CUR
               MOVE    060             TO  SPA-GMN-W24-CUR
           WHEN    "SHOHOUKBN"
               MOVE    212             TO  SPA-GMN-W24-CUR
           WHEN    "RYOYODSP"
               MOVE    008             TO  SPA-GMN-W24-CUR
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
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-W24-CUR =   2 )
               IF    ( W24-SYOKUJI-STATE (1)   =   WIDGET-INSENSITIVE )
                   MOVE    3           TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W24-CUR =   3 )
               IF    ( W24-SYOKUJI-STATE (2)   =   WIDGET-INSENSITIVE )
                   MOVE    46          TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           PERFORM VARYING IDX0    FROM    8   BY  1
                   UNTIL ( IDX0    >   27 )
      *
               IF    ( SPA-GMN-W24-CUR     =   IDX0 )
                   IF    ( SPA-GMN-W24-NYUKSNMEI (IDX0 - 7)
                                           =   SPACE )
                       MOVE    28      TO  SPA-GMN-W24-CUR
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-W24-CUR     =   48 )
               IF    ( W24-SENTEI-TANKAIP-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    41          TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W24-CUR     =   49 )
               IF    ( W24-SENTEI-TANKARJ-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    41          TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W24-CUR     =   53 )
               IF    ( W24-HYOUKETU-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    54           TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W24-CUR     =   54 )
               IF    ( W24-TEISU-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    55           TO  SPA-GMN-W24-CUR
               END-IF
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
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
               MOVE    "該当の診療コードは存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "診療コード入力規則にあっていません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "加算に必要な施設基準が設定されていません"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "食事療養費入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "特別管理加算入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "食堂加算入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "地域加算入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "急性期病院加算と急性期特定病院加算は同時"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "に加算できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1002"
               MOVE    "地域医療支援病院入院診療加算１と２は同時"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "に加算できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1003"
               MOVE    "診療所に関する加算は行えません"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "病院に関する加算は行えません"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "同じ診療行為が重複して設定されています"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "負担計算を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "点数単価は９９点以下で入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "一般／老人と一般１日（老人１日）はいずれ"
                                           DELIMITED   BY  SIZE
                       "かを入力してください"
                                           DELIMITED   BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "1009"
               MOVE    "標欠と定数超過は同時設定できません"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "標欠または定数超過と同時設定できません"
                                       TO  SPA-ERRMSG
           WHEN    "1201"
               MOVE    "更新できませんでした"
                                       TO  SPA-ERRMSG
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
           MOVE    "W24"               TO  SPA-MOTOPG
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
           WHEN    "3002"
               MOVE    "削除処理をします"
                                        TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W24"                TO  SPA-MOTOPG
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
      *    平成１８年度改正チェック処理
      *****************************************************************
       800-H1804-KAISEI-CHK-SEC            SECTION.
      *
           IF    ( SPA-W01-SELNUM      >   ZERO )
               IF    ( CONST-H180401   >   SPA-W01-SEL-STYUKYMD )
                AND  ( CONST-H180401  <=   SPA-W01-EDYUKYMD )
                   PERFORM 310-SYS-5000-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       MOVE    SYS-5000-EDYUKYMD   TO  WRK-EDYUKYMD
                       MOVE    "20060331"          TO  SYS-5000-EDYUKYMD
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD    TO  SYS-5000-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-5000-UPHMS
                       MOVE    SYS-5000-REC    TO  SYSKANRI-REC
                       MOVE    SYS-5000-HOSPNUM    TO  SYSUP-HOSPNUM
                       MOVE    SYS-5000-KANRICD    TO  SYSUP-KANRICD
                       MOVE    SYS-5000-KBNCD      TO  SYSUP-KBNCD
                       MOVE    SYS-5000-STYUKYMD   TO  SYSUP-STYUKYMD
                       MOVE    WRK-EDYUKYMD        TO  SYSUP-EDYUKYMD
                       MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF    ( MCP-RC  NOT =   ZERO )
                           DISPLAY "1:DBUPDATE ERR"
                       END-IF
      *
                       MOVE    SPA-W01-STYUKYMD    TO  WRK-STYUKYMD
                       MOVE    SPA-W01-SELNUM      TO  WRK-SELNUM
                       MOVE    SPA-W01-SEL-STYUKYMD
                                                   TO  WRK-SEL-STYUKYMD
      *
                       INITIALIZE                  SPA-W01-SELNUM
                                                   SPA-W01-SEL-STYUKYMD
                                                   SPA-W01-SEL-EDYUKYMD
      *
                       MOVE    CONST-H180401       TO  SPA-W01-STYUKYMD
                       PERFORM 3101-SPASET-SEC
                       PERFORM 4811-HOSP-NYUINKHN-EDIT-SEC
      *
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD        TO  SYS-5000-CREYMD
                                                       SYS-5000-UPYMD
                       MOVE    SMCNDATE-HMS        TO  SYS-5000-UPHMS
      *
                       MOVE    SYS-5000-REC    TO  SYSKANRI-REC
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC  NOT =   ZERO )
                           DISPLAY "2:DBINSERT ERR"
                       END-IF
                       MOVE    WRK-SELNUM      TO  SPA-W01-SELNUM
                       MOVE    WRK-STYUKYMD    TO  SPA-W01-STYUKYMD
                       MOVE    WRK-SEL-STYUKYMD
                                               TO  SPA-W01-SEL-STYUKYMD
      *
                       MOVE    20060331        TO  SPA-W01-SEL-EDYUKYMD
                                                   SPA-W01-EDYUKYMD
      *
                       MOVE    SPA-W01-STYUKYMD    TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-W01-STYUKYMDH
      *
                       MOVE    SPA-W01-EDYUKYMD    TO  WRK-YMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-W01-EDYUKYMDH
      *
                   END-IF
               END-IF
           ELSE
               IF    ( CONST-H180401   >  SPA-W01-STYUKYMD )
                AND  ( CONST-H180401  <=  SPA-W01-EDYUKYMD )
                   MOVE    SPA-W01-EDYUKYMD    TO  WRK-EDYUKYMD
                   MOVE    20060331            TO  SPA-W01-EDYUKYMD
                   PERFORM 3101-SPASET-SEC
                   PERFORM 4811-HOSP-NYUINKHN-EDIT-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYS-5000-CREYMD
                                               SYS-5000-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5000-UPHMS
      *
                   MOVE    SYS-5000-REC    TO  SYSKANRI-REC
                   MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC  NOT =   ZERO )
                       DISPLAY "3:DBINSERT ERR"
                   END-IF
      *
                   MOVE    CONST-H180401   TO  SPA-W01-STYUKYMD
                   MOVE    WRK-EDYUKYMD    TO  SPA-W01-EDYUKYMD
                   PERFORM 3101-SPASET-SEC
                   PERFORM 4811-HOSP-NYUINKHN-EDIT-SEC
      *
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD    TO  SYS-5000-CREYMD
                                               SYS-5000-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYS-5000-UPHMS
      *
                   MOVE    SYS-5000-REC    TO  SYSKANRI-REC
                   MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC  NOT =   ZERO )
                       DISPLAY "4:DBINSERT ERR"
                   END-IF
      *
                   MOVE    CONST-H180401   TO  SPA-W01-STYUKYMD
                   MOVE    WRK-EDYUKYMD    TO  SPA-W01-EDYUKYMD
      *
                   MOVE    SPA-W01-STYUKYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-W01-STYUKYMDH
      *
                   MOVE    SPA-W01-EDYUKYMD    TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-W01-EDYUKYMDH
      *
               END-IF
           END-IF
      *
           .
      *
       800-H1804-KAISEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
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
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
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
      *    システム管理検索処理(KEY16)
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
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
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-W01-EDYUKYMD    TO  TNS-YUKOSTYMD
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
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
