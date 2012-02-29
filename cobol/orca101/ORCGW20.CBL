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
       PROGRAM-ID.                               ORCGW20.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 病棟管理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/30    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 00.00.01     NACL−太田   02/11/29  有効開始日を基準にシス管の
      *                                     情報を取得するよう修正
      *                                     病棟科を最大９９件に変更
      * 00.00.02     NACL−太田   03/01/29  有効開始日のチェック追加
      * 01.00.01     NACL-太田    03.11.17  更新日付編集対応
      * 02.08.00     NACL-太田    06.03.09  平成１８年度改正対応
      * 03.05.00     NACL-多々納  07/05/08  グループ診療対応
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
           COPY    "W20SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-BTU-NUM                         PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-BRM-SONZAI                      PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-NYUKHN                          PIC 9(01).
           03  FLG-SYS5001                         PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-SETKHNRYO                       PIC 9(01).
           03  FLG-BTULST-ROW                      PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-KSNERR                          PIC 9(01).
           03  FLG-LOOPEND                         PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
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
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-MAX                             PIC 9(05).
           03  IDX-LST                             PIC 9(05).
           03  IDX-BTULST                          PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
           03  IDX-GKSN                            PIC 9(05).
           03  IDX-SST                             PIC 9(05).
           03  IDX-KSN2                            PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-STR                             PIC 9(05).
           03  CNT-SST                             PIC 9(05).
           03  CNT-KSN                             PIC 9(05).
           03  CNT-NYUKSNKBNCHK                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-MAX                             PIC 9(05).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-MAE-INPUT               PIC X(400).
           03  WRK-KANACHK-OUT-INPUT               PIC X(400).
           03  WRK-BTUNUM                          PIC 9(02).
           03  WRK-ZZZ9                            PIC ZZZ9.
           03  WRK-ZZZZZZZZZ9                      PIC Z(09)9.
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
               05  WRK-CMB-ITM4                    PIC X(200).
           03  WRK-KIJUN-YMD                       PIC X(08).
           03  WRK-BTU-NINZU                       PIC 9(05).
           03  WRK-BTULST                          PIC 9(05).
           03  WRK-TNS-NAME                        PIC X(30).
           03  WRK-STYUKYMD                        PIC X(08).
           03  WRK-EDYUKYMD                        PIC X(08).
           03  WRK-CDKBNNUM                        PIC 9(03).
           03  WRK-CDKOUBAN                        PIC 9(02).
           03  WRK-TK-CDKBNNUM                     PIC 9(03).
           03  WRK-TK-CDKOUBAN                     PIC 9(02).
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-KJNYMD                      PIC X(08).
           03  WRK-99                              PIC 9(02).
           03  WRK-BTULST-SEL                      PIC 9(02).
           03  WRK-STATE                           PIC S9(9) BINARY.
      *    入力項目名
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-YUKOSTYMD                       PIC X(08).
           03  WRK-YUKOEDYMD                       PIC X(08).
           03  WRK-KJN-YUKSTYMD                    PIC X(08).
           03  WRK-PATHNAME                        PIC X(64).
           03  WRK-BASE-CUR                        PIC 9(03).
           03  WRK-KSNCD                           PIC X(09).
           03  WRK-KSNCD-SEN                       PIC X(09).
           03  WRK-KSNCD-TOK                       PIC X(09).
           03  WRK-KSNIDX                          PIC 9(03).
           03  WRK-KSNKSNKBN                       PIC 9(03).
           03  WRK-NYUKSNKBN-G.
               05  WRK-NYUKSNKBN-MAX               PIC 9(03).
               05  WRK-NYUKSNKBN                   PIC 9(03)
                                                   OCCURS   10.
           03  WRK-KSNKBN                          PIC 9(03).
           03  WRK-NYUINKBN                        PIC 9(03).
           03  WRK-SSTKJNCD-G.
               05  WRK-SSTKJNCD-OCC                OCCURS 10.
                   07  WRK-SSTKJNCDSEP             PIC X(01).
                   07  WRK-SSTKJNCD                PIC X(03).
                   07  WRK-SSTKJNCD-R              REDEFINES
                       WRK-SSTKJNCD.
                       09  WRK-SSTKJNCD9           PIC 9(03).
           03  WRK-KSNMEI                          PIC X(100).
           03  WRK-PNT                             PIC 9(03).
           03  WRK-STYMD                           PIC X(08).
           03  WRK-EDYMD                           PIC X(08).
           03  WRK-KBN1-G.
               05  WRK-KBN1                        PIC 9(01).
           03  WRK-TEN                             PIC 9(05).
           03  WRK-HYOUKACD                        PIC X(09).
           03  WRK-NUM02X.
               05  WRK-NUM02                       PIC 9(02).
           03  WRK-FROM                            PIC 9(05).
      *
       01  WRK-ROW.
           03  WRK-BTULST-ROW                      PIC S9(9)   BINARY.
      *
       01  TKSN-AREA.
           03  TKSN-MAX                            PIC 9(03).
           03  TKSN-OCC                            OCCURS  28.
               05  TKSN-LBL                        PIC X(36).
               05  TKSN-IDX                        PIC 9(03).
               05  TKSN-SRYCD                      PIC X(09).
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
           03  CONST-H24-AREA.
               05  CONST-H2404-DRHJO-VAL.
                   07                      PIC X(09) VALUE "190135710".
                   07                      PIC X(09) VALUE "190135810".
                   07                      PIC X(09) VALUE "190127110".
                   07                      PIC X(09) VALUE "190145510".
                   07                      PIC X(09) VALUE "190145610".
                   07                      PIC X(09) VALUE "190127210".
                   07                      PIC X(09) VALUE "190127310".
                   07                      PIC X(09) VALUE "190127410".
               05  CONST-H2404-DRHJO-R
                               REDEFINES CONST-H2404-DRHJO-VAL.
                   07  CONST-H2404-DRHJO   PIC X(09) OCCURS 8.
               05  CONST-H2404-KYUKNGHJ-VAL.
                   07                      PIC X(09) VALUE "190145710".
                   07                      PIC X(09) VALUE "190145810".
                   07                      PIC X(09) VALUE "190135910".
                   07                      PIC X(09) VALUE "190136010".
               05  CONST-H2404-KYUKNGHJ-R
                               REDEFINES CONST-H2404-KYUKNGHJ-VAL.
                   07  CONST-H2404-KYUKNGHJ PIC X(09) OCCURS 4.
               05  CONST-H2404-YKNKYUKNGHJ1 PIC X(09) VALUE "190145970".
               05  CONST-H2404-YKNKYUKNGHJ2 PIC X(09) VALUE "190146070".
               05  CONST-H2404-KNGYKNHICHI  PIC X(09) VALUE "190146170".
               05  CONST-H2404-KNGHITU1     PIC X(09) VALUE "190143070".
               05  CONST-H2404-KNGHITU1-SEN PIC X(09) VALUE "190144570".
               05  CONST-H2404-KNGHITU1-TOK PIC X(09) VALUE "190143970".
               05  CONST-H2404-KNGHITU2     PIC X(09) VALUE "190143170".
               05  CONST-H2404-KNGHITU2-SEN PIC X(09) VALUE "190144670".
               05  CONST-H2404-KNGHITU2-TOK PIC X(09) VALUE "190144070".
               05  CONST-H2404-BTUYKZ       PIC X(09) VALUE "190149110".
           03  CONST-SYOUBTUBRMCD-VAL.
               05                      PIC X(09) VALUE "         ".
               05                      PIC X(09) VALUE "188001070".
               05                      PIC X(09) VALUE "188001170".
               05                      PIC X(09) VALUE "188001270".
               05                      PIC X(09) VALUE "188001370".
               05                      PIC X(09) VALUE "188001470".
               05                      PIC X(09) VALUE "188001570".
               05                      PIC X(09) VALUE "188001670".
               05                      PIC X(09) VALUE "188001770".
               05                      PIC X(09) VALUE "188001870".
               05                      PIC X(09) VALUE "188001970".
               05                      PIC X(09) VALUE "188002070".
               05                      PIC X(09) VALUE "188002170".
               05                      PIC X(09) VALUE "188002270".
               05                      PIC X(09) VALUE "188002370".
               05                      PIC X(09) VALUE "188002470".
           03  CONST-SYOUBTUBRMCD-R    REDEFINES CONST-SYOUBTUBRMCD-VAL.
               05  CONST-SYOUBTUBRMCD  PIC X(09) OCCURS      16.
           03  CONST-BTULST-MAX        PIC 9(03) VALUE 50.
           03  CONST-RIREKILST-MAX     PIC 9(03) VALUE 20.
           03  CONST-TOKNYUIN-MAX      PIC 9(03) VALUE 30.
           03  CONST-SRYKA-MAX         PIC 9(03) VALUE 99.
           03  CONST-SBT-MAX           PIC 9(03) VALUE 15.
           03  CONST-YKNKNG-MAX        PIC 9(03) VALUE 10.
           03  CONST-KHNRYO-MAX        PIC 9(03) VALUE 50.
           03  CONST-SYOUBTUBRM-MAX    PIC 9(03) VALUE 16.
           03  CONST-KSN-MAX           PIC 9(03) VALUE 28.
           03  CONST-GKSN-MAX          PIC 9(03) VALUE 16.
           03  CONST-YKNKNGCD-VAL.
               05                      PIC X(09) VALUE "190109870".
               05                      PIC X(09) VALUE "190076070".
               05                      PIC X(09) VALUE "190076170".
               05                      PIC X(09) VALUE "190076370".
               05                      PIC X(09) VALUE "190076470".
           03  CONST-YKNKNGCD-R        REDEFINES CONST-YKNKNGCD-VAL.
               05  CONST-YKNKNGCD-OCC  OCCURS 5.
                   07  CONST-YKNKNGCD  PIC X(09).
           03  CONST-SYOKUDOCD         PIC X(09) VALUE "197000570".
           03  CONST-HOIKUCD           PIC X(09) VALUE "190111370".
           03  CONST-NANBYO            PIC X(09) VALUE "190101770".
           03  CONST-TOKUSYUCD         PIC X(09) VALUE "190101970".
           03  CONST-JIDOCD            PIC X(09) VALUE "190110170".
           03  CONST-SEISINCD          PIC X(09) VALUE "190106970".
           03  CONST-KANGOCD-VAL.
               05                      PIC X(10) VALUE "2190102070".
               05                      PIC X(10) VALUE "3190102170".
               05                      PIC X(10) VALUE "4190102270".
               05                      PIC X(10) VALUE "5190102370".
               05                      PIC X(10) VALUE "6190102470".
           03  CONST-KANGOCD-R         REDEFINES   CONST-KANGOCD-VAL.
               05  CONST-KANGOCD-OCC   OCCURS 5.
                   07  CONST-KANGOCDL  PIC X(01).
                   07  CONST-KANGOCD   PIC X(09).
           03  CONST-KNGTEKCD          PIC X(09) VALUE "190109670".
           03  CONST-DRTEKCD           PIC X(09) VALUE "190117070".
           03  CONST-H2204-DRTEKCD1    PIC X(09) VALUE "190135570".
           03  CONST-KNGHJO-VAL.
               05                      PIC X(11) VALUE "01190103070".
               05                      PIC X(11) VALUE "01190103170".
               05                      PIC X(11) VALUE "01190103270".
               05                      PIC X(11) VALUE "01190103370".
               05                      PIC X(11) VALUE "02190103470".
               05                      PIC X(11) VALUE "02190103570".
               05                      PIC X(11) VALUE "02190103670".
               05                      PIC X(11) VALUE "03190103770".
               05                      PIC X(11) VALUE "03190103870".
               05                      PIC X(11) VALUE "04190103970".
               05                      PIC X(11) VALUE "05190104070".
               05                      PIC X(11) VALUE "06190103770".
               05                      PIC X(11) VALUE "07190103870".
               05                      PIC X(11) VALUE "08190103970".
               05                      PIC X(11) VALUE "09190104070".
           03  CONST-KNGHJOCD-R        REDEFINES   CONST-KNGHJO-VAL.
               05  CONST-KNGHJOCD-OCC  OCCURS 15.
                   07  CONST-KNGHJOCDL PIC X(02).
                   07  CONST-KNGHJOCD  PIC X(09).
           03  CONST-H2204-DRHJO-VAL.
               05                      PIC X(09) VALUE "190135710".
               05                      PIC X(09) VALUE "190135810".
               05                      PIC X(09) VALUE "190127110".
               05                      PIC X(09) VALUE "190127210".
               05                      PIC X(09) VALUE "190127310".
               05                      PIC X(09) VALUE "190127410".
           03  CONST-H2204-DRHJO-R     REDEFINES CONST-H2204-DRHJO-VAL.
               05  CONST-H2204-DRHJO   PIC X(09) OCCURS 6.
           03  CONST-DRHJO-VAL.
               05                      PIC X(09) VALUE "190127110".
               05                      PIC X(09) VALUE "190127210".
               05                      PIC X(09) VALUE "190127310".
               05                      PIC X(09) VALUE "190127410".
           03  CONST-DRHJO-R           REDEFINES CONST-DRHJO-VAL.
               05  CONST-DRHJO         PIC X(09) OCCURS 4.
           03  CONST-JKAIFUKU          PIC X(09) VALUE "190129870".
           03  CONST-YKNKNKYU          PIC X(09) VALUE "190126470".
           03  CONST-SNRHICH1          PIC X(09) VALUE "190126570".
           03  CONST-SNRHICH2          PIC X(09) VALUE "190109670".
           03  CONST-YKNHICH1          PIC X(09) VALUE "190126670".
           03  CONST-YKNHICH2          PIC X(09) VALUE "190126770".
           03  CONST-KYUKNGHJ1         PIC X(09) VALUE "190135910".
           03  CONST-KYUKNGHJ2         PIC X(09) VALUE "190136010".
           03  CONST-H2204-SNRHICH1    PIC X(09) VALUE "190109670".
           03  CONST-H2204-SNRHICH2    PIC X(09) VALUE "190126570".
           03  CONST-H2204-YKNHICH1    PIC X(09) VALUE "190126770".
           03  CONST-H2204-YKNHICH2    PIC X(09) VALUE "190126670".
           03  CONST-SEICHIIKI         PIC X(09) VALUE "190127810".
           03  CONST-HYOUKA            PIC X(09) VALUE "190131570".
           03  CONST-HYOUKA-TOK        PIC X(09) VALUE "190134870".
           03  CONST-HYOUKA-SEN        PIC X(09) VALUE "190134970".
           03  CONST-KJTRIHA           PIC X(09) VALUE "190140870".
           03  CONST-BTUMIKJNCD        PIC X(09) VALUE "188000270".
           03  CONST-KNGHJOCD-MAX      PIC 9(03) VALUE 15.
           03  CONST-5001-KSN-MAX      PIC 9(03) VALUE 14.
           03  CONST-KANSATU           PIC X(02) VALUE "10".
           03  CONST-H180401           PIC X(08) VALUE "20060401".
           03  CONST-H180701           PIC X(08) VALUE "20060701".
           03  CONST-H181001           PIC X(08) VALUE "20061001".
           03  CONST-H200401           PIC X(08) VALUE "20080401".
           03  CONST-H200701           PIC X(08) VALUE "20080701".
           03  CONST-H220401           PIC X(08) VALUE "20100401".
           03  CONST-H240401           PIC X(08) VALUE "20120401".
      *
           COPY    "CPSK5001.INC"  REPLACING  //SYS-//
                                   BY         //TMP-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    病棟管理
           COPY  "CPSK5001.INC".
      *    病室管理
           COPY  "CPSK5002.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    施設基準
           COPY  "CPSK1006.INC".
      *    診療科コード
           COPY  "CPSK1005.INC".
      *    病棟種別
           COPY  "CPSK5101.INC".
      *    平均在院日数
           COPY  "CPSK5102.INC".
      *    看護配置
           COPY  "CPSK5103.INC".
      *    看護師比率
           COPY  "CPSK5104.INC".
      *    看護補助加算
           COPY  "CPSK5105.INC".
      *    夜間勤務等看護加算
           COPY  "CPSK5106.INC".
      *    特定入院料（病棟）
           COPY  "CPSK5110.INC".
      *    特定入院料（病室）
           COPY  "CPSK5111.INC".
      *    看護補助配置
           COPY  "CPSK5115.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    入院料算定マスタ
      *01  NYUINKHN-REC.
      *    COPY  "CPNYUINKHN.INC".
      *    点数マスタ
           COPY  "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    入院基本料算定サブ
           COPY  "CPW20SUB01.INC".
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
           COPY    "W121.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
           WHEN        "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"             ALSO   "CLICKED"
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
           MOVE    SPA-W20FREE     TO  SPA-FREE
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
           MOVE    "W20    "           TO  MCP-WINDOW.
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
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "W21"
               IF    ( SPA-W20-W21-MOTOPG  =   "W20" )
      *
                   INITIALIZE  SPA-W20-W21-AREA
      *
      *            病棟リストを再編集する
                   MOVE    SPA-GMN-W20-BTULST-SEL
                                       TO  WRK-BTULST-SEL
                   PERFORM 3101-EDIT-BTULST-SEC 
                   MOVE    WRK-BTULST-SEL
                                       TO  SPA-GMN-W20-BTULST-SEL
      *
                   PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
                   GO  TO  300-SCREEN-EXT
               ELSE
                   PERFORM 310-SPASET-SEC
      *            病棟情報設定処理
                   PERFORM 310-BTU-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           WHEN    "W201"
      *            病棟リストを再編集する
                   MOVE    SPA-GMN-W20-BTULST-SEL
                                       TO  WRK-BTULST-SEL
                   PERFORM 3101-EDIT-BTULST-SEC 
                   MOVE    WRK-BTULST-SEL
                                       TO  SPA-GMN-W20-BTULST-SEL
      *
                   PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
                   GO  TO  300-SCREEN-EXT
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
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
           INITIALIZE                  SPA-W20-AREA
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W20-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W20-KJNYMD
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
      *    医療機関情報取得処理
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-GET-SYS1001-SEC
      *
      *    コンボ初期処理
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-CMB-SYOKI-SEC
      *
           MOVE    SPA-GMN-W20-EDKBNLST (1)
                                   TO  SPA-GMN-W20-EDKBN-G
      *
      *    病棟管理情報（病棟リストに表示）
           PERFORM 3101-EDIT-BTULST-SEC
      *
      *    入力情報領域初期化処理
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報設定処理（病室画面より遷移時）
      *****************************************************************
       310-BTU-SET-SEC                 SECTION.
      *
           MOVE    1               TO  SPA-GMN-W20-CUR
      *
      *    入力項目領域編集処理
           MOVE    SPA-W20-W21-BTUNUM
                                   TO  WRK-BTUNUM 
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KIJUN-YMD
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           IF    ( FLG-SYS5001     =   ZERO )
      *
               PERFORM 4100-BTUNUM-LSTSEL-SEC
               MOVE    IDX-BTULST  TO  SPA-GMN-W20-BTULST-SEL
      *
      *        履歴データ編集処理
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
      *
               MOVE    2           TO  SPA-GMN-W20-CUR
      *
           END-IF
      *
           .
       310-BTU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボ初期処理
      *****************************************************************
       3101-CMB-SYOKI-SEC          SECTION.
      *
           INITIALIZE  SPA-GMN-W20-CMB-DAT-AREA
                       SPA-NAI-W20-CMB-DAT-AREA
      *
      *    特定入院料取得処理
           PERFORM 3101-GET-SYS5110-SEC
      *
      *    病棟科名取得処理
           PERFORM 3101-GET-SYS1005-SEC
      *
      *    病棟種別取得処理
           PERFORM 3101-GET-SYS5101-SEC
      *
      *    レセプト表示区分
           PERFORM 3101-RECEDSPKBN-GET-SEC
      *
      *    終了区分
           MOVE    2       TO  SPA-GMN-W20-EDKBN-MAX
           MOVE    "1"     TO  SPA-GMN-W20-EDKBNL    (1)
           MOVE    "継続"  TO  SPA-GMN-W20-EDKBNMEIL (1)
           MOVE    "2"     TO  SPA-GMN-W20-EDKBNL    (2)
           MOVE    "廃止"  TO  SPA-GMN-W20-EDKBNMEIL (2)
      *
      *    入院基本料コンボ編集処理
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
      *    加算共通コンボボックス初期処理
           PERFORM 3101-CMB-KSNCOMMON-SET-SEC
      *
           IF    ( WRK-KJN-YUKSTYMD    >=   CONST-H200401 )
            AND  ( SPA-GMN-W20-SBT      =   CONST-KANSATU )
                   PERFORM 31014-CMB-KSN-SYOKI-SEC
           ELSE
               EVALUATE    TRUE
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H240401 )
                   PERFORM 31015-CMB-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H200401 )
                   PERFORM 31013-CMB-KSN-SYOKI-SEC
               WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H180401 )
                   PERFORM 31012-CMB-KSN-SYOKI-SEC
               WHEN    OTHER
                   PERFORM 31011-CMB-KSN-SYOKI-SEC
               END-EVALUATE
           END-IF
      *
           .
       3101-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院加算コンボ初期処理(H14.4〜H18.3)
      *****************************************************************
       31011-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    看護補助加算
           PERFORM 3101-CMB-KNGHJO-SET-SEC
      *
      *    夜間勤務等看護加算
           PERFORM 3101-GET-SYS5106-SEC
      *
      *    プレイルーム、保育士等加算
           PERFORM 3101-CMB-HOIKU-SET-SEC
      *
      *    食堂加算
           PERFORM 3101-CMB-SYOKUDO-SET-SEC
      *
      *    難病患者等入院診療加算
           PERFORM 3101-CMB-NANBYO-SET-SEC
      *
      *    特殊疾患入院施設管理加算
           PERFORM 3101-CMB-TOKUSYU-SET-SEC
      *
      *    児童・思春期精神科入院医療管理加算
           PERFORM 3101-CMB-JIDO-SET-SEC
      *
      *    精神科応急入院施設管理加算
           PERFORM 3101-CMB-SEISIN-SET-SEC
      *
      *    看護配置加算
           PERFORM 3101-CMB-HICH-SET-SEC
      *
           .
       31011-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H18.4〜H20.3)
      *****************************************************************
       31012-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    看護補助加算
           PERFORM 3101-CMB-KNGHJO-SET-SEC
      *
      *    夜間勤務等看護加算
           PERFORM 3101-GET-SYS5106-SEC
      *
      *    プレイルーム、保育士等加算
           PERFORM 3101-CMB-HOIKU-SET-SEC
      *
      *    食堂加算
           PERFORM 3101-CMB-SYOKUDO-SET-SEC
      *
      *    難病患者等入院診療加算
           PERFORM 3101-CMB-NANBYO-SET-SEC
      *
      *    特殊疾患入院施設管理加算
           PERFORM 3101-CMB-TOKUSYU-SET-SEC
      *
      *    児童・思春期精神科入院医療管理加算
           PERFORM 3101-CMB-JIDO-SET-SEC
      *
      *    精神科応急入院施設管理加算
           PERFORM 3101-CMB-SEISIN-SET-SEC
      *
      *    看護配置加算
           PERFORM 3101-CMB-HICH-SET-SEC
      *
      *    看護配置適合加算
           PERFORM 3101-CMB-HICHTEK-SET-SEC
      *
      *    複数医師及び看護配置適合加算
           PERFORM 3101-CMB-DRTEK-SET-SEC
      *
           .
       31012-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H20.4〜)
      *****************************************************************
       31013-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    看護補助加算
           PERFORM 3101-CMB-KNGHJO-SET-SEC
      *
      *    プレイルーム、保育士等加算
           PERFORM 3101-CMB-HOIKU-SET-SEC
      *
      *    食堂加算
           PERFORM 3101-CMB-SYOKUDO-SET-SEC
      *
      *    難病患者等入院診療加算
           PERFORM 3101-CMB-NANBYO-SET-SEC
      *
      *    特殊疾患入院施設管理加算
           PERFORM 3101-CMB-TOKUSYU-SET-SEC
      *
      *    児童・思春期精神科入院医療管理加算
           PERFORM 3101-CMB-JIDO-SET-SEC
      *
      *    精神科応急入院施設管理加算
           PERFORM 3101-CMB-SEISIN-SET-SEC
      *
      *    看護配置加算
           PERFORM 3101-CMB-HICH-SET-SEC
      *
      *    医師事務作業補助体制加算
           PERFORM 3101-CMB-DRHJO-SET-SEC
      *
      *    重症患者回復病棟加算
           PERFORM 3101-CMB-JKAIFUKU-SET-SEC
      *
      *    医師配置適合加算
           PERFORM 3101-CMB-DRTEK-SET-SEC
      *
      *    夜間緊急体制確保加算
           PERFORM 3101-CMB-YKNKNKYU-SET-SEC
      *
      *    看護配置加算１（診療所）
           PERFORM 3101-CMB-SNRHICH1-SET-SEC
      *
      *    看護配置加算２（診療所）
           PERFORM 3101-CMB-SNRHICH2-SET-SEC
      *
      *    夜間看護配置加算１
           PERFORM 3101-CMB-YKNHICH1-SET-SEC
      *
      *    夜間看護配置加算２
           PERFORM 3101-CMB-YKNHICH2-SET-SEC
      *
      *    急性期看護補助体制加算
           PERFORM 3101-CMB-KYUKNGHJ-SET-SEC
      *
      *    精神科地域移行実施加算
           PERFORM 3101-CMB-SEICHIIKI-SET-SEC
      *
      *    医師配置加算２
           PERFORM 3101-CMB-DRTEK2-SET-SEC
      *
      *    一般病棟看護必要度評価加算
           PERFORM 3101-CMB-HYOUKA-SET-SEC
      *
      *    休日リハビリテーション提供体制加算
           PERFORM 3101-CMB-KJTRIHA-SET-SEC
      *
           .
       31013-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(医療観察法)
      *****************************************************************
       31014-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    小規格病棟病床数加算
           PERFORM 3101-CMB-SYOUBTUBRMKSN-SET-SEC
      *
      *    病棟未基準減算
           PERFORM 3101-CMB-BTUMIKJN-SET-SEC
      *
      *    食堂加算
           PERFORM 3101-CMB-SYOKUDO-SET-SEC
      *
           .
       31014-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H24.4〜)
      *****************************************************************
       31015-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    看護補助加算
           PERFORM 3101-CMB-KNGHJO-SET-SEC
      *
      *    プレイルーム、保育士等加算
           PERFORM 3101-CMB-HOIKU-SET-SEC
      *
      *    食堂加算
           PERFORM 3101-CMB-SYOKUDO-SET-SEC
      *
      *    難病患者等入院診療加算
           PERFORM 3101-CMB-NANBYO-SET-SEC
      *
      *    特殊疾患入院施設管理加算
           PERFORM 3101-CMB-TOKUSYU-SET-SEC
      *
      *    精神科応急入院施設管理加算
           PERFORM 3101-CMB-SEISIN-SET-SEC
      *
      *    看護配置加算
           PERFORM 3101-CMB-HICH-SET-SEC
      *
      *    医師事務作業補助体制加算
           PERFORM 3101-CMB-DRHJO-SET-SEC
      *
      *    医師配置適合加算
           PERFORM 3101-CMB-DRTEK-SET-SEC
      *
      *    夜間緊急体制確保加算
           PERFORM 3101-CMB-YKNKNKYU-SET-SEC
      *
      *    看護配置加算１（診療所）
           PERFORM 3101-CMB-SNRHICH1-SET-SEC
      *
      *    看護配置加算２（診療所）
           PERFORM 3101-CMB-SNRHICH2-SET-SEC
      *
      *    夜間看護配置加算１
           PERFORM 3101-CMB-YKNHICH1-SET-SEC
      *
      *    夜間看護配置加算２
           PERFORM 3101-CMB-YKNHICH2-SET-SEC
      *
      *    急性期看護補助体制加算
           PERFORM 3101-CMB-KYUKNGHJ-SET-SEC
      *
      *    夜間急性期看護補助体制加算
           PERFORM 3101-CMB-YKNKYUKNGHJ-SET-SEC
      *
      *    看護職員夜間配置加算
           PERFORM 3101-CMB-KNGYKNHICHI-SET-SEC
      *
      *    精神科地域移行実施加算
           PERFORM 3101-CMB-SEICHIIKI-SET-SEC
      *
      *    医師配置加算２
           PERFORM 3101-CMB-DRTEK2-SET-SEC
      *
      *    一般病棟看護必要度評価加算
           PERFORM 3101-CMB-HYOUKA-SET-SEC
      *
      *    休日リハビリテーション提供体制加算
           PERFORM 3101-CMB-KJTRIHA-SET-SEC
      *
      *    看護必要度評価加算
           PERFORM 3101-CMB-KNGHITU-SET-SEC
      *
      *    病棟薬剤業務実施加算
           PERFORM 3101-CMB-BTUYKZ-SET-SEC
      *
           .
       31015-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    小規格病棟病床数加算コンボ初期処理
      *****************************************************************
       3101-CMB-SYOUBTUBRMKSN-SET-SEC  SECTION.
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   CONST-SYOUBTUBRM-MAX )
      *
               IF    ( IDX-CMB     =   1 )
      *
                   MOVE    "00"    TO  SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                   MOVE    "算定しない"
                                   TO  SPA-GMN-W20-SYOUBTUBRMMEIL
                                                               (IDX-CMB)
      *
               ELSE
                   COMPUTE WRK-NUM02
                       =   IDX-CMB
                   MOVE    WRK-NUM02X
                                   TO  SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                   COMPUTE WRK-NUM02
                       =   IDX-CMB +   13
                   MOVE    WRK-NUM02X
                                   TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-SEC
                   STRING  "小規格病棟病床数加算（"   DELIMITED BY SIZE
                           WRK-KANACHK-OUT-INPUT      DELIMITED BY SPACE
                           "床）"                     DELIMITED BY SIZE
                   INTO    SPA-GMN-W20-SYOUBTUBRMMEIL (IDX-CMB)
                   END-STRING
               END-IF
      *
               COMPUTE SPA-GMN-W20-SYOUBTUBRM-MAX
                   =   SPA-GMN-W20-SYOUBTUBRM-MAX  +   1
      *
           END-PERFORM
      *
           .
       3101-CMB-SYOUBTUBRMKSN-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟未基準減算
      *****************************************************************
       3101-CMB-BTUMIKJN-SET-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-BTUMIKJNL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W20-BTUMIKJNMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-BTUMIKJNL(2)
           MOVE    "算定する"          TO  SPA-GMN-W20-BTUMIKJNMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-BTUMIKJN-MAX
      *
           .
       3101-CMB-BTUMIKJN-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護補助加算コンボ初期処理
      *****************************************************************
       3101-CMB-KNGHJO-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H180401 )
      *
               MOVE    "00"                TO  SPA-GMN-W20-KNGHJOL(1)
               MOVE    "算定しない"        TO  SPA-GMN-W20-KNGHJOMEIL(1)
      *
               MOVE    "06"                TO  SPA-GMN-W20-KNGHJOL(2)
               MOVE    "看護補助加算１"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(2)
      *
               MOVE    "07"                TO  SPA-GMN-W20-KNGHJOL(3)
               MOVE    "看護補助加算１（１３対１入院基本料）"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(3)
      *
               MOVE    "08"                TO  SPA-GMN-W20-KNGHJOL(4)
               MOVE    "看護補助加算２"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(4)
      *
               MOVE    "09"                TO  SPA-GMN-W20-KNGHJOL(5)
               MOVE    "看護補助加算３"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(5)
      *
               MOVE    5                   TO  SPA-GMN-W20-KNGHJO-MAX
           WHEN    OTHER
      *
               MOVE    "00"                TO  SPA-GMN-W20-KNGHJOL(1)
               MOVE    "算定しない"        TO  SPA-GMN-W20-KNGHJOMEIL(1)
      *
               MOVE    "01"                TO  SPA-GMN-W20-KNGHJOL(2)
               MOVE    "４対１看護補助加算"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(2)
      *
               MOVE    "02"                TO  SPA-GMN-W20-KNGHJOL(3)
               MOVE    "５対１看護補助加算"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(3)
      *
               MOVE    "03"                TO  SPA-GMN-W20-KNGHJOL(4)
               MOVE    "６対１看護補助加算"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(4)
      *
               MOVE    "04"                TO  SPA-GMN-W20-KNGHJOL(5)
               MOVE    "１０対１看護補助加算"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(5)
      *
               MOVE    "05"                TO  SPA-GMN-W20-KNGHJOL(6)
               MOVE    "１５対１看護補助加算"
                                           TO  SPA-GMN-W20-KNGHJOMEIL(6)
      *
               MOVE    6               TO  SPA-GMN-W20-KNGHJO-MAX
           END-EVALUATE
      *
           .
       3101-CMB-KNGHJO-SET-EXT.
           EXIT.
      *****************************************************************
      *    加算共通コンボボックス初期処理
      *****************************************************************
       3101-CMB-KSNCOMMON-SET-SEC          SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-KSNCOMMONL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W20-KSNCOMMONMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-KSNCOMMONL(2)
           MOVE    "算定する"          TO  SPA-GMN-W20-KSNCOMMONMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-KSNCOMMON-MAX
      *
           .
       3101-CMB-KSNCOMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    プレイルーム、保育士等加算コンボ初期処理
      *****************************************************************
       3101-CMB-HOIKU-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-HOIKULST-G
      *
           .
       3101-CMB-HOIKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算コンボ初期処理
      *****************************************************************
       3101-CMB-SYOKUDO-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SYOKUDOLST-G
      *
           .
       3101-CMB-SYOKUDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    難病患者等入院診療加算コンボ初期処理
      *****************************************************************
       3101-CMB-NANBYO-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-NANBYOLST-G
      *
           .
       3101-CMB-NANBYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    特殊疾患入院施設管理加算コンボ初期処理
      *****************************************************************
       3101-CMB-TOKUSYU-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-TOKUSYULST-G
      *
           .
       3101-CMB-TOKUSYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    児童・思春期精神科入院医療管理加算コンボ初期処理
      *****************************************************************
       3101-CMB-JIDO-SET-SEC        SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-JIDOL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W20-JIDOMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-JIDOL(2)
           MOVE    "算定する"          TO  SPA-GMN-W20-JIDOMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-JIDO-MAX
      *
           .
       3101-CMB-JIDO-SET-EXT.
           EXIT.
      *****************************************************************
      *    精神科応急入院施設管理加算コンボ初期処理
      *****************************************************************
       3101-CMB-SEISIN-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SEISINLST-G
      *
           .
       3101-CMB-SEISIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算コンボ初期処理
      *****************************************************************
       3101-CMB-HICH-SET-SEC           SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H180401 )
      *
               MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                           TO  SPA-GMN-W20-HICHLST-G
      *
           WHEN    OTHER
      *
               MOVE    "1"                 TO  SPA-GMN-W20-HICHL (1)
               MOVE    "算定しない"        TO  SPA-GMN-W20-HICHMEIL (1)
      *
               MOVE    "2"                 TO  SPA-GMN-W20-HICHL (2)
               MOVE    "算定する（入院基本料３）"
                                           TO  SPA-GMN-W20-HICHMEIL (2)
      *
               MOVE    "3"                 TO  SPA-GMN-W20-HICHL (3)
               MOVE    "算定する（入院基本料４）"
                                           TO  SPA-GMN-W20-HICHMEIL (3)
      *
               MOVE    "4"                 TO  SPA-GMN-W20-HICHL (4)
               MOVE    "算定する（入院基本料５）"
                                           TO  SPA-GMN-W20-HICHMEIL (4)
      *
               MOVE    "5"                 TO  SPA-GMN-W20-HICHL (5)
               MOVE    "算定する（入院基本料６）"
                                           TO  SPA-GMN-W20-HICHMEIL (5)
      *
               MOVE    "6"                 TO  SPA-GMN-W20-HICHL (6)
               MOVE    "算定する（入院基本料７）"
                                           TO  SPA-GMN-W20-HICHMEIL (6)
      *
               MOVE    6                   TO  SPA-GMN-W20-HICH-MAX
           END-EVALUATE
      *
           .
       3101-CMB-HICH-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護配置適合加算コンボ初期処理
      *****************************************************************
       3101-CMB-HICHTEK-SET-SEC        SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W20-HICHTEKL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W20-HICHTEKMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W20-HICHTEKL(2)
           MOVE    "算定する"          TO  SPA-GMN-W20-HICHTEKMEIL(2)
           MOVE    2                   TO  SPA-GMN-W20-HICHTEK-MAX
      *
           .
       3101-CMB-HICHTEK-SET-EXT.
           EXIT.
      *****************************************************************
      *    複数医師及び看護配置適合加算コンボ初期処理
      *****************************************************************
       3101-CMB-DRTEK-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-DRTEKLST-G
      *
           .
       3101-CMB-DRTEK-SET-EXT.
           EXIT.
      *****************************************************************
      *    複数医師及び看護配置適合加算コンボ初期処理
      *****************************************************************
       3101-CMB-DRTEK2-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-DRTEK2LST-G
      *
           .
       3101-CMB-DRTEK2-SET-EXT.
           EXIT.
      *****************************************************************
      *    医師事務作業補助体制加算コンボ初期処理
      *****************************************************************
       3101-CMB-DRHJO-SET-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H240401 )
               MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W20-DRHJOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
               MOVE    "１５対１"      TO  SPA-GMN-W20-DRHJOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
               MOVE    "２０対１"      TO  SPA-GMN-W20-DRHJOMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
               MOVE    "２５対１"      TO  SPA-GMN-W20-DRHJOMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
               MOVE    "３０対１"      TO  SPA-GMN-W20-DRHJOMEIL(5)
               MOVE    "6"             TO  SPA-GMN-W20-DRHJOL(6)
               MOVE    "４０対１"      TO  SPA-GMN-W20-DRHJOMEIL(6)
               MOVE    "7"             TO  SPA-GMN-W20-DRHJOL(7)
               MOVE    "５０対１"      TO  SPA-GMN-W20-DRHJOMEIL(7)
               MOVE    "8"             TO  SPA-GMN-W20-DRHJOL(8)
               MOVE    "７５対１"      TO  SPA-GMN-W20-DRHJOMEIL(8)
               MOVE    "9"             TO  SPA-GMN-W20-DRHJOL(9)
               MOVE    "１００対１"    TO  SPA-GMN-W20-DRHJOMEIL(9)
               MOVE    9               TO  SPA-GMN-W20-DRHJO-MAX
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H220401 )
               MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W20-DRHJOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
               MOVE    "１５対１"      TO  SPA-GMN-W20-DRHJOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
               MOVE    "２０対１"      TO  SPA-GMN-W20-DRHJOMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
               MOVE    "２５対１"      TO  SPA-GMN-W20-DRHJOMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
               MOVE    "５０対１"      TO  SPA-GMN-W20-DRHJOMEIL(5)
               MOVE    "6"             TO  SPA-GMN-W20-DRHJOL(6)
               MOVE    "７５対１"      TO  SPA-GMN-W20-DRHJOMEIL(6)
               MOVE    "7"             TO  SPA-GMN-W20-DRHJOL(7)
               MOVE    "１００対１"    TO  SPA-GMN-W20-DRHJOMEIL(7)
               MOVE    7               TO  SPA-GMN-W20-DRHJO-MAX
           WHEN    OTHER
               MOVE    "1"             TO  SPA-GMN-W20-DRHJOL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W20-DRHJOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W20-DRHJOL(2)
               MOVE    "２５対１"      TO  SPA-GMN-W20-DRHJOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W20-DRHJOL(3)
               MOVE    "５０対１"      TO  SPA-GMN-W20-DRHJOMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W20-DRHJOL(4)
               MOVE    "７５対１"      TO  SPA-GMN-W20-DRHJOMEIL(4)
               MOVE    "5"             TO  SPA-GMN-W20-DRHJOL(5)
               MOVE    "１００対１"    TO  SPA-GMN-W20-DRHJOMEIL(5)
               MOVE    5               TO  SPA-GMN-W20-DRHJO-MAX
           END-EVALUATE
      *
      *
           .
       3101-CMB-DRHJO-SET-EXT.
           EXIT.
      *****************************************************************
      *    一般病棟看護必要度評価加算コンボ初期処理
      *****************************************************************
       3101-CMB-HYOUKA-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-HYOUKALST-G
      *
           .
       3101-CMB-HYOUKA-SET-EXT.
           EXIT.
      *****************************************************************
      *    休日リハビリテーション提供体制加算コンボ初期処理
      *****************************************************************
       3101-CMB-KJTRIHA-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-KJTRIHALST-G
      *
           .
       3101-CMB-KJTRIHA-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護必要度評価加算
      *****************************************************************
       3101-CMB-KNGHITU-SET-SEC     SECTION.
      *
           MOVE    "1"             TO  SPA-GMN-W20-KNGHITUL(1)
           MOVE    "算定しない"    TO  SPA-GMN-W20-KNGHITUMEIL(1)
      *
           MOVE    "2"             TO  SPA-GMN-W20-KNGHITUL(2)
           MOVE    "看護必要度加算１"
                                   TO  SPA-GMN-W20-KNGHITUMEIL(2)
      *
           MOVE    "3"             TO  SPA-GMN-W20-KNGHITUL(3)
           MOVE    "看護必要度加算２"
                                   TO  SPA-GMN-W20-KNGHITUMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-W20-KNGHITU-MAX
      *
           .
       3101-CMB-KNGHITU-SET-EXT.
           EXIT.
      *****************************************************************
      *    病棟薬剤業務実施加算
      *****************************************************************
       3101-CMB-BTUYKZ-SET-SEC     SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-BTUYKZLST-G
      *
           .
       3101-CMB-BTUYKZ-SET-EXT.
           EXIT.
      *****************************************************************
      *    重症患者回復病棟加算コンボ初期処理
      *****************************************************************
       3101-CMB-JKAIFUKU-SET-SEC          SECTION.
      *
           MOVE    "1"             TO  SPA-GMN-W20-JKAIFUKUL(1)
           MOVE    "算定しない"    TO  SPA-GMN-W20-JKAIFUKUMEIL(1)
           MOVE    "2"             TO  SPA-GMN-W20-JKAIFUKUL(2)
           MOVE    "算定する"      TO  SPA-GMN-W20-JKAIFUKUMEIL(2)
           MOVE    2               TO  SPA-GMN-W20-JKAIFUKU-MAX
      *
           .
       3101-CMB-JKAIFUKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    夜間緊急体制確保加算コンボ初期処理
      *****************************************************************
       3101-CMB-YKNKNKYU-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-YKNKNKYULST-G
      *
           .
       3101-CMB-YKNKNKYU-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算１（診療所）
      *****************************************************************
       3101-CMB-SNRHICH1-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-SNRHICH1LST-G
      *
           .
       3101-CMB-SNRHICH1-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算２（診療所）
      *****************************************************************
       3101-CMB-SNRHICH2-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                   TO  SPA-GMN-W20-SNRHICH2LST-G
      *
           .
       3101-CMB-SNRHICH2-SET-EXT.
           EXIT.
      *****************************************************************
      *    夜間看護配置加算１
      *****************************************************************
       3101-CMB-YKNHICH1-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-YKNHICH1LST-G
      *
           .
       3101-CMB-YKNHICH1-SET-EXT.
           EXIT.
      *****************************************************************
      *    夜間看護配置加算２
      *****************************************************************
       3101-CMB-YKNHICH2-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-YKNHICH2LST-G
      *
           .
       3101-CMB-YKNHICH2-SET-EXT.
           EXIT.
      *****************************************************************
      *    急性期看護補助体制加算
      *****************************************************************
       3101-CMB-KYUKNGHJ-SET-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H240401 )
               MOVE    "1"             TO  SPA-GMN-W20-KYUKNGHJL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W20-KYUKNGHJMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KYUKNGHJL(2)
               MOVE    "２５対１（５割以上） "
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KYUKNGHJL(3)
               MOVE    "２５対１（５割未満） "
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(3)
      *
               MOVE    "4"             TO  SPA-GMN-W20-KYUKNGHJL(4)
               MOVE    "５０対１"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(4)
      *
               MOVE    "5"             TO  SPA-GMN-W20-KYUKNGHJL(5)
               MOVE    "７５対１"
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(5)
      *
               MOVE    5               TO  SPA-GMN-W20-KYUKNGHJ-MAX
           WHEN   OTHER
               MOVE    "1"             TO  SPA-GMN-W20-KYUKNGHJL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W20-KYUKNGHJMEIL(1)
      *
               MOVE    "2"             TO  SPA-GMN-W20-KYUKNGHJL(2)
               MOVE    "急性期看護補助体制加算１" 
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(2)
      *
               MOVE    "3"             TO  SPA-GMN-W20-KYUKNGHJL(3)
               MOVE    "急性期看護補助体制加算２" 
                                       TO  SPA-GMN-W20-KYUKNGHJMEIL(3)
      *
               MOVE    3               TO  SPA-GMN-W20-KYUKNGHJ-MAX
           END-EVALUATE
      *
           .
       3101-CMB-KYUKNGHJ-SET-EXT.
           EXIT.
      *****************************************************************
      *    夜間急性期看護補助体制加算
      *****************************************************************
       3101-CMB-YKNKYUKNGHJ-SET-SEC    SECTION.
      *
               MOVE    "1"         TO  SPA-GMN-W20-YKNKYUKNGHJL(1)
               MOVE    "算定しない"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(1)
      *
               MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJL(2)
               MOVE    "５０対１"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(2)
      *
               MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJL(3)
               MOVE    "１００対１"
                                   TO  SPA-GMN-W20-YKNKYUKNGHJMEIL(3)
      *
               MOVE    3           TO  SPA-GMN-W20-YKNKYUKNGHJ-MAX
      *
           .
       3101-CMB-YKNKYUKNGHJ-SET-EXT.
           EXIT.
      *****************************************************************
      *    看護職員夜間配置加算
      *****************************************************************
       3101-CMB-KNGYKNHICHI-SET-SEC    SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-KNGYKNHICHILST-G
      *
           .
       3101-CMB-KNGYKNHICHI-SET-EXT.
           EXIT.
      *****************************************************************
      *    精神科地域移行実施加算
      *****************************************************************
       3101-CMB-SEICHIIKI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-W20-KSNCOMMONLST-G
                                       TO  SPA-GMN-W20-SEICHIIKILST-G
      *
           .
       3101-CMB-SEICHIIKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    医療機関情報取得処理
      *****************************************************************
       3101-GET-SYS1001-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           MOVE    SYS-1001-HOSPSBT
                                   TO  SPA-NAI-W20-HOSPSBT
      *
           .
       3101-GET-SYS1001-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料取得処理
      *****************************************************************
       3101-GET-SYS5110-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5110"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-TOKNYUIN-MAX )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5110-REC
      *
               IF    ( SYS-5110-SEIKATU-RYOKBN NOT =   SPACE )
                OR   ( SYS-5110-KBNCD  =   "24" OR "25" )
                   CONTINUE
               ELSE
      *
                   COMPUTE IDX-CMB =   IDX-CMB +   1
                   MOVE    IDX-CMB     TO  SPA-GMN-W20-TOKNYUIN-MAX
      *
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                               TO  SPA-GMN-W20-TOKNYUINMEIL
                                                         (IDX-CMB)
      *
                   MOVE    SYS-5110-KBNCD
                               TO  SPA-GMN-W20-TOKNYUINL-KBNCD
                                                         (IDX-CMB)
                   MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM2
                               TO  SPA-GMN-W20-TOKNYUINL-TANMEI
                                                         (IDX-CMB)
                   MOVE    SYS-5110-SRYCD
                               TO  SPA-GMN-W20-TOKNYUINL-SRYCD
                                                         (IDX-CMB)
                   MOVE    SYS-5110-SRYCD  TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-KOUHYOJYUNNUM
                                       TO  SPA-GMN-W20-TOKNYUINL-JUNNUM
                                                         (IDX-CMB)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE IDXS    =   CONST-TOKNYUIN-MAX  +   1
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-W20-TOKNYUIN-MAX )
      *
               COMPUTE WRK-FROM    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    WRK-FROM   BY  1
                       UNTIL ( IDXB    >   SPA-GMN-W20-TOKNYUIN-MAX )
                   IF    ( SPA-GMN-W20-TOKNYUINL-JUNNUM (IDXA)
                               >   SPA-GMN-W20-TOKNYUINL-JUNNUM (IDXB))
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXA)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXS)
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXB)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXA)
                       MOVE    SPA-GMN-W20-TOKNYUINLST-OCC (IDXS)
                               TO  SPA-GMN-W20-TOKNYUINLST-OCC (IDXB)
                   END-IF
               END-PERFORM
      *
               COMPUTE WRK-NUM02   =   IDXA
               MOVE    WRK-NUM02X  TO  SPA-GMN-W20-TOKNYUINL (IDXA)
      *
           END-PERFORM
      *
           .
       3101-GET-SYS5110-EXT.
           EXIT.
      *****************************************************************
      *    診療科コード取得処理
      *****************************************************************
       3101-GET-SYS1005-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-SRYKA-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-SRYKA-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD
                           TO  SPA-GMN-W20-SRYKAL   (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME
                           TO  SPA-GMN-W20-SRYKAMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS1005-EXT.
           EXIT.
      *****************************************************************
      *    病棟種別取得処理
      *****************************************************************
       3101-GET-SYS5101-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5101"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-SBT-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-SBT-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5101-REC
      *
               MOVE    SYS-5101-KBNCD
                           TO  SPA-GMN-W20-SBTL   (IDX-CMB)
               MOVE    SYS-5101-BTU-SBT-NM
                           TO  SPA-GMN-W20-SBTMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS5101-EXT.
           EXIT.
      *****************************************************************
      *    夜間勤務等看護加算取得処理
      *****************************************************************
       3101-GET-SYS5106-SEC        SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5106"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-CMB
      *
           COMPUTE IDX-CMB =   IDX-CMB +   1
           MOVE    "0"             TO  SPA-GMN-W20-YKNKNGL   (IDX-CMB)
           MOVE    "算定しない"    TO SPA-GMN-W20-YKNKNGMEIL (IDX-CMB)
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-CMB        >=   CONST-YKNKNG-MAX )
      *
               COMPUTE IDX-CMB =   IDX-CMB +   1
               MOVE    IDX-CMB         TO  SPA-GMN-W20-YKNKNG-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5106-REC
      *
               MOVE    SYS-5106-KBNCD(2:1)
                           TO  SPA-GMN-W20-YKNKNGL   (IDX-CMB)
               MOVE    SYS-5106-BTU-YAKANKNGKSN-NM
                           TO  SPA-GMN-W20-YKNKNGMEIL(IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-GET-SYS5106-EXT.
           EXIT.
      *****************************************************************
      *    病棟管理情報検索処理
      *****************************************************************
       3101-EDIT-BTULST-SEC        SECTION.
      *
      *    システム日付の内容でコンボを再編集
           MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-CMB-SYOKI-SEC
      *
           INITIALIZE                  SPA-GMN-W20-BTULST
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W20-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY7-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( IDX-LST        >=   CONST-BTULST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
               MOVE    IDX-LST TO  SPA-GMN-W20-BTULST-MAX
      *
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *        病棟リスト編集
               PERFORM 31011-EDIT-BTULST-SEC
      *
               PERFORM 900-SYSKANRI-KEY7-FET-SEC
      *
           END-PERFORM
      *
      **   収容人数設定処理
           PERFORM 31011-GET-BTU-NINZU-SEC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1               TO  FLG-BTULST-ROW
      *
           .
       3101-EDIT-BTULST-EXT.
           EXIT.
      *****************************************************************
      *    レセプト表示区分取得処理
      *****************************************************************
       3101-RECEDSPKBN-GET-SEC         SECTION.
      *
           INITIALIZE  SPA-GMN-W20-RECEDSPKBNLST-G
                       SPA-NAI-W20-RECEDSPKBNLST-G
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD   >=    CONST-H181001 )
               MOVE    "01"    TO  SPA-GMN-W20-RECEDSPKBNL (1)
               MOVE    "02"    TO  SPA-GMN-W20-RECEDSPKBNL (2)
               MOVE    "07"    TO  SPA-GMN-W20-RECEDSPKBNL (3)
      *
               MOVE    "精神"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(1)
               MOVE    "結核"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(2)
               MOVE    "療養"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(3)
      *
               MOVE    "02"    TO  SPA-NAI-W20-RECEDSPKBNLST (1)
               MOVE    "03"    TO  SPA-NAI-W20-RECEDSPKBNLST (2)
               MOVE    "04"    TO  SPA-NAI-W20-RECEDSPKBNLST (3)
      *
               MOVE    3       TO  SPA-GMN-W20-RECEDSPKBN-MAX
           ELSE
      *
               MOVE    "01"    TO  SPA-GMN-W20-RECEDSPKBNL (1)
               MOVE    "02"    TO  SPA-GMN-W20-RECEDSPKBNL (2)
               MOVE    "03"    TO  SPA-GMN-W20-RECEDSPKBNL (3)
               MOVE    "05"    TO  SPA-GMN-W20-RECEDSPKBNL (4)
               MOVE    "07"    TO  SPA-GMN-W20-RECEDSPKBNL (5)
               MOVE    "08"    TO  SPA-GMN-W20-RECEDSPKBNL (6)
               MOVE    "09"    TO  SPA-GMN-W20-RECEDSPKBNL (7)
      *
               MOVE    "精神"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(1)
               MOVE    "結核"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(2)
               MOVE    "特例"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(3)
               MOVE    "老人"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(4)
               MOVE    "療養"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(5)
               MOVE    "複合"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(6)
               MOVE    "複療"  TO  SPA-GMN-W20-RECEDSPKBNMEIL(7)
      *
               MOVE    "02"    TO  SPA-NAI-W20-RECEDSPKBNLST (1)
               MOVE    "03"    TO  SPA-NAI-W20-RECEDSPKBNLST (2)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (3)
               MOVE    "07"    TO  SPA-NAI-W20-RECEDSPKBNLST (4)
               MOVE    "04"    TO  SPA-NAI-W20-RECEDSPKBNLST (5)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (6)
               MOVE    "**"    TO  SPA-NAI-W20-RECEDSPKBNLST (7)
      *
               MOVE    7           TO  SPA-GMN-W20-RECEDSPKBN-MAX
           END-IF
      *
           .
       3101-RECEDSPKBN-GET-EXT.
           EXIT.
      *****************************************************************
      *    病棟リスト編集処理
      *****************************************************************
       31011-EDIT-BTULST-SEC           SECTION.
      *
      *    病棟番号
           MOVE    SYS-5001-BTU-NUM
                           TO  SPA-GMN-W20-TBTUNUM (IDX-LST)
      *    病棟名称
           MOVE    SYS-5001-BTU-NAME
                           TO  SPA-GMN-W20-TNAME (IDX-LST)
      *    短縮病棟名称
           MOVE    SYS-5001-BTU-TANNAME
                           TO  SPA-GMN-W20-TTANNAME (IDX-LST)
      *
      **   病棟種別
           MOVE    SYS-5001-BTU-SBT    TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           MOVE    WRK-CMB-ITM (4:)
                           TO  SPA-GMN-W20-TSBT (IDX-LST)
      *
      *    特定入院料
           MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           MOVE    WRK-CMB-ITM2
                           TO  SPA-GMN-W20-TTOKNYUIN (IDX-LST)
      *
      *    病棟科名
           MOVE    SYS-5001-BTU-KA     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM (4:)
                           TO  SPA-GMN-W20-TSRYKA (IDX-LST)
      *
           MOVE    SYS-5001-BTU-KHNSRYCD
                           TO  SPA-GMN-W20-TKHNSRYCD (IDX-LST)
      *
      *    入院基本点数をサブプロより取得する
           INITIALIZE                      W20SUB01-LNK
           MOVE    "1"                 TO  W20SUB01-I-KBN
           IF    ( SYS-5001-KHNSRYCD   NOT =   SPACE )
               MOVE    SYS-5001-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           ELSE
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           END-IF
           MOVE    SYS-5001-STYUKYMD   TO  W20SUB01-I-KJNYMD
      *
           CALL    "ORCGW20SUB01"      USING
                                       W20SUB01-LNK
                                       SPA-AREA
           IF    ( W20SUB01-O-RC   =   ZERO )
               MOVE    W20SUB01-O-NAME
                                   TO  SPA-GMN-W20-TKHNRYO (IDX-LST)
               MOVE    W20SUB01-O-KHNTEN
                                   TO  SPA-GMN-W20-TKHNTEN (IDX-LST)
           END-IF
      *
      *    有効開始日
           MOVE    SYS-5001-STYUKYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W20-TYUKOSTYMD
                                                           (IDX-LST)
      *
      *    有効終了日
           MOVE    SYS-5001-EDYUKYMD   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-W20-TYUKOEDYMD
                                                           (IDX-LST)
      *
           .
       31011-EDIT-BTULST-EXT.
           EXIT.
      *****************************************************************
      *    病棟人数取得処理
      *****************************************************************
       31011-GET-BTU-NINZU-SEC     SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST  >  SPA-GMN-W20-BTULST-MAX )
      *
               INITIALIZE                  WRK-BTU-NINZU
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5002"          TO  SYS-KANRICD
               MOVE    SPA-GMN-W20-TBTUNUM (IDX-LST)
                                       TO  SYS-KBNCD
               MOVE    "%"             TO  SYS-KBNCD(3:1)
               MOVE    SPA-NAI-W20-KJNYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
                   MOVE    SYSKANRI-REC
                                       TO  SYS-5002-REC
      *
                   COMPUTE WRK-BTU-NINZU
                       =   WRK-BTU-NINZU   +   SYS-5002-BRM-NINZU
      *
                   PERFORM 900-SYSKANRI-KEY-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               MOVE    WRK-BTU-NINZU
                                   TO  SPA-GMN-W20-TNINZU (IDX-LST)
      *
            END-PERFORM
      *
           .
       31011-GET-BTU-NINZU-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "2002"
                       PERFORM 4201-EDYUKYMD-UPDATE-SEC
                       IF    ( SPA-ERRCD   =   SPACE )
                           PERFORM 4201-TUIKA-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    101         TO      SPA-GMN-W20-CUR
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    病室管理情報画面へ
           WHEN    "CLICKED"       ALSO    "B09"
               IF  ( SPA-W20-W21-MOTOPG    =   SPACE )
                   PERFORM 420-BYOSITU-KANRI-SEC
               END-IF
      *    入院料加算設定画面へ
           WHEN    "CLICKED"       ALSO    "B10"
               IF (SPA-GMN-W20-BTUNUM    NOT =   ZERO)   AND
                  (SPA-GMN-W20-SELNUM    NOT =   ZERO)
                   IF   SPA-GMN-W20-SBT  NOT =   "10"
                       PERFORM 420-W201-SYORI-SEC
                   ELSE
                       MOVE   "1109"   TO      SPA-ERRCD
                   END-IF
               ELSE
                   MOVE   "1108"   TO      SPA-ERRCD
               END-IF
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
      *    病棟番号
           WHEN    "ACTIVATE"      ALSO    "BTUNUM"
                   PERFORM 4100-BTUNUM-CHK-SEC
      *
      *    病棟種別
           WHEN    "ACTIVATE"      ALSO    "SBT"
                   PERFORM 4100-SBT-CHK-SEC
      *    基準日
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
                   PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4100-SELNUM-CHK-SEC
      *    病棟リスト
           WHEN    ANY             ALSO    "BTULST"
               PERFORM 4100-BTULST-CHK-SEC
      *    履歴リスト
           WHEN    ANY             ALSO    "RIREKILST"
                   PERFORM 4100-RIREKILST-CHK-SEC
      *    入院基本料
           WHEN    "ACTIVATE"      ALSO    "KHNRYO"
      *
      *        入院基本料チェック処理
               PERFORM 4100-KHNRYO-CHK-SEC
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
           MOVE    ZERO        TO      SPA-GMN-W20-CUR
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
      *    病棟名称
           MOVE    W20-NAME        TO  SPA-GMN-W20-NAME
      *
      *    短縮病棟名
           MOVE    W20-TANNAME     TO  SPA-GMN-W20-TANNAME
      *
      *    特定入院料
           MOVE    W20-TOKUTEINYUIN
                                   TO  SPA-GMN-W20-TOKNYUIN-G
      *
      *    病棟科名
           MOVE    W20-SRYKA       TO  SPA-GMN-W20-SRYKA-G
      *
      *    レセプト表示区分
           MOVE    W20-RECEDSPKBN  TO  SPA-GMN-W20-RECEDSPKBN-G
      *
      *    有効期間開始日
           MOVE    W20-YUKOSTYMD   TO  SPA-GMN-W20-YUKOSTYMD
      *
      *    有効期間終了日
           MOVE    W20-YUKOEDYMD   TO  SPA-GMN-W20-YUKOEDYMD
      *
      *    病棟終了区分
           MOVE    W20-EDKBN       TO  SPA-GMN-W20-EDKBN-G
      *
           IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
      *        小規模病棟病床数加算
               MOVE    W20-KNGHJO  TO  SPA-GMN-W20-SYOUBTUBRM-G
           ELSE
      *        看護補助加算
               MOVE    W20-KNGHJO  TO  SPA-GMN-W20-KNGHJO-G
           END-IF
      *
      *    入院料加算
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   TO  IDX-GKSN
               IF    ( IDX-GKSN     >   ZERO )
                   MOVE    W20-KSN (IDX-GKSN)
                                   TO  SPA-GMN-W20-KSNCD-G
                                                      (IDX-KSN)
               END-IF
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
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟番号チェック処理
               PERFORM 4101-BTUNUM-CHK-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 420-CLEAR-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間開始日チェック処理
               PERFORM 4101-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間終了日チェック処理
               PERFORM 4101-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟名称チェック処理
               PERFORM 4101-NAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        短縮病棟名チェック処理
               PERFORM 4101-TANNAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        特定入院料チェック処理
               PERFORM 4101-TOKUTEINYUIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟科名チェック処理
               PERFORM 4101-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟種別チェック処理
               PERFORM 4101-SBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        レセプト表示区分チェック処理
               PERFORM 4101-RECEDSPKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟継続区分チェック処理
               PERFORM 4101-EDKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        入院基本料チェック
               PERFORM 4101-KHNRYO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    TRUE
               WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
      *            小規格病棟病床数加算
                   PERFORM 4101-SYOUBTUBRM-CHK-SEC
               WHEN  ( SPA-NAI-W20-HOSPSBT     =   1 )
      *            看護補助加算
                   PERFORM 4101-KNGHJO-CHK-SEC
               END-EVALUATE
           END-IF
      *
      *    入院料加算チェック
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    TRUE
               WHEN    ( SPA-GMN-W20-SBT   =   CONST-KANSATU )
                   PERFORM 41021-KNST-KSN-CHK-SEC
               WHEN    OTHER
                   PERFORM 41021-KSN-CHK-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
                   CONTINUE
               ELSE
      *            入院加算情報チェック処理-施設基準チェック処理
                   PERFORM 4901-SSTKIJUN-CHK-SEC
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療観察法入院料加算チェック処理
      *****************************************************************
       41021-KNST-KSN-CHK-SEC          SECTION.
      *
           IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
               EVALUATE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
               WHEN    CONST-BTUMIKJNCD
                   PERFORM 4101-BTUMIKJN-CHK-SEC
               WHEN    CONST-SYOKUDOCD
                   PERFORM 4101-SYOKUDO-CHK-SEC
               END-EVALUATE
      *
           END-IF
      *
           .
       41021-KNST-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算チェック処理
      *****************************************************************
       41021-KSN-CHK-SEC           SECTION.
      *
           IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
               MOVE    SPA-GMN-W20-KSNCD (IDX-KSN)
                                   TO  WRK-CMB-CD
      *
               MOVE    SPACE       TO  WRK-CMB-ITM-G
               MOVE    SPA-GMN-W20-KSNLST-MAX (IDX-KSN)
                                   TO  WRK-MAX
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   WRK-MAX )
                        OR   ( WRK-CMB-ITM-G   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-W20-KSNCDL (IDX-KSN IDXL)
                                   =   SPA-GMN-W20-KSNCD (IDX-KSN))
                       MOVE    SPA-GMN-W20-KSNLST (IDX-KSN IDXL)
                                   TO  WRK-CMB-ITM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CMB-ITM =   SPACE )
                   MOVE    SPA-GMN-W20-KSNLST (IDX-KSN 1)
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               ELSE
                   MOVE    WRK-CMB-ITM
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               END-IF
      *
           END-IF
      *
           .
       41021-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF    ( SPA-W20-W21-MOTOPG  =   SPACE )
               MOVE    "W01"               TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-W20-W21-MOTOPG  TO  SPA-SAKIPG
           END-IF
           MOVE    "W20"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    条件クリア処理（コラムリストも再編集）
      *****************************************************************
       421-CLEAR-SEC               SECTION.
      *
           PERFORM 3101-EDIT-BTULST-SEC
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           .
       421-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
      *    入力項目入力時チェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        共通関連チェック
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    1                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               EVALUATE    FLG-KIKAN-DABURI
               WHEN    ZERO
                   PERFORM 4201-TUIKA-SEC
               WHEN    1
                   MOVE    "2002"      TO  SPA-WIDCD
               WHEN    OTHER
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-EVALUATE
           END-IF
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       4201-TUIKA-SEC               SECTION.
      *
      *    病棟終了区分が廃止の場合、
      *    病室管理情報（有効期間終了日）を更新する
           IF    ( SPA-GMN-W20-EDKBN   =   "2" )
      *        病室管理情報更新処理
               PERFORM 4201-BRM-UPDATE-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4201-TUIKA-EXT
               END-IF
      *
           END-IF
      *
      *    病棟情報追加編集処理
           PERFORM 4201-BTU-INSERT-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-TUIKA-EXT
           END-IF
      *
           PERFORM 421-CLEAR-SEC
      *
      *    病棟リスト最大件数チェック
           IF    ( SPA-GMN-W20-BTULST-MAX  >=  CONST-BTULST-MAX )
               MOVE    "1001"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-GMN-W20-BTULST-SEL  =   SPA-GMN-W20-BTULST-MAX )
               MOVE    2           TO  FLG-BTULST-ROW
           ELSE
               MOVE    ZERO        TO  FLG-BTULST-ROW
           END-IF
      *
           .
       4201-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    有効期間変更処理
      *****************************************************************
       4201-EDYUKYMD-UPDATE-SEC        SECTION.
      *
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
      *
               MOVE    SYS-5001-REC    TO  SYSKANRI-REC
               MOVE    SYS-KEY         TO  SYSUPD-KEY
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  SYS-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1205"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4201-EDYUKYMD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
      *    入力項目入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    リストに該当番号の病棟が存在するかチェック
           PERFORM 4100-BTUNUM-LSTSEL-SEC
           IF    ( IDX-BTULST      =   ZERO )
               MOVE    "1002"      TO  SPA-ERRCD
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    共通関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    有効期間の開始日と終了日が同時に変更されていないこと
           IF    ( SPA-NAI-W20-YUKOSTYMD
                                   NOT =   SPA-NAI-W20-YUKOSTYMD-OLD )
            AND
                 ( SPA-NAI-W20-YUKOEDYMD
                                   NOT =   SPA-NAI-W20-YUKOEDYMD-OLD )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    既存の履歴データをチェックし、期間の重複したデータが
      *    存在しないこと
           MOVE    2                   TO  FLG-UPD-KBN
           PERFORM 4100-RIREKI-CHK-SEC
           IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
               MOVE    "1104"          TO  SPA-ERRCD
               MOVE    51              TO  SPA-GMN-W20-CUR
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    病棟終了区分が廃止の場合、
      *    病室管理情報（有効期間終了日）を更新する
           IF    ( SPA-GMN-W20-EDKBN   =   "2" )
      *        病室管理情報更新処理
               PERFORM 4201-BRM-UPDATE-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   GO  TO  420-HENKOU-EXT
               END-IF
      *
           END-IF
      *
      *    病棟管理情報更新処理
           PERFORM 4201-BTU-UPDATE-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *
           PERFORM 421-CLEAR-SEC
      *
           MOVE    ZERO            TO  FLG-BTULST-ROW
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
      *
      *    病棟情報検索処理
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                    OR   ( FLG-KIKAN-DABURI    NOT =   ZERO )
      *
      *        有効期間が重複するデータがないかチェック
      *        （変更処理の場合、元データは対象外とする）
               IF    ( FLG-UPD-KBN         =  2 )
                AND  ( SPA-NAI-W20-YUKOSTYMD-OLD 
                                           =   SYS-5001-STYUKYMD )
                AND  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                           =   SYS-5001-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-W20-YUKOEDYMD
                                           <   SYS-5001-STYUKYMD )
                     OR  ( SPA-NAI-W20-YUKOSTYMD
                                           >   SYS-5001-EDYUKYMD )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-W20-YUKOSTYMD
                                           >   SYS-5001-STYUKYMD )
                        AND  ( SPA-NAI-W20-YUKOSTYMD
                                          <=   SYS-5001-EDYUKYMD )
                           MOVE    1       TO  FLG-KIKAN-DABURI
                       ELSE
                           MOVE    2       TO  FLG-KIKAN-DABURI
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
      *    病棟番号チェック処理
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 420-CLEAR-SEC
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *   リストに該当番号の病棟が存在するかチェック
           PERFORM 4100-BTUNUM-LSTSEL-SEC
           IF    ( IDX-BTULST      =   ZERO )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *    削除する病棟の有効期間内に病室の情報が存在しないかチェック
           PERFORM 4200-BRM-SONZAI-CHK-SEC
           IF    ( FLG-BRM-SONZAI  =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1006"      TO  SPA-ERRCD
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
           MOVE    "2001"          TO  SPA-WIDCD
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
      *    病棟情報削除処理
           PERFORM  4201-BTU-DELETE-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  421-SAKUJO-EXT
           END-IF
      *
           PERFORM 421-CLEAR-SEC
      *
           MOVE    ZERO            TO  FLG-BTULST-ROW
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    病室管理情報存在チェック処理
      *****************************************************************
       4200-BRM-SONZAI-CHK-SEC         SECTION.
      *
      *    削除する病棟の有効期間内に病室の情報が存在しないかチェック
      *    する
           MOVE    ZERO            TO  FLG-BRM-SONZAI
      *
           PERFORM 900-SYSKANRI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-BRM-SONZAI      =   1 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD-OLD
                                      >=   SYS-5002-EDYUKYMD )
                 OR  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                      <=   SYS-5002-STYUKYMD )
                   CONTINUE
               ELSE
      *
                   MOVE    1       TO  FLG-BRM-SONZAI
      *
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
       4200-BRM-SONZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病室管理へ
      *****************************************************************
       420-BYOSITU-KANRI-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE  SPA-W20-W21-AREA
      *
      *    病棟が選択されている場合は、病室管理画面に病棟番号を引継ぐ
           IF    ( SPA-GMN-W20-BTULST-SEL
                                       NOT =   ZERO )
                 MOVE    SPA-GMN-W20-BTULST-SEL 
                                       TO  IDX1
                 MOVE    SPA-GMN-W20-TBTUNUM (IDX1)
                                       TO  SPA-W20-W21-BTUNUM
           END-IF
      *
           MOVE    "W21"               TO  SPA-SAKIPG
           MOVE    "W20"               TO  SPA-MOTOPG
                                           SPA-W20-W21-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       420-BYOSITU-KANRI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料加算設定へ
      *****************************************************************
       420-W201-SYORI-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD
                                   NOT =   SPA-NAI-W20-YUKOSTYMD-OLD )
               MOVE    "1110"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4201-W201-SYORI-SEC
           END-IF
      *
           .
       420-W201-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算設定へ
      *****************************************************************
       4201-W201-SYORI-SEC           SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-W20FREE         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW201"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-W20FREE
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
      *
           MOVE    "W201"              TO  SPA-SAKIPG
           MOVE    "W20"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      SPA-W201-TKSNCD2(1) =   SPACE
               MOVE    "KSNCD2"        TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"        TO  MCP-WIDGET
           END-IF
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W201"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       4201-W201-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    病室管理情報更新処理
      *****************************************************************
       4201-BRM-UPDATE-SEC             SECTION.
      *
      *    病室管理情報検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    "%"             TO  SYS-KBNCD(3:1)
           MOVE    SPA-NAI-W20-KJNYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
               MOVE    SYS-5002-EDYUKYMD
                                       TO  WRK-EDYUKYMD
      *
      *        変更項目
               MOVE    SPA-NAI-W20-YUKOEDYMD
                                       TO  SYS-5002-EDYUKYMD
      *
               MOVE    SYS-5002-REC    TO  SYSKANRI-REC
      *
      *        キー項目
               MOVE    SYS-5002-KANRICD
                                       TO  SYSUP-KANRICD
               MOVE    SYS-5002-KBNCD
                                       TO  SYSUP-KBNCD
               MOVE    SYS-5002-STYUKYMD
                                       TO  SYSUP-STYUKYMD
               MOVE    WRK-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1204"      TO  SPA-ERRCD
                   GO  TO  4201-BRM-UPDATE-EXT
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       4201-BRM-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報更新処理
      *****************************************************************
       4201-BTU-UPDATE-SEC             SECTION.
      *
      *    アップデート対象のデータを読み直す
           INITIALIZE                  SYSKANRI-REC
      *
            MOVE   "5001"              TO  SYS-KANRICD
            MOVE   SPA-GMN-W20-BTUNUM  TO  SYS-KBNCD
            MOVE   SPA-NAI-W20-YUKOSTYMD-OLD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF   ( FLG-SYSKANRI    NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
               GO  TO  4201-BTU-UPDATE-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    病棟情報編集処理
           PERFORM 4201-BTU-EDIT-SEC
      *
      *    病棟加算情報編集処理
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           MOVE    SYS-5001-KANRICD
                                   TO  SYSUP-KANRICD
           MOVE    SYS-5001-KBNCD  TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                   TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                   TO  SYSUP-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYSUP-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "upd1"          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC      NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
           END-IF
           .
      *
       4201-BTU-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報追加処理
      *****************************************************************
       4201-BTU-INSERT-SEC             SECTION.
      *
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           MOVE    SYS-5001-REC    TO  TMP-5001-REC
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING
                                       STS-AREA-DAY
                                       LNK-DAY6-AREA
           IF    ( LNK-DAY6-KEISAN     NOT =   TMP-5001-EDYUKYMD )
               MOVE    SPACE       TO  TMP-5001-REC
               INITIALIZE              TMP-5001-REC
           END-IF
      *
           MOVE    SPACE           TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
      *
      *    病棟情報編集処理
           PERFORM 4201-BTU-EDIT-SEC
      *
      *    病棟加算情報編集処理
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
      *    入院料加算（ユーザ設定領域）
           MOVE    TMP-5001-KSN2   TO  SYS-5001-KSN2
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-5001-CREYMD
                                       SYS-5001-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-5001-UPHMS
      *
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    "1202"      TO  SPA-ERRCD
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYS-5001-HOSPNUM
               MOVE    SYS-5001-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1202"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       4201-BTU-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報削除処理
      *****************************************************************
       4201-BTU-DELETE-SEC             SECTION.
      *
           INITIALIZE                     SYS-5001-REC
           MOVE    "5001"             TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM TO  SYS-5001-KBNCD
           MOVE    SPA-NAI-W20-YUKOSTYMD-OLD
                                      TO  SYS-5001-STYUKYMD
           MOVE    SPA-NAI-W20-YUKOEDYMD-OLD
                                      TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM        TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC       TO  MCPDATA-REC
           MOVE    "tbl_syskanri"     TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC         NOT =   ZERO )
               MOVE    "1203"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-BTU-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理
      *****************************************************************
       4201-BTU-EDIT-SEC               SECTION.
      *
      *    MOVE    SPACE       TO  SYS-5001-REC
      *    INITIALIZE              SYS-5001-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
      *
      *    管理コード
           MOVE    "5001"              TO  SYS-5001-KANRICD
      *
      *    区分コード
           MOVE    SPA-GMN-W20-BTUNUM  TO  WRK-BTUNUM 
           MOVE    WRK-BTUNUM          TO  SYS-5001-KBNCD
      *
      *    有効開始年月日
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-5001-STYUKYMD
      *
      *    有効終了年月日
           MOVE    SPA-NAI-W20-YUKOEDYMD
                                       TO  SYS-5001-EDYUKYMD
      *
      *    病棟番号
           MOVE    WRK-BTUNUM          TO  SYS-5001-BTU-NUM
      *
      *    病棟名称
           MOVE    SPA-GMN-W20-NAME    TO  SYS-5001-BTU-NAME
      *
      *    病棟短縮名称
           MOVE    SPA-GMN-W20-TANNAME TO  SYS-5001-BTU-TANNAME
      *
      *    特定入院料
           MOVE    SPA-NAI-W20-TOKNYUIN
                                       TO  SYS-5001-BTU-TOKTEI-NYUIN
      *
      *    病棟科
           MOVE    SPA-GMN-W20-SRYKA   TO  SYS-5001-BTU-KA
      *
      *    病棟種別
           MOVE    SPA-GMN-W20-SBT     TO  SYS-5001-BTU-SBT
      *
      *    レセプト表示区分
           MOVE    SPA-GMN-W20-RECEDSPKBN
                                       TO  SYS-5001-RECEDSPKBN
      *
           PERFORM 42011-KHNRYO-EDIT-SEC
      *
      *    病棟終了区分
           MOVE    SPA-GMN-W20-EDKBN   TO  SYS-5001-BTU-EDKBN
      *
           .
       4201-BTU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理
      *****************************************************************
       42011-KHNRYO-EDIT-SEC           SECTION.
      *
      *    入院基本料点数コード
           IF    ( SPA-GMN-W20-SBT         =   "04" OR "09" )
            AND  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180701 )
      *
               MOVE    SPACE           TO  SYS-5001-BTU-KHNSRYCD
                                           SYS-5001-KHNSRYCD
                                           SYS-5001-KHNKBNCD
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY2-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
               PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                        OR   ( SYS-5001-KHNSRYCD   NOT =   SPACE )
      *
                   IF    ( SPA-NAI-W20-KHNSRYCD    =   SYS-5111-SRYCD )
                       MOVE    SPA-NAI-W20-KHNSRYCD
                                       TO  SYS-5001-KHNSRYCD
                       MOVE    "3"     TO  SYS-5001-KHNKBNCD (1:1)
                       MOVE    SYS-5111-KBNCD
                                       TO  SYS-5001-KHNKBNCD (2:2)
                   END-IF
      *
                   PERFORM 900-SYSKANRI-KEY2-FET-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           ELSE
               MOVE    SPA-NAI-W20-KHNSRYCD
                                       TO  SYS-5001-BTU-KHNSRYCD
               MOVE    SPACE           TO  SYS-5001-KHNSRYCD
                                           SYS-5001-KHNKBNCD
           END-IF
      *
           .
       42011-KHNRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理
      *****************************************************************
       4201-BTU-KSN-EDIT-SEC           SECTION.
      *
           INITIALIZE                  TKSN-AREA
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               PERFORM 42013-BTU-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
               PERFORM 42014-BTU-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H200401 )
               PERFORM 42012-BTU-KSN-EDIT-SEC
           WHEN    OTHER
               PERFORM 42011-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
           .
       4201-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理(〜H20.03)
      *****************************************************************
       42011-BTU-KSN-EDIT-SEC          SECTION.
      *
      *    --  加算情報に初期値を設定  --
      *
      *    看護補助加算区分
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    夜間勤務等看護加算区分
           IF    ( SPA-GMN-W20-YKNKNG  =   "0" )
               MOVE    SPACE           TO  SYS-5001-BTU-YAKANKNGKSN
           ELSE
               MOVE    "00"            TO  SYS-5001-BTU-YAKANKNGKSN
               MOVE    SPA-GMN-W20-YKNKNG
                                       TO  SYS-5001-BTU-YAKANKNGKSN(2:1)
           END-IF
      *
      *    食堂加算区分
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    保育士人数区分
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    難病患者等特別入院診療加算
           MOVE    CONST-NANBYO    TO  SYS-5001-NANBYO-CD
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    "2"             TO  SYS-5001-NANBYO-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-NANBYO-KBN
           END-IF
      *
      *    特殊疾患入院施設管理加算
           MOVE    CONST-TOKUSYUCD TO  SYS-5001-TOKUSYU-CD
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    "2"             TO  SYS-5001-TOKUSYU-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-TOKUSYU-KBN
           END-IF
      *
      *    児童・思春期精神科入院医療管理加算
           MOVE    CONST-JIDOCD    TO  SYS-5001-JIDO-CD
           IF    ( SPA-GMN-W20-JIDO    =   "2" )
               MOVE    "2"             TO  SYS-5001-JIDO-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-JIDO-KBN
           END-IF
      *
      *    精神科応急入院施設管理加算
           MOVE    CONST-SEISINCD  TO  SYS-5001-SEISIN-CD
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    "2"             TO  SYS-5001-SEISIN-KBN
           ELSE
               MOVE    "1"             TO  SYS-5001-SEISIN-KBN
           END-IF
      *
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H180401 )
               MOVE    CONST-KANGOCD (1)
                                   TO  SYS-5001-KANGO-CD  (1)
               IF    ( SPA-GMN-W20-HICH    =   "2" )
                   MOVE    "2"         TO  SYS-5001-KANGO-KBN (1)
               ELSE
                   MOVE    "1"         TO  SYS-5001-KANGO-KBN (1)
               END-IF
           ELSE
      *        看護配置加算
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   5 )
                   MOVE    CONST-KANGOCD (IDX1)
                                       TO  SYS-5001-KANGO-CD (IDX1)
      *
                   IF    ( SPA-GMN-W20-HICH    =   CONST-KANGOCDL(IDX1))
                       MOVE    "2"     TO  SYS-5001-KANGO-KBN (IDX1)
                   ELSE
                       MOVE    "1"     TO  SYS-5001-KANGO-KBN (IDX1)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           MOVE    CONST-KNGTEKCD  TO  SYS-5001-KNGTEK-CD
           IF    ( SPA-GMN-W20-HICHTEK =   "2" )
               MOVE    "2"         TO  SYS-5001-KNGTEK-KBN
           ELSE
               MOVE    "1"         TO  SYS-5001-KNGTEK-KBN
           END-IF
      *
           MOVE    CONST-DRTEKCD   TO  SYS-5001-DRTEK-CD
           IF    ( SPA-GMN-W20-DRTEK   =   "2" )
               MOVE    "2"         TO  SYS-5001-DRTEK-KBN
           ELSE
               MOVE    "1"         TO  SYS-5001-DRTEK-KBN
           END-IF
      *
           .
       42011-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    医療観察法加算編集処理
      *****************************************************************
       42013-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    小規格病棟病床数加算
           IF    ( SPA-GMN-W20-SYOUBTUBRM  NOT =   "00" )
               MOVE    SPA-NAI-W20-SYOUBTUBRMCD
                                       TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    未基準（病棟に係る基準をみたさない場合）
           IF    ( SPA-GMN-W20-BTUMIKJN   =   "2" )
               MOVE    CONST-BTUMIKJNCD
                                   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    食堂加算区分
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
           .
       42013-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理(H20.04〜)
      *****************************************************************
       42012-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    看護補助加算区分
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    食堂加算区分
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    保育士人数区分
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    難病患者等特別入院診療加算
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-NANBYO    TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    特殊疾患入院施設管理加算
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-TOKUSYUCD TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    児童・思春期精神科入院医療管理加算
           IF    ( SPA-GMN-W20-JIDO    =   "2" )
               MOVE    CONST-JIDOCD    TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    精神科応急入院施設管理加算
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-SEISINCD  TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護配置加算
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-KANGOCD (1)
                                   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    医師事務作業補助体制加算
           IF    ( SPA-GMN-W20-DRHJO   IS  NUMERIC )
               MOVE    SPA-GMN-W20-DRHJO   TO  WRK-KBN1-G
               IF    ( WRK-KBN1        >   1 )
                AND  ( WRK-KBN1        <=  SPA-GMN-W20-DRHJO-MAX )
                   COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                       MOVE    CONST-H2204-DRHJO (WRK-KBN1)
                                           TO  WRK-KSNCD
                       PERFORM 420121-BTU-KSN-EDIT-SEC
                   WHEN    OTHER
                       MOVE    CONST-DRHJO (WRK-KBN1)
                                           TO  WRK-KSNCD
                       PERFORM 420121-BTU-KSN-EDIT-SEC
                   END-EVALUATE
               END-IF
           END-IF
      *
      *    重症患者回復病棟加算
           IF    ( SPA-GMN-W20-JKAIFUKU    =   "2" )
               MOVE    CONST-JKAIFUKU     TO   WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    医師配置加算
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    CONST-H2204-DRTEKCD1    TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    医師配置加算２
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    夜間緊急体制確保加算
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-YKNKNKYU      TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護配置加算１（診療所）
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    CONST-H2204-SNRHICH1
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    CONST-SNRHICH1
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    看護配置加算２（診療所）
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-SNRHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-H2204-SNRHICH2
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-SNRHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-SNRHICH2
                                          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    夜間看護配置加算１
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-YKNHICH1
                                                TO  WRK-KBN1-G
                   MOVE    CONST-H2204-YKNHICH1
                                                TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-YKNHICH1 TO  WRK-KBN1-G
                   MOVE    CONST-YKNHICH1       TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    夜間看護配置加算２
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
                   MOVE    SPA-GMN-W20-YKNHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-H2204-YKNHICH2 TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               ELSE
                   MOVE    SPA-GMN-W20-YKNHICH2 TO  WRK-KBN1-G
                   MOVE    CONST-YKNHICH2       TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               EVALUATE    SPA-GMN-W20-KYUKNGHJ
      *        急性期看護補助体制加算１
               WHEN    "2"
                   MOVE    CONST-KYUKNGHJ1         TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
      *        急性期看護補助体制加算２
               WHEN    "3"
                   MOVE    CONST-KYUKNGHJ2         TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-EVALUATE
           END-IF
      *
      *    精神科地域移行実施加算
           IF    ( SPA-GMN-W20-SEICHIIKI     =  "2" )
               MOVE    SPA-GMN-W20-SEICHIIKI TO  WRK-KBN1-G
               MOVE    CONST-SEICHIIKI       TO  WRK-KSNCD
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    一般病棟看護必要度評価加算
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
                   MOVE    SPA-GMN-W20-HYOUKA    TO  WRK-KBN1-G
                   MOVE    CONST-HYOUKA          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
      *    休日リハビリテーション提供体制加算
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
               IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
                   MOVE    SPA-GMN-W20-KJTRIHA    TO  WRK-KBN1-G
                   MOVE    CONST-KJTRIHA          TO  WRK-KSNCD
                   PERFORM 420121-BTU-KSN-EDIT-SEC
               END-IF
           END-IF
      *
           .
       42012-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理(H24.04〜)
      *****************************************************************
       42014-BTU-KSN-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5001-KSNAREA
      *
      *    看護補助加算区分
           IF    ( SPA-GMN-W20-KNGHJO  =   "00" )
               MOVE    SPACE           TO  SYS-5001-BTU-KNGHJOKSN
           ELSE
               MOVE    SPA-GMN-W20-KNGHJO
                                       TO  SYS-5001-BTU-KNGHJOKSN
           END-IF
      *
      *    食堂加算区分
           IF    ( SPA-GMN-W20-SYOKUDO =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-SYOKUDOKBN
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-SYOKUDOKBN
           END-IF
      *
      *    保育士人数区分
           IF    ( SPA-GMN-W20-HOIKU   =   "2" )
               MOVE    "2"             TO  SYS-5001-BTU-HIKSISUKBN
               MOVE    CONST-HOIKUCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HOIKU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           ELSE
               MOVE    "1"             TO  SYS-5001-BTU-HIKSISUKBN
           END-IF
      *
      *    難病患者等特別入院診療加算
           IF    ( SPA-GMN-W20-NANBYO  =   "2" )
               MOVE    CONST-NANBYO    TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-NANBYO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    特殊疾患入院施設管理加算
           IF    ( SPA-GMN-W20-TOKUSYU =   "2" )
               MOVE    CONST-TOKUSYUCD TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-TOKUSYU-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    精神科応急入院施設管理加算
           IF    ( SPA-GMN-W20-SEISIN  =   "2" )
               MOVE    CONST-SEISINCD  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEISIN-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護配置加算
           IF    ( SPA-GMN-W20-HICH    =   "2" )
               MOVE    CONST-KANGOCD (1)
                                   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HICH-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    医師事務作業補助体制加算
           IF    ( SPA-GMN-W20-DRHJO   >=  "2"  )
             AND ( SPA-GMN-W20-DRHJO   <=  "9"  )
               MOVE    SPA-GMN-W20-DRHJO
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H2404-DRHJO (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRHJO-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    急性期看護補助体制加算
           IF    ( SPA-GMN-W20-KYUKNGHJ    >=   "2" )
            AND  ( SPA-GMN-W20-KYUKNGHJ    <=   "5" )
               MOVE    SPA-GMN-W20-KYUKNGHJ
                                   TO  WRK-KBN1-G
               COMPUTE WRK-KBN1    =   WRK-KBN1    -   1
               MOVE    CONST-H2404-KYUKNGHJ (WRK-KBN1)
                                       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KYUKNGHJ-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    夜間急性期看護補助体制加算
           EVALUATE    SPA-GMN-W20-YKNKYUKNGHJ
           WHEN    "2"
               MOVE    CONST-H2404-YKNKYUKNGHJ1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-YKNKYUKNGHJ2
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKYUKNGHJ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    看護職員夜間配置加算
           IF    ( SPA-GMN-W20-KNGYKNHICHI   =  "2" )
               MOVE    CONST-H2404-KNGYKNHICHI
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGYKNHICHI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    精神科地域移行実施加算
           IF    ( SPA-GMN-W20-SEICHIIKI   =  "2" )
               MOVE    CONST-SEICHIIKI     TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SEICHIIKI-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    一般病棟看護必要度評価加算
           IF    ( SPA-GMN-W20-HYOUKA  =   "2" )
               MOVE    CONST-HYOUKA        TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-HYOUKA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    休日リハビリテーション提供体制加算
           IF    ( SPA-GMN-W20-KJTRIHA    =   "2" )
               MOVE    CONST-KJTRIHA       TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KJTRIHA-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護必要度評価加算
           EVALUATE    SPA-GMN-W20-KNGHITU
           WHEN    "2"
               MOVE    CONST-H2404-KNGHITU1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-KNGHITU2
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-KNGHITU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    病棟薬剤業務実施加算
           IF    ( SPA-GMN-W20-BTUYKZ      =   "2" )
               MOVE    CONST-H2404-BTUYKZ  TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-BTUYKZ-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    医師配置加算
           IF    ( SPA-GMN-W20-DRTEK       =   "2" )
               MOVE    CONST-H2204-DRTEKCD1
                                           TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    医師配置加算２
           IF    ( SPA-GMN-W20-DRTEK2      =   "2" )
               MOVE    CONST-DRTEKCD   TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-DRTEK2-IDX
                                       TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    夜間緊急体制確保加算
           IF    ( SPA-GMN-W20-YKNKNKYU    =   "2" )
               MOVE    CONST-YKNKNKYU      TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNKNKYU-IDX
                                           TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護配置加算１（診療所）
           IF    ( SPA-GMN-W20-SNRHICH1     =  "2" )
               MOVE    CONST-H2204-SNRHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    看護配置加算２（診療所）
           IF    ( SPA-GMN-W20-SNRHICH2     =  "2" )
               MOVE    CONST-H2204-SNRHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-SNRHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    夜間看護配置加算１
           IF    ( SPA-GMN-W20-YKNHICH1     =  "2" )
               MOVE    CONST-H2204-YKNHICH1 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH1-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
      *    夜間看護配置加算２
           IF    ( SPA-GMN-W20-YKNHICH2     =  "2" )
               MOVE    CONST-H2204-YKNHICH2 TO  WRK-KSNCD
               MOVE    SPA-NAI-W20-YKNHICH2-IDX
                                            TO  WRK-KSNIDX
               PERFORM 420121-BTU-KSN-EDIT-SEC
           END-IF
      *
           .
       42014-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理(H20.04〜)
      *****************************************************************
       420121-BTU-KSN-EDIT-SEC          SECTION.
      *
           IF    ( WRK-KSNCD            =  CONST-HOIKUCD )
               CONTINUE
           ELSE
               IF    ( CONST-5001-KSN-MAX   >  CNT-KSN )
                   COMPUTE CNT-KSN =   CNT-KSN +   1
      *
                   MOVE    WRK-KSNCD   TO SYS-5001-KSNCD (CNT-KSN)
                   MOVE    "2"         TO SYS-5001-KSNKBN (CNT-KSN)
               END-IF
           END-IF
      *
           IF    ( CONST-KSN-MAX       >   TKSN-MAX )
               COMPUTE TKSN-MAX    =   TKSN-MAX    +   1
               MOVE    WRK-KSNCD   TO  TKSN-SRYCD (TKSN-MAX)
               MOVE    WRK-KSNIDX  TO  TKSN-IDX   (TKSN-MAX)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-KSN-MAX )
                        OR   ( TKSN-LBL (TKSN-MAX) NOT =   SPACE )
      *
                   IF    ( SPA-NAI-W20-KSNINF-IDX (IDX0)
                                       =   WRK-KSNIDX )
                       MOVE    SPA-GMN-W20-KSN-LBL (IDX0)
                                   TO  TKSN-LBL (TKSN-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       420121-BTU-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号
      *****************************************************************
       4100-BTUNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-BTUNUM      TO  SPA-GMN-W20-BTUNUM
      *
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 420-CLEAR-SEC
               GO  TO  4100-BTUNUM-CHK-EXT
           END-IF
      *
      *    入力項目領域編集処理
           MOVE    SPA-GMN-W20-BTUNUM  TO  WRK-BTUNUM
           MOVE    SPA-NAI-W20-KJNYMD          TO  WRK-KIJUN-YMD
      *
      *    画面初期化
           PERFORM 420-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           IF    ( FLG-SYS5001      =   ZERO )
      *
               PERFORM 4100-BTUNUM-LSTSEL-SEC
               MOVE    IDX-BTULST   TO  SPA-GMN-W20-BTULST-SEL
      *
      *        履歴データ編集処理
               PERFORM 4100-RIREKI-AREA-EDIT-SEC
           ELSE
      *
               MOVE    W20-BTUNUM      TO  SPA-GMN-W20-BTUNUM
      *
      *        システム管理情報再設定
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
               MOVE    SPA-GMN-W20-EDKBNLST (1)
                                       TO  SPA-GMN-W20-EDKBN-G
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           .
      *
       4100-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟種別
      *****************************************************************
       4100-SBT-CHK-SEC                SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-SBT         TO  SPA-GMN-W20-SBT-G
      *
           IF    ( SPA-GMN-W20-SBT-G   =   SPACE )
      *
               INITIALIZE  SPA-NAI-W20-KHNSRYCDLST-G
                           SPA-GMN-W20-KHNRYOLST-G
                           SPA-NAI-W20-KHNTENLST-G
      *
               INITIALIZE  SPA-GMN-W20-KHNRYO-G
                           SPA-NAI-W20-KHNSRYCD
                           SPA-NAI-W20-KHNTEN
                           SPA-GMN-W20-RECEDSPKBN-G
                           SPA-NAI-W20-SBT-MOT
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           PERFORM 4101-SBT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   SPA-NAI-W20-SBT-MOT )
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   CONST-KANSATU )
            OR   ( SPA-NAI-W20-SBT-MOT =   CONST-KANSATU )
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT     TO  SPA-NAI-W20-SBT-MOT
      *
           INITIALIZE      SPA-NAI-W20-KHNSRYCDLST-G
                           SPA-GMN-W20-KHNRYOLST-G
                           SPA-NAI-W20-KHNTENLST-G
      *
           INITIALIZE      SPA-GMN-W20-KHNRYO-G
                           SPA-NAI-W20-KHNSRYCD
                           SPA-NAI-W20-KHNTEN
                           SPA-GMN-W20-RECEDSPKBN-G
      *
      *    入院基本料コンボ編集処理
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                   MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                       TO  SPA-GMN-W20-YKNKNG-G
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT     TO  WRK-CMB-CD
           PERFORM 4201-RECEDSPKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-RECEDSPKBN-G
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           .
      *
       4100-SBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟番号
      *****************************************************************
       4101-BTUNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W20-BTUNUM      =   ZERO )
               MOVE    "1101"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-W20-CUR
           END-IF
           .
      *
       4101-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称
      *****************************************************************
       4101-NAME-CHK-SEC               SECTION.
      *
      *    全角半角チェック
           MOVE    SPA-GMN-W20-NAME
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-W20-NAME
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    2           TO  SPA-GMN-W20-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    短縮病棟名
      *****************************************************************
       4101-TANNAME-CHK-SEC        SECTION.
      *
      *    全角半角チェック
           MOVE    SPA-GMN-W20-TANNAME
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  SPA-GMN-W20-TANNAME
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    3           TO  SPA-GMN-W20-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4101-TANNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料
      *****************************************************************
       4101-TOKUTEINYUIN-CHK-SEC   SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-W20-TOKNYUIN
      *
           IF    ( SPA-GMN-W20-TOKNYUIN-G  =   SPACE )
               GO  TO  4101-TOKUTEINYUIN-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W20-TOKNYUIN    TO  WRK-CMB-CD
           PERFORM 4200-TOKNYUINLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    4           TO  SPA-GMN-W20-CUR
               MOVE    "0004"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKNYUIN-G
               MOVE    WRK-CMB-ITM4
                                   TO  SPA-NAI-W20-TOKNYUIN
           END-IF
      *
           .
      *
       4101-TOKUTEINYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟科名
      *****************************************************************
       4101-SRYKA-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W20-SRYKA-G =   SPACE )
               GO  TO  4101-SRYKA-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W20-SRYKA   TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    5           TO  SPA-GMN-W20-CUR
               MOVE    "0005"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SRYKA-G
           END-IF
      *
           .
      *
       4101-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟種別
      *****************************************************************
       4101-SBT-CHK-SEC                SECTION.
      *
           IF    ( SPA-GMN-W20-SBT-G   =   SPACE )
               GO  TO  4101-SBT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-SBT TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    6           TO  SPA-GMN-W20-CUR
               MOVE    "0006"      TO  SPA-ERRCD
               GO  TO  4101-SBT-CHK-EXT
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SBT-G
           END-IF
      *
           .
      *
       4101-SBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料コンボ編集処理
      *****************************************************************
       4100-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE  SPA-NAI-W20-KHNSRYCDLST-G
                       SPA-GMN-W20-KHNRYOLST-G
                       SPA-NAI-W20-KHNTENLST-G
      *
           MOVE    ZERO            TO  IDX-CMB
      *
           IF    ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               CONTINUE
           ELSE
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
                AND  ( SPA-GMN-W20-SBT         =   "08" )
                   PERFORM 41001-KHNRYO-CMB-EDT-SEC
               ELSE
                   PERFORM 41002-KHNRYO-CMB-EDT-SEC
               END-IF
           END-IF
      *
      *
           .
      *
       4100-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料コンボ編集処理
      *****************************************************************
       41001-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           MOVE    "190097010"         TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-TEN             TO  WRK-TEN
      *
           MOVE    "099999950"         TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    WRK-TEN         TO  TNS-TEN
               PERFORM 410011-KHNRYO-CMB-SET-SEC
           END-IF
      *
           MOVE    "190097210"         TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-TEN             TO  WRK-TEN
      *
           MOVE    "099999951"         TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               MOVE    WRK-TEN         TO  TNS-TEN
               PERFORM 410011-KHNRYO-CMB-SET-SEC
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
      *
               MOVE    "190135110"     TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN         TO  WRK-TEN
      *
               MOVE    "099999952"     TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
      *
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   MOVE    WRK-TEN     TO  TNS-TEN
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
           END-IF
      *
           .
      *
       41001-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料コンボ編集処理
      *****************************************************************
       410011-KHNRYO-CMB-SET-SEC       SECTION.
      *
           COMPUTE IDX-CMB     =   IDX-CMB    +   1
           MOVE    IDX-CMB     TO  SPA-GMN-W20-KHNRYO-MAX
      *
           MOVE    TNS-SRYCD   TO  SPA-NAI-W20-KHNSRYCDLST
                                               (IDX-CMB)
      *
           MOVE    IDX-CMB     TO  WRK-99
           MOVE    WRK-99      TO  SPA-GMN-W20-KHNRYOL
                                                   (IDX-CMB)
           MOVE    TNS-NAME    TO  SPA-GMN-W20-KHNRYOMEIL
                                                   (IDX-CMB)
           MOVE    TNS-TEN     TO  SPA-NAI-W20-KHNTENLST
                                                   (IDX-CMB)
      *
           .
       410011-KHNRYO-CMB-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料コンボ編集処理
      *****************************************************************
       41002-KHNRYO-CMB-EDT-SEC     SECTION.
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H220401 )
             AND ( SPA-GMN-W20-SBT         =   "04" )
               MOVE    "099999953"             TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
               MOVE    "099999954"             TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
           END-IF
      *
      *    点数マスタ検索処理
           PERFORM 900-TENSU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-TENSU   NOT =   ZERO )
                OR       ( IDX-CMB        >=   CONST-KHNRYO-MAX  )
      *
               MOVE    1           TO  FLG-SETKHNRYO
      *
      *        選定入院料は読み飛ばす
               MOVE    ZERO        TO  CNT-STR
               INSPECT TNS-NAME    TALLYING   CNT-STR
                       FOR     ALL "（選）", ALL "（選定療養）"
      *
               IF    ( CNT-STR     >   ZERO )
                   MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
               IF    ( TNS-ROUTEKKBN       =   "2" )
                   MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
               IF    ( SPA-GMN-W20-SBT =   "04"  OR "09" )
                AND  ( SPA-NAI-W20-YUKOSTYMD
                                      >=   CONST-H180701 )
                   IF    ( TNS-SRYCD   =   "190081310"
                                        OR "190081410"
                                        OR "190080610"
                                        OR "190080710"
                                        OR "190098210"
                                        OR "190098310"
      *以下生活療養コード
                                        OR "190123710"
                                        OR "190123810"
                                        OR "190123910"
                                        OR "190124010"
                                        OR "190124110"
                                        OR "190124210"
                                        OR "190124310"
                                        OR "190124410"
                                        OR "190124510"
                                        OR "190124610"
                                        OR "190124710"
                                        OR "190124810"
                                        OR "190131710"
                                        OR "190131910"
                                        OR "190132110"
                                        OR "190132310"
                                        OR "190132510"
                                        OR "190132710"
                                        OR "190132910"
                                        OR "190133110"
                                        OR "190133310"
                                        OR "190133510"
                                        OR "190133710"
                                        OR "190133910"
                                        OR "190134110"
      *
                                                        )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
                   END-IF
               END-IF
      *
               IF    ( TNS-SRYCD   =   "190799410"
                                    OR "190799510"  )
                       MOVE    ZERO    TO  FLG-SETKHNRYO
               END-IF
      *
               IF    ( FLG-SETKHNRYO   =   1 )
                   PERFORM 410011-KHNRYO-CMB-SET-SEC
               END-IF
      *
      *        点数マスタ読込処理
               PERFORM 900-TENSU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       41002-KHNRYO-CMB-EDT-EXT.
           EXIT.
      *****************************************************************
      *    レセプト表示区分
      *****************************************************************
       4101-RECEDSPKBN-CHK-SEC     SECTION.
      *
           IF    ( SPA-GMN-W20-RECEDSPKBN-G
                                   =   SPACE )
               GO  TO  4101-RECEDSPKBN-CHK-EXT
      *
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W20-RECEDSPKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-RECEDSPKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    8           TO  SPA-GMN-W20-CUR
               MOVE    "0009"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-RECEDSPKBN-G
           END-IF
      *
           .
      *
       4101-RECEDSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日
      *****************************************************************
       4101-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-YUKOSTYMD
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD-MOT   =   SPACE )
               MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD-MOT
           END-IF
      *
           MOVE    SPACE           TO  SPA-NAI-W20-YUKOSTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-W20-YUKOSTYMD   =   SPACE )
               MOVE    SPA-GMN-W20-KJNYMD (1 : 6)
                                       TO  SPA-GMN-W20-YUKOSTYMD (1 : 6)
               MOVE    ". 1"           TO  SPA-GMN-W20-YUKOSTYMD (7 : 3)
               MOVE    SPA-NAI-W20-KJNYMD   (1 : 6)
                                       TO  SPA-NAI-W20-YUKOSTYMD (1 : 6)
               MOVE    "01"            TO  SPA-NAI-W20-YUKOSTYMD (7 : 2)
      *
      *
      *        システム管理情報再設定
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
               MOVE    SPACE           TO  SPA-NAI-W20-YUKOSTYMD-MOT
      *
           ELSE
               MOVE    SPA-GMN-W20-YUKOSTYMD   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               IF  ( FLG-HIZUKE-ERR    =   ZERO )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOSTYMD
                   MOVE    "0010"      TO  SPA-ERRCD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-IF
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD   <   "20020401" )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    "H14. 4. 1" TO  SPA-GMN-W20-YUKOSTYMD
                   MOVE    "20020401"  TO  SPA-NAI-W20-YUKOSTYMD
                   MOVE    51          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD
                               NOT =   SPA-NAI-W20-YUKOSTYMD-MOT )
      *        システム管理情報再設定
               PERFORM 4100-SYS-KANRI-SAI-SET-SEC
           END-IF
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD-MOT   TO  WRK-STYMD
           MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-EDYMD
           IF    ( SPA-NAI-W20-YUKOSTYMD-MOT
                               >    SPA-NAI-W20-YUKOSTYMD )
               MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-STYMD
               MOVE    SPA-NAI-W20-YUKOSTYMD-MOT   TO  WRK-EDYMD
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( WRK-EDYMD                  >=   CONST-H180401 )
             AND ( WRK-STYMD                  <    CONST-H180401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H200401 )
             AND ( WRK-STYMD                  <    CONST-H200401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H200701 )
             AND ( WRK-STYMD                   <   CONST-H200701 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H220401 )
             AND ( WRK-STYMD                   <   CONST-H220401 )
           WHEN  ( WRK-EDYMD                  >=   CONST-H240401 )
             AND ( WRK-STYMD                   <   CONST-H240401 )
                   PERFORM 4101-NYUINRYO-CLEAR-SEC
           END-EVALUATE
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                   MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                           TO  SPA-GMN-W20-YKNKNG-G
               END-IF
      *
           END-IF
      *
           IF    ( WRK-EDYMD      >=   CONST-H180701 )
               IF    ( WRK-STYMD   <   CONST-H180701 )
                   IF    ( SPA-GMN-W20-SBT =   "04" OR "09" )
                       PERFORM 4101-NYUINRYO-CLEAR-SEC
                   END-IF
      *
               END-IF
      *
               MOVE  SPA-GMN-W20-YKNKNGLST (01)
                                           TO  SPA-GMN-W20-YKNKNG-G
           END-IF
      *
           PERFORM 4100-BTUNUM-LSTSEL-SEC
           IF    ( IDX-BTULST      >   ZERO )
               EVALUATE    TRUE
               WHEN  ( WRK-EDYMD          >=   CONST-H220401 )
                 AND ( WRK-STYMD          <    CONST-H220401 )
               WHEN  ( WRK-EDYMD          >=   CONST-H240401 )
                 AND ( WRK-STYMD          <    CONST-H240401 )
                   INITIALIZE                     SYSKANRI-REC
                   PERFORM 900-SYSKANRI-KEY16-SEL-SEC
                   PERFORM 41001-INPUT-AREA-EDIT-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD-MOT
      *
           .
      *
       4101-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料初期化処理
      *****************************************************************
       4101-NYUINRYO-CLEAR-SEC          SECTION.
      *
           INITIALIZE              SPA-NAI-W20-KHNSRYCD
                                   SPA-NAI-W20-KHNTEN
                                   SPA-GMN-W20-KHNRYO-G
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           .
       4101-NYUINRYO-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日
      *****************************************************************
       4101-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-W20-YUKOEDYMD
                                   TO  WRK-YUKOEDYMD
           MOVE    SPACE           TO  SPA-NAI-W20-YUKOEDYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-W20-YUKOEDYMD   =   SPACE )
               MOVE    "99999999"  TO  SPA-GMN-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD
               GO  TO  4101-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-YUKOEDYMD
                                   TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF  ( FLG-HIZUKE-ERR    =   ZERO )
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W20-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOEDYMD
           ELSE
               MOVE    WRK-YUKOEDYMD
                                   TO  WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W20-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-YUKOEDYMD
               MOVE    "0011"      TO  SPA-ERRCD
               MOVE    52          TO  SPA-GMN-W20-CUR
               GO  TO  4101-YUKOEDYMD-CHK-EXT
           END-IF
      *
           .
      *
       4101-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟終了区分
      *****************************************************************
       4101-EDKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-W20-EDKBN   TO  WRK-CMB-CD
           PERFORM 4200-EDKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-EDKBNLST (1)
                                       TO  SPA-GMN-W20-EDKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-EDKBN-G
           END-IF
      *
           .
      *
       4101-EDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料
      *****************************************************************
       4100-KHNRYO-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           INITIALIZE              SPA-NAI-W20-KHNSRYCD
                                   SPA-NAI-W20-KHNTEN
      *
           MOVE    W20-KHNRYO      TO  SPA-GMN-W20-KHNRYO-G
      *
      *    入院基本料チェック
           PERFORM 4101-KHNRYO-CHK-SEC
      *
           .
      *
       4100-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料
      *****************************************************************
       4101-KHNRYO-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W20-KHNRYO  =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-W20-KHNRYO  TO  WRK-CMB-CD
               PERFORM 4200-KHNRYOLST-CHK-SEC
               IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    "0012"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-W20-CUR
               ELSE
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KHNRYO-G
                   MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W20-KHNSRYCD
                   MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W20-KHNTEN
               END-IF
           END-IF
      *
           .
      *
       4101-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護補助加算チェック
      *****************************************************************
       4101-KNGHJO-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-W20-KNGHJO  TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                       TO  SPA-GMN-W20-KNGHJO-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KNGHJO-G
           END-IF
      *
           .
      *
       4101-KNGHJO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算
      *****************************************************************
       4101-SYOKUDO-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-W20-SYOKUDO TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           IF    ( WRK-CMB-CD          =   SPACE )
               MOVE    SPA-GMN-W20-SYOKUDOLST (1)
                                       TO  SPA-GMN-W20-SYOKUDO-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-SYOKUDO-G
           END-IF
      *
           .
      *
       4101-SYOKUDO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    未基準（病棟に係る基準をみたさない場合）
      *****************************************************************
       4101-BTUMIKJN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-W20-BTUMIKJN    TO  WRK-CMB-CD
           PERFORM 4200-BTUMIKJNLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-W20-BTUMIKJNLST (1)
                                   TO  SPA-GMN-W20-BTUMIKJN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-BTUMIKJN-G
           END-IF
      *
           .
      *
       4101-BTUMIKJN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小規格病棟病床数加算
      *****************************************************************
       4101-SYOUBTUBRM-CHK-SEC         SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-W20-SYOUBTUBRMCD
           MOVE    SPA-GMN-W20-SYOUBTUBRM    TO  WRK-CMB-CD
           PERFORM 4200-SYOUBTUBRMLST-CHK-SEC
           IF    ( WRK-CMB-CD      =   SPACE )
               MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                   TO  SPA-GMN-W20-SYOUBTUBRM-G
           ELSE
               MOVE    WRK-CMB-ITM  TO SPA-GMN-W20-SYOUBTUBRM-G
               MOVE    WRK-CMB-ITM2 TO SPA-NAI-W20-SYOUBTUBRMCD
           END-IF
      *
           .
      *
       4101-SYOUBTUBRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-W20-CUR
           MOVE    W20-KJNYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF  ( SPA-ERRCD         =   SPACE )
      *
               INITIALIZE              SPA-W20-AREA
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-W20-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W20-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    101         TO  SPA-GMN-W20-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           MOVE    W20-SELNUM      TO  SPA-GMN-W20-SELNUM
      *
           IF  (   SPA-GMN-W20-SELNUM  =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
      *    範囲内の番号が選択されていること
           IF    ( SPA-GMN-W20-SELNUM  >   SPA-GMN-W20-RIREKILST-MAX )
               MOVE    100         TO  SPA-GMN-W20-CUR
               MOVE    "0013"      TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W20-SELNUM  TO  SPA-GMN-W20-RIREKILST-SEL
      *
      *    入力項目領域編集処理
           MOVE    SPA-NAI-W20-RIREKILST-BTUNUM 
                                   TO  WRK-BTUNUM 
           MOVE    SPA-NAI-W20-TRYUKOSTYMD (SPA-GMN-W20-SELNUM)
                                   TO  WRK-KIJUN-YMD
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           MOVE    W20-SELNUM      TO  SPA-GMN-W20-SELNUM
      *
           MOVE    ZERO            TO  SPA-GMN-W20-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟リスト
      *****************************************************************
       4100-BTULST-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-W20-BTULST-SEL
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   CONST-BTULST-MAX )
                     OR      ( SPA-GMN-W20-BTULST-SEL
                                       >   ZERO )
      *
               IF      (   W20-BTULST-SEL  (IDX2)  =   "T" )
      *
      *            入力項目領域編集処理
                   MOVE    SPA-GMN-W20-TBTUNUM (IDX2)
                                           TO  WRK-BTUNUM 
                   MOVE    SPA-NAI-W20-KJNYMD      TO  WRK-KIJUN-YMD
      *
      *            画面初期化
                   PERFORM 420-CLEAR-SEC
      *
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
      *            履歴データ編集処理
                   PERFORM 4100-RIREKI-AREA-EDIT-SEC
      *
                   MOVE    IDX2        TO  SPA-GMN-W20-BTULST-SEL
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4100-BTULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴リスト
      *****************************************************************
       4100-RIREKILST-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W20-RIREKILST-SEL
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   CONST-RIREKILST-MAX )
                    OR       ( SPA-GMN-W20-RIREKILST-SEL
                                       >   ZERO )
      *
               IF    ( W20-RIREKILST-SEL (IDX2)
                                       =   "T" )
      *
      *            入力項目領域編集処理
                   MOVE    SPA-NAI-W20-RIREKILST-BTUNUM 
                                       TO  WRK-BTUNUM 
                   MOVE    SPA-NAI-W20-TRYUKOSTYMD (IDX2)
                                       TO  WRK-KIJUN-YMD
      *
      *            入力領域初期化
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
                   MOVE    IDX2        TO  SPA-GMN-W20-RIREKILST-SEL
                                           SPA-GMN-W20-SELNUM
               END-IF
      *
           END-PERFORM
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SYS5001
      *
      *    病棟情報検索処理
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    WRK-BTUNUM      TO  SYS-5001-KBNCD
           MOVE    WRK-KIJUN-YMD   TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
           MOVE    SYS-5001-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   1 )
               MOVE    1           TO  FLG-SYS5001
               GO  TO  4100-INPUT-AREA-EDIT-EXT
           END-IF
      *
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    有効開始日
           MOVE    SYS-5001-STYUKYMD
                                   TO  SPA-NAI-W20-YUKOSTYMD
                                       SPA-NAI-W20-YUKOSTYMD-OLD
                                       SPA-NAI-W20-YUKOSTYMD-MOT
                                       WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W20-YUKOSTYMD
      *
      *    有効終了日
           MOVE    SYS-5001-EDYUKYMD
                                   TO  SPA-NAI-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD-OLD
                                       WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-W20-YUKOEDYMD
      *
           PERFORM 41001-INPUT-AREA-EDIT-SEC
      *
      *    入院料加算設定画面の引き継ぎ領域編集
           PERFORM    VARYING   IDX-KSN2   FROM   1   BY  1
             UNTIL   (IDX-KSN2   >   10)    OR
                     (SYS-5001-KSNCD2(IDX-KSN2)  =  SPACE)
                 MOVE   SYS-5001-KSNCD2(IDX-KSN2)
                                   TO  SPA-W201-TKSNCD2 (IDX-KSN2)
           END-PERFORM
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       41001-INPUT-AREA-EDIT-SEC       SECTION.
      *
      *    病棟種別
           MOVE    SYS-5001-BTU-SBT    TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SBT-G
           MOVE    SYS-5001-BTU-SBT    TO  SPA-NAI-W20-SBT-MOT
      *
      *    システム管理情報再設定
           PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
      *    病棟番号
           MOVE    SYS-5001-BTU-NUM    TO  SPA-GMN-W20-BTUNUM
      *
      *    病棟名称
           MOVE    SYS-5001-BTU-NAME   TO  SPA-GMN-W20-NAME
      *
      *    短縮病棟名称
           MOVE    SYS-5001-BTU-TANNAME
                                       TO  SPA-GMN-W20-TANNAME
      *
      *    特定入院料
           MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                       TO  WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKNYUIN-G
           MOVE    WRK-CMB-ITM4        TO  SPA-NAI-W20-TOKNYUIN
      *
      *    病棟科名
           MOVE    SYS-5001-BTU-KA     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SRYKA-G
      *
      *    レセプト表示区分
           MOVE    SYS-5001-RECEDSPKBN TO  WRK-CMB-CD
           PERFORM 4200-RECEDSPKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-RECEDSPKBN-G
      *
      *    病棟終了区分
           MOVE    SYS-5001-BTU-EDKBN  TO  WRK-CMB-CD
           PERFORM 4200-EDKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-EDKBN-G
      *
      *    入院基本料コンボ編集処理
           PERFORM 4100-KHNRYO-CMB-EDT-SEC
      *
           MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-W20-KHNSRYCD
      *
      *    入院基本点数をサブプロより取得する
           INITIALIZE W20SUB01-LNK
           MOVE    "1"             TO  W20SUB01-I-KBN
           IF    ( SYS-5001-KHNSRYCD   NOT =   SPACE )
               MOVE    SYS-5001-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           ELSE
               MOVE    SYS-5001-BTU-KHNSRYCD
                                       TO  W20SUB01-I-KHNSRYCD
           END-IF
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  W20SUB01-I-KJNYMD
      *
           CALL    "ORCGW20SUB01"      USING
                                       W20SUB01-LNK
                                       SPA-AREA
           IF    ( W20SUB01-O-RC   =   ZERO )
               MOVE    W20SUB01-O-KHNSRYCD
                                   TO  WRK-CMB-CD
               PERFORM 4201-KHNRYOLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W20-KHNRYO-G
               MOVE    WRK-CMB-ITM2    TO  SPA-NAI-W20-KHNSRYCD
               MOVE    WRK-CMB-ITM3    TO  SPA-NAI-W20-KHNTEN
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT          =   CONST-KANSATU )
               PERFORM 41005-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H240401 )
               PERFORM 41006-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H220401 )
               PERFORM 41004-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H200401 )
               PERFORM 41003-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=   CONST-H180401 )
               PERFORM 41002-KSN-EDIT-SEC
           WHEN    OTHER
               PERFORM 41001-KSN-EDIT-SEC
           END-EVALUATE
      *
           .
       41001-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H14.4〜H18.3)
      *****************************************************************
       41001-KSN-EDIT-SEC               SECTION.
      *
      *    看護補助加算
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    夜間勤務等看護加算
           MOVE    SYS-5001-BTU-YAKANKNGKSN(2:1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-YAKANKNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-YKNKNG-G
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    プレイルーム、保育士等加算
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
      *    難病患者等入院診療加算
           MOVE    SYS-5001-NANBYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NANBYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-NANBYO-G
      *
      *    特殊疾患入院施設管理加算
           MOVE    SYS-5001-TOKUSYU-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOKUSYULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKUSYU-G
      *
      *    児童・思春期精神科入院医療管理加算
           MOVE    SYS-5001-JIDO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-JIDO-G
      *
      *    精神科応急入院施設管理加算
           MOVE    SYS-5001-SEISIN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SEISINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SEISIN-G
      *
      *    看護配置加算
           MOVE    "0"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5001-KANGO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (3)  =   "2" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (4)  =   "2" )
               MOVE    "5"             TO  WRK-CMB-CD
           WHEN  ( SYS-5001-KANGO-KBN (5)  =   "2" )
               MOVE    "6"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-HICHLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICH-G
      *
           .
       41001-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H18.4〜H20.3)
      *****************************************************************
       41002-KSN-EDIT-SEC               SECTION.
      *
      *    看護補助加算
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    夜間勤務等看護加算
           MOVE    SYS-5001-BTU-YAKANKNGKSN(2:1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-YAKANKNGLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-YKNKNG-G
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    プレイルーム、保育士等加算
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
      *    難病患者等入院診療加算
           MOVE    SYS-5001-NANBYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-NANBYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-NANBYO-G
      *
      *    特殊疾患入院施設管理加算
           MOVE    SYS-5001-TOKUSYU-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-TOKUSYULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-TOKUSYU-G
      *
      *    児童・思春期精神科入院医療管理加算
           MOVE    SYS-5001-JIDO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-JIDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-JIDO-G
      *
      *    精神科応急入院施設管理加算
           MOVE    SYS-5001-SEISIN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SEISINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SEISIN-G
      *
      *    看護配置加算
           MOVE    SYS-5001-KANGO-KBN (1)
                                       TO  WRK-CMB-CD
           PERFORM 4200-HICHLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICH-G
      *
      *    看護配置適合加算
           MOVE    SYS-5001-KNGTEK-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HICHTEKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HICHTEK-G
      *
      *    複数医師及び看護配置適合加算
           MOVE    SYS-5001-DRTEK-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-DRTEKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-DRTEK-G
      *
           .
       41002-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H24.4〜)
      *****************************************************************
       41006-KSN-EDIT-SEC               SECTION.
      *
      *    看護補助加算
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  SPA-GMN-W20-SYOKUDO
      *
      *    プレイルーム、保育士等加算
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  SPA-GMN-W20-HOIKU
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  SPA-GMN-W20-NANBYO
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  SPA-GMN-W20-TOKUSYU
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  SPA-GMN-W20-SEISIN
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  SPA-GMN-W20-HICH
      *
                   WHEN    CONST-H2404-DRHJO (1)
                       MOVE    "2"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (2)
                       MOVE    "3"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (3)
                       MOVE    "4"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (4)
                       MOVE    "5"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (5)
                       MOVE    "6"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (6)
                       MOVE    "7"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (7)
                       MOVE    "8"         TO  SPA-GMN-W20-DRHJO
                   WHEN    CONST-H2404-DRHJO (8)
                       MOVE    "9"         TO  SPA-GMN-W20-DRHJO
      *
                   WHEN    CONST-H2404-KYUKNGHJ (1)
                       MOVE    "2"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (2)
                       MOVE    "3"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (3)
                       MOVE    "4"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-KYUKNGHJ (4)
                       MOVE    "5"         TO  SPA-GMN-W20-KYUKNGHJ
                   WHEN    CONST-H2404-YKNKYUKNGHJ1
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H2404-YKNKYUKNGHJ2
                       MOVE    "3"         TO  SPA-GMN-W20-YKNKYUKNGHJ
                   WHEN    CONST-H2404-KNGYKNHICHI
                       MOVE    "2"         TO  SPA-GMN-W20-KNGYKNHICHI
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  SPA-GMN-W20-SEICHIIKI
                   WHEN    CONST-HYOUKA
                       MOVE    "2"         TO  SPA-GMN-W20-HYOUKA
                   WHEN    CONST-KJTRIHA
                       MOVE    "2"         TO  SPA-GMN-W20-KJTRIHA
                   WHEN    CONST-H2404-KNGHITU1
                       MOVE    "2"         TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H2404-KNGHITU2
                       MOVE    "3"         TO  SPA-GMN-W20-KNGHITU
                   WHEN    CONST-H2404-BTUYKZ
                       MOVE    "2"         TO  SPA-GMN-W20-BTUYKZ
      *
                   WHEN    CONST-H2204-DRTEKCD1
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  SPA-GMN-W20-DRTEK2
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  SPA-GMN-W20-YKNKNKYU
                   WHEN    CONST-H2204-SNRHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH1
                   WHEN    CONST-H2204-SNRHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-SNRHICH2
                   WHEN    CONST-H2204-YKNHICH1
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH1
                   WHEN    CONST-H2204-YKNHICH2
                       MOVE    "2"         TO  SPA-GMN-W20-YKNHICH2
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41006-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(医療観察法)
      *****************************************************************
       41005-KSN-EDIT-SEC               SECTION.
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0 >  SPA-GMN-W20-SYOUBTUBRM-MAX )
      *
                       IF    ( SYS-5001-KSNCD (IDX-KSN)
                                          =    CONST-SYOUBTUBRMCD(IDX0))
                           MOVE    SPA-GMN-W20-SYOUBTUBRMLST(IDX0)
                                          TO   SPA-GMN-W20-SYOUBTUBRM-G
                       END-IF
      *
                   END-PERFORM
      *
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-BTUMIKJNCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-BTUMIKJNLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-BTUMIKJN-G
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
           .
       41005-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H22.4〜H24.3)
      *****************************************************************
       41004-KSN-EDIT-SEC               SECTION.
      *
      *    看護補助加算
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    プレイルーム、保育士等加算
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-NANBYOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-NANBYO-G
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-TOKUSYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKUSYU-G
                   WHEN    CONST-JIDOCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JIDOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JIDO-G
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEISINLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEISIN-G
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HICHLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HICH-G
                   WHEN    CONST-H2204-DRHJO (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (2)
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (3)
                       MOVE    "4"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (4)
                       MOVE    "5"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (5)
                       MOVE    "6"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-H2204-DRHJO (6)
                       MOVE    "7"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-JKAIFUKU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JKAIFUKULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JKAIFUKU-G
                   WHEN    CONST-H2204-DRTEKCD1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEKLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK-G
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEK2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK2-G
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNKNKYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNKNKYU-G
                   WHEN    CONST-H2204-SNRHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH1-G
                   WHEN    CONST-H2204-SNRHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH2-G
                   WHEN    CONST-H2204-YKNHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH1-G
                   WHEN    CONST-H2204-YKNHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH2-G
                   WHEN    CONST-KYUKNGHJ1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-KYUKNGHJLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KYUKNGHJ-G
                   WHEN    CONST-KYUKNGHJ2
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-KYUKNGHJLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KYUKNGHJ-G
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEICHIIKILST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEICHIIKI-G
                   WHEN    CONST-HYOUKA
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HYOUKALST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HYOUKA-G
                   WHEN    CONST-KJTRIHA
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-KJTRIHALST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-KJTRIHA-G
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       41004-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H20.4〜H22.3)
      *****************************************************************
       41003-KSN-EDIT-SEC               SECTION.
      *
      *    看護補助加算
           MOVE    SYS-5001-BTU-KNGHJOKSN
                                       TO  WRK-CMB-CD
           PERFORM 4200-KNGHJOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-KNGHJO-G
      *
      *    食堂加算
           MOVE    SYS-5001-BTU-SYOKUDOKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SYOKUDOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-SYOKUDO-G
      *
      *    プレイルーム、保育士等加算
           MOVE    SYS-5001-BTU-HIKSISUKBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOIKULST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W20-HOIKU-G
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5001-KSN-MAX )
      *
               IF    ( SYS-5001-KSNKBN (IDX-KSN)   =   "2" )
                   EVALUATE    SYS-5001-KSNCD (IDX-KSN)
                   WHEN    CONST-NANBYO
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-NANBYOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-NANBYO-G
                   WHEN    CONST-TOKUSYUCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-TOKUSYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-TOKUSYU-G
                   WHEN    CONST-JIDOCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JIDOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JIDO-G
                   WHEN    CONST-SEISINCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEISINLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEISIN-G
                   WHEN    CONST-KANGOCD (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-HICHLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-HICH-G
                   WHEN    CONST-DRHJO (1)
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (2)
                       MOVE    "3"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (3)
                       MOVE    "4"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-DRHJO (4)
                       MOVE    "5"         TO  WRK-CMB-CD
                       PERFORM 4200-DRHJOLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRHJO-G
                   WHEN    CONST-JKAIFUKU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-JKAIFUKULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-JKAIFUKU-G
                   WHEN    CONST-DRTEKCD
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-DRTEKLST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-DRTEK-G
                   WHEN    CONST-YKNKNKYU
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNKNKYULST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNKNKYU-G
                   WHEN    CONST-SNRHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH1-G
                   WHEN    CONST-SNRHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SNRHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SNRHICH2-G
                   WHEN    CONST-YKNHICH1
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH1LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH1-G
                   WHEN    CONST-YKNHICH2
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-YKNHICH2LST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-YKNHICH2-G
                   WHEN    CONST-SEICHIIKI
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-SEICHIIKILST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-W20-SEICHIIKI-G
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       41003-KSN-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴領域編集処理
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-W20-RIREKILST
                           SPA-NAI-W20-RIREKILST
      *
           MOVE    SPA-GMN-W20-BTUNUM
                                   TO  SPA-NAI-W20-RIREKILST-BTUNUM 
      *
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
      *
           MOVE    ZERO            TO  IDX-LST
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX-LST            >=   CONST-RIREKILST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
               MOVE    IDX-LST    TO  SPA-GMN-W20-RIREKILST-MAX
      *
      *        選択番号
               MOVE    IDX-LST    TO  SPA-GMN-W20-TRSELNUM (IDX-LST)
      *
      *        有効開始日
               MOVE    SYS-5001-STYUKYMD
                                  TO  SPA-NAI-W20-TRYUKOSTYMD (IDX-LST)
                                          WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG
                                  TO  SPA-GMN-W20-TRYUKOSTYMD (IDX-LST)
      *
      *        有効終了日
               MOVE    SYS-5001-EDYUKYMD
                                   TO  SPA-NAI-W20-TRYUKOEDYMD(IDX-LST)
                                           WRK-YMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W20-TRYUKOEDYMD(IDX-LST)
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD-OLD
                                   =   SPA-NAI-W20-TRYUKOSTYMD(IDX-LST))
                AND  ( SPA-NAI-W20-YUKOEDYMD-OLD
                                   =   SPA-NAI-W20-TRYUKOEDYMD(IDX-LST))
                   MOVE    IDX-LST    TO  SPA-GMN-W20-SELNUM
                   MOVE    IDX-LST    TO  SPA-GMN-W20-RIREKILST-SEL
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W20-INPUT-AREA
                       SPA-NAI-W20-INPUT-AREA
      *
           IF    ( SPA-W20-W21-MOTOPG  =    SPACE )
               MOVE    "病室"      TO  SPA-GMN-W20-B09
           END-IF
      *
      *    有効期間の開始日と終了日を設定
           MOVE    SPA-GMN-W20-KJNYMD (1 : 6)
                                   TO  SPA-GMN-W20-YUKOSTYMD (1 : 6)
           MOVE    ". 1"           TO  SPA-GMN-W20-YUKOSTYMD (7 : 3)
           MOVE    SPA-NAI-W20-KJNYMD   (1 : 6)
                                   TO  SPA-NAI-W20-YUKOSTYMD (1 : 6)
           MOVE    "01"            TO  SPA-NAI-W20-YUKOSTYMD (7 : 2)
      *
           MOVE    "99999999"      TO  SPA-GMN-W20-YUKOEDYMD
                                       SPA-NAI-W20-YUKOEDYMD
      *
           MOVE    1               TO  SPA-GMN-W20-CUR
      *
           PERFORM 4100-SYS-KANRI-SAI-SET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算領域初期化処理
      *****************************************************************
       4201-KSN-AREA-CLEAR-SEC         SECTION.
      *
           MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                   TO  SPA-GMN-W20-KNGHJO-G
      *
           MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                   TO  SPA-GMN-W20-SYOUBTUBRM-G
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                      >    ZERO )
                   MOVE   SPA-GMN-W20-KSNLST (IDX-KSN 1)
                                   TO  SPA-GMN-W20-KSNCD-G (IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4201-KSN-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *   履歴情報領域初期化処理
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-W20-RIREKILST
                       SPA-NAI-W20-RIREKILST
      *
           MOVE    ZERO        TO  SPA-GMN-W20-BTULST-SEL
      *
           .
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    システム管理情報再設定処理
      *****************************************************************
       4100-SYS-KANRI-SAI-SET-SEC      SECTION.
      *
      *    医療機関情報取得処理
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-GET-SYS1001-SEC
      *
      *    コンボ初期処理
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-CMB-SYOKI-SEC
      *
           INITIALIZE  SPA-NAI-W20-KSNINF-G
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               MOVE    SPACE               TO  SPA-GMN-W20-KSN-LBL
                                                           (IDX-KSN)
                                               SPA-NAI-W20-KSNINF-CD
                                                           (IDX-KSN)
      *
           END-PERFORM
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT =   CONST-KANSATU )
               MOVE    "未基準（病棟に係る基準を満たさない）"
                                           TO  SPA-GMN-W20-BTUMIKJN-LBL
               MOVE    CONST-BTUMIKJNCD    TO  SPA-NAI-W20-BTUMIKJN-CD
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H240401 )
      *
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "プレイルーム、保育士等加算"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "難病患者等入院診療加算"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "特殊疾患入院施設管理加算"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "精神科応急入院施設管理加算"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "看護配置加算"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "医師事務作業補助体制加算"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H2404-DRHJO (1)
                                           TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "急性期看護補助体制加算"
                                           TO SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-H2404-KYUKNGHJ(1)
                                           TO  SPA-NAI-W20-KYUKNGHJ-CD
      *
               MOVE    "夜間急性期看護補助体制加算"
                                       TO SPA-GMN-W20-YKNKYUKNGHJ-LBL
               MOVE    CONST-H2404-YKNKYUKNGHJ1
                                       TO  SPA-NAI-W20-YKNKYUKNGHJ-CD
      *
               MOVE    "看護職員夜間配置加算"
                                       TO SPA-GMN-W20-KNGYKNHICHI-LBL
               MOVE    CONST-H2404-KNGYKNHICHI
                                       TO  SPA-NAI-W20-KNGYKNHICHI-CD
      *
               MOVE    "精神科地域移行実施加算"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
      *
               MOVE    "一般病棟看護必要度評価加算"
                                            TO SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-HYOUKA         TO SPA-NAI-W20-HYOUKA-CD
      *
               MOVE    "休日リハビリテーション提供体制加算"
                                            TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-KJTRIHA        TO SPA-NAI-W20-KJTRIHA-CD
      *
               MOVE    "看護必要度評価加算"
                                            TO SPA-GMN-W20-KNGHITU-LBL
               MOVE    CONST-H2404-KNGHITU1 TO SPA-NAI-W20-KNGHITU-CD
      *
               MOVE    "病棟薬剤業務実施加算"
                                            TO SPA-GMN-W20-BTUYKZ-LBL
               MOVE    CONST-H2404-BTUYKZ   TO SPA-NAI-W20-BTUYKZ-CD
      *
               MOVE    "医師配置加算１"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H2204-DRTEKCD1  TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "医師配置加算２"    TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "夜間緊急体制確保加算"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "看護配置加算１"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H2204-SNRHICH1
                                           TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "看護配置加算２"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H2204-SNRHICH2
                                           TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "夜間看護配置加算１" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H2204-YKNHICH1
                                           TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "夜間看護配置加算２" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H2204-YKNHICH2
                                            TO  SPA-NAI-W20-YKNHICH2-CD
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H220401 )
      *
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "プレイルーム、保育士等加算"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "難病患者等入院診療加算"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "特殊疾患入院施設管理加算"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "児童・思春期精神科入院医療管理加算"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "精神科応急入院施設管理加算"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "看護配置加算"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "医師事務作業補助体制加算"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-H2204-DRHJO (1)
                                           TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "重症患者回復病棟加算"
                                           TO  SPA-GMN-W20-JKAIFUKU-LBL
               MOVE    CONST-JKAIFUKU      TO  SPA-NAI-W20-JKAIFUKU-CD
               MOVE    "医師配置加算１"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-H2204-DRTEKCD1  TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "医師配置加算２"    TO  SPA-GMN-W20-DRTEK2-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK2-CD
               MOVE    "夜間緊急体制確保加算"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "看護配置加算１"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-H2204-SNRHICH1
                                           TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "看護配置加算２"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-H2204-SNRHICH2
                                           TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "夜間看護配置加算１" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-H2204-YKNHICH1
                                           TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "夜間看護配置加算２" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-H2204-YKNHICH2
                                            TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "急性期看護補助体制加算"
                                           TO SPA-GMN-W20-KYUKNGHJ-LBL
               MOVE    CONST-KYUKNGHJ1     TO  SPA-NAI-W20-KYUKNGHJ-CD
               MOVE    "精神科地域移行実施加算"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
               MOVE    "一般病棟看護必要度評価加算"
                                            TO SPA-GMN-W20-HYOUKA-LBL
               MOVE    CONST-HYOUKA         TO SPA-NAI-W20-HYOUKA-CD
               MOVE    "休日リハビリテーション提供体制加算"
                                            TO SPA-GMN-W20-KJTRIHA-LBL
               MOVE    CONST-KJTRIHA        TO SPA-NAI-W20-KJTRIHA-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H200401 )
      *
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "プレイルーム、保育士等加算"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "難病患者等入院診療加算"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "特殊疾患入院施設管理加算"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "児童・思春期精神科入院医療管理加算"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "精神科応急入院施設管理加算"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "看護配置加算"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "医師事務作業補助体制加算"
                                           TO  SPA-GMN-W20-DRHJO-LBL
               MOVE    CONST-DRHJO (1)     TO  SPA-NAI-W20-DRHJO-CD
               MOVE    "重症患者回復病棟加算"
                                           TO  SPA-GMN-W20-JKAIFUKU-LBL
               MOVE    CONST-JKAIFUKU      TO  SPA-NAI-W20-JKAIFUKU-CD
               MOVE    "医師配置適合加算"      TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK-CD
               MOVE    "夜間緊急体制確保加算"
                                           TO  SPA-GMN-W20-YKNKNKYU-LBL
               MOVE    CONST-YKNKNKYU      TO  SPA-NAI-W20-YKNKNKYU-CD
               MOVE    "看護配置加算１"    TO  SPA-GMN-W20-SNRHICH1-LBL
               MOVE    CONST-SNRHICH1      TO  SPA-NAI-W20-SNRHICH1-CD
               MOVE    "看護配置加算２"    TO  SPA-GMN-W20-SNRHICH2-LBL
               MOVE    CONST-SNRHICH2      TO  SPA-NAI-W20-SNRHICH2-CD
               MOVE    "夜間看護配置加算１" TO SPA-GMN-W20-YKNHICH1-LBL
               MOVE    CONST-YKNHICH1      TO  SPA-NAI-W20-YKNHICH1-CD
               MOVE    "夜間看護配置加算２" TO SPA-GMN-W20-YKNHICH2-LBL
               MOVE    CONST-YKNHICH2      TO  SPA-NAI-W20-YKNHICH2-CD
               MOVE    "精神科地域移行実施加算"
                                            TO SPA-GMN-W20-SEICHIIKI-LBL
               MOVE    CONST-SEICHIIKI      TO SPA-NAI-W20-SEICHIIKI-CD
      *
           WHEN  ( SPA-NAI-W20-YUKOSTYMD   >=  CONST-H180401 )
      *
               MOVE    "夜間勤務等看護加算"  TO  SPA-GMN-W20-YKNKNG-LBL
               MOVE    CONST-YKNKNGCD(1)   TO  SPA-NAI-W20-YKNKNG-CD
      *
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "プレイルーム、保育士等加算"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "難病患者等入院診療加算"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "特殊疾患入院施設管理加算"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "児童・思春期精神科入院医療管理加算"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "精神科応急入院施設管理加算"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "看護配置加算"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
      *
               MOVE    "看護配置適合加算"  TO  SPA-GMN-W20-HICHTEK-LBL
               MOVE    CONST-KNGTEKCD      TO  SPA-NAI-W20-HICHTEK-CD
      *
               MOVE    "複数医師及び看護配置適合加算"
                                           TO  SPA-GMN-W20-DRTEK-LBL
               MOVE    CONST-DRTEKCD       TO  SPA-NAI-W20-DRTEK-CD
           WHEN    OTHER
               MOVE    "夜間勤務等看護加算"  TO  SPA-GMN-W20-YKNKNG-LBL
               MOVE    CONST-YKNKNGCD(1)   TO  SPA-NAI-W20-YKNKNG-CD
      *
               MOVE    "食堂加算"          TO  SPA-GMN-W20-SYOKUDO-LBL
               MOVE    CONST-SYOKUDOCD     TO  SPA-NAI-W20-SYOKUDO-CD
      *
               MOVE    "小児入院医療管理料加算"
                                           TO  SPA-GMN-W20-HOIKU-LBL
               MOVE    CONST-HOIKUCD       TO  SPA-NAI-W20-HOIKU-CD
      *
               MOVE    "難病患者等入院診療加算"
                                           TO  SPA-GMN-W20-NANBYO-LBL
               MOVE    CONST-NANBYO        TO  SPA-NAI-W20-NANBYO-CD
      *
               MOVE    "特殊疾患入院施設管理加算"
                                           TO  SPA-GMN-W20-TOKUSYU-LBL
               MOVE    CONST-TOKUSYUCD     TO  SPA-NAI-W20-TOKUSYU-CD
      *
               MOVE    "児童・思春期精神科入院医療管理加算"
                                           TO  SPA-GMN-W20-JIDO-LBL
               MOVE    CONST-JIDOCD        TO  SPA-NAI-W20-JIDO-CD
      *
               MOVE    "精神科応急入院施設管理加算"
                                           TO  SPA-GMN-W20-SEISIN-LBL
               MOVE    CONST-SEISINCD      TO  SPA-NAI-W20-SEISIN-CD
      *
               MOVE    "看護配置加算"      TO  SPA-GMN-W20-HICH-LBL
               MOVE    CONST-KANGOCD (1)   TO  SPA-NAI-W20-HICH-CD
           END-EVALUATE
      *
           MOVE    ZERO                    TO  IDX-GKSN
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                                      NOT  =   SPACE )
      *
                   MOVE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                                       TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       IF    ( TNS-HOSPSRYKBN  =   ZERO )
                        OR   ( TNS-HOSPSRYKBN  =   SPA-NAI-W20-HOSPSBT )
                           COMPUTE    IDX-GKSN =   IDX-GKSN +   1
                           MOVE   IDX-GKSN  TO  SPA-NAI-W20-KSNINF-IDX
                                                               (IDX-KSN)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-SYS-KANRI-SAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    病棟名称が入力されていること
           IF    ( SPA-GMN-W20-NAME    NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1007"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    短縮病棟名が入力されていること
           IF    ( SPA-GMN-W20-TANNAME NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1008"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    病院の場合、病院の種別が選択されていること
           IF    ( SPA-NAI-W20-HOSPSBT     =   1 )
               IF    ( SPA-GMN-W20-SBT     =   "01"
                                           OR  "02"
                                           OR  "03"
                                           OR  "04"
                                           OR  "05"
                                           OR  "06"
                                           OR  "07"
                                           OR  "10" )
                   CONTINUE
               ELSE
                   MOVE    "1106"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-W20-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    診療所の場合、診療所の種別が選択されていること
           IF    ( SPA-NAI-W20-HOSPSBT     =   2 )
               IF    ( SPA-GMN-W20-SBT     =   "08"
                                           OR  "09" )
                   CONTINUE
               ELSE
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-W20-CUR
                   GO  TO  4900-KANRENCHK-EXT
               END-IF
           END-IF
      *
      *    有効期間開始日が入力されていること
           IF    ( SPA-NAI-W20-YUKOSTYMD   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1102"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間終了日が入力されていること
           IF    ( SPA-NAI-W20-YUKOEDYMD   NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1103"      TO  SPA-ERRCD
               MOVE    52          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間開始日≦有効期間終了日となっていること
           IF    ( SPA-NAI-W20-YUKOSTYMD   <=  SPA-NAI-W20-YUKOEDYMD )
               CONTINUE
           ELSE
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    51          TO  SPA-GMN-W20-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   CONST-KANSATU )
               CONTINUE
           ELSE
      *
      *        特定入院料が設定されていない場合病棟ごとに必須項目を
      *        チェックする
               IF    ( SPA-GMN-W20-TOKNYUIN-G      =   SPACE )
      *
      *            病棟種別が設定されていること
                   IF    ( SPA-GMN-W20-SBT     NOT =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "1009"  TO  SPA-ERRCD
                       MOVE    6       TO  SPA-GMN-W20-CUR
                       GO  TO  4900-KANRENCHK-EXT
                   END-IF
      *
      *            入院基本料が算定されていること
                   IF    ( SPA-GMN-W20-KHNRYO      =   SPACE )
                       IF    ( SPA-GMN-W20-SBT       =   "04" OR "09"  )
                        AND  ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H180701 )
                            CONTINUE
                       ELSE
                           MOVE    "1014"  TO  SPA-ERRCD
                           GO  TO  4900-KANRENCHK-EXT
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       4901-SSTKIJUN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-KSNMEI
      *
      *    施設基準検索処理
           PERFORM 4100-SYS-1006-SEL-SEC
      *
      *    入院料加算チェック
           IF    ( SPA-GMN-W20-KNGHJO      =   "00" )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-LOOPEND
               PERFORM VARYING IDX-KSN FROM    1   BY  1
                       UNTIL ( IDX-KSN >   CONST-KNGHJOCD-MAX )
                        OR   ( FLG-LOOPEND     =   1 )
                   IF    ( SPA-GMN-W20-KNGHJO
                                       =   CONST-KNGHJOCDL (IDX-KSN))
      *
                       MOVE    CONST-KNGHJOCD (IDX-KSN)
                                       TO  WRK-KSNCD
                       PERFORM 41011-SSTKIJUN-CHK-SEC
                       IF    ( FLG-KSNERR      =   ZERO )
                           MOVE    1   TO  FLG-LOOPEND
                       END-IF
      *
                   END-IF
               END-PERFORM
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-KSNERR      =   1 )
                       MOVE    "1011"      TO  SPA-ERRCD
                       MOVE    9           TO  SPA-GMN-W20-CUR
                       MOVE    SPA-GMN-W20-KNGHJOMEI
                                       TO  WRK-KSNMEI
      *
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H240401 )
                   PERFORM 49012-SSTKIJUN-CHK-SEC
               ELSE
                   PERFORM 49011-SSTKIJUN-CHK-SEC
               END-IF
           END-IF
      *
           .
       4901-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理 (〜H24.3)
      *****************************************************************
       49011-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
                   MOVE    SPACE   TO  WRK-KSNCD
      *
                   EVALUATE    SPA-NAI-W20-KSNINF-CD (IDX-KSN)
                   WHEN    CONST-YKNKNGCD (1)
                       EVALUATE    SPA-GMN-W20-YKNKNG
                       WHEN    "1"
                           MOVE    CONST-YKNKNGCD (1)
                                           TO  WRK-KSNCD
                       WHEN    "2"
                           MOVE    CONST-YKNKNGCD (2)
                                           TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-YKNKNGCD (3)
                                           TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-YKNKNGCD (4)
                                           TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-YKNKNGCD (5)
                                           TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-SYOKUDOCD
                       CONTINUE
                   WHEN    CONST-HOIKUCD
                       IF    ( SPA-GMN-W20-HOIKU   =   "2" )
                           MOVE    CONST-HOIKUCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-NANBYO
                       IF    ( SPA-GMN-W20-NANBYO   =   "2" )
                           MOVE    CONST-NANBYO
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-TOKUSYUCD
                       IF    ( SPA-GMN-W20-TOKUSYU  =   "2" )
                           MOVE    CONST-TOKUSYUCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-JIDOCD
                       IF    ( SPA-GMN-W20-JIDO     =   "2" )
                           MOVE    CONST-JIDOCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-SEISINCD
                       IF    ( SPA-GMN-W20-SEISIN   =   "2" )
                           MOVE    CONST-SEISINCD
                                           TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-KANGOCD (1)
                       IF    ( SPA-NAI-W20-YUKOSTYMD >=  CONST-H180401 )
                          IF    ( SPA-GMN-W20-HICH    =   "2" )
                              MOVE    CONST-KANGOCD (1)
                                                      TO  WRK-KSNCD
                          END-IF
                       ELSE
                           PERFORM VARYING IDX1    FROM    1   BY  1
                                   UNTIL ( IDX1    >   5 )
                                    OR   ( WRK-KSNCD   NOT =   SPACE )
      *
                               IF    ( SPA-GMN-W20-HICH
                                               =   CONST-KANGOCDL(IDX1))
                                   MOVE    CONST-KANGOCD (IDX1)
                                               TO  WRK-KSNCD
                               END-IF
      *
                           END-PERFORM
                       END-IF
                   WHEN    CONST-H2204-DRTEKCD1
                           IF    ( SPA-GMN-W20-DRTEK   =  "2" )
                               MOVE    CONST-H2204-DRTEKCD1
                                               TO  WRK-KSNCD
                           END-IF
                   WHEN    CONST-DRTEKCD
                       IF    ( SPA-NAI-W20-YUKOSTYMD  >=  CONST-H220401)
                           IF    ( SPA-GMN-W20-DRTEK2  =  "2" )
                               MOVE    CONST-DRTEKCD
                                               TO  WRK-KSNCD
                           END-IF
                       ELSE
                           IF    ( SPA-GMN-W20-DRTEK   =  "2" )
                               MOVE    CONST-DRTEKCD
                                               TO  WRK-KSNCD
                           END-IF
                       END-IF
                   WHEN    CONST-H2204-DRHJO (1)
                       EVALUATE    SPA-GMN-W20-DRHJO
                       WHEN    "2"
                           MOVE    CONST-H2204-DRHJO (1)
                                                   TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-H2204-DRHJO (2)
                                                   TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-H2204-DRHJO (3)
                                                   TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-H2204-DRHJO (4)
                                                   TO  WRK-KSNCD
                       WHEN    "6"
                           MOVE    CONST-H2204-DRHJO (5)
                                                   TO  WRK-KSNCD
                       WHEN    "7"
                           MOVE    CONST-H2204-DRHJO (6)
                                                   TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-DRHJO (1)
                       EVALUATE    SPA-GMN-W20-DRHJO
                       WHEN    "2"
                           MOVE    CONST-DRHJO (1) TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-DRHJO (2) TO  WRK-KSNCD
                       WHEN    "4"
                           MOVE    CONST-DRHJO (3) TO  WRK-KSNCD
                       WHEN    "5"
                           MOVE    CONST-DRHJO (4) TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-JKAIFUKU
                       IF   (  SPA-GMN-W20-JKAIFUKU    =   "2" )
                           MOVE    CONST-JKAIFUKU  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-YKNKNKYU
                       IF   (  SPA-GMN-W20-YKNKNKYU    =   "2" )
                           MOVE    CONST-YKNKNKYU  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-SNRHICH1
                   WHEN    CONST-H2204-SNRHICH2
                       IF    ( SPA-NAI-W20-YUKOSTYMD  >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-SNRHICH2    =   "2" )
                               MOVE    CONST-H2204-SNRHICH2
                                                       TO  WRK-KSNCD
                           END-IF
                       ELSE
                           IF    ( SPA-GMN-W20-SNRHICH1    =   "2" )
                               MOVE    CONST-SNRHICH1  TO  WRK-KSNCD
                           END-IF
                       END-IF
                   WHEN    CONST-H2204-SNRHICH1
                   WHEN    CONST-SNRHICH2
                   WHEN    CONST-KNGTEKCD
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-SNRHICH1    =   "2" )
                               MOVE    CONST-H2204-SNRHICH1
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H200401 )
                           IF    ( SPA-GMN-W20-SNRHICH2  =   "2" )
                               MOVE    CONST-SNRHICH2  TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-HICHTEK     =   "2" )
                               MOVE    CONST-KNGTEKCD  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-YKNHICH1
                   WHEN    CONST-H2204-YKNHICH2
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-YKNHICH2    =   "2" )
                               MOVE    CONST-H2204-YKNHICH2
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-YKNHICH1    =   "2" )
                               MOVE    CONST-YKNHICH1  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-YKNHICH2
                   WHEN    CONST-H2204-YKNHICH1
                       EVALUATE    TRUE
                       WHEN    ( SPA-NAI-W20-YUKOSTYMD
                                              >= CONST-H220401 )
                           IF    ( SPA-GMN-W20-YKNHICH1    =   "2" )
                               MOVE    CONST-H2204-YKNHICH1
                                                       TO  WRK-KSNCD
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-GMN-W20-YKNHICH2    =   "2" )
                               MOVE    CONST-YKNHICH2  TO  WRK-KSNCD
                           END-IF
                       END-EVALUATE
                   WHEN    CONST-KYUKNGHJ1
                       EVALUATE    SPA-GMN-W20-KYUKNGHJ
                       WHEN    "2"
                           MOVE    CONST-KYUKNGHJ1  TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-KYUKNGHJ2  TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-SEICHIIKI
                       IF    ( SPA-GMN-W20-SEICHIIKI    =   "2" )
                           MOVE    CONST-SEICHIIKI  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-HYOUKA
                       IF    ( SPA-GMN-W20-HYOUKA   =   "2" )
                           MOVE    CONST-HYOUKA     TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-KJTRIHA
                       IF    ( SPA-GMN-W20-KJTRIHA   =   "2" )
                           MOVE    CONST-KJTRIHA     TO  WRK-KSNCD
                       END-IF
                   END-EVALUATE
      *
                   IF    ( WRK-KSNCD       NOT =   SPACE )
                       PERFORM 41011-SSTKIJUN-CHK-SEC
      *
                       IF    ( FLG-SSTKIJUNERR     =   1 )
                           MOVE    "1010"          TO  SPA-ERRCD
                           COMPUTE SPA-GMN-W20-CUR
                               =   SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                               +   9
                           MOVE    SPA-GMN-W20-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                       END-IF
      *
                       IF    ( SPA-ERRCD           =   SPACE )
                           IF    ( FLG-KSNERR      =   1 )
                               MOVE    "1011"  TO  SPA-ERRCD
                               COMPUTE SPA-GMN-W20-CUR
                                   =   SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   +   9
                               MOVE    SPA-GMN-W20-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       49011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理 (H24.4〜)
      *****************************************************************
       49012-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM 4201-BTU-KSN-EDIT-SEC
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   TKSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               MOVE    TKSN-SRYCD (IDX-KSN)    TO  WRK-KSNCD
      *
               PERFORM 41011-SSTKIJUN-CHK-SEC
      *
               IF    ( FLG-SSTKIJUNERR     =   1 )
                   MOVE    "1010"      TO  SPA-ERRCD
                   COMPUTE SPA-GMN-W20-CUR
                       =   TKSN-IDX (IDX-KSN)
                       +   9
                   MOVE    TKSN-LBL (IDX-KSN)
                                       TO  WRK-KSNMEI
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-KSNERR      =   1 )
                       MOVE    "1011"  TO  SPA-ERRCD
                       COMPUTE SPA-GMN-W20-CUR
                           =   TKSN-IDX (IDX-KSN)
                           +   9
                       MOVE    TKSN-LBL (IDX-KSN)
                                       TO  WRK-KSNMEI
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       49012-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-SSTKJNCD-G
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
                                       FLG-KSNERR
                                       WRK-NYUKSNKBN-G
                                       WRK-KSNKSNKBN
                                       CNT-NYUKSNKBNCHK
      *
           MOVE    SPACE           TO  WRK-KSNCD-SEN
                                       WRK-KSNCD-TOK
           EVALUATE    WRK-KSNCD
           WHEN    CONST-HYOUKA
               MOVE    CONST-HYOUKA-SEN         TO  WRK-KSNCD-SEN
               MOVE    CONST-HYOUKA-TOK         TO  WRK-KSNCD-TOK
           WHEN    CONST-H2404-KNGHITU1
               MOVE    CONST-H2404-KNGHITU1-SEN TO  WRK-KSNCD-SEN
               MOVE    CONST-H2404-KNGHITU1-TOK TO  WRK-KSNCD-TOK
           WHEN    CONST-H2404-KNGHITU2
               MOVE    CONST-H2404-KNGHITU2-SEN TO  WRK-KSNCD-SEN
               MOVE    CONST-H2404-KNGHITU2-TOK TO  WRK-KSNCD-TOK
           END-EVALUATE
           IF    ( WRK-KSNCD-SEN   NOT =   SPACE )
               IF    ( SPA-GMN-W20-SBT =   "06" )
                   MOVE    WRK-KSNCD-SEN   TO  WRK-KSNCD
               ELSE
                   MOVE    SPA-NAI-W20-KHNSRYCD
                                           TO  WRK-TNS-SRYCD
                   MOVE    SPA-NAI-W20-YUKOSTYMD
                                           TO  WRK-TNS-KJNYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( TNS-CDKBN-KBN   =   "A" )
                       IF    ( TNS-CDKBN-KBNNUM    =   104 )
                           MOVE    WRK-KSNCD-TOK
                                           TO  WRK-KSNCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W20-KHNSRYCD    NOT =   SPACE )
               EVALUATE    SPA-NAI-W20-KHNSRYCD
               WHEN    "099999950"
                   MOVE    "190097010"         TO  WRK-TNS-SRYCD
               WHEN    "099999951"
                   MOVE    "190097210"         TO  WRK-TNS-SRYCD
               WHEN    "099999952"
                   MOVE    "190135110"         TO  WRK-TNS-SRYCD
               WHEN    OTHER
                   MOVE    SPA-NAI-W20-KHNSRYCD
                                               TO  WRK-TNS-SRYCD
               END-EVALUATE
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN        TO  WRK-NYUKSNKBN (1)
      *
           END-IF
      *
           IF    ( SPA-GMN-W20-TOKNYUIN    NOT =   SPACE )
               MOVE    SPA-GMN-W20-TOKNYUIN    TO  WRK-CMB-CD
               PERFORM 4200-TOKNYUINLST-CHK-SEC
               MOVE    WRK-CMB-ITM3            TO  WRK-TNS-SRYCD
               MOVE    SPA-NAI-W20-YUKOSTYMD   TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUKHNKSNKBN        TO WRK-NYUKSNKBN (2)
           END-IF
      *
           MOVE    2                           TO  WRK-NYUKSNKBN-MAX
      *
           MOVE    WRK-KSNCD                   TO  WRK-TNS-SRYCD
           MOVE    SPA-NAI-W20-YUKOSTYMD       TO  WRK-TNS-KJNYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-NYUKHNKSNKBN TO WRK-KSNKSNKBN
      *
           MOVE    ZERO            TO  FLG-HIT
                                       CNT-SST
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   10 )
                    OR   ( FLG-HIT     =   1 )
      *
               IF    ( TNS-SSTKIJUNCD (IDX5)   >   ZERO )
                   COMPUTE CNT-SST =   CNT-SST +   1
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  WRK-SSTKJNCD9(CNT-SST)
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  IDX-SST
                   IF    ( SSTKJN-CD (IDX-SST) =   1 )
                       MOVE    1   TO  FLG-HIT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SST     >   ZERO )
               IF    ( FLG-HIT =   ZERO )
                   MOVE        1   TO  FLG-SSTKIJUNERR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-SBT     =   "04"  OR  "09" )
               CONTINUE
           ELSE
      *
               IF    ( FLG-SSTKIJUNERR     =   ZERO )
                AND  ( WRK-KSNKSNKBN       >   ZERO )
      *
                   MOVE    ZERO        TO  FLG-HIT
      *
                   PERFORM VARYING IDX6    FROM    1   BY  1
                           UNTIL ( IDX6    >   WRK-NYUKSNKBN-MAX )
                            OR   ( FLG-HIT =   1 )
      *
                       IF    ( WRK-NYUKSNKBN (IDX6)    >   ZERO )
                           COMPUTE CNT-NYUKSNKBNCHK
                               =   CNT-NYUKSNKBNCHK
                               +   1
                           MOVE    WRK-NYUKSNKBN (IDX6)
                                                       TO  WRK-NYUINKBN
                           MOVE    WRK-KSNKSNKBN       TO  WRK-KSNKBN
                           MOVE    SPA-NAI-W20-YUKOSTYMD
                                                       TO  WRK-YUKOSTYMD
                           PERFORM 900-NYUKSNCHK-KEY-SEL-SEC
                           IF    ( NYUKSNCHK-CHKKBN    =   "1" OR "2" )
                               MOVE    1       TO  FLG-HIT
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF    ( CNT-NYUKSNKBNCHK    >   ZERO )
                       IF   (  FLG-HIT     =       ZERO )
                            MOVE    1      TO      FLG-KSNERR
                       END-IF
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
           MOVE    SPA-NAI-W20-YUKOSTYMD
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
      *    病棟番号検索処理
      *****************************************************************
       4100-BTUNUM-LSTSEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-BTULST
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX-BTULST  >   ZERO )
                OR       ( IDX1        >   SPA-GMN-W20-BTULST-MAX )
      *
      *
               IF    ( SPA-GMN-W20-BTUNUM
                                       =   SPA-GMN-W20-TBTUNUM (IDX1) )
                   MOVE    IDX1    TO  IDX-BTULST
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-BTUNUM-LSTSEL-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4200-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKNYUINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-W20-TOKNYUINL-TANMEI (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W20-TOKNYUINL-SRYCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W20-TOKNYUINL-KBNCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKUTEINYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボチェック処理
      *****************************************************************
       4201-TOKNYUINLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKNYUINL-KBNCD (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKNYUINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-GMN-W20-TOKNYUINL-TANMEI (IDX-CMB)
                                       TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W20-TOKNYUINL-SRYCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W20-TOKNYUINL-KBNCD  (IDX-CMB)
                                       TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4201-TOKUTEINYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科コードコンボチェック処理
      *****************************************************************
       4200-SRYKALST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SRYKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SRYKAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SRYKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟種別コンボチェック処理
      *****************************************************************
       4200-SBTLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SBT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SBTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SBTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SBTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護補助加算コンボチェック処理
      *****************************************************************
       4200-KNGHJOLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KNGHJO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KNGHJOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KNGHJOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-KNGHJOLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KNGHJOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    夜間勤務等看護加算コンボチェック処理
      *****************************************************************
       4200-YAKANKNGLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNKNG-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNKNGL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNKNGLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-W20-YKNKNGLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YAKANKNGLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    食堂加算コンボチェック処理
      *****************************************************************
       4200-SYOKUDOLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOKUDO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SYOKUDOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOKUDOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOKUDOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOKUDOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保育士・プレイルーム加算コンボチェック処理
      *****************************************************************
       4200-HOIKULST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HOIKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HOIKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HOIKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HOIKULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HOIKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    難病患者等入院診療加算コンボチェック処理
      *****************************************************************
       4200-NANBYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-NANBYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-NANBYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-NANBYOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-NANBYOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NANBYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特殊疾患入院施設管理加算コンボチェック処理
      *****************************************************************
       4200-TOKUSYULST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-TOKUSYU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-TOKUSYUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-TOKUSYULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-TOKUSYULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TOKUSYULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    児童・思春期精神科入院医療管理加算コンボチェック処理
      *****************************************************************
       4200-JIDOLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-JIDO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-JIDOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-JIDOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-JIDOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JIDOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神科応急入院施設管理加算コンボチェック処理
      *****************************************************************
       4200-SEISINLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SEISIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SEISINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SEISINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SEISINLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SEISINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算コンボチェック処理
      *****************************************************************
       4200-HICHLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HICH-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HICHL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HICHLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HICHLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HICHLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数医師及び看護配置適合加算コンボチェック処理
      *****************************************************************
       4200-DRTEKLST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRTEK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRTEKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRTEKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRTEKLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRTEKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医師配置加算２コンボチェック処理
      *****************************************************************
       4200-DRTEK2LST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRTEK2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRTEK2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRTEK2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRTEK2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRTEK2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護配置適合加算コンボチェック処理
      *****************************************************************
       4200-HICHTEKLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HICHTEK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HICHTEKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HICHTEKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HICHTEKLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HICHTEKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医師事務作業補助体制加算コンボチェック処理
      *****************************************************************
       4200-DRHJOLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-DRHJO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-DRHJOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-DRHJOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-DRHJOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-DRHJOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    重症患者回復病棟加算コンボチェック処理
      *****************************************************************
       4200-JKAIFUKULST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-JKAIFUKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-JKAIFUKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-JKAIFUKULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-JKAIFUKULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JKAIFUKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    夜間緊急体制確保加算コンボチェック処理
      *****************************************************************
       4200-YKNKNKYULST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNKNKYU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNKNKYUL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNKNKYULST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNKNKYULST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNKNKYULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算１（診療所）コンボチェック処理
      *****************************************************************
       4200-SNRHICH1LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SNRHICH1-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SNRHICH1L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SNRHICH1LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SNRHICH1LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SNRHICH1LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    看護配置加算２（診療所）コンボチェック処理
      *****************************************************************
       4200-SNRHICH2LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SNRHICH2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SNRHICH2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SNRHICH2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SNRHICH2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SNRHICH2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    夜間看護配置加算１コンボチェック処理
      *****************************************************************
       4200-YKNHICH1LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNHICH1-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNHICH1L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNHICH1LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNHICH1LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNHICH1LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    夜間看護配置加算２コンボチェック処理
      *****************************************************************
       4200-YKNHICH2LST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-YKNHICH2-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-YKNHICH2L (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-YKNHICH2LST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-YKNHICH2LST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-YKNHICH2LST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性期看護補助体制加算コンボチェック処理
      *****************************************************************
       4200-KYUKNGHJLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KYUKNGHJ-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KYUKNGHJL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KYUKNGHJLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-KYUKNGHJLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KYUKNGHJLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    精神科地域移行実施加算コンボチェック処理
      *****************************************************************
       4200-SEICHIIKILST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SEICHIIKI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SEICHIIKIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SEICHIIKILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SEICHIIKILST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SEICHIIKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一般病棟看護必要度評価加算コンボチェック処理
      *****************************************************************
       4200-HYOUKALST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-HYOUKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-HYOUKAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-HYOUKALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-HYOUKALST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HYOUKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    休日リハビリテーション提供体制加算コンボチェック処理
      *****************************************************************
       4200-KJTRIHALST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KJTRIHA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KJTRIHAL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-KJTRIHALST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-KJTRIHALST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-KJTRIHALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    未基準（病棟に係る基準をみたさない場合）コンボチェック処理
      *****************************************************************
       4200-BTUMIKJNLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-BTUMIKJN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-BTUMIKJNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-BTUMIKJNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-BTUMIKJNLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-BTUMIKJNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小規格病棟病床数加算コンボチェック処理
      *****************************************************************
       4200-SYOUBTUBRMLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOUBTUBRM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-SYOUBTUBRML (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    CONST-SYOUBTUBRMCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYOUBTUBRMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小規格病棟病床数加算コンボチェック処理
      *****************************************************************
       4201-SYOUBTUBRMLST-CHK-SEC        SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-SYOUBTUBRM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( CONST-SYOUBTUBRMCD (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    CONST-SYOUBTUBRMCD (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W20-SYOUBTUBRMLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4201-SYOUBTUBRMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプト表示区分コンボチェック処理
      *****************************************************************
       4200-RECEDSPKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-RECEDSPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-RECEDSPKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4200-RECEDSPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプト表示区分コンボチェック処理
      *****************************************************************
       4201-RECEDSPKBNLST-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-RECEDSPKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W20-RECEDSPKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4201-RECEDSPKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了区分区分コンボチェック処理
      *****************************************************************
       4200-EDKBNLST-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-EDKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-EDKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W20-EDKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-EDKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本料コンボチェック処理
      *****************************************************************
       4200-KHNRYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KHNRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W20-KHNRYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W20-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
      *
                   MOVE    SPA-NAI-W20-KHNTENLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM3
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KHNRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本料コンボチェック処理
      *****************************************************************
       4201-KHNRYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W20-KHNRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W20-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-NAI-W20-KHNSRYCDLST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
      *
                   MOVE    SPA-NAI-W20-KHNTENLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM3
               END-IF
      *
           END-PERFORM
      *
           .
       4201-KHNRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W20    "           TO  MCP-WINDOW.
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
           MOVE    W20-BTULST-ROW          TO  WRK-BTULST-ROW
      *
           MOVE    SPACE                   TO  W20
           INITIALIZE                          W20
      *
      *    病室ボタン
           MOVE    SPA-GMN-W20-B09         TO  W20-B09
      *
      *    入院料加算ボタン
           MOVE    "入院料加算"            TO  W20-B10
      *
      *    基準日
           MOVE    SPA-GMN-W20-KJNYMD      TO  W20-KJNYMD
      *
      *    病棟番号
           MOVE    SPA-GMN-W20-BTUNUM      TO  W20-BTUNUM
      *
      *    病棟名称
           MOVE    SPA-GMN-W20-NAME        TO  W20-NAME
      *
      *    短縮病棟名
           MOVE    SPA-GMN-W20-TANNAME     TO  W20-TANNAME
      *
      *    特定入院料コンボ
           MOVE    SPA-GMN-W20-TOKNYUIN-G  TO  W20-TOKUTEINYUIN
           MOVE    SPA-GMN-W20-TOKNYUIN-MAX
                                           TO  W20-TOKUTEINYUIN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-TOKUTEINYUIN-CNT )
                MOVE   SPA-GMN-W20-TOKNYUINLST (IDX1)
                                   TO  W20-TOKUTEINYUIN-ITM (IDX1)
           END-PERFORM
      *
      *    病棟科名コンボ
           MOVE    SPA-GMN-W20-SRYKA-G     TO  W20-SRYKA
           MOVE    SPA-GMN-W20-SRYKA-MAX   TO  W20-SRYKA-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-SRYKA-CNT )
                MOVE   SPA-GMN-W20-SRYKALST (IDX1)
                                   TO  W20-SRYKA-ITM (IDX1)
           END-PERFORM
      *
      *    病棟種別コンボ
           MOVE    SPA-GMN-W20-SBT-G   TO  W20-SBT
           MOVE    SPA-GMN-W20-SBT-MAX TO  W20-SBT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-SBT-CNT )
                MOVE   SPA-GMN-W20-SBTLST (IDX1)
                                   TO  W20-SBT-ITM (IDX1)
           END-PERFORM
      *
      *    レセプト表示区分
           MOVE    SPA-GMN-W20-RECEDSPKBN-G    TO  W20-RECEDSPKBN
           MOVE    SPA-GMN-W20-RECEDSPKBN-MAX
                                       TO  W20-RECEDSPKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-RECEDSPKBN-CNT )
                MOVE   SPA-GMN-W20-RECEDSPKBNLST (IDX1)
                                   TO  W20-RECEDSPKBN-ITM (IDX1)
           END-PERFORM
      *
      *    有効期間開始日
           MOVE    SPA-GMN-W20-YUKOSTYMD   TO  W20-YUKOSTYMD
      *
      *    有効期間終了日
           MOVE    SPA-GMN-W20-YUKOEDYMD   TO  W20-YUKOEDYMD
      *
      *    病棟終了区分
           MOVE    SPA-GMN-W20-EDKBN-G     TO  W20-EDKBN
           MOVE    SPA-GMN-W20-EDKBN-MAX   TO  W20-EDKBN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-EDKBN-CNT )
                MOVE   SPA-GMN-W20-EDKBNLST (IDX1)
                                   TO  W20-EDKBN-ITM (IDX1)
           END-PERFORM
      *
      *    入院基本料コンボ
           MOVE    SPA-GMN-W20-KHNRYO-G    TO  W20-KHNRYO
           MOVE    SPA-GMN-W20-KHNRYO-MAX  TO  W20-KHNRYO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-KHNRYO-CNT )
                MOVE   SPA-GMN-W20-KHNRYOLST (IDX1)
                                   TO  W20-KHNRYO-ITM (IDX1)
           END-PERFORM
      *
      *    入院基本点数
           MOVE    SPA-NAI-W20-KHNTEN      TO  W20-KHNTEN
      *
      *
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-W20-SBT         =   CONST-KANSATU )
      *        小規格病棟病床数加算（看護補助加算コンボを借用）
               MOVE    "小規格病棟病床数加算"
                                       TO  W20-LBLKNGHJO
               MOVE    SPA-GMN-W20-SYOUBTUBRM-G
                                       TO  W20-KNGHJO
               MOVE    SPA-GMN-W20-SYOUBTUBRM-MAX
                                       TO  W20-KNGHJO-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   W20-KNGHJO-CNT )
                    MOVE   SPA-GMN-W20-SYOUBTUBRMLST (IDX1)
                                   TO  W20-KNGHJO-ITM (IDX1)
               END-PERFORM
           WHEN  ( SPA-NAI-W20-HOSPSBT     =   1 )
      *
               MOVE    "看護補助加算"      TO  W20-LBLKNGHJO
      *
      *        看護補助加算コンボ
               MOVE    SPA-GMN-W20-KNGHJO-G
                                       TO  W20-KNGHJO
               MOVE    SPA-GMN-W20-KNGHJO-MAX
                                       TO  W20-KNGHJO-CNT
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   W20-KNGHJO-CNT )
                    MOVE   SPA-GMN-W20-KNGHJOLST (IDX1)
                                   TO  W20-KNGHJO-ITM (IDX1)
               END-PERFORM
           END-EVALUATE
      *
      *    入院料加算
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W20-KSNINF-IDX (IDX-KSN)
                                   TO  IDX-GKSN
      *
               IF    ( IDX-GKSN    >   ZERO )
                   MOVE    SPA-GMN-W20-KSN-LBL    (IDX-KSN)
                                   TO  W20-LBLKSN (IDX-GKSN)
                   MOVE    SPA-GMN-W20-KSNCD-G    (IDX-KSN)
                                   TO  W20-KSN (IDX-GKSN)
                   MOVE    SPA-GMN-W20-KSNLST-MAX (IDX-KSN)
                                   TO  W20-KSN-CNT(IDX-GKSN)
                                       WRK-MAX
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-MAX )
                       MOVE   SPA-GMN-W20-KSNLST (IDX-KSN IDX1)
                                   TO  W20-KSN-ITM (IDX-GKSN IDX1)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
      *    選択番号
           MOVE    SPA-GMN-W20-SELNUM      TO  W20-SELNUM
      *
      *    病棟リスト
           MOVE    SPA-GMN-W20-BTULST-MAX  TO  W20-BTULST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   W20-BTULST-CNT )
      *
      *        病棟番号
               MOVE    SPA-GMN-W20-TBTUNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W20-TBTUNUM (IDX1)
      *
      *        病棟名称
               MOVE    SPA-GMN-W20-TNAME (IDX1)
                                       TO  W20-TNAME (IDX1)
      *
      *        病棟種別
               MOVE    SPA-GMN-W20-TSBT (IDX1)
                                       TO  W20-TSBT (IDX1)
      *
      *        特定入院料
               MOVE    SPA-GMN-W20-TTOKNYUIN (IDX1)
                                       TO  W20-TTOKUTEINYUIN (IDX1)
      *
      *        病棟科名
               MOVE    SPA-GMN-W20-TSRYKA (IDX1)
                                       TO  W20-TSRYKA (IDX1)
      *
      *        入院基本料
               MOVE    SPA-GMN-W20-TKHNRYO (IDX1)
                                       TO  W20-TKHNRYO (IDX1)
      *
      *        入院基本点数
               MOVE    SPA-GMN-W20-TKHNTEN (IDX1)
                                       TO  W20-TKHNTEN (IDX1)
      *
      *        収容人数
               MOVE    SPA-GMN-W20-TNINZU (IDX1)
                                       TO  WRK-ZZZZZZZZZ9
               MOVE    WRK-ZZZZZZZZZ9
                                       TO  W20-TNINZU (IDX1)
      *
      *        有効期間開始日
               MOVE    SPA-GMN-W20-TYUKOSTYMD (IDX1)
                                       TO  W20-TYUKOSTYMD (IDX1)
      *
      *        有効期間終了日
               MOVE    SPA-GMN-W20-TYUKOEDYMD (IDX1)
                                       TO  W20-TYUKOEDYMD (IDX1)
      *
               IF    ( SPA-GMN-W20-BTULST-SEL  =   IDX1 )
                   MOVE    "T"         TO  W20-BTULST-SEL (IDX1)
               ELSE
                   MOVE    "F"         TO  W20-BTULST-SEL (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    履歴リスト
           MOVE    SPA-GMN-W20-RIREKILST-MAX
                                       TO  W20-RIREKILST-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W20-RIREKILST-CNT )
      *
               MOVE    SPA-GMN-W20-TRSELNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W20-TSELNUM (IDX1)
      *
               STRING  SPA-GMN-W20-TRYUKOSTYMD (IDX1)
                       " 〜 "
                       SPA-GMN-W20-TRYUKOEDYMD (IDX1)
                       DELIMITED   BY  SIZE
               INTO    W20-TRIREKI (IDX1)
               END-STRING
      *
               IF    ( IDX1    =   SPA-GMN-W20-RIREKILST-SEL )
                   MOVE    "T"         TO  W20-RIREKILST-SEL (IDX1)
               ELSE
                   MOVE    "F"         TO  W20-RIREKILST-SEL (IDX1)
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    FLG-BTULST-ROW
           WHEN    1
               MOVE    ZERO               TO  W20-BTULST-ROW
               MOVE    ZERO               TO  W20-BTULST-ROWATTR
           WHEN    2
               MOVE    W20-BTULST-CNT     TO  W20-BTULST-ROW
               MOVE    1                  TO  W20-BTULST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-BTULST-ROW     TO  W20-BTULST-ROW
               MOVE    ZERO               TO  W20-BTULST-ROWATTR
           END-EVALUATE
      *
      *    項目状態編集
           PERFORM 500-EDIT-GMNHEN-STATE-SEC
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
       500-EDIT-GMNHEN-STATE-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  WRK-STATE
      *
           MOVE    WRK-STATE       TO  W20-B02-STATE
                                       W20-B05-STATE
                                       W20-B06-STATE
                                       W20-B07-STATE
                                       W20-B08-STATE
                                       W20-B09-STATE
                                       W20-B10-STATE
                                       W20-B12-STATE
                                       W20-NAME-STATE
                                       W20-TANNAME-STATE
                                       W20-TOKUTEINYUIN-STATE
                                       W20-TOKUTEINYUIN-ITM-STATE
                                       W20-SRYKA-STATE
                                       W20-SRYKA-ITM-STATE
                                       W20-SBT-STATE
                                       W20-SBT-ITM-STATE
                                       W20-KNGHJO-STATE
                                       W20-KNGHJO-ITM-STATE
                                       W20-RECEDSPKBN-STATE
                                       W20-RECEDSPKBN-ITM-STATE
                                       W20-YUKOSTYMD-STATE
                                       W20-YUKOEDYMD-STATE
                                       W20-EDKBN-STATE
                                       W20-EDKBN-ITM-STATE
                                       W20-KHNRYO-STATE
                                       W20-KHNRYO-ITM-STATE
                                       W20-KHNTEN-STATE
      *
           IF   ( SPA-NAI-W20-HOSPSBT  =    1 )
               CONTINUE
           ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-KNGHJO-STATE
                                           W20-KNGHJO-ITM-STATE
           END-IF
      *
      *    IF    ( SPA-GMN-W20-KHNRYO-MAX  =   ZERO )
      *        MOVE    WIDGET-INSENSITIVE
      *                                TO  W20-KHNRYO-STATE
      *                                    W20-KHNRYO-ITM-STATE
      *    END-IF
      *
           PERFORM VARYING IDX-GKSN FROM    1   BY  1
                   UNTIL ( IDX-GKSN >  CONST-GKSN-MAX )
               IF    ( W20-KSN-CNT (IDX-GKSN)   >   ZERO )
                   MOVE    WIDGET-NORMAL   
                                           TO  W20-KSN-STATE (IDX-GKSN)
                                               W20-KSN-ITM-STATE
                                                             (IDX-GKSN)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  W20-KSN-STATE (IDX-GKSN)
                                               W20-KSN-ITM-STATE
                                                             (IDX-GKSN)
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-W20-BTULST-MAX >=   CONST-BTULST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W20-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-W20-YKNKNG-IDX  >   ZERO )
               MOVE    SPA-NAI-W20-YKNKNG-IDX
                                       TO  IDX-GKSN
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
                   IF    ( SPA-GMN-W20-SBT NOT =   "04" )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  W20-KSN-STATE    (IDX-GKSN)
                                           W20-KSN-ITM-STATE(IDX-GKSN)
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180701 )
                   MOVE    WIDGET-INSENSITIVE
                                       TO  W20-KSN-STATE    (IDX-GKSN)
                                           W20-KSN-ITM-STATE(IDX-GKSN)
               END-IF
           END-IF
      *
           .
       500-EDIT-GMNHEN-STATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-W20-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W20-CUR
           WHEN    001
               MOVE    "BTUNUM"            TO  MCP-WIDGET
           WHEN    002
               MOVE    "NAME"              TO  MCP-WIDGET
           WHEN    003
               MOVE    "TANNAME"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "TOKUTEINYUIN"      TO  MCP-WIDGET
           WHEN    005
               MOVE    "SRYKA"             TO  MCP-WIDGET
           WHEN    006
               MOVE    "SBT"               TO  MCP-WIDGET
           WHEN    007
               MOVE    "KHNRYO"            TO  MCP-WIDGET
           WHEN    008
               MOVE    "RECEDSPKBN"        TO  MCP-WIDGET
           WHEN    009
               MOVE    "KNGHJO"            TO  MCP-WIDGET
           WHEN    010
               MOVE    "KSN01"             TO  MCP-WIDGET
           WHEN    011
               MOVE    "KSN02"             TO  MCP-WIDGET
           WHEN    012
               MOVE    "KSN03"             TO  MCP-WIDGET
           WHEN    013
               MOVE    "KSN04"             TO  MCP-WIDGET
           WHEN    014
               MOVE    "KSN05"             TO  MCP-WIDGET
           WHEN    015
               MOVE    "KSN06"             TO  MCP-WIDGET
           WHEN    016
               MOVE    "KSN07"             TO  MCP-WIDGET
           WHEN    017
               MOVE    "KSN08"             TO  MCP-WIDGET
           WHEN    018
               MOVE    "KSN09"             TO  MCP-WIDGET
           WHEN    019
               MOVE    "KSN10"             TO  MCP-WIDGET
           WHEN    020
               MOVE    "KSN11"             TO  MCP-WIDGET
           WHEN    021
               MOVE    "KSN12"             TO  MCP-WIDGET
           WHEN    022
               MOVE    "KSN13"             TO  MCP-WIDGET
           WHEN    023
               MOVE    "KSN14"             TO  MCP-WIDGET
           WHEN    024
               MOVE    "KSN15"             TO  MCP-WIDGET
           WHEN    025
               MOVE    "KSN16"             TO  MCP-WIDGET
           WHEN    051
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    052
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    053
               MOVE    "EDKBN"             TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM"            TO  MCP-WIDGET
           WHEN    101
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    207
               MOVE    "B07"               TO  MCP-WIDGET
           WHEN    208
               MOVE    "B08"               TO  MCP-WIDGET
           WHEN    209
               MOVE    "B09"               TO  MCP-WIDGET
           WHEN    210
               MOVE    "B10"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-W20-CUR     =   9 )
               IF    ( W20-KNGHJO-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    10          TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W20-CUR     =   7 )
               IF    ( W20-KHNRYO-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    8           TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-GKSN    FROM    1   BY  1
                   UNTIL ( IDX-GKSN    >   CONST-GKSN-MAX )
      *
               IF    ( SPA-GMN-W20-CUR =   IDX-GKSN + 9 )
      *
                   IF    ( W20-KSN-STATE (IDX-GKSN)
                                       =   WIDGET-INSENSITIVE )
                       IF    ( IDX-GKSN    =   CONST-GKSN-MAX )
                           COMPUTE SPA-GMN-W20-CUR
                               =   51
                       ELSE
                           COMPUTE SPA-GMN-W20-CUR
                               =   IDX-GKSN
                               +   10
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-W20-CUR     =   206 )
               IF    ( W20-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    207         TO  SPA-GMN-W20-CUR
               END-IF
           END-IF
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
           WHEN    "BTUNUM"
               MOVE    002             TO  SPA-GMN-W20-CUR
           WHEN    "NAME"
               MOVE    003             TO  SPA-GMN-W20-CUR
           WHEN    "TANNAME"
               MOVE    004             TO  SPA-GMN-W20-CUR
           WHEN    "TOKUTEINYUIN"
               MOVE    005             TO  SPA-GMN-W20-CUR
           WHEN    "SRYKA"
               MOVE    006             TO  SPA-GMN-W20-CUR
           WHEN    "SBT"
               MOVE    007             TO  SPA-GMN-W20-CUR
           WHEN    "KHNRYO"
               MOVE    008             TO  SPA-GMN-W20-CUR
           WHEN    "RECEDSPKBN"
               MOVE    009             TO  SPA-GMN-W20-CUR
           WHEN    "KNGHJO"
               MOVE    010             TO  SPA-GMN-W20-CUR
           WHEN    "KSN01"
               MOVE    011             TO  SPA-GMN-W20-CUR
           WHEN    "KSN02"
               MOVE    012             TO  SPA-GMN-W20-CUR
           WHEN    "KSN03"
               MOVE    013             TO  SPA-GMN-W20-CUR
           WHEN    "KSN04"
               MOVE    014             TO  SPA-GMN-W20-CUR
           WHEN    "KSN05"
               MOVE    015             TO  SPA-GMN-W20-CUR
           WHEN    "KSN06"
               MOVE    016             TO  SPA-GMN-W20-CUR
           WHEN    "KSN07"
               MOVE    017             TO  SPA-GMN-W20-CUR
           WHEN    "KSN08"
               MOVE    018             TO  SPA-GMN-W20-CUR
           WHEN    "KSN09"
               MOVE    019             TO  SPA-GMN-W20-CUR
           WHEN    "KSN10"
               MOVE    020             TO  SPA-GMN-W20-CUR
           WHEN    "KSN11"
               MOVE    021             TO  SPA-GMN-W20-CUR
           WHEN    "KSN12"
               MOVE    022             TO  SPA-GMN-W20-CUR
           WHEN    "KSN13"
               MOVE    023             TO  SPA-GMN-W20-CUR
           WHEN    "KSN14"
               MOVE    024             TO  SPA-GMN-W20-CUR
           WHEN    "KSN15"
               MOVE    025             TO  SPA-GMN-W20-CUR
           WHEN    "KSN16"
               MOVE    051             TO  SPA-GMN-W20-CUR
           WHEN    "YUKOSTYMD"
               MOVE    052             TO  SPA-GMN-W20-CUR
           WHEN    "YUKOEDYMD"
               MOVE    053             TO  SPA-GMN-W20-CUR
           WHEN    "EDKBN"
               MOVE    206             TO  SPA-GMN-W20-CUR
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-W20-CUR
           WHEN    "KJNYMD"
               MOVE    001             TO  SPA-GMN-W20-CUR
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
               MOVE    "全角文字でのみ入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "有効期間開始日は平成１４年４月１日以降にして"
                       "ください"
                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0004"
                   MOVE    "特定入院料入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
                   MOVE    "診療科入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
                   MOVE    "病棟種別入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
                   MOVE    "レセプト表示区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
                   MOVE    "有効開始日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
                   MOVE    "有効終了日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
                   MOVE    "入院基本料入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
                   MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "追加可能最大件数に達しました"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "変更対象の病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "削除対象の病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "病棟の有効期間内に病室管理情報が存在するため"
                       "削除できません"
                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1007"
               MOVE    "病棟名称を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "短縮病棟名を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "病棟種別を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    1               TO  WRK-PNT
               IF    ( WRK-KSNMEI      =   SPACE )
                   STRING  "加算に必要な施設基準が設定されていません"
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               ELSE
                   STRING  "必要な施設基準が設定されていません／"
                           DELIMITED   BY  SPACE
                           WRK-KSNMEI
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-IF
      *
               PERFORM VARYING IDX-SST FROM    1   BY  1
                       UNTIL ( IDX-SST >   CNT-SST )
      *
                   STRING  WRK-SSTKJNCD-OCC  (IDX-SST)
                           DELIMITED   BY  SIZE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-PERFORM
           WHEN    "1011"
               STRING  "入院料に対して"
                       WRK-KSNMEI      DELIMITED   BY  SPACE
                       "は算定できません"
                                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1014"
               MOVE    "入院基本料が算定されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1101"
               MOVE     "病棟番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               MOVE    "有効期間の重複するデータが存在します"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "有効期間の開始日と終了日は同時に変更できません"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "病院の病棟種別を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "診療所の病棟種別を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1108"
               MOVE    "登録済みの病棟が選択されていません"
                                       TO  SPA-ERRMSG
           WHEN    "1109"
               MOVE   "医療観察病棟では入院料加算の追加設定はできません"
                                       TO  SPA-ERRMSG
           WHEN    "1110"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "病棟の有効開始日が変更されています。"
                                           DELIMITED   BY  SIZE
                       "先に変更または追加処理を行ってください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1201"
           WHEN    "1202"
           WHEN    "1203"
           WHEN    "1204"
           WHEN    "1205"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W20"               TO  SPA-MOTOPG
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
           WHEN    "2001"
               MOVE    "該当の病棟を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "2002"
               MOVE    SPACE            TO  WRK-WIDMSG
               STRING  "有効期間の重複するデータが存在します。"
                                            DELIMITED   BY  SIZE
                       "前の期間を区切りますか"
                                            DELIMITED   BY  SIZE
               INTO    WRK-WIDMSG
               END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W20"                TO  SPA-MOTOPG
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
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
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
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
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
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC             SECTION.
      *
           INITIALIZE                  WRK-KANACHK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               ELSE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-EXT.
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
           EVALUATE    TRUE
           WHEN  ( WRK-YMD (1:3)   =   "000" )
               MOVE    "00000000"  TO  WRK-HENYMDG
                                       WRK-SYMD
           WHEN  ( WRK-YMD (1:3)   =   "999" )
               MOVE    "99999999"  TO  WRK-HENYMDG
                                       WRK-SYMD
           WHEN    OTHER
      *
      *        日付画面チェックサブ
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKE-ERR
               END-IF
           END-EVALUATE
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
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
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
      *    システム管理検索処理(KEY)
      *****************************************************************
       900-SYSKANRI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY)
      *****************************************************************
       900-SYSKANRI-KEY-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY7)
      *****************************************************************
       900-SYSKANRI-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY4)
      *****************************************************************
       900-SYSKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5002-KBNCD
           MOVE    "%"                 TO  SYS-5002-KBNCD (3:1)
           MOVE    SPA-HOSPNUM         TO  SYS-5002-HOSPNUM
           MOVE    SYS-5002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
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
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5002-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    SPA-GMN-W20-BTUNUM  TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                       TO  SYS-5001-STYUKYMD
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5001-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5001-REC
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
      *    入院料加算チェックテーブル検索処理(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-NYUINKBN        TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSNKBN          TO  NYUKSNCHK-KSNKBN
           MOVE    WRK-YUKOSTYMD       TO  NYUKSNCHK-YUKOSTYMD
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
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-SEL-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           IF    ( SPA-NAI-W20-YUKOSTYMD  >=   CONST-H180401 )
               PERFORM 9002-EDIT-TENSU-PATH-SEC
           ELSE
               PERFORM 9001-EDIT-TENSU-PATH-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索パス編集処理（平成１７年度まで）
      *****************************************************************
       9001-EDIT-TENSU-PATH-SEC         SECTION.
      *
           IF    ( SPA-GMN-W20-SBT =   "07" )
               MOVE    "-"         TO  TNS-CDKBN-KBN
           ELSE
               MOVE    "A"         TO  TNS-CDKBN-KBN
           END-IF
      *
      *    区分番号設定処理
           PERFORM 9001-CDKBNNUM-SET-SEC
           MOVE    WRK-CDKBNNUM    TO  TNS-CDKBN-KBNNUM
           MOVE    WRK-CDKOUBAN    TO  TNS-CDKBN-KOUBAN
           MOVE    WRK-TK-CDKBNNUM TO  TNSXX-TK-CDKBN-KBNNUM
           MOVE    WRK-TK-CDKOUBAN TO  TNSXX-TK-CDKBN-KOUBAN
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
      *    点数マスタ検索
           EVALUATE    SPA-GMN-W20-SBT
           WHEN    "01"
           WHEN    "02"
           WHEN    "03"
               MOVE    "key15"     TO  WRK-PATHNAME
           WHEN    "07"
               MOVE    "key14"     TO  WRK-PATHNAME
           WHEN    OTHER
               MOVE    "key13"     TO  WRK-PATHNAME
           END-EVALUATE
      *
           .
       9001-EDIT-TENSU-PATH-EXT.
           EXIT.
      *****************************************************************
      *    区分番号設定処理（平成１７年度まで）
      *****************************************************************
       9001-CDKBNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-CDKBNNUM
                                       WRK-CDKOUBAN
                                       WRK-TK-CDKBNNUM
                                       WRK-TK-CDKOUBAN
      *
           EVALUATE    SPA-GMN-W20-SBT
      *    一般病棟
           WHEN    "01"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    1               TO  WRK-TK-CDKOUBAN
               MOVE    100             TO  WRK-CDKBNNUM
      *    精神病棟
           WHEN    "02"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    3               TO  WRK-TK-CDKOUBAN
               MOVE    103             TO  WRK-CDKBNNUM
      *    結核病棟
           WHEN    "03"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    2               TO  WRK-TK-CDKOUBAN
               MOVE    102             TO  WRK-CDKBNNUM
      *    療養病棟
           WHEN    "04"
               MOVE    101             TO  WRK-CDKBNNUM
      *    障害者施設
           WHEN    "05"
               MOVE    106             TO  WRK-CDKBNNUM
      *    専門病院
           WHEN    "06"
               MOVE    105             TO  WRK-CDKBNNUM
      *    老人病棟
           WHEN    "07"
               MOVE    ZERO            TO  WRK-CDKBNNUM
               MOVE    8               TO  WRK-CDKOUBAN
      *    有床診療所入院基本料
           WHEN    "08"
               MOVE    108             TO  WRK-CDKBNNUM
      *    有床診療所療養病床入院料
           WHEN    "09"
               MOVE    109             TO  WRK-CDKBNNUM
           END-EVALUATE
      *
           .
      *
       9001-CDKBNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索パス編集処理（平成１８年度以降）
      *****************************************************************
       9002-EDIT-TENSU-PATH-SEC         SECTION.
      *
           MOVE    "A"             TO  TNS-CDKBN-KBN
      *
      *    区分番号設定処理
           PERFORM 9002-CDKBNNUM-SET-SEC
           MOVE    WRK-CDKBNNUM    TO  TNS-CDKBN-KBNNUM
           MOVE    WRK-CDKOUBAN    TO  TNS-CDKBN-KOUBAN
           MOVE    WRK-TK-CDKBNNUM TO  TNSXX-TK-CDKBN-KBNNUM
           MOVE    WRK-TK-CDKOUBAN TO  TNSXX-TK-CDKBN-KOUBAN
      *
           MOVE    SPA-NAI-W20-YUKOSTYMD
                                   TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
      *
      *    点数マスタ検索
           MOVE    "key25"         TO  WRK-PATHNAME
      *
           .
       9002-EDIT-TENSU-PATH-EXT.
           EXIT.
      *****************************************************************
      *    区分番号設定処理（平成１８年度以降）
      *****************************************************************
       9002-CDKBNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-CDKBNNUM
                                       WRK-CDKOUBAN
                                       WRK-TK-CDKBNNUM
                                       WRK-TK-CDKOUBAN
      *
           EVALUATE    SPA-GMN-W20-SBT
      *    一般病棟
           WHEN    "01"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    1               TO  WRK-TK-CDKOUBAN
               MOVE    100             TO  WRK-CDKBNNUM
      *    精神病棟
           WHEN    "02"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    3               TO  WRK-TK-CDKOUBAN
               MOVE    103             TO  WRK-CDKBNNUM
      *    結核病棟
           WHEN    "03"
               MOVE    104             TO  WRK-TK-CDKBNNUM
               MOVE    2               TO  WRK-TK-CDKOUBAN
               MOVE    102             TO  WRK-CDKBNNUM
      *    療養病棟
           WHEN    "04"
               MOVE    101             TO  WRK-CDKBNNUM
      *    障害者施設
           WHEN    "05"
               MOVE    106             TO  WRK-CDKBNNUM
      *    専門病院
           WHEN    "06"
               MOVE    105             TO  WRK-CDKBNNUM
      *    有床診療所療養病床入院料
           WHEN    "09"
               MOVE    109             TO  WRK-CDKBNNUM
           END-EVALUATE
      *
           .
      *
       9002-CDKBNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタFETCH処理
      *****************************************************************
       900-TENSU-FET-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           .
      *
       900-TENSU-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

