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
       PROGRAM-ID.             ORCGY01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約状況入力（Ｙ０１）
      *  管理者            : 
      *  00/12/01    MCC−   　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.00.02    NACL-多々納  02/08/02  画面のＳＰＡを保存領域へ編集
      *  01.00.03    NACL-多々納  02/11/20  GO TO 先修正他
      *  01.00.04    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.05    NACL-多々納  03/12/01  一覧表を２００件へ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *
       01  SPA-Y01.
           03  SPA-Y01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(04).
               05  SPA-GMN-CUR         PIC 9(02).
               05  SPA-YYK-MAX         PIC 9(04).
               05  SPA-YYK-SYORI       PIC 9(01).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYNAIYO.
                       09  SPA-GMN-SRYNAIYOCD  PIC X(02).
                       09  SPA-GMN-SK1         PIC X(01).
                       09  SPA-GMN-SRYNAIYOMEI PIC X(18).
                   07  SPA-GMN-SRYNAIYOLST     PIC X(21)
                                               OCCURS  30.
                   07  SPA-SRYNAIYO-MAX        PIC 9(04).
      *
                   07  SPA-GMN-DRNAME.
                       09  SPA-GMN-DRCD        PIC X(04).
                       09  SPA-GMN-D1          PIC X(01).
                       09  SPA-GMN-DRNAMEMEI   PIC X(24).
      *
                   07  SPA-GMN-DRCDLST-G.
                       09  SPA-GMN-DRCDLST    PIC X(29)
                                           OCCURS  50.
                   07  SPA-DRCD-MAX        PIC 9(04).
      *
      *        予約内容
               05  SPA-GMN-YYKAREA.
                   07  SPA-GMN-PTNUM       PIC X(20).
                   07  SPA-GMN-PTID        PIC 9(10).
                   07  SPA-GMN-NAME        PIC X(50).
                   07  SPA-GMN-SEX         PIC X(02).
                   07  SPA-GMN-BIRTHDAYH   PIC X(09).
                   07  SPA-GMN-YYKNAME     PIC X(50).
      *
                   07  SPA-GMN-YYKYMD      PIC X(10).
                   07  SPA-GMN-YYKTIME     PIC X(05).
      *            診療科
                   07  SPA-GMN-SRYKA-G.
                       09  SPA-GMN-SRYKA       PIC X(02).
                       09  SPA-GMN-SRYKAH1     PIC X(01).
                       09  SPA-GMN-SRYKAMEI    PIC X(12).
      *            予約内容
                   07  SPA-GMN-YYKKBN-G.
                       09  SPA-GMN-YYKKBN       PIC X(02).
                       09  SPA-GMN-YYKKBNH1     PIC X(01).
                       09  SPA-GMN-YYKKBNMEI    PIC X(24).
      *            確認連絡
                   07  SPA-GMN-KAKREN-G.
                       09  SPA-GMN-KAKREN       PIC X(02).
                       09  SPA-GMN-KAKRENH1     PIC X(01).
                       09  SPA-GMN-KAKRENMEI    PIC X(20).
      *
                   07  SPA-NAI-YYKYMD          PIC X(08).
                   07  SPA-NAI-YYKYMD-R        REDEFINES
                                               SPA-NAI-YYKYMD.
                       09  SPA-NAI-YYKYY       PIC 9(04).
                       09  SPA-NAI-YYKMM       PIC 9(02).
                       09  SPA-NAI-YYKDD       PIC 9(02).
                   07  SPA-NAI-YYKID           PIC 9(02).
                   07  SPA-NAI-KYYKTIME        PIC 9(04).
                   07  SPA-NAI-YYKTIME         PIC 9(04).
                   07  SPA-OLD-YYKYMD          PIC X(08).
      *
      *
      *        表示リスト等
               05  SPA-GMN-HYOU-AERA.
                   07  SPA-GMN-SRYKALST    PIC X(17)   OCCURS  40.
                   07  SPA-SRYKA-MAX       PIC 9(04).
      *
                   07  SPA-GMN-YYKKBNLST    PIC X(29)   OCCURS  10.
                   07  SPA-YYKKBN-MAX       PIC 9(04).
      *
                   07  SPA-GMN-KAKRENLST    PIC X(23)   OCCURS  10.
                   07  SPA-KAKREN-MAX       PIC 9(04).
      *
                   07  SPA-GMN-HYOJHI      PIC X(02).
                   07  SPA-GMN-TAIYYMM     PIC X(12).
                   07  SPA-GMN-TAIYMD      PIC X(16).
                   07  SPA-GMN-TAITIME     PIC X(06).
      *
                   07  SPA-GMN-GOKKEN-G.
                       09  SPA-GMN-GOKKEN      PIC X(05)  OCCURS  7.
                   07  SPA-GMN-KENSUU-G.
                       09  SPA-GMN-KENSUU-T    OCCURS   98.
                           11  SPA-GMN-KENSUU      PIC  9(03).
                   07  SPA-GMN-KENSUU-GR       REDEFINES
                                               SPA-GMN-KENSUU-G.
                       09  SPA-GMN-KENSUU-TR   OCCURS   14.
                           11  SPA-GMN-YYKCNT     PIC  9(03)
                                                   OCCURS  7.
      *            受付患者一覧
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO        OCCURS  200.
                           11  SPA-GMN-TBANGO      PIC 9(03).
                           11  SPA-GMN-TYYKTIME    PIC X(04).
                           11  SPA-GMN-TNAME       PIC X(20).
                           11  SPA-GMN-TKAMEI      PIC X(04).
                           11  SPA-GMN-TRAIINZUMI  PIC X(04).
      *
                   07  SPA-GMN-SELBAN              PIC X(03).
      *           カレンダー領域
                   07  SPA-GMN-CALENDR.
                       09  SPA-GMN-YEAR                PIC 9(04).
                       09  SPA-GMN-MONTH               PIC 9(02).
                       09  SPA-GMN-DAY                 PIC 9(02).
                   07  SPA-GMN-HYOTBL-G.
                        09  SPA-GMN-HYODD-T     OCCURS   42.
                           11  SPA-GMN-HYODD       PIC  9(02).
                   07  SPA-GMN-HYOTBL-GR       REDEFINES
                                               SPA-GMN-HYOTBL-G.
                       09  SPA-GMN-HYODD-TR    OCCURS   6.
                           11  SPA-GMN-HYODDG      PIC  9(02)
                                                   OCCURS  7.
      *
               05  SPA-MAE-AREA.
                   07  SPA-MAE-SRYNAIYOCD      PIC X(02).
                   07  SPA-MAE-DRCD            PIC X(04).
                   07  SPA-MAE-PTNUM           PIC X(20).
                   07  SPA-MAE-YYKNAME         PIC X(50).
                   07  SPA-MAE-YYKYMD          PIC X(10).
      *
                   07  SPA-CHK-YYKYMD          PIC X(08).
                   07  SPA-CHK-KYYKTIME        PIC 9(04).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-SELBAN              PIC 9(03).
                   07  SPA-NAI-HYOJHI              PIC 9(02).
      *
      *           カレンダー領域
                   07  SPA-NAI-HYOTBL-G.
                       09  SPA-NAI-HYODD-T     OCCURS   42.
                           11  SPA-NAI-YMD         PIC  X(08).
                   07  SPA-NAI-HYOTBL-GR       REDEFINES
                                               SPA-NAI-HYOTBL-G.
                       09  SPA-NAI-HYODD-TR    OCCURS   6.
                           11  SPA-NAI-YMDR        PIC  X(08)
                                                   OCCURS  7.
      *            全予約内容テーブル
                   07  SPA-NAI-YOYAKU-G.
                       09  SPA-NAI-YOYAKU-T    OCCURS   14.
                           11  SPA-NAI-YOYAKU-T2   OCCURS   7.
                               13  SPA-NAI-YYYKYMD      PIC X(08).
                               13  SPA-NAI-YYYKTIME     PIC 9(04).
                               13  SPA-NAI-YYYKMAXCNT   PIC 9(03).
      *            受付患者一覧
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO        OCCURS   200.
                           11  SPA-NAI-TPTID       PIC X(10).
                           11  SPA-NAI-TPTNUM      PIC X(20).
                           11  SPA-NAI-TYYKYMD     PIC X(08).
                           11  SPA-NAI-TKYYKTIME   PIC 9(04).
                           11  SPA-NAI-TYYKID      PIC 9(02).
                           11  SPA-NAI-TSRYKA      PIC X(02).
                           11  SPA-NAI-TYYKKBN     PIC X(02).
                           11  SPA-NAI-TKAKREN     PIC X(02).
      *            対象の週・曜日
                   07  SPA-SEL-AREA.
                       09  SPA-SEL-SYUU            PIC 9(02).
                       09  SPA-SEL-YOUBI           PIC 9(01).
                       09  SPA-SEL-TIME            PIC 9(02).
      *            確定対象
                   07  SPA-KAKSEL-AREA.
                       09  SPA-SEL-YYKYMD          PIC X(08).
                       09  SPA-SEL-YYKTIME         PIC 9(04).
                       09  SPA-SEL-YYKID           PIC 9(02).
      *            ドクターコード（５桁）
                   07  SPA-NAI-DRCD.
                       09  SPA-NAI-DRCD1          PIC X(01).
                       09  SPA-NAI-DRCD2          PIC X(04).
      *
      *            ドクターの専門家
                   07  SPA-NAI-DRCD-AREA.
                       09  SPA-NAI-DR-SEMON        OCCURS  50.
                           11  SPA-NAI-DR-SEMONKACD   PIC X(02)
                                                      OCCURS  5.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-HENFLG          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
           03  WRK-WSYSYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-GMN-YYKTIME     PIC X(05).
      *
           03  WRK-HI              PIC 9(02).
           03  WRK-KEI             PIC 9(03).
           03  WRK-SEL-SYUU        PIC 9(02).
           03  WRK-KEY-YYKTIME     PIC 9(04).
      *
           03  WRK-SPA-Y01-PTNUM   PIC X(20).
           03  WRK-SPA-Y01-PTID    PIC X(10).
           03  WRK-SPA-Y01-NAME    PIC X(50).
           03  WRK-GMN-GOKSEL      PIC 9(01).
           03  WRK-GMN-KENSEL      PIC 9(02).
           03  WRK-MOJISU          PIC 9(04).
     *
           03  WRK-TIME-X.
               05  WRK-THHX        PIC X(02).
               05  WRK-TSSX        PIC X(02).
           03  WRK-TIME.
               05  WRK-THH         PIC 9(02).
               05  WRK-TSS         PIC 9(02).
           03  FLG-TIME            PIC 9(01).
     *
           03  WRK-DRCD           PIC X(04).
      *
           03  WRK-HKENSUU.
      *        05  FILLER          PIC X(01).
               05  WRK-KENSUU      PIC Z,ZZZ.
      *        05  FILLER          PIC X(01).
      *
           03  WRK-KNJ.
               05  WRK-PTNUM       PIC X(20).
               05  WRK-PTID        PIC X(10).
               05  WRK-NAME        PIC X(30).
               05  WRK-SEX         PIC X(02).
               05  WRK-BIRTHDAYH   PIC X(09).
      *
           03  WRK-YYKID           PIC 9(02).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-NAI-DR-SEMON.
               05  WRK-DR-SEMONKACD   PIC X(02)
                                              OCCURS  5.
      *
      *    コラムリスト位置
           03  WRK-Y01-ROW             PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *  ＳＯＲＴ用ワーク
      *    受付患者一覧
       01  WRK-SORT-AREA.
           03  WRK-YYKTIME             PIC X(04).
           03  WRK-IDX                 PIC 9(04).
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO        OCCURS   200.
                   07  WRK-GMN-TBANGO      PIC 9(03).
                   07  WRK-GMN-TYYKTIME    PIC X(04).
                   07  WRK-GMN-TNAME       PIC X(20).
                   07  WRK-GMN-TKAMEI      PIC X(04).
                   07  WRK-GMN-TRAIINZUMI  PIC X(04).
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO        OCCURS   200.
                   07  WRK-NAI-TPTID       PIC X(10).
                   07  WRK-NAI-TPTNUM      PIC X(20).
                   07  WRK-NAI-TYYKYMD     PIC X(08).
                   07  WRK-NAI-TKYYKTIME   PIC 9(04).
                   07  WRK-NAI-TYYKID      PIC 9(02).
                   07  WRK-NAI-TSRYKA      PIC X(02).
                   07  WRK-NAI-TYYKKBN     PIC X(02).
                   07  WRK-NAI-TKAKREN     PIC X(02).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
       01  WRK-GMN-KENSUU.
           03  WRK-TBL-KENSUU      PIC X(05)   OCCURS 98.
      *
       01  WRK-CAL-YMD.
           03  WRK-CAL-YEAR        PIC 9(04).
           03  WRK-CAL-MONTH       PIC 9(02).
           03  WRK-CAL-DAY         PIC 9(02).
       01  WRK-GMN-TAIYYMM         PIC X(12).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療内容情報
           COPY  "CPSK1012.INC".
      *    診療科
           COPY  "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    予約確認連絡コード情報
           COPY  "CPSK0051.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
           COPY  "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "Y01.INC".
      *****COPY    "Y02.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "YERR.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Y01
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO MCP-EVENT ALSO MCP-WIDGET(1:1)
               WHEN    "LINK"      ALSO ANY       ALSO ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO "CLICKED" ALSO "B"
                   PERFORM 210-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
      *
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
      *****MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "Y01"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-Y01-AREA
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
      *
           IF      (SPA-MOTOPG(1:1)    =   "Y"  )  OR
                   (SPA-MOTOPG         =   "U01" OR  "P97")
               MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
           ELSE
               INITIALIZE                  SPA-Y01KYOUTU
      ******** IF      SPA-MOTOPG         =   "I41"
      *            MOVE    SPA-PTID            TO  SPA-Y01-PTID
      *            MOVE    SPA-PTNUM           TO  SPA-Y01-PTNUM
      *            MOVE    SPA-SEX             TO  SPA-Y01-SEX
      *            MOVE    SPA-BIRTHDAY        TO  SPA-Y01-BIRTHDAY
      *********END-IF
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           EVALUATE    SPA-MOTOPG(1:1)
               WHEN    "Y"
                   MOVE    SPACE               TO  SPA-GMN-PTNUM
                   MOVE    ZERO                TO  SPA-GMN-PTID
                   MOVE    SPACE               TO  SPA-GMN-NAME
                   MOVE    SPACE               TO  SPA-GMN-SEX
                   MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
                   MOVE    SPACE               TO  SPA-GMN-YYKNAME
               WHEN    OTHER
      *            氏名検索画面より選択後
                   IF     (SPA-MOTOPG(1:3)    =   "P97"  OR  "U01") AND
                          (SPA-PTID       NOT =   ZERO   )
      *****************CONTINUE
      **************** MOVE    SPA-NAME       TO  SPA-GMN-YYKNAME
      *************ELSE
                       MOVE    SPA-PTID       TO  SPA-GMN-PTID
                       MOVE    SPA-PTNUM      TO  SPA-GMN-PTNUM
                       MOVE    SPA-NAME       TO  SPA-GMN-NAME
                       MOVE    SPA-SEX        TO  SPA-GMN-SEX
                       MOVE    SPA-BIRTHDAYW  TO  SPA-GMN-BIRTHDAYH
                       MOVE    SPA-NAME       TO  SPA-GMN-YYKNAME
                   END-IF
      *************IF      SPA-MOTOPG         =   "I41"
      *                MOVE    SPA-PTID       TO  SPA-GMN-PTID
      *                MOVE    SPA-PTNUM      TO  SPA-GMN-PTNUM
      *                MOVE    SPA-NAME       TO  SPA-GMN-NAME
      *                MOVE    SPA-SEX        TO  SPA-GMN-SEX
      *                MOVE    SPA-BIRTHDAYW  TO  SPA-GMN-BIRTHDAYH
      *                MOVE    SPA-NAME       TO  SPA-GMN-YYKNAME
      ************ END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-GMN-SELBAN
           MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
      *    初期表示があるので、患者入力より
           IF     (SPA-GMN-PTNUM       =   SPACE  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE  )
               MOVE    3                   TO  SPA-GMN-CUR
           ELSE
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           IF    (SPA-Y01-YYKYMDS  NUMERIC  )  AND
                 (SPA-Y01-YYKYMDS  NOT =   SPACE )  AND
                 (SPA-Y01-YYKYMDS  NOT =   ZERO  )
               MOVE    SPA-Y01-YYKYMDS
                                   TO  WRK-SYSYMD
           ELSE
               MOVE    SPA-SYSYMD
                                   TO  WRK-SYSYMD
           END-IF
           INITIALIZE              SPA-Y01-AREA
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      *D   MOVE    ZERO                TO  SYS-1012-STYUKYMD
      *D   MOVE    ALL "9"             TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   30   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1012-REC
      *
               MOVE    SYS-1012-KBNCD    TO  SPA-GMN-SRYNAIYOCD
               MOVE    SPACE             TO  SPA-GMN-SK1
               MOVE    SYS-1012-SRYNAIYODSP
                                         TO  SPA-GMN-SRYNAIYOMEI
      *
               MOVE    SPA-GMN-SRYNAIYO  TO  SPA-GMN-SRYNAIYOLST (IDX)
      *
               MOVE    IDX               TO  SPA-SRYNAIYO-MAX
      *
               MOVE    "SYSKANRI-KEY2"   TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SPA-GMN-SRYNAIYOLST (01)
                                       TO  SPA-GMN-SRYNAIYO
      *
      *    職員情報をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      *D   MOVE    ZERO                TO  SYS-1010-STYUKYMD
      *D   MOVE    ALL "9"             TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       (IDX            >=  50   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
                   ADD     1                   TO  IDX
                   MOVE    SYS-1010-KBNCD(2:4)
                                               TO  SPA-GMN-DRCD
                   MOVE    SPACE               TO  SPA-GMN-D1
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEI
                   MOVE    SPA-GMN-DRNAME      TO  SPA-GMN-DRCDLST (IDX)
      *            専門家
                   MOVE    SYS-1010-SEMONKACD (1)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 1)
                   MOVE    SYS-1010-SEMONKACD (2)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 2)
                   MOVE    SYS-1010-SEMONKACD (3)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 3)
                   MOVE    SYS-1010-SEMONKACD (4)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 4)
                   MOVE    SYS-1010-SEMONKACD (5)
                                               TO  SPA-NAI-DR-SEMONKACD
                                                                (IDX 5)
      *
                   MOVE    IDX                 TO  SPA-DRCD-MAX
               END-IF
      *
               MOVE    "SYSKANRI-KEY2"         TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           MOVE    SPA-GMN-DRCDLST (01)
                                       TO  SPA-GMN-DRNAME
           MOVE    "1"                 TO  SPA-NAI-DRCD1
           MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRCD2
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-KEY
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
      *
               MOVE    SYS-1005-KBNCD (1:2)    TO  SPA-GMN-SRYKA
               MOVE    SPACE                   TO  SPA-GMN-SRYKAH1
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-GMN-SRYKAMEI
               MOVE    SPA-GMN-SRYKA-G         TO  SPA-GMN-SRYKALST
                                                              (IDX)
               MOVE    IDX                     TO  SPA-SRYKA-MAX
      *
               MOVE    "SYSKANRI-KEY2"         TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           MOVE    SPA-GMN-SRYKALST (1)    TO  SPA-GMN-SRYKA-G
      *
      *    予約確認連絡コード
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-0051-REC
           INITIALIZE                  SYS-0051-REC
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      *
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-0051-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   10   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-0051-REC
      *
               MOVE    SYS-0051-KBNCD (1:2)    TO  SPA-GMN-KAKREN
               MOVE    SPACE                   TO  SPA-GMN-KAKRENH1
               MOVE    SYS-0051-YYKKAKNAME     TO  SPA-GMN-KAKRENMEI
               MOVE    SPA-GMN-KAKREN-G        TO  SPA-GMN-KAKRENLST
                                                              (IDX)
               MOVE    IDX                     TO  SPA-KAKREN-MAX
      *
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           MOVE    SPA-GMN-KAKRENLST (1)
                                       TO  SPA-GMN-KAKREN-G
      *
           MOVE    "00 　　　　"       TO  SPA-GMN-YYKKBNLST (01)
           MOVE    "01 患者による予約" TO  SPA-GMN-YYKKBNLST (02)
           MOVE    "02 医師による予約" TO  SPA-GMN-YYKKBNLST (03)
           MOVE    SPA-GMN-YYKKBNLST (2)
                                       TO  SPA-GMN-YYKKBN-G
           MOVE    3                   TO  SPA-YYKKBN-MAX
      *
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
              MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
              MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
              MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
              MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
              MOVE "月"                TO  SPA-GMN-TAIYYMM (11:2)
           ELSE
              MOVE SPACE               TO  SPA-GMN-TAIYYMM
           END-IF
      *
      * １日の曜日算出
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           MOVE    "01"                TO  LNK-DAY3-YMD(7:2)
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE   0                   TO  LNK-DAY3-YOBI
           END-IF
      *
           IF      LNK-DAY3-YOBI   NOT =   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   LNK-DAY3-YOBI )
                   MOVE  ZERO          TO  SPA-GMN-HYODD (IDX)
                   MOVE  ZERO          TO  SPA-NAI-YMD (IDX)
               END-PERFORM
           END-IF
      *
      * 月末日の算出
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYSYMD (1:6)    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                            LNK-DAY7-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE WRK-SYSYMD          TO  LNK-DAY7-KOYOMI
              MOVE "31"                TO  LNK-DAY7-KOYOMI (7:2)
           END-IF
      *
           MOVE    IDX                 TO  WRK-IDX
           IF      WRK-IDX             =   ZERO
               MOVE    1               TO  WRK-IDX
           END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    WRK-IDX   BY  1
                   UNTIL      (IDX         >   42  )   OR
                              (WRK-HI      =   LNK-DAY7-KOYOMI (7:2) )
               ADD     1               TO  WRK-HI
               MOVE    WRK-HI          TO  SPA-GMN-HYODD (IDX)
               MOVE    WRK-SYSYMD (1:6)
                                       TO  SPA-NAI-YMD (IDX)(1:6)
               MOVE    WRK-HI          TO  SPA-NAI-YMD (IDX)(7:2)
           END-PERFORM
      *
      *    初期画面では、システム日付を対象とし、表示する
           MOVE    WRK-SYSDD           TO  SPA-NAI-HYOJHI
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           END-IF
      *
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *    予約テーブル編集処理
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       3101-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       3101-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約テーブル編集処理
      *****************************************************************
       510-YYKTBL-SET-SEC           SECTION.
      *
           MOVE    SPA-SEL-SYUU        TO  WRK-SEL-SYUU
           INITIALIZE                  SPA-GMN-GOKKEN-G
           INITIALIZE                  SPA-GMN-KENSUU-G
           INITIALIZE                  SPA-NAI-YOYAKU-G
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-GMN-SELBAN
           INITIALIZE                  SPA-NAI-SELBAN
           INITIALIZE                  SPA-KAKSEL-AREA
           INITIALIZE                  SPA-NAI-TBL
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   7
               MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                       TO  SPA-NAI-YYYKYMD  (01 IDX)
               MOVE    0800            TO  SPA-NAI-YYYKTIME (01 IDX)
               MOVE    0900            TO  SPA-NAI-YYYKTIME (02 IDX)
               MOVE    1000            TO  SPA-NAI-YYYKTIME (03 IDX)
               MOVE    1100            TO  SPA-NAI-YYYKTIME (04 IDX)
               MOVE    1200            TO  SPA-NAI-YYYKTIME (05 IDX)
               MOVE    1300            TO  SPA-NAI-YYYKTIME (06 IDX)
               MOVE    1400            TO  SPA-NAI-YYYKTIME (07 IDX)
               MOVE    1500            TO  SPA-NAI-YYYKTIME (08 IDX)
               MOVE    1600            TO  SPA-NAI-YYYKTIME (09 IDX)
               MOVE    1700            TO  SPA-NAI-YYYKTIME (10 IDX)
               MOVE    1800            TO  SPA-NAI-YYYKTIME (11 IDX)
               MOVE    1900            TO  SPA-NAI-YYYKTIME (12 IDX)
               MOVE    2000            TO  SPA-NAI-YYYKTIME (13 IDX)
               MOVE    2100            TO  SPA-NAI-YYYKTIME (14 IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   7
      *            UNTIL       IDX     >   1
               IF      SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                       NOT =   SPACE  AND  ZERO
      *            対象見出日をカレンダーよりセット
                   MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)(7:2)
                                           TO  SPA-GMN-GOKKEN (IDX)
                                                          (3:2)
      *            時間帯毎の予約を保存する
                   INITIALIZE                  YYK-REC
                   MOVE    SPA-HOSPID          TO  YYK-HOSPID
                   MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
                   MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
                   MOVE    SPA-NAI-YMDR (SPA-SEL-SYUU IDX)
                                               TO  YYK-YYKYMD
      *
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "YYK-KEY3"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    "YYK-KEY3"          TO  ORC-DBPATH
                       PERFORM 900-YOYAKU-READ-SEC
                   ELSE
                       MOVE    1               TO  FLG-YYK
                   END-IF
      * 
                   PERFORM UNTIL       FLG-YYK     =   1
                       PERFORM 5201-YYKHEN-SEC
      *
                       MOVE    "YYK-KEY3"          TO  ORC-DBPATH
                       PERFORM 900-YOYAKU-READ-SEC
                   END-PERFORM
      *    カーソルクロース
                   MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                   MOVE    "YYK-KEY3"          TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
               END-IF
           END-PERFORM
      *
           .
       510-YYKTBL-SET-EXT.
           EXIT.
      *       
      *****************************************************************
      *    時間帯毎の予約編集処理
      *****************************************************************
       5201-YYKHEN-SEC           SECTION.
      *
           EVALUATE    YYK-KEYYYKTIME
               WHEN    0800
               WHEN    0859
                   MOVE    1           TO  IDX-Y
               WHEN    0900
                   MOVE    2           TO  IDX-Y
               WHEN    1000
                   MOVE    3           TO  IDX-Y
               WHEN    1100
                   MOVE    4           TO  IDX-Y
               WHEN    1200
                   MOVE    5           TO  IDX-Y
               WHEN    1300
                   MOVE    6           TO  IDX-Y
               WHEN    1400
                   MOVE    7           TO  IDX-Y
               WHEN    1500
                   MOVE    8           TO  IDX-Y
               WHEN    1600
                   MOVE    9           TO  IDX-Y
               WHEN    1700
                   MOVE   10           TO  IDX-Y
               WHEN    1800
                   MOVE   11           TO  IDX-Y
               WHEN    1900
                   MOVE   12           TO  IDX-Y
               WHEN    2000
                   MOVE   13           TO  IDX-Y
               WHEN    2100
                   MOVE   14           TO  IDX-Y
               WHEN    OTHER
                   MOVE   ZERO         TO  IDX-Y
           END-EVALUATE
      *
           IF      YYK-YYKID           =   ZERO   AND
                   IDX-Y               >   ZERO
               MOVE    YYK-YYKMAXCNT   TO  SPA-NAI-YYYKMAXCNT
                                                         (IDX-Y IDX)
               MOVE    YYK-YYKCNT      TO  SPA-GMN-YYKCNT  (IDX-Y IDX)
           END-IF
      *
           .
        5201-YYKHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
      *            MOVE ZERO             TO   FLG-END
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    予約取消
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 420-YYKDEL-SEC
      *    予約
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 480-YOYAKU-SEC
      *    予約一覧へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-YYKICHI-SEC
      *    未院一覧
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MINICHI-SEC
      *    患者一覧
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-KANJCHI-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    予約登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOROKU-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
          EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYNAIYO"
      *        診療内容入力
                   PERFORM 41011-SRYNAIYO-SEC
      *       ドクター入力
               WHEN    "ACTIVATE"      ALSO    "DRNAME"
                   PERFORM 41012-DRNAME-SEC
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41013-KNJBAN-SEC
      *        新患予約者氏名
               WHEN    "ACTIVATE"      ALSO    "YYKNAME"
                   PERFORM 41014-YYKNAME-SEC
      ******** 予約日入力
      *        WHEN    "ACTIVATE"      ALSO    "YYKYMD"
      *            PERFORM 41014-YYKYMD-SEC
      *        予約時間
      *        WHEN    "ACTIVATE"      ALSO    "YYKTIME"
      *            PERFORM 41015-YYKTIME-SEC
      *        診療科
      *        WHEN    "ACTIVATE"      ALSO    "SRYKA"
      *            PERFORM 41016-SRYKA-SEC
      *        予約内容
      *        WHEN    "ACTIVATE"      ALSO    "YYKKBN"
      *            PERFORM 41017-YYKKBN-SEC
      *        確認連絡
      *        WHEN    "ACTIVATE"      ALSO    "KAKREN"
      *******      PERFORM 41018-KAKREN-SEC
      *        対象日入力
      *        WHEN    "ACTIVATE"      ALSO    "HYOJHI"
      *            MOVE    05          TO  SPA-GMN-CUR
      *            PERFORM 41015-HYOJHI-SEC
      *        日選択入力
      *        WHEN    "ACTIVATE"      ALSO    "GOKKEN"
      *            MOVE    06          TO  SPA-GMN-CUR
      *            PERFORM 41016-GOKSEL-SEC
      *        時間選択入力
      *        WHEN    "ACTIVATE"      ALSO    "KENSUU"
      *            MOVE    07          TO  SPA-GMN-CUR
      *            PERFORM 41017-KENSEL-SEC
      *        選択番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41018-SELBAN-SEC
      *        リスト選択
               WHEN    "SELECT"        ALSO    "PTLIST"
                   PERFORM 41019-CLIST-SEC
      *        カレンダー選択
               WHEN    "CLICKED"       ALSO    "C01"
               WHEN    "CLICKED"       ALSO    "CALENDAR1"
                   PERFORM 41020-CALENDAR-SEC
      *        その他選択
               WHEN    "CLICKED"       ALSO     ANY
                   PERFORM 4101A-SELFURIWAKE-SEC
      *        一括入力分
               WHEN    OTHER
      *           入力チェック処理
                  MOVE    1                   TO  FLG-TOUROKU
                  PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    予約患者振分処理
      *****************************************************************
       4101A-SELFURIWAKE-SEC         SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET(1:6)
               WHEN    "CLICKED"       ALSO    "CH_DAY"
      *        日選択入力
                   MOVE    MCP-WIDGET(7:1)
                                       TO  WRK-GMN-GOKSEL
                   PERFORM 41016-GOKSEL-SEC
                   GO                  TO  4101A-SELFURIWAKE-EXT
           END-EVALUATE
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET(1:9)
               WHEN    "CLICKED"       ALSO    "CH_KENSUU"
      *        時間選択入力
                   MOVE    MCP-WIDGET(10:2)
                                       TO  WRK-GMN-KENSEL
                   PERFORM 41017-KENSEL-SEC
           END-EVALUATE
           .
       4101A-SELFURIWAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    Y01-YYKYMD          TO  SPA-GMN-YYKYMD
           MOVE    Y01-YYKTIME         TO  SPA-GMN-YYKTIME
           MOVE    Y01-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    Y01-KAKREN          TO  SPA-GMN-KAKREN-G
           MOVE    Y01-YYKKBN          TO  SPA-GMN-YYKKBN-G
           MOVE    Y01-KAKREN          TO  SPA-GMN-KAKREN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *    予約日入力
           PERFORM 41014-YYKYMD-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    予約時間
           PERFORM 41015-YYKTIME-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    診療科
           PERFORM 41016-SRYKA-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    予約内容
           PERFORM 41017-YYKKBN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    確認連絡
           PERFORM 41018-KAKREN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療内容入力
      *****************************************************************
       41011-SRYNAIYO-SEC            SECTION.
      *
           MOVE    01                  TO  SPA-GMN-CUR
      *
           MOVE    Y01-SRYNAIYO        TO  SPA-GMN-SRYNAIYO
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYNAIYO-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYNAIYOCD =   SPA-GMN-SRYNAIYOLST(IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-SRYNAIYOLST (IDY)
                                           TO  SPA-GMN-SRYNAIYO
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-GMN-CUR
               GO      TO          41011-SRYNAIYO-EXT
           END-IF
      *
           IF      SPA-GMN-SRYNAIYOCD  NOT =   SPA-MAE-SRYNAIYOCD
      *****IF      SPA-YYK-SYORI         =   ZERO
      *        予約表設定処理
               PERFORM 410151-HYOJI-HEN-SEC
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD  TO  SPA-MAE-SRYNAIYOCD
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       41011-SRYNAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力
      *****************************************************************
       41012-DRNAME-SEC            SECTION.
      *
           MOVE    02                  TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-DRCD        TO  WRK-DRCD
      *
           MOVE    Y01-DRNAME          TO  SPA-GMN-DRNAME
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    "1"                 TO  SPA-NAI-DRCD1
               MOVE    ZERO                TO  SPA-NAI-DRCD2
               GO      TO      41012-DRNAME-EXT
           END-IF
      *    コード変換
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-DRCD        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-GMN-CUR
               GO      TO          41012-DRNAME-EXT
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-DRCD
           END-IF
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-DRCD-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-DRCD       =   SPA-GMN-DRCDLST(IDY)(1:4)
                   MOVE    SPA-GMN-DRCDLST (IDY)
                                           TO  SPA-GMN-DRNAME
                   MOVE    1               TO  FLG-HENFLG
                   MOVE    SPA-NAI-DR-SEMON (IDY)
                                           TO  WRK-NAI-DR-SEMON
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
      *        ドクターがない時、システム管理を検索する
               PERFORM 410121-DRCD-CHK-SEC
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    02                  TO  SPA-GMN-CUR
                   GO      TO          41012-DRNAME-EXT
               END-IF
           END-IF
      *
           MOVE    "1"                 TO  SPA-NAI-DRCD1
           MOVE    SPA-GMN-DRCD        TO  SPA-NAI-DRCD2
      *
      *    専門科を初期表示する
           IF      SPA-GMN-NAME        =   SPACE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   5
                   IF      WRK-DR-SEMONKACD (IDX)  NOT =   SPACE
                       PERFORM VARYING     IDY     FROM    1   BY  1
                               UNTIL       IDY     >   SPA-SRYKA-MAX
                           IF      WRK-DR-SEMONKACD (IDX)
                                           =   SPA-GMN-SRYKALST(IDY)
                                                               (1:2)
                               MOVE    SPA-GMN-SRYKALST(IDY)
                                                   TO  SPA-GMN-SRYKA-G
                               MOVE    SPA-SRYKA-MAX   TO  IDY
                               MOVE    5               TO  IDX
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *****IF      SPA-YYK-SYORI         =   ZERO
               IF      SPA-GMN-DRCD    NOT =   WRK-DRCD
      *            予約表設定処理
                    PERFORM 410151-HYOJI-HEN-SEC
               END-IF
      **** END-IF
      *
           MOVE    3                   TO  SPA-GMN-CUR
           .
       41012-DRNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力チェック処理
      *****************************************************************
       410121-DRCD-CHK-SEC            SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    SPA-GMN-DRCD        TO  SYS-1010-KBNCD(2:4)
      *
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    ORCSDR-SYSYMD       TO  ORC-DBYMD
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SPACE               TO  SPA-GMN-D1
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEI
               MOVE    SYS-1010-SEMONKACD(1)
                                           TO  WRK-DR-SEMONKACD(1)
               MOVE    SYS-1010-SEMONKACD(2)
                                           TO  WRK-DR-SEMONKACD(2)
               MOVE    SYS-1010-SEMONKACD(3)
                                           TO  WRK-DR-SEMONKACD(3)
               MOVE    SYS-1010-SEMONKACD(4)
                                           TO  WRK-DR-SEMONKACD(4)
               MOVE    SYS-1010-SEMONKACD(5)
                                           TO  WRK-DR-SEMONKACD(5)
      *        最後の行へ編集する
               IF      SPA-DRCD-MAX        =   50
                   MOVE    20              TO  SPA-DRCD-MAX
               ELSE
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPA-GMN-DRNAME      TO  SPA-GMN-DRCDLST
                                                      (SPA-DRCD-MAX)
               MOVE    1                   TO  FLG-HENFLG
           END-IF
      *
           .
       410121-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力
      *****************************************************************
       41013-KNJBAN-SEC            SECTION.
      *
           MOVE    03                  TO  SPA-GMN-CUR
           MOVE    Y01-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    SPACE               TO  SPA-GMN-SEX
               MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
      *        氏名入力へ
               MOVE    4               TO  SPA-GMN-CUR
               GO      TO      41013-KNJBAN-EXT
           END-IF
      *
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPID          TO  SPTID-HOSPID
           MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-1009-TBL
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0003"           TO  SPA-ERRCD
               MOVE    ZERO             TO  SPA-GMN-PTID
               MOVE    SPACE            TO  SPA-GMN-NAME
               MOVE    SPACE            TO  SPA-GMN-SEX
               MOVE    SPACE            TO  SPA-GMN-BIRTHDAYH
               MOVE    03                  TO  SPA-GMN-CUR
               GO  TO                  41013-KNJBAN-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 920-PTINF-READ-SEC
           IF      FLG-PTINF      NOT =   ZERO
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    SPACE               TO  SPA-GMN-NAME
               MOVE    SPACE               TO  SPA-GMN-SEX
               MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    03                  TO  SPA-GMN-CUR
               GO      TO      41013-KNJBAN-EXT
           END-IF
      *
           MOVE    PTINF-NAME         TO  SPA-GMN-NAME
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAYH
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41013-KNJBAN-EXT
           END-IF
      *
           IF      SPA-GMN-YYKNAME NOT =   SPA-GMN-NAME
      *********MOVE    "0006"          TO  SPA-ERRCD
               MOVE    SPA-GMN-NAME    TO  SPA-GMN-YYKNAME
           END-IF
           MOVE    1                   TO   SPA-YYK-SYORI
      *
           INITIALIZE                  SPA-Y01KYOUTU
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
      *
           .
       41013-KNJBAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    新患予約者氏名
      *****************************************************************
       41014-YYKNAME-SEC            SECTION.
      *
           MOVE    04                  TO  SPA-GMN-CUR
      *
           MOVE    Y01-YYKNAME         TO  SPA-GMN-YYKNAME
      *
           INITIALIZE                  SPA-Y01KYOUTU
      *
           IF      SPA-GMN-YYKNAME     =   SPACE
               GO      TO      41014-YYKNAME-EXT
           END-IF
      *
           IF      SPA-GMN-PTNUM   NOT =   SPACE
               IF      SPA-GMN-YYKNAME     =   SPA-GMN-NAME
                   GO      TO      41014-YYKNAME-EXT
               END-IF
           END-IF
      *
      *    全角半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-YYKNAME     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0017"          TO  SPA-ERRCD
               GO      TO      41014-YYKNAME-EXT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-YYKNAME
           END-IF
      *
           MOVE    SPACE           TO  WRK-KNJ
           PERFORM 410141-KNJKEN-HEN-SEC
      *
      *    IF      IDZ                  =   ZERO
      *        MOVE    "0004"          TO  SPA-ERRCD
      *        GO      TO      41014-YYKNAME-EXT
      *    END-IF
      *
           IF      IDZ                  >   1
      *        氏名検索へ
               PERFORM 470-SIMEIKEN-SEC
           ELSE
               IF      WRK-PTID           =   SPACE
                   MOVE    ZERO                TO  SPA-GMN-PTID
                   MOVE    SPACE               TO  SPA-GMN-PTNUM
                   MOVE    SPACE               TO  SPA-GMN-NAME
                   MOVE    SPACE               TO  SPA-GMN-SEX
                   MOVE    SPACE               TO  SPA-GMN-BIRTHDAYH
               ELSE
      *        患者マスタに予約者あり
                   MOVE    WRK-PTID        TO  SPA-GMN-PTID
                   MOVE    WRK-PTNUM       TO  SPA-GMN-PTNUM
                   MOVE    WRK-NAME        TO  SPA-GMN-NAME
                   MOVE    WRK-SEX         TO  SPA-GMN-SEX
                   MOVE    WRK-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYH
                   MOVE    WRK-NAME        TO  SPA-GMN-YYKNAME
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                        GO      TO      41014-YYKNAME-EXT
                   END-IF
               END-IF
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
           MOVE    1                   TO   SPA-YYK-SYORI
           .
       41014-YYKNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       410141-KNJKEN-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDZ
      *
      *    漢字検索
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-YYKNAME     TO  PTINF-NAME
      *
      *    患者マスターを漢字氏名で検索
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "PTINF-KEY2"        TO  ORC-DBPATH
               PERFORM 910-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           PERFORM UNTIL      (IDZ             >   1         ) OR
                              (FLG-PTINF       =   1         )
               PERFORM 410141-PTINF-HENSYU-SEC
      *
               MOVE    "PTINF-KEY2"        TO  ORC-DBPATH
               PERFORM 910-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY2"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    かな検索
           IF      IDZ                 =   ZERO
               INITIALIZE                  PTINF-REC
               MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-GMN-YYKNAME     TO  PTINF-KANANAME
      *
      *        患者マスターをかな氏名で検索
               MOVE    PTINF-REC           TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
                   PERFORM 910-PTINF-READ-SEC
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1                   TO  FLG-PTINF
               END-IF
               PERFORM UNTIL      (IDZ             >   1     ) OR
                                  (FLG-PTINF       =   1     )
                   PERFORM 410141-PTINF-HENSYU-SEC
      *
                   MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
                   PERFORM 910-PTINF-READ-SEC
               END-PERFORM
            END-IF
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY3"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       410141-KNJKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       410141-PTINF-HENSYU-SEC          SECTION.
      *
      *    患者番号変換より、患者NOより患者IDを取得する
      *
           ADD     1                   TO  IDZ
           MOVE    PTINF-PTID          TO  WRK-PTID
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPID          TO  SPTID-HOSPID
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
      *----(01.00.01) LINE ADD START ----------------------------------
                                              SPA-1009-TBL
      *----(01.00.01) LINE ADD END   ----------------------------------
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    PTINF-NAME         TO  WRK-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  WRK-SEX
               WHEN    "2"
                   MOVE    "女"                TO  WRK-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-BIRTHDAYH
           .
       410141-PTINF-HENSYU-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約日入力
      *****************************************************************
       41014-YYKYMD-SEC             SECTION.
      *
      *     MOVE    5                  TO  SPA-GMN-CUR
      *
      **** MOVE    Y01-YYKYMD          TO  SPA-GMN-YYKYMD
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-YYKYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-YYKYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-YYKYMD
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
               GO      TO      41014-YYKYMD-EXT
           END-IF
      *
      *    一覧表より選択時、予約時間へ
           IF      SPA-YYK-SYORI       =   2
      *********MOVE    6                   TO  SPA-GMN-CUR
               CONTINUE
           ELSE
               IF     (SPA-NAI-YYKYMD  NUMERIC   )  AND
                      (SPA-GMN-YYKYMD  NOT =   SPA-GMN-TAIYMD )
                   MOVE    SPA-GMN-YYKTIME     TO  WRK-GMN-YYKTIME
                   MOVE    SPA-NAI-YYKYY       TO  Y01-YEAR
                   MOVE    SPA-NAI-YYKMM       TO  Y01-MONTH
                   MOVE    SPA-NAI-YYKDD       TO  Y01-DAY
                   PERFORM 41020-CALENDAR-SEC
                   IF      WRK-GMN-YYKTIME     NOT =   SPACE
                       MOVE    WRK-GMN-YYKTIME     TO  SPA-GMN-YYKTIME
                   END-IF
      *******      MOVE    6                   TO  SPA-GMN-CUR
      *******  ELSE
      ******       MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41014-YYKYMD-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約時間入力
      *****************************************************************
       41015-YYKTIME-SEC             SECTION.
      *
      **   MOVE    6                   TO  SPA-GMN-CUR
      *
      **   MOVE    Y01-YYKTIME         TO  SPA-GMN-YYKTIME
           IF     (SPA-GMN-YYKTIME     =   SPACE )  AND
                  (FLG-TOUROKU         =   1     )
               GO      TO      41015-YYKTIME-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-TIME-X
           MOVE    ZERO                TO  WRK-TIME
           MOVE    ZERO                TO  IDX-T1
           MOVE    ZERO                TO  IDX-T2
           MOVE    ZERO                TO  FLG-TIME
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-YYKTIME(IDX:1)  NOT =   SPACE
                   IF      SPA-GMN-YYKTIME(IDX:1)  =   ":"
                       MOVE    1               TO  FLG-TIME
                   ELSE
                     IF      FLG-TIME            =   ZERO
      *                時
                       ADD     1               TO  IDX-T1
                       IF      IDX-T1          >   2
                           MOVE    1               TO  FLG-TIME
                       ELSE
                           MOVE    SPA-GMN-YYKTIME(IDX:1)
                                               TO  WRK-THHX(IDX-T1:1)
                       END-IF
                     END-IF
                     IF      FLG-TIME          =   1
      *                分
                       ADD     1               TO  IDX-T2
                       IF      IDX-T2          >   2
                           MOVE    "0008"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-YYKTIME(IDX:1)
                                               TO  WRK-TSSX(IDX-T2:1)
                       END-IF
                     END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6                   TO  SPA-GMN-CUR
               GO    TO      41015-YYKTIME-EXT
           END-IF
      *
      *    予約時
           INITIALIZE                      ORCSNUMAREA
           MOVE    2                   TO  SNUM-KETSUU
           MOVE    WRK-THHX            TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-THH
           END-IF
      *    予約分
           INITIALIZE                      ORCSNUMAREA
           MOVE    2                   TO  SNUM-KETSUU
           MOVE    WRK-TSSX            TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-TSS
           END-IF
      *
           MOVE    WRK-THH             TO  SPA-GMN-YYKTIME(1:2)
           MOVE    ":"                 TO  SPA-GMN-YYKTIME(3:1)
           MOVE    WRK-TSS             TO  SPA-GMN-YYKTIME(4:2)
      *
           IF      WRK-TSS             >=  60
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           END-IF
           IF      WRK-THH             >   24
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
               GO      TO      41015-YYKTIME-EXT
           END-IF
      *    予約時間
           MOVE    WRK-TIME            TO  SPA-NAI-YYKTIME
           MOVE    WRK-TIME            TO  SPA-NAI-KYYKTIME
           IF      SPA-NAI-YYKTIME(1:2)    <=  "08"
               MOVE    "08"                TO  SPA-NAI-KYYKTIME(1:2)
           END-IF
           IF      SPA-NAI-YYKTIME(1:2)    >=  "21"
               MOVE    "21"                TO  SPA-NAI-KYYKTIME(1:2)
           END-IF
      *    キー予約時間
      *****MOVE    SPA-NAI-YYKTIME         TO  SPA-NAI-KYYKTIME(1:2)
           MOVE    ZERO                    TO  SPA-NAI-KYYKTIME(3:2)
      *
           IF      (SPA-NAI-YYKYMD     =   SPA-Y01-YYKYMDS )  AND
                   (SPA-NAI-KYYKTIME   =   SPA-Y01-YYKTIME )
               CONTINUE
           ELSE
               IF      (SPA-NAI-YYKYMD      =   SPA-CHK-YYKYMD )  AND
                       (SPA-NAI-KYYKTIME    =   SPA-CHK-KYYKTIME)
                   CONTINUE
               ELSE
      *            予約件数チェック
                   PERFORM 410161-YYKKEN-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    6                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
      *****MOVE    7                   TO  SPA-GMN-CUR
      *
           .
       41015-YYKTIME-EXT.
           EXIT.
      * 
      *****************************************************************
      *    予約件数チェック
      *****************************************************************
       410161-YYKKEN-CHK-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-CHK-YYKYMD
           MOVE    ZERO                TO  SPA-CHK-KYYKTIME
      *
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               IF      YYK-YYKMAXCNT       <=  YYK-YYKCNT
                   MOVE    SPA-NAI-YYKYMD      TO  SPA-CHK-YYKYMD
                   MOVE    SPA-NAI-KYYKTIME    TO  SPA-CHK-KYYKTIME
                   MOVE    "1000"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410161-YYKKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科入力
      *****************************************************************
       41016-SRYKA-SEC            SECTION.
      *
      **** MOVE    7                   TO  SPA-GMN-CUR
      *
      **** MOVE    Y01-SRYKA           TO  SPA-GMN-SRYKA-G
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYKA-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYKA      =   SPA-GMN-SRYKALST (IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-SRYKALST (IDY)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *    選択なしはエラー
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
               GO      TO          41016-SRYKA-EXT
           END-IF
      *
      *****MOVE    8                   TO  SPA-GMN-CUR
           .
       41016-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    予約内容入力
      *****************************************************************
       41017-YYKKBN-SEC            SECTION.
      *
      *****MOVE    8                   TO  SPA-GMN-CUR
      *
      *****MOVE    Y01-YYKKBN        TO  SPA-GMN-YYKKBN-G
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-YYKKBN-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-YYKKBN     =   SPA-GMN-YYKKBNLST (IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-YYKKBNLST (IDY)
                                           TO  SPA-GMN-YYKKBN-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *    選択なしはエラー
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO          41017-YYKKBN-EXT
           END-IF
      *
      *****MOVE    9                   TO  SPA-GMN-CUR
           .
       41017-YYKKBN-EXT.
           EXIT.
      *****************************************************************
      *    確認連絡入力
      *****************************************************************
       41018-KAKREN-SEC            SECTION.
      *
      **** MOVE    9                   TO  SPA-GMN-CUR
      **** MOVE    Y01-KAKREN          TO  SPA-GMN-KAKREN-G
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-KAKREN-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-KAKREN     =   SPA-GMN-KAKRENLST (IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-KAKRENLST (IDY)
                                           TO  SPA-GMN-KAKREN-G
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
      *
      *****MOVE    10                  TO  SPA-GMN-CUR
           .
       41018-KAKREN-EXT.
           EXIT.
      *       
      *****************************************************************
      *    日選択入力
      *****************************************************************
       410151-HYOJI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
               GO      TO      410151-HYOJI-HEN-EXT
           END-IF
      *    予約テーブル編集
           INITIALIZE                  SPA-SEL-AREA
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
           .
       410151-HYOJI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日選択入力
      *****************************************************************
       41016-GOKSEL-SEC             SECTION.
      *
           MOVE    WRK-GMN-GOKSEL   TO  SPA-SEL-YOUBI
      *****COMPUTE SPA-SEL-YOUBI = WRK-GMN-GOKSEL * 1
           MOVE    ZERO             TO  SPA-SEL-TIME
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX      >   ZERO
                   MOVE    12               TO  SPA-GMN-CUR
               ELSE
      ***********  IF      SPA-GMN-CUR      =   12
                       MOVE    3            TO  SPA-GMN-CUR
      ***********  END-IF
               END-IF
           ELSE
               MOVE    5               TO  SPA-GMN-CUR
           END-IF
           .
       41016-GOKSEL-EXT.
           EXIT.
      *       
      *****************************************************************
      *    予約患者一覧再編集
      *****************************************************************
       4001-KNJICHI-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
           INITIALIZE                  SPA-GMN-SELBAN
           INITIALIZE                  SPA-NAI-SELBAN
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           MOVE    SPA-SEL-YOUBI       TO  IDX-Y
           IF      SPA-SEL-TIME        =   ZERO
               MOVE    1               TO  IDX-T
           ELSE
               MOVE    SPA-SEL-TIME    TO  IDX-T
           END-IF
      *
      *    対象日を予約日へ
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  SPA-NAI-YYKYMD
      *
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  YYK-YYKYMD
           IF      SPA-SEL-TIME        =   ZERO
               MOVE    ZERO            TO  WRK-KEY-YYKTIME
           ELSE
               MOVE    SPA-NAI-YYYKTIME (IDX-T IDX-Y)
                                       TO  WRK-KEY-YYKTIME
           END-IF
           MOVE    WRK-KEY-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
      *    時間帯毎の予約を保存する
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  YYK-YYKYMD
           IF      SPA-SEL-TIME        =   ZERO
      *        MOVE    ZERO            TO  YYK-KEYYYKTIME
               MOVE    "YYK-KEY3"      TO  WRK-PATH
           ELSE
               MOVE    SPA-NAI-YYYKTIME (IDX-T IDX-Y)
                                       TO  YYK-KEYYYKTIME
               MOVE    "YYK-KEY2"      TO  WRK-PATH
           END-IF
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
            ELSE
                MOVE    1              TO  FLG-YYK
            END-IF
           MOVE    ZERO                TO  IDX
           PERFORM 
               UNTIL     (FLG-YYK          =   1       )
                     OR  (IDX              >=  200     )
      *
               IF      YYK-YYKID       =   ZERO
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
                   MOVE    IDX             TO  SPA-GMN-TBANGO   (IDX)
                   MOVE    YYK-YYKTIME     TO  SPA-GMN-TYYKTIME (IDX)
                   MOVE    YYK-NAME        TO  SPA-GMN-TNAME    (IDX)
                   PERFORM 41002-SRYKAMEI-SET-SEC
                   IF      YYK-RAIINZUMI   =   "1"
                       MOVE    "来院"      TO  SPA-GMN-TRAIINZUMI(IDX)
                   END-IF
                   MOVE    YYK-PTID        TO  SPA-NAI-TPTID   (IDX)
                   IF      YYK-PTID    NOT =   ZERO
                       INITIALIZE                      ORCSPTIDAREA
                       MOVE    YYK-HOSPID          TO  SPTID-HOSPID
                       MOVE    YYK-PTID            TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
      *----(01.01.01) LINE ADD START ----------------------------------
                                                       SPA-1009-TBL
      *----(01.01.01) LINE ADD END   ----------------------------------
                       IF      SPTID-RC        =   ZERO
                            MOVE    SPTID-PTNUM    TO  SPA-NAI-TPTNUM
                                                                 (IDX)
                       END-IF
                   END-IF
                   MOVE    YYK-KEYYYKTIME  TO  SPA-NAI-TKYYKTIME (IDX)
                   MOVE    YYK-YYKYMD      TO  SPA-NAI-TYYKYMD  (IDX)
                   MOVE    YYK-YYKID       TO  SPA-NAI-TYYKID   (IDX)
                   MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA   (IDX)
                   MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA   (IDX)
                   MOVE    YYK-YYKKBN      TO  SPA-NAI-TYYKKBN  (IDX)
                   MOVE    YYK-KAKREN      TO  SPA-NAI-TKAKREN  (IDX)
      *
                   MOVE    IDX             TO  SPA-YYK-MAX
               END-IF
      *
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *
           MOVE    IDX             TO  SPA-GMN-MAX
      *
           PERFORM 40011-TBLSORT-SEC 
      *
      *     対象時間表示
           IF      WRK-KEY-YYKTIME     =   ZERO
               MOVE    SPACE                   TO  SPA-GMN-TAITIME
               MOVE    ZERO                    TO  SPA-NAI-KYYKTIME
               MOVE    SPACE                   TO  SPA-GMN-YYKTIME
               MOVE    ZERO                    TO  SPA-NAI-YYKTIME
           ELSE
               MOVE    WRK-KEY-YYKTIME(1:2)    TO  SPA-GMN-TAITIME(1:2)
               MOVE    ":"                     TO  SPA-GMN-TAITIME(3:1)
               MOVE    WRK-KEY-YYKTIME(3:2)    TO  SPA-GMN-TAITIME(4:2)
      *        予約時間の表示
               MOVE    WRK-KEY-YYKTIME         TO  SPA-NAI-KYYKTIME
               MOVE    SPA-GMN-TAITIME         TO  SPA-GMN-YYKTIME
               MOVE    WRK-KEY-YYKTIME         TO  SPA-NAI-YYKTIME
           END-IF
      *
      *     対象日表示
           MOVE    SPA-NAI-YYYKYMD (01 IDX-Y)
                                       TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIYMD
      *
           MOVE    SPA-GMN-TAIYMD      TO  SPA-GMN-YYKYMD
      *
           .
       4001-KNJICHI-EXT.
           EXIT.
      *       
      *****************************************************************
      *    科名設定処理
      *****************************************************************
       41002-SRYKAMEI-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    YYK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    YYK-YYKYMD          TO  ORC-DBYMD
      *D   MOVE    ZERO                TO  SYS-1005-STYUKYMD
      *D   MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRIT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME3 TO  SPA-GMN-TKAMEI  (IDX)
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       41002-SRYKAMEI-SET-EXT.
           EXIT.
      *       
      *****************************************************************
      *    患者一覧ＳＯＲＴ処理
      *****************************************************************
       40011-TBLSORT-SEC             SECTION.
      *
           INITIALIZE                  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           MOVE    SPA-NAI-TBL         TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               MOVE    HIGH-VALUE          TO  WRK-YYKTIME
               MOVE    ZERO                TO  WRK-IDX
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-GMN-MAX
                   IF      WRK-GMN-TBANGO (IDY)  =   ZERO
                       CONTINUE
                   ELSE
                       IF      WRK-GMN-TYYKTIME (IDY)  <  WRK-YYKTIME
                           MOVE    WRK-GMN-TYYKTIME (IDY)
                                               TO  WRK-YYKTIME
                           MOVE    IDY         TO  WRK-IDX
                       END-IF
                   END-IF
               END-PERFORM
               IF      WRK-IDX         >   ZERO
                   MOVE    WRK-GMN-TBLO (WRK-IDX)
                                           TO  SPA-GMN-TBLO(IDX)
                   MOVE    WRK-NAI-TBLO (WRK-IDX)
                                           TO  SPA-NAI-TBLO(IDX)
                   MOVE    IDX             TO  SPA-GMN-TBANGO (IDX)
      *
      *            INITIALIZE              WRK-GMN-TBLO (WRK-IDX)
      *            INITIALIZE              WRK-NAI-TBLO (WRK-IDX)
                   MOVE    SPACE           TO  WRK-GMN-TBLO (WRK-IDX)
                   MOVE    ZERO            TO  WRK-GMN-TBANGO (WRK-IDX)
                   MOVE    SPACE           TO  WRK-NAI-TBLO   (WRK-IDX)
                   MOVE    ZERO            TO  WRK-NAI-TKYYKTIME
                                                              (WRK-IDX)
                   MOVE    ZERO            TO  WRK-NAI-TYYKID
                                                              (WRK-IDX)
               END-IF
           END-PERFORM
           .
       40011-TBLSORT-EXT.
           EXIT.
      *       
      *****************************************************************
      *    時間選択入力
      *****************************************************************
       41017-KENSEL-SEC             SECTION.
      *
           COMPUTE SPA-SEL-TIME     =  (WRK-GMN-KENSEL -  1 )
                                    /   7    +  1
           COMPUTE WRK-KEI          =   SPA-SEL-TIME   *  7
           COMPUTE SPA-SEL-YOUBI    =   7
                                    -  (WRK-KEI    -   WRK-GMN-KENSEL)
      *
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX      >   ZERO
                   MOVE    12               TO  SPA-GMN-CUR
               ELSE
                   MOVE    3            TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
      *
           .
       41017-KENSEL-EXT.
           EXIT.
      *       
      *****************************************************************
      *    選択番号入力
      *****************************************************************
       41018-SELBAN-SEC             SECTION.
      *
           MOVE    12                TO  SPA-GMN-CUR
           MOVE    Y01-SELNUM        TO  SPA-GMN-SELBAN
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    Y01-SELNUM          TO  SNUM-INX
           MOVE    3                   TO  SNUM-KETSUU
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0005"              TO  SPA-ERRCD
               GO      TO      41018-SELBAN-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-SELBAN
           END-IF
      *
           IF      SPA-NAI-SELBAN      <   1   OR  >  SPA-GMN-MAX
               MOVE    "0005"          TO  SPA-ERRCD
               GO      TO      41018-SELBAN-EXT
           END-IF
      *
           PERFORM 410181-YYKSET-SEC
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
           .
       41018-SELBAN-EXT.
           EXIT.
      *****************************************************************
      *    予約リスト選択時
      *****************************************************************
       410181-YYKSET-SEC            SECTION.
      *
      *    予約マスター変更へ
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKYMD
           MOVE    SPA-NAI-YYKYMD      TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG        TO  SPA-GMN-YYKYMD
      *
           MOVE    SPA-GMN-TYYKTIME(SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKTIME
           MOVE    SPA-NAI-YYKTIME(1:2)    TO  SPA-GMN-YYKTIME(1:2)
           MOVE    ":"                     TO  SPA-GMN-YYKTIME(3:1)
           MOVE    SPA-NAI-YYKTIME(3:2)    TO  SPA-GMN-YYKTIME(4:2)
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  SPA-NAI-KYYKTIME
           MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
                                       TO  SPA-NAI-YYKID
           MOVE    SPA-NAI-TPTNUM  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-PTNUM
           MOVE    SPA-NAI-TPTID  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-PTID
           MOVE    SPA-GMN-TNAME  (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-NAME
           MOVE    SPA-NAI-TSRYKA (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-SRYKA
           MOVE    SPA-NAI-TYYKKBN (SPA-NAI-SELBAN)
                                       TO  SPA-GMN-YYKKBN
           MOVE    SPA-NAI-TKAKREN(SPA-NAI-SELBAN)
                                       TO  SPA-GMN-KAKREN
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-NAME        TO  SPA-GMN-YYKNAME
               MOVE    SPACE               TO  SPA-GMN-NAME
           ELSE
               MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-GMN-PTID        TO  PTINF-PTID
               PERFORM 920-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
                   MOVE    ZERO                TO  IDZ
                   PERFORM 410141-PTINF-HENSYU-SEC
                   MOVE    PTINF-NAME          TO  SPA-GMN-NAME
                   MOVE    WRK-SEX             TO  SPA-GMN-SEX
                   MOVE    WRK-BIRTHDAYH       TO  SPA-GMN-BIRTHDAYH
                   MOVE    PTINF-NAME          TO  SPA-GMN-YYKNAME
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
               END-IF
           END-IF
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKALST(IDX)(1:2)  =   SPA-GMN-SRYKA
                   MOVE    SPA-GMN-SRYKALST(IDX)   TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *    予約内容
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YYKKBN-MAX
               IF      SPA-GMN-YYKKBNLST(IDX)(1:2)
                                            =   SPA-GMN-YYKKBN
                   MOVE    SPA-GMN-YYKKBNLST(IDX)
                                           TO  SPA-GMN-YYKKBN-G
                   MOVE    SPA-YYKKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    連絡
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KAKREN-MAX
               IF      SPA-GMN-KAKRENLST(IDX)(1:2)
                                            =   SPA-GMN-KAKREN
                   MOVE    SPA-GMN-KAKRENLST(IDX)
                                           TO  SPA-GMN-KAKREN-G
                   MOVE    SPA-KAKREN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    2                   TO  SPA-YYK-SYORI
      *
           INITIALIZE                  SPA-Y01KYOUTU
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
           MOVE    SPA-GMN-SRYNAIYOCD  TO  SPA-Y01-SRYNAIYO
           MOVE    SPA-GMN-YYKYMD      TO  SPA-Y01-YYKYMD
           MOVE    SPA-NAI-YYKYMD      TO  SPA-Y01-YYKYMDS
           MOVE    SPA-NAI-KYYKTIME    TO  SPA-Y01-YYKTIME
           MOVE    SPA-NAI-YYKID       TO  SPA-Y01-YYKID
           MOVE    SPA-NAI-YYKYMD      TO  SPA-OLD-YYKYMD
      *
           .
       410181-YYKSET-EXT.
           EXIT.
      *****************************************************************
      *    予約リスト選択時
      *****************************************************************
       41019-CLIST-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
      ************ UNTIL      (IDX         >   Y01-PTLIST-COUNT)
                   UNTIL      (IDX         >   SPA-GMN-MAX     )
               IF      Y01-PTLIST-SELECT (IDX) =   "T"
                   MOVE  SPA-GMN-TBANGO(IDX)
                                               TO  Y01-SELNUM
                   MOVE    SPA-GMN-MAX         TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM 41018-SELBAN-SEC
           .
       41019-CLIST-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー選択時
      *****************************************************************
       41020-CALENDAR-SEC         SECTION.
      *    画面入力の年月＝ＳＰＡの年月のチェック
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    Y01-YEAR            TO  WRK-CAL-YEAR
      ****                                 SPA-GMN-YEAR
           MOVE    Y01-MONTH           TO  WRK-CAL-MONTH
      ****                                 SPA-GMN-MONTH
           MOVE    Y01-DAY             TO  WRK-CAL-DAY
      *******                              SPA-GMN-DAY
                                           SPA-NAI-HYOJHI
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-CAL-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
                   MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
                   MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
                   MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
                   MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
                   MOVE "月"           TO  SPA-GMN-TAIYYMM (11:2)
                   MOVE  SPA-GMN-TAIYYMM
                                       TO  WRK-GMN-TAIYYMM
           ELSE
      *            MOVE SPACE          TO  SPA-GMN-TAIYYMM
                   MOVE    "0015"      TO  SPA-ERRCD
                   GO                  TO  41020-CALENDAR-EXT
           END-IF
           MOVE    WRK-CAL-YMD         TO  WRK-SYMD
           PERFORM 3101-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-TAIYMD
      *
           IF      (WRK-CAL-YEAR       =   SPA-GMN-YEAR )  AND
                   (WRK-CAL-MONTH      =   SPA-GMN-MONTH )
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
               PERFORM 41015-HYOJHI-SEC
           ELSE
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
               PERFORM 410202-CALSET-SEC
           END-IF
      *
      *    予約処理中でなく、選択数がある時、選択番号へ
           IF      SPA-YYK-SYORI       =   ZERO
               IF      SPA-GMN-MAX      >   ZERO
                   MOVE    12               TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-GMN-CUR      =   12  OR  ZERO
                       MOVE    3            TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41020-CALENDAR-EXT.
           EXIT.
      *       
      *****************************************************************
      *    対象日入力
      *****************************************************************
       41015-HYOJHI-SEC             SECTION.
      *
      *
           IF      SPA-GMN-DAY        >=  01  AND  <=  31
      *        予約表設定処理
               PERFORM 410151-HYOJI-HEN-SEC
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
           END-IF
           .
       41015-HYOJHI-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー選択時の患者一覧等の設定（異月）
      *****************************************************************
       410202-CALSET-SEC               SECTION.
      *
      *
           MOVE    SPACE               TO  SPA-GMN-GOKKEN-G
                                           SPA-NAI-HYOTBL-G
           MOVE    ZERO                TO  SPA-GMN-HYOTBL-G
           MOVE    Y01-YEAR            TO  WRK-SYSYY
           MOVE    Y01-MONTH           TO  WRK-SYSMM
           MOVE    Y01-DAY             TO  WRK-SYSDD
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"      USING    STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
              MOVE LNK-DAY2-EDTYMD3 (1:4)
                                       TO  SPA-GMN-TAIYYMM
              MOVE LNK-DAY2-EDTYMD1 (2:2)
                                       TO  SPA-GMN-TAIYYMM (5:2)
              MOVE "年"                TO  SPA-GMN-TAIYYMM (7:2)
              MOVE LNK-DAY2-EDTYMD1 (5:2)
                                       TO  SPA-GMN-TAIYYMM (9:2)
              MOVE "月"                TO  SPA-GMN-TAIYYMM (11:2)
           ELSE
              MOVE SPACE               TO  SPA-GMN-TAIYYMM
           END-IF
      *
      * １日の曜日算出
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           MOVE    "01"                TO  LNK-DAY3-YMD(7:2)
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE   0                   TO  LNK-DAY3-YOBI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           IF      LNK-DAY3-YOBI   NOT =   0
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   LNK-DAY3-YOBI )
                   MOVE  ZERO          TO  SPA-GMN-HYODD (IDX)
                   MOVE  ZERO          TO  SPA-NAI-YMD (IDX)
               END-PERFORM
           END-IF
      *
      * 月末日の算出
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-SYSYMD (1:6)    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING    STS-AREA-DAY
                                            LNK-DAY7-AREA
           IF      STS-DAY-RC1      NOT =   ZERO
              MOVE WRK-SYSYMD          TO  LNK-DAY7-KOYOMI
              MOVE "31"                TO  LNK-DAY7-KOYOMI (7:2)
           END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           IF      IDX                 =   ZERO
               MOVE    1               TO  IDX-S
           ELSE
               MOVE    IDX             TO  IDX-S
           END-IF
           PERFORM VARYING     IDX     FROM    IDX-S   BY  1
                   UNTIL      (IDX         >   42  )   OR
                              (WRK-HI      =   LNK-DAY7-KOYOMI (7:2) )
               ADD     1               TO  WRK-HI
               MOVE    WRK-HI          TO  SPA-GMN-HYODD (IDX)
               MOVE    WRK-SYSYMD (1:6)
                                       TO  SPA-NAI-YMD (IDX)(1:6)
               MOVE    WRK-HI          TO  SPA-NAI-YMD (IDX)(7:2)
           END-PERFORM
      *
           MOVE    WRK-CAL-DAY         TO  SPA-NAI-HYOJHI
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           END-IF
      *
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
      *    予約テーブル編集処理
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
      *
           PERFORM 4001-KNJICHI-SEC
      *
           .
       410202-CALSET-EXT.
           EXIT.
      *****************************************************************
      *    予約取消し
      *****************************************************************
       420-YYKDEL-SEC             SECTION.
           MOVE    Y01-SELNUM          TO  SNUM-INX
           MOVE    3                   TO  SNUM-KETSUU
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0005"              TO  SPA-ERRCD
               GO      TO      420-YYKDEL-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-SELBAN
           END-IF
           IF      SPA-NAI-SELBAN      <   01   OR  >  SPA-GMN-MAX
               MOVE    "0005"          TO  SPA-ERRCD
               GO      TO      420-YYKDEL-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  YYK-YYKYMD
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  YYK-KEYYYKTIME
           MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
                                       TO  YYK-YYKID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
      *    予約人数変更
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  YYK-YYKYMD
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
      *
           IF      FLG-YYK             =   ZERO
               COMPUTE YYK-YYKCNT      =   YYK-YYKCNT   -  1
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
      *        更新
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               DISPLAY "Y01 YYK DEL :" MCP-RC "."
           END-IF
      *
           MOVE    ZERO                TO  WRK-HI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   42
               IF      SPA-NAI-HYOJHI      =   SPA-GMN-HYODD (IDX)
                   MOVE    IDX             TO  WRK-HI
                   MOVE    98              TO  IDX
               END-IF
           END-PERFORM
           IF      WRK-HI          >   ZERO
               COMPUTE WRK-SEL-SYUU    =  (WRK-HI    -  1  )
                                       /   7    +    1
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
               GO      TO      420-YYKDEL-EXT
           END-IF
      *    予約テーブル編集
           INITIALIZE                  SPA-SEL-AREA
           MOVE    WRK-SEL-SYUU        TO  SPA-SEL-SYUU
           PERFORM 510-YYKTBL-SET-SEC
      *
      *    予約患者一覧再編集
           COMPUTE SPA-SEL-YOUBI    =   WRK-HI
                                    -  ((SPA-SEL-SYUU  -  1 )  *  7 )
           MOVE    ZERO             TO  SPA-SEL-TIME
           PERFORM 4001-KNJICHI-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-SELBAN
           MOVE    SPACE               TO  SPA-GMN-SELBAN
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-YYKYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-YYKYMD
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-KAKRENLST(1)
                                       TO  SPA-GMN-KAKREN-G
           MOVE    SPA-GMN-YYKKBNLST (2)
                                       TO  SPA-GMN-YYKKBN-G
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       420-YYKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理へ
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-YYKYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-YYKYMD
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-KAKRENLST(1)
                                       TO  SPA-GMN-KAKREN-G
           MOVE    SPA-GMN-YYKKBNLST (2)
                                       TO  SPA-GMN-YYKKBN-G
           MOVE    SPACE               TO  SPA-GMN-SELBAN
           MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約　処理へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-YYK-SYORI
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
          .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録　処理
      *****************************************************************
       490-TOROKU-SEC             SECTION.
      *
      *    登録チェック処理
           PERFORM 4901-TOROKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOROKU-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-Y01-YYKID       =   ZERO
      *        予約追加処理
               PERFORM 4901-YYKADD-SEC
           ELSE
               IF      (SPA-GMN-SRYNAIYOCD =   SPA-Y01-SRYNAIYO) AND
                       (SPA-NAI-DRCD(2:4)  =   SPA-Y01-DRCD(2:4)) AND
                       (SPA-NAI-YYKYMD     =   SPA-OLD-YYKYMD )   AND
                       (SPA-NAI-KYYKTIME   =   SPA-Y01-YYKTIME)
      *            予約修正処理
                   PERFORM 4902-YYKUPD-SEC
               ELSE
      *            予約削除処理
                   PERFORM 4903-YYKDEL-SEC
      *            予約追加処理
                   PERFORM 4901-YYKADD-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
      *    更新後内容表示
           MOVE    SPA-NAI-YYKYY       TO  Y01-YEAR
           MOVE    SPA-NAI-YYKMM       TO  Y01-MONTH
           MOVE    SPA-NAI-YYKDD       TO  Y01-DAY
           INITIALIZE                  SPA-GMN-YYKAREA
           PERFORM 41020-CALENDAR-SEC
      *
           MOVE    ZERO                TO  SPA-Y01-KEIKOKU
      *
           MOVE    SPACE               TO  SPA-CHK-YYKYMD
           MOVE    ZERO                TO  SPA-CHK-KYYKTIME
      *
      *    MOVE    SPA-SYSYMD          TO  SPA-NAI-YYKYMD
      *    MOVE    SPA-SYSYMDWH        TO  SPA-GMN-YYKYMD
      *
           MOVE    SPA-GMN-SRYKALST  (1)   TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-KAKRENLST (1)
                                       TO  SPA-GMN-KAKREN-G
           MOVE    SPA-GMN-YYKKBNLST (2)
                                       TO  SPA-GMN-YYKKBN-G
      *
      *
           MOVE    SPACE               TO  SPA-GMN-SELBAN
           MOVE    ZERO                TO  SPA-NAI-SELBAN
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           .
       490-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       4901-TOROKU-CHK-SEC             SECTION.
      *
      *    診療内容チェック
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-SRYNAIYO-MAX  )  OR
                          (FLG-HENFLG     =   1  )
               IF      SPA-GMN-SRYNAIYOCD =   SPA-GMN-SRYNAIYOLST(IDY)
                                                                  (1:2)
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    01                  TO  SPA-GMN-CUR
           END-IF
      *   ドクター入力
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY     FROM   1   BY  1
                   UNTIL  (IDY     >      SPA-DRCD-MAX  )  OR
                          (SPA-ERRCD  NOT =   SPACE     )  OR
                          (FLG-HENFLG     =   1         )
               IF      SPA-GMN-DRCD       =   SPA-GMN-DRCDLST(IDY)(1:4)
                   MOVE    1               TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF     (FLG-HENFLG          =   ZERO ) AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-GMN-CUR
           END-IF
      *    入力チェック処理
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  FLG-TOUROKU
               PERFORM 410-INPUT-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *
           IF     (SPA-GMN-PTNUM       =   SPACE  )  AND
                  (SPA-GMN-YYKNAME     =   SPACE  )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-YYKTIME     =   SPACE  OR  ZERO
               MOVE    6                   TO  SPA-GMN-CUR
               MOVE    "0014"              TO  SPA-ERRCD
               GO      TO      4901-TOROKU-CHK-EXT
           END-IF
      *    予約日か過去の時警告
           IF     (SPA-Y01-KEIKOKU     =   ZERO )  AND
                  (SPA-NAI-YYKYMD      <   SPA-SYSYMD)
               MOVE    "K001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-Y01-KEIKOKU
           END-IF
          .
       4901-TOROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    予約追加処理
      *****************************************************************
       4901-YYKADD-SEC             SECTION.
      *    予約ＩＤ＝００検索
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           MOVE    ZERO                TO  YYK-PTID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               ADD     1                   TO  YYK-YYKCNT
               MOVE    YYK-YYKCNT          TO  WRK-YYKID
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "YYK-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"      TO  SPA-ERRCD
                   DISPLAY "Y01 YYK-KEY UPD ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO                  TO  4901-YYKADD-EXT
               END-IF
           ELSE
               MOVE    SPACE               TO  YYK-REC
               INITIALIZE                      YYK-REC
               MOVE    SPA-HOSPID          TO  YYK-HOSPID
               MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
               MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
               MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
               MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
               MOVE    ZERO                TO  YYK-YYKID
               MOVE    10                  TO  YYK-YYKMAXCNT
               MOVE    1                   TO  YYK-YYKCNT
               MOVE    1                   TO  WRK-YYKID
               MOVE    ZERO                TO  YYK-PTID
      *
               MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"  TO  SPA-ERRCD
                   DISPLAY "Y01 YYK-KEY INS ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO              TO  4901-YYKADD-EXT
               END-IF
           END-IF
      *
      *    明細レコード追加
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YYK-KEY7"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "YYK-KEY7"          TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
           IF      FLG-YYK             =   ZERO
               MOVE    YYK-YYKID           TO  WRK-YYKID
               IF      WRK-YYKID           =   99
                   MOVE    "1013"          TO  SPA-ERRCD
                   GO      TO      4901-YYKADD-EXT
               END-IF
               ADD     1                   TO  WRK-YYKID
           END-IF
      *
      *    明細レコード追加
           MOVE    SPACE               TO  YYK-REC
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    WRK-YYKID           TO  YYK-YYKID
      *
           MOVE    SPA-NAI-YYKTIME     TO  YYK-YYKTIME
           MOVE    SPA-GMN-PTID        TO  YYK-PTID
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-YYKNAME     TO  YYK-NAME
           ELSE
               MOVE    SPA-GMN-NAME        TO  YYK-NAME
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  YYK-SRYKA
           MOVE    SPA-GMN-YYKKBN      TO  YYK-YYKKBN
           MOVE    SPA-GMN-KAKREN      TO  YYK-KAKREN
      *
           MOVE    SMCNDATE-YMD        TO  YYK-CREYMD
           MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
           MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT  =  ZERO
               MOVE    "1011"           TO  SPA-ERRCD
               DISPLAY "Y01 YYK MEI INS ERR:" MCP-RC "."
               GO      TO      4901-YYKADD-EXT
           END-IF
           .
       4901-YYKADD-EXT.
           EXIT.
      *****************************************************************
      *    予約修正処理
      *****************************************************************
       4902-YYKUPD-SEC            SECTION.
      *
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-NAI-KYYKTIME    TO  YYK-KEYYYKTIME
           MOVE    SPA-NAI-YYKID       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK          =   ZERO
               MOVE    SPA-NAI-YYKTIME     TO  YYK-YYKTIME
               MOVE    SPA-GMN-SRYKA       TO  YYK-SRYKA
               MOVE    SPA-GMN-YYKKBN      TO  YYK-YYKKBN
               MOVE    SPA-GMN-KAKREN      TO  YYK-KAKREN
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "YYK-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1012"  TO  SPA-ERRCD
                   DISPLAY "Y01 YYK MEI UPD ERR:" MCP-RC "."
               END-IF
           ELSE
      *        予約追加処理
               PERFORM 4901-YYKADD-SEC
           END-IF
           .
       4902-YYKUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約削除処理
      *****************************************************************
       4903-YYKDEL-SEC            SECTION.
      *
      *    前予約ＩＤ＝００修正
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-Y01-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    "1"                 TO  YYK-DRCD(1:1)
           MOVE    SPA-Y01-DRCD(2:4)   TO  YYK-DRCD(2:4)
           MOVE    SPA-OLD-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    ZERO                TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK          =   ZERO
               COMPUTE YYK-YYKCNT          =   YYK-YYKCNT  -   1
      ******** MOVE    SPA-NAI-YYKTIME     TO  YYK-YYKTIME
      ******** MOVE    SPA-GMN-SRYKA       TO  YYK-SRYKA
      *        MOVE    SPA-GMN-YYKKBN      TO  YYK-YYKKBN
      ******** MOVE    SPA-GMN-KAKREN      TO  YYK-KAKREN
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1010"      TO  SPA-ERRCD
                   GO                  TO  4903-YYKDEL-EXT
               END-IF
           END-IF
      *
      *    前予約ＩＤ≠００削除
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-Y01-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    "1"                 TO  YYK-DRCD(1:1)
           MOVE    SPA-Y01-DRCD(2:4)   TO  YYK-DRCD(2:4)
           MOVE    SPA-OLD-YYKYMD      TO  YYK-YYKYMD
           MOVE    SPA-Y01-YYKTIME     TO  YYK-KEYYYKTIME
           MOVE    SPA-Y01-YYKID       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "YYK-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"      USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
               IF      MCP-RC          NOT  =  ZERO
                   MOVE    "1012"      TO  SPA-ERRCD
                   DISPLAY "Y01 YYK MEI DEL ERR:" MCP-RC "."
                           ",KEY:" YYK-KEY
                   GO                  TO  4903-YYKDEL-EXT
               END-IF
           ELSE
               DISPLAY "Y01 YYK MEI DEL ??:" MCP-RC "."
                       ",KEY:" YYK-KEY
           END-IF
          .
       4903-YYKDEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    来院済み　処理
      *****************************************************************
       450-RAIZUMI-SEC             SECTION.
      *
           IF      SPA-NAI-SELBAN      <   01   OR  >  SPA-GMN-MAX
               MOVE    "0016"          TO  SPA-ERRCD
               GO      TO      450-RAIZUMI-EXT
           END-IF
      *
      *    予約マスター読込処理
           MOVE    SPA-HOSPID          TO  YYK-HOSPID
           MOVE    SPA-GMN-SRYNAIYOCD  TO  YYK-SRYNAIYO
           MOVE    SPA-NAI-DRCD        TO  YYK-DRCD
           MOVE    SPA-NAI-TYYKYMD (SPA-NAI-SELBAN)
                                       TO  YYK-YYKYMD
           MOVE    SPA-NAI-TKYYKTIME(SPA-NAI-SELBAN)
                                       TO  YYK-KEYYYKTIME
           MOVE    SPA-NAI-TYYKID  (SPA-NAI-SELBAN)
                                       TO  YYK-YYKID
           PERFORM 910-YOYAKU-INV-SEC
           IF      FLG-YYK             =   ZERO
               IF      YYK-RAIINZUMI       =   "1"
                   MOVE    SPACE           TO  YYK-RAIINZUMI
               ELSE
                   MOVE    "1"             TO  YYK-RAIINZUMI
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
      *        更新
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-IF
      *
      *    予約患者一覧再編集
           PERFORM 4001-KNJICHI-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-SELBAN
           MOVE    SPACE               TO  SPA-GMN-SELBAN
      *
           MOVE    ZERO                TO  SPA-YYK-SYORI
           INITIALIZE                  SPA-GMN-YYKAREA
      *
           .
       450-RAIZUMI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約一覧処理へ
      *****************************************************************
       440-YYKICHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01KYOUTU
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *    予約一覧の時ゼロを編集
           MOVE    ZERO                TO  SPA-Y01-YYKID
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
      *D     MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *D   IF      SPA-MOTOPG2         =   SPACE
      *D       MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *D   END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y03"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y03"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       440-YYKICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    未院一覧処理へ
      *****************************************************************
       460-MINICHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01KYOUTU
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *    未院一覧の時01を編集
           MOVE    01                  TO  SPA-Y01-YYKID
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *D   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      *    IF      SPA-MOTOPG2         =   SPACE
      *        MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *    END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y03"               TO  SPA-SAKIPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y03"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       460-MINICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者一覧処理へ
      *****************************************************************
       470-KANJCHI-SEC            SECTION.
      *
           INITIALIZE                      SPA-Y01KYOUTU
      *
           MOVE    SPA-GMN-SRYNAIYO    TO  SPA-Y01-SRYNAIYOG
           MOVE    SPA-GMN-DRNAME      TO  SPA-Y01-DRNAME
           MOVE    SPA-NAI-DRCD        TO  SPA-Y01-DRCD
      *    予約日が選択時、予約時、以外の時、システム日付とする
           IF      SPA-SEL-YOUBI       =   ZERO
               MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
           ELSE
                MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
           END-IF
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "Y04"               TO  SPA-SAKIPG
      *
           MOVE    SPA-Y01KYOUTU       TO  SPA-WORK
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
          .
       470-KANJCHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
      *    画面移行用のスパを編集する
           INITIALIZE                  SPA-KYOTUKEY
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
           MOVE    SPA-GMN-YYKNAME     TO  SPA-NAME
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P97    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    画面移行用のスパを編集する
           INITIALIZE                  SPA-KYOTUKEY
      *
           MOVE    "Y01"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-MOTOPG3         TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-MOTOPG
      *
           INITIALIZE                   SPA-KYOTUKEY
           MOVE    SPA-KYOUTU           TO  LNK-KYOUTU
      *
           IF      SPA-SAKIPG           =   SPACE   OR
                                            "U01"
      *         業務選択へ
                MOVE   "M01"            TO  SPA-SAKIPG
            END-IF
      *
           MOVE   "CHANGE"              TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
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
      *    MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Y01    "           TO  MCP-WINDOW.
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
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-Y01-ROW
           ELSE
               MOVE    Y01-PTLIST-ROW      TO  WRK-Y01-ROW
           END-IF
      *
           MOVE    SPACE               TO  Y01
      *
           MOVE    ZERO                TO  Y01-PTLIST-ROW
           MOVE    ZERO                TO  Y01-PTLIST-ROWATTR
           IF      WRK-Y01-ROW     NOT =   ZERO
               MOVE    WRK-Y01-ROW         TO  Y01-PTLIST-ROW
           END-IF
      *
      *    カレンダー
      *    予約日が選択時、予約時、以外の時、システム日付とする
      *    IF      SPA-SEL-YOUBI       =   ZERO
      *********MOVE    WRK-SYSYMD          TO  SPA-Y01-YYKYMDS
      *        MOVE    SPA-GMN-YEAR        TO  WRK-CAL-YEAR
      *        MOVE    SPA-GMN-MONTH       TO  WRK-CAL-MONTH
      *        MOVE    SPA-GMN-DAY         TO  WRK-CAL-DAY
      *    ELSE
      *        MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
      *                                    TO  SPA-Y01-YYKYMDS
      *        MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
      *    END-IF
      ****  MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
           IF      SPA-GMN-YEAR        =   ZERO
               MOVE    SPA-NAI-YYYKYMD (01  SPA-SEL-YOUBI)
                                           TO  SPA-Y01-YYKYMDS
               MOVE    SPA-Y01-YYKYMDS     TO  WRK-CAL-YMD
               MOVE    WRK-CAL-YMD         TO  SPA-GMN-CALENDR
           ELSE
               MOVE    SPA-GMN-CALENDR     TO  SPA-Y01-YYKYMDS
           END-IF
      *
           MOVE    SPA-GMN-YEAR        TO  Y01-YEAR
           MOVE    SPA-GMN-MONTH       TO  Y01-MONTH
           MOVE    SPA-GMN-DAY         TO  Y01-DAY
      *
      *    診療内容
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYNAIYO-MAX
               MOVE    SPA-GMN-SRYNAIYOLST (IDX)
                                           TO  Y01-SRYNAIYO-LST (IDX)
           END-PERFORM
           MOVE    SPA-SRYNAIYO-MAX    TO  Y01-SRYNAIYO-COUNT
           MOVE    SPA-GMN-SRYNAIYO    TO  Y01-SRYNAIYO
      *    ドクター
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (IDX)
                                           TO  Y01-DRNAME-LST (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  Y01-DRNAME-COUNT
           MOVE    SPA-GMN-DRNAME      TO  Y01-DRNAME
      *    診療科
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  Y01-SRYKA-LST (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  Y01-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  Y01-SRYKA
      *    予約内容
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-YYKKBN-MAX
               MOVE    SPA-GMN-YYKKBNLST (IDX)
                                           TO  Y01-YYKKBN-LST (IDX)
           END-PERFORM
           MOVE    SPA-YYKKBN-MAX       TO  Y01-YYKKBN-COUNT
           MOVE    SPA-GMN-YYKKBN-G     TO  Y01-YYKKBN
      *    連絡確認
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-KAKREN-MAX
               MOVE    SPA-GMN-KAKRENLST (IDX)
                                           TO  Y01-KAKREN-LST (IDX)
           END-PERFORM
           MOVE    SPA-KAKREN-MAX      TO  Y01-KAKREN-COUNT
           MOVE    SPA-GMN-KAKREN-G    TO  Y01-KAKREN
      *
           MOVE    SPA-GMN-PTNUM       TO  Y01-PTNUM
      *****MOVE    SPA-GMN-NAME        TO  Y01-NAME
           MOVE    SPA-GMN-SEX         TO  Y01-SEX
           MOVE    SPA-GMN-BIRTHDAYH   TO  Y01-BARTHDAY
           MOVE    SPA-GMN-YYKNAME     TO  Y01-YYKNAME
           MOVE    SPA-GMN-YYKYMD      TO  Y01-YYKYMD
           MOVE    SPA-GMN-YYKTIME     TO  Y01-YYKTIME
      *
           MOVE    SPA-GMN-TAIYYMM     TO  Y01-TAIYYMM
           MOVE    SPA-GMN-TAIYMD      TO  Y01-TAIYMD
           MOVE    SPA-GMN-TAITIME     TO  Y01-TAIKENSUU
      *    MOVE    SPA-GMN-HYOJHI      TO  Y01-HYOJHI
      *    予約表示
           MOVE    SPA-GMN-GOKKEN (01) TO  Y01-DAY1
           MOVE    SPA-GMN-GOKKEN (02) TO  Y01-DAY2
           MOVE    SPA-GMN-GOKKEN (03) TO  Y01-DAY3
           MOVE    SPA-GMN-GOKKEN (04) TO  Y01-DAY4
           MOVE    SPA-GMN-GOKKEN (05) TO  Y01-DAY5
           MOVE    SPA-GMN-GOKKEN (06) TO  Y01-DAY6
           MOVE    SPA-GMN-GOKKEN (07) TO  Y01-DAY7
      *
           INITIALIZE   WRK-GMN-KENSUU
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   98
               MOVE    SPACE               TO  WRK-HKENSUU
               MOVE    SPA-GMN-KENSUU (IDX)
                                           TO  WRK-KENSUU
      *        MOVE    WRK-HKENSUU(2:4)    TO  WRK-TBL-KENSUU (IDX)
               MOVE    WRK-HKENSUU         TO  WRK-TBL-KENSUU (IDX)
           END-PERFORM
           MOVE     WRK-GMN-KENSUU         TO  Y01-FIXED2
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
      *******IF  SPA-GMN-TBANGO (IDX)  NOT  =  ZERO
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  Y01-TBANGO (IDX)
               IF      SPA-GMN-TYYKTIME(IDX)   NOT =   SPACE
                   MOVE    SPA-GMN-TYYKTIME (IDX)(1:2)
                                           TO  Y01-TYYKTIME (IDX)(1:2)
                   MOVE    ":"             TO  Y01-TYYKTIME (IDX)(3:1)
                   MOVE    SPA-GMN-TYYKTIME (IDX)(3:2)
                                           TO  Y01-TYYKTIME (IDX)(4:2)
               END-IF
               MOVE    SPA-GMN-TNAME (IDX)
                                           TO  Y01-TNAME (IDX)
               MOVE    SPA-GMN-TKAMEI (IDX)
                                           TO  Y01-TKAMEI (IDX)
               MOVE    SPA-GMN-TRAIINZUMI(IDX)
                                           TO  Y01-TRAIINZUMI (IDX)
               MOVE    IDX
                                           TO  Y01-PTLIST-COUNT
      *        選択行反転
               IF      SPA-GMN-TBANGO (IDX)    =   SPA-NAI-SELBAN
                   MOVE    "T"             TO  Y01-PTLIST-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  Y01-PTLIST-SELECT  (IDX)
               END-IF
      ****** END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  Y01-PTLIST-COUNT
      *
      *
           IF      SPA-YYK-MAX         =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   (SPA-YYK-MAX   -  1  )
                                       /   12   +   1
           END-IF
      *
           MOVE    SPA-GMN-SELBAN      TO  Y01-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Y01-PTNUM-STATE
           MOVE    WIDGET-NORMAL       TO  Y01-YYKNAME-STATE
      *
      *    選択番号が入力時は、患者番号入力不可
           IF      SPA-YYK-SYORI         =   2
               MOVE    WIDGET-INSENSITIVE      TO  Y01-PTNUM-STATE
               IF      SPA-GMN-PTNUM   NOT =   SPACE
                   MOVE    WIDGET-INSENSITIVE  TO  Y01-YYKNAME-STATE
               END-IF
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-GMN-CURSET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE  "SRYNAIYO"    TO   MCP-WIDGET
                WHEN    02
                    MOVE  "DRNAME"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "PTNUM"       TO   MCP-WIDGET
                WHEN    04
                    MOVE  "YYKNAME"     TO   MCP-WIDGET
                WHEN    05
                    MOVE  "YYKYMD"      TO   MCP-WIDGET
                WHEN    06
                    MOVE  "YYKTIME"     TO   MCP-WIDGET
                WHEN    07
                    MOVE  "SRYKA"       TO   MCP-WIDGET
                WHEN    08
                    MOVE  "YYKKBN"      TO   MCP-WIDGET
                WHEN    09
                    MOVE  "KAKREN"      TO   MCP-WIDGET
                WHEN    10
                    MOVE  "B12"         TO   MCP-WIDGET
                WHEN    11
                    MOVE  "DAY1"        TO   MCP-WIDGET
                WHEN    12
                    MOVE  "SELNUM"      TO   MCP-WIDGET
                WHEN    OTHER
                    MOVE  "DAY1"        TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力後カーソルセット処理
      *****************************************************************
       50011-GMN-CURSET-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YYKYMD"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "YYKTIME"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "YYKKBN"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "KAKREN"
                   MOVE    10                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-GMN-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療内容コードの入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "ドクターコードの入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "入力された患者番号は登録されていません"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "選択番号に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "患者番号と予約氏名が共に入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "予約日の暦日エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "予約時間はHH:SSで入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "診療科選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "予約内容選択エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "予約時間の入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "予約時間は２４時間で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "予約患者を指定して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "予約時間を指定して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "カレンダーの選択エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "選択番号選択エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "予約氏名は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "1000"
                   MOVE    "警告！予約件数がオーバーしてます"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    "予約キーデータ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "予約明細データ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "予約明細データ削除エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "予約時間の予約が合計で９９以上になります。"
                                               TO  SPA-ERRMSG
                   MOVE    "予約時間の予約すべて削除して下さい。"
                                               TO  SPA-ERRMSG(43:)
               WHEN    "K001"
                   MOVE    "注意！予約日が過去です。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y01"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YOYAKU-INV-SEC         SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "YYK-KEY"           TO  ORC-DBPATH
               PERFORM 900-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "YYK-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-YOYAKU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRIT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRIT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
