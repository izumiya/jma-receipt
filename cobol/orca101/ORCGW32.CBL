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
       PROGRAM-ID.             ORCGW32.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 包括診療行為設定情報（Ｗ３２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/02/21    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.04.00    NACL-多々納  08/12/10  療養病棟入院料追加
      *  04.05.00    NACL-多々納  09/09/XX  慢性維持透析患者外来管理料、
      *                                     小児科外来診察料等追加
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
       01  SPA-W32.
           03  SPA-W32-AREA.
               05  SPA-GMN-CUR         PIC 9(03).
               05  SPA-GMN-SYORI       PIC 9(01).
      *
               05  SPA-GMN-AREA.
      *            区分コード
                   07  SPA-GMN-KBNCD-G.
                       09  SPA-GMN-KBNCD          PIC X(03).
                       09  SPA-GMN-KBNCD-F        PIC X(01).
                       09  SPA-GMN-KBNCD-MEI      PIC X(36).
                   07  SPA-GMN-KBNCD-LST-G.
                       09  SPA-GMN-KBNCD-LST      OCCURS   100.
                           11  SPA-GMN-TKBNCD     PIC X(03).
                           11  SPA-GMN-TKBNCD-F   PIC X(01).
                           11  SPA-GMN-TKBNCD-MEI PIC X(36).
                       09  SPA-KBNCD-MAX            PIC 9(04).
      *
               05  SPA-GMN-AREA2.
                   07  SPA-GMN-STYUKYMD       PIC X(10).
                   07  SPA-GMN-EDYUKYMD       PIC X(10).
      *
                   07  SPA-GMN-KIKANTBL.
                       09  SPA-GMN-TBLO          OCCURS  10.
                           11  SPA-GMN-TBANGO     PIC 9(02).
                           11  SPA-GMN-TSTYUKYMD  PIC X(09).
                           11  SPA-GMN-TEDYUKYMD  PIC X(09).
      *
                           11  SPA-NAI-TSTYUKYMD  PIC X(08).
                           11  SPA-NAI-TEDYUKYMD  PIC X(08).
                       09  SPA-GMN-TBL-MAX        PIC 9(04).
                   07  SPA-GMN-SELNUM             PIC 9(02).
      *
                   07  SPA-GMN-AREA3.
      *                包括算定方法
                       09  SPA-GMN-SANTEIHOU-G.
                           11  SPA-GMN-SANTEIHOU     PIC X(01).
                           11  SPA-GMN-SANTEIHOU-F   PIC X(01).
                           11  SPA-GMN-SANTEIHOU-MEI PIC X(38).
      *                包括算定区分
                       09  SPA-GMN-SANTEIKBN-G.
                           11  SPA-GMN-SANTEIKBN     PIC X(01).
                           11  SPA-GMN-SANTEIKBN-F   PIC X(01).
                           11  SPA-GMN-SANTEIKBN-MEI PIC X(38).
      *                入外区分
                       09  SPA-GMN-NYUGAIKBN-G.
                           11  SPA-GMN-NYUGAIKBN     PIC X(01).
                           11  SPA-GMN-NYUGAIKBN-F   PIC X(01).
                           11  SPA-GMN-NYUGAIKBN-MEI PIC X(38).
      *                保険組合せ区分
                       09  SPA-GMN-HKNCOMBIKBN-G.
                           11  SPA-GMN-HKNCOMBIKBN     PIC X(01).
                           11  SPA-GMN-HKNCOMBIKBN-F   PIC X(01).
                           11  SPA-GMN-HKNCOMBIKBN-MEI PIC X(38).
      *                診療区分
                       09  SPA-GMN-SRYKBN-ARA.
                           11  SPA-GMN-SRYKBN-TBL    OCCURS  12.
                               13  SPA-GMN-SRYKBN-G.
                                   15  SPA-GMN-SRYKBN     PIC X(01).
                                   15  SPA-GMN-SRYKBN-F   PIC X(01).
                                   15  SPA-GMN-SRYKBN-MEI PIC X(18).
      *
      *
               05  SPA-GMN-INIT-G.
      *            包括算定方法
                       09  SPA-GMN-SANTEIHOU-LST-G.
                           11  SPA-GMN-SANTEIHOU-LST
                                                   PIC X(40) OCCURS  10.
                           11  SPA-SANTEIHOU-MAX   PIC 9(02).
      *
                       09  SPA-GMN-SANTEIKBN-LST-G.
                           11  SPA-GMN-SANTEIKBN-LST
                                                   PIC X(40) OCCURS  10.
                           11  SPA-SANTEIKBN-MAX   PIC 9(02).
                       09  SPA-GMN-NYUGAIKBN-LST-G.
                           11  SPA-GMN-NYUGAIKBN-LST
                                                   PIC X(40) OCCURS  5.
                           11  SPA-NYUGAIKBN-MAX   PIC 9(02).
                       09  SPA-GMN-HKNCOMBIKBN-LST-G.
                           11  SPA-GMN-HKNCOMBIKBN-LST
                                                   PIC X(40) OCCURS  5.
                           11  SPA-HKNCOMBIKBN-MAX   PIC 9(02).
                       09  SPA-GMN-SRYKBN-LST-G.
                           11  SPA-GMN-SRYKBN-LST
                                                   PIC X(20) OCCURS  5.
                           11  SPA-SRYKBN-MAX      PIC 9(02).
      *
                       09  SPA-GMN-SRYKBN-MAX      PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC X(08).
                   07  SPA-NAI-EDYUKYMD       PIC X(08).
      *
                   07  SPA-MAE-STYUKYMD       PIC X(08).
                   07  SPA-MAE-EDYUKYMD       PIC X(08).
      *
                   07  SPA-NAI-KEIFLG         PIC 9(01).
      *
               05  SPA-NAIB-AREA2.
                   07  SPA-NAIB-KBNCD-LST-G.
                       09  SPA-NAIB-KBNCD-LST     OCCURS   100.
                           11  SPA-NAIB-YUKOSTYMD     PIC X(08).
                           11  SPA-NAIB-YUKOEDYMD     PIC X(08).
      *            包括診療データの期間
                   07  SPA-HKT-YUKOSTYMD       PIC X(08).
                   07  SPA-HKT-YUKOEDYMD       PIC X(08).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKTSRYCD        PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-PATH            PIC X(64).
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
      *    職員区分
           03  WRK-MAE-KBNCD-G.
               05  WRK-MAE-KBNCD       PIC X(03).
               05  WRK-MAE-KBNCDF      PIC X(01).
               05  WRK-MAE-KBNCDMEI    PIC X(38).
      *
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-CD              PIC X(02).
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
           COPY  "CPSK1014.INC".
      *    包括診療コードテーブル
       01  HKTSRYCD-REC.
           COPY  "CPHKTSRYCD.INC".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W32
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W32         TO  SPA-FREE
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
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      * 
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W32"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
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
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
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
           INITIALIZE   SPA-W32
      *
      *    包括診療コードより管理番号決定
           MOVE    SPACE               TO  HKTSRYCD-REC
           INITIALIZE                      HKTSRYCD-REC
           MOVE    SPA-SYSYMD          TO  HKTSRYCD-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  HKTSRYCD-YUKOEDYMD
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    HKTSRYCD-REC        TO  MCPDATA-REC
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKTSRYCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-HKTSRYCD   =   1  )
                       OR  (IDX            >=  100)
               IF      HKTSRYCD-RENNUM     =   1
                   ADD     1                   TO  IDX
                   MOVE    HKTSRYCD-KANRICD    TO  SPA-GMN-TKBNCD
                                                             (IDX)
                   MOVE    HKTSRYCD-YUKOSTYMD  TO  SPA-NAIB-YUKOSTYMD
                                                             (IDX)
                   MOVE    HKTSRYCD-YUKOEDYMD  TO  SPA-NAIB-YUKOEDYMD
                                                             (IDX)
                   PERFORM 3101-KBNCDMEI-HEN-SEC
                   MOVE    IDX                 TO  SPA-KBNCD-MAX
               END-IF
               MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-HKTSRYCD-READ-SEC
           END-PERFORM
           MOVE    "tbl_hktsrycd"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    包括算定方法
           MOVE    "0 包括算定しない"
                                       TO  SPA-GMN-SANTEIHOU-LST (1)
           MOVE    "1 包括算定する（会計データ作成）"
                                       TO  SPA-GMN-SANTEIHOU-LST (2)
           MOVE    "2 包括算定する（エラー扱い）"
                                       TO  SPA-GMN-SANTEIHOU-LST (3)
           MOVE    3                   TO  SPA-SANTEIHOU-MAX
      *    包括算定区分
           MOVE    "1 会計内において包括する"
                                       TO  SPA-GMN-SANTEIKBN-LST (1)
           MOVE    "2 算定日において包括する"
                                       TO  SPA-GMN-SANTEIKBN-LST (2)
           MOVE    "3 算定月において包括する"
                                       TO  SPA-GMN-SANTEIKBN-LST (3)
           MOVE    "4 算定日以降において包括する"
                                       TO  SPA-GMN-SANTEIKBN-LST (4)
           MOVE    4                   TO  SPA-SANTEIKBN-MAX
      *    入外区分
           MOVE    "1 入院・外来別に包括する" 
                                       TO  SPA-GMN-NYUGAIKBN-LST(1) 
           MOVE    "2 入院・外来に関係なく包括する" 
                                       TO  SPA-GMN-NYUGAIKBN-LST(2) 
           MOVE    2                   TO  SPA-NYUGAIKBN-MAX
      *    保険組合区分
           MOVE    "0 保険組合せに関係なく包括する" 
                                       TO  SPA-GMN-HKNCOMBIKBN-LST(1) 
           MOVE    "1 労災・自賠責・健保毎に包括する" 
                                       TO  SPA-GMN-HKNCOMBIKBN-LST(2) 
           MOVE    2                   TO  SPA-HKNCOMBIKBN-MAX
      *    診療区分画面
           MOVE    12                  TO   SPA-GMN-SRYKBN-MAX
      *    診療区分（共通）
           MOVE    "0 包括しない"      TO  SPA-GMN-SRYKBN-LST (1)
           MOVE    "1 包括する"        TO  SPA-GMN-SRYKBN-LST (2)
           MOVE    2                   TO  SPA-SRYKBN-MAX
      *
      *    初期表示
           MOVE    SPA-GMN-KBNCD-LST(1)   TO  SPA-GMN-KBNCD-G
           MOVE    SPA-NAIB-YUKOSTYMD (1) TO  SPA-HKT-YUKOSTYMD
           MOVE    SPA-NAIB-YUKOEDYMD (1) TO  SPA-HKT-YUKOEDYMD
           IF      SPA-KBNCD-MAX       =   1
               PERFORM 320-KANRICD-HENSYU-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括内容編集処理
      *****************************************************************
       3101-KBNCDMEI-HEN-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-TKBNCD (IDX)
               WHEN    "001"
                   MOVE    "外来診療料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "501"
                   MOVE    "療養病棟入院基本料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "502"
      ****         MOVE    "後期高齢者特定入院基本料" 
                   MOVE    "特定入院基本料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "503"
                   MOVE    "有床診療所療養病床入院基本料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
      *
               WHEN    "201"
                   MOVE    "慢性維持透析患者外来医学管理料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "202"
                   MOVE    "小児科外来診察料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
               WHEN    "203"
                   MOVE    "生活習慣病管理料" 
                                            TO  SPA-GMN-TKBNCD-MEI(IDX)
           END-EVALUATE
           . 
       3101-KBNCDMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括算定初期表示編集処理
      *****************************************************************
       3101-INIT-GMN-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-SYORI
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       3101-INIT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括管理番号の編集処理 
      *****************************************************************
       320-KANRICD-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-NAIB-AREA
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD (1:3)
           MOVE    "%"                 TO  SYS-1014-KBNCD (4:1)
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM  VARYING    IDX     FROM    1   BY  1
               UNTIL          (IDX     >     100   )  OR
                              (FLG-SYSKANRI    =   1 )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               ADD     1                   TO  SPA-GMN-TBL-MAX
               MOVE    IDX                 TO  SPA-GMN-TBANGO    (IDX)
               MOVE    SYS-1014-STYUKYMD   TO  SPA-NAI-TSTYUKYMD (IDX)
               MOVE    SYS-1014-STYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD (IDX)
               MOVE    SYS-1014-EDYUKYMD   TO  SPA-NAI-TEDYUKYMD (IDX)
               MOVE    SYS-1014-EDYUKYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    管理番号により包括選択の決定
           PERFORM 3201-INIT-TBL-SEC
           IF      SPA-GMN-TBL-MAX     =   ZERO
      *        新規
               MOVE    1                   TO  SPA-GMN-SYORI
               MOVE    3                   TO  SPA-GMN-CUR
               PERFORM 200212-INIT-GMN-SEC
           ELSE
               MOVE    2                   TO  SPA-GMN-SYORI
               IF      SPA-GMN-TBL-MAX     =   1
      *            １件を選択とする
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   MOVE    SPA-GMN-SELNUM      TO  W32-SELNUM
                   PERFORM 2003-SELNUM-CHK-SEC
               ELSE
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           . 
       320-KANRICD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理番号により包括選択の決定処理
      *****************************************************************
       3201-INIT-TBL-SEC              SECTION.
      *
      *    EVALUATE    SPA-GMN-KBNCD
      *        WHEN    "001"
      *            外来診療料 すべてを包括しない
      *            MOVE    1               TO  SPA-SANTEIKBN-MAX
      *            MOVE    1               TO  SPA-NYUGAIKBN-MAX
      *            MOVE    1               TO  SPA-HKNCOMBIKBN-MAX
      *            MOVE    1               TO  SPA-SRYKBN-MAX
      *        WHEN    OTHER
      *            MOVE    4               TO  SPA-SANTEIKBN-MAX
      *            MOVE    2               TO  SPA-NYUGAIKBN-MAX
      *            MOVE    2               TO  SPA-HKNCOMBIKBN-MAX
      *            MOVE    2               TO  SPA-SRYKBN-MAX
      *    END-EVALUATE
      *
           . 
       3201-INIT-TBL-EXT.
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
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"      ALSO    "B02"
                   PERFORM  402-CLERA-SEC
      *    削除
               WHEN    "CLICKED"      ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"      ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
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
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        包括管理番号
               WHEN    "ACTIVATE"      ALSO    "KBNCD"
                   PERFORM 2001-KBNCD-SYORI-SEC
      *        職員番号入力
      *        番号選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 2003-SELNUM-CHK-SEC
      *        明細選択
               WHEN    ANY             ALSO    "KIKAN_LIST"
                   PERFORM 2004-KIKANLIST-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括管理番号入力処理
      *****************************************************************
       2001-KBNCD-SYORI-SEC         SECTION.
      *
           MOVE    SPA-GMN-KBNCD-G     TO  WRK-MAE-KBNCD-G
      *
           MOVE    W32-KBNCD           TO  SPA-GMN-KBNCD-G
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KBNCD-MAX
               IF      SPA-GMN-KBNCD       =   SPA-GMN-TKBNCD(IDX)
                   MOVE    SPA-GMN-KBNCD-LST (IDX)
                                           TO  SPA-GMN-KBNCD-G
                   MOVE    SPA-NAIB-YUKOSTYMD(IDX)
                                           TO  SPA-HKT-YUKOSTYMD
                   MOVE    SPA-NAIB-YUKOEDYMD (IDX)
                                           TO  SPA-HKT-YUKOEDYMD
                   MOVE    SPA-KBNCD-MAX   TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               INITIALIZE                      SPA-GMN-AREA2
               INITIALIZE                      SPA-NAIB-AREA
      *        包括管理番号編集
               PERFORM 320-KANRICD-HENSYU-SEC
           END-IF
      *
           .
       2001-KBNCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規初期表示処理
      *****************************************************************
       200212-INIT-GMN-SEC       SECTION.
      *
      *    包括算定方法
           MOVE    SPA-GMN-SANTEIHOU-LST (1)   TO  SPA-GMN-SANTEIHOU-G
      *    包括算定区分以下は区分毎の初期表示
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            外来診療料
                   PERFORM 200211-KBNCD-001-SEC
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
      *            療養病棟入院料
                   PERFORM 200211-KBNCD-501-SEC
               WHEN    "201"
      *            慢性維持透析患者管理料
                   PERFORM 200211-KBNCD-201-SEC
               WHEN    "202"
      *            小児科外来診察料
                   PERFORM 200211-KBNCD-202-SEC
               WHEN    "203"
      *            生活習慣病管理料
                   PERFORM 200211-KBNCD-203-SEC
           END-EVALUATE
      *    包括データの有効期間
           MOVE    SPA-HKT-YUKOSTYMD   TO  SPA-NAI-STYUKYMD
           MOVE    SPA-HKT-YUKOEDYMD   TO  SPA-NAI-EDYUKYMD
      *
           MOVE    SPA-NAI-STYUKYMD    TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
      *
           .
       200212-INIT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    外来診療料処理
      *****************************************************************
       200211-KBNCD-001-SEC       SECTION.
      *
           MOVE    SPA-GMN-SANTEIKBN-LST (1)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                       TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-001-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟入院料処理
      *****************************************************************
       200211-KBNCD-501-SEC       SECTION.
      *
      *    包括算定区分
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =   1
                                       OR  2
                                       OR  3
                                       OR  5
                                       OR  10
      *            投薬・注射・処置・検査・病理診断が包括
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-501-EXT.
           EXIT.
      *****************************************************************
      *    慢性維持透析患者外来医学管理処理
      *****************************************************************
       200211-KBNCD-201-SEC       SECTION.
      *
      *    包括算定区分
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-201-EXT.
           EXIT.
      *****************************************************************
      *    小児科外来診察料処理
      *****************************************************************
       200211-KBNCD-202-SEC       SECTION.
      *
      *    包括算定区分
           MOVE    SPA-GMN-SANTEIKBN-LST (2)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
      *        包括
               MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
           END-PERFORM
      *
           .
       200211-KBNCD-202-EXT.
           EXIT.
      *****************************************************************
      *    生活習慣病管理処理
      *****************************************************************
       200211-KBNCD-203-SEC       SECTION.
      *
      *    包括算定区分
           MOVE    SPA-GMN-SANTEIKBN-LST (3)   TO  SPA-GMN-SANTEIKBN-G
           MOVE    SPA-GMN-NYUGAIKBN-LST (1)   TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-HKNCOMBIKBN-LST(1)  TO  SPA-GMN-HKNCOMBIKBN-G
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKBN-MAX
               IF      IDX             =    1
                                       OR   2
                                       OR   5
                                       OR   10
      *            投薬・注射・検査・病理診断が包括
                   MOVE    SPA-GMN-SRYKBN-LST (2)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               ELSE
                   MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-IF
           END-PERFORM
      *
           .
       200211-KBNCD-203-EXT.
           EXIT.
      *****************************************************************
      *    番号選択チェック処理
      *****************************************************************
       2003-SELNUM-CHK-SEC       SECTION.
      *
           MOVE    W32-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF     (SPA-GMN-SELNUM      >=  1   )  AND
                  (SPA-GMN-SELNUM      <=  SPA-GMN-TBL-MAX)
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    SPA-GMN-TSTYUKYMD(IDX)  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-GMN-TEDYUKYMD(IDX)  TO  SPA-GMN-EDYUKYMD
      *
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-MAE-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-MAE-EDYUKYMD
      *        包括算定編集
               PERFORM 2009-HOUKATU-HENSYU-SEC
               MOVE    6               TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-SELNUM      =   ZERO
      *            選択番号が無い時、新規追加とする
                   MOVE    SPACE           TO  SPA-MAE-STYUKYMD
                   MOVE    SPACE           TO  SPA-MAE-EDYUKYMD
                   MOVE    1               TO  SPA-GMN-SYORI
                   MOVE    3               TO  SPA-GMN-CUR
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       2003-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間リスト選択処理
      *****************************************************************
       2004-KIKANLIST-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               IF      W32-KIKAN-SELECT(IDX)   =   "T"
                   IF      IDX                 =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-TBL-MAX     TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
      *
               MOVE    SPA-GMN-SELNUM      TO  W32-SELNUM
               PERFORM 2003-SELNUM-CHK-SEC
           END-IF
           .
        2004-KIKANLIST-EXT.
           EXIT.
      *****************************************************************
      *    包括算定編集処理
      *****************************************************************
       2009-HOUKATU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1014-STYUKYMD 
           MOVE    SPA-NAI-STYUKYMD    TO  SYS-1014-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1014-REC
               MOVE    SYS-1014-SANTEI-HOU     TO  SPA-GMN-SANTEIHOU
               MOVE    SYS-1014-SANTEI-KBN     TO  SPA-GMN-SANTEIKBN
               MOVE    SYS-1014-NYUGAIKBN      TO  SPA-GMN-NYUGAIKBN
               MOVE    SYS-1014-HKNCOMBI-KBN   TO  SPA-GMN-HKNCOMBIKBN
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SRYKBN-MAX
                   MOVE    SYS-1014-SRYKBN-KBN (IDX)
                                           TO  SPA-GMN-SRYKBN (IDX)
                   IF      SYS-1014-SRYKBN-KBN (IDX)   =   SPACE
                       MOVE    "0"         TO  SPA-GMN-SRYKBN (IDX)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-SANTEIHOU-MAX
                   IF      SPA-GMN-SANTEIHOU-LST (IDX)(1:1)
                                           =   SPA-GMN-SANTEIHOU
                       MOVE    SPA-GMN-SANTEIHOU-LST (IDX)
                                           TO  SPA-GMN-SANTEIHOU-G
                       MOVE    SPA-SANTEIHOU-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-SANTEIKBN-MAX
                   IF      SPA-GMN-SANTEIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-SANTEIKBN
                       MOVE    SPA-GMN-SANTEIKBN-LST (IDX)
                                           TO  SPA-GMN-SANTEIKBN-G
                       MOVE    SPA-SANTEIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-NYUGAIKBN-MAX
                   IF      SPA-GMN-NYUGAIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-NYUGAIKBN
                       MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                       MOVE    SPA-NYUGAIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-HKNCOMBIKBN-MAX
                   IF      SPA-GMN-HKNCOMBIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-HKNCOMBIKBN
                       MOVE    SPA-GMN-HKNCOMBIKBN-LST (IDX)
                                           TO  SPA-GMN-HKNCOMBIKBN-G
                       MOVE    SPA-HKNCOMBIKBN-MAX   TO  IDX
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   SPA-GMN-SRYKBN-MAX
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >   SPA-SRYKBN-MAX
                       IF      SPA-GMN-SRYKBN-LST (IDY)(1:1)
                                           =   SPA-GMN-SRYKBN (IDX)
                           MOVE    SPA-GMN-SRYKBN-LST (IDY)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                           MOVE    SPA-SRYKBN-MAX   TO  IDY
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           . 
       2009-HOUKATU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
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
           MOVE    W32-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W32-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *
           MOVE    W32-SANTEIHOU       TO  SPA-GMN-SANTEIHOU-G
           MOVE    W32-SANTEIKBN       TO  SPA-GMN-SANTEIKBN-G
           MOVE    W32-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
           MOVE    W32-HKNCOMBIKBN     TO  SPA-GMN-HKNCOMBIKBN-G
      *    診療区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    W32-SRYKBN(IDX)     TO  SPA-GMN-SRYKBN-G(IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    管理番号チェック
           IF     (SPA-GMN-KBNCD       =   SPACE ) OR
                  (SPA-GMN-KBNCD       NOT NUMERIC )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-STYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41021-EDYUKYMD-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-SANTEIHOU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4112-SANTEIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4113-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4114-HKNCOMBIKBN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4115-SRYKBN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効開始日チェック処理
      *****************************************************************
       41021-STYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-STYUKYMD
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "STYUKYMD"
                   MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-GMN-STYUKYMD
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
        41021-STYUKYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効終了日チェック処理
      *****************************************************************
       41021-EDYUKYMD-CHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  SPA-NAI-EDYUKYMD
      *
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               IF      WRK-MCP-WIDGET     =   "EDYUKYMD"
                   MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
               END-IF
           END-IF
      *
           IF      SPA-GMN-EDYUKYMD    NOT =   SPACE
               MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD 
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-GMN-STYUKYMD    NOT =   SPACE ) AND
                  (SPA-GMN-EDYUKYMD    NOT =   SPACE )
               IF      SPA-NAI-STYUKYMD    >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    4                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
        41021-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括算定方法チェック
      *****************************************************************
       4100-SANTEIHOU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SANTEIHOU-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-SANTEIHOU   =   SPA-GMN-SANTEIHOU-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIHOU-LST (IDX)
                                           TO  SPA-GMN-SANTEIHOU-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
       4100-SANTEIHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    包括算定区分チェック
      *****************************************************************
       4112-SANTEIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            外来診療料
                   MOVE    SPA-GMN-SANTEIKBN-LST (1)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
      *            療養病棟入院料 算定日で包括
                   MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "201"
      *            慢性維持透析管理料　算定月で包括
                   MOVE    SPA-GMN-SANTEIKBN-LST (3)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "202"
      *            小児科外来診察料　算定日で包括
                   MOVE    SPA-GMN-SANTEIKBN-LST (2)
                                               TO  SPA-GMN-SANTEIKBN-G
               WHEN    "203"
      *            生活習慣病管理料　算定月で包括
                   MOVE    SPA-GMN-SANTEIKBN-LST (3)
                                               TO  SPA-GMN-SANTEIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-SANTEIKBN-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-SANTEIKBN   =   SPA-GMN-SANTEIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIKBN-LST (IDX)
                                           TO  SPA-GMN-SANTEIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
           .
       4112-SANTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       4113-NYUGAIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            外来診療料
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
      *            療養病棟入院料
               WHEN    "201"
      *            慢性維持透析管理料
               WHEN    "202"
      *            小児科外来診察料
               WHEN    "203"
      *            生活習慣病管理料
                   MOVE    SPA-GMN-NYUGAIKBN-LST (1)
                                               TO  SPA-GMN-NYUGAIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NYUGAIKBN-MAX )   OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
           .
       4113-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック
      *****************************************************************
       4114-HKNCOMBIKBN-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-KBNCD
               WHEN    "001"
      *            外来診療料
               WHEN    "501"
               WHEN    "502"
               WHEN    "503"
      *            療養病棟入院料
               WHEN    "201"
      *            慢性維持透析管理料
               WHEN    "202"
      *            小児科外来診察料
               WHEN    "203"
      *            生活習慣病管理料
                   MOVE    SPA-GMN-HKNCOMBIKBN-LST (1)
                                               TO  SPA-GMN-HKNCOMBIKBN-G
           END-EVALUATE
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKNCOMBIKBN-MAX ) OR
                              (FLG-CHK     =   1    )
               IF      SPA-GMN-HKNCOMBIKBN   =   SPA-GMN-HKNCOMBIKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-HKNCOMBIKBN-LST (IDX)
                                           TO  SPA-GMN-HKNCOMBIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
           .
       4114-HKNCOMBIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療区分せチェック
      *****************************************************************
       4115-SRYKBN-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SRYKBN-MAX ) OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-GMN-KBNCD
                   WHEN    "001"
      *            外来診療料
                       MOVE    SPA-GMN-SRYKBN-LST (1)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
               END-EVALUATE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   SPA-SRYKBN-MAX ) OR
                                  (FLG-CHK     =   1    )
                   IF      SPA-GMN-SRYKBN (IDX)
                                           =   SPA-GMN-SRYKBN-LST
                                                        (IDY)(1:1)
                       MOVE    SPA-GMN-SRYKBN-LST (IDY)
                                           TO  SPA-GMN-SRYKBN-G (IDX)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
                   COMPUTE SPA-GMN-CUR         =   IDX
                                               +   20
               END-IF
           END-PERFORM
           .
       4115-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W32"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    クリア処理
      *****************************************************************
       402-CLERA-SEC                               SECTION.
      *
      *    初期表示
           PERFORM 3101-INIT-GMN-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       402-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    "1003"              TO  SPA-WIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"                  TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD           TO  SYS-1014-KBNCD
           MOVE    SPA-MAE-STYUKYMD        TO  SYS-1014-STYUKYMD
           MOVE    SPA-MAE-EDYUKYMD        TO  SYS-1014-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "W32 DB DEL ERR SYSKANRI-KEY =>" SYS-1014-KEY 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力項目のチェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
              MOVE    "1002"           TO  SPA-WIDCD
      *       有効開始日の変更は追加
              IF     (SPA-GMN-SELNUM       =   ZERO )  OR
                     (SPA-GMN-SYORI        =   1    )
                  MOVE    "1001"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *    管理コード
           IF     (SPA-GMN-KBNCD       =   SPACE )  OR
                  (SPA-GMN-KBNCD       NOT NUMERIC)
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-KBNCD-MAX) OR
                                  (FLG-OK      =   1    )
                   IF      SPA-GMN-KBNCD    =   SPA-GMN-TKBNCD
                                                         (IDX)
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *        入力済みの時、選択があること
      *        IF      FLG-OK          =   1
      *            IF      SPA-GMN-TBL-MAX =   ZERO
      *                MOVE    "0021"              TO  SPA-ERRCD
      *                MOVE    2                   TO  SPA-GMN-CUR
      *            END-IF
      *        END-IF
           END-IF
      *
           IF      SPA-GMN-STYUKYMD   =   SPACE
               MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-GMN-STYUKYMD
               MOVE    SPA-HKT-YUKOSTYMD  TO  SPA-NAI-STYUKYMD
           END-IF
           IF      SPA-GMN-EDYUKYMD   =   SPACE
               MOVE    SPA-HKT-YUKOEDYMD  TO  SPA-GMN-EDYUKYMD
               MOVE    SPA-HKT-YUKOEDYMD  TO  SPA-NAI-EDYUKYMD
           END-IF
           IF      SPA-ERRCD          =   SPACE
               IF      SPA-NAI-STYUKYMD   >   SPA-NAI-EDYUKYMD
                   MOVE    "0005"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    包括診療コードの期間内であること
           IF      SPA-ERRCD          =   SPACE
               IF     (SPA-NAI-STYUKYMD   <   SPA-HKT-YUKOSTYMD)
                   OR (SPA-NAI-EDYUKYMD   >   SPA-HKT-YUKOEDYMD )
                   MOVE    "0021"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    期間重複チェック
           IF     (SPA-GMN-SELNUM      =   ZERO  )  OR
                  (SPA-NAI-STYUKYMD    NOT =   SPA-MAE-STYUKYMD)
               MOVE    1               TO  SPA-GMN-SYORI
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN-TBL-MAX )
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   IF      (SPA-NAI-STYUKYMD    >=  SPA-NAI-TSTYUKYMD
                                                               (IDX))
                     AND   (SPA-NAI-STYUKYMD    <=  SPA-NAI-TEDYUKYMD
                                                               (IDX))
                      MOVE    "0020"           TO  SPA-ERRCD
                      MOVE    3                TO  SPA-GMN-CUR
                   END-IF
                   IF      (SPA-NAI-EDYUKYMD    >=  SPA-NAI-TSTYUKYMD
                                                               (IDX))
                     AND  (SPA-NAI-EDYUKYMD    <=  SPA-NAI-TEDYUKYMD
                                                               (IDX))
                      MOVE    "0020"           TO  SPA-ERRCD
                      MOVE    3                TO  SPA-GMN-CUR
                   END-IF
                   IF     (SPA-ERRCD       NOT =   SPACE) AND
                          (SPA-GMN-SELNUM      >   ZERO ) AND
                          (IDX                 =   SPA-GMN-SELNUM)
                       MOVE    2               TO  SPA-GMN-SYORI
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                      SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-GMN-KBNCD       TO  SYS-1014-KBNCD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1014-STYUKYMD
           MOVE    SPA-MAE-STYUKYMD    TO  SYS-1014-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-1014-REC
           ELSE
               MOVE    SPACE                   TO  SYS-1014-REC
               INITIALIZE                      SYS-1014-REC
               MOVE   "1014"                   TO  SYS-1014-KANRICD
               MOVE    SPA-GMN-KBNCD           TO  SYS-1014-KBNCD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD        TO  SYS-1014-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD        TO  SYS-1014-EDYUKYMD
           MOVE    SPA-GMN-SANTEIHOU       TO  SYS-1014-SANTEI-HOU 
           MOVE    SPA-GMN-SANTEIKBN       TO  SYS-1014-SANTEI-KBN 
           MOVE    SPA-GMN-NYUGAIKBN       TO  SYS-1014-NYUGAIKBN 
           MOVE    SPA-GMN-HKNCOMBIKBN     TO  SYS-1014-HKNCOMBI-KBN
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN (IDX)
                                           TO  SYS-1014-SRYKBN-KBN(IDX)
           END-PERFORM
      *
           IF     (FLG-SYSKANRI        =   ZERO ) AND
                  (SPA-GMN-SYORI       =   2    )
      *        更新
               MOVE    SMCNDATE-YMD        TO  SYS-1014-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1014-UPHMS
               MOVE    SYS-1014-REC        TO  SYSKANRI-REC
      *
               MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
               MOVE    SPA-MAE-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-MAE-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
      *
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W32 DB UPD ERR KEY:" SYS-1014-KEY 
               END-IF
           ELSE
      *        追加
               MOVE    SMCNDATE-YMD        TO  SYS-1014-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1014-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1014-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
               MOVE    SYS-1014-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W32 DB INS ERR KEY:" SYS-1014-KEY 
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        初期表示
               PERFORM 3101-INIT-GMN-SEC
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W32    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  W32
           INITIALIZE                      W32
      *
           MOVE    SPA-GMN-KBNCD-G     TO  W32-KBNCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KBNCD-MAX
               MOVE    SPA-GMN-KBNCD-LST (IDX)
                                           TO  W32-KBNCD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KBNCD-MAX       TO  W32-KBNCD-COUNT
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W32-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W32-EDYUKYMD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-TBL-MAX
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  W32-TNUM (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX)
                                           TO  W32-TSTYUKYMD (IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX)
                                           TO  W32-TEDYUKYMD (IDX)
           END-PERFORM
           MOVE    SPA-GMN-TBL-MAX         TO  W32-KIKAN-COUNT
           MOVE    SPA-GMN-SELNUM          TO  W32-SELNUM
      *
           MOVE    SPA-GMN-SANTEIHOU-G     TO  W32-SANTEIHOU
           MOVE    SPA-GMN-SANTEIKBN-G     TO  W32-SANTEIKBN
           MOVE    SPA-GMN-NYUGAIKBN-G     TO  W32-NYUGAIKBN
           MOVE    SPA-GMN-HKNCOMBIKBN-G   TO  W32-HKNCOMBIKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIHOU-MAX
               MOVE    SPA-GMN-SANTEIHOU-LST(IDX)
                                       TO  W32-SANTEIHOU-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SANTEIHOU-MAX     TO  W32-SANTEIHOU-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIKBN-MAX
               MOVE    SPA-GMN-SANTEIKBN-LST(IDX)
                                       TO  W32-SANTEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-SANTEIKBN-MAX     TO  W32-SANTEIKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAIKBN-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST(IDX)
                                       TO  W32-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-NYUGAIKBN-MAX     TO  W32-NYUGAIKBN-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBIKBN-MAX
               MOVE    SPA-GMN-HKNCOMBIKBN-LST(IDX)
                                       TO  W32-HKNCOMBIKBN-ITEM(IDX)
           END-PERFORM
           MOVE  SPA-HKNCOMBIKBN-MAX   TO  W32-HKNCOMBIKBN-COUNT
      *    診療区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-G (IDX)  TO  W32-SRYKBN (IDX)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYKBN-MAX
                   MOVE    SPA-GMN-SRYKBN-LST (IDY) 
                                           TO  W32-SRYKBN-ITEM
                                                        (IDX IDY)
               END-PERFORM
               MOVE    SPA-SRYKBN-MAX       TO  W32-SRYKBN-COUNT(IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           IF     (SPA-GMN-SYORI       =   1   ) AND
                  (SPA-GMN-SELNUM      =   ZERO)
               MOVE    WIDGET-INSENSITIVE  TO  W32-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W32-B03-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
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
                   MOVE    "包括管理区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "有効開始日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "有効終了日入力エラ−"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "選択番号がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "包括算定方法がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "包括算定区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "入外区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "保険組合せ区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "診療区分コードがありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   STRING  "包括診療コードの期間外です。"
                           "期間を変更して下さい。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W32"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
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
           MOVE    SPACE                TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "包括診療行為設定を追加登録します"
                                                     TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "包括診療行為設定を更新します"
                                                     TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "包括診療行為設定を削除します" 
                                                     TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W32"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    1
                    MOVE  "KBNCD"          TO   MCP-WIDGET
                WHEN    3
                    MOVE  "STYUKYMD"       TO   MCP-WIDGET
                WHEN    4
                    MOVE  "EDYUKYMD"       TO   MCP-WIDGET
                WHEN    5
                    MOVE  "SELNUM"         TO   MCP-WIDGET
                WHEN    6
                    MOVE  "SANTEIHOU"      TO   MCP-WIDGET
                WHEN    7
                    MOVE  "SANTEIKBN"      TO   MCP-WIDGET
                WHEN    8
                    MOVE  "NYUGAIKBN"      TO   MCP-WIDGET
                WHEN    9
                    MOVE  "HKNCOMBIKBN"    TO   MCP-WIDGET
      *         診療区分
                WHEN    21   THRU  33
                    MOVE     SPA-GMN-CUR       TO  IDX2
                    MOVE     "SRYKBN"          TO  MCP-WIDGET
                    COMPUTE  IDX2              =   IDX2   -   20
                    MOVE     IDX2              TO  MCP-WIDGET(7:2)
      *
                WHEN    99
                    MOVE  "B12"            TO   MCP-WIDGET
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
               WHEN    "KBNCD"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "STYUKYMD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "EDYUKYMD"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SANTEIHOU"
                   MOVE    99              TO  SPA-GMN-CUR
               WHEN    "SANTEIKBN"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "HKNCOMBIKBN"
                   MOVE    21              TO  SPA-GMN-CUR
      *
               WHEN     OTHER
                   IF      WRK-MCP-WIDGET(1:6) =   "SRYKBN"
                       MOVE    WRK-MCP-WIDGET(7:2) TO  IDX2
                       COMPUTE SPA-GMN-CUR     =   IDX2
                                               +   21
      *                更新へ
                       IF      IDX2            =   12
                           MOVE    99          TO  SPA-GMN-CUR
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
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
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-HKTSRYCD-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-HKTSRYCD
               MOVE    MCPDATA-REC     TO  HKTSRYCD-REC
           ELSE
               INITIALIZE                  HKTSRYCD-REC
               MOVE    1               TO  FLG-HKTSRYCD
           END-IF
      *
           .
       900-HKTSRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       990-DBCLOSE-SEC           SECTION.
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
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
