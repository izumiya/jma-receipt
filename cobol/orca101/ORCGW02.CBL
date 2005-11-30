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
       PROGRAM-ID.             ORCGW02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 医療機関情報（Ｗ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.06.18  院外処方せん発行フラグ等追加
      *  01.01.02    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.03    MCC-藤原     01/10/30  項目をコンボボックスに変更
      *  01.00.04    MCC-藤原     01/11/08  削除処理の修正 
      *  01.00.05    MCC-多々納   02/02/12  エラーメッセージの修正
      *  01.00.06    MCC-多々納   02/02/21  院外処方せんフラグの変更
      *  01.00.07    MCC-多々納   02/06/13  削除を非活性にする
      *  01.00.08    MCC-伊藤     02/07/17  医療機関ＩＤは入力不可とする
      *                                     ためカーソル制御を変更
      *  01.00.09    NACL-森脇    02.08.16  薬剤情報発行フラグ追加
      *  01.00.10    NACL-多々納  02/11/12  一括入力、全角チェック追加
      *  01.00.11    NACL-多々納  03/01/10  病床数（一般）追加
      *  01.00.12    NACL-太田    03/05/21  減免端数区分追加
      *  01.00.13    NACL-多々納  03/05/27  診療内容追加（１０１２）
      *  01.00.14    NACL-太田    03/06/06  消費税端数区分追加
      *  01.00.15    NACL-太田    03/07/09  割引率区分・定額定率区分の
      *                                     入力をコンボボックスに変更
      *  01.00.16    NACL-太田    03/07/31  減免事由情報に割引対象を追加
      *  01.00.17    NACL-多々納  03/11/11  基本情報期間変更追加
      *  01.00.18    NACL-多々納  04/05/11  減免事由の割引対象を９９件へ
      *  01.00.19    NACL-太田    04/07/05  請求書発行コンボ項目追加
      *  01.00.20    NACL-太田    04/07/07  入金方法情報タブ追加
      *  01.00.21    NACL-太田    04/09/30  請求額端数区分（減免無）追加
      *  01.00.22    NACL-太田    04/12/01  更正・育成限度額日割計算区分追加
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
       01  SPA-W02.
           03  SPA-W02-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).
                   07  SPA-GMN-PAGENO         PIC 9(01).
                   07  SPA-GMN-1001-AREA.
                       09  SPA-GMN-PREFNUMLST         OCCURS  47.
                           11  SPA-GMN-PREFNUML       PIC X(02).
                           11  SPA-GMN-F1L            PIC X(01).
                           11  SPA-GMN-PREFNUMMEIL    PIC X(10).
                       09  SPA-GMN-TENHYOKBNLST       OCCURS  2.
                           11  SPA-GMN-TENHYOKBNL     PIC X(01).
                           11  SPA-GMN-F2L            PIC X(01).
                           11  SPA-GMN-TENHYOKBNMEIL  PIC X(06).
                       09  SPA-GMN-HOSPSBTLST         OCCURS  2.
                           11  SPA-GMN-HOSPSBTL       PIC X(01).
                           11  SPA-GMN-F3L            PIC X(01).
                           11  SPA-GMN-HOSPSBTMEIL    PIC X(08).
                       09  SPA-GMN-ROUPAYKBNLST       OCCURS  2.
                           11  SPA-GMN-ROUPAYKBNL     PIC X(01).
                           11  SPA-GMN-F4L            PIC X(01).
                           11  SPA-GMN-ROUPAYKBNMEIL  PIC X(06).
                       09  SPA-GMN-HOSPSBT1LST        OCCURS  2.
                           11  SPA-GMN-HOSPSBT1L      PIC X(01).
                           11  SPA-GMN-F5L            PIC X(01).
                           11  SPA-GMN-HOSPSBT1MEIL   PIC X(18).
                       09  SPA-GMN-SYOHOKBNLST        OCCURS  2.
                           11  SPA-GMN-SYOHOKBNL      PIC X(01).
                           11  SPA-GMN-F6L            PIC X(01).
                           11  SPA-GMN-SYOHOKBNMEIL   PIC X(06).
                       09  SPA-GMN-SKYPRTFLGLST       OCCURS  3.
                           11  SPA-GMN-SKYPRTFLGL     PIC X(01).
                           11  SPA-GMN-F7L            PIC X(01).
                           11  SPA-GMN-SKYPRTFLGMEIL  PIC X(20).
                       09  SPA-GMN-SYOHOPRTFLGLST     OCCURS  3.
                           11  SPA-GMN-SYOHOPRTFLGL   PIC X(01).
                           11  SPA-GMN-F8L            PIC X(01).
                           11  SPA-GMN-SYOHOPRTFLGMEIL
                                                      PIC X(12).
                       09  SPA-GMN-ZENSYOHOFLGLST     OCCURS  2.
                           11  SPA-GMN-ZENSYOHOFLGL   PIC X(01).
                           11  SPA-GMN-F9L            PIC X(01).
                           11  SPA-GMN-ZENSYOHOFLGMEIL
                                                      PIC X(12).
      *----(01.00.09) LINE ADD START ----------------------------------
                       09  SPA-GMN-YAKUJYOFLGLST      OCCURS  2.
                           11  SPA-GMN-YAKUJYOFLGL    PIC X(01).
                           11  SPA-GMN-F10L           PIC X(01).
                           11  SPA-GMN-YAKUJYOFLGMEIL
                                                      PIC X(12).
      *----(01.00.09) LINE ADD END   ----------------------------------
                       09  SPA-GMN-DISCOUNT-HASU-KBNLST
                                                      OCCURS  4.
                           11  SPA-GMN-DISCOUNT-HASU-KBNL
                                                      PIC X(01).
                           11  SPA-GMN-F11L           PIC X(01).
                           11  SPA-GMN-DISCOUNT-HASU-KBNMEIL
                                                      PIC X(30).
                       09  SPA-GMN-DISCOUNT-HASU-KBN2LST
                                                      OCCURS  4.
                           11  SPA-GMN-DISCOUNT-HASU-KBN2L
                                                      PIC X(01).
                           11                         PIC X(01).
                           11  SPA-GMN-DISCOUNT-HASU-KBN2MEIL
                                                      PIC X(30).
                       09  SPA-GMN-TAX-HASU-KBNLST
                                                      OCCURS  4.
                           11  SPA-GMN-TAX-HASU-KBNL
                                                      PIC X(01).
                           11  SPA-GMN-F12L           PIC X(01).
                           11  SPA-GMN-TAX-HASU-KBNMEIL
                                                      PIC X(30).
                       09  SPA-GMN-HIWARIKBNLST
                                                      OCCURS  2.
                           11  SPA-GMN-HIWARIKBNL
                                                      PIC X(01).
                           11  SPA-GMN-F13L           PIC X(01).
                           11  SPA-GMN-HIWARIKBNMEIL
                                                      PIC X(18).
                       09  SPA-GMN-KOUMOKU.
                           11  SPA-GMN-PREFNUM-G.
                               13  SPA-GMN-PREFNUM    PIC X(02).
                               13  SPA-GMN-F1         PIC X(01).
                               13  SPA-GMN-PREFNUMMEI PIC X(10).
                           11  SPA-GMN-TENHYOKBN-G.
                               13  SPA-GMN-TENHYOKBN  PIC X(01).
                               13  SPA-GMN-F2         PIC X(01).
                               13  SPA-GMN-TENHYOKBNMEI
                                                      PIC X(06).
                           11  SPA-GMN-HOSPCD         PIC 9(07).
                           11  SPA-GMN-HOSPSBT-G.
                               13  SPA-GMN-HOSPSBT    PIC X(01).
                               13  SPA-GMN-F3         PIC X(01).
                               13  SPA-GMN-HOSPSBTMEI PIC X(08).
                           11  SPA-GMN-HOSPID         PIC X(24).
                           11  SPA-GMN-HOSPNAME       PIC X(120).
                           11  SPA-GMN-KAISETUNAME    PIC X(120).
                           11  SPA-GMN-KANRINAME      PIC X(80).
                           11  SPA-GMN-BEDSU          PIC 9(04).
                           11  SPA-GMN-BEDSUIPN       PIC 9(04).
                           11  SPA-GMN-ROUPAYKBN-G.
                               13  SPA-GMN-ROUPAYKBN  PIC X(01).
                               13  SPA-GMN-F4         PIC X(01).
                               13  SPA-GMN-ROUPAYKBNMEI
                                                      PIC X(06).
                           11  SPA-GMN-HOSPSBT1-G.
                               13  SPA-GMN-HOSPSBT1   PIC X(01).
                               13  SPA-GMN-F5         PIC X(01).
                               13  SPA-GMN-HOSPSBT1MEI
                                                      PIC X(18).
                           11  SPA-GMN-HOSPCDN        PIC X(20).
                           11  SPA-GMN-SYOHOKBN-G.
                               13  SPA-GMN-SYOHOKBN   PIC X(01).
                               13  SPA-GMN-F6         PIC X(01).
                               13  SPA-GMN-SYOHOKBNMEI
                                                      PIC X(06).
                           11  SPA-GMN-SKYPRTFLG-G.
                               13  SPA-GMN-SKYPRTFLG  PIC X(01).
                               13  SPA-GMN-F7         PIC X(01).
                               13  SPA-GMN-SKYPRTFLGMEI
                                                      PIC X(20).
      *----(01.00.02) LINE ADD START ----------------------------------
      *院外処方せん発行フラグ
                           11  SPA-GMN-SYOHOPRTFLG-G.
                               13  SPA-GMN-SYOHOPRTFLG
                                                      PIC X(01).
                               13  SPA-GMN-F8         PIC X(01).
                               13  SPA-GMN-SYOHOPRTFLGMEI
                                                      PIC X(12).
      *前回処方表示フラグ
                           11  SPA-GMN-ZENSYOHOFLG-G.
                               13  SPA-GMN-ZENSYOHOFLG
                                                      PIC X(01).
                               13  SPA-GMN-F9         PIC X(01).
                               13  SPA-GMN-ZENSYOHOFLGMEI
                                                      PIC X(12).
      *----(01.00.02) LINE ADD END   ----------------------------------
      *----(01.00.09) LINE ADD START ----------------------------------
      *薬剤情報発行フラグ
                           11  SPA-GMN-YAKUJYOFLG-G.
                               13  SPA-GMN-YAKUJYOFLG
                                                      PIC X(01).
                               13  SPA-GMN-F10        PIC X(01).
                               13  SPA-GMN-YAKUJYOFLGMEI
                                                      PIC X(12).
                           11  SPA-GMN-DISCOUNT-HASU-KBN-G.
                               13  SPA-GMN-DISCOUNT-HASU-KBN
                                                      PIC X(01).
                               13  SPA-GMN-F11        PIC X(01).
                               13  SPA-GMN-DISCOUNT-HASU-KBNMEI
                                                      PIC X(30).
                           11  SPA-GMN-DISCOUNT-HASU-KBN2-G.
                               13  SPA-GMN-DISCOUNT-HASU-KBN2
                                                      PIC X(01).
                               13                     PIC X(01).
                               13  SPA-GMN-DISCOUNT-HASU-KBN2MEI
                                                      PIC X(30).
                           11  SPA-GMN-TAX-HASU-KBN-G.
                               13  SPA-GMN-TAX-HASU-KBN
                                                      PIC X(01).
                               13  SPA-GMN-F12        PIC X(01).
                               13  SPA-GMN-TAX-HASU-KBNMEI
                                                      PIC X(30).
                           11  SPA-GMN-HIWARIKBN-G.
                               13  SPA-GMN-HIWARIKBN
                                                      PIC X(01).
                               13  SPA-GMN-F13        PIC X(01).
                               13  SPA-GMN-HIWARIKBNMEI
                                                      PIC X(18).
      *----(01.00.09) LINE ADD END   ----------------------------------
                   07  SPA-GMN-1002-AREA.
                       09  SPA-GMN-POST           PIC X(07).
                       09  SPA-GMN-ADRS           PIC X(100).
                       09  SPA-GMN-TEL            PIC X(15).
                       09  SPA-GMN-FAX            PIC X(15). 
                   07  SPA-GMN-1003-AREA.
                       09  SPA-GMN-EMAIL          PIC X(100).
                       09  SPA-GMN-HPURL          PIC X(100).
                   07  SPA-GMN-1026-AREA.
                       09  SPA-GMN-DISCOUNT-KBNLST    OCCURS  99.
                           11  SPA-GMN-DISCOUNT-KBNL
                                                  PIC X(02).
                           11                     PIC X(01).
                           11  SPA-GMN-DISCOUNT-KBNMEIL
                                                  PIC X(20).
                       09  SPA-GMN-DISCOUNT-KBN-MAX
                                                  PIC 9(05).
                       09  SPA-GMN-DISCOUNT-BODY  PIC X(20).
                       09  SPA-GMN-DISCOUNT-KBN-G.
                           11  SPA-GMN-DISCOUNT-KBN
                                                  PIC X(02).
                           11                     PIC X(01).
                           11  SPA-GMN-DISCOUNT-KBNMEI
                                                  PIC X(20).
                       09  SPA-GMN-DISCOUNT-OBJLST    OCCURS  10.
                           11  SPA-GMN-DISCOUNT-OBJL
                                                  PIC X(01).
                           11                     PIC X(01).
                           11  SPA-GMN-DISCOUNT-OBJMEIL
                                                  PIC X(18).
                       09  SPA-GMN-DISCOUNT-OBJ-G.
                           11  SPA-GMN-DISCOUNT-OBJ
                                                  PIC X(01).
                           11                     PIC X(01).
                           11  SPA-GMN-DISCOUNT-OBJMEI
                                                  PIC X(18).
                   07  SPA-GMN-1027-AREA.
                       09  SPA-GMN-TEIRITULST         OCCURS  2.
                           11  SPA-GMN-TEIRITUL       PIC X(01).
                           11                         PIC X(01).
                           11  SPA-GMN-TEIRITUMEIL    PIC X(08).
                       09  SPA-GMN-TEIRITU-G.
                           11  SPA-GMN-TEIRITU        PIC X(01).
                           11                         PIC X(01).
                           11  SPA-GMN-TEIRITUMEI     PIC X(08).
                       09  SPA-GMN-DISCOUNT-RATE  PIC 9(07).
                       09  SPA-GMN-LBL-RATE       PIC X(02).
                   07  SPA-GMN-1018-AREA.
                       09  SPA-GMN-CONDITION-BODY PIC X(20).
                       09  SPA-GMN-CONDITION-WORD PIC X(02).
                       09  SPA-GMN-ICON-FILENAME  PIC X(100).
                   07  SPA-GMN-1013-AREA.
                       09  SPA-GMN-JIHINAME       PIC X(20).
      *
                   07  SPA-GMN-1012-AREA.
                       09  SPA-GMN-SRYNAIYO       PIC X(20).
                       09  SPA-GMN-SRYNAIYODSP    PIC X(12).
      *
                   07  SPA-GMN-1041-AREA.
                       09  SPA-GMN-NYKN-KBNCD     PIC X(02).
                       09  SPA-GMN-NYKN-NAME      PIC X(30).
                       09  SPA-GMN-NYKN-TANNAME   PIC X(10).
                       09  SPA-GMN-NYKN-BUNRUI-G.
                           11  SPA-GMN-NYKN-BUNRUI
                                                  PIC X(02).
                           11                     PIC X(01).
                           11  SPA-GMN-NYKN-BUNRUIMEI
                                                  PIC X(20).
                       09  SPA-GMN-NYKN-BUNRUILST OCCURS  10.
                           11  SPA-GMN-NYKN-BUNRUIL    PIC X(02).
                           11                          PIC X(01).
                           11  SPA-GMN-NYKN-BUNRUIMEIL PIC X(20).
                       09  SPA-GMN-NYKN-BUNRUI-MAX     PIC 9(03).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD           PIC 9(08).
                   07  SPA-NAI-EDYUKYMD           PIC 9(08).
      *
      *        基本情報の期間変更
               05  SPA-NAI1001-AREA.
                   07  SPA-1001-STYUKYMD          PIC X(08).
                   07  SPA-1001-EDYUKYMD          PIC X(08).
                   07  SPA-1001-ADD               PIC 9(01).
      *   
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
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
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
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
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-RATE-AREA.
               05  WRK-RATE-EDIT.
                   07  WRK-RATE-EDITZ  PIC ZZZZZZZ.
                   07  WRK-RATE-TANI   PIC X(04).
               05  WRK-RATE-EDIT-R  REDEFINES    WRK-RATE-EDIT.
                   07  WRK-RATE-EDITR  PIC X(11).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    診療内容情報
           COPY  "CPSK1001.INC".
      *    診療科
           COPY  "CPSK1002.INC".
      *    職員情報
           COPY  "CPSK1003.INC".
      *    減免事由情報
           COPY  "CPSK1026.INC".
      *    割引率情報
           COPY  "CPSK1027.INC".
      *    状態情報
           COPY  "CPSK1018.INC".
      *    請求書自費名称情報
           COPY  "CPSK1013.INC".
      *
      *    診療内容名称情報
           COPY  "CPSK1012.INC".
      *
      *    入金方法情報
           COPY  "CPSK1041.INC".
      *
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W02
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
           MOVE    SPA-W02         TO  SPA-FREE
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
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W02"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
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
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
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
           IF      SPA-WID1-FLG    =   "OK"
               IF      SPA-WIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
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
           MOVE    1                   TO  SPA-GMN-CUR
      *
           INITIALIZE SPA-W02-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           MOVE    "01"                TO  SPA-GMN-PREFNUML   (1) 
           MOVE    "北海道"            TO  SPA-GMN-PREFNUMMEIL(1)
           MOVE    "02"                TO  SPA-GMN-PREFNUML   (2)
           MOVE    "青森"              TO  SPA-GMN-PREFNUMMEIL(2)
           MOVE    "03"                TO  SPA-GMN-PREFNUML   (3)
           MOVE    "岩手"              TO  SPA-GMN-PREFNUMMEIL(3) 
           MOVE    "04"                TO  SPA-GMN-PREFNUML   (4)
           MOVE    "宮城"              TO  SPA-GMN-PREFNUMMEIL(4)
           MOVE    "05"                TO  SPA-GMN-PREFNUML   (5)
           MOVE    "秋田"              TO  SPA-GMN-PREFNUMMEIL(5)
           MOVE    "06"                TO  SPA-GMN-PREFNUML   (6)
           MOVE    "山形"              TO  SPA-GMN-PREFNUMMEIL(6)
           MOVE    "07"                TO  SPA-GMN-PREFNUML   (7)
           MOVE    "福島"              TO  SPA-GMN-PREFNUMMEIL(7)
           MOVE    "08"                TO  SPA-GMN-PREFNUML   (8)
           MOVE    "茨城"              TO  SPA-GMN-PREFNUMMEIL(8)
           MOVE    "09"                TO  SPA-GMN-PREFNUML   (9)
           MOVE    "栃木"              TO  SPA-GMN-PREFNUMMEIL(9)
           MOVE    "10"                TO  SPA-GMN-PREFNUML   (10)
           MOVE    "群馬"              TO  SPA-GMN-PREFNUMMEIL(10)
           MOVE    "11"                TO  SPA-GMN-PREFNUML   (11)
           MOVE    "埼玉"              TO  SPA-GMN-PREFNUMMEIL(11)
           MOVE    "12"                TO  SPA-GMN-PREFNUML   (12)
           MOVE    "千葉"              TO  SPA-GMN-PREFNUMMEIL(12)
           MOVE    "13"                TO  SPA-GMN-PREFNUML   (13)
           MOVE    "東京"              TO  SPA-GMN-PREFNUMMEIL(13)
           MOVE    "14"                TO  SPA-GMN-PREFNUML   (14)
           MOVE    "神奈川"            TO  SPA-GMN-PREFNUMMEIL(14)
           MOVE    "15"                TO  SPA-GMN-PREFNUML   (15)
           MOVE    "新潟"              TO  SPA-GMN-PREFNUMMEIL(15)
           MOVE    "16"                TO  SPA-GMN-PREFNUML   (16)
           MOVE    "富山"              TO  SPA-GMN-PREFNUMMEIL(16)
           MOVE    "17"                TO  SPA-GMN-PREFNUML   (17)
           MOVE    "石川"              TO  SPA-GMN-PREFNUMMEIL(17)
           MOVE    "18"                TO  SPA-GMN-PREFNUML   (18)
           MOVE    "福井"              TO  SPA-GMN-PREFNUMMEIL(18)
           MOVE    "19"                TO  SPA-GMN-PREFNUML   (19)
           MOVE    "山梨"              TO  SPA-GMN-PREFNUMMEIL(19)
           MOVE    "20"                TO  SPA-GMN-PREFNUML   (20)
           MOVE    "長野"              TO  SPA-GMN-PREFNUMMEIL(20)
           MOVE    "21"                TO  SPA-GMN-PREFNUML   (21)
           MOVE    "岐阜"              TO  SPA-GMN-PREFNUMMEIL(21)
           MOVE    "22"                TO  SPA-GMN-PREFNUML   (22)
           MOVE    "静岡"              TO  SPA-GMN-PREFNUMMEIL(22)
           MOVE    "23"                TO  SPA-GMN-PREFNUML   (23)
           MOVE    "愛知"              TO  SPA-GMN-PREFNUMMEIL(23)
           MOVE    "24"                TO  SPA-GMN-PREFNUML   (24)
           MOVE    "三重"              TO  SPA-GMN-PREFNUMMEIL(24)
           MOVE    "25"                TO  SPA-GMN-PREFNUML   (25)
           MOVE    "滋賀"              TO  SPA-GMN-PREFNUMMEIL(25)
           MOVE    "26"                TO  SPA-GMN-PREFNUML   (26)
           MOVE    "京都"              TO  SPA-GMN-PREFNUMMEIL(26)
           MOVE    "27"                TO  SPA-GMN-PREFNUML   (27)
           MOVE    "大阪"              TO  SPA-GMN-PREFNUMMEIL(27)
           MOVE    "28"                TO  SPA-GMN-PREFNUML   (28)
           MOVE    "兵庫"              TO  SPA-GMN-PREFNUMMEIL(28)
           MOVE    "29"                TO  SPA-GMN-PREFNUML   (29)
           MOVE    "奈良"              TO  SPA-GMN-PREFNUMMEIL(29)
           MOVE    "30"                TO  SPA-GMN-PREFNUML   (30)
           MOVE    "和歌山"            TO  SPA-GMN-PREFNUMMEIL(30)
           MOVE    "31"                TO  SPA-GMN-PREFNUML   (31)
           MOVE    "鳥取"              TO  SPA-GMN-PREFNUMMEIL(31)
           MOVE    "32"                TO  SPA-GMN-PREFNUML   (32)
           MOVE    "島根"              TO  SPA-GMN-PREFNUMMEIL(32)
           MOVE    "33"                TO  SPA-GMN-PREFNUML   (33)
           MOVE    "岡山"              TO  SPA-GMN-PREFNUMMEIL(33)
           MOVE    "34"                TO  SPA-GMN-PREFNUML   (34)
           MOVE    "広島"              TO  SPA-GMN-PREFNUMMEIL(34)
           MOVE    "35"                TO  SPA-GMN-PREFNUML   (35)
           MOVE    "山口"              TO  SPA-GMN-PREFNUMMEIL(35)
           MOVE    "36"                TO  SPA-GMN-PREFNUML   (36)
           MOVE    "徳島"              TO  SPA-GMN-PREFNUMMEIL(36)
           MOVE    "37"                TO  SPA-GMN-PREFNUML   (37)
           MOVE    "香川"              TO  SPA-GMN-PREFNUMMEIL(37)
           MOVE    "38"                TO  SPA-GMN-PREFNUML   (38)
           MOVE    "愛媛"              TO  SPA-GMN-PREFNUMMEIL(38)
           MOVE    "39"                TO  SPA-GMN-PREFNUML   (39)
           MOVE    "高知"              TO  SPA-GMN-PREFNUMMEIL(39)
           MOVE    "40"                TO  SPA-GMN-PREFNUML   (40)
           MOVE    "福岡"              TO  SPA-GMN-PREFNUMMEIL(40)
           MOVE    "41"                TO  SPA-GMN-PREFNUML   (41)
           MOVE    "佐賀"              TO  SPA-GMN-PREFNUMMEIL(41)
           MOVE    "42"                TO  SPA-GMN-PREFNUML   (42)
           MOVE    "長崎"              TO  SPA-GMN-PREFNUMMEIL(42)
           MOVE    "43"                TO  SPA-GMN-PREFNUML   (43)
           MOVE    "熊本"              TO  SPA-GMN-PREFNUMMEIL(43)
           MOVE    "44"                TO  SPA-GMN-PREFNUML   (44)
           MOVE    "大分"              TO  SPA-GMN-PREFNUMMEIL(44)
           MOVE    "45"                TO  SPA-GMN-PREFNUML   (45)
           MOVE    "宮崎"              TO  SPA-GMN-PREFNUMMEIL(45)
           MOVE    "46"                TO  SPA-GMN-PREFNUML   (46)
           MOVE    "鹿児島"            TO  SPA-GMN-PREFNUMMEIL(46)
           MOVE    "47"                TO  SPA-GMN-PREFNUML   (47)
           MOVE    "沖縄"              TO  SPA-GMN-PREFNUMMEIL (47)
      *
           MOVE    "1"                TO  SPA-GMN-TENHYOKBNL   (1)
           MOVE    "医科"             TO  SPA-GMN-TENHYOKBNMEIL(1)
      *
           MOVE    "1"                TO  SPA-GMN-HOSPSBTL     (1)
           MOVE    "病院"             TO  SPA-GMN-HOSPSBTMEIL  (1)
           MOVE    "2"                TO  SPA-GMN-HOSPSBTL     (2)
           MOVE    "診療所"           TO  SPA-GMN-HOSPSBTMEIL  (2)
      *
           MOVE    "1"                TO  SPA-GMN-ROUPAYKBNL   (1) 
           MOVE    "定率"             TO  SPA-GMN-ROUPAYKBNMEIL(1)
           MOVE    "2"                TO  SPA-GMN-ROUPAYKBNL   (2)
           MOVE    "定額"             TO  SPA-GMN-ROUPAYKBNMEIL(2)
      *
           MOVE    "0"                TO  SPA-GMN-HOSPSBT1L    (1) 
           MOVE    "旧総合病院でない" TO  SPA-GMN-HOSPSBT1MEIL (1)
           MOVE    "1"                TO  SPA-GMN-HOSPSBT1L    (2)
           MOVE    "旧総合病院である" TO  SPA-GMN-HOSPSBT1MEIL (2)
      *
           MOVE    "0"                TO  SPA-GMN-SYOHOKBNL    (1) 
           MOVE    "院内"             TO  SPA-GMN-SYOHOKBNMEIL (1)
           MOVE    "1"                TO  SPA-GMN-SYOHOKBNL    (2)
           MOVE    "院外"             TO  SPA-GMN-SYOHOKBNMEIL (2)
      *
           MOVE    "0"                TO  SPA-GMN-SKYPRTFLGL   (1) 
           MOVE    "発行しない"       TO  SPA-GMN-SKYPRTFLGMEIL(1)
           MOVE    "1"                TO  SPA-GMN-SKYPRTFLGL   (2)
           MOVE    "発行する"         TO  SPA-GMN-SKYPRTFLGMEIL(2)
           MOVE    "2"                TO  SPA-GMN-SKYPRTFLGL   (3)
           MOVE    "発行する（請求あり）"
                                      TO  SPA-GMN-SKYPRTFLGMEIL(3)
      *
           MOVE    "0"                TO  SPA-GMN-SYOHOPRTFLGL    (1) 
           MOVE    "発行しない"       TO  SPA-GMN-SYOHOPRTFLGMEIL (1)
           MOVE    "1"                TO  SPA-GMN-SYOHOPRTFLGL    (2)
           MOVE    "発行する"         TO  SPA-GMN-SYOHOPRTFLGMEIL (2)
           MOVE    "2"                TO  SPA-GMN-SYOHOPRTFLGL    (3)
           MOVE    "院内処方発行"     TO  SPA-GMN-SYOHOPRTFLGMEIL (3)
      *
           MOVE    "0"                TO  SPA-GMN-ZENSYOHOFLGL    (1) 
           MOVE    "表示する"         TO  SPA-GMN-ZENSYOHOFLGMEIL (1)
           MOVE    "1"                TO  SPA-GMN-ZENSYOHOFLGL    (2)
           MOVE    "表示しない"       TO  SPA-GMN-ZENSYOHOFLGMEIL (2)
      *
      *----(01.00.09) LINE ADD START ----------------------------------
           MOVE    "0"                TO  SPA-GMN-YAKUJYOFLGL    (1)
           MOVE    "発行しない"       TO  SPA-GMN-YAKUJYOFLGMEIL (1)
           MOVE    "1"                TO  SPA-GMN-YAKUJYOFLGL    (2)
           MOVE    "発行する"         TO  SPA-GMN-YAKUJYOFLGMEIL (2)
      *----(01.00.09) LINE ADD END   ----------------------------------
           MOVE    "1"         TO  SPA-GMN-DISCOUNT-HASU-KBNL    (1)
           MOVE    "１０円未満四捨五入"  
                               TO  SPA-GMN-DISCOUNT-HASU-KBNMEIL (1)
           MOVE    "2"         TO  SPA-GMN-DISCOUNT-HASU-KBNL    (2)
           MOVE    "１０円未満切り捨て" 
                               TO  SPA-GMN-DISCOUNT-HASU-KBNMEIL (2)
           MOVE    "3"         TO  SPA-GMN-DISCOUNT-HASU-KBNL    (3)
           MOVE    "１０円未満切り上げ"   
                               TO  SPA-GMN-DISCOUNT-HASU-KBNMEIL (3)
           MOVE    "4"         TO  SPA-GMN-DISCOUNT-HASU-KBNL    (4)
           MOVE    "１０円未満端数処理なし"
                               TO  SPA-GMN-DISCOUNT-HASU-KBNMEIL (4)
           MOVE    SPA-GMN-DISCOUNT-HASU-KBNLST (1)
                               TO  SPA-GMN-DISCOUNT-HASU-KBN-G
      *
           MOVE    "1"         TO  SPA-GMN-DISCOUNT-HASU-KBN2L    (1)
           MOVE    "１０円未満四捨五入"  
                               TO  SPA-GMN-DISCOUNT-HASU-KBN2MEIL (1)
           MOVE    "2"         TO  SPA-GMN-DISCOUNT-HASU-KBN2L    (2)
           MOVE    "１０円未満切り捨て" 
                               TO  SPA-GMN-DISCOUNT-HASU-KBN2MEIL (2)
           MOVE    "3"         TO  SPA-GMN-DISCOUNT-HASU-KBN2L    (3)
           MOVE    "１０円未満切り上げ"   
                               TO  SPA-GMN-DISCOUNT-HASU-KBN2MEIL (3)
           MOVE    "4"         TO  SPA-GMN-DISCOUNT-HASU-KBN2L    (4)
           MOVE    "１０円未満端数処理なし"
                               TO  SPA-GMN-DISCOUNT-HASU-KBN2MEIL (4)
           MOVE    SPA-GMN-DISCOUNT-HASU-KBN2LST (4)
                               TO  SPA-GMN-DISCOUNT-HASU-KBN2-G
      *
           MOVE    "1"         TO  SPA-GMN-TAX-HASU-KBNL    (1)
           MOVE    "１円未満四捨五入"  
                               TO  SPA-GMN-TAX-HASU-KBNMEIL (1)
           MOVE    "2"         TO  SPA-GMN-TAX-HASU-KBNL    (2)
           MOVE    "１円未満切り捨て" 
                               TO  SPA-GMN-TAX-HASU-KBNMEIL (2)
           MOVE    SPA-GMN-TAX-HASU-KBNLST (2)
                               TO  SPA-GMN-TAX-HASU-KBN-G
      *
           MOVE    "1"         TO  SPA-GMN-HIWARIKBNL    (1)
           MOVE    "日割計算しない"  
                               TO  SPA-GMN-HIWARIKBNMEIL (1)
           MOVE    "2"         TO  SPA-GMN-HIWARIKBNL    (2)
           MOVE    "日割計算する" 
                               TO  SPA-GMN-HIWARIKBNMEIL (2)
           MOVE    SPA-GMN-HIWARIKBNLST (1)
                               TO  SPA-GMN-HIWARIKBN-G
      *
           MOVE    "1"         TO  SPA-GMN-TEIRITUL (1)
           MOVE    "定率"      TO  SPA-GMN-TEIRITUMEIL (1)
      * 
           MOVE    "2"         TO  SPA-GMN-TEIRITUL (2)
           MOVE    "定額"      TO  SPA-GMN-TEIRITUMEIL (2)
      *
           MOVE    "1"         TO  SPA-GMN-DISCOUNT-OBJL (1)
           MOVE    "保険診療＋自費分" 
                               TO  SPA-GMN-DISCOUNT-OBJMEIL (1)
      *
           MOVE    "2"         TO  SPA-GMN-DISCOUNT-OBJL (2)
           MOVE    "保険診療のみ"
                               TO  SPA-GMN-DISCOUNT-OBJMEIL (2)
      *
           MOVE    "3"         TO  SPA-GMN-DISCOUNT-OBJL (3)
           MOVE    "自費分のみ"
                               TO  SPA-GMN-DISCOUNT-OBJMEIL (3)
      * 
      *    割引率区分設定処理
           PERFORM 3101-DISCOUNT-KBN-SET-SEC
      * 
      *    入金方法分類区分設定処理
           PERFORM 3101-NYKN-BUNRUI-SET-SEC
      * 
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-W01-KANRICD         TO  SYS-1001-KANRICD
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1012"
               WHEN    "1013"
               WHEN    "1018"
               WHEN    "1019"
               WHEN    "1020"
               WHEN    "1026"
               WHEN    "1027"
               WHEN    "1041"
                   MOVE    SPA-W01-KBNCD       TO  SYS-1001-KBNCD
               WHEN    OTHER
                   MOVE    "*"                 TO  SYS-1001-KBNCD
           END-EVALUATE
           MOVE    SYS-1001-REC            TO  MCPDATA-REC
           MOVE    SPA-W01-STYUKYMD        TO  ORC-DBYMD
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               EVALUATE    SPA-W01-KANRICD
      *            医療機関情報ー基本情報
                   WHEN    "1001"
                       MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *                基本情報編集処理
                       PERFORM 31091-KIHON-HENSYU-SEC
      *            医療機関情報ー所在地、連絡先情報
                   WHEN    "1002"
                       MOVE    MCPDATA-REC         TO  SYS-1002-REC
                       MOVE    SYS-1002-POST       TO  SPA-GMN-POST 
                       MOVE    SYS-1002-ADRS       TO  SPA-GMN-ADRS 
                       MOVE    SYS-1002-TEL        TO  SPA-GMN-TEL 
                       MOVE    SYS-1002-FAX        TO  SPA-GMN-FAX
      *            医療機関情報ー広告情報
                   WHEN    "1003"
                       MOVE    MCPDATA-REC         TO  SYS-1003-REC
                       MOVE    SYS-1003-EMAIL      TO  SPA-GMN-EMAIL 
                       MOVE    SYS-1003-HPURL      TO  SPA-GMN-HPURL
      *    医療機関情報ー減免事由情報
                   WHEN    "1026"
                       MOVE    MCPDATA-REC         TO  SYS-1026-REC
                       MOVE    SYS-1026-DISCOUNT-BODY  TO
                                               SPA-GMN-DISCOUNT-BODY
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL ( IDY
                                       >   SPA-GMN-DISCOUNT-KBN-MAX )
                           IF    ( SYS-1026-DISCOUNT-KBN
                                       =   SPA-GMN-DISCOUNT-KBNL (IDY) )
                               MOVE    SPA-GMN-DISCOUNT-KBNLST (IDY)
                                       TO  SPA-GMN-DISCOUNT-KBN-G
                               MOVE    SPA-GMN-DISCOUNT-KBN-MAX
                                       TO  IDY
                           END-IF
                       END-PERFORM
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL ( IDY
                                       >   3 )
      *                    空白の場合は"1"に置き換える
                           IF    ( SYS-1026-DISCOUNT-OBJ   =   SPACE )
                               MOVE    "1"
                                       TO  SYS-1026-DISCOUNT-OBJ
                           END-IF
                           IF    ( SYS-1026-DISCOUNT-OBJ
                                       =   SPA-GMN-DISCOUNT-OBJL (IDY) )
                               MOVE    SPA-GMN-DISCOUNT-OBJLST (IDY)
                                       TO  SPA-GMN-DISCOUNT-OBJ-G
                               MOVE    3
                                       TO  IDY
                           END-IF
                       END-PERFORM
      *    医療機関情報ー割引率情報
                   WHEN    "1027"
                       MOVE    MCPDATA-REC         TO  SYS-1027-REC
      *
                       EVALUATE    SYS-1027-TEIRITU
                       WHEN    "1"
                           MOVE    SPA-GMN-TEIRITULST (1)
                                                   TO  SPA-GMN-TEIRITU-G
                           MOVE    "％"            TO  SPA-GMN-LBL-RATE
                       WHEN    "2"
                           MOVE    SPA-GMN-TEIRITULST (2)
                                                   TO  SPA-GMN-TEIRITU-G
                           MOVE    "円"            TO  SPA-GMN-LBL-RATE
                       END-EVALUATE
      *
                       MOVE    SYS-1027-DISCOUNT-RATE  TO
                                               SPA-GMN-DISCOUNT-RATE
      *    医療機関情報ー状態コメント情報
                   WHEN    "1018"
                   WHEN    "1019"
                   WHEN    "1020" 
                       MOVE    MCPDATA-REC         TO  SYS-1018-REC
                       MOVE    SYS-1018-CONDITION-BODY TO
                                               SPA-GMN-CONDITION-BODY
                       MOVE    SYS-1018-CONDITION-WORD TO
                                               SPA-GMN-CONDITION-WORD
                       MOVE    SYS-1018-ICON-FILENAME  TO
                                               SPA-GMN-ICON-FILENAME
      *    請求書自費名称情報
                   WHEN    "1013"
                       MOVE    MCPDATA-REC         TO  SYS-1013-REC
                       MOVE    SYS-1013-JIHINAME   TO
                                               SPA-GMN-JIHINAME 
      *    診療内容名称情報
                   WHEN    "1012"
                       MOVE    MCPDATA-REC         TO  SYS-1012-REC
                       MOVE    SYS-1012-SRYNAIYO   TO  SPA-GMN-SRYNAIYO
                       MOVE    SYS-1012-SRYNAIYODSP
                                               TO  SPA-GMN-SRYNAIYODSP
      *    入金方法情報
                   WHEN    "1041"
                       MOVE    MCPDATA-REC     TO  SYS-1041-REC
                       MOVE    SYS-1041-KBNCD  TO  SPA-GMN-NYKN-KBNCD
                       MOVE    SYS-1041-NYKN-NAME
                                               TO  SPA-GMN-NYKN-NAME
                       MOVE    SYS-1041-NYKN-TANNAME
                                               TO  SPA-GMN-NYKN-TANNAME
                       MOVE    SPA-GMN-NYKN-BUNRUILST (1)
                                               TO  SPA-GMN-NYKN-BUNRUI-G
      *
                       PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   SPA-GMN-NYKN-BUNRUI-MAX )
                           IF    ( SYS-1041-NYKN-BUNRUI
                                       =   SPA-GMN-NYKN-BUNRUIL (IDX1))
                               MOVE    SPA-GMN-NYKN-BUNRUILST (IDX1)
                                   TO  SPA-GMN-NYKN-BUNRUI-G
                           END-IF
                       END-PERFORM
      *
               END-EVALUATE
      *
           ELSE
      *        基本情報は期間変更があることする
               EVALUATE    SPA-W01-KANRICD
      *            医療機関情報ー基本情報
                   WHEN    "1001"
                       PERFORM 3109-1001-HENKOU-SEC
                   WHEN    "1041"
                       MOVE    SPA-W01-KBNCD   TO  SPA-GMN-NYKN-KBNCD
                       MOVE    SPA-GMN-NYKN-BUNRUILST (1)
                                               TO  SPA-GMN-NYKN-BUNRUI-G
               END-EVALUATE
           END-IF 
      *  
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1001"
                   MOVE    1                   TO  SPA-GMN-CUR
               WHEN    "1002"
                   MOVE    20                  TO  SPA-GMN-CUR
               WHEN    "1003"
                   MOVE    30                  TO  SPA-GMN-CUR
               WHEN    "1026"
                   MOVE    50                  TO  SPA-GMN-CUR
               WHEN    "1027"
                   MOVE    40                  TO  SPA-GMN-CUR
               WHEN    "1018"
               WHEN    "1019"
               WHEN    "1020"
                   MOVE    60                  TO  SPA-GMN-CUR
               WHEN    "1013"
                   MOVE    70                  TO  SPA-GMN-CUR
               WHEN    "1012"
                   MOVE    80                  TO  SPA-GMN-CUR
               WHEN    "1041"
                   MOVE    90                  TO  SPA-GMN-CUR
           END-EVALUATE
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報期間変更処理
      *****************************************************************
       3109-1001-HENKOU-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *
           MOVE    SPA-W01-STYUKYMD    TO  ORC-DBYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY5"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY5"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    SPACE               TO  SYS-1001-REC
           PERFORM UNTIL   FLG-SYSKANRI    =   1
      *        前の内容を複写する
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *
               MOVE    "SYSKANRI-KEY5"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           IF      SYS-1001-KANRICD    =   SPACE
      *        未登録の時
               GO      TO      3109-1001-HENKOU-EXT
           END-IF
      *    基本情報編集処理
           PERFORM 31091-KIHON-HENSYU-SEC
           MOVE    SYS-1001-STYUKYMD   TO  SPA-1001-STYUKYMD
           MOVE    SYS-1001-EDYUKYMD   TO  SPA-1001-EDYUKYMD
           MOVE    1                   TO  SPA-1001-ADD
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1001-EDYUKYMD
      *
           . 
       3109-1001-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報編集処理
      *****************************************************************
       31091-KIHON-HENSYU-SEC       SECTION.
     *
           MOVE    SYS-1001-PREFNUM    TO  SPA-GMN-PREFNUM
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       47
               IF      SPA-GMN-PREFNUM     =   SPA-GMN-PREFNUML (IDY)
                   MOVE    SPA-GMN-PREFNUMLST (IDY)
                                           TO  SPA-GMN-PREFNUM-G
                   MOVE    47               TO  IDY
               END-IF
           END-PERFORM
     *
           MOVE    SYS-1001-TENHYOKBN  TO  SPA-GMN-TENHYOKBN
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       1
               IF      SYS-1001-TENHYOKBN  =   SPA-GMN-TENHYOKBNL (IDY)
                   MOVE    SPA-GMN-TENHYOKBNLST (IDY)
                                           TO  SPA-GMN-TENHYOKBN-G
               END-IF
           END-PERFORM
           MOVE    SYS-1001-HOSPCD     TO  SPA-GMN-HOSPCD
           MOVE    SYS-1001-HOSPSBT    TO  SPA-GMN-HOSPSBT
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
              IF       SYS-1001-HOSPSBT    =   SPA-GMN-HOSPSBTL (IDY)
                   MOVE    SPA-GMN-HOSPSBTLST (IDY)
                                           TO  SPA-GMN-HOSPSBT-G
                   MOVE    2                TO  IDY
              END-IF
           END-PERFORM
           MOVE    SYS-1001-HOSPID     TO  SPA-GMN-HOSPID
           MOVE    SYS-1001-HOSPNAME   TO  SPA-GMN-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  SPA-GMN-KAISETUNAME
           MOVE    SYS-1001-KANRINAME  TO  SPA-GMN-KANRINAME
           MOVE    SYS-1001-BEDSU      TO  SPA-GMN-BEDSU
           IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                  (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
               MOVE    ZERO                TO  SPA-GMN-BEDSUIPN
           ELSE
               MOVE    SYS-1001-BEDSUIPN   TO  SPA-GMN-BEDSUIPN
           END-IF
           MOVE    SYS-1001-ROUPAYKBN  TO  SPA-GMN-ROUPAYKBN
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SYS-1001-ROUPAYKBN  =   SPA-GMN-ROUPAYKBNL (IDY)
                   MOVE    SPA-GMN-ROUPAYKBNLST (IDY)
                                           TO  SPA-GMN-ROUPAYKBN-G
                   MOVE    2               TO  IDY
               END-IF
           END-PERFORM
           MOVE    SYS-1001-HOSPSBT1   TO  SPA-GMN-HOSPSBT1
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SYS-1001-HOSPSBT1   =   SPA-GMN-HOSPSBT1L (IDY)
                   MOVE    SPA-GMN-HOSPSBT1LST (IDY)
                                           TO  SPA-GMN-HOSPSBT1-G
                   MOVE    2               TO  IDY
               END-IF
           END-PERFORM
           MOVE    SYS-1001-HOSPCDN    TO  SPA-GMN-HOSPCDN
           MOVE    SYS-1001-SYOHOKBN   TO  SPA-GMN-SYOHOKBN
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SYS-1001-SYOHOKBN   =   SPA-GMN-SYOHOKBNL (IDY)
                   MOVE    SPA-GMN-SYOHOKBNLST (IDY)
                                           TO  SPA-GMN-SYOHOKBN-G
                   MOVE    2               TO  IDY
               END-IF
           END-PERFORM
           MOVE    SYS-1001-SKYPRTFLG  TO  SPA-GMN-SKYPRTFLG
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       3
               IF      SYS-1001-SKYPRTFLG  =   SPA-GMN-SKYPRTFLGL (IDY)
                   MOVE    SPA-GMN-SKYPRTFLGLST (IDY)
                                           TO  SPA-GMN-SKYPRTFLG-G
                   MOVE    3               TO  IDY
               END-IF
           END-PERFORM
           MOVE    SYS-1001-SYOHOPRTFLG    TO  SPA-GMN-SYOHOPRTFLG 
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       3
               IF      SYS-1001-SYOHOPRTFLG
                                       =   SPA-GMN-SYOHOPRTFLGL (IDY)
                   MOVE    SPA-GMN-SYOHOPRTFLGLST (IDY)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
                   MOVE    3               TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SYS-1001-ZENSYOHOFLG    TO  SPA-GMN-ZENSYOHOFLG
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SYS-1001-ZENSYOHOFLG
                                       =   SPA-GMN-ZENSYOHOFLGL (IDY)
                   MOVE    SPA-GMN-ZENSYOHOFLGLST (IDY)
                                           TO  SPA-GMN-ZENSYOHOFLG-G
                   MOVE    2               TO  IDY
               END-IF
           END-PERFORM
      *
      *    薬剤情報発効空白の時、ゼロとする
           IF      SYS-1001-YAKUJYOPRTFLG  =   SPACE
               MOVE    ZERO                TO  SYS-1001-YAKUJYOPRTFLG
           END-IF
           MOVE    SYS-1001-YAKUJYOPRTFLG  TO  SPA-GMN-YAKUJYOFLG
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SYS-1001-YAKUJYOPRTFLG
                                       =   SPA-GMN-YAKUJYOFLGL (IDY)
                   MOVE    SPA-GMN-YAKUJYOFLGLST (IDY)
                                           TO  SPA-GMN-YAKUJYOFLG-G
                   MOVE    2               TO  IDY
               END-IF
           END-PERFORM
      *    請求額端数区分（減免有） 空白の時、"1"とする
           IF      SYS-1001-DISCOUNT-HASU-KBN    =   SPACE
               MOVE    "1"             TO  SYS-1001-DISCOUNT-HASU-KBN
           END-IF
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
               IF      SYS-1001-DISCOUNT-HASU-KBN
                                   =   SPA-GMN-DISCOUNT-HASU-KBNL (IDY)
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBNLST (IDY)
                                       TO  SPA-GMN-DISCOUNT-HASU-KBN-G
                   MOVE    4               TO  IDY
                END-IF
           END-PERFORM
      *
      *    請求額端数区分（減免無） 空白の時、"4"とする
           IF      SYS-1001-DISCOUNT-HASU-KBN2    =   SPACE
               MOVE    "4"             TO  SYS-1001-DISCOUNT-HASU-KBN2
           END-IF
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
               IF      SYS-1001-DISCOUNT-HASU-KBN2
                                   =   SPA-GMN-DISCOUNT-HASU-KBN2L (IDY)
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBN2LST (IDY)
                                       TO  SPA-GMN-DISCOUNT-HASU-KBN2-G
                   MOVE    4               TO  IDY
                END-IF
           END-PERFORM
      *
      *    消費税端数区分 空白の時、"2"とする
           IF      SYS-1001-TAX-HASU-KBN   =   SPACE
               MOVE    "2"             TO  SYS-1001-TAX-HASU-KBN
           END-IF
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       2
               IF      SYS-1001-TAX-HASU-KBN
                                       =   SPA-GMN-TAX-HASU-KBNL (IDY)
                   MOVE    SPA-GMN-TAX-HASU-KBNLST (IDY)
                                           TO  SPA-GMN-TAX-HASU-KBN-G
                   MOVE    2               TO  IDY
                END-IF
           END-PERFORM
      *
      *    更正・育成限度額日割計算区分 空白の時、"1"とする
           IF      SYS-1001-HIWARIKBN   =   SPACE
               MOVE    "1"             TO  SYS-1001-HIWARIKBN
           END-IF
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       2
               IF      SYS-1001-HIWARIKBN
                                       =   SPA-GMN-HIWARIKBNL (IDY)
                   MOVE    SPA-GMN-HIWARIKBNLST (IDY)
                                           TO  SPA-GMN-HIWARIKBN-G
                   MOVE    2               TO  IDY
                END-IF
           END-PERFORM
      *
           . 
       31091-KIHON-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    割引率区分設定処理
      *****************************************************************
       3101-DISCOUNT-KBN-SET-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-1027-REC 
           INITIALIZE                      SYS-1027-REC
           MOVE    "1027"                  TO  SYS-1027-KANRICD
           MOVE    SPA-W01-STYUKYMD        TO  ORC-DBYMD
           MOVE    SYS-1027-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-DISCOUNT-KBN-MAX
           PERFORM     UNTIL ( FLG-SYSKANRI    =   1 )
                       OR    ( SPA-GMN-DISCOUNT-KBN-MAX  >=  99 )
               MOVE    MCPDATA-REC     TO  SYS-1027-REC
      *
               COMPUTE SPA-GMN-DISCOUNT-KBN-MAX
                   =   SPA-GMN-DISCOUNT-KBN-MAX    +   1
      *
               MOVE    SPACE           TO  WRK-RATE-AREA
               MOVE    SYS-1027-DISCOUNT-RATE
                                       TO  WRK-RATE-EDITZ
               EVALUATE    SYS-1027-TEIRITU
               WHEN    "0"
                   MOVE    "該当なし"  TO  WRK-RATE-EDIT-R
               WHEN    "1"
                   MOVE    "(%)"       TO  WRK-RATE-TANI
               WHEN    "2"
                   MOVE    "(円)"      TO  WRK-RATE-TANI
               END-EVALUATE
               MOVE    SYS-1027-KBNCD (1 : 2)
                                       TO  SPA-GMN-DISCOUNT-KBNL
                                            (SPA-GMN-DISCOUNT-KBN-MAX )
               MOVE    WRK-RATE-AREA   TO  SPA-GMN-DISCOUNT-KBNMEIL
                                            (SPA-GMN-DISCOUNT-KBN-MAX )
      *
               MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       3101-DISCOUNT-KBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入金方法分類区分設定処理
      *****************************************************************
       3101-NYKN-BUNRUI-SET-SEC        SECTION.
      *
           MOVE    "01"                TO  SPA-GMN-NYKN-BUNRUIL (1)
           MOVE    "現金"              TO  SPA-GMN-NYKN-BUNRUIMEIL (1)
           MOVE    "02"                TO  SPA-GMN-NYKN-BUNRUIL (2)
           MOVE    "振込み"            TO  SPA-GMN-NYKN-BUNRUIMEIL (2)
           MOVE    "03"                TO  SPA-GMN-NYKN-BUNRUIL (3)
           MOVE    "デビットカード"    TO  SPA-GMN-NYKN-BUNRUIMEIL (3)
           MOVE    "99"                TO  SPA-GMN-NYKN-BUNRUIL (4)
           MOVE    "その他"            TO  SPA-GMN-NYKN-BUNRUIMEIL (4)
           MOVE    4                   TO  SPA-GMN-NYKN-BUNRUI-MAX
      *
           .
       3101-NYKN-BUNRUI-SET-EXT.
           EXIT.
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
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    タグ
               WHEN    "CLICKED"       ALSO    "N01"
               WHEN    "CLICKED"       ALSO    "N02"
               WHEN    "CLICKED"       ALSO    "N03"
               WHEN    "CLICKED"       ALSO    "N04"
               WHEN    "CLICKED"       ALSO    "N05"
               WHEN    "CLICKED"       ALSO    "N06"
                   PERFORM 440-TAG-CHANGE-SEC
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
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
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
      *    基本情報タグ
           MOVE    W02-PREFNUM         TO  SPA-GMN-PREFNUM-G
           MOVE    W02-TENHYOKBN       TO  SPA-GMN-TENHYOKBN-G
           MOVE    W02-HOSPCD          TO  SPA-GMN-HOSPCD
           MOVE    W02-HOSPSBT         TO  SPA-GMN-HOSPSBT-G
           MOVE    W02-HOSPID          TO  SPA-GMN-HOSPID
           MOVE    W02-HOSPNAME        TO  SPA-GMN-HOSPNAME
           MOVE    W02-KAISETUNAME     TO  SPA-GMN-KAISETUNAME
           MOVE    W02-KANRINAME       TO  SPA-GMN-KANRINAME
           MOVE    W02-BEDSU           TO  SPA-GMN-BEDSU
           MOVE    W02-BEDSUIPPN       TO  SPA-GMN-BEDSUIPN
           MOVE    W02-ROUPAYKBN       TO  SPA-GMN-ROUPAYKBN-G
           MOVE    W02-HOSPSBT1        TO  SPA-GMN-HOSPSBT1-G
           MOVE    W02-HOSPCDN         TO  SPA-GMN-HOSPCDN
           MOVE    W02-SYOHOKBN        TO  SPA-GMN-SYOHOKBN-G
           MOVE    W02-SKYPRTFLG       TO  SPA-GMN-SKYPRTFLG-G
           MOVE    W02-SYOHOPRTFLG     TO  SPA-GMN-SYOHOPRTFLG-G
           MOVE    W02-ZENSYOHOFLG     TO  SPA-GMN-ZENSYOHOFLG-G
           MOVE    W02-YAKUJYOFLG      TO  SPA-GMN-YAKUJYOFLG-G
           MOVE    W02-DISCOUNTHASU    TO  SPA-GMN-DISCOUNT-HASU-KBN-G
           MOVE    W02-DISCOUNTHASU2   TO  SPA-GMN-DISCOUNT-HASU-KBN2-G
           MOVE    W02-TAXHASU         TO  SPA-GMN-TAX-HASU-KBN-G
           MOVE    W02-HIWARIKBN       TO  SPA-GMN-HIWARIKBN-G
      *
      *    連絡先情報タグ
           MOVE    W02-POST            TO  SPA-GMN-POST
           MOVE    W02-ADRS            TO  SPA-GMN-ADRS
           MOVE    W02-TEL             TO  SPA-GMN-TEL
           MOVE    W02-FAX             TO  SPA-GMN-FAX
           MOVE    W02-EMAIL           TO  SPA-GMN-EMAIL
           MOVE    W02-HPURL           TO  SPA-GMN-HPURL
           MOVE    W02-TEIRITU         TO  SPA-GMN-TEIRITU-G
      *
           MOVE    W02-DISCOUNTRATE    TO  SPA-GMN-DISCOUNT-RATE
      *
           MOVE    W02-DISCOUNTBODY    TO  SPA-GMN-DISCOUNT-BODY
           MOVE    W02-DISCOUNTKBN     TO  SPA-GMN-DISCOUNT-KBN-G
           MOVE    W02-DISCOUNTOBJ     TO  SPA-GMN-DISCOUNT-OBJ-G
      *
           MOVE    W02-CONDITIONBODY   TO  SPA-GMN-CONDITION-BODY
           MOVE    W02-CONDITIONWORD   TO  SPA-GMN-CONDITION-WORD
           MOVE    W02-ICONFILENAME    TO  SPA-GMN-ICON-FILENAME
      *
           MOVE    W02-JIHINAME        TO  SPA-GMN-JIHINAME
      *
      *    診療内容
           MOVE    W02-SRYNAIYO        TO  SPA-GMN-SRYNAIYO
           MOVE    W02-SRYNAIYODSP     TO  SPA-GMN-SRYNAIYODSP
      *
      *    入金方法
           MOVE    W02-NYKN-KBNCD      TO  SPA-GMN-NYKN-KBNCD
           MOVE    W02-NYKN-NAME       TO  SPA-GMN-NYKN-NAME
           MOVE    W02-NYKN-TANNAME    TO  SPA-GMN-NYKN-TANNAME
           MOVE    W02-NYKN-BUNRUI     TO  SPA-GMN-NYKN-BUNRUI-G
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
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1001"
      *            医療機関情報ー基本情報
                   PERFORM 41021-KIHON-1001-SEC
               WHEN    "1002"
      *            医療機関情報ー所在地、連絡先情報
                   PERFORM 41021-KIHON-1002-SEC
               WHEN    "1003"
      *            医療機関情報ー広告情報
                   PERFORM 41021-KIHON-1003-SEC
               WHEN    "1013"
      *            医療機関情報ー請求書自費名称情報
                   PERFORM 41021-KIHON-1013-SEC
               WHEN    "1012"
      *            診療内容名称情報
                   PERFORM 41021-KIHON-1012-SEC
               WHEN    "1018"
               WHEN    "1019"
               WHEN    "1020"
      *            医療機関情報ー状態情報
                   PERFORM 41021-KIHON-1018-SEC
               WHEN    "1026"
      *             医療機関情報ー減免事由情報
                   PERFORM 41021-KIHON-1026-SEC
               WHEN    "1027"
      *             医療機関情報ー割引率情報
                   PERFORM 41021-KIHON-1027-SEC
               WHEN    "1041"
      *             医療機関情報ー入金方法情報
                   PERFORM 41021-KIHON-1041-SEC
           END-EVALUATE
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     医療機関情報ー基本情報基本チェック処理
      *****************************************************************
       41021-KIHON-1001-SEC       SECTION.
      *
           PERFORM 4100-PREFNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4101-TENHYOKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4102-HOSPCD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4103-HOSPSBT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4104-HOSPID-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4105-HOSPNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4106-KAISETUNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4107-KANRINAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4108-BEDSU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4109-ROUPAYKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4110-HOSPSBT1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
           PERFORM 4111-HOSPCDN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4118-SYOHOKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4126-SKYPRTFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4127-SYOHOPRTFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4128-ZENSYOHOFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4129-YAKUJYOFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4130-DISCOUNT-HASU-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4132-DISCOUNT-HASU-KBN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4131-TAX-HASU-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           PERFORM 4131-HIWARIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KIHON-1001-EXT
           END-IF
      *
           .
       41021-KIHON-1001-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報ー所在地、連絡先情報チェック処理
      *****************************************************************
       41021-KIHON-1002-SEC       SECTION.
      *
      *    住所チェック
           PERFORM 4113-ADRS-CHK-SEC
      *
           .
       41021-KIHON-1002-EXT.
           EXIT.
      *
      *****************************************************************
      *    広告情報タグ処理
      *****************************************************************
       41021-KIHON-1003-SEC       SECTION.
      *
           .
       41021-KIHON-1003-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書自費名称情報チェック処理
      *****************************************************************
       41021-KIHON-1013-SEC       SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-JIHINAME    TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-JIHINAME
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    70              TO  SPA-GMN-CUR
           END-IF
      *
           .
       41021-KIHON-1013-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容名称情報チェック処理
      *****************************************************************
       41021-KIHON-1012-SEC       SECTION.
      *
           .
       41021-KIHON-1012-EXT.
           EXIT.
      *            
      *
      *****************************************************************
      *    状態情報チェック処理
      *****************************************************************
       41021-KIHON-1018-SEC       SECTION.
      *
           .
       41021-KIHON-1018-EXT.
           EXIT.
      *
      *****************************************************************
      *    減免事由情報チェック処理
      *****************************************************************
       41021-KIHON-1026-SEC       SECTION.
      *
      *    割引率区分チェック処理
           PERFORM 4122-DISCOUNT-KBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41021-KIHON-1026-EXT
           END-IF
      *
      *    暫定 ST 2003.08.12
      *     割引対象チェック処理
      *     PERFORM 4122-DISCOUNT-OBJ-CHK-SEC
      *     IF      SPA-ERRCD       NOT =   SPACE
      *         GO  TO  41021-KIHON-1026-EXT
      *     END-IF
      *    暫定 ED
           .
       41021-KIHON-1026-EXT.
           EXIT.
      *
      *****************************************************************
      *    割引率情報タグ処理
      *****************************************************************
       41021-KIHON-1027-SEC       SECTION.
      *
      *    定額定率区分チェック処理
           PERFORM 4122-TEIRITU-CHK-SEC
      *
           .
       41021-KIHON-1027-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法情報タグ処理
      *****************************************************************
       41021-KIHON-1041-SEC       SECTION.
      *
      *    入金方法名称チェック
           PERFORM 4122-NYKN-NAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41021-KIHON-1041-EXT
           END-IF
      *
      *    入金方法短縮名称チェック
           PERFORM 4122-NYKN-TANNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41021-KIHON-1041-EXT
           END-IF
      *
      *    分類区分チェック
           PERFORM 4122-NYKN-BUNRUI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  41021-KIHON-1041-EXT
           END-IF
      *
           .
       41021-KIHON-1041-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県番号チェック
      *****************************************************************
       4100-PREFNUM-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       47
               IF      SPA-GMN-PREFNUM     =   SPA-GMN-PREFNUML (IDY)
                   MOVE    SPA-GMN-PREFNUMLST (IDY)
                                           TO  SPA-GMN-PREFNUM-G
                   MOVE    47              TO  IDY
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-PREFNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数表区分チェック
      *****************************************************************
       4101-TENHYOKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       1
               IF      SPA-GMN-TENHYOKBN   =   SPA-GMN-TENHYOKBNL (IDY)
                   MOVE    SPA-GMN-TENHYOKBNLST (IDY)
                                           TO  SPA-GMN-TENHYOKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-TENHYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関コードチェック
      *****************************************************************
       4102-HOSPCD-CHK-SEC     SECTION.
      *
           .
       4102-HOSPCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関種別チェック
      *****************************************************************
       4103-HOSPSBT-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-HOSPSBT     =   SPA-GMN-HOSPSBTL (IDY)
                   MOVE    SPA-GMN-HOSPSBTLST (IDY)
                                           TO  SPA-GMN-HOSPSBT-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-HOSPSBT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関ＩＤチェック
      *****************************************************************
       4104-HOSPID-CHK-SEC          SECTION.
           .
       4104-HOSPID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称チェック
      *****************************************************************
       4105-HOSPNAME-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-HOSPNAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-HOSPNAME
           END-IF
           .
       4105-HOSPNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開設者名チェック
      *****************************************************************
       4106-KAISETUNAME-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-KAISETUNAME TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-KAISETUNAME
           END-IF
      *
           .
       4106-KAISETUNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理者名チェック
      *****************************************************************
       4107-KANRINAME-CHK-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-KANRINAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-KANRINAME
           END-IF
      *
           .
       4107-KANRINAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病床数チェック
      *****************************************************************
       4108-BEDSU-CHK-SEC        SECTION.
           .
       4108-BEDSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人支払区分チェック
      *****************************************************************
       4109-ROUPAYKBN-CHK-SEC            SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-ROUPAYKBN   =   SPA-GMN-ROUPAYKBNL (IDY)
                   MOVE    SPA-GMN-ROUPAYKBNLST (IDY)
                                           TO  SPA-GMN-ROUPAYKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
           .
       4109-ROUPAYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧総合病院フラグチェック
      *****************************************************************
       4110-HOSPSBT1-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-HOSPSBT1    =   SPA-GMN-HOSPSBT1L (IDY)
                   MOVE    SPA-GMN-HOSPSBT1LST (IDY)
                                           TO  SPA-GMN-HOSPSBT1-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4110-HOSPSBT1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関コード（漢字）チェック
      *****************************************************************
       4111-HOSPCDN-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-HOSPCDN     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    12              TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-HOSPCDN
           END-IF
      *
           .
       4111-HOSPCDN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方区分チェック
      *****************************************************************
       4118-SYOHOKBN-CHK-SEC       SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-SYOHOKBN    =   SPA-GMN-SYOHOKBNL (IDY)
                   MOVE    SPA-GMN-SYOHOKBNLST (IDY)
                                           TO  SPA-GMN-SYOHOKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    32                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4118-SYOHOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行フラグチェック
      *****************************************************************
       4126-SKYPRTFLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       3
               IF      SPA-GMN-SKYPRTFLG    =   SPA-GMN-SKYPRTFLGL (IDY)
                   MOVE    SPA-GMN-SKYPRTFLGLST (IDY)
                                           TO  SPA-GMN-SKYPRTFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    33                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4126-SKYPRTFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方せん発行フラグ
      *****************************************************************
       4127-SYOHOPRTFLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       3
               IF      SPA-GMN-SYOHOPRTFLG =   SPA-GMN-SYOHOPRTFLGL
                                                              (IDY)
                   MOVE    SPA-GMN-SYOHOPRTFLGLST (IDY)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    34                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4127-SYOHOPRTFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回処方表示フラグ
      *****************************************************************
       4128-ZENSYOHOFLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-ZENSYOHOFLG =   SPA-GMN-ZENSYOHOFLGL
                                                              (IDY)
                   MOVE    SPA-GMN-ZENSYOHOFLGLST (IDY)
                                           TO  SPA-GMN-ZENSYOHOFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    35                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4128-ZENSYOHOFLG-CHK-EXT.
           EXIT.
      *
      *----(01.00.09) LINE ADD START ----------------------------------
      *****************************************************************
      *    薬剤情報発行フラグ
      *****************************************************************
       4129-YAKUJYOFLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-YAKUJYOFLG =   SPA-GMN-YAKUJYOFLGL
                                                              (IDY)
                   MOVE    SPA-GMN-YAKUJYOFLGLST (IDY)
                                           TO  SPA-GMN-YAKUJYOFLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    36                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4129-YAKUJYOFLG-CHK-EXT.
           EXIT.
      *
      *----(01.00.09) LINE ADD END   ----------------------------------
      *****************************************************************
      *    請求額端数設定（減免有）
      *****************************************************************
       4130-DISCOUNT-HASU-KBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
               IF      SPA-GMN-DISCOUNT-HASU-KBN
                                       =   SPA-GMN-DISCOUNT-HASU-KBNL
                                                              (IDY)
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBNLST (IDY)
                                       TO  SPA-GMN-DISCOUNT-HASU-KBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    38              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4130-DISCOUNT-HASU-KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求額端数設定（減免無）
      *****************************************************************
       4132-DISCOUNT-HASU-KBN2-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
               IF      SPA-GMN-DISCOUNT-HASU-KBN2
                                   =   SPA-GMN-DISCOUNT-HASU-KBN2L
                                                              (IDY)
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBN2LST (IDY)
                                   TO  SPA-GMN-DISCOUNT-HASU-KBN2-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    201             TO  SPA-GMN-CUR
           END-IF
      *
           .
       4132-DISCOUNT-HASU-KBN2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    消費税端数設定
      *****************************************************************
       4131-TAX-HASU-KBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-TAX-HASU-KBN
                                       =   SPA-GMN-TAX-HASU-KBNL
                                                              (IDY)
                   MOVE    SPA-GMN-TAX-HASU-KBNLST (IDY)
                                       TO  SPA-GMN-TAX-HASU-KBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    39              TO  SPA-GMN-CUR
           END-IF
      *
           .
       4131-TAX-HASU-KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    更正・育成限度額日割計算区分
      *****************************************************************
       4131-HIWARIKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               IF      SPA-GMN-HIWARIKBN
                                       =   SPA-GMN-HIWARIKBNL
                                                              (IDY)
                   MOVE    SPA-GMN-HIWARIKBNLST (IDY)
                                       TO  SPA-GMN-HIWARIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0021"          TO  SPA-ERRCD
               MOVE    202             TO  SPA-GMN-CUR
           END-IF
      *
           .
       4131-HIWARIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    郵便番号チェック処理
      *****************************************************************
       4112-POST-CHK-SEC           SECTION.
      *
           .
       4112-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       4113-ADRS-CHK-SEC        SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-ADRS        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-ADRS
           END-IF
     *
           . 
       4113-ADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    割引率区分チェック処理
      *****************************************************************
       4122-DISCOUNT-KBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( FLG-OK      =   1 )
                    OR   ( IDZ         >   SPA-GMN-DISCOUNT-KBN-MAX )
               IF    ( SPA-GMN-DISCOUNT-KBN
                                   =   SPA-GMN-DISCOUNT-KBNL (IDZ) )
                   MOVE    SPA-GMN-DISCOUNT-KBNLST (IDZ)
                                   TO  SPA-GMN-DISCOUNT-KBN-G
                   MOVE    1       TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    51                  TO  SPA-GMN-CUR
               GO  TO  4122-DISCOUNT-KBN-CHK-EXT
           END-IF
      *
           .
       4122-DISCOUNT-KBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    割引率区分チェック処理
      *****************************************************************
       4122-DISCOUNT-OBJ-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( FLG-OK      =   1 )
                    OR   ( IDZ     >   3 )
               IF    ( SPA-GMN-DISCOUNT-OBJ
                                   =   SPA-GMN-DISCOUNT-OBJL (IDZ) )
                   MOVE    SPA-GMN-DISCOUNT-OBJLST (IDZ)
                                   TO  SPA-GMN-DISCOUNT-OBJ-G
                   MOVE    1       TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    52                  TO  SPA-GMN-CUR
               GO  TO  4122-DISCOUNT-OBJ-CHK-EXT
           END-IF
      *
           .
       4122-DISCOUNT-OBJ-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    割引率区分チェック処理
      *****************************************************************
       4122-TEIRITU-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-TEIRITU-G   =   SPACE )
               MOVE    ZERO        TO  SPA-GMN-DISCOUNT-RATE
               MOVE    SPACE       TO  SPA-GMN-LBL-RATE
               GO  TO  4122-TEIRITU-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-LBL-RATE
           EVALUATE    SPA-GMN-TEIRITU
           WHEN    "1"
               MOVE    SPA-GMN-TEIRITULST (1)
                                       TO  SPA-GMN-TEIRITU-G
               MOVE    "％"            TO  SPA-GMN-LBL-RATE
           WHEN    "2"
               MOVE    SPA-GMN-TEIRITULST (2)
                                       TO  SPA-GMN-TEIRITU-G
               MOVE    "円"            TO  SPA-GMN-LBL-RATE
           WHEN    OTHER
               MOVE    "0015"          TO  SPA-ERRCD
               MOVE    41              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       4122-TEIRITU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法名称チェック処理
      *****************************************************************
       4122-NYKN-NAME-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-NYKN-NAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0017"          TO  SPA-ERRCD
               MOVE    90              TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-NYKN-NAME
           END-IF
      *
           .
       4122-NYKN-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法短縮名称チェック処理
      *****************************************************************
       4122-NYKN-TANNAME-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-NYKN-TANNAME
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0018"          TO  SPA-ERRCD
               MOVE    91              TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  SPA-GMN-NYKN-TANNAME
           END-IF
      *
           .
       4122-NYKN-TANNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分類区分チェック処理
      *****************************************************************
       4122-NYKN-BUNRUI-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-NYKN-BUNRUI-MAX )
                    OR   ( FLG-OK  =       1 )
      *
               IF    ( SPA-GMN-NYKN-BUNRUI
                                   =   SPA-GMN-NYKN-BUNRUIL (IDX1))
                   MOVE    SPA-GMN-NYKN-BUNRUILST (IDX1)
                                           TO  SPA-GMN-NYKN-BUNRUI-G
                   MOVE    1               TO  FLG-OK
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-OK          =   ZERO )
               MOVE    SPA-GMN-NYKN-BUNRUILST (1)
                                           TO  SPA-GMN-NYKN-BUNRUI-G
           END-IF
      *
           .
       4122-NYKN-BUNRUI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    タグ編集処理
      *****************************************************************
       440-TAG-CHANGE-SEC            SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "N01"
                   MOVE   0            TO  IDX
               WHEN    "N02"
                  MOVE    1            TO  IDX
               WHEN    "N03"
                   MOVE   2            TO  IDX
           END-EVALUATE
      *
      *    タグが変わったとき関連チェック
           IF      IDX              NOT =   W02-PAGENO
               PERFORM 4900-KANRENCHK-SEC
           END-IF
           IF      SPA-ERRCD            =   SPACE
               EVALUATE    IDX
                   WHEN    0
                       MOVE    1            TO  SPA-GMN-CUR
                   WHEN    1
                       MOVE    20           TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    30           TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF        
           .
       440-TAG-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W02"               TO  SPA-MOTOPG
      *
           INITIALIZE                  SPA-W01KYOUTU
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
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
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
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-W01-KANRICD         TO  SYS-1001-KANRICD
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1013"
               WHEN    "1012"
               WHEN    "1018"
               WHEN    "1019"
               WHEN    "1020"
               WHEN    "1026"
               WHEN    "1027"
               WHEN    "1041"
                   MOVE    SPA-W01-KBNCD       TO  SYS-1001-KBNCD
               WHEN    OTHER
                   MOVE    "*"                 TO  SYS-1001-KBNCD
           END-EVALUATE
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1001-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC            TO  MCPDATA-REC
      *****MOVE    SPA-W01-STYUKYMD        TO  ORC-DBYMD
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1001-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-WIDCD
      *        基本情報の期間変更の場合
               IF      SPA-1001-ADD     =   1
                   MOVE    "1011"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      * 
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-W01-KANRICD         TO  SYS-1001-KANRICD
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1013"
               WHEN    "1012"
               WHEN    "1018"
               WHEN    "1019"
               WHEN    "1020"
               WHEN    "1026"
               WHEN    "1027"
               WHEN    "1041"
                   MOVE    SPA-W01-KBNCD       TO  SYS-1001-KBNCD
               WHEN    OTHER
                   MOVE    "*"                 TO  SYS-1001-KBNCD
           END-EVALUATE
           MOVE    SPA-W01-STYUKYMD        TO  ORC-DBYMD
           MOVE    SYS-1001-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           EVALUATE    SPA-W01-KANRICD
      *    医療機関情報ー基本情報
               WHEN    "1001"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1001-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1001-REC
                       INITIALIZE                  SYS-1001-REC
                       MOVE   "1001"           TO  SYS-1001-KANRICD
                       MOVE    "*"             TO  SYS-1001-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1001-EDYUKYMD
                   MOVE    SPA-GMN-PREFNUM     TO  SYS-1001-PREFNUM 
                   MOVE    SPA-GMN-TENHYOKBN   TO  SYS-1001-TENHYOKBN 
                   MOVE    SPA-GMN-HOSPCD      TO  SYS-1001-HOSPCD 
                   MOVE    SPA-GMN-HOSPSBT     TO  SYS-1001-HOSPSBT 
                   MOVE    SPA-GMN-HOSPID      TO  SYS-1001-HOSPID 
                   MOVE    SPA-GMN-HOSPNAME    TO  SYS-1001-HOSPNAME 
                   MOVE    SPA-GMN-KAISETUNAME TO  SYS-1001-KAISETUNAME
                   MOVE    SPA-GMN-KANRINAME   TO  SYS-1001-KANRINAME 
                   MOVE    SPA-GMN-BEDSU       TO  SYS-1001-BEDSU 
                   MOVE    SPA-GMN-BEDSUIPN    TO  SYS-1001-BEDSUIPN
                   MOVE    SPA-GMN-ROUPAYKBN   TO  SYS-1001-ROUPAYKBN 
                   MOVE    SPA-GMN-HOSPSBT1    TO  SYS-1001-HOSPSBT1 
                   MOVE    SPA-GMN-HOSPCDN     TO  SYS-1001-HOSPCDN
                   MOVE    SPA-GMN-SYOHOKBN    TO  SYS-1001-SYOHOKBN
                   MOVE    SPA-GMN-SKYPRTFLG   TO  SYS-1001-SKYPRTFLG
                   MOVE    SPA-GMN-SYOHOPRTFLG TO  SYS-1001-SYOHOPRTFLG
                   MOVE    SPA-GMN-ZENSYOHOFLG TO  SYS-1001-ZENSYOHOFLG
      *----(01.00.09) LINE ADD START ----------------------------------
                   MOVE    SPA-GMN-YAKUJYOFLG
                                       TO  SYS-1001-YAKUJYOPRTFLG
      *----(01.00.09) LINE ADD END   ----------------------------------
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBN
                                       TO  SYS-1001-DISCOUNT-HASU-KBN
                   MOVE    SPA-GMN-DISCOUNT-HASU-KBN2
                                       TO  SYS-1001-DISCOUNT-HASU-KBN2
                   MOVE    SPA-GMN-TAX-HASU-KBN
                                       TO  SYS-1001-TAX-HASU-KBN
                   MOVE    SPA-GMN-HIWARIKBN
                                       TO  SYS-1001-HIWARIKBN
                   MOVE    SYS-1001-REC    TO  SYSKANRI-REC
      * 
      *    医療機関情報ー所在地、連絡先情報
               WHEN    "1002"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1002-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1002-REC
                       INITIALIZE                  SYS-1002-REC
                       MOVE   "1002"           TO  SYS-1002-KANRICD
                       MOVE    "*"             TO  SYS-1002-KBNCD 
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1002-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1002-EDYUKYMD
                   MOVE    SPA-GMN-POST        TO  SYS-1002-POST 
                   MOVE    SPA-GMN-ADRS        TO  SYS-1002-ADRS 
                   MOVE    SPA-GMN-TEL         TO  SYS-1002-TEL 
                   MOVE    SPA-GMN-FAX         TO  SYS-1002-FAX
                   MOVE    SYS-1002-REC        TO  SYSKANRI-REC
      * 
      *    医療機関情報ー広告情報
               WHEN    "1003"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1003-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1003-REC
                       INITIALIZE                  SYS-1003-REC
                       MOVE   "1003"           TO  SYS-1003-KANRICD
                       MOVE    "*"             TO  SYS-1003-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1003-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1003-EDYUKYMD
                   MOVE    SPA-GMN-EMAIL       TO  SYS-1003-EMAIL 
                   MOVE    SPA-GMN-HPURL       TO  SYS-1003-HPURL 
                   MOVE    SYS-1003-REC        TO  SYSKANRI-REC
      * 
      *    医療機関情報ー請求書自費名称情報
               WHEN    "1013"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1013-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1013-REC
                       INITIALIZE                  SYS-1013-REC
                       MOVE   "1013"           TO  SYS-1013-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1013-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1013-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1013-EDYUKYMD
                   MOVE    SPA-GMN-JIHINAME    TO  SYS-1013-JIHINAME
                   MOVE    SYS-1013-REC        TO  SYSKANRI-REC
      * 
      * 
      *    診療内容名称情報
               WHEN    "1012"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1012-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1012-REC
                       INITIALIZE                  SYS-1012-REC
                       MOVE   "1012"           TO  SYS-1012-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1012-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1012-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1012-EDYUKYMD
                   MOVE    SPA-GMN-SRYNAIYO    TO  SYS-1012-SRYNAIYO
                   MOVE    SPA-GMN-SRYNAIYODSP TO  SYS-1012-SRYNAIYODSP
                   MOVE    SYS-1012-REC        TO  SYSKANRI-REC
      * 
      *    医療機関情報ー状態情報
               WHEN    "1018"    
               WHEN    "1019"    
               WHEN    "1020"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1018-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1018-REC
                       INITIALIZE                  SYS-1018-REC
                       MOVE    SPA-W01-KANRICD TO  SYS-1018-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1018-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1018-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1018-EDYUKYMD
                   MOVE    SPA-GMN-CONDITION-BODY
                                           TO  SYS-1018-CONDITION-BODY
                   MOVE    SPA-GMN-CONDITION-WORD
                                           TO  SYS-1018-CONDITION-WORD
                   MOVE    SPA-GMN-ICON-FILENAME
                                           TO  SYS-1018-ICON-FILENAME 
                   MOVE    SYS-1018-REC        TO  SYSKANRI-REC
      * 
      *    医療機関情報ー減免事由情報
               WHEN    "1026"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1026-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1026-REC
                       INITIALIZE                  SYS-1026-REC
                       MOVE   "1026"           TO  SYS-1026-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1026-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1026-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1026-EDYUKYMD
                   MOVE    SPA-GMN-DISCOUNT-BODY
                                           TO  SYS-1026-DISCOUNT-BODY 
                   MOVE    SPA-GMN-DISCOUNT-KBN
                                           TO  SYS-1026-DISCOUNT-KBN 
      *    暫定 ST 2003.08.12
      *            MOVE    SPA-GMN-DISCOUNT-OBJ
      *                                    TO  SYS-1026-DISCOUNT-OBJ 
      *    暫定 ED
                   MOVE    SYS-1026-REC        TO  SYSKANRI-REC
      * 
      *    医療機関情報ー割引率情報
               WHEN    "1027"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1027-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1027-REC
                       INITIALIZE                  SYS-1027-REC
                       MOVE   "1027"           TO  SYS-1027-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1027-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1027-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1027-EDYUKYMD
                   IF    ( SPA-GMN-TEIRITU     =   SPACE )
                       MOVE    "0"             TO  SYS-1027-TEIRITU 
                   ELSE
                       MOVE    SPA-GMN-TEIRITU TO  SYS-1027-TEIRITU 
                   END-IF
                   MOVE    SPA-GMN-DISCOUNT-RATE
                                           TO  SYS-1027-DISCOUNT-RATE
                   MOVE    SYS-1027-REC        TO  SYSKANRI-REC
      *    入金方法情報
               WHEN    "1041"    
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1041-REC
                   ELSE
                       MOVE    SPACE           TO  SYS-1041-REC
                       INITIALIZE                  SYS-1041-REC
                       MOVE   "1041"           TO  SYS-1041-KANRICD
                       MOVE    SPA-W01-KBNCD   TO  SYS-1041-KBNCD
                   END-IF 
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-1041-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD    TO  SYS-1041-EDYUKYMD
                   MOVE    SPA-GMN-NYKN-NAME   TO  SYS-1041-NYKN-NAME
                   MOVE    SPA-GMN-NYKN-TANNAME
                                               TO  SYS-1041-NYKN-TANNAME
                   MOVE    SPA-GMN-NYKN-BUNRUI
                                               TO  SYS-1041-NYKN-BUNRUI
                   MOVE    SYS-1041-REC        TO  SYSKANRI-REC
           END-EVALUATE 
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "SYSKANRI-UPD1"         TO  ORC-DBPATH
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1001"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF      
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報で期間変更の時、所在地・広告変更処理
      *****************************************************************
       4901-KIHON-HENKOU-SEC           SECTION.
      *
           .
       4901-KIHON-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
      *
           EVALUATE    W02-PAGENO
               WHEN    0
                   PERFORM 4901-TAG1-CHK-SEC
               WHEN    1
                   PERFORM 4901-TAG2-CHK-SEC
               WHEN    2
                   PERFORM 4901-TAG3-CHK-SEC
               WHEN    3
                   PERFORM 4901-TAG4-CHK-SEC
               WHEN    4
                   PERFORM 4901-TAG5-CHK-SEC
               WHEN    5
                   PERFORM 4901-TAG6-CHK-SEC
               WHEN    8
                   PERFORM 4901-TAG9-CHK-SEC
           END-EVALUATE    
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       基本情報タグの関連チェック処理
      *****************************************************************
       4901-TAG1-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-HOSPID      =   ZERO    
           OR      SPA-GMN-HOSPSBT     =   ZERO    
               MOVE  "0099"                TO  SPA-ERRCD
               GO  TO  4901-TAG1-CHK-EXT
           END-IF
      *
           .
       4901-TAG1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       連絡先情報タグの関連チェック処理
      *****************************************************************
       4901-TAG2-CHK-SEC           SECTION.
      *
           .
       4901-TAG2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       広告情報タグの関連チェック処理
      *****************************************************************
       4901-TAG3-CHK-SEC           SECTION.
      *
           .
       4901-TAG3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       連絡先情報タグの関連チェック処理
      *****************************************************************
       4901-TAG4-CHK-SEC           SECTION.
      *
           .
       4901-TAG4-CHK-EXT.
           EXIT.

      *
      *****************************************************************
      *       広告情報タグの関連チェック処理
      *****************************************************************
       4901-TAG5-CHK-SEC           SECTION.
      *
           .
       4901-TAG5-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       連絡先情報タグの関連チェック処理
      *****************************************************************
       4901-TAG6-CHK-SEC           SECTION.
      *
           .
       4901-TAG6-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       入金方法情報タグの関連チェック処理
      *****************************************************************
       4901-TAG9-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-NYKN-NAME       =   SPACE )
               MOVE    90              TO  SPA-GMN-CUR
               MOVE    "0099"          TO  SPA-ERRCD
               GO  TO  4901-TAG9-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-NYKN-TANNAME    =   SPACE )
               MOVE    91              TO  SPA-GMN-CUR
               MOVE    "0099"          TO  SPA-ERRCD
               GO  TO  4901-TAG9-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-NYKN-BUNRUI-G   =   SPACE )
               MOVE    92              TO  SPA-GMN-CUR
               MOVE    "0099"          TO  SPA-ERRCD
               GO  TO  4901-TAG9-CHK-EXT
           END-IF
      *
           .
       4901-TAG9-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
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
           MOVE    "W02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W02-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W02-EDYUKYMD
      *
      *            医療機関情報ー基本情報
           MOVE    SPA-GMN-PREFNUM-G   TO  W02-PREFNUM
           MOVE    SPA-GMN-TENHYOKBN-G TO  W02-TENHYOKBN
           MOVE    SPA-GMN-HOSPCD      TO  W02-HOSPCD
           MOVE    SPA-GMN-HOSPSBT-G   TO  W02-HOSPSBT
           MOVE    SPA-GMN-HOSPID      TO  W02-HOSPID
           MOVE    SPA-GMN-HOSPNAME    TO  W02-HOSPNAME
           MOVE    SPA-GMN-KAISETUNAME TO  W02-KAISETUNAME
           MOVE    SPA-GMN-KANRINAME   TO  W02-KANRINAME
           MOVE    SPA-GMN-BEDSU       TO  W02-BEDSU  
           MOVE    SPA-GMN-BEDSUIPN    TO  W02-BEDSUIPPN
           MOVE    SPA-GMN-ROUPAYKBN-G TO  W02-ROUPAYKBN
           MOVE    SPA-GMN-HOSPSBT1-G  TO  W02-HOSPSBT1
           MOVE    SPA-GMN-HOSPCDN     TO  W02-HOSPCDN
           MOVE    SPA-GMN-SYOHOKBN-G  TO  W02-SYOHOKBN
           MOVE    SPA-GMN-SKYPRTFLG-G TO  W02-SKYPRTFLG
           MOVE    SPA-GMN-SYOHOPRTFLG-G
                                       TO  W02-SYOHOPRTFLG
           MOVE    SPA-GMN-ZENSYOHOFLG-G
                                       TO  W02-ZENSYOHOFLG
      *----(01.00.09) LINE ADD START ----------------------------------
           MOVE    SPA-GMN-YAKUJYOFLG-G
                                       TO  W02-YAKUJYOFLG
      *----(01.00.09) LINE ADD END   ----------------------------------
           MOVE    SPA-GMN-DISCOUNT-HASU-KBN-G
                                       TO  W02-DISCOUNTHASU
           MOVE    SPA-GMN-DISCOUNT-HASU-KBN2-G
                                       TO  W02-DISCOUNTHASU2
           MOVE    SPA-GMN-TAX-HASU-KBN-G
                                       TO  W02-TAXHASU
           MOVE    SPA-GMN-HIWARIKBN-G
                                       TO  W02-HIWARIKBN
      *     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       47
               MOVE    SPA-GMN-PREFNUMLST (IDX)
                                       TO  W02-PREFNUMLST   (IDX)
           END-PERFORM
           MOVE    47                  TO  W02-PREFNUM-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       1
               MOVE    SPA-GMN-TENHYOKBNLST (IDX)
                                       TO  W02-TENHYOKBNLST (IDX)
           END-PERFORM    
           MOVE    1                   TO  W02-TENHYOKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-HOSPSBTLST(IDX)
                                       TO  W02-HOSPSBTLST   (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-HOSPSBT-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-ROUPAYKBNLST(IDX)
                                       TO  W02-ROUPAYKBNLST (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-ROUPAYKBN-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-HOSPSBT1LST(IDX)
                                       TO  W02-HOSPSBT1LST  (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-HOSPSBT1-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-SYOHOKBNLST(IDX)
                                       TO  W02-SYOHOKBNLST  (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-SYOHOKBN-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               MOVE    SPA-GMN-SKYPRTFLGLST(IDX)
                                       TO  W02-SKYPRTFLGLST  (IDX)
           END-PERFORM    
           MOVE    3                   TO  W02-SKYPRTFLG-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               MOVE    SPA-GMN-SYOHOPRTFLGLST(IDX)
                                       TO  W02-SYOHOPRTFLGLST  (IDX)
           END-PERFORM    
           MOVE    3                   TO  W02-SYOHOPRTFLG-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-ZENSYOHOFLGLST(IDX)
                                       TO  W02-ZENSYOHOFLGLST  (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-ZENSYOHOFLG-COUNT     
      *----(01.00.09) LINE ADD START ----------------------------------
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-YAKUJYOFLGLST(IDX)
                                       TO  W02-YAKUJYOFLGLST  (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-YAKUJYOFLG-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    SPA-GMN-DISCOUNT-HASU-KBNLST(IDX)
                                       TO  W02-DISCOUNTHASULST (IDX)
           END-PERFORM    
           MOVE    4                   TO  W02-DISCOUNTHASU-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    SPA-GMN-DISCOUNT-HASU-KBN2LST(IDX)
                                       TO  W02-DISCOUNTHASU2LST (IDX)
           END-PERFORM    
           MOVE    4                   TO  W02-DISCOUNTHASU2-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-TAX-HASU-KBNLST(IDX)
                                       TO  W02-TAXHASULST (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-TAXHASU-COUNT     
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-HIWARIKBNLST(IDX)
                                       TO  W02-HIWARIKBNLST (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-HIWARIKBN-COUNT     
      *----(01.00.09) LINE ADD END   ----------------------------------
      *
      *    医療機関情報ー所在地、連絡先情報
           MOVE    SPA-GMN-POST         TO  W02-POST
           MOVE    SPA-GMN-ADRS         TO  W02-ADRS  
           MOVE    SPA-GMN-TEL          TO  W02-TEL
           MOVE    SPA-GMN-FAX          TO  W02-FAX
      *
      *    医療機関情報ー広告情報
           MOVE    SPA-GMN-EMAIL        TO  W02-EMAIL
           MOVE    SPA-GMN-HPURL        TO  W02-HPURL
      *
      *    医療機関情報ー減免事由情報
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               MOVE    SPA-GMN-TEIRITULST(IDX)
                                       TO  W02-TEIRITULST (IDX)
           END-PERFORM    
           MOVE    2                   TO  W02-TEIRITU-COUNT     
           MOVE    SPA-GMN-TEIRITU-G        TO  W02-TEIRITU
           MOVE    SPA-GMN-DISCOUNT-RATE    TO  W02-DISCOUNTRATE
           MOVE    SPA-GMN-LBL-RATE         TO  W02-LBL-RATE
      *
      *    医療機関情報ー割引率情報
           MOVE    SPA-GMN-DISCOUNT-KBN-MAX
                                       TO  W02-DISCOUNTKBN-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-DISCOUNT-KBN-MAX
               MOVE    SPA-GMN-DISCOUNT-KBNLST (IDX)
                                       TO  W02-DISCOUNTKBNLST (IDX)
           END-PERFORM    
           MOVE    SPA-GMN-DISCOUNT-BODY    TO  W02-DISCOUNTBODY
           MOVE    SPA-GMN-DISCOUNT-KBN-G   TO  W02-DISCOUNTKBN
           MOVE    3                   TO  W02-DISCOUNTOBJ-COUNT
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       3 )
               MOVE    SPA-GMN-DISCOUNT-OBJLST (IDX)
                                       TO  W02-DISCOUNTOBJLST (IDX)
           END-PERFORM    
           MOVE    SPA-GMN-DISCOUNT-OBJ-G   TO  W02-DISCOUNTOBJ
      *
      *    医療機関情報ー状態コメント情報
           MOVE    SPA-GMN-CONDITION-BODY   TO  W02-CONDITIONBODY
           MOVE    SPA-GMN-CONDITION-WORD   TO  W02-CONDITIONWORD
           MOVE    SPA-GMN-ICON-FILENAME    TO  W02-ICONFILENAME 
      *
      *    請求書自費名称情報
           MOVE    SPA-GMN-JIHINAME     TO  W02-JIHINAME
      *
      *    診療内容名称情報
           MOVE    SPA-GMN-SRYNAIYO    TO  W02-SRYNAIYO
           MOVE    SPA-GMN-SRYNAIYODSP TO  W02-SRYNAIYODSP
      *
      *    入金方法情報
           MOVE    SPA-GMN-NYKN-KBNCD      TO  W02-NYKN-KBNCD
           MOVE    SPA-GMN-NYKN-NAME       TO  W02-NYKN-NAME
           MOVE    SPA-GMN-NYKN-TANNAME    TO  W02-NYKN-TANNAME
           MOVE    SPA-GMN-NYKN-BUNRUI-G   TO  W02-NYKN-BUNRUI
           MOVE    SPA-GMN-NYKN-BUNRUI-MAX TO  W02-NYKN-BUNRUI-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYKN-BUNRUI-MAX )
      *
               MOVE    SPA-GMN-NYKN-BUNRUILST (IDX1)
                                           TO  W02-NYKN-BUNRUILST (IDX1)
      *
           END-PERFORM
      *
      *    画面カーソルセット処理    
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
           MOVE    WIDGET-INSENSITIVE      TO  W02-N01-STATE
                                               W02-N02-STATE
                                               W02-N03-STATE
                                               W02-N04-STATE
                                               W02-N05-STATE
                                               W02-N06-STATE
                                               W02-N07-STATE
                                               W02-N08-STATE
                                               W02-N09-STATE
                                               W02-B03-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO    
               MOVE    WIDGET-NORMAL      TO  W02-B03-STATE
           END-IF 
      *             
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1001" 
                   MOVE    WIDGET-NORMAL      TO  W02-N01-STATE
                   IF      SPA-W01-STYUKYMD   =   ZERO
                       MOVE    WIDGET-INSENSITIVE TO  W02-B03-STATE
                   END-IF
               WHEN    "1002" 
                   MOVE    WIDGET-NORMAL      TO  W02-N02-STATE
                   IF      SPA-W01-STYUKYMD   =   ZERO
                       MOVE    WIDGET-INSENSITIVE TO  W02-B03-STATE
                   END-IF
               WHEN    "1003" 
                   MOVE    WIDGET-NORMAL      TO  W02-N03-STATE
                   IF      SPA-W01-STYUKYMD   =   ZERO
                       MOVE    WIDGET-INSENSITIVE TO  W02-B03-STATE
                   END-IF
               WHEN    "1026" 
                   MOVE    WIDGET-NORMAL      TO  W02-N05-STATE
               WHEN    "1027" 
                   MOVE    WIDGET-NORMAL      TO  W02-N04-STATE
               WHEN    "1018" 
               WHEN    "1019" 
               WHEN    "1020" 
                   MOVE    WIDGET-NORMAL      TO  W02-N06-STATE
               WHEN    "1013" 
                   MOVE    WIDGET-NORMAL      TO  W02-N07-STATE
               WHEN    "1012" 
                   MOVE    WIDGET-NORMAL      TO  W02-N08-STATE
               WHEN    "1041" 
                   MOVE    WIDGET-NORMAL      TO  W02-N09-STATE
           END-EVALUATE
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
                   MOVE    "都道府県番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "点数表入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "医療機関種別入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "老人支払区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "旧総合病院フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "院外処方区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "請求書発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "院外処方せん発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "前回処方表示フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "割引率区分が登録されていません"
                                               TO  SPA-ERRMSG
      *----(01.00.09) LINE ADD START ----------------------------------
               WHEN    "0011"
                   MOVE    "薬剤情報発行フラグ入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "請求金端数設定（減免有）入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "消費税端数設定入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "定率定額区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "割引対象入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "入金方法名称入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "入金方法短縮名称入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "分類区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "請求金端数設定（減免無）入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "更正・育成限度額日割計算区分入力エラー"
                                               TO  SPA-ERRMSG
      *----(01.00.09) LINE ADD END   ----------------------------------
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新処理ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W02"                TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"    TO  WRK-WIDMSG
               WHEN    "1011"
                   MOVE    "基本情報を追加登録します。"
                                                 TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W02"                TO  SPA-MOTOPG
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
                WHEN    01
                    MOVE  "PREFNUM"        TO   MCP-WIDGET
                WHEN    02
                    MOVE  "TENHYOKBN"      TO   MCP-WIDGET
                WHEN    03
                    MOVE  "HOSPCD"         TO   MCP-WIDGET
                WHEN    04
                    MOVE  "HOSPSBT"        TO   MCP-WIDGET
                WHEN    05
                    MOVE  "HOSPID"         TO   MCP-WIDGET
                WHEN    06
                    MOVE  "HOSPNAME"       TO   MCP-WIDGET
                WHEN    07
                    MOVE  "KAISETUNAME"    TO   MCP-WIDGET
                WHEN    08
                    MOVE  "KANRINAME"      TO   MCP-WIDGET
                WHEN    09
                    MOVE  "BEDSU"          TO   MCP-WIDGET
                WHEN    37
                    MOVE  "BEDSUIPPN"      TO   MCP-WIDGET
                WHEN    10
                    MOVE  "ROUPAYKBN"      TO   MCP-WIDGET
                WHEN    11
                    MOVE  "HOSPSBT1"       TO   MCP-WIDGET
                WHEN    12
                    MOVE  "HOSPCDN"        TO   MCP-WIDGET
                WHEN    20
                    MOVE  "POST"           TO   MCP-WIDGET
                WHEN    21
                    MOVE  "ADRS"           TO   MCP-WIDGET
                WHEN    22
                    MOVE  "TEL"            TO   MCP-WIDGET
                WHEN    23
                    MOVE  "FAX"            TO   MCP-WIDGET
                WHEN    30
                    MOVE  "EMAIL"          TO   MCP-WIDGET
                WHEN    31
                    MOVE  "HPURL"          TO   MCP-WIDGET
                WHEN    32
                    MOVE  "SYOHOKBN"       TO   MCP-WIDGET
                WHEN    33
                    MOVE  "SKYPRTFLG"      TO   MCP-WIDGET
                WHEN    34
                    MOVE  "SYOHOPRTFLG"    TO   MCP-WIDGET
                WHEN    35
                    MOVE  "ZENSYOHOFLG"    TO   MCP-WIDGET
      *----(01.00.09) LINE ADD START ----------------------------------
                WHEN    36
                    MOVE  "YAKUJYOFLG"     TO   MCP-WIDGET
      *----(01.00.09) LINE ADD END   ----------------------------------
                WHEN    38
                    MOVE  "DISCOUNTHASU"
                                           TO   MCP-WIDGET
                WHEN    201
                    MOVE  "DISCOUNTHASU2"
                                           TO   MCP-WIDGET
                WHEN    39
                    MOVE  "TAXHASU"
                                           TO   MCP-WIDGET
                WHEN    202
                    MOVE  "HIWARIKBN"
                                           TO   MCP-WIDGET
                WHEN    40
                    MOVE  "TEIRITU"        TO   MCP-WIDGET
                WHEN    41
                    MOVE  "DISCOUNTRATE"   TO   MCP-WIDGET
                WHEN    50
                    MOVE  "DISCOUNTBODY"   TO   MCP-WIDGET
                WHEN    51
                    MOVE  "DISCOUNTKBN"    TO   MCP-WIDGET
                WHEN    52
      * 暫定 ST 2003.08.12
      *             MOVE  "DISCOUNTOBJ"    TO   MCP-WIDGET
                    MOVE  "DISCOUNTBODY"   TO   MCP-WIDGET
      * 暫定 ED
                WHEN    60
                    MOVE  "CONDITIONBODY"  TO   MCP-WIDGET
                WHEN    61
                    MOVE  "CONDITIONWORD"  TO   MCP-WIDGET
                WHEN    62
                    MOVE  "ICONFILENAME"   TO   MCP-WIDGET
                WHEN    70
                    MOVE  "JIHINAME"       TO   MCP-WIDGET
                WHEN    80
                    MOVE  "SRYNAIYO"       TO   MCP-WIDGET
                WHEN    81
                    MOVE  "SRYNAIYODSP"    TO   MCP-WIDGET
      *
                WHEN    90
                    MOVE  "NYKN_NAME"      TO   MCP-WIDGET
                WHEN    91
                    MOVE  "NYKN_TANNAME"   TO   MCP-WIDGET
                WHEN    92
                    MOVE  "NYKN_BUNRUI"    TO   MCP-WIDGET
            END-EVALUATE
      *
            EVALUATE    SPA-GMN-CUR
                WHEN    1   THRU    19
      *----(01.00.09) LINE ADD START ----------------------------------
                WHEN    32  THRU    39
      *          WHEN    32  THRU    33
                WHEN    201 THRU    209
                    MOVE    0                TO  W02-PAGENO
      *----(01.00.09) LINE ADD END   ----------------------------------
                WHEN    20  THRU    29
                    MOVE    1                TO  W02-PAGENO
                WHEN    30  THRU    31
                    MOVE    2                TO  W02-PAGENO
                WHEN    40  THRU    49
                    MOVE    3                TO  W02-PAGENO
                WHEN    50  THRU    59
                    MOVE    4                TO  W02-PAGENO
                WHEN    60  THRU    69
                    MOVE    5                TO  W02-PAGENO
                WHEN    70  THRU    79
                    MOVE    6                TO  W02-PAGENO
                WHEN    80  THRU    89
                    MOVE    7                TO  W02-PAGENO
                WHEN    90  THRU    99
                    MOVE    8                TO  W02-PAGENO
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理（入力位置より）
      *****************************************************************
       50011-CUR-SET-SEC             SECTION.
      *   基本情報
          EVALUATE    WRK-MCP-WIDGET
              WHEN    "PREFNUM"
                  MOVE    2                TO  SPA-GMN-CUR
              WHEN    "TENHYOKBN"
                  MOVE    3                TO  SPA-GMN-CUR
              WHEN    "HOSPCD"
                  MOVE    4                TO  SPA-GMN-CUR
              WHEN    "HOSPSBT"
                  MOVE    6                TO  SPA-GMN-CUR
              WHEN    "HOSPNAME"
                  MOVE    7                TO  SPA-GMN-CUR
              WHEN    "KAISETUNAME"
                  MOVE    8                TO  SPA-GMN-CUR
              WHEN    "KANRINAME"
                  MOVE    9                TO  SPA-GMN-CUR
              WHEN    "BEDSU"
      ************MOVE    10               TO  SPA-GMN-CUR
                  MOVE    37               TO  SPA-GMN-CUR
              WHEN    "BEDSUIPPN"
                  MOVE    10               TO  SPA-GMN-CUR
              WHEN    "ROUPAYKBN"
                  MOVE    11               TO  SPA-GMN-CUR
              WHEN    "HOSPSBT1"
                  MOVE    32               TO  SPA-GMN-CUR
              WHEN    "SYOHOKBN"
                  MOVE    33               TO  SPA-GMN-CUR
              WHEN    "SKYPRTFLG"
                  MOVE    34               TO  SPA-GMN-CUR
              WHEN    "SYOHOPRTFLG"
                  MOVE    35               TO  SPA-GMN-CUR
              WHEN    "ZENSYOHOFLG"
                  MOVE    36               TO  SPA-GMN-CUR
              WHEN    "YAKUJYOFLG"
                  MOVE    12               TO  SPA-GMN-CUR
              WHEN    "HOSPCDN"
                  MOVE    38               TO  SPA-GMN-CUR
              WHEN    "DISCOUNTHASU"
                  MOVE    201              TO  SPA-GMN-CUR
              WHEN    "DISCOUNTHASU2"
                  MOVE    39               TO  SPA-GMN-CUR
              WHEN    "TAXHASU"
                  MOVE    202              TO  SPA-GMN-CUR
              WHEN    "HIWARIKBN"
                  MOVE    01               TO  SPA-GMN-CUR
      *       連絡先情報
              WHEN    "POST"
                  MOVE    21               TO  SPA-GMN-CUR
              WHEN    "ADRS"
                  MOVE    22               TO  SPA-GMN-CUR
              WHEN    "TEL"
                  MOVE    23               TO  SPA-GMN-CUR
              WHEN    "FAX"
                  MOVE    20               TO  SPA-GMN-CUR
      *       広告情報
              WHEN    "EMAIL"
                  MOVE    31               TO  SPA-GMN-CUR
              WHEN    "HPURL"
                  MOVE    30               TO  SPA-GMN-CUR
      *       割引率情報
              WHEN    "TEIRITU"
                  MOVE    41               TO  SPA-GMN-CUR
              WHEN    "DISCOUNTRATE"
                  MOVE    40               TO  SPA-GMN-CUR
      *       減免事由情報
              WHEN    "DISCOUNTBODY"
                  MOVE    51               TO  SPA-GMN-CUR
              WHEN    "DISCOUNTKBN"
                  MOVE    52               TO  SPA-GMN-CUR
              WHEN    "DISCOUNTOBJ"
                  MOVE    50               TO  SPA-GMN-CUR
      *       減免事由情報
              WHEN    "CONDITIONBODY"
                  MOVE    61               TO  SPA-GMN-CUR
              WHEN    "CONDITIONWORD"
                  MOVE    62               TO  SPA-GMN-CUR
              WHEN    "ICONFILENAME"
                  MOVE    60               TO  SPA-GMN-CUR
      *       請求書情報
              WHEN    "JIHINAME"
                  MOVE    70               TO  SPA-GMN-CUR
      *       診療内容情報
              WHEN    "SRYNAIYO"
                  MOVE    81               TO  SPA-GMN-CUR
              WHEN    "SRYNAIYODSP"
                  MOVE    80               TO  SPA-GMN-CUR
      *       入金方法情報
              WHEN    "NYKN_NAME"
                  MOVE    91               TO  SPA-GMN-CUR
              WHEN    "NYKN_TANNAME"
                  MOVE    92               TO  SPA-GMN-CUR
              WHEN    "NYKN_BUNRUI"
                  MOVE    90               TO  SPA-GMN-CUR
          END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
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
       900-SYSKANRI-READ-EXT.
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
