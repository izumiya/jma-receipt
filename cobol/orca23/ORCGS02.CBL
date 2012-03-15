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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGS02.
      **************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納登録
      *  コンポーネント名  : 請求一覧（S02）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      **************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−小豆沢  01/04/18  伝票発行日の修正
      *  01.00.02    MCC−小豆沢  01/04/18  再計算処理時の引き渡しパラメタ
      *                                     作成時に自動算定区分の判定を
      *                                     追加
      *  01.00.03    MCC−小豆沢  01/04/18  薬剤料逓減入力時の再計算修正
      *  01.00.04    MCC−小豆沢  01/04/18  診療行為テーブル読み込み条件
      *                                     を修正
      *  01.00.05    MCC−小豆沢  01/06/05  再計算処理時の引き渡しパラメタ
      *                                     作成時に診療種別区分の判定を
      *                                     追加
      *  01.00.06    MCC−小豆沢  01/06/05  再計算処理時の引き渡しパラメタ
      *                                     作成時に診療行為コードの判定を
      *                                     追加（薬剤であることの判定）
      *  01.00.07    MCC−小豆沢  01/07/12  保険適用外金額対応
      *                                     診療種別区分　.950　消費税なし
      *                                                   .960　消費税あり
      *  01.00.07    MCC−小豆沢  01/07/16  ＪＤ入金の削除 
      *                                     調整金のボタン中止      
      *  01.00.08    MCC−多々納  01/08/23  診療行為よりの遷移を追加
      *  01.00.09    MCC−小豆沢  01/09/19  コラムリストの選択追加
      *  01.01.01    MCC-藤原     01/11/07  患者番号、診療科、請求年月を
      *                                     入力項目に変更
      *                                     S01の削除
      *  01.01.01    MCC-多々納   01/12/12  画面遷移の修正
      *  01.01.03    MCC-小豆沢   01/12/13  再計算時の薬剤判定の修正
      *  01.01.04    MCC-多々納   02/01/31  選択番号の表示修正
      *  01.01.05    MCC-小豆沢   02/03/19  請求書再発行時に請求額と入金額
      *                                     のセット
      *  01.01.06    MCC-多々納   02/04/25  自費の判定に’０９６’を追加
      *  01.01.07    MCC-小豆沢   02/05/30  労災保険対応を追加
      *  01.01.08    MCC-多々納   02/06/05  患者氏名での検索を追加
      *  01.01.09    MCC-多々納   02/07/19  患者検索エラーメッセージ修正
      *  01.01.10    NACL-多々納  02/09/02  請求書をカスタマイズに修正
      *  01.01.11    NACL-小豆沢  02/09/12  自費の再計算時に薬剤、器材の
      *                                     集計を追加
      *  01.01.12    NACL-門脇    02/09/17  自賠責対応
      *  01.01.13    NACL-多々納  02/09/18  保険表示修正
      *  01.01.14    NACL-藤原    02/11/05  入外区分追加
      *  01.01.15    NACL-門脇    02.11.21  自賠責（ポリネック等の対応）
      *  01.01.16    NACL-太田    02.12.11  診療科・入外区分を判断して
      *                                     初期表示するよう修正
      *  01.01.17    NACL-太田    02.12.13  入院分の請求書再発行対応
      *  01.01.18    NACL-太田    02.12.16  帳票引数入外区分に選択された
      *                                     収納の入外区分を渡すよう修正
      *                                     コラムリストの金額欄を
      *                                     カンマ編集するよう修正
      *  01.01.19    NACL-森脇    02/12/19  支払証明書印刷追加
      *  01.01.20    NACL-太田    03/02/18  入院請求は取消可能とする
      *  01.01.21    NACL-太田    03/03/07  排他制御機能追加
      *  01.01.22    NACL-太田    03/03/13  入院請求書複数枚一括印刷対応
      *  01.01.23    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.01.24    NACL-門脇    03.04.15  自賠責文書料消費税対応
      *  01.01.25    NACL-門脇    03.04.16  船員保険（被保険者）＋通勤災害
      *                                     ＋初診時　対応
      *  01.01.26    NACL-太田    03.05.01  収納一覧リスト表示修正
      *  01.01.27    NACL-太田    03.05.13  患者が特定された時点で
      *                                     支払証明書が出力できるよう修正
      *  01.01.28    NACL-門脇    03.05.14  自賠責：腰部固定帯対応
      *  01.01.29    NACL-門脇    03.05.20  自賠責：療養担当手当て対応
      *  01.00.30    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.31    NACL-太田    03.08.26  請求書の引数設定修正
      *                                     （労災対応）
      *  01.00.32    NACL-小豆沢  03.09.17  福岡県の対応追加（再計算時に給付
      *                                     対象外点数を取り込み）
      *  01.00.33    NACL-太田    03.10.01  未収金管理機能強化対応
      *  01.00.34    NACL-太田    03.11.12  更新日付編集対応
      *  01.00.35    NACL-門脇    04.01.14  診療行為の包括分は除く
      *                                     SRYACCT2→32
      *  01.00.36    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.37    NACL-太田    04.02.09  コラムリスト編集修正
      *  01.00.38    NACL-門脇    04.07.16  再計算をサブへ修正
      *  01.00.39    NACL-門脇    04.12.01  収納外来一括再計算対応
      *  01.00.40    NACL-門脇    06.03.20  自立支援法対応
      *  01.00.41    NACL-太田    06.08.18  まとめ再発行対応
      *  03.04.01    NACL-森脇    06.12.19  支払証明書プレビュー表示
      *  03.05.00    NACL-太田    07/05/10  グループ診療対応
      *  04.04.00    NACL-太田    08/12/08  収納検索画面を吸収
      *
      *  04.05.01    NACL-門脇    09/06/16  一括再計算方法選択機能対応
      *  04.05.02    NACL-門脇    12/03/09  改正対応：外来現物給付対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    収納共通パラメタ
           COPY    "S01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
       01  SPA-S02.
      *
           03  SPA-S02-AREA.
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(02).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM              PIC X(10).
                   07  SPA-GMN-HEAD.
                       09  SPA-GMN-PTID            PIC 9(10).
                       09  SPA-GMN-PTNUM           PIC X(20).
                       09  SPA-GMN-KANANAME        PIC X(100).
                       09  SPA-GMN-SEX             PIC X(02).
                       09  SPA-GMN-NAME            PIC X(100).
                       09  SPA-GMN-BIRTHDAY        PIC X(09).
                       09  SPA-GMN-SRYKA-G.
                           11  SPA-GMN-SRYKA       PIC X(02).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SRYKAMEI    PIC X(16).
                       09  SPA-GMN-NYUGAIKBN-G.
                           11  SPA-GMN-NYUGAIKBN   PIC X(01).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEI
                                                   PIC X(06).
                       09  SPA-GMN-MODE-G.
                           11  SPA-GMN-MODE        PIC X(01).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-MODEMEI     PIC X(20).
                       09  SPA-GMN-SORTKBN-G.
                           11  SPA-GMN-SORTKBN     PIC X(01).
                           11  SPA-GMN-F1          PIC X(01).
                           11  SPA-GMN-SORTKBNMEI  PIC X(20).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-REL-MAX         PIC 9(05).
                       09  SPA-GMN-MAX             PIC 9(05).
                       09  SPA-GMN-NEXT            PIC 9(01).
                       09  SPA-GMN-TBLREC  OCCURS  500.
                           11  SPA-GMN-TBANGO      PIC 9(03).
                           11  SPA-GMN-TDENPNUM    PIC 9(07).
                           11  SPA-GMN-DENPGRP     PIC X(01).
                           11  SPA-GMN-TNYUGAIKBN  PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(06).
                           11  SPA-GMN-THKNCOMBI   PIC X(50).
                           11  SPA-GMN-TFTNRATE    PIC X(08).
                           11  SPA-GMN-TSRYYMD     PIC X(15).
                           11  SPA-GMN-TSKYEDYMD   PIC X(06).
                           11  SPA-GMN-THAKYMD     PIC X(09).
                           11  SPA-GMN-TSEIKYU     PIC 9(10).
                           11  SPA-GMN-TNYUKIN     PIC S9(10).
                           11  SPA-GMN-TMISYU      PIC S9(10).
                           11  SPA-GMN-TJYOTAI     PIC X(16).
                           11  SPA-GMN-SIKIBETUFLG PIC X(01).
                       09  SPA-GMN-TBL-SEL         PIC 9(05).
      *
                   07  SPA-GMN-INPUT.
                       09  SPA-GMN-SELBAN          PIC 9(04).
                   07  SPA-GMN-MISYU.
                       09  SPA-GMN-GAI-MISYU       PIC S9(08).
                       09  SPA-GMN-NYU-MISYU       PIC S9(08).
                   07  SPA-GMN-TUKISKY.
                       09  SPA-GMN-GAI-TUKISKY     PIC S9(10).
                       09  SPA-GMN-NYU-TUKISKY     PIC S9(10).
      *
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-SRYKALST-G.
                       09  SPA-GMN-SRYKA-MAX       PIC 9(03).
                       09  SPA-GMN-SRYKALST        OCCURS  99.
                          11  SPA-GMN-SRYKAL       PIC X(02).
                          11  SPA-GMN-SRYKAFL      PIC X(01).
                          11  SPA-GMN-SRYKAMEIL    PIC X(16).
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBN-MAX   PIC 9(02).
                       09  SPA-GMN-NYUGAIKBNLST    OCCURS  3.
                           11  SPA-GMN-NYUGAIKBNL  PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFL PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL
                                                   PIC X(06).
                   07  SPA-GMN-MODELST-G.
                       09  SPA-GMN-MODE-MAX        PIC 9(02).
                       09  SPA-GMN-MODELST         OCCURS  5.
                           11  SPA-GMN-MODEL       PIC X(01).
                           11  SPA-GMN-MODEFL      PIC X(01).
                           11  SPA-GMN-MODEMEIL    PIC X(20).
                   07  SPA-GMN-SORTKBNLST-G.
                       09  SPA-GMN-SORTKBN-MAX     PIC 9(02).
                       09  SPA-GMN-SORTKBNLST      OCCURS  5.
                           11  SPA-GMN-SORTKBNL    PIC X(01).
                           11  SPA-GMN-SORTKBNFL   PIC X(01).
                           11  SPA-GMN-SORTKBNMEIL PIC X(20).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM               PIC X(08).
                   07  SPA-NAI-SYS1039TBL          PIC X(500).
                   07  SPA-NAI-HEAD.
                       09  SPA-NAI-LOCK            PIC 9(01).
                       09  SPA-NAI-SLOCK-MSG       PIC X(32).
                       09  SPA-NAI-MODE-SRYYM      PIC X(06).
                       09  SPA-NAI-BIRTHDAY        PIC X(08).
                       09  SPA-NAI-NYUKIN-HOHO     PIC X(02).
                       09  SPA-NAI-SKYPRT          PIC X(01).
                       09  SPA-NAI-HKNCOMBI-G.
                           11  SPA-NAI-HKNCOMBI-MAX        PIC 9(05).
                           11  SPA-NAI-HKNCOMBILST         OCCURS  100.
                               13  SPA-NAI-HKNCOMBIL       PIC X(04).
                               13  SPA-NAI-HKNCOMBIMEIL    PIC X(50).
                               13  SPA-NAI-HKNNUML         PIC X(03).
                               13  SPA-NAI-RSI-HKNKBNL     PIC X(01).
                   07  SPA-TUKI-PARA-G.
                       09  SPA-TUKI-SRYYM          PIC  X(06).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-THSRYYM         PIC  X(06).
                       09  SPA-NAI-TBLREC  OCCURS  500.
                           11  SPA-NAI-TSRYKA      PIC  X(02).
                           11  SPA-NAI-TSRYKAMEI   PIC  X(16).
                           11  SPA-NAI-TSRYYMD     PIC  X(08).
                           11  SPA-NAI-THKNNUM     PIC  X(03).
                           11  SPA-NAI-TRSI-HKNKBN PIC  X(01).
                           11  SPA-NAI-THAKYMD     PIC  X(08).
                           11  SPA-NAI-TSKYEDYMD   PIC  X(08).
                           11  SPA-NAI-TNYUGAIKBN  PIC  X(01).
                           11  SPA-NAI-TJYOTAIKBN  PIC  X(01).
      *
               05  SPA-NAI-COMBO-AREA.
                   07  SPA-NAI-SRYKALST-G.
                       09  SPA-NAI-SRYKALST        OCCURS  99.
                          11  SPA-NAI-SRYKAMEIL    PIC X(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-TEIKIRRK        PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYULIST-ROW     PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-HIT             PIC 9(01).
           03  FLG-HIZUKE-RC       PIC 9(01).
           03  FLG-SEIKYU-PRT      PIC 9(01).
           03  FLG-MEISAI-PRT      PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-MISYORI2        PIC 9(01).
           03  FLG-GENBUTU         PIC 9(01).
           03  FLG-GENBUTU2        PIC 9(01).
           03  FLG-GENBUTU3        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX7                PIC 9(05).
           03  IDX9                PIC 9(05).
           03  IDX10               PIC 9(05).
           03  IDX-COMB            PIC 9(05).
           03  IDX12               PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-CMB             PIC 9(05).
           03  IDXD                PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-PRTKANRI-RENNUM PIC 9(05).
           03  CNT-JOBID           PIC 9(07).
           03  CNT-PAGE            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SIDCD           PIC X(04).
           03  WRK-SIDMSG          PIC X(70).
           03  WRK-CMB-CD          PIC X(50).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM     PIC X(200).
               05  WRK-CMB-ITM2    PIC X(200).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-SYMD            PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-NAI-SRYYM       PIC X(06).
      *----(03.04.01)--UPD-START---
           03  WRK-PGNAME-G.
               05  WRK-PGNAME-OCC  OCCURS  3.
                   07  WRK-PGNAME  PIC X(20).
                   07  WRK-PGTYPE  PIC X(20).
      *----(03.04.01)--UPD-END-----
      *
      *    保険組合せ名称
           03  WRK-KUMINO          PIC X(04).
           03  WRK-HKNKUMI         PIC X(40).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-RSI-HKNKBN      PIC X(01).
           03  WRK-NYUKIN          PIC S9(10).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-SAGAKU          PIC S9(07).
      *
           03  WRK-TBANGO-W        PIC ZZZ.
           03  WRK-TSEIKYU-W       PIC ZZ,ZZZ,ZZZ.
           03  WRK-TNYUKIN-W       PIC --,---,---.
           03  WRK-TMISYU-W        PIC --,---,---.
           03  WRK-SELBAN-W        PIC ZZZZ.  
      *    請求書ＰＧ名
           03  WRK-SEIKYU-PG       PIC X(20).
           03  WRK-MEISAI-PG       PIC X(20).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-MISYU-G.
               05  WRK-MISYU       PIC S9(08).
               05  WRK-MISYU-P     PIC S9(08).
               05  WRK-MISYU-M     PIC S9(08).
           03  WRK-TBL-SEL-MOT     PIC 9(05).
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
      *----(03.04.01)--ADD-START---
           03  WRK-PRT-PAGE        PIC 9(10).
      *----(03.04.01)--ADD-END-----
      *
           03  WRK-DENPNUM-END-TABLE.
               05  WRK-DENPNUM-END PIC 9(07)   OCCURS  100.
      *
           03  WRK-FILEMEI  .
               05  WRK-FILEMEI-YMD PIC X(08).
               05  WRK-FILEMEI-HMS PIC X(06).
               05  WRK-FILEMEI-CD  PIC X(06).
      *
           03  WRK-GAI-SRYYMD      PIC X(08).
           03  WRK-GAI-DENPNUM     PIC 9(07).
           03  WRK-GAI-SEIKYU      PIC S9(10).
           03  WRK-GAI-NYUKIN      PIC S9(10).
           03  WRK-GAI-MISYU-G.
               05  WRK-GAI-MISYU   PIC S9(10).
               05  WRK-GAI-MISYU-P PIC S9(10).
               05  WRK-GAI-MISYU-M PIC S9(10).
           03  WRK-NYU-MISYU       PIC S9(10).
           03  WRK-GAI-ZENMISYU    PIC S9(10).
           03  WRK-JYOTAI          PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SRYYM           PIC X(06).
           03  WRK-SYULIST-PAGE-G.
               05  WRK-SYULIST-PAGE    PIC ZZZZZ.
               05  WRK-SYULIST-LBL     PIC X(02).
           03  WRK-SKYYM           PIC X(06).
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-ORG-NYUGAIKBN   PIC X(01).
           03  WRK-ORG-DENPNUM     PIC 9(07).
           03  WRK-PAGE-CNT        PIC 9(06).
      *
       01  WRK-ROW.
           05  WRK-SYULIST-ROW     PIC S9(9)   BINARY.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG2        PIC X(22).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WK-GMN-RITU.
           03  WK-GMN-FTNRITU      PIC ZZZ.
           03  WK-GMN-PA           PIC X(01).
           03  FILLER              PIC X(01).
      *
      *    スパ領域 ワーク
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WRK-SPA-//.
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *----(03.04.01)--ADD-START---
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *----(03.04.01)--ADD-END-----
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  SHELL-AREA.
           03  SHELL-NAME                      PIC X(26)   VALUE
               "scripts/allways/orcbs01.sh".
      *
       01  CONST-AREA.
           03  CONST-MODE-NORMON               PIC 9(01) VALUE 0.
           03  CONST-MODE-NORYEAR              PIC 9(01) VALUE 1.
           03  CONST-MODE-NORALL               PIC 9(01) VALUE 2.
           03  CONST-MODE-MISYU                PIC 9(01) VALUE 3.
           03  CONST-MODE-NYUHEN               PIC 9(01) VALUE 4.
           03  CONST-SYULST-MAX                PIC 9(03) VALUE 500.
           03  CONST-SRYKA-MAX                 PIC 9(03) VALUE 99.
           03  CONST-ZENSRYKA-G.
               05  CONST-ZENSRYKA      PIC X(02)   VALUE "00".
               05                      PIC X(01)   VALUE SPACE.
               05  CONST-ZENSRYKAMEI   PIC X(16)   VALUE "全科".
      *
      *    システム管理ＤＢ（３００３）のキー値
           03  CONST-3003-KBNCD            PIC X(08)   VALUE "023".
      *
      *    印刷管理ＤＢのキー値
           03  CONST-PRTKANRI-TBL-KEY      PIC X(08)   VALUE "ORCBS01".
           03  CONST-PRTKANRI-TBL-GROUP    PIC X(14)   VALUE SPACE.
      *
      *    ジョブ管理ＤＢのキー値
           03  CONST-JOB-SHELLID           PIC X(08)   VALUE "ORCBS01".
      *
      *    収納明細マスターＷＫ
       01  WKSYUMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //WKSMEI-//.
      *
      **************************************************************
      *    ファイルレイアウト
      **************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *    帳票ＰＧ
           COPY    "CPSK1031.INC".
           COPY    "CPSK1032.INC".
           COPY    "CPSK1035.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK3003.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA02.INC".
           COPY    "CPSYUPARA03.INC".
      *
      **************************************************************
      *    サブプロ用　領域
      **************************************************************
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
           COPY    "CPORCHCN03.INC".
      *    明細書印刷パラメタ
           COPY    "CPORCHC04.INC".
           COPY    "CPORCHCN04.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    負担金再計算用パラメタ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *    主科情報アクセス用パラメタ
       01  ORCSSYUACCAREA.
           COPY    "CPORCSSYUACC.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    患者番号取得サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCHC03S02.INC".
      *
      *    入院請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
       01  UIDIO-AREA.
           03  C-RET                   PIC X(2).
           03  C-UID                   PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      **************************************************************
      *    連絡　領域
      **************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "S02.INC".
           COPY    "S03.INC".
           COPY    "S03N.INC".
           COPY    "S04.INC".
           COPY    "S06.INC".
           COPY    "S07.INC".
           COPY    "S10.INC".
           COPY    "S99.INC".
           COPY    "SERR.INC".
           COPY    "SID1.INC".
           COPY    "SID2.INC".
           COPY    "SID3.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      **************************************************************
      *    主　　処理
      **************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE        TO  SPA-S02
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
           IF    ( FLG-END             =   ZERO )
              PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      **************************************************************
      *    初期　処理
      **************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "SERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
      *
               EVALUATE    SPA-SIDCD
               WHEN     "2000"     THRU "2099"
                   PERFORM 520-SIDSET-SEC
                   GO  TO  100-INIT-EXT
               WHEN     "2101"
                   PERFORM 520-SID2SET-SEC
                   GO  TO  100-INIT-EXT
               WHEN     "2102"
                   PERFORM 520-SID3SET-SEC
                   GO  TO  100-INIT-EXT
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "S02"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *
      **************************************************************
      *    初期画面処理
      **************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他画面より
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
      *        元画面表示
      *
               IF    ( WRK-MOTOPG      =   "Y01"  OR "U01"  OR  "P97"
                                       OR  "XA01" OR "XB01" OR  "XC01"
                                       )
                   MOVE    LNK-COMMON  TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA  TO  SPA-S02
                   MOVE    SPA-WORK    TO  SPA-S01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF    ( WRK-MOTOPG      =   "Y01"
                                       OR  "XA01"
                                       OR  "XB01"
                                       OR  "XC01" )
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF    ( WRK-MOTOPG(1:3) =   "U01" 
                                       OR  "P97" )
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
           END-IF
      *
      *
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "K02"
           WHEN    "K02N"
           WHEN    "KT01"
           WHEN    "U02"
           WHEN    "I01"
               PERFORM 320-SENIJI-SYOGMN-EDIT-SEC
           WHEN    "S03"
           WHEN    "S03N"
           WHEN    "S06"
               PERFORM 320-S03SPASET-SEC
           WHEN    "S07"
               PERFORM 320-S03SPASET-SEC
               IF    ( SPA-S01-TUKI-SKYPRT =   "1" )
                OR   ( SPA-S01-TUKI-SRYPRT =   "1" )
                   PERFORM 4901-GAI-TUKISKYPRT-SEC
               END-IF
           WHEN    "S04"
               PERFORM 320-S03SPASET-SEC
               IF    ( SPA-S01-SHR-ORDER  NOT =   SPACE )
                   PERFORM 340-SHIHARAI-PRT-SEC
               ELSE
                   MOVE    SPACE           TO  SPA-S01-SHR-ORDER
               END-IF
               MOVE    SPACE           TO  SPA-S99-FLG
               MOVE    SPACE           TO  SPA-SIDCD
           WHEN    "S10"
               PERFORM 320-S03SPASET-SEC
               IF    ( SPA-S10-PTNUM   NOT =   SPACE )
                   PERFORM 3101-S10SPASET-SEC
               END-IF
           WHEN    "SID1"
               PERFORM 330-SID1-SET-SEC
           WHEN    "SID2"
               PERFORM 330-SID2-SET-SEC
           WHEN    "SID3"
               PERFORM 330-SID3-SET-SEC
           WHEN    "S99"
               PERFORM 340-S99-SET-SEC
           WHEN    OTHER
               PERFORM 310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ１）ＯＫ処理
      *****************************************************************
       330-SID1-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-SIDCD
      *
           IF    ( SPA-SID1-FLG    =   "OK" )
               EVALUATE    SPA-SIDCD
               WHEN    "2001"
                   PERFORM 430-TORIKESI-KOUSHIN-SEC
               WHEN    "2002"
                   PERFORM 440-IKATUSAIKEI-SEC
               END-EVALUATE
      *
           END-IF
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    WRK-SIDCD       TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＳＩＤ２）ＯＫ処理
      *****************************************************************
       330-SID2-SET-SEC            SECTION.
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2101"
               EVALUATE    SPA-SID1-FLG
               WHEN    "OK"
                   PERFORM 4701-NYUDEL-SEC
               WHEN    "NO"
                   INITIALIZE              SPA-S01KYOUTU
                   MOVE    "10"        TO  SPA-S01-SYORINO
                   PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
                   MOVE    "S02"       TO  SPA-MOTOPG
      *
                   IF  ( SPA-S01-NYUGAIKBN   =   "1" )
                       MOVE    "S03N"          TO  SPA-SAKIPG
                       MOVE   "JOIN"           TO  MCP-PUTTYPE
                       MOVE   "S03N"           TO  MCP-WINDOW
                   ELSE
                       MOVE    "S03"           TO  SPA-SAKIPG
                       MOVE   "JOIN"           TO  MCP-PUTTYPE
                       MOVE   "S03    "        TO  MCP-WINDOW
                   END-IF
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
      *
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＳＩＤ３）ＯＫ処理
      *****************************************************************
       330-SID3-SET-SEC            SECTION.
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2102"
               EVALUATE    SPA-SID1-FLG
               WHEN    "O1"
                   MOVE    1       TO  FLG-SEIKYU-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               WHEN    "O2"
                   MOVE    1       TO  FLG-MEISAI-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               WHEN    "OK"
                   MOVE    1       TO  FLG-SEIKYU-PRT
                   MOVE    1       TO  FLG-MEISAI-PRT
                   PERFORM 420-SAIHAKKOU-SEC
               END-EVALUATE
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID3-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-S99-SET-SEC             SECTION.
      *
           IF    ( SPA-S99-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-S99-FLG
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
      *
           .
       340-S99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"           TO  SPA-MOTOPG
           MOVE    "XC01"          TO  SPA-SAKIPG
      *
           MOVE    0               TO  WRK-STPAGE
           MOVE    0               TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO            TO  LNK-XC01-RENNUM
           MOVE    SPA-S02-TBL-GROUP
                                   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO            TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE      TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE      TO  LNK-XC01-ED-PAGE
           MOVE    "N23"           TO  LNK-XC01-MOTOPG
           MOVE    SPACE           TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA   TO  SPA-WORK
      *
           MOVE    SPACE           TO  LINKAREA
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU      TO  LNK-COMMON
           MOVE    SPA-S02         TO  LNK-SCRSPA
      *
           MOVE   "JOIN"           TO  MCP-PUTTYPE
           MOVE   "XC01   "        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    受付、患者検索より
           IF    ( WRK-SPA-PTNUM       =   SPA-GMN-PTNUM )
               CONTINUE
           ELSE
               IF    ( WRK-SPA-PTNUM   NOT =   SPACE )
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  S02-PTNUM
                   PERFORM 4102-PTNUM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-PTID    =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               ELSE
                   MOVE    3           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       3101-P97SPASET-EXT.
           EXIT.
      *****************************************************************
      *    患者一覧画面より戻り時の処理
      *****************************************************************
       3101-S10SPASET-SEC              SECTION.
      *
           MOVE    SPA-S10-PTNUM   TO  S02-PTNUM
           PERFORM 4102-PTNUM-CHK-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    3                   TO  SPA-GMN-CUR
      *
           .
       3101-S10SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボ初期値設定処理
      *****************************************************************
       3101-COMBO-SYOKI-SET-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    SPA-GMN-MODE        TO  WRK-CMB-CD
           PERFORM 4200-MODELST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-MODE-G
      *
           MOVE    SPA-GMN-SORTKBN     TO  WRK-CMB-CD
           PERFORM 4200-SORTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SORTKBN-G
      *
           .
       3101-COMBO-SYOKI-SET-EXT.
           EXIT.
      **************************************************************
      *    画面編集処理
      **************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           PERFORM 311-SPA-SET-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    画面編集処理
      **************************************************************
       311-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                   SPA-S02
                                        SPA-S01KYOUTU
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           MOVE    SYS-1039-TBL        TO  SPA-NAI-SYS1039TBL
      *
           MOVE    SPA-SYSYMDWH(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-SYSYMD  (1:6)    TO  SPA-NAI-SRYYM
      *
           PERFORM 3101-COMBO-EDIT-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       311-SPA-SET-EXT.
           EXIT.
      **************************************************************
      *    コンボボックス編集処理
      **************************************************************
       3101-COMBO-EDIT-SEC             SECTION.
      *
           PERFORM 3101-SRYKALST-SEC
      *
           PERFORM 3101-NYUGAIKBNLST-SEC
      *
           PERFORM 3101-MODELST-SEC
      *
           PERFORM 3101-SORTKBNLST-SEC
      *
           .
       3101-COMBO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科ＳＰＡ画面編集処理
      *****************************************************************
       3101-SRYKALST-SEC              SECTION.
      *
           INITIALIZE  SPA-GMN-SRYKALST-G
                       SPA-NAI-SRYKALST-G
      *
           MOVE    CONST-ZENSRYKA-G    TO  SPA-GMN-SRYKALST (1)
           MOVE    1                   TO  SPA-GMN-SRYKA-MAX
      *
           INITIALIZE  SYS-1005-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    "%"                 TO  SYS-KBNCD
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-SRYYM       =   SPACE )
               MOVE    "99999999"      TO  SYS-STYUKYMD
               MOVE    "00000000"      TO  SYS-EDYUKYMD
           WHEN  ( SPA-NAI-SRYYM(5:)   =   SPACE )
               MOVE    SPA-NAI-SRYYM   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "1231"          TO  SYS-STYUKYMD (5 : 4)
               MOVE    "0101"          TO  SYS-EDYUKYMD (5 : 4)
           WHEN  OTHER
               MOVE    SPA-NAI-SRYYM   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               MOVE    "31"            TO  SYS-STYUKYMD (7 : 2)
               MOVE    "01"            TO  SYS-EDYUKYMD (7 : 2)
           END-EVALUATE
      *
           PERFORM 900-SYSKANRI-KEY21-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO    )
                    OR   ( SPA-GMN-SRYKA-MAX  >=   CONST-SRYKA-MAX )
      *
               COMPUTE SPA-GMN-SRYKA-MAX   =   SPA-GMN-SRYKA-MAX   +   1
               MOVE    SPA-GMN-SRYKA-MAX   TO  IDX-CMB
      *
               MOVE    SYS-1005-KBNCD (1:2)
                                       TO  SPA-GMN-SRYKAL    (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SPA-GMN-SRYKAMEIL (IDX-CMB)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SPA-NAI-SRYKAMEIL (IDX-CMB)
      *
               PERFORM 900-SYSKANRI-KEY21-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key21"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SRYKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分ＳＰＡ画面編集処理
      *****************************************************************
       3101-NYUGAIKBNLST-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-NYUGAIKBNLST-G
      *
           IF    ( SPA-BEDSU           >   ZERO )
               MOVE    "0"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "全部"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL       (2)
               MOVE    "入院"          TO  SPA-GMN-NYUGAIKBNMEIL    (2)
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (3)
               MOVE    "外来"          TO  SPA-GMN-NYUGAIKBNMEIL    (3)
               MOVE    3               TO  SPA-GMN-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL       (1)
               MOVE    "外来"          TO  SPA-GMN-NYUGAIKBNMEIL    (1)
               MOVE    1               TO  SPA-GMN-NYUGAIKBN-MAX
           END-IF
      *
           .
       3101-NYUGAIKBNLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面モードＳＰＡ画面編集処理
      *****************************************************************
       3101-MODELST-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-MODELST-G
      *
           MOVE    "0"                 TO  SPA-GMN-MODEL       (1)
           MOVE    "収納一覧（月）"    TO  SPA-GMN-MODEMEIL    (1)
           MOVE    "1"                 TO  SPA-GMN-MODEL       (2)
           MOVE    "収納一覧（年）"    TO  SPA-GMN-MODEMEIL    (2)
           MOVE    "2"                 TO  SPA-GMN-MODEL       (3)
           MOVE    "収納一覧（全部）"  TO  SPA-GMN-MODEMEIL    (3)
           MOVE    "3"                 TO  SPA-GMN-MODEL       (4)
           MOVE    "未収一覧"          TO  SPA-GMN-MODEMEIL    (4)
           MOVE    "4"                 TO  SPA-GMN-MODEL       (5)
           MOVE    "入返金日"          TO  SPA-GMN-MODEMEIL    (5)
           MOVE    5                   TO  SPA-GMN-MODE-MAX
      *
           .
       3101-MODELST-EXT.
           EXIT.
      *****************************************************************
      *    並び順ＳＰＡ画面編集処理
      *****************************************************************
       3101-SORTKBNLST-SEC                SECTION.
      *
           INITIALIZE                  SPA-GMN-SORTKBNLST-G
      *
           IF    ( SPA-GMN-MODE        =   CONST-MODE-MISYU
                                       OR  CONST-MODE-NYUHEN )

               MOVE    "0"             TO  SPA-GMN-SORTKBNL       (1)
               MOVE    "発行日順"      TO  SPA-GMN-SORTKBNMEIL    (1)
           ELSE
               MOVE    "0"             TO  SPA-GMN-SORTKBNL       (1)
               MOVE    "伝票番号順"    TO  SPA-GMN-SORTKBNMEIL    (1)
           END-IF
           MOVE    "1"                 TO  SPA-GMN-SORTKBNL       (2)
           MOVE    "診療日順"          TO  SPA-GMN-SORTKBNMEIL    (2)
           MOVE    2                   TO  SPA-GMN-SORTKBN-MAX
      *
           .
       3101-SORTKBNLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    他の業務より遷移時初期処理
      *****************************************************************
       320-SENIJI-SYOGMN-EDIT-SEC      SECTION.
      *
           PERFORM 311-SPA-SET-SEC
      *
           IF  (   SPA-PTID        =  ZERO )
      *
               MOVE    2           TO  SPA-GMN-CUR
      *
           ELSE
      *
               PERFORM 320-PTINF-EDIT-SEC
               MOVE    1           TO  SPA-GMN-CUR
      *
           END-IF
      *
           .
       320-SENIJI-SYOGMN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       320-PTINF-EDIT-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                  SPA-GMN-HEAD
                                       SPA-NAI-HEAD
                                       SPA-GMN-PAGE
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-TUKISKY
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           MOVE    SPA-PTID             TO  SPA-GMN-PTID
           MOVE    PTNUM-PTNUM          TO  SPA-GMN-PTNUM
      *
           PERFORM 320-PTINF-SET-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           MOVE    SPA-NAI-SRYYM   TO  S02-SRYYM
           PERFORM 4104-SRYYM-CHK-SEC
           IF    ( SPA-GMN-MAX     = ZERO )
               MOVE    3           TO  SPA-GMN-CUR
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           .
       320-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       320-PTINF-SET-SEC               SECTION.
      *
      *    患者マスター存在チェック
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF           =   ZERO )
      *
               MOVE    PTINF-NAME      TO  SPA-GMN-NAME
               MOVE    PTINF-KANANAME  TO  SPA-GMN-KANANAME
               EVALUATE    PTINF-SEX
               WHEN    "1" 
                   MOVE    "男"        TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"        TO  SPA-GMN-SEX
               END-EVALUATE
               MOVE    PTINF-BIRTHDAY  TO  SPA-NAI-BIRTHDAY
               MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-BIRTHDAY
      *
              IF    ( PTINF-NYUKIN-HOHO    =   SPACE )
                   MOVE    "01"                TO  SPA-NAI-NYUKIN-HOHO
               ELSE
                   MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NAI-NYUKIN-HOHO
               END-IF
      *
               MOVE    PTINF-SKYPRTFLG TO  SPA-NAI-SKYPRT
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   MOVE    1               TO  SPA-NAI-LOCK
                   MOVE    SLOCK-MSG       TO  SPA-NAI-SLOCK-MSG
                   MOVE    SPA-GMN-PTID    TO  SPA-LAST-PTID
                   MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
               END-IF
      *
               PERFORM 320-HKNCOMBI-EDIT-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               PERFORM 3101-TUKISKY-EDIT-SEC
      *
           ELSE
               MOVE    1               TO  FLG-PTINF
               MOVE    "0031"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-PTID    TO  SPA-LAST-PTID
               MOVE    SPA-GMN-PTNUM   TO  SPA-LAST-PTNUM
           END-IF
      *
           .
       320-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       320-HKNCOMBI-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SPA-NAI-HKNCOMBI-G
      *
           INITIALIZE                  ORCSHKNSETAREA
                                       WRK-SPA-AREA
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-SPA-BIRTHDAY
           MOVE    "2"                 TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING
                                       WRK-SPA-AREA
                                       ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-NAI-HKNCOMBI-MAX
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   ORCSHKN-HKN-MAX )
                   OR    ( IDX-COMB    >   100 )
      *
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX-COMB)
                                   TO  SPA-NAI-HKNCOMBIL    (IDX-COMB)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX-COMB)
                                   TO  SPA-NAI-HKNCOMBIMEIL (IDX-COMB)
               MOVE    ORCSHKN-NAI-HKNNUM  (IDX-COMB)
                                   TO  SPA-NAI-HKNNUML      (IDX-COMB)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN  (IDX-COMB)
                                   TO  SPA-NAI-RSI-HKNKBNL  (IDX-COMB)
      *
           END-PERFORM
      *
           .
       320-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求確認戻り画面編集処理
      *****************************************************************
       320-S03SPASET-SEC          SECTION.
      *
           INITIALIZE                  SPA-S02
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
           MOVE    SYS-1039-TBL        TO  SPA-NAI-SYS1039TBL
      *
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-PTID        TO  SPA-GMN-PTID
           MOVE    SPA-S01-KANANAME    TO  SPA-GMN-KANANAME
           MOVE    SPA-S01-SEX         TO  SPA-GMN-SEX
           MOVE    SPA-S01-SRYYMW      TO  SPA-GMN-SRYYM
           MOVE    SPA-S01-SRYYM       TO  SPA-NAI-SRYYM
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
           MOVE    SPA-S01-LOCK        TO  SPA-NAI-LOCK
           MOVE    SPA-S01-SLOCK-MSG   TO  SPA-NAI-SLOCK-MSG
           MOVE    SPA-S01-BIRTHDAYW   TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-S01-BIRTHDAY    TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-S01-NYUKIN-HOHO TO  SPA-NAI-NYUKIN-HOHO
           MOVE    SPA-S01-SKYPRT      TO  SPA-NAI-SKYPRT
           MOVE    SPA-S01-HSRYKA      TO  SPA-GMN-SRYKA
           MOVE    SPA-S01-HNYUGAIKBN  TO  SPA-GMN-NYUGAIKBN
           MOVE    SPA-S01-PAGE        TO  SPA-GMN-PAGE
           MOVE    SPA-S01-MODE        TO  SPA-GMN-MODE
           MOVE    SPA-S01-SORTKBN     TO  SPA-GMN-SORTKBN
      *
           PERFORM 320-HKNCOMBI-EDIT-SEC
      *
           PERFORM 3101-COMBO-EDIT-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL >   ZERO )
      *
               IF    ( SPA-S01-NYUGAIKBN
                                   =   SPA-NAI-TNYUGAIKBN (IDX-LST))
                AND  ( SPA-S01-DENPNUM
                                   =   SPA-GMN-TDENPNUM   (IDX-LST))
      *
                   MOVE    IDX-LST     TO  SPA-GMN-TBL-SEL
                                           SPA-GMN-SELBAN
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N" OR "KT01" )
           AND   ( SPA-PTNUM       NOT =   SPACE )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-MOTOPG          =   "S04" )
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       320-S03SPASET-EXT.
           EXIT.
      *
      **************************************************************
      *    収納マスタ編集処理
      **************************************************************
       300-SYUNOU-HENSYU-SEC                  SECTION.
      *
           MOVE    SPA-GMN-TBL-SEL     TO  WRK-TBL-SEL-MOT
      *
           MOVE    SPA-NAI-THSRYYM     TO  WRK-NAI-SRYYM
      *
           INITIALIZE                      SPA-GMN-TBL
                                           SPA-NAI-TBL
                                           SPA-GMN-TUKISKY
      *
           IF    ( SPA-GMN-MODE         =  CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL
                                        OR CONST-MODE-NYUHEN )
               MOVE    SPA-NAI-SRYYM    TO SPA-NAI-THSRYYM
           ELSE
               MOVE    WRK-NAI-SRYYM    TO SPA-NAI-THSRYYM
           END-IF
      *
           PERFORM 3101-SYULIST-EDIT-SEC
      *
           PERFORM 3101-TUKISKY-EDIT-SEC
      *
           MOVE    WRK-TBL-SEL-MOT      TO  SPA-GMN-TBL-SEL
      *
           .
       300-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    支払証明書印刷処理
      *****************************************************************
       340-SHIHARAI-PRT-SEC            SECTION.
      *
      *----(03.04.01)--ADD-START---
           INITIALIZE                  SPA-S02-TBL-GROUP
                                       SPA-S02-JOB-JOBID
      *----(03.04.01)--ADD-START---
      *
      *----(03.04.01)--UPD-START---
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-3003-REC
               MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1006"          TO  SPA-ERRCD
                   GO  TO  340-SHIHARAI-PRT-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-JOB-SHELLID
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1007"          TO  SPA-ERRCD
               GO  TO  340-SHIHARAI-PRT-EXT
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    ZERO                TO  CNT-JOBID
                                           CNT-PRTKANRI-RENNUM
           MOVE    SPACE               TO  CONST-PRTKANRI-TBL-GROUP
      *
           MOVE    SMCNDATE-YMD    TO  CONST-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS    TO  CONST-PRTKANRI-TBL-GROUP (9:)
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR023ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    1                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
      *    帳票発行プログラム名取得
           PERFORM 3401-PGNAME-SHIHARAI-SEC
      *
           PERFORM VARYING IDX2 FROM   1   BY  1
                   UNTIL ( IDX2 >      3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               IF      SPA-S01-SHR-PRTFLG(IDX2)  =   "1"
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *    印刷処理
      *
           IF     ( SPA-S01-SHR-ORDER  =  "OK"  OR "PV" )
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    CONST-PRTKANRI-TBL-GROUP
                                           TO  SPA-S02-TBL-GROUP
               MOVE    CNT-JOBID           TO  SPA-S02-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *
               MOVE    SPACE               TO  SHELLTBL
      *        ファイルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    "/scripts/allways/print_prv.sh"
                                           TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    CONST-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY
               MOVE    CONST-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *
               MOVE    ERRFLPARA           TO  SHELLTBL-ARG12
               MOVE    CNT-JOBID           TO  SHELLTBL-ARG13
               MOVE    CONST-JOB-SHELLID   TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-S01-SHR-ORDER
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "OK"
                   MOVE    "2"             TO  SHELLTBL-ARG17
               END-EVALUATE
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "shell"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *----(03.04.01)--UPD-END-----
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
      *
           .
       340-SHIHARAI-PRT-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書プログラム名取得処理
      *****************************************************************
       3401-PGNAME-SHIHARAI-SEC        SECTION.
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                      WRK-PGNAME-G
      *
      *    支払証明書（日別）
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1032-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PGNAME (1)
           ELSE
              MOVE    "ORCHCM31"       TO  WRK-PGNAME (1)
           END-IF
      *
           MOVE       "HC31"           TO  WRK-PGTYPE (1)
      *
      *    支払証明書（月別）
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1032-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-PGNAME (2)
           ELSE
              MOVE    "ORCHCM32"       TO  WRK-PGNAME (2)
           END-IF
      *
           MOVE       "HC32"           TO  WRK-PGTYPE (2)
      *
      *----(03.04.01)--ADD-START---
           MOVE    "ORCHCM50"          TO  WRK-PGNAME (3)
      *
           MOVE       "HC50"           TO  WRK-PGTYPE (3)
      *----(03.04.01)--ADD-START---
           .
      *
       3401-PGNAME-SHIHARAI-EXT.
           EXIT.
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    印刷用スクリプト処理
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  CNT-PRTKANRI-RENNUM
           MOVE    CNT-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD(1:6) TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "orcbs01.sh"    TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           EVALUATE    IDX2
               WHEN    1
                   MOVE    SYS-1031-PRTNM (9)
                                       TO  LNK-PRTKANRI-PRTNM
               WHEN    2
                   MOVE    SYS-1031-PRTNM (9)
                                       TO  LNK-PRTKANRI-PRTNM
                   IF    ( SYS-1031-PRTNM (12) NOT =   SPACE )
                       MOVE    SYS-1031-PRTNM (12)
                                       TO  LNK-PRTKANRI-PRTNM
                   END-IF
               WHEN    3
                   MOVE    SYS-1031-PRTNM (15)
                                       TO  LNK-PRTKANRI-PRTNM
           END-EVALUATE
      *
           MOVE    SPACE           TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    SHELL-NAME      TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    CNT-JOBID       TO  SHELLTBL-ARG12
           MOVE    CONST-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    ERRFLPARA       TO  SHELLTBL-ARG14
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG15
           MOVE    WRK-PGTYPE (IDX2)
                                   TO  SHELLTBL-ARG16
           MOVE    WRK-PGNAME (IDX2)
                                   TO  SHELLTBL-ARG17
           MOVE    SPA-STAFFCD
                                   TO  SHELLTBL-ARG18
           MOVE    SPA-S01-SHR-STYMD
                                   TO  SHELLTBL-ARG19
           MOVE    SPA-S01-SHR-EDYMD
                                   TO  SHELLTBL-ARG20
           MOVE    SPA-S01-SHR-KIKANKBN
                                   TO  SHELLTBL-ARG21
           MOVE    SPA-S01-SHR-SYORIKBN
                                   TO  SHELLTBL-ARG22
           MOVE    SPA-S01-SHR-DSPKBN
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-GMN-PTID
                                   TO  SHELLTBL-ARG24
           MOVE    SPA-S01-SHR-NYUGAIKBN
                                   TO  SHELLTBL-ARG25
           MOVE    SPA-S01-SHR-JIHSUMKBN
                                   TO  SHELLTBL-ARG26
           MOVE    SPA-S01-SHR-SUMKBN
                                   TO  SHELLTBL-ARG27
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF
           .
      *
       4501-PRINT-EXT.
           EXIT.
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           COMPUTE CNT-JOBID   =   CNT-JOBID   +   1
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-JOBID       TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-JOBID       TO  JOB-JOBID
           MOVE    CONST-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    CONST-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    CNT-JOBID           TO  SYS-3003-JOBID
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF    ( FLG-SYSKANRI        =   ZERO )
               PERFORM 910-DBUPDATE-SEC
           ELSE
               PERFORM 910-DBINSERT-SEC
           END-IF
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      **************************************************************
      *   収納リスト編集処理
      **************************************************************
       3101-SYULIST-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           IF    ( SPA-GMN-SORTKBN     =   "1" )
               EVALUATE SPA-GMN-MODE
               WHEN    CONST-MODE-MISYU
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key66"         TO  WRK-PATHNAME
               WHEN    CONST-MODE-NYUHEN
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key67"         TO  WRK-PATHNAME
               WHEN    OTHER
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key68"         TO  WRK-PATHNAME
               END-EVALUATE
           ELSE
               EVALUATE SPA-GMN-MODE
               WHEN    CONST-MODE-MISYU
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key24"         TO  WRK-PATHNAME
               WHEN    CONST-MODE-NYUHEN
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key58"         TO  WRK-PATHNAME
               WHEN    OTHER
                   MOVE    "tbl_syunou"    TO  WRK-TABLE
                   MOVE    "key64"         TO  WRK-PATHNAME
               END-EVALUATE
           END-IF
      *
           PERFORM 900-SYUNOU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( SPA-GMN-MAX    >=   CONST-SYULST-MAX )
      *
               COMPUTE SPA-GMN-REL-MAX
                   =   SPA-GMN-REL-MAX
                   +   1
      *
               MOVE    1               TO  FLG-SYORI
      *
               IF    ( SPA-NAI-LOCK    =  ZERO )
                   CONTINUE
               ELSE
                   MOVE    ZERO        TO  FLG-SYORI
               END-IF
      *
               EVALUATE    SPA-GMN-NYUGAIKBN
               WHEN    "0"
                   CONTINUE
               WHEN    "1"
               WHEN    "2"
                   IF    ( SPA-GMN-NYUGAIKBN   =   SYU-NYUGAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-SYORI
                   END-IF
               WHEN    OTHER
                       MOVE    ZERO    TO  FLG-SYORI
               END-EVALUATE
      *
               EVALUATE    SPA-GMN-SRYKA
               WHEN    "00"
                   CONTINUE
               WHEN    SYU-SRYKA
                   CONTINUE
               WHEN    OTHER
                       MOVE    ZERO    TO  FLG-SYORI
               END-EVALUATE
      *
               IF    ( SPA-NAI-SRYYM   =   SPACE )
                   IF    ( SPA-GMN-REL-MAX
                               > ( SPA-GMN-PAGE * CONST-SYULST-MAX ))
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-SYORI
                   END-IF
               END-IF
      *
               IF    ( FLG-SYORI       =   1 )
                   PERFORM 3101-SYULST-SEC
               END-IF
      *
               PERFORM 900-SYUNOU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          IF    ( FLG-SYUNOU           =   ZERO )
              MOVE     1               TO  SPA-GMN-NEXT
          END-IF
      *
           .
       3101-SYULIST-EDIT-EXT.
           EXIT.
      **************************************************************
      *   未収金額編集処理
      **************************************************************
       3101-MISYU-EDIT-SEC             SECTION.
      *
           INITIALIZE  SPA-GMN-MISYU
      *
      *    入院分未収金額集計
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  SPA-GMN-NYU-MISYU
      *
      *    外来分未収金額集計
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  SPA-GMN-GAI-MISYU
      *
           .
       3101-MISYU-EDIT-EXT.
           EXIT.
      **************************************************************
      *   未収金集計処理
      **************************************************************
       3101-MISYU-SUM-SEC              SECTION.
      *
           INITIALIZE                      WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY53-SEL-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           PERFORM 900-SYUNOU-KEY54-SEL-SEC
      *
           COMPUTE WRK-MISYU-M
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           COMPUTE WRK-MISYU
               =   WRK-MISYU-P
               +   WRK-MISYU-M
      *
           .
       3101-MISYU-SUM-EXT.
           EXIT.
      **************************************************************
      *   未収金集計処理（月別）
      **************************************************************
       3101-TUKIMISYU-SUM-SEC          SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY55-SEL-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           PERFORM 900-SYUNOU-KEY56-SEL-SEC
      *
           COMPUTE WRK-MISYU-M
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           COMPUTE WRK-MISYU
               =   WRK-MISYU-P
               +   WRK-MISYU-M
      *
           .
       3101-TUKIMISYU-SUM-EXT.
           EXIT.
      **************************************************************
      *   診療年月請求額編集処理
      **************************************************************
       3101-TUKISKY-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-TUKISKY
      *
           PERFORM 900-SYUNOU-KEY61-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU   NOT =   ZERO )
      *
               IF    ( SYU-NYUGAIKBN       =   "1" )
                   COMPUTE SPA-GMN-NYU-TUKISKY
                       =   SPA-GMN-NYU-TUKISKY +   SYU-SKYMONEY
               ELSE
                   COMPUTE SPA-GMN-GAI-TUKISKY
                       =   SPA-GMN-GAI-TUKISKY +   SYU-SKYMONEY
               END-IF
      *
               PERFORM 900-SYUNOU-KEY61-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key61"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-TUKISKY-SUM-EXT.
           EXIT.
      **************************************************************
      *   収納マスターより画面内容編集処理
      **************************************************************
       3101-SYULST-SEC                SECTION.
      *
      *    収納マスタ内容編集
           COMPUTE SPA-GMN-MAX =   SPA-GMN-MAX +   1
      *
           MOVE    SPA-GMN-MAX         TO  IDX-LST
      *
           MOVE    IDX-LST             TO  SPA-GMN-TBANGO   (IDX-LST)
      *
           MOVE    SYU-DENPNUM         TO  SPA-GMN-TDENPNUM (IDX-LST)
      *
           IF    ( SYU-GRP-DENPNUM     NOT =  ZERO )
               MOVE   "M"              TO  SPA-GMN-DENPGRP  (IDX-LST)
           END-IF
      *
      *    入外区分
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN  (IDX-LST)
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"            TO  SPA-GMN-TNYUGAIKBN  (IDX-LST)
           WHEN    "2"
               MOVE    "外"            TO  SPA-GMN-TNYUGAIKBN  (IDX-LST)
           END-EVALUATE
      *
           IF      SYU-DENPJTIKBN  NOT =   "3"
                IF  (SPA-NAI-SRYYM(1:6)  IS  NUMERIC     )
                OR  (SYU-SRYYMD(1:6)  =   SPA-SYSYMD(1:6))
                  EVALUATE  SYU-SECMONEY
                    WHEN      1
                       MOVE      "1"   TO  SPA-GMN-SIKIBETUFLG(IDX-LST)
                    WHEN      2
                       MOVE      "2"   TO  SPA-GMN-SIKIBETUFLG(IDX-LST)
                  END-EVALUATE
                END-IF
           END-IF
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"               TO SYS-KANRICD
           MOVE    SYU-SRYKA            TO SYS-KBNCD
           MOVE    SYU-SRYYMD           TO SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           MOVE    SYSKANRI-REC         TO SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME2  TO SPA-GMN-TSRYKA (IDX-LST)
           MOVE    SYU-SRYKA            TO SPA-NAI-TSRYKA (IDX-LST)
           MOVE    SYS-1005-SRYKANAME1  TO SPA-NAI-TSRYKAMEI (IDX-LST)
      *
      *    診療日
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    SYU-SKYSTYMD    TO  SPA-NAI-TSRYYMD   (IDX-LST)
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD   (IDX-LST)
               MOVE    SYU-SKYEDYMD    TO  SPA-NAI-TSKYEDYMD (IDX-LST)
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    "-"         TO  SPA-GMN-TSRYYMD (IDX-LST)(10:1)
               MOVE    WRK-HENYMDG (5:5)
                                   TO  SPA-GMN-TSRYYMD (IDX-LST) (11:5)
           WHEN    "2" 
               MOVE    SYU-SRYYMD      TO  SPA-NAI-TSRYYMD (IDX-LST)
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-HIZUKE-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD (IDX-LST)
           END-EVALUATE
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-THAKYMD (IDX-LST)
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-THAKYMD (IDX-LST)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SPA-GMN-TSEIKYU (IDX-LST)
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-TNYUKIN (IDX-LST)
      *    未収金額
           COMPUTE SPA-GMN-TMISYU(IDX-LST)
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
      *    保険組合せより（保険と負担）
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KUMINO
           PERFORM 510-HKNKUMI-MEI-SEC
           MOVE    WRK-HKNKUMI         TO  SPA-GMN-THKNCOMBI (IDX-LST)
           MOVE    WRK-HKNNUM          TO  SPA-NAI-THKNNUM   (IDX-LST)
           MOVE    WRK-RSI-HKNKBN      TO  SPA-NAI-TRSI-HKNKBN(IDX-LST)
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-TFTNRATE (IDX-LST)
           ELSE
               MOVE    SPACE           TO  WK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WK-GMN-FTNRITU
               MOVE    "%"             TO  WK-GMN-PA
               MOVE    WK-GMN-RITU     TO  SPA-GMN-TFTNRATE (IDX-LST)
           END-IF
      *
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-TJYOTAIKBN (IDX-LST)
      *
      *    伝票状態区分
           MOVE    SPACE               TO  WRK-JYOTAI
           EVALUATE   SYU-DENPJTIKBN   ALSO   SYU-CREATEKBN
           WHEN   SPACE                ALSO   SPACE
               MOVE  "請求額なし"      TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   SPACE
               MOVE  "入金済"          TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   SPACE
               MOVE  "未入金"          TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   SPACE
               MOVE  "請求取消し"      TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   SPACE
               MOVE  "継続"            TO  WRK-JYOTAI
           WHEN   SPACE                ALSO   "1"
               MOVE  "請求額なし（退）"
                                       TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   "1"
               MOVE  "入金済（退）"
                                       TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   "1"
               MOVE  "未入金（退）"
                                       TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   "1"
               MOVE  "請求取消し（退）"
                                       TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   "1"
               MOVE  "継続（退）"      TO  WRK-JYOTAI
           WHEN   SPACE                ALSO   "2"
               MOVE  "請求額なし（定）"
                                       TO  WRK-JYOTAI
           WHEN   "1"                  ALSO   "2"
               MOVE  "入金済（定）"    TO  WRK-JYOTAI
           WHEN   "2"                  ALSO   "2"
               MOVE  "未入金（定）"    TO  WRK-JYOTAI
           WHEN   "3"                  ALSO   "2"
               MOVE  "請求取消し（定）"
                                       TO  WRK-JYOTAI
           WHEN   "7"                  ALSO   "2"
               MOVE  "継続（定）"      TO  WRK-JYOTAI
           END-EVALUATE
      *
           MOVE    WRK-JYOTAI      TO  SPA-GMN-TJYOTAI (IDX-LST)
      *
           .
       3101-SYULST-EXT.
           EXIT.
      *
      *----(03.04.01)--UPD-START---
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      S99
           MOVE    ZERO                TO  S99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  S99-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC 
               PERFORM VARYING IDX2 FROM   1   BY  1
                       UNTIL ( IDX2 >      SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX2            TO  JOB-JOBID
                   MOVE    CONST-JOB-SHELLID
                                           TO  JOB-SHELLID
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       MOVE    JOB-JOBID       TO  S99-RENNUM   (IDX2)
                       MOVE    JOB-SHELLMSG    TO  S99-SHORINM  (IDX2)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  S99-STARTTIME(IDX2)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  S99-ENDTIME  (IDX2)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  S99-PAGE     (IDX2)
                       MOVE    JOB-YOBI        TO  S99-ERRMSG   (IDX2)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  S99-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                                   INTO  S99-MSG
                           END-IF
                       END-IF
                       MOVE    IDX2             TO  S99-COUNT
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( S99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( S99-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  S99-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  S99-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-S01-SHR-ORDER   =   "PV" )
               MOVE    "F12:プレビュー"    TO  S99-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  S99-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "S99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "S99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *----(03.04.01)--UPD-END-----
      *****************************************************************
      *    再印刷処理
      *****************************************************************
       450-REPRINT-SEC                 SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       450-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    情報削除
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-JOB-SHELLID   TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       490-JYOUHOU-DEL-EXT.
           EXIT.
      **************************************************************
      *    自画面編集処理
      **************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           EVALUATE    SPA-SIDCD
           WHEN     "2000"     THRU "2099"
               PERFORM 520-SIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           WHEN     "2101"
               PERFORM 520-SID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           WHEN     "2102"
               PERFORM 520-SID3SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "S02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面編集処理
      **************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    S02-SYULIST-ROW     TO  WRK-SYULIST-ROW
      *
           MOVE    SPACE               TO  S02
           INITIALIZE                      S02
      *
           EVALUATE    FLG-SYULIST-ROW
           WHEN    1
               MOVE    ZERO            TO  S02-SYULIST-ROW
               MOVE    ZERO            TO  S02-SYULIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SYULIST-ROW TO  S02-SYULIST-ROW
               MOVE    ZERO            TO  S02-SYULIST-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-PTNUM       TO  S02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  S02-KANANAME
           MOVE    SPA-GMN-NAME        TO  S02-NAME
           MOVE    SPA-GMN-SEX         TO  S02-SEX
           MOVE    SPA-GMN-SRYYM      TO  S02-SRYYM
           MOVE    SPA-GMN-BIRTHDAY    TO  S02-BIRTHDAY
           MOVE    SPA-GMN-SRYKA-G     TO  S02-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN-G TO  S02-NYUGAIKBN
           MOVE    SPA-GMN-MODE-G      TO  S02-MODE
           MOVE    SPA-GMN-SORTKBN-G   TO  S02-SORTKBN
      *
           MOVE    SPA-GMN-SRYKA-MAX   TO  S02-SRYKA-COUNT
           PERFORM VARYING IDX-CMB    FROM    1   BY  1
                   UNTIL ( IDX-CMB    >   SPA-GMN-SRYKA-MAX )
               MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                       TO  S02-SRYKALST(IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUGAIKBN-MAX   TO  S02-NYUGAIKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                       TO  S02-NYUGAIKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-MODE-MAX   TO  S02-MODE-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-MODE-MAX )
               MOVE    SPA-GMN-MODELST (IDX-CMB)
                                       TO  S02-MODE-ITEM (IDX-CMB)
           END-PERFORM
      *
           MOVE    SPA-GMN-SORTKBN-MAX TO  S02-SORTKBN-COUNT
           PERFORM VARYING IDX-CMB     FROM   1   BY  1
                   UNTIL ( IDX-CMB     >      SPA-GMN-SORTKBN-MAX )
               MOVE    SPA-GMN-SORTKBNLST (IDX-CMB)
                                       TO  S02-SORTKBN-ITEM (IDX-CMB)
           END-PERFORM
      *
      *    暫定 ST
           MOVE    WIDGET-NORMAL       TO  S02-B04-STATE
           MOVE    WIDGET-INSENSITIVE  TO  S02-B03S-STATE
      *    暫定 ED
      *
           MOVE    ZERO                TO  FLG-MISYORI2
      *
           PERFORM VARYING     IDX-LST     FROM    1   BY  1
                   UNTIL     ( IDX-LST     >       SPA-GMN-MAX )
               MOVE    SPA-GMN-TBANGO (IDX-LST)
                                       TO  WRK-TBANGO-W
               MOVE    WRK-TBANGO-W    TO  S02-TNUM       (IDX-LST)
               MOVE    SPA-GMN-TDENPNUM(IDX-LST)
                                       TO  S02-TDENPNUM   (IDX-LST)
               MOVE    SPA-GMN-DENPGRP(IDX-LST)
                                       TO  S02-TDENPNUM   (IDX-LST)(9:1)
               MOVE    SPA-GMN-TNYUGAIKBN(IDX-LST)
                                       TO  S02-TNYUGAI    (IDX-LST)
               MOVE    SPA-GMN-TSRYKA(IDX-LST)
                                       TO  S02-TSRYKA    (IDX-LST)
               MOVE    SPA-GMN-THKNCOMBI(IDX-LST)
                                       TO  S02-THKN       (IDX-LST)
               MOVE    SPA-GMN-TFTNRATE(IDX-LST)
                                       TO  S02-TFTNRATE   (IDX-LST)
               MOVE    SPA-GMN-THAKYMD (IDX-LST)
                                       TO  S02-TSKYSTYMD  (IDX-LST)
               MOVE    SPA-GMN-TSRYYMD (IDX-LST)
                                       TO  S02-TSRYYMD    (IDX-LST)
               MOVE    SPA-GMN-TSEIKYU(IDX-LST)
                                       TO  WRK-TSEIKYU-W
               MOVE    WRK-TSEIKYU-W   TO  S02-TSKYMONEY  (IDX-LST)
               MOVE    SPA-GMN-TNYUKIN (IDX-LST)
                                       TO  WRK-TNYUKIN-W
               MOVE    WRK-TNYUKIN-W   TO  S02-TNYUKINMONEY(IDX-LST)
               MOVE    SPA-GMN-TMISYU (IDX-LST)
                                       TO  WRK-TMISYU-W
               MOVE    WRK-TMISYU-W    TO  S02-TMISYUMONEY(IDX-LST)
               MOVE    SPA-GMN-TJYOTAI (IDX-LST)
                                       TO  S02-TDENPJTI   (IDX-LST)
      *
               IF    ( IDX-LST         =   SPA-GMN-TBL-SEL )
                   MOVE    "T"         TO  S02-SYULIST-SELECT (IDX-LST)
      *        暫定 ST
                   IF    ( SPA-NAI-TNYUGAIKBN(IDX-LST) =   "1" )
                       MOVE    WIDGET-INSENSITIVE
                                        TO  S02-B04-STATE
                       MOVE    WIDGET-NORMAL
                                        TO  S02-B03S-STATE
                   END-IF
      *        暫定 ED
               ELSE
                   MOVE    "F"         TO  S02-SYULIST-SELECT (IDX-LST)
               END-IF
      *
               EVALUATE  SPA-GMN-SIKIBETUFLG(IDX-LST)
                 WHEN        "1"
                   MOVE     1          TO  FLG-MISYORI2
                 WHEN        "2"
                   MOVE     2          TO  FLG-MISYORI2
               END-EVALUATE
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX                 TO  S02-SYULIST-COUNT
      *
           MOVE    SPA-GMN-SELBAN              TO  WRK-SELBAN-W
           MOVE    WRK-SELBAN-W                TO  S02-SELNUM
      *
           IF      FLG-MISYORI2   NOT =  ZERO
             EVALUATE  FLG-MISYORI2
               WHEN       1
                 MOVE    "red"    TO    S02-LBLMSG-STYLE
       MOVE "★★　【７０歳未満現物給付】一括再計算を行ってください　"
                                  TO    S02-LBLMSG(1:56)
       MOVE "★★"                TO    S02-LBLMSG(57:)
               WHEN       2
                 MOVE    "blue"   TO    S02-LBLMSG-STYLE
       MOVE "★★　【高齢者現物給付】一括再計算を行ってください　★★"
                                  TO    S02-LBLMSG
             END-EVALUATE
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  S02-B06-STATE
                                           S02-B07-STATE
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-SRYYM       =   SPACE )
               MOVE    "前頁"          TO  S02-B06-LABEL
               MOVE    "次頁"          TO  S02-B07-LABEL
               STRING  "　　　　　　　　　　　"  DELIMITED   BY  SIZE
                       "請求額計：外来"          DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
               IF    ( SPA-GMN-PAGE    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  S02-B06-STATE
               END-IF
      *
               IF    ( SPA-GMN-NEXT    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  S02-B07-STATE
               END-IF
           WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
               MOVE    "前年"          TO  S02-B06-LABEL
               MOVE    "次年"          TO  S02-B07-LABEL
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "0101"          TO  WRK-SYMD(5:)
               PERFORM 800-HIZUKE-SEC
               STRING  "　　　　　　"      DELIMITED   BY  SIZE
                       WRK-HENYMDG2 (1:10) DELIMITED   BY  SIZE
                       "請求額計：外来"    DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
           WHEN  ( SPA-NAI-SRYYM (7:)  =   SPACE )
               MOVE    "前月"          TO  S02-B06-LABEL
               MOVE    "次月"          TO  S02-B07-LABEL
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:)
               PERFORM 800-HIZUKE-SEC
               STRING  "　　　"
                        WRK-HENYMDG2 (1:16) DELIMITED   BY  SIZE
                       "請求額計：外来"    DELIMITED   BY  SIZE
               INTO    S02-LBLSRYYMG
               END-STRING
           WHEN  OTHER
               MOVE    "前日"          TO  S02-B06-LABEL
               MOVE    "次日"          TO  S02-B07-LABEL
           END-EVALUATE
      *
           IF    ( SPA-GMN-MODE        =   CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL
                                                          )
               MOVE    "入院"          TO  S02-LBLSRYYMN
           ELSE
               MOVE    SPACE           TO  S02-LBLSRYYMG
           END-IF
      *
           MOVE    "black"             TO  S02-LBLMSG4-STYLE
           PERFORM 4901-SKYYM-CHK-SEC
           IF    ( WRK-SKYYM       NOT =   SPACE )
               MOVE    WRK-SKYYM       TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD(7:2)
               PERFORM 800-HIZUKE-SEC
               STRING  "一括再計算は"
                                               DELIMITED   BY  SIZE
                       WRK-HENYMDG2 (1:16)     DELIMITED   BY  SIZE
                       "が対象となります。"    DELIMITED   BY  SIZE
               INTO    S02-LBLMSG4
               END-STRING
           END-IF
      *
           MOVE    "black"                     TO  S02-LBLMSG2-STYLE
      *
           IF    ( SPA-GMN-PTID    NOT =   ZERO )
      *
               MOVE    "blue"                  TO  S02-LBLMSG2-STYLE
      *
               IF    ( SPA-GMN-REL-MAX     =   ZERO )
                   EVALUATE    SPA-GMN-MODE
                   WHEN    CONST-MODE-NORMON
                   WHEN    CONST-MODE-NORYEAR
                   WHEN    CONST-MODE-NORALL
                       EVALUATE    TRUE
                       WHEN  ( SPA-NAI-SRYYM       =   SPACE )
                           MOVE    "該当患者に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
                           MOVE    "診療年に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       WHEN  OTHER
                           MOVE    "診療年月に収納の登録はありません。"
                                                   TO  S02-LBLMSG2
                       END-EVALUATE
                   WHEN    CONST-MODE-MISYU
                           MOVE    "未収金（過入金）はありません"
                                                   TO  S02-LBLMSG2
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    "black"                     TO  S02-LBLMSG6-STYLE
                                                   S02-KANANAME-STYLE
                                                   S02-NAME-STYLE
      *
           IF    ( SPA-NAI-LOCK        =   ZERO )
               CONTINUE
           ELSE
               MOVE    "red"                   TO  S02-LBLMSG2-STYLE
                                                   S02-KANANAME-STYLE
                                                   S02-NAME-STYLE
      *
               MOVE    "他端末で使用中です。"  TO  S02-LBLMSG2
               MOVE    SPA-NAI-SLOCK-MSG       TO  S02-LBLMSG2(21:)
      *
           END-IF
      *
           IF    ( SPA-NAI-SRYYM   =   SPACE )
               IF    ( SPA-GMN-PAGE    >   ZERO )
                OR   ( SPA-GMN-NEXT    =   1    )
                   INITIALIZE  WRK-SYULIST-PAGE-G
                   COMPUTE CNT-PAGE    =   SPA-GMN-PAGE    +   1
                   MOVE    CNT-PAGE    TO  WRK-SYULIST-PAGE
                   MOVE    "頁"        TO  WRK-SYULIST-LBL
                   MOVE    WRK-SYULIST-PAGE-G  TO  S02-LBLMSG5
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-MODE
           WHEN    CONST-MODE-NORMON
           WHEN    CONST-MODE-NORYEAR
           WHEN    CONST-MODE-NORALL
               MOVE    "未収一覧"              TO  S02-B09S-LABEL
           WHEN    CONST-MODE-MISYU
               MOVE    "入返金日"              TO  S02-B09S-LABEL
           WHEN    CONST-MODE-NYUHEN
               MOVE    "収納一覧"              TO  S02-B09S-LABEL
           END-EVALUATE
      *
           MOVE    WIDGET-NORMAL               TO  S02-SRYYM-STATE
           IF    ( SPA-GMN-MODE                =   CONST-MODE-MISYU )
               MOVE    WIDGET-INSENSITIVE      TO  S02-SRYYM-STATE
           END-IF
      *
           MOVE    SPA-GMN-GAI-MISYU           TO  S02-GAI-MISYU
      *
           MOVE    SPA-GMN-NYU-MISYU           TO  S02-NYU-MISYU
      *
           IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON
                                    OR CONST-MODE-NORYEAR
                                    OR CONST-MODE-NORALL  )
               MOVE    SPA-GMN-GAI-TUKISKY     TO  S02-GAI-TUKISKY
      *
               MOVE    SPA-GMN-NYU-TUKISKY     TO  S02-NYU-TUKISKY
           END-IF
      *
           MOVE    WIDGET-NORMAL               TO  S02-PTNUM-STATE
      *
           MOVE    WIDGET-NORMAL               TO  S02-B04S-STATE
           IF    ( SPA-GMN-NYUGAIKBN           =   "1" )
               MOVE    WIDGET-INSENSITIVE      TO  S02-B04S-STATE
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE          TO  S02-B12C-STATE
           MOVE    "一括再計算該当"            TO  S02-B12C-LABEL
           IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON )
               MOVE    WIDGET-NORMAL           TO  S02-B12C-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面カーソルセット処理
      **************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( S02-SRYYM-STATE =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-MAX >   ZERO )
                       MOVE    1   TO  SPA-GMN-CUR
                   ELSE
                       MOVE    6   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE    "SELNUM"    TO  MCP-WIDGET
           WHEN    2
               MOVE    "PTNUM"     TO  MCP-WIDGET
           WHEN    3
               MOVE    "SRYYM"     TO  MCP-WIDGET
           WHEN    4
               MOVE    "SRYKA"     TO  MCP-WIDGET
           WHEN    5
               MOVE    "NYUGAIKBN" TO  MCP-WIDGET
           WHEN    6
               MOVE    "MODE"      TO  MCP-WIDGET
           WHEN    7
               MOVE    "SORTKBN"   TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      **************************************************************
      *    画面遷移処理
      **************************************************************
       200-GMNSENI                     SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
      *    外来月別
           WHEN    "CLICKED"       ALSO    "B01S"
               PERFORM 490-GAI-TUKISKYPRT-SEC
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    前回患者処理
           WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *
      *    再発行
           WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 419-SAIHAKKOU-SEC
      *    請求取消処理
           WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 430-TORIKESI-SEC
      *    再計算
           WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-SAIKEI-SEC
      *    返金
           WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-HENKIN-SEC
      *    入金
           WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 460-NYUKIN-SEC
      *    一括入金画面（Ｓ06）
           WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 460-IKKATU-NYUKIN-SEC
      *    入金取消
           WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-NYUDEL-SEC
      *    修正履歴
           WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 490-RIREKI-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
      *    一括再計算該当
           WHEN    "CLICKED"       ALSO    "B12C"
                   PERFORM 412-S10-SENI-SEC
      *    ＪＤ入金
      ***  WHEN    "CLICKED"       ALSO    "B13"
      ***          PERFORM 490-JD-NYUKIN-SEC
      *    前月表示
      *****WHEN    "CLICKED"       ALSO    "B14"
           WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 490-ZENGETU-SEC
      *    次月表示
      *****WHEN    "CLICKED"       ALSO    "B15"
           WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 490-JIGETU-SEC
      *    画面モード変更
           WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 470-MODE-CHENGE-SEC
      *    氏名検索（Ｐ９７）
           WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 470-SIMEIKEN-SEC
      *    予約登録へ（Ｙ０２）
           WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ（Ｕ０１）
           WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *----(01.01.19)--ADD-START---
      *    支払証明書処理
           WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 500-SHIHARAI-SEC
      *----(01.01.19)--ADD-END-----
      *    処理結果画面
           WHEN    "CLICKED"       ALSO    "B12S"
               PERFORM 490-KAKUNIN-SEC
      *    情報削除画面
           WHEN    "CLICKED"       ALSO    "B10S"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B08S"
               PERFORM 450-REPRINT-SEC
      *    外来分一括再計算
           WHEN    "CLICKED"       ALSO    "B04S"
               PERFORM 440-IKATUSAIKEI-CHK-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      **************************************************************
      *    会話　処理
      **************************************************************
       200-ENTRY                       SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    選択番号
           WHEN    ANY                 ALSO    "SYULIST"
               PERFORM 4105-LSTSEL-SEC
           WHEN    "ACTIVATE"          ALSO    "SELNUM"
               PERFORM 4101-SELNUM-CHK-SEC
      *    患者番号
           WHEN    "ACTIVATE"          ALSO    "PTNUM"
               PERFORM 4102-PTNUM-CHK-SEC
      *    診療科
           WHEN    "ACTIVATE"          ALSO    "SRYKA"
               PERFORM 4103-SRYKA-CHK-SEC
      *    請求年月
           WHEN    "ACTIVATE"          ALSO    "SRYYM"
               PERFORM 4104-SRYYM-CHK-SEC
      *    入外区分
           WHEN    "ACTIVATE"          ALSO    "NYUGAIKBN"
               PERFORM 4106-NYUGAIKBN-CHK-SEC
      *    モード変更
           WHEN    "ACTIVATE"          ALSO    "MODE"
               PERFORM 4106-MODE-CHK-SEC
      *    並び順変更
           WHEN    "ACTIVATE"          ALSO    "SORTKBN"
               PERFORM 4106-SORTKBN-CHK-SEC
      *
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      **************************************************************
      *    番号選択入力処理
      **************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-TBL-SEL
      *
           MOVE    S02-SELNUM          TO  SPA-GMN-SELBAN
      *
           IF    ( SPA-GMN-SELBAN      =  ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELBAN  >   SPA-GMN-MAX )
                   MOVE    "0001"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-SELBAN  TO  SPA-GMN-TBL-SEL
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    患者番号チェック処理
      **************************************************************
       4102-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           INITIALIZE                  SPA-GMN-HEAD
                                       SPA-NAI-HEAD
                                       SPA-GMN-PAGE
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-TUKISKY
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
      *
           MOVE    S02-PTNUM           TO  SPA-GMN-PTNUM
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *
           EVALUATE    SPTNUM-RC
           WHEN    00
               MOVE    SPTNUM-PTNUM    TO  SPA-GMN-PTNUM
               MOVE    SPTNUM-PTID     TO  SPA-GMN-PTID
      *
               PERFORM 320-PTINF-SET-SEC
      *
               PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
               MOVE    SPA-NAI-SRYYM   TO  S02-SRYYM
               PERFORM 4104-SRYYM-CHK-SEC
               IF    ( SPA-GMN-MAX     = ZERO )
                   MOVE    3           TO  SPA-GMN-CUR
               ELSE
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           WHEN   10
               MOVE    "0031"          TO  SPA-ERRCD
           WHEN    20
      *        氏名検索へ
               MOVE    SPACE           TO  LINKAREA
               MOVE    SPA-KYOUTU      TO  LNK-COMMON
               MOVE    SPA-S02         TO  LNK-SCRSPA
      *
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    "P97"           TO  SPA-SAKIPG
      *        氏名セット
               MOVE    SPA-GMN-PTNUM   TO  SPA-NAME
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
               MOVE    "P97"           TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           WHEN    OTHER
               MOVE    "0031"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
       4102-PTNUM-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療科入力処理
      **************************************************************
       4103-SRYKA-CHK-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-SRYKA (1:2)     TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療年月入力処理
      **************************************************************
       4104-SRYYM-CHK-SEC              SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYMD
      *
           MOVE    S02-SRYYM           TO  WRK-SYMD
           PERFORM 801-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKE-RC       =   ZERO )
      *
               INITIALIZE                  SPA-GMN-PAGE
                                           SPA-GMN-SRYYM
                                           SPA-NAI-SRYYM
                                           SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
      *
               MOVE    WRK-SYMD    TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG TO  SPA-GMN-SRYYM
               IF    ( SPA-GMN-MODE    =   CONST-MODE-NORMON
                                        OR CONST-MODE-NORYEAR
                                        OR CONST-MODE-NORALL  )
                   EVALUATE    TRUE
                   WHEN  ( WRK-SYMD        =   SPACE )
                       MOVE    "2"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   WHEN  ( WRK-SYMD (5:)   =   SPACE )
                       MOVE    "1"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   WHEN  OTHER
                       MOVE    SPACE       TO  SPA-GMN-SRYYM (7:)
                                               SPA-NAI-SRYYM (7:)
                       MOVE    "0"         TO  WRK-CMB-CD
                       PERFORM 4200-MODELST-CHK-SEC
                       MOVE    WRK-CMB-ITM TO  SPA-GMN-MODE-G
                   END-EVALUATE
               END-IF
      *
               PERFORM 3101-COMBO-EDIT-SEC
      *
               PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               MOVE    1               TO  FLG-SYULIST-ROW
               PERFORM 300-SYUNOU-HENSYU-SEC
               IF    ( SPA-GMN-MAX     >   ZERO )
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
      *
               MOVE    "0033"          TO  SPA-ERRCD
      *
               MOVE    WRK-SRYYMD      TO  WRK-SYMD
               PERFORM 801-HIZUKE-CHK-SEC
               MOVE    WRK-SYMD    TO  SPA-NAI-SRYYM
               MOVE    WRK-HENYMDG TO  SPA-GMN-SRYYM
           END-IF
      *
           .
       4104-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-LSTSEL-SEC                 SECTION.
      *
      *    行選択
           MOVE    ZERO            TO  SPA-GMN-SELBAN
                                       SPA-GMN-TBL-SEL
      *
           PERFORM VARYING IDX-LST    FROM    1   BY  1
                   UNTIL ( IDX-LST    >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL    >   ZERO )
      *
               IF    ( S02-SYULIST-SELECT (IDX-LST)    =   "T" )
                   MOVE    IDX-LST TO  SPA-GMN-SELBAN
                                       SPA-GMN-TBL-SEL
               END-IF
      *
           END-PERFORM
      *
           .
       4105-LSTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4106-NYUGAIKBN-CHK-SEC          SECTION.
      *   
           MOVE    5                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-NYUGAIKBN (1:1) TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    モード  入力処理
      *****************************************************************
       4106-MODE-CHK-SEC               SECTION.
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           IF    ( S02-MODE (1:1)      =   SPA-GMN-MODE )
               CONTINUE
           ELSE
      *
               MOVE    S02-MODE (1:1)  TO  WRK-CMB-CD
               PERFORM 4200-MODELST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-MODE-G
      *
               MOVE    SPA-NAI-THSRYYM
                                       TO  S02-SRYYM
               IF    ( SPA-GMN-TBL-SEL >   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)
                                       TO  S02-SRYYM
               END-IF
      *
               EVALUATE    SPA-GMN-MODE
               WHEN    CONST-MODE-NORMON
                   IF    ( S02-SRYYM (1:6) IS  NUMERIC )
                       MOVE    SPACE       TO  S02-SRYYM (7:)
                   ELSE
                       MOVE    SPACE       TO  S02-SRYYM
                       MOVE    SPA-SYSYMD (1:6)
                                           TO  S02-SRYYM (1:6)
                   END-IF
               WHEN    CONST-MODE-NORYEAR
                   IF    ( S02-SRYYM (1:4) IS  NUMERIC )
                       MOVE    SPACE       TO  S02-SRYYM (5:)
                   ELSE
                       MOVE    SPACE       TO  S02-SRYYM
                       MOVE    SPA-SYSYMD (1:4)
                                           TO  S02-SRYYM (1:4)
                   END-IF
               WHEN    CONST-MODE-NORALL
                       MOVE    SPACE       TO  S02-SRYYM
               WHEN    CONST-MODE-MISYU
                       MOVE    SPACE       TO  S02-SRYYM
               WHEN    CONST-MODE-NYUHEN
                       MOVE    SPA-SYSYMD  TO  S02-SRYYM
               END-EVALUATE
      *
               PERFORM 4104-SRYYM-CHK-SEC
      *
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順  入力処理
      *****************************************************************
       4106-SORTKBN-CHK-SEC               SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
           PERFORM 4200-SAVE-SYULST-SEL-SEC
      *
           INITIALIZE                  SPA-GMN-PAGE
      *
           MOVE    S02-SORTKBN (1:1) TO  WRK-CMB-CD
           PERFORM 4200-SORTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM       TO  SPA-GMN-SORTKBN-G
      *
           MOVE    1               TO  FLG-SYULIST-ROW
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 4200-LOAD-SYULST-SEL-SEC
      *
           .
       4106-SORTKBN-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    戻る　処理
      **************************************************************
       210-BACK                    SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    診療行為より遷移時、診療行為へ
           MOVE    SPACE               TO  LINKAREA
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N" OR "KT01"
                                       OR  "U02" OR "I01" )
               MOVE    SPA-MOTOPG2     TO  SPA-SAKIPG
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-MOTOPG2
               MOVE    SPACE           TO  LINKAREA
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           ELSE
               MOVE    "M01"           TO  SPA-SAKIPG
               MOVE    "S02"           TO  SPA-MOTOPG
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
           END-IF
      *
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      **************************************************************
      *    再発行処理
      **************************************************************
       419-SAIHAKKOU-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELBAN      NOT =   ZERO )
               IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN)
                                       =   "3" )
                   MOVE    "0010"          TO  SPA-ERRCD
               END-IF
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    "2102"      TO  SPA-SIDCD
               END-IF
           END-IF
      *
           .
       419-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    再発行処理
      **************************************************************
       420-SAIHAKKOU-SEC           SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
      *
           IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN)
                                       =   "3" )
               MOVE    "0010"          TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       =   "1" )
                   PERFORM 4201-NYU-SAIHAKKOU-SEC
               ELSE
                   PERFORM 4201-GAI-SAIHAKKOU-SEC
               END-IF
           END-IF
      *
           .
       420-SAIHAKKOU-EXT.
           EXIT.
      *
      **************************************************************
      *    入院再発行処理
      **************************************************************
       4201-NYU-SAIHAKKOU-SEC          SECTION.
      *
      *    収納マスタを読み込みし、診療日の取込み
      *    選択された伝票番号から再発行を行う
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF   (  FLG-SYUNOU          =   ZERO )
               MOVE   SPA-AREA         TO  WRK-SPA-AREA
               MOVE   SYU-SRYYMD       TO  WRK-SPA-SRYYMD
               MOVE   SYU-NYUGAIKBN    TO  WRK-SPA-NYUGAIKBN
               MOVE   SPA-GMN-PTID     TO  WRK-SPA-PTID
               MOVE   SPA-GMN-PTNUM    TO  WRK-SPA-PTNUM
               MOVE   SPA-GMN-NAME     TO  WRK-SPA-NAME
      *
      *        労災判定
               IF   ( SPA-NAI-THKNNUM (SPA-GMN-SELBAN)
                                       =   "971" OR "973" )
                   MOVE    1           TO  WRK-SPA-HKNKBN
               ELSE
                   MOVE    ZERO        TO  WRK-SPA-HKNKBN
               END-IF
      *
               MOVE   SPA-NAI-TRSI-HKNKBN (SPA-GMN-SELBAN)
                                       TO  WRK-SPA-RSI-HKNKBN
      *
               PERFORM 4201-NYU-PRINT-PG-SEC
      *
               INITIALIZE      ORCHCN03AREA
      *
               MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HEAD-HOSPNUM
               MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
               MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
               MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
               MOVE    1               TO  ORCHCN03-CNT
      *
               MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HOSPNUM (1)
               MOVE    WRK-SPA-PTID    TO  ORCHCN03-PTID   (1)
               MOVE    WRK-SPA-PTNUM   TO  ORCHCN03-PTNUM  (1)
               MOVE    "1"             TO  ORCHCN03-NYUGAIKBN (1)
      *
               MOVE    SYU-SKYSTYMD    TO  ORCHCN03-KJNYMD  (1)
               MOVE    SYU-DENPNUM     TO  ORCHCN03-DENPNUM (1)
               MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                                (1)
               MOVE    SYU-SKYMONEY    TO  ORCHCN03-SEIKYU  (1)
               MOVE    SYU-NYUKIN-TOTAL
                                       TO  ORCHCN03-NYUKIN  (1)
               MOVE    1               TO  ORCHCN03-HAKKOFLG(1)
               MOVE    1               TO  ORCHCN03-MOTOID
      *
               IF    ( FLG-SEIKYU-PRT  =   1 )
                   MOVE    "01"            TO      WRK-SPA-PRT-FLG
                   CALL    "ORCHCN03OP"    USING   ORCHCN03AREA
                                                   WRK-SPA-AREA
                   MOVE    SPACE           TO  WRK-SPA-PRT-FLG
               END-IF
      *
               IF    ( FLG-MEISAI-PRT  =   1 )
                   PERFORM 42011-NYU-MEISAI-PRT-SEC
               END-IF
      *
           END-IF
      *
           .
       4201-NYU-SAIHAKKOU-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書出力処理
      *****************************************************************
       42011-NYU-MEISAI-PRT-SEC        SECTION.
      *
           INITIALIZE          ORCHCN03S02AREA
           MOVE    1       TO  ORCHCN03S02-IDX
           CALL    "ORCHCN03S02"   USING
                                   ORCHCN03AREA
                                   ORCHCN03S02AREA
                                   SYUNOU-REC
                                   WRK-SPA-AREA
           IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *
               INITIALIZE                  ORCHCN04AREA
      *
               MOVE    WRK-SPA-PTID    TO  ORCHCN04-PTID
               MOVE    WRK-SPA-PTNUM   TO  ORCHCN04-PTNUM
               MOVE    WRK-SPA-NAME    TO  ORCHCN04-NAME
               MOVE    SYU-SKYSTYMD    TO  ORCHCN04-SKYSTYMD
               MOVE    SYU-SKYEDYMD    TO  ORCHCN04-SKYEDYMD
               MOVE    1               TO  ORCHCN04-HAKKOFLG
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   ORCHCN03S02-GRP-CNT )
      *
                   MOVE    ORCHCN03S02-GRP-DENPNUM (IDX0)
                                       TO  ORCHCN04-DENPNUM (IDX0)
      *
               END-PERFORM
      *
               MOVE    "05"            TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG    USING
                                        WRK-SPA-AREA
                                        ORCHCN04AREA
               MOVE    SPACE           TO  WRK-SPA-PRT-FLG
           END-IF
      *
           .
       42011-NYU-MEISAI-PRT-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       4201-GAI-SAIHAKKOU-SEC          SECTION.
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4201-GAI-SAIHAKKOU-EXT
           END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
      *    帳票プログラム取得（外来）
           PERFORM 4201-PRINT-PG-SEC
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-NYU-MISYU
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-GAI-MISYU
           MOVE    WRK-MISYU-P         TO  WRK-GAI-MISYU-P
           MOVE    WRK-MISYU-M         TO  WRK-GAI-MISYU-M
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    WRK-SPA-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    WRK-SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           MOVE    SYU-SRYYMD          TO  WRK-GAI-SRYYMD
           MOVE    SYU-DENPNUM         TO  WRK-GAI-DENPNUM
           MOVE    SYU-SKYMONEY        TO  WRK-GAI-SEIKYU
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-GAI-NYUKIN
      *
           COMPUTE WRK-GAI-ZENMISYU
               =   SYU-SKYMONEY        -   SYU-NYUKIN-TOTAL
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
      *
           MOVE    WRK-NYU-MISYU       TO  SYUPARA01-NYU-MISYU
           MOVE    WRK-GAI-MISYU       TO  SYUPARA01-GAI-MISYU
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
      *
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
           MOVE    1                   TO  SYUPARA01-HAKKOFLG
           IF    ( SYU-GRP-DENPNUM     NOT =   ZERO )
             AND ( SYS-1039-S02SKYSAIKBN   =   "1" )
               MOVE    SYU-GRP-HAKHOUFLG
                                       TO  SYUPARA01-SKYSUMFLG
           ELSE
               MOVE    ZERO            TO  SYUPARA01-SKYSUMFLG
           END-IF
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    C-UID               TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
      *
           IF    ( SYU-GRP-DENPNUM     NOT =   ZERO )
            AND  ( SYS-1039-S02SKYSAIKBN   =   "1" )
      *
               PERFORM 900-SYUNOU-KEY27-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
                   COMPUTE SYUPARA01-GAI-MISYU
                       =   SYUPARA01-GAI-MISYU
                       - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
                   IF    ( SYU-SKYMONEY        <   SYU-NYUKIN-TOTAL )
                       COMPUTE SYUPARA01-GAI-MISYU-M
                           =   SYUPARA01-GAI-MISYU-M
                           - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
                   ELSE
                       COMPUTE SYUPARA01-GAI-MISYU-P
                           =   SYUPARA01-GAI-MISYU-P
                           - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
                   END-IF
      *
                   PERFORM 42011-SYUPARA02-HEN-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4201-GAI-SAIHAKKOU-EXT
                   END-IF
      *
                   PERFORM 900-SYUNOU-KEY27-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           ELSE
      *
               COMPUTE SYUPARA01-GAI-MISYU
                   =   SYUPARA01-GAI-MISYU
                   - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
               IF    ( SYU-SKYMONEY        <   SYU-NYUKIN-TOTAL )
                   COMPUTE SYUPARA01-GAI-MISYU-M
                       =   SYUPARA01-GAI-MISYU-M
                       - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
               ELSE
                   COMPUTE SYUPARA01-GAI-MISYU-P
                       =   SYUPARA01-GAI-MISYU-P
                       - ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
               END-IF
      *
               PERFORM 42011-SYUPARA02-HEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4201-GAI-SAIHAKKOU-EXT
               END-IF
           END-IF
      *
           IF    ( SYS-1039-S02SKYMISYUPRTKBN  =   "0" )
               MOVE    ZERO            TO  SYUPARA01-NYU-MISYU
                                           SYUPARA01-GAI-MISYU
                                           SYUPARA01-GAI-MISYU-P
                                           SYUPARA01-GAI-MISYU-M
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    C-UID               TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SYUPARA01-AREA      TO  PARA-DATA-REC
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4001"      TO  SPA-ERRCD
               GO  TO  4201-GAI-SAIHAKKOU-EXT
           END-IF
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
           MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
           MOVE    C-UID           TO      ORCHC03S02-TERMID
           MOVE    "00101"         TO      WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI     TO      ORCHC03S02-FILEMEI
           CALL    "ORCHC03S02"    USING   WRK-SPA-AREA
                                           ORCHC03S02AREA
           IF    ( ORCHC03S02-RC   NOT =   ZERO )
               MOVE    "4003"      TO  SPA-ERRCD
               GO  TO  4201-GAI-SAIHAKKOU-EXT
           END-IF
      *
           MOVE    WRK-GAI-SRYYMD      TO  WRK-SPA-SRYYMD
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           IF    ( FLG-SEIKYU-PRT      =   1 )
               PERFORM 42011-GAI-SEIKYU-PRT-SEC
           END-IF
      *
           IF    ( FLG-MEISAI-PRT      =   1 )
               PERFORM 42011-GAI-MEISAI-PRT-SEC
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
      *
           .
       4201-GAI-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    外来請求書兼領収書再発行処理
      **************************************************************
       42011-GAI-SEIKYU-PRT-SEC    SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    WRK-GAI-SEIKYU      TO  ORCHC03-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  ORCHC03-NYUKIN
           MOVE    1                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
           IF    ( SYS-1039-S02SKYMISYUPRTKBN  =   "0" )
               CONTINUE
           ELSE
               COMPUTE ORCHC03-ZENMISYU
                   =   WRK-GAI-MISYU
                   -   WRK-GAI-ZENMISYU
               IF    ( ORCHC03-ZENMISYU    <   ZERO )
                   MOVE    ORCHC03-ZENMISYU
                                       TO  ORCHC03-HENKIN
                   MOVE    ZERO        TO  ORCHC03-ZENMISYU
               END-IF
      *
               MOVE    WRK-GAI-MISYU-P TO  ORCHC03-ZENMISYU-2
      *
               COMPUTE ORCHC03-KANYUKIN
                   =   WRK-GAI-MISYU-M     *   -1
      *
               IF    ( WRK-GAI-ZENMISYU    <   ZERO )
                   COMPUTE ORCHC03-KANYUKIN
                       =   ORCHC03-KANYUKIN
                       - ( WRK-GAI-ZENMISYU    *   -1 )
               ELSE
                   COMPUTE ORCHC03-ZENMISYU-2
                       =   ORCHC03-ZENMISYU-2
                       -   WRK-GAI-ZENMISYU
               END-IF
           END-IF
           MOVE    "S02"               TO  ORCHC03-PARA-GYOUMUID
           MOVE    C-UID               TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    1                   TO  ORCHC03-MOTOID
           MOVE    "02"                TO  WRK-SPA-PRT-FLG
           CALL    WRK-SEIKYU-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           .
       42011-GAI-SEIKYU-PRT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書サブ呼出処理
      *****************************************************************
       42011-GAI-MEISAI-PRT-SEC        SECTION.
      *
           MOVE    ZERO                    TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
      *
               INITIALIZE                  ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM
                                       TO  ORCHC04-DENPNUM
               MOVE    1               TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG
                                       TO  ORCHC04-HAKHOUFLG
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
               MOVE    WRK-PAGE-CNT
                                       TO  ORCHC04-PAGE-CNT
      *
               MOVE    "04"        TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE       TO  WRK-SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                   TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       42011-GAI-MEISAI-PRT-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       42011-SYUPARA02-HEN-SEC         SECTION.
      *
           COMPUTE PARA-EDANUM =   PARA-EDANUM +   1
      *
           INITIALIZE              SYUPARA02-AREA
           MOVE    SYU-DENPNUM     TO  SYUPARA02-DENPNUM
           MOVE    SYU-NYUKIN-TOTAL TO  SYUPARA02-NYUKIN
           MOVE    SYUPARA02-AREA  TO  PARA-DATA-REC
           MOVE    PARA-REC    TO  MCPDATA-REC
           MOVE    "tbl_para"      TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
       42011-SYUPARA02-HEN-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       490-GAI-TUKISKYPRT-SEC          SECTION.
      *
           IF     ( SPA-GMN-PTID   =   ZERO )
               MOVE    "1003"      TO  SPA-ERRCD
           ELSE
      *
               MOVE    SPACE       TO  SPA-TUKI-SRYYM
      *
               IF    ( SPA-NAI-SRYYM (1:6) IS  NUMERIC )
                   MOVE    SPA-NAI-SRYYM
                                   TO  SPA-TUKI-SRYYM
               END-IF
               IF    ( SPA-GMN-TBL-SEL NOT =   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)(1:6)
                                   TO  SPA-TUKI-SRYYM
               END-IF
      *
               INITIALIZE              SPA-S01KYOUTU
      *
               PERFORM 4901-SPAKYOTUKEY-SET-SEC
               MOVE    SPA-TUKI-SRYYM
                                       TO  SPA-S01-TUKI-SRYYM
      *
               MOVE    SPA-S01KYOUTU   TO  SPA-WORK
               MOVE    SPA-AREA        TO  SPA-COMMON
               MOVE    SPA-S02         TO  SPA-FREE
      *
               MOVE    "LINK"          TO  MCP-STATUS
               MOVE    SPACE           TO  MCP-EVENT
      *
               CALL    "ORCGS07"       USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
      *
               MOVE    SPA-COMMON      TO  SPA-AREA
               MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               MOVE    SPA-FREE        TO  SPA-S02
      *
               MOVE    "S02"           TO  SPA-MOTOPG
               MOVE    "S07"           TO  SPA-SAKIPG
      *
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "S07    "       TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       490-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       412-S10-SENI-SEC            SECTION.
      *
           MOVE    SPACE       TO  SPA-TUKI-SRYYM
      *
           IF    ( SPA-NAI-SRYYM (1:6) IS  NUMERIC )
               MOVE    SPA-NAI-SRYYM
                               TO  SPA-TUKI-SRYYM
           END-IF
           IF    ( SPA-TUKI-SRYYM  =   SPACE )
               MOVE    SPA-SYSYMD(1:6)
                               TO  SPA-TUKI-SRYYM
           END-IF
      *
           INITIALIZE              SPA-S01KYOUTU
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
           MOVE    SPA-TUKI-SRYYM
                               TO  SPA-S01-TUKI-SRYYM
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-S02         TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGS10"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE        TO  SPA-S02
      *
           MOVE    "S02"           TO  SPA-MOTOPG
           MOVE    "S10"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "S10"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       412-S10-SENI-EXT.
           EXIT.
      **************************************************************
      *    請求年月チェック処理
      **************************************************************
       4901-SKYYM-CHK-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-SKYYM
      *
           IF    ( SPA-GMN-PTID    NOT =   ZERO )
            AND  ( SPA-GMN-MAX     >   ZERO )
               MOVE    SPACE           TO  WRK-SYMD
               IF    ( SPA-NAI-SRYYM (1:6)     IS  NUMERIC )
                   MOVE    SPA-NAI-SRYYM
                                       TO  WRK-SKYYM
               END-IF
               IF    ( SPA-GMN-TBL-SEL NOT =   ZERO )
                   MOVE    SPA-NAI-TSRYYMD (SPA-GMN-TBL-SEL)(1:6)
                                       TO  WRK-SKYYM
               END-IF
           END-IF
      *
           .
       4901-SKYYM-CHK-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       4901-GAI-TUKISKYPRT-SEC         SECTION.
      *
           MOVE    SPA-S01-TUKI-SRYYM
                                   TO  SPA-TUKI-SRYYM
      *
           MOVE    SPA-TUKI-SRYYM  TO  WRK-SKYYM
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
      *
           PERFORM 800-ORCUIDSET-SEC
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SMCNDATE-YMD    TO  WRK-FILEMEI-YMD
           MOVE    SMCNDATE-HMS    TO  WRK-FILEMEI-HMS
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4901-GAI-TUKISKYPRT-EXT
           END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-GMN-PTID        TO  WRK-SPA-PTID
           MOVE    "2"                 TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTNUM       TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-NAME        TO  WRK-SPA-NAME
      *
      *    帳票プログラム取得（外来）
           PERFORM 4201-PRINT-PG-SEC
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1039-STYUKYMD
                                           SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-NYU-MISYU
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-TUKIMISYU-SUM-SEC
           MOVE    WRK-MISYU           TO  WRK-GAI-MISYU
           MOVE    WRK-MISYU-P         TO  WRK-GAI-MISYU-P
           MOVE    WRK-MISYU-M         TO  WRK-GAI-MISYU-M
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    WRK-SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-S01-TUKI-DENPNUM TO SYU-DENPNUM
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           MOVE    SYU-SRYYMD          TO  WRK-GAI-SRYYMD
           MOVE    SYU-DENPNUM         TO  WRK-GAI-DENPNUM
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-GAI-NYUKIN
      *
           INITIALIZE                  SYUPARA01-AREA
                                       SYUPARA02-AREA
      *
           MOVE    "2"                 TO  SYUPARA01-NYUGAIKBN
           MOVE    WRK-NYU-MISYU       TO  SYUPARA01-NYU-MISYU
           MOVE    WRK-GAI-MISYU       TO  SYUPARA01-GAI-MISYU
           MOVE    WRK-GAI-MISYU-P     TO  SYUPARA01-GAI-MISYU-P
           MOVE    WRK-GAI-MISYU-M     TO  SYUPARA01-GAI-MISYU-M
           MOVE    ZERO                TO  SYUPARA01-NYU-NYUKIN
           MOVE    ZERO                TO  SYUPARA01-GAI-NYUKIN
           MOVE    4                   TO  SYUPARA01-HAKKOFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
           IF    ( SYS-1039-TUKISKYMISYUPRTKBN =   "0" )
               MOVE    ZERO            TO  SYUPARA01-NYU-MISYU
                                           SYUPARA01-GAI-MISYU
                                           SYUPARA01-GAI-MISYU-P
                                           SYUPARA01-GAI-MISYU-M
           END-IF
      *
           EVALUATE    SPA-S01-TUKI-GRPKBN
           WHEN    "1"
               MOVE    1               TO  SYUPARA01-SKYSUMFLG
           WHEN    "2"
               MOVE    2               TO  SYUPARA01-SKYSUMFLG
           WHEN    "3"
               MOVE    3               TO  SYUPARA01-SKYSUMFLG
           WHEN    "4"
               MOVE    4               TO  SYUPARA01-SKYSUMFLG
           WHEN    OTHER
               MOVE    4               TO  SYUPARA01-SKYSUMFLG
           END-EVALUATE
      *
           IF    ( SYS-1039-TUKISUMKBN     =   "1" )
               MOVE    "1"             TO  SYUPARA01-TUKISUMKBN
           ELSE
               MOVE    "0"             TO  SYUPARA01-TUKISUMKBN
           END-IF
      *
           MOVE    SPA-S01-TUKI-KIKANKBN
                                       TO  SYUPARA01-TUKIKIKANKBN
           MOVE    SPA-S01-TUKI-STYMD  TO  SYUPARA01-TUKISTYMD
           MOVE    SPA-S01-TUKI-EDYMD  TO  SYUPARA01-TUKIEDYMD
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    C-UID               TO  PARA-TERMID
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    1                   TO  PARA-EDANUM
           MOVE    SYUPARA01-AREA      TO  PARA-DATA-REC
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4001"      TO  SPA-ERRCD
               GO  TO  4901-GAI-TUKISKYPRT-EXT
           END-IF
      *
           MOVE     "__"               TO  SYU-SRYYMD (7:2)
           PERFORM 900-SYUNOU-KEY73-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    1               TO  FLG-HIT
      *
               IF    ( SPA-S01-TUKI-KIKANKBN   =   "1" )
                   IF    ( SYU-SRYYMD  >=  SPA-S01-TUKI-STYMD )
                    AND  ( SYU-SRYYMD  <=  SPA-S01-TUKI-EDYMD )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-HIT
                   END-IF
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
                   PERFORM 42011-SYUPARA02-HEN-SEC
                   IF    ( SPA-ERRCD       NOT =   SPACE )
                       GO  TO  4901-GAI-TUKISKYPRT-EXT
                   END-IF
               END-IF
      *
               PERFORM 900-SYUNOU-KEY73-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
           MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
           MOVE    C-UID           TO      ORCHC03S02-TERMID
           MOVE    "00101"         TO      WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI     TO      ORCHC03S02-FILEMEI
           CALL    "ORCHC03S02"    USING   WRK-SPA-AREA
                                           ORCHC03S02AREA
           IF    ( ORCHC03S02-RC   NOT =   ZERO )
               MOVE    "4003"      TO  SPA-ERRCD
               GO  TO  4901-GAI-TUKISKYPRT-EXT
           END-IF
      *
           MOVE    WRK-GAI-SRYYMD      TO  WRK-SPA-SRYYMD
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           IF    ( SPA-S01-TUKI-SKYPRT =   "1" )
               PERFORM 49011-GAI-TUKISKYPRT-SEC
           END-IF
      *
           IF    ( SPA-S01-TUKI-SRYPRT =   "1" )
               PERFORM 49011-GAI-TUKIMEISAI-PRT-SEC
           END-IF
      *
           MOVE    "001%"          TO  WRK-FILEMEI-CD
           PERFORM 4801-DELETE-PARA-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4901-GAI-TUKISKYPRT-EXT
           END-IF
      *
           .
       4901-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       49011-GAI-TUKISKYPRT-SEC        SECTION.
      *
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    WRK-GAI-SEIKYU      TO  ORCHC03-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  ORCHC03-NYUKIN
           MOVE    4                   TO  ORCHC03-HAKKOFLG
           MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC03-HAKHOUFLG
           MOVE    1                   TO  ORCHC03-PARAFLG
      *
           MOVE    "2"                 TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SUM-SEC
      *
           IF    ( SYS-1039-TUKISKYMISYUPRTKBN =   "0" )
               CONTINUE
           ELSE
               COMPUTE ORCHC03-ZENMISYU
                   =   WRK-GAI-MISYU
               IF    ( ORCHC03-ZENMISYU    <   ZERO )
                   MOVE    ORCHC03-ZENMISYU
                                       TO  ORCHC03-HENKIN
                   MOVE    ZERO        TO  ORCHC03-ZENMISYU
               END-IF
      *
               MOVE    WRK-GAI-MISYU-P TO  ORCHC03-ZENMISYU-2
      *
               COMPUTE ORCHC03-KANYUKIN
                   =   WRK-GAI-MISYU-M     *   -1
           END-IF
      *
           MOVE    "S02"               TO  ORCHC03-PARA-GYOUMUID
           MOVE    C-UID               TO  ORCHC03-PARA-TERMID
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    2                   TO  ORCHC03-MOTOID
           MOVE    "03"                TO  WRK-SPA-PRT-FLG
      *
           CALL    WRK-SEIKYU-PG           USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           .
       49011-GAI-TUKISKYPRT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書サブ呼出処理
      *****************************************************************
       49011-GAI-TUKIMEISAI-PRT-SEC    SECTION.
      *
           MOVE    ZERO                    TO  WRK-PAGE-CNT
      *
           PERFORM 900-PARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
      *
               MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
      *
               INITIALIZE                  ORCHC04AREA
               MOVE    SYUPARA03-DENPNUM   TO  ORCHC04-DENPNUM
               MOVE    4                   TO  ORCHC04-HAKKOFLG
               MOVE    SYUPARA01-SKYSUMFLG TO  ORCHC04-HAKHOUFLG
               MOVE    SPA-SYSYMD          TO  ORCHC04-DENPPRTYMD
               MOVE    SYUPARA03-STSRYYMD  TO  ORCHC04-KIKAN-STYMD
               MOVE    SYUPARA03-EDSRYYMD  TO  ORCHC04-KIKAN-EDYMD
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   SYUPARA03-GRP-DENPNUM-MAX )
      *
                   MOVE    SYUPARA03-GRP-DENPNUM (IDXD)
                                           TO  ORCHC04-KIKAN-DENPNUM
                                                               (IDXD)
               END-PERFORM
      *
               MOVE    WRK-PAGE-CNT    TO  ORCHC04-PAGE-CNT
               MOVE    "04"            TO  WRK-SPA-PRT-FLG
               CALL    WRK-MEISAI-PG       USING
                                           WRK-SPA-AREA
                                           ORCHC04AREA
               MOVE    SPACE           TO  WRK-SPA-PRT-FLG
               MOVE    ORCHC04-PAGE-CNT
                                       TO  WRK-PAGE-CNT
      *
               PERFORM 900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       49011-GAI-TUKIMEISAI-PRT-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       4801-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    C-UID               TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4000"          TO  SPA-ERRCD
           END-IF
      *
           .
       4801-DELETE-PARA-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4201-PRINT-PG-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-SEIKYU-PG
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHC03"           TO  WRK-SEIKYU-PG
           END-IF
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHC04"           TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       4201-PRINT-PG-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理 （入院）
      *****************************************************************
       4201-NYU-PRINT-PG-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SEIKYU-PG
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHCN03"          TO  WRK-SEIKYU-PG
           END-IF
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG  TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHCN04"       TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       4201-NYU-PRINT-PG-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消処理
      **************************************************************
       430-TORIKESI-SEC                SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
      *
      *        外来分の請求取消は不可
               IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       =   "2" )
                       MOVE    "0003"  TO  SPA-ERRCD
               END-IF
      *
      *        請求取消データの取消不可
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-NAI-TJYOTAIKBN (SPA-GMN-SELBAN)
                                       =   "3" )
                       MOVE    "0003"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   MOVE    "2001"      TO  SPA-SIDCD
               END-IF
           END-IF
      *
           .
      *
       430-TORIKESI-EXT.
           EXIT.
      **************************************************************
      *    請求取消更新処理
      **************************************************************
       430-TORIKESI-KOUSHIN-SEC        SECTION.
      *
           MOVE    SPA-GMN-SELBAN      TO  IDX4
      *
      *    収納マスタと収納明細の請求金額を０円に変更する
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (IDX4)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM   (IDX4)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           IF    ( FLG-SYUNOU          =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
           IF    ( SYU-CREATEKBN   =   2 )
      *
      *        患者定期請求履歴更新処理
               PERFORM 4301-PTTEIKIRRK-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  430-TORIKESI-KOUSHIN-EXT
               END-IF
           END-IF
      *
           MOVE    "3"             TO  SYU-DENPJTIKBN
           MOVE    ZERO            TO  SYU-SKYMONEY
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
           PERFORM 800-SS002-SEC
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   ( MCP-RC       NOT =   ZERO )
                MOVE    "1002"     TO  SPA-ERRCD
                GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
      *    収納明細作成処理
           PERFORM    4301-SMEI-SAKUSEI-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  430-TORIKESI-KOUSHIN-EXT
           END-IF
      *
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       430-TORIKESI-KOUSHIN-EXT.
           EXIT.
      *
      **************************************************************
      *    請求取消処理（収納明細レコード作成）
      **************************************************************
       4301-SMEI-SAKUSEI-SEC           SECTION.
      *
           PERFORM 900-SYUMEI-KEY7-SEL-SEC
           COMPUTE IDX5    =   SMEI-DENPEDANUM +   1
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (IDX4)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM   (IDX4)
                                       TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX5                TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SPA-NAI-TSRYKA (IDX4)
                                       TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           COMPUTE SMEI-SKYMONEY   =   SPA-GMN-TSEIKYU (IDX4)
                                   *   -1
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
      *    既に存在する場合はエラー
           MOVE SYUMEI-REC             TO  WKSYUMEI-REC
      *
           PERFORM 900-SYUMEI-KEY-SEL-SEC
           IF    ( FLG-SYUMEI          =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  4301-SMEI-SAKUSEI-EXT
           END-IF
      *
           MOVE    WKSYUMEI-REC        TO  SYUMEI-REC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4301-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      **************************************************************
      *    患者定期請求履歴更新処理
      **************************************************************
       4301-PTTEIKIRRK-UPD-SEC         SECTION.
      *
      *    患者定期請求情報更新
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "1004"      TO  SPA-ERRCD
               GO  TO  4301-PTTEIKIRRK-UPD-EXT
           END-IF
      *
           .
       4301-PTTEIKIRRK-UPD-EXT.
           EXIT.
      **************************************************************
      *    再計算処理
      **************************************************************
       440-SAIKEI-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      440-SAIKEI-EXT
           END-IF
      *
      *    共通パラメタ更新・画面遷移
      *    PERFORM 4901-PARA-HEN-SEC
      *
      ******************************
      *    負担金再計算処理
      ******************************
           INITIALIZE                   LNK-ORCSSAIKEISAN-REC
      *
           MOVE     SPA-HOSPNUM     TO   LNK-ORCSSKS-HOSPNUM
           MOVE     SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                   TO   LNK-ORCSSKS-NYUGAIKBN
           MOVE     SPA-GMN-PTID   TO   LNK-ORCSSKS-PTID
           MOVE     SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                   TO   LNK-ORCSSKS-DENPNUM
           MOVE     SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                   TO   LNK-ORCSSKS-SRYKA
      *
           CALL    "ORCSSAIKEISAN"     USING
                                       SPA-AREA
                                       LNK-ORCSSAIKEISAN-REC
      *
           INITIALIZE                 ORCSSYUACCAREA
           MOVE    "03"           TO  LNK-SYUACC-I-KBN
           MOVE    SPA-NAI-TSRYYMD (SPA-GMN-SELBAN)(1:6)
                                  TO  LNK-SYUACC-I-SRYYM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                  TO  SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTID   TO  SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY
                                  TO  SPA-BIRTHDAY
           CALL    "ORCSSYUKAMAIN"    USING
                                      SPA-AREA
                                      ORCSSYUACCAREA
      *
      *****PERFORM    310-SPA-SET-SEC.
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       440-SAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    外来分一括再計算チェック処理
      **************************************************************
       440-IKATUSAIKEI-CHK-SEC       SECTION.
      *
           PERFORM 4901-SKYYM-CHK-SEC
           IF    ( WRK-SKYYM           =   SPACE )
               MOVE    "0060"          TO  SPA-ERRCD
           ELSE
      *
               MOVE    ZERO                TO  FLG-HIT
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   SPA-GMN-MAX )
                        OR   ( FLG-HIT =   1 )
      *
                   IF    ( SPA-NAI-TSRYYMD (IDX3)(1:6) =   WRK-SKYYM )
                    AND  ( SPA-NAI-TNYUGAIKBN (IDX3)   =   "2" )
                       MOVE    1           TO  FLG-HIT
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-HIT         =   ZERO )
                   MOVE    "0060"          TO  SPA-ERRCD
               ELSE
                   MOVE    "2002"          TO  SPA-SIDCD
               END-IF
      *
           END-IF
      *
           .
      *
       440-IKATUSAIKEI-CHK-EXT.
           EXIT.
      **************************************************************
      *    外来分一括再計算処理
      **************************************************************
       440-IKATUSAIKEI-SEC           SECTION.
      *
           PERFORM 4901-SKYYM-CHK-SEC
      *
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    WRK-SKYYM           TO  SYS-1039-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1039-STYUKYMD(7:2)
           MOVE    SYS-1039-STYUKYMD   TO  SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1039-REC
      *
           MOVE    ZERO                TO  IDX6
           INITIALIZE     WRK-DENPNUM-END-TABLE
      *
      ***  IF    ( SPA-GMN-NYUGAIKBN   =   "0" OR  "2" )
      ***      MOVE    "2"             TO  WRK-NYUGAIKBN
      ***  ELSE
      ***      GO      TO      440-IKATUSAIKEI-EXT
      ***  END-IF
           MOVE    "2"                 TO  WRK-NYUGAIKBN
      *
      *    現物給付計算：一括再計算対象の収納データがあるかチェック
      *
           MOVE    ZERO                TO  FLG-GENBUTU
           MOVE    ZERO                TO  FLG-GENBUTU2
           MOVE    ZERO                TO  FLG-GENBUTU3
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    WRK-SKYYM           TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    "tbl_syunou"        TO  WRK-TABLE
           MOVE    "key35"             TO  WRK-PATHNAME
           PERFORM 900-SYUNOU-SEL-IKT-SEC
           PERFORM   UNTIL   FLG-SYUNOU  =  1
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               IF    SYU-DENPJTIKBN  NOT =  "3"
                 IF   SYU-SAIKEISANKBN    =  "1"
                    MOVE    1     TO  FLG-GENBUTU2
                 END-IF
                 IF  (SYU-SYUHKNNUM   NOT = SPACE)
                 AND (SYU-KOH1HKNNUM  NOT = SPACE)
                    MOVE    1     TO  FLG-GENBUTU3
                 END-IF
               END-IF
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key35"         TO  WRK-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
           END-PERFORM
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key35"        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF   FLG-GENBUTU2  =   1
           AND  FLG-GENBUTU3  =   1
                    MOVE    1     TO  FLG-GENBUTU
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    WRK-SKYYM           TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           IF   FLG-GENBUTU = ZERO
             IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                MOVE    "tbl_syunou"   TO  WRK-TABLE
                MOVE    "key35"        TO  WRK-PATHNAME
             ELSE
                MOVE    "tbl_syunou"   TO  WRK-TABLE
                MOVE    "key69"        TO  WRK-PATHNAME
             END-IF
           ELSE
             IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                MOVE    "tbl_syunou"   TO  WRK-TABLE
                MOVE    "key82"        TO  WRK-PATHNAME
             ELSE
                MOVE    "tbl_syunou"   TO  WRK-TABLE
                MOVE    "key83"        TO  WRK-PATHNAME
             END-IF
           END-IF
           PERFORM 900-SYUNOU-SEL-IKT-SEC
      *
           PERFORM   UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC     TO   SYUNOU-REC
      *
               MOVE    ZERO            TO   FLG-ARI
               PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                   >  100 )
                         OR   (WRK-DENPNUM-END(IDX3)  =  ZERO)
                         OR   (FLG-ARI                =   1  )
                   IF  WRK-DENPNUM-END(IDX3)  =   SYU-DENPNUM
                         MOVE   1    TO   FLG-ARI
                   END-IF
               END-PERFORM
      *
               IF   SYU-DENPJTIKBN  NOT =   "3"
               AND  FLG-ARI             =   ZERO
                  INITIALIZE                   LNK-ORCSSAIKEISAN-REC
      *
                  MOVE     SYU-HOSPNUM     TO   LNK-ORCSSKS-HOSPNUM
                  MOVE     SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
                  MOVE     SYU-PTID       TO   LNK-ORCSSKS-PTID
                  MOVE     SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
                  MOVE     SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
                  MOVE     SYS-1039-IKTSORTKBN
                                          TO   LNK-ORCSSKS-IKTSORTKBN
                  MOVE     SYS-1039-IKTKEISANKBN
                                          TO   LNK-ORCSSKS-IKTKEISANKBN
                  IF   FLG-GENBUTU  =  1
                    MOVE     "1"          TO   LNK-ORCSSKS-IKTKEISANKBN
                    MOVE     "1"          TO   LNK-ORCSSKS-IKTGENKEIFLG
                  END-IF
      *
                  CALL    "ORCSSAIKEISAN"     USING
                                              SPA-AREA
                                              LNK-ORCSSAIKEISAN-REC
      *
                  PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                          >   15 )
                         OR   (LNK-ORCSSKS-DENPNUM-END(IDX3) =  ZERO)
                      ADD    1     TO   IDX6
                      MOVE   LNK-ORCSSKS-DENPNUM-END(IDX3)
                                   TO   WRK-DENPNUM-END(IDX6)
                  END-PERFORM
      *
               END-IF
      *
               IF   FLG-GENBUTU = ZERO
                 IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                    MOVE    "tbl_syunou"   TO  WRK-TABLE
                    MOVE    "key35"        TO  WRK-PATHNAME
                 ELSE
                    MOVE    "tbl_syunou"   TO  WRK-TABLE
                    MOVE    "key69"        TO  WRK-PATHNAME
                 END-IF
               ELSE
                 IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
                    MOVE    "tbl_syunou"   TO  WRK-TABLE
                    MOVE    "key82"        TO  WRK-PATHNAME
                 ELSE
                    MOVE    "tbl_syunou"   TO  WRK-TABLE
                    MOVE    "key83"        TO  WRK-PATHNAME
                 END-IF
               END-IF
               PERFORM 900-SYUNOU-FET-SEC
      *
               IF    SPA-ERRCD   NOT =  SPACE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           IF   FLG-GENBUTU = ZERO
             IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    "key35"        TO  MCP-PATHNAME
             ELSE
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    "key69"        TO  MCP-PATHNAME
             END-IF
           ELSE
             IF   SYS-1039-IKTSORTKBN  =  "0" OR SPACE
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    "key82"        TO  MCP-PATHNAME
             ELSE
               MOVE    "tbl_syunou"   TO  MCP-TABLE
               MOVE    "key83"        TO  MCP-PATHNAME
             END-IF
           END-IF
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    SPA-ERRCD   NOT =  SPACE
                  MOVE    "0064"       TO  SPA-ERRCD
           ELSE
              IF    WRK-DENPNUM-END(1)  =  ZERO
                  MOVE    "0060"       TO  SPA-ERRCD
              END-IF
           END-IF
      *
           INITIALIZE                 ORCSSYUACCAREA
           MOVE    "03"           TO  LNK-SYUACC-I-KBN
           MOVE    WRK-SKYYM      TO  LNK-SYUACC-I-SRYYM
           MOVE    WRK-NYUGAIKBN  TO  SPA-NYUGAIKBN
           MOVE    SPA-GMN-PTID   TO  SPA-PTID
           MOVE    SPA-NAI-BIRTHDAY
                                  TO  SPA-BIRTHDAY
           CALL    "ORCSSYUKAMAIN"    USING
                                      SPA-AREA
                                      ORCSSYUACCAREA
      *
      *****PERFORM    310-SPA-SET-SEC.
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       440-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    返金処理
      **************************************************************
       450-HENKIN-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      450-HENKIN-EXT
           END-IF
      *
      *    収納マスタの請求金額と入金合計額を比較して
      *    入金合計額が多ければ、収納マスタの入金合計額
      *    を請求金額と同じにして、収納明細マスタを作成する
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC                            
      *
           IF      FLG-SYUNOU          =   ZERO
             IF    SYU-SKYMONEY        <  SYU-NYUKIN-TOTAL
                MOVE    ZERO                TO    WRK-SAGAKU
                COMPUTE WRK-SAGAKU = SYU-SKYMONEY - SYU-NYUKIN-TOTAL
                MOVE    SYU-SKYMONEY        TO    SYU-NYUKIN-TOTAL
      *
                PERFORM 800-MCNDATE-SEC
                MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                MOVE    SYUNOU-REC          TO  MCPDATA-REC
                MOVE    "tbl_syunou"        TO  MCP-TABLE
                MOVE    "key"               TO  MCP-PATHNAME
                PERFORM 910-DBUPDATE-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1002"              TO  SPA-ERRCD
                   GO  TO  450-HENKIN-EXT
                END-IF     
      *
                PERFORM 900-SYUMEI-KEY7-SEL-SEC
                COMPUTE IDX1   =   SMEI-DENPEDANUM +  1
      *
                PERFORM    4501-SMEI-SAKUSEI-SEC
             ELSE
                MOVE    "0004"     TO    SPA-ERRCD
             END-IF
           END-IF
     *
           IF      SPA-ERRCD          =   SPACE 
     ******    PERFORM    310-SPA-SET-SEC
      *
               PERFORM 3101-MISYU-EDIT-SEC
      *
               PERFORM 300-SYUNOU-HENSYU-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       450-HENKIN-EXT.
           EXIT.
      *
      *************************************************************
      *    返金処理（収納明細レコード作成）
      *************************************************************
       4501-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX1                 TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "2"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    WRK-SAGAKU          TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "4"                 TO  SMEI-JOUTAIKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"              TO  SPA-ERRCD
           END-IF     
           .
       4501-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      *************************************************************
      *    入金処理
      *************************************************************
       460-NYUKIN-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      460-NYUKIN-EXT
           END-IF
      *
      *収納請求額のチェックを行う
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *                                 
           IF      FLG-SYUNOU          =   1
                   CONTINUE
           ELSE
      *収納請求額がゼロの時はエラー
             IF    SYU-SKYMONEY        =  ZERO
                   MOVE    "0006"     TO    SPA-ERRCD
                   GO    TO    460-NYUKIN-EXT
             END-IF
      *収納請求額が収納入金額より少なければエラー
             IF    SYU-SKYMONEY        <= SYU-NYUKIN-TOTAL
                   MOVE    "0006"     TO    SPA-ERRCD
                   GO    TO    460-NYUKIN-EXT
             END-IF
           END-IF
      *
      *****MOVE    SPA-GMN-THKNCOMBI(SPA-GMN-SELBAN) TO
      *****                            SPA-HKNCOMBIMEI
      *****MOVE    SPA-GMN-TSRYKAMEI(SPA-GMN-SELBAN) TO
      *****                            SPA-SRYKAMEI
      *****MOVE    SPA-GMN-TDENPNUM(SPA-GMN-SELBAN)  TO
      *****                            SPA-S01-DENPNUM
      *
           INITIALIZE                  SPA-S01KYOUTU
           MOVE    "05"                TO  SPA-S01-SYORINO
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    "S02"           TO  SPA-MOTOPG
      *
           IF  ( SPA-S01-NYUGAIKBN   =   "1" )
               MOVE    "S03N"          TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03N"           TO  MCP-WINDOW
           ELSE
               MOVE    "S03"           TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03    "        TO  MCP-WINDOW
           END-IF
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       460-NYUKIN-EXT.
           EXIT.
      *************************************************************
      *    一括入金処理
      *************************************************************
       460-IKKATU-NYUKIN-SEC           SECTION.
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
               MOVE    "1003"          TO  SPA-ERRCD
               GO  TO  460-IKKATU-NYUKIN-EXT
           END-IF
      *
           INITIALIZE                  SPA-S01KYOUTU
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
      *
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S06"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "S06    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       460-IKKATU-NYUKIN-EXT.
           EXIT.
      *
      *************************************************************
      *    入金取消
      *************************************************************
       470-NYUDEL-SEC                  SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
               MOVE    SPA-GMN-PTID        TO  SYU-PTID
               MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
               PERFORM 900-SYUNOU-KEY-SEL-SEC
               IF    ( FLG-SYUNOU      =   ZERO  )
                   IF ( SYU-NYUKIN-TOTAL   <=   ZERO  )
                       MOVE    "0002"  TO      SPA-ERRCD
                   END-IF
                   IF    ( SPA-ERRCD   =   SPACE )
                       MOVE    ZERO    TO  WRK-NYUKIN
                       PERFORM 900-SYUMEI-KEY2-SEL-SEC
      *
                       PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
                           IF    ( SPA-SYSYMD  =   SMEI-NYUHEN-YMD )
                               CONTINUE
                           ELSE
                               COMPUTE WRK-NYUKIN
                                   =   WRK-NYUKIN
                                   +   SMEI-NYUHEN-MONEY
                           END-IF
      *
                           PERFORM 900-SYUMEI-KEY2-FET-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_syumei"    TO  MCP-TABLE
                       MOVE    "key2"          TO  MCP-PATHNAME
                       PERFORM 910-DBCLOSECURSOR-SEC
      *
                       IF    ( WRK-NYUKIN      =   ZERO )
                           PERFORM 4701-NYUDEL-SEC
                       ELSE
                           MOVE    "2101"      TO  SPA-SIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       470-NYUDEL-EXT.
           EXIT.
      *
      *************************************************************
      *    入金取消
      *************************************************************
       4701-NYUDEL-SEC           SECTION.
      *
      *    収納マスタと収納明細の入金額を０円に変更する
      *
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *     
           IF      FLG-SYUNOU          =   1
                   CONTINUE
           ELSE
             IF ( SYU-NYUKIN-TOTAL   <=    ZERO    )
                  MOVE    "0002"     TO    SPA-ERRCD
             ELSE
                 IF    (  SYU-DENPJTIKBN   =       "3" )
                      CONTINUE
                 ELSE
                   IF    ( SYU-SKYMONEY    =   ZERO )
                       MOVE    SPACE TO    SYU-DENPJTIKBN
                   ELSE
                       MOVE    "2"   TO    SYU-DENPJTIKBN
                   END-IF
                 END-IF
                  MOVE    SYU-NYUKIN-TOTAL
                                     TO    WRK-SAGAKU
                  MOVE    ZERO       TO    SYU-NYUKIN-TOTAL
      ***         MOVE    ZERO       TO    SYU-NYUKIN-KAISU
      *
                  PERFORM 800-MCNDATE-SEC
                  MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                  MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                  MOVE    SYUNOU-REC          TO  MCPDATA-REC
                  MOVE    "tbl_syunou"        TO  MCP-TABLE
                  MOVE    "key"               TO  MCP-PATHNAME
                  PERFORM 910-DBUPDATE-SEC
                  PERFORM 900-SYUMEI-KEY7-SEL-SEC
                  COMPUTE IDX1 =   SMEI-DENPEDANUM +   1
      *
                  PERFORM    4701-SMEI-SAKUSEI-SEC
             END-IF
           END-IF
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       4701-NYUDEL-EXT.
           EXIT.
      *************************************************************
      *    入金取消処理（収納明細レコード作成）
      *************************************************************
       4701-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX1                 TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "2"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
           COMPUTE   SMEI-NYUHEN-MONEY  =  WRK-SAGAKU  *  -1
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "6"                 TO  SMEI-JOUTAIKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"              TO  SPA-ERRCD
           END-IF     
           .
       4701-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      *************************************************************
      *    履歴修正
      *************************************************************
       490-RIREKI-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      490-RIREKI-EXT
           END-IF
      *
      *****MOVE    SPA-GMN-THKNCOMBI(SPA-GMN-SELBAN) TO
      *****                            SPA-HKNCOMBIMEI
      *****MOVE    SPA-GMN-TSRYKAMEI(SPA-GMN-SELBAN) TO
      *****                            SPA-SRYKAMEI
      *****MOVE    SPA-GMN-TDENPNUM(SPA-GMN-SELBAN)  TO
      *****                            SPA-S01-DENPNUM
      *
           INITIALIZE                  SPA-S01KYOUTU
           MOVE    "08"                TO  SPA-S01-SYORINO
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    "S02"           TO  SPA-MOTOPG
      *
           IF    ( SPA-S01-NYUGAIKBN   =   "1" )
               MOVE    "S03N"          TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03N"           TO  MCP-WINDOW
           ELSE
               MOVE    "S03"           TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03    "        TO  MCP-WINDOW
           END-IF
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-RIREKI-EXT.
           EXIT.
      *
      *************************************************************
      *    変更確定　処理
      *************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      490-KAKUTEI-EXT
           END-IF
      *
      *****MOVE    SPA-GMN-THKNCOMBI(SPA-GMN-SELBAN) TO
      *****                            SPA-HKNCOMBIMEI
      *****MOVE    SPA-GMN-TSRYKAMEI(SPA-GMN-SELBAN) TO
      *****                            SPA-SRYKAMEI
           MOVE    SPA-GMN-TDENPNUM(SPA-GMN-SELBAN)  TO
                                       SPA-S01-DENPNUM
      *
           INITIALIZE                  SPA-S01KYOUTU
           MOVE    "09"                TO  SPA-S01-SYORINO
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    "S02"           TO  SPA-MOTOPG
      *
           IF    ( SPA-S01-NYUGAIKBN   =   "1" )
               MOVE    "S03N"          TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03N"           TO  MCP-WINDOW
           ELSE
               MOVE    "S03"           TO  SPA-SAKIPG
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               MOVE   "S03    "        TO  MCP-WINDOW
           END-IF
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       収納共通ＳＰＡ編集処理
      *****************************************************************
       4901-SPAKYOTUKEY-SET-SEC               SECTION.
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-S01-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-S01-PTID
           MOVE    SPA-GMN-KANANAME    TO  SPA-S01-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-S01-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-S01-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-S01-BIRTHDAYW
           MOVE    SPA-NAI-NYUKIN-HOHO TO  SPA-S01-NYUKIN-HOHO
           MOVE    SPA-NAI-SKYPRT      TO  SPA-S01-SKYPRT
           MOVE    SPA-NAI-LOCK        TO  SPA-S01-LOCK
           MOVE    SPA-NAI-SLOCK-MSG   TO  SPA-S01-SLOCK-MSG
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-S01-BIRTHDAY
           IF    ( SPA-GMN-SELBAN      >   ZERO )
               MOVE    SPA-NAI-TSRYKA     (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYKA
               MOVE    SPA-NAI-TSRYKAMEI  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYKAMEI
               MOVE    SPA-GMN-THKNCOMBI  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-HKNCOMBIMEI
               MOVE    SPA-GMN-TFTNRATE   (SPA-GMN-SELBAN)
                                       TO  SPA-S01-FTNRATEMEI
               MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SPA-S01-DENPNUM
               MOVE    SPA-NAI-THAKYMD    (SPA-GMN-SELBAN)
                                       TO  SPA-S01-DENPPRTYMD
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SPA-S01-NYUGAIKBN
               MOVE    SPA-NAI-TSRYYMD    (SPA-GMN-SELBAN)
                                       TO  SPA-S01-SRYYMD
               MOVE    SPA-NAI-THKNNUM  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-HKNNUM
               MOVE    SPA-NAI-TRSI-HKNKBN  (SPA-GMN-SELBAN)
                                       TO  SPA-S01-RSI-HKNKBN
           END-IF
      *
           MOVE    SPA-GMN-SRYYM       TO  SPA-S01-SRYYMW
           MOVE    SPA-NAI-SRYYM       TO  SPA-S01-SRYYM
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-S01-HNYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  SPA-S01-HSRYKA
           MOVE    SPA-GMN-TBL-SEL     TO  SPA-S01-SYULISTSEL
           MOVE    SPA-GMN-GAI-MISYU   TO  SPA-S01-GAI-MISYU
           MOVE    SPA-GMN-NYU-MISYU   TO  SPA-S01-NYU-MISYU
           MOVE    SPA-GMN-PAGE        TO  SPA-S01-PAGE
           MOVE    SPA-GMN-MODE        TO  SPA-S01-MODE
           MOVE    SPA-GMN-SORTKBN     TO  SPA-S01-SORTKBN
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
          .
       4901-SPAKYOTUKEY-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    前月表示処理
      **************************************************************
       490-ZENGETU-SEC                 SECTION.
      *
           IF    ( SPA-NAI-SRYYM       =   SPACE )
               IF    ( SPA-GMN-PAGE    >   ZERO )
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    -   1
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
               END-IF
           ELSE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
      *                前年の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:4)
                       MOVE    "0101"          TO  LNK-DAY6-KIJUN (5:)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -12            TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:4)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:3)
                                           TO  SPA-GMN-SRYYM
               WHEN  ( SPA-GMN-MODE        =   CONST-MODE-NYUHEN )
                 AND ( SPA-NAI-SRYYM (1:8)     IS  NUMERIC )
      *                前日の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -1             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-SRYYM
               WHEN  OTHER
      *                前月の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
                       MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     -1             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:6)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-SRYYM
               END-EVALUATE
      *
                   PERFORM 3101-COMBO-EDIT-SEC
                   MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
                   PERFORM 4200-SRYKALST-CHK-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
           END-IF
      *
           .
       490-ZENGETU-EXT.
           EXIT.
      **************************************************************
      *    次月表示処理
      **************************************************************
       490-JIGETU-SEC                 SECTION.
      *
           IF    ( SPA-NAI-SRYYM      =   SPACE )
               IF    ( SPA-GMN-NEXT   =   1 )
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    +   1
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
               END-IF
           ELSE
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-SRYYM (5:)  =   SPACE )
      *                次年の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:4)
                       MOVE    "0101"          TO  LNK-DAY6-KIJUN (5:)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     12             TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:4)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:3)
                                           TO  SPA-GMN-SRYYM
               WHEN  ( SPA-GMN-MODE        =   CONST-MODE-NYUHEN )
                 AND ( SPA-NAI-SRYYM (1:8)     IS  NUMERIC )
      *                次日の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     1              TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-SRYYM
               WHEN  OTHER
      *                次月の月算出
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    SPA-NAI-SRYYM   TO  LNK-DAY6-KIJUN (1:6)
                       MOVE    "01"            TO  LNK-DAY6-KIJUN (7:2)
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     1              TO  LNK-DAY6-ZOGEN 
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN (1:6)
                                               TO  SPA-NAI-SRYYM
                       MOVE    LNK-DAY6-KEISAN TO  WRK-SYMD
                       PERFORM 801-HIZUKE-CHK-SEC
                       MOVE    WRK-HENYMDG (1:6)
                                               TO  SPA-GMN-SRYYM
               END-EVALUATE
      *
                   PERFORM 3101-COMBO-EDIT-SEC
                   MOVE    SPA-GMN-SRYKA       TO  WRK-CMB-CD
                   PERFORM 4200-SRYKALST-CHK-SEC
                   MOVE    WRK-CMB-ITM         TO  SPA-GMN-SRYKA-G
      *
                   MOVE    1           TO  FLG-SYULIST-ROW
                   PERFORM 300-SYUNOU-HENSYU-SEC
      *
                   INITIALIZE              SPA-GMN-INPUT
                                           SPA-GMN-TBL-SEL
           END-IF
      *
           .
       490-JIGETU-EXT.
           EXIT.
      **************************************************************
      *    共通パラメタ更新・画面遷移処理
      **************************************************************
       4901-PARA-HEN-SEC                 SECTION.
      *
      *
           .
       4901-PARA-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    診療科コンボボックスチェック処理
      **************************************************************
       4200-SRYKALST-CHK-SEC             SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SRYKA-MAX )
      *
               IF    ( SPA-GMN-SRYKAL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
          IF    (  WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-SRYKALST (1)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (1)
                                   TO  WRK-CMB-ITM2
          END-IF
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      **************************************************************
      *    診療科コンボボックスチェック処理
      **************************************************************
       4201-SRYKALST-CHK-SEC             SECTION.
      *
           INITIALIZE                    WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SRYKA-MAX )
      *
               IF    ( SPA-GMN-SRYKAL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-SRYKALST (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4201-SRYKALST-CHK-EXT.
           EXIT.
      **************************************************************
      *    入外区分コンボボックスチェック処理
      **************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NYUGAIKBN-MAX )
      *
               IF    ( SPA-GMN-NYUGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      **************************************************************
      *    モードコンボボックスチェック処理
      **************************************************************
       4200-MODELST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-MODE-MAX )
      *
               IF    ( SPA-GMN-MODEL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-MODELST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-MODELST (1)
                                   TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-MODELST-CHK-EXT.
           EXIT.
      **************************************************************
      *    並び順コンボボックスチェック処理
      **************************************************************
       4200-SORTKBNLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-SORTKBN-MAX )
      *
               IF    ( SPA-GMN-SORTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
                   MOVE    SPA-GMN-SORTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-NAI-SYS1039TBL  TO  SYS-1039-TBL
               IF    ( SYS-1039-S02SORTKBN =   "1" )
                   MOVE    SPA-GMN-SORTKBNLST (2)
                                   TO  WRK-CMB-ITM
               ELSE
                   MOVE    SPA-GMN-SORTKBNLST (1)
                                   TO  WRK-CMB-ITM
               END-IF
           END-IF
      *
           .
       4200-SORTKBNLST-CHK-EXT.
           EXIT.
      **************************************************************
      *    収納リスト選択状態保持
      **************************************************************
       4200-SAVE-SYULST-SEL-SEC        SECTION.
      *
           INITIALIZE                  WRK-ORG-NYUGAIKBN
                                       WRK-ORG-DENPNUM
           IF    ( SPA-GMN-TBL-SEL >   ZERO )
               MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-TBL-SEL)
                                   TO  WRK-ORG-NYUGAIKBN
               MOVE    SPA-GMN-TDENPNUM (SPA-GMN-TBL-SEL)
                                   TO  WRK-ORG-DENPNUM
           END-IF
      *
           .
       4200-SAVE-SYULST-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納リスト選択状態回復
      **************************************************************
       4200-LOAD-SYULST-SEL-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT
                                       SPA-GMN-TBL-SEL
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-TBL-SEL >   ZERO )
      *
               IF    ( WRK-ORG-NYUGAIKBN
                                   =   SPA-NAI-TNYUGAIKBN (IDX-LST))
                AND  ( WRK-ORG-DENPNUM
                                   =   SPA-GMN-TDENPNUM   (IDX-LST))
      *
                   MOVE    IDX-LST     TO  SPA-GMN-TBL-SEL
                                           SPA-GMN-SELBAN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-LOAD-SYULST-SEL-EXT.
           EXIT.
      **************************************************************
      *    保険組合せ名称編集処理
      **************************************************************
       510-HKNKUMI-MEI-SEC             SECTION.
      *
      *    対象の保険組み合わせが存在すればその名称を、
      *    なければ保険組み合わせ番号を表示する
      *
           MOVE    SPACE           TO  WRK-HKNKUMI
                                       WRK-HKNNUM
                                       WRK-RSI-HKNKBN
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( FLG-CHK =   1   )
                   OR    ( IDX-COMB    >   100 )
               IF    ( WRK-KUMINO  =   SPA-NAI-HKNCOMBIL (IDX-COMB))
                   MOVE    SPA-NAI-HKNCOMBIMEIL (IDX-COMB)
                                       TO  WRK-HKNKUMI
                   MOVE    SPA-NAI-HKNNUML (IDX-COMB)
                                       TO  WRK-HKNNUM
                   MOVE    SPA-NAI-RSI-HKNKBNL (IDX-COMB)
                                       TO  WRK-RSI-HKNKBN
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK         =   ZERO )
               MOVE    WRK-KUMINO  TO  WRK-HKNKUMI
           END-IF
      *
           .
       510-HKNKUMI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア画面へ
      *****************************************************************
       420-CLEAR-SEC        SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE              TO   WRK-NAI-SRYYM
      *
           IF    ( SPA-GMN-TBL-SEL    NOT  =   ZERO )
               MOVE    SPA-NAI-TSRYYMD(SPA-GMN-SELBAN) (1:6)
                                      TO   WRK-NAI-SRYYM
           ELSE
               IF    ( SPA-GMN-MODE   =    ZERO )
                   MOVE    SPA-NAI-SRYYM
                                      TO   WRK-NAI-SRYYM
               ELSE
                   MOVE    SPA-NAI-THSRYYM
                                      TO   WRK-NAI-SRYYM
               END-IF
               IF    ( WRK-NAI-SRYYM (1:6) IS   NUMERIC )
                   CONTINUE
               ELSE
                   MOVE    SPA-SYSYMD (1:6)
                                      TO   WRK-NAI-SRYYM
               END-IF
           END-IF
      *
           PERFORM 311-SPA-SET-SEC
      *
           MOVE    WRK-NAI-SRYYM       TO  WRK-SYMD
           PERFORM 801-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-SRYYM
           MOVE    WRK-SYMD    (1:6)   TO  SPA-NAI-SRYYM
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC               SECTION.
      *
           IF    ( SPA-LAST-PTNUM      =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-LAST-PTNUM  TO  S02-PTNUM
               PERFORM 4102-PTNUM-CHK-SEC
           END-IF
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    画面モード変更
      *****************************************************************
       470-MODE-CHENGE-SEC             SECTION.
      *
           MOVE    SPACE                   TO  S02-MODE
           EVALUATE    SPA-GMN-MODE
           WHEN    CONST-MODE-NORMON
           WHEN    CONST-MODE-NORYEAR
           WHEN    CONST-MODE-NORALL
               MOVE    "3"                 TO  S02-MODE
           WHEN    CONST-MODE-MISYU
               MOVE    "4"                 TO  S02-MODE
           WHEN    OTHER
               MOVE    SPA-NAI-THSRYYM     TO  SPA-NAI-SRYYM
               MOVE    "0"                 TO  S02-MODE
           END-EVALUATE
      *
           PERFORM 4106-MODE-CHK-SEC
      *
           .
       470-MODE-CHENGE-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-SIMEIKEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-SIMEIKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-S02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *----(01.01.19)--ADD-START---
      *****************************************************************
      *    支払証明書
      *****************************************************************
       500-SHIHARAI-SEC            SECTION.
      *
           IF       SPA-GMN-PTID   =   ZERO
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  500-SHIHARAI-EXT
           END-IF
      *
           INITIALIZE                  SPA-S01KYOUTU
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
      *
           PERFORM  41011-S04-SET-SEC
      *
      *     MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "S04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE.
           MOVE    "S04    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                    TO  FLG-END
      *
           .
       500-SHIHARAI-EXT.
           EXIT.
      *****************************************************************
      *    支払証明書印刷画面編集処理
      *****************************************************************
       41011-S04-SET-SEC               SECTION.
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-S02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGS04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-S01KYOUTU
           MOVE    SPA-FREE            TO  SPA-S02
      *
           .
       41011-S04-SET-EXT.
           EXIT.
      *
      *----(01.01.19)--ADD-END-----
      **************************************************************
      *    エラーメッセージ表示理
      **************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "入金取消が可能なデータではありません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "請求取消が可能なデータではありません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "返金はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "入金はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "履歴修正はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "Ｊデビットでの入金はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0009" 
               MOVE    "診療月以前のデータなし"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "請求取消し済みの為、再発行はできません"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "前月データはありません"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "次月データはありません"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "対象の診療年月に収納はありません"
                                       TO  SPA-ERRMSG
           WHEN    "0030"
               MOVE    "診療科目入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0031"
               MOVE    "該当の患者が存在しません。"
                                               TO  SPA-ERRMSG
           WHEN    "0033"
               MOVE    "診療年月入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0034"
               MOVE    "入外区分入力エラー"
                                               TO  SPA-ERRMSG
           WHEN    "0050"
               MOVE "患者番号・診療年月の入力がありません" 
                                               TO  SPA-ERRMSG
      *
           WHEN    "0060"
               MOVE "該当収納データが見つかりません" 
                                               TO  SPA-ERRMSG
           WHEN    "0061"
               MOVE "請求取消済みの為、再計算はできません"
                                               TO  SPA-ERRMSG
           WHEN    "0062"
               MOVE "再計算できません。診療行為で行って下さい"
                                               TO  SPA-ERRMSG
           WHEN    "0063"
               MOVE "受診履歴が見つかりません"
                                               TO  SPA-ERRMSG
           WHEN    "0064"
               MOVE "再計算できないデータがあります。"
                                               TO  SPA-ERRMSG
      *
           WHEN    "1001"
               MOVE    "収納明細の更新処理でエラーになりました"
                                               TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "収納の更新処理でエラーになりました"
                                               TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "患者が選択されていません"
                                               TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE "患者定期請求情報の削除処理でエラーになりました"
                                               TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE "定期請求情報の更新処理でエラーになりました"
                                               TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                               TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
           WHEN    "4000"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                               TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
               MOVE    SLOCK-MSG               TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE                TO  SERR
           MOVE    SPA-ERRCD            TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  SERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SERR"               TO  MCP-WINDOW
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
       520-SIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SIDMSG
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2001"
               MOVE    "請求データを取り消します"
                                        TO  WRK-SIDMSG
           WHEN    "2002"
               MOVE    "外来収納の一括再計算を行います"
                                        TO  WRK-SIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  SID1
           MOVE    SPA-SIDCD            TO  SID1-ID1CODE
           MOVE    WRK-SIDMSG           TO  SID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SID2SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  SID2
      *
           EVALUATE    SPA-SIDCD
           WHEN    "2101"
               MOVE   "別の診療日に入金記録があります。よろしいですか"
                                        TO  SID2-ID1MSG
               MOVE   "F11:請求確認画面"
                                        TO  SID2-B11-LABEL
               MOVE   "F12:確定"        TO  SID2-B12-LABEL
           END-EVALUATE
           MOVE    SPA-SIDCD            TO  SID2-ID1CODE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SID2SET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-SID3SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  SID3
      *
           EVALUATE     SPA-SIDCD
           WHEN    "2102"
      *
               EVALUATE SPA-NAI-SKYPRT
               WHEN    "0"
                   MOVE    "【請求書・明細書不要】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "1"
                   MOVE    "【請求書・明細書必要】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "2"
                   MOVE    "【請求書・明細書必要（請求あり）】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "3"
                   MOVE    "【請求書・明細書必要（訂正時なし）】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "4"
                   MOVE    "【請求書必要（明細書不要）】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "5"
                   MOVE    "【請求書必要（請求あり）（明細書不要）】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "6"
                   MOVE    "【請求書必要（訂正時なし）（明細書不要）】"
                                   TO  SID3-LBLID1SKYPRT
               WHEN    "7"
                   MOVE    "【請求書必要（請求あり）（明細書必要）】"
                                   TO  SID3-LBLID1SKYPRT
               END-EVALUATE
      *
               STRING "再発行を行います"
                                            DELIMITED BY SIZE
               INTO   SID3-ID1MSG
               END-STRING
           END-EVALUATE
           MOVE    SPA-SIDCD            TO  SID3-ID1CODE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "S02"                TO  SPA-MOTOPG
           MOVE    "SID3"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "SID3"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-SID3SET-EXT.
           EXIT.
      **************************************************************
      *    西暦和暦編集処理
      **************************************************************
       800-HIZUKE-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG2
      *
           IF    ( WRK-SYMD            =   ALL "9"  OR   SPACE )
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
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG2
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       801-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
           MOVE    ZERO                TO  FLG-HIZUKE-RC
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "2"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD            TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF  ( SGDAY-RC              =   ZERO )
      *
                   MOVE    SGDAY-SDAY      TO  WRK-SYMD
                   EVALUATE    TRUE
                   WHEN  ( WRK-SYMD (5:)   =   SPACE )
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    "0101"      TO  WRK-SYMD  (5:)
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG (1:3)
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2(1:10)
                       MOVE    SPACE       TO  WRK-SYMD  (5:)
                   WHEN  ( WRK-SYMD (7:)   =   SPACE )
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    "01"        TO  WRK-SYMD  (7:)
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG (1:6)
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2(1:16)
                       MOVE    SPACE       TO  WRK-SYMD  (7:)
                   WHEN    OTHER
                       INITIALIZE              STS-AREA-DAY
                       INITIALIZE              LNK-DAY2-AREA
                       MOVE    "21"        TO  LNK-DAY2-IRAI
                       MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"       USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                       MOVE    LNK-DAY2-EDTYMD1
                                           TO  WRK-HENYMDG
                       MOVE    LNK-DAY2-EDTYMD3
                                           TO  WRK-HENYMDG2
                   END-EVALUATE
               ELSE
                   MOVE    1               TO  FLG-HIZUKE-RC
               END-IF
           END-IF
      *
           .
       801-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC                SECTION.
      *
           CALL    "ORCSDAILYKEY"      USING
                                       SDAILYKEYAREA
                                       SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
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
      *
      *****************************************************************
      *    収納更新差額取得サブ
      *****************************************************************
       800-SS002-SEC           SECTION.
      *
           INITIALIZE                      SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           .
      *
       800-SS002-EXT.
           EXIT.
      *****************************************************************
      *    収納明細更新差額設定処理
      *****************************************************************
       800-SET-SS002-SMEI-SEC      SECTION.
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
           .
      *
       800-SET-SS002-SMEI-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理（一括再計算）
      **************************************************************
       900-SYUNOU-SEL-IKT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-IKT-EXT.
           EXIT.
      **************************************************************
      *    収納マスター読込（主キー）
      **************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY27-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY73-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY73-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key73"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY73-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY53-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key53"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key53"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY53-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY54-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key54"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key54"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY54-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY55-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           IF    ( SPA-S01-TUKI-KIKANKBN   =   "1" )
               MOVE    SPA-S01-TUKI-STYMD
                                   TO  SYU-SRYYMD
           ELSE
               MOVE    WRK-SKYYM   TO  SYU-SRYYMD
               MOVE    "01"        TO  SYU-SRYYMD(7:2)
           END-IF
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key55"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY55-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY56-SEL-SEC        SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           STRING  WRK-SKYYM           DELIMITED   BY  SPACE
                   "01"                DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key56"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY56-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納検索処理
      **************************************************************
       900-SYUNOU-KEY61-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
           MOVE    "%"             TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  SYU-PTID
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key61"        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           .
       900-SYUNOU-KEY61-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納FETCH処理
      **************************************************************
       900-SYUNOU-KEY61-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"   TO  MCP-TABLE
           MOVE    "key61"        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY61-FET-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY7-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細検索処理
      **************************************************************
       900-SYUMEI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM   (SPA-GMN-SELBAN)
                                       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納明細FETCH処理
      **************************************************************
       900-SYUMEI-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-FET-EXT.
           EXIT.
      **************************************************************
      *    患者情報検索処理(KEY)
      **************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
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
      *    システム管理検索処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY21)
      *****************************************************************
       900-SYSKANRI-KEY21-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key21"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY21-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
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
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
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
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    C-UID               TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
       910-DBDELETE-SEC                SECTION.
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
      *    ＤＢインサート処理
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
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
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
      **************************************************************
      *    ＰＵＴ　処理
      **************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
