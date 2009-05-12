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
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-CUR                     PIC 9(02).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYKALST-G.
                       09  SPA-GMN-SRYKALST        OCCURS  99.
                          11  SPA-GMN-SRYKAL       PIC X(02).
                          11  SPA-GMN-F1L          PIC X(01).
                          11  SPA-GMN-SRYKAMEIL    PIC X(16).
                   07  SPA-GMN-SRYKA-MAX           PIC 9(02).
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBNLST    OCCURS  3.
                           11  SPA-GMN-NYUGAIKBNL  PIC X(01).
                           11  SPA-GMN-F1L         PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL
                                                   PIC X(06).
                   07  SPA-GMN-NYUGAIKBN-MAX       PIC 9(02).
                   07  SPA-GMN-SRYYM               PIC X(07).
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
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLREC  OCCURS   200.
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
                           11  SPA-GMN-FTNHENFLG   PIC 9(02).
                           11  SPA-GMN-SIKIBETUFLG PIC X(01).
                       09  SPA-GMN-TBL-SEL         PIC 9(05).
      *
                   07  SPA-GMN-INPUT.
                       09  SPA-GMN-SELBAN          PIC 9(04).
                   07  SPA-GMN-MISYU.
                       09  SPA-GMN-GAI-MISYU       PIC S9(08).
                       09  SPA-GMN-NYU-MISYU       PIC S9(08).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM               PIC X(06).
                   07  SPA-NAI-HEAD.
                       09  SPA-NAI-LOCK            PIC 9(01).
                       09  SPA-NAI-BIRTHDAY        PIC X(08).
                       09  SPA-NAI-NYUKIN-HOHO     PIC X(02).
                       09  SPA-NAI-HKNCOMBI-G.
                           11  SPA-NAI-HKNCOMBI-MAX        PIC 9(05).
                           11  SPA-NAI-HKNCOMBILST         OCCURS  100.
                               13  SPA-NAI-HKNCOMBIL       PIC X(04).
                               13  SPA-NAI-HKNCOMBIMEIL    PIC X(50).
                               13  SPA-NAI-HKNNUML         PIC X(03).
                               13  SPA-NAI-RSI-HKNKBNL     PIC X(01).
      *
                   07  SPA-NAI-SRYKALST-G.
                       09  SPA-NAI-SRYKALST        OCCURS  99.
                          11  SPA-NAI-SRYKAMEIL    PIC X(06).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLREC  OCCURS  200.
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX7                PIC 9(05).
           03  IDX9                PIC 9(05).
           03  IDX10               PIC 9(05).
           03  IDX11               PIC 9(05).
           03  IDX12               PIC 9(05).
           03  IDX-LST             PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-PRTKANRI-RENNUM PIC 9(05).
           03  CNT-JOBID           PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD           PIC X(04).
           03  WRK-SIDMSG          PIC X(70).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
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
      *
           03  WRK-SYORINO         PIC X(02).
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
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-PTNUM           PIC X(20).
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
               05  WRK-DENPNUM-END PIC 9(07)   OCCURS  15.
      *
           03  WRK-FTNHENFLG1      PIC 9(01).
           03  WRK-FTNHENFLG2      PIC 9(01).
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
      *----(03.04.01)--ADD-START---
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *----(03.04.01)--ADD-END-----
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
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    帳票サブ < 請求書兼領収書 >
           COPY    "CPORCHCN03.INC".
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
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
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
      *----(01.01.19)--ADD-START---
       01  SCRAREA.
      *****COPY    "S01.INC".
           COPY    "S02.INC".
           COPY    "S03.INC".
           COPY    "S03N.INC".
           COPY    "S04.INC".
           COPY    "S05.INC".
           COPY    "S06.INC".
           COPY    "S99.INC".
           COPY    "SERR.INC".
           COPY    "SID1.INC".
           COPY    "SID2.INC".
      *----(01.01.19)--ADD-END-----
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
           IF      SPA-MOTOPG      =   "SERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( WRK-ERRCD   NOT =   SPACE )
                   MOVE    WRK-ERRCD   TO  SPA-ERRCD
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        元画面表示
      *
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"
                                                      OR  "P97"
                                                      OR  "XA01"
                                                      OR  "XB01"
      *----(03.04.01)--ADD-START---
                                                      OR  "XC01"
      *----(03.04.01)--ADD-START---
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-S02
                   MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG          =   "Y01"
                                           OR  "XA01"
                                           OR  "XB01"
      *----(03.04.01)--ADD-START---
                                           OR  "XC01"
      *----(03.04.01)--ADD-START---
                   GO      TO      300-SCREEN-EXT
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
      *
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "K02"
               WHEN    "K02N"
               WHEN    "U02"
               WHEN    "I01"
      *            他の業務より遷移時の初期処理
                   PERFORM 320-SENIJI-SYOGMN-EDIT-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "S03"
               WHEN    "S03N"
               WHEN    "S06"
                   PERFORM 320-S03SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "S04"
                   PERFORM 320-S03SPASET-SEC
      *----(03.04.01)--UPD-START---
                   IF    ( SPA-S01-SHR-ORDER  NOT =   SPACE )
                       PERFORM 340-SHIHARAI-PRT-SEC
                   ELSE
                       MOVE    SPACE           TO  SPA-S01-SHR-ORDER
                   END-IF
                   MOVE    SPACE           TO  SPA-S99-FLG
                   MOVE    SPACE           TO  SPA-SIDCD
      *----(03.04.01)--UPD-END-----
                   GO  TO  300-SCREEN-EXT
               WHEN    "S05"
                   PERFORM 320-S05SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "SID1"
                   PERFORM 330-SID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "S99"
      *----(03.04.01)--ADD-START---
                   PERFORM 340-S99-SET-SEC
      *----(03.04.01)--ADD-END-----
                   GO  TO  300-SCREEN-EXT
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
           IF      SPA-SID1-FLG    =   "OK"
               EVALUATE    SPA-SIDCD
               WHEN    "2001"
                   PERFORM 430-TORIKESI-KOUSHIN-SEC
                   MOVE    SPA-ERRCD
                                   TO  WRK-ERRCD
               WHEN    "2002"
                   PERFORM 440-IKATUSAIKEI-SEC
                   MOVE    SPA-ERRCD
                                   TO  WRK-ERRCD
               WHEN    "2003"
                   PERFORM 420-SAIHAKKOU-SEC
                   MOVE    SPA-ERRCD
                                   TO  WRK-ERRCD
               END-EVALUATE
      *
           END-IF
      *
      *----(03.04.01)--ADD-START---
           MOVE    SPACE           TO  SPA-S99-FLG
      *----(03.04.01)--ADD-END-----
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
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
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  S02-PTNUM
                   PERFORM 4102-PTNUM-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  3101-P97SPASET-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボ初期値設定処理
      *****************************************************************
       3101-COMBO-SYOKI-SET-SEC        SECTION.
      *
           MOVE    1               TO  FLG-CHK
           PERFORM VARYING IDX7   FROM    1   BY  1
               UNTIL     ( IDX7   >       SPA-GMN-SRYKA-MAX )
               IF    ( SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX7) )
                   MOVE    SPA-GMN-SRYKALST (IDX7)
                                       TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-GMN-SRYKA-MAX
                                       TO  IDX7
                   MOVE    ZERO        TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK     NOT =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-SRYKA-G
           END-IF
      *
           MOVE    1               TO  FLG-CHK
           PERFORM VARYING IDX7   FROM    1   BY  1
               UNTIL     ( IDX7   >       SPA-GMN-NYUGAIKBN-MAX )
               IF    ( SPA-GMN-NYUGAIKBN
                                   =   SPA-GMN-NYUGAIKBNL (IDX7) )
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX7)
                                       TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  IDX7
                   MOVE    ZERO        TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF    ( FLG-CHK     NOT =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           .
       3101-COMBO-SYOKI-SET-EXT.
           EXIT.
      **************************************************************
      *    画面編集処理
      **************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                   SPA-S02
                                        SPA-S01KYOUTU
      *
           MOVE    SPA-SYSYMDWH(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-SYSYMD  (1:6)    TO  SPA-NAI-SRYYM
           MOVE    SPA-SYSYMD           TO  SPA-SRYYMD
           MOVE    SPA-SRYYMD           TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG          TO  SPA-SRYYMDW
      *
           PERFORM 3101-SRYKA-SPA-SEC
      *
           PERFORM 3102-NYUGAIKBN-SPA-SEC
      *
           MOVE    SPA-GMN-SRYKALST (1)
                                   TO  SPA-GMN-SRYKA-G
      *
      *    入外区分
           IF    ( SPA-BEDSU       >   ZERO )
               MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                   TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                   TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科ＳＰＡ画面編集処理
      *****************************************************************
       3101-SRYKA-SPA-SEC              SECTION.
      *
           INITIALIZE  SPA-GMN-SRYKALST-G
                       SPA-NAI-SRYKALST-G
      *
      *    “全科”を設定
           MOVE    CONST-ZENSRYKA-G    TO  SPA-GMN-SRYKALST (1)
           MOVE    1                   TO  SPA-GMN-SRYKA-MAX
      *
           INITIALIZE  SYS-1005-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    "01"                TO  SYS-STYUKYMD (7 : 2)
                                           SYS-EDYUKYMD (7 : 2)
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           PERFORM VARYING     IDX1   FROM    2    BY  1
                   UNTIL     ( IDX1           >    99   )
                   OR        ( FLG-SYSKANRI   =    1    )
      *
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                             SPA-GMN-SRYKAL    (IDX1)
               MOVE    SYS-1005-SRYKANAME1   TO
                                             SPA-GMN-SRYKAMEIL (IDX1)
               MOVE    SYS-1005-SRYKANAME2   TO
                                             SPA-NAI-SRYKAMEIL (IDX1)
               MOVE    IDX1                  TO
                                             SPA-GMN-SRYKA-MAX
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1005-REC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SRYKA-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分ＳＰＡ画面編集処理
      *****************************************************************
       3102-NYUGAIKBN-SPA-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAIKBNLST-G
      *
           MOVE    "0"                 TO  SPA-GMN-NYUGAIKBNL       (1)
           MOVE    "全部"              TO  SPA-GMN-NYUGAIKBNMEIL    (1)
           MOVE    "1"                 TO  SPA-GMN-NYUGAIKBNL       (2)
           MOVE    "入院"              TO  SPA-GMN-NYUGAIKBNMEIL    (2)
           MOVE    "2"                 TO  SPA-GMN-NYUGAIKBNL       (3)
           MOVE    "外来"              TO  SPA-GMN-NYUGAIKBNMEIL    (3)
           MOVE    3                   TO  SPA-GMN-NYUGAIKBN-MAX
      *
           .
       3102-NYUGAIKBN-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    他の業務より遷移時初期処理
      *****************************************************************
       320-SENIJI-SYOGMN-EDIT-SEC      SECTION.
      *
           INITIALIZE                  SPA-S02
                                       SPA-S01KYOUTU
      *
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
           ELSE
               MOVE    SPA-SRYYMD          TO  WRK-SYMD
               PERFORM 31011-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
               MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
           END-IF
      *
           PERFORM 3101-SRYKA-SPA-SEC
      *
           PERFORM 3102-NYUGAIKBN-SPA-SEC
      *
           IF  (   SPA-PTNUM       NOT =  SPACE )
      *        患者情報を編集
               MOVE    SPA-PTNUM       TO  WRK-PTNUM
               PERFORM 320-PTINF-EDIT-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   IF    ( SPA-ERRCD   =   "9999" )
      *
      *                未収額を編集
                       PERFORM 3101-MISYU-EDIT-SEC
                   END-IF
                   GO  TO  320-SENIJI-SYOGMN-EDIT-EXT
               END-IF
               PERFORM 320-HKNCOMBI-EDIT-SEC
           END-IF
      *
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    入外区分
           IF    ( SPA-BEDSU       >   ZERO )
               MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
           END-IF
      *
           IF  (   SPA-PTNUM       NOT =  SPACE )
      *
      *        未収額を編集
               PERFORM 3101-MISYU-EDIT-SEC
      *
      *        該当月の収納データを表示
               PERFORM 300-SYUNOU-HENSYU-SEC
           END-IF
      *
           IF    ( SPA-GMN-PTNUM       NOT =   SPACE )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
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
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
                                       SPA-GMN-MAX
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    WRK-PTNUM           TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
      *
           MOVE    SPTNUM-PTID          TO  SPA-GMN-PTID
           MOVE    SPTNUM-PTNUM         TO  SPA-GMN-PTNUM
      *
           PERFORM 320-PTINF-SET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  320-PTINF-EDIT-EXT
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
           IF    ( FLG-PTINF   NOT     =   ZERO )
               MOVE    1               TO  FLG-PTINF
               MOVE    "0031"          TO  SPA-ERRCD
               GO  TO  320-PTINF-SET-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           EVALUATE    PTINF-SEX
               WHEN    "1" 
                   MOVE    "男"        TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"        TO  SPA-GMN-SEX
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
      *
           IF    ( PTINF-NYUKIN-HOHO   NOT =   SPACE )
               MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NAI-NYUKIN-HOHO
           ELSE
               MOVE    "01"                TO  SPA-NAI-NYUKIN-HOHO
           END-IF
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1               TO  SPA-NAI-LOCK
               GO  TO  320-PTINF-SET-EXT
           END-IF
      *
      *    前回患者
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
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
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
               GO  TO  320-HKNCOMBI-EDIT-EXT
           END-IF
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-NAI-HKNCOMBI-MAX
      *
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( IDX11   >   ORCSHKN-HKN-MAX )
                   OR    ( IDX11   >   100 )
      *
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX11)
                                   TO  SPA-NAI-HKNCOMBIL (IDX11)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX11)
                                   TO  SPA-NAI-HKNCOMBIMEIL (IDX11)
               MOVE    ORCSHKN-NAI-HKNNUM  (IDX11)
                                   TO  SPA-NAI-HKNNUML      (IDX11)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN  (IDX11)
                                   TO  SPA-NAI-RSI-HKNKBNL  (IDX11)
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
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-PTID        TO  SPA-GMN-PTID
           MOVE    SPA-S01-KANANAME    TO  SPA-GMN-KANANAME
           MOVE    SPA-S01-SEX         TO  SPA-GMN-SEX
           MOVE    SPA-S01-SRYYMW      TO  SPA-GMN-SRYYM
           MOVE    SPA-S01-SRYYM       TO  SPA-NAI-SRYYM
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
           MOVE    SPA-S01-LOCK        TO  SPA-NAI-LOCK
           MOVE    SPA-S01-BIRTHDAYW   TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-S01-BIRTHDAY    TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-S01-NYUKIN-HOHO TO  SPA-NAI-NYUKIN-HOHO
           MOVE    SPA-S01-HSRYKA      TO  SPA-GMN-SRYKA
           MOVE    SPA-S01-HNYUGAIKBN  TO  SPA-GMN-NYUGAIKBN 
      *
           PERFORM 320-HKNCOMBI-EDIT-SEC
      *
           PERFORM 3101-SRYKA-SPA-SEC
      *
           PERFORM 3102-NYUGAIKBN-SPA-SEC
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           MOVE    SPA-S01-SYULISTSEL  TO  SPA-GMN-TBL-SEL
                                           SPA-GMN-SELBAN
      *
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N" )
           AND   ( SPA-PTNUM       NOT =   SPACE )
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE        
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF    
      *----(01.01.19)--ADD-START---
           IF      SPA-MOTOPG          =   "S04"
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *----(01.01.19)--ADD-END-----
           .
       320-S03SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収検索戻り画面編集処理
      *****************************************************************
       320-S05SPASET-SEC          SECTION.
      *
           INITIALIZE                  SPA-S02
      *
           MOVE    SPA-S01-PTNUM       TO  SPA-GMN-PTNUM
           MOVE    SPA-S01-PTID        TO  SPA-GMN-PTID
           MOVE    SPA-S01-KANANAME    TO  SPA-GMN-KANANAME
           MOVE    SPA-S01-SEX         TO  SPA-GMN-SEX
           MOVE    SPA-S01-SRYYMW      TO  SPA-GMN-SRYYM
           MOVE    SPA-S01-SRYYM       TO  SPA-NAI-SRYYM
           MOVE    SPA-S01-NAME        TO  SPA-GMN-NAME
           MOVE    SPA-S01-LOCK        TO  SPA-NAI-LOCK
           MOVE    SPA-S01-BIRTHDAYW   TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-S01-BIRTHDAY    TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-S01-NYUKIN-HOHO TO  SPA-NAI-NYUKIN-HOHO
      *
      *
           PERFORM 320-HKNCOMBI-EDIT-SEC
      *
           PERFORM 3101-SRYKA-SPA-SEC
      *
           PERFORM 3102-NYUGAIKBN-SPA-SEC
      *
           IF    ( SPA-S01-DENPNUM     NOT =   ZERO )
               MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
      *        入外区分
               IF    ( SPA-BEDSU       >   ZERO )
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
               ELSE
                   IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
                       MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
                   END-IF
               END-IF
           ELSE
               MOVE    SPA-S01-HSRYKA  TO  SPA-GMN-SRYKA
               MOVE    SPA-S01-HNYUGAIKBN
                                       TO  SPA-GMN-NYUGAIKBN 
           END-IF
      *
           PERFORM 3101-COMBO-SYOKI-SET-SEC
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-SELBAN
                                           SPA-GMN-TBL-SEL
           IF    ( SPA-S01-DENPNUM     NOT =   ZERO )
               PERFORM VARYING IDX12   FROM    1   BY  1
                       UNTIL ( IDX12   >   SPA-GMN-MAX )
                   IF    ( SPA-GMN-TDENPNUM (IDX12)
                                       =   SPA-S01-DENPNUM )
                       MOVE    IDX12   TO  SPA-GMN-SELBAN
                                           SPA-GMN-TBL-SEL
                       MOVE    SPA-GMN-MAX
                                       TO  IDX12
                   END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-S01-SYULISTSEL
                                       TO  SPA-GMN-SELBAN
                                           SPA-GMN-TBL-SEL
           END-IF
      *
           IF    ( SPA-S01-DENPNUM     NOT =   ZERO )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    3               TO  SPA-GMN-CUR
           END-IF
           .
       320-S05SPASET-EXT.
           EXIT.
      *
      **************************************************************
      *    収納マスタ編集処理
      **************************************************************
       300-SYUNOU-HENSYU-SEC                  SECTION.
      *
           IF    ( SPA-NAI-LOCK        NOT =  ZERO )
               GO  TO  300-SYUNOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPA-GMN-TBL-SEL     TO  WRK-TBL-SEL-MOT
      *
           MOVE    1                   TO  FLG-SYULIST-ROW
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                      SPA-GMN-TBL
                                           SPA-NAI-TBL
      *
           MOVE   ZERO                 TO  IDX1
      *
      *    収納マスターを編集（入院分）
           IF    ( SPA-GMN-NYUGAIKBN   =   "0" OR  "1" )
               MOVE    "1"             TO  WRK-NYUGAIKBN
               PERFORM 3101-SYULIST-EDIT-SEC
           END-IF
      *
      *    収納マスターを編集（外来分）
           IF    ( SPA-GMN-NYUGAIKBN   =   "0" OR  "2" )
               MOVE    "2"             TO  WRK-NYUGAIKBN
               PERFORM 3101-SYULIST-EDIT-SEC
           END-IF
      *
           MOVE    IDX1                 TO  SPA-GMN-MAX
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
           MOVE    SPA-S01-SHR-SUMKBN
                                   TO  SHELLTBL-ARG26
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
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    ZERO            TO  FLG-SYORI
               IF    ( SPA-GMN-SRYKA       NOT =   SPACE )
                   IF    ( SPA-GMN-SRYKA       =   "00" )
                    OR   ( SPA-GMN-SRYKA       =   SYU-SRYKA)
                       CONTINUE
                   ELSE
                       MOVE    1       TO  FLG-SYORI
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-SYORI
               END-IF
      *
               IF    ( SPA-NAI-SRYYM  NOT  =   SPACE   )
                   IF   (SPA-NAI-SRYYM     =   SYU-SKYSTYMD(1:6))
                       CONTINUE
                   ELSE
                       MOVE    1       TO  FLG-SYORI
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-SYORI
               END-IF
      *
               IF      FLG-SYORI       =   ZERO
                   PERFORM 3101-SIN-HEN-SEC
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYUNOU-REC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
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
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key53"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key54"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
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
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
                   "01"                DELIMITED   BY  SIZE
           INTO    SYU-SRYYMD
           END-STRING
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
           COMPUTE WRK-MISYU-P
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           STRING  SPA-NAI-SRYYM       DELIMITED   BY  SPACE
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
      *   収納マスターより画面内容編集処理
      **************************************************************
       3101-SIN-HEN-SEC                SECTION.
      *
      *    収納マスタ内容編集
           COMPUTE IDX1    =   IDX1    +   1
      *
           MOVE    IDX1                TO  SPA-GMN-TBANGO   (IDX1)
      *
           MOVE    SYU-DENPNUM         TO  SPA-GMN-TDENPNUM (IDX1)
      *
           IF      SYU-GRP-DENPNUM     NOT =  ZERO
               MOVE   "M"              TO  SPA-GMN-DENPGRP (IDX1)
           END-IF
      *
      *    入外区分
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN  (IDX1)
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"            TO  SPA-GMN-TNYUGAIKBN  (IDX1)
           WHEN    "2"
               MOVE    "外"            TO  SPA-GMN-TNYUGAIKBN  (IDX1)
           END-EVALUATE
      *
           IF   SYU-DENPJTIKBN   NOT =  "3"
             EVALUATE    SYU-NYNSHOKAISU
               WHEN         1
                 MOVE  SYU-NYNSHOKAISU TO  SPA-GMN-FTNHENFLG  (IDX1)
                 MOVE  SYU-NYUGAIKBN   TO  SPA-GMN-SIKIBETUFLG(IDX1)
               WHEN         2
                 MOVE  SYU-NYNSHOKAISU TO  SPA-GMN-FTNHENFLG  (IDX1)
                 MOVE  SYU-NYUGAIKBN   TO  SPA-GMN-SIKIBETUFLG(IDX1)
                 MOVE    "○"      TO  SPA-GMN-TNYUGAIKBN(IDX1)(3:2)
             END-EVALUATE
           END-IF
      *
      *    診療科名
           PERFORM VARYING IDX9    FROM    1   BY  1
                   UNTIL ( IDX9        >   SPA-GMN-SRYKA-MAX )
      *
               IF    ( SYU-SRYKA       =   SPA-GMN-SRYKAL (IDX9))
                   MOVE    SPA-NAI-SRYKAMEIL (IDX9)
                                       TO  SPA-GMN-TSRYKA (IDX1)
                   MOVE    SPA-GMN-SRYKAL (IDX9)
                                       TO  SPA-NAI-TSRYKA (IDX1)
                   MOVE    SPA-GMN-SRYKAMEIL (IDX9)
                                       TO  SPA-NAI-TSRYKAMEI (IDX1)
                   MOVE    SPA-GMN-SRYKA-MAX
                                       TO  IDX9
               END-IF
      *
           END-PERFORM
      *
      *    診療日
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    SYU-SKYSTYMD    TO  SPA-NAI-TSRYYMD   (IDX1)
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 31011-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD   (IDX1)
               MOVE    SYU-SKYEDYMD    TO  SPA-NAI-TSKYEDYMD (IDX1)
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 31011-SEIWA-HEN-SEC
               MOVE    "-"             TO  SPA-GMN-TSRYYMD (IDX1)(10:1)
               MOVE    WRK-HENYMDG (5:5)
                                       TO  SPA-GMN-TSRYYMD (IDX1) (11:5)
           WHEN    "2" 
               MOVE    SYU-SRYYMD      TO  SPA-NAI-TSRYYMD (IDX1)
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 31011-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSRYYMD (IDX1)
           END-EVALUATE
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-THAKYMD (IDX1)
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 31011-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-THAKYMD (IDX1)
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SPA-GMN-TSEIKYU (IDX1)
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-TNYUKIN (IDX1)
      *    未収金額
           COMPUTE SPA-GMN-TMISYU(IDX1) =   SYU-SKYMONEY
                                        -   SYU-NYUKIN-TOTAL
      *
      *    保険組合せより（保険と負担）
           MOVE    SYU-HKNCOMBINUM     TO  WRK-KUMINO
           PERFORM 510-HKNKUMI-MEI-SEC
           MOVE    WRK-HKNKUMI         TO  SPA-GMN-THKNCOMBI (IDX1)
           MOVE    WRK-HKNNUM          TO  SPA-NAI-THKNNUM   (IDX1)
           MOVE    WRK-RSI-HKNKBN      TO  SPA-NAI-TRSI-HKNKBN(IDX1)
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-TFTNRATE (IDX1)
           ELSE
               MOVE    SPACE           TO  WK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WK-GMN-FTNRITU
               MOVE    "%"             TO  WK-GMN-PA
               MOVE    WK-GMN-RITU     TO  SPA-GMN-TFTNRATE (IDX1)
           END-IF
      *
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-TJYOTAIKBN (IDX1)
      *
      *    伝票状態区分
           EVALUATE   SYU-DENPJTIKBN   ALSO   SYU-CREATEKBN
           WHEN   SPACE                ALSO   SPACE
               MOVE  "請求額なし"      TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "1"                  ALSO   SPACE
               MOVE  "入金済"          TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "2"                  ALSO   SPACE
               MOVE  "未入金"          TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "3"                  ALSO   SPACE
               MOVE  "請求取消し"      TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "7"                  ALSO   SPACE
               MOVE  "継続"            TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   SPACE                ALSO   "1"
               MOVE  "請求額なし（退）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "1"                  ALSO   "1"
               MOVE  "入金済（退）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "2"                  ALSO   "1"
               MOVE  "未入金（退）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "3"                  ALSO   "1"
               MOVE  "請求取消し（退）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "7"                  ALSO   "1"
               MOVE  "継続（退）"      TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   SPACE                ALSO   "2"
               MOVE  "請求額なし（定）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "1"                  ALSO   "2"
               MOVE  "入金済（定）"    TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "2"                  ALSO   "2"
               MOVE  "未入金（定）"    TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "3"                  ALSO   "2"
               MOVE  "請求取消し（定）"
                                       TO  SPA-GMN-TJYOTAI (IDX1)
           WHEN   "7"                  ALSO   "2"
               MOVE  "継続（定）"      TO  SPA-GMN-TJYOTAI (IDX1)
           END-EVALUATE
      *
           .
       3101-SINKAI-SPA-EXT.
           EXIT.
      *
      **************************************************************
      *    西暦和暦編集処理
      **************************************************************
       31011-SEIWA-HEN-SEC             SECTION.
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
       31011-SEIWA-HEN-EXT.
           EXIT.
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
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           IF      SPA-SIDCD       NOT =   SPACE
               PERFORM 520-SIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
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
           MOVE    SPA-GMN-SRYYM       TO  S02-SRYYM
           MOVE    SPA-GMN-BIRTHDAY    TO  S02-BIRTHDAY
           MOVE    SPA-GMN-SRYKA-G     TO  S02-SRYKA
           MOVE    SPA-GMN-NYUGAIKBN-G TO  S02-NYUGAIKBN
      *     
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SRYKA-MAX )
                   OR    ( SPA-GMN-SRYKAL  (IDX1)   =   SPACE )
               MOVE    SPA-GMN-SRYKALST (IDX1)
                                       TO  S02-SRYKALST(IDX1)
               MOVE    IDX1            TO  S02-SRYKA-COUNT
           END-PERFORM
      *     
           PERFORM VARYING IDX1     FROM   1   BY  1
                   UNTIL ( IDX1     >      SPA-GMN-NYUGAIKBN-MAX )
                   OR    ( SPA-GMN-NYUGAIKBNL  (IDX1)  =   SPACE )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX1)
                                       TO  S02-NYUGAIKBN-ITEM (IDX1)
               MOVE    IDX1            TO  S02-NYUGAIKBN-COUNT
           END-PERFORM
      *
      *    暫定 ST
           MOVE    WIDGET-NORMAL       TO  S02-B04-STATE
           MOVE    WIDGET-INSENSITIVE  TO  S02-B03S-STATE
      *    暫定 ED
      *
           MOVE    ZERO                TO  WRK-FTNHENFLG1
                                           WRK-FTNHENFLG2
      *
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL     ( IDX1     >       SPA-GMN-MAX )
               MOVE    SPA-GMN-TBANGO (IDX1)   TO  WRK-TBANGO-W
               MOVE    WRK-TBANGO-W            TO  S02-TNUM       (IDX1)
               MOVE    SPA-GMN-TDENPNUM(IDX1)  TO  S02-TDENPNUM   (IDX1)
               MOVE    SPA-GMN-DENPGRP(IDX1)   TO  S02-TDENPNUM   (IDX1)
                                                                  (9:1)
               MOVE    SPA-GMN-TNYUGAIKBN(IDX1)
                                               TO  S02-TNYUGAI    (IDX1)
               MOVE    SPA-GMN-TSRYKA(IDX1)    TO  S02-TSRYKA    (IDX1)
               MOVE    SPA-GMN-THKNCOMBI(IDX1) TO  S02-THKN       (IDX1)
               MOVE    SPA-GMN-TFTNRATE(IDX1)  TO  S02-TFTNRATE   (IDX1)
               MOVE    SPA-GMN-THAKYMD (IDX1)  TO  S02-TSKYSTYMD  (IDX1)
               MOVE    SPA-GMN-TSRYYMD (IDX1)  TO  S02-TSRYYMD    (IDX1)
               MOVE    SPA-GMN-TSEIKYU(IDX1)   TO  WRK-TSEIKYU-W
               MOVE    WRK-TSEIKYU-W           TO  S02-TSKYMONEY  (IDX1)
               MOVE    SPA-GMN-TNYUKIN (IDX1) TO  WRK-TNYUKIN-W
               MOVE    WRK-TNYUKIN-W          TO  S02-TNYUKINMONEY(IDX1)
               MOVE    SPA-GMN-TMISYU (IDX1)   TO  WRK-TMISYU-W
               MOVE    WRK-TMISYU-W            TO  S02-TMISYUMONEY(IDX1)
               MOVE    SPA-GMN-TJYOTAI (IDX1)  TO  S02-TDENPJTI   (IDX1)
      *
               IF      IDX1             =   SPA-GMN-TBL-SEL
                   MOVE    "T"             TO  S02-SYULIST-SELECT (IDX1)
      *        暫定 ST
                   IF  SPA-NAI-TNYUGAIKBN(IDX1) =   "1"
                       MOVE    WIDGET-INSENSITIVE
                                               TO  S02-B04-STATE
                       MOVE    WIDGET-NORMAL
                                               TO  S02-B03S-STATE
                   END-IF
      *        暫定 ED
               ELSE
                   MOVE    "F"             TO  S02-SYULIST-SELECT (IDX1)
               END-IF
      *
               EVALUATE   SPA-GMN-SIKIBETUFLG(IDX1)
                 WHEN         "1"
                   EVALUATE   SPA-GMN-FTNHENFLG(IDX1)
                     WHEN         1
                       EVALUATE   WRK-FTNHENFLG1
                         WHEN        ZERO
                           MOVE   1   TO   WRK-FTNHENFLG1
                         WHEN         2
                           MOVE   3   TO   WRK-FTNHENFLG1
                       END-EVALUATE
                     WHEN         2
                       EVALUATE   WRK-FTNHENFLG1
                         WHEN        ZERO
                           MOVE   2   TO   WRK-FTNHENFLG1
                         WHEN         1
                           MOVE   3   TO   WRK-FTNHENFLG1
                       END-EVALUATE
                   END-EVALUATE
                 WHEN         "2"
                   EVALUATE   SPA-GMN-FTNHENFLG(IDX1)
                     WHEN         1
                       EVALUATE   WRK-FTNHENFLG2
                         WHEN        ZERO
                           MOVE   1   TO   WRK-FTNHENFLG2
                         WHEN         2
                           MOVE   3   TO   WRK-FTNHENFLG2
                       END-EVALUATE
                     WHEN         2
                       EVALUATE   WRK-FTNHENFLG2
                         WHEN        ZERO
                           MOVE   2   TO   WRK-FTNHENFLG2
                         WHEN         1
                           MOVE   3   TO   WRK-FTNHENFLG2
                       END-EVALUATE
                   END-EVALUATE
               END-EVALUATE
      *
           END-PERFORM
           MOVE    SPA-GMN-MAX                 TO  S02-SYULIST-COUNT
      *
           MOVE    SPA-GMN-SELBAN              TO  WRK-SELBAN-W
           MOVE    WRK-SELBAN-W                TO  S02-SELNUM
      *
           IF     (WRK-FTNHENFLG1  =  3) AND (WRK-FTNHENFLG2  NOT =  3)
           MOVE    "red"       TO    S02-LBLMSG-STYLE
       MOVE "自立支援医療負担割合変更  同月請求分確認及び再請求（入）"
                               TO    S02-LBLMSG
           END-IF
           IF     (WRK-FTNHENFLG1  NOT =  3) AND (WRK-FTNHENFLG2  =  3)
           MOVE    "red"       TO    S02-LBLMSG-STYLE
       MOVE "自立支援医療負担割合変更  同月請求分確認及び再請求（外）"
                               TO    S02-LBLMSG
           END-IF
           IF     (WRK-FTNHENFLG1      =  3) AND (WRK-FTNHENFLG2  =  3)
           MOVE    "red"       TO    S02-LBLMSG-STYLE
       MOVE "自立支援医療負担割合変更  同月請求分確認及び再請求（入外）"
                               TO    S02-LBLMSG
           END-IF
      *
           MOVE    SPA-GMN-GAI-MISYU           TO  S02-GAI-MISYU
      *
           MOVE    SPA-GMN-NYU-MISYU           TO  S02-NYU-MISYU
      *
           MOVE    WIDGET-NORMAL               TO  S02-PTNUM-STATE
      *
           MOVE    WIDGET-NORMAL               TO  S02-B04S-STATE
           IF    ( SPA-GMN-NYUGAIKBN           =   "1" )
               MOVE    WIDGET-INSENSITIVE      TO  S02-B04S-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面カーソルセット処理
      **************************************************************
       5001-MAPCUR-SEC             SECTION.
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
               PERFORM 420-CLEA-SEC
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
      *    調整金
      **   WHEN    "CLICKED"       ALSO    "B10"
      **           PERFORM 480-CHOSEI-SEC
      *    修正履歴
           WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 490-RIREKI-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
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
      *    未収検索（Ｓ０５）
           WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 470-MISYUKEN-SEC
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
               GO  TO                  4101-SELNUM-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SELBAN      >   SPA-GMN-MAX )
               MOVE    "0001"              TO  SPA-ERRCD
               GO  TO                  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELBAN      TO  SPA-GMN-TBL-SEL
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
                                       SPA-GMN-TBL
                                       SPA-GMN-MISYU
                                       SPA-GMN-INPUT
                                       SPA-NAI-TBL
                                       SPA-GMN-MAX
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
               MOVE    SPTNUM-PTNUM    TO  SPTID-PTNUM
               MOVE    SPTNUM-PTID     TO  SPTID-PTID
           WHEN   10
               MOVE    "0031"          TO  SPA-ERRCD
               GO  TO  4102-PTNUM-CHK-EXT
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
               GO  TO     4102-PTNUM-CHK-EXT
           WHEN    OTHER
               MOVE    "0031"          TO  SPA-ERRCD
               GO  TO     4102-PTNUM-CHK-EXT
           END-EVALUATE
      *
           MOVE    SPTID-PTID          TO  SPA-GMN-PTID
           MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
      *
           PERFORM 320-PTINF-SET-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               IF    ( SPA-ERRCD   =   "9999" )
      *            未収額を編集
                   PERFORM 3101-MISYU-EDIT-SEC
               END-IF
               GO      TO      4102-PTNUM-CHK-EXT
           END-IF
      *
           PERFORM 320-HKNCOMBI-EDIT-SEC
      *
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    入外区分
           IF    ( SPA-BEDSU       >   ZERO )
               MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
                   MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-IF
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     = ZERO )
               MOVE    "0020"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-CUR
           ELSE
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
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
           MOVE    S02-SRYKA           TO  SPA-GMN-SRYKA-G
      *
           IF  (   SPA-GMN-SRYKAMEI    =   SPACE )
               MOVE    "0030"          TO  SPA-ERRCD
               GO  TO  4103-SRYKA-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           PERFORM VARYING IDX7   FROM 1   BY  1
                   UNTIL ( IDX7   >    SPA-GMN-SRYKA-MAX )
               IF    ( SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX7) )
                   MOVE    ZERO        TO  FLG-ERR
                   MOVE    SPA-GMN-SRYKA-MAX
                                       TO  IDX7
               END-IF
           END-PERFORM
      *
           IF    ( FLG-ERR         NOT =   ZERO )
               MOVE    "0030"          TO  SPA-ERRCD
               GO  TO  4103-SRYKA-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
            OR   ( SPA-NAI-SRYYM       =   ZERO )
               MOVE    "0050"              TO  SPA-ERRCD
               GO  TO  4103-SRYKA-CHK-EXT
           END-IF
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療年月入力処理
      **************************************************************
       4104-SRYYM-CHK-SEC            SECTION.
      *
           MOVE    3                  TO  SPA-GMN-CUR
      *
           INITIALIZE                 SPA-GMN-TBL
                                      SPA-GMN-MAX
                                      SPA-NAI-SRYYM
                                      SPA-NAI-TBL
                                      SPA-GMN-INPUT
      *
           MOVE    S02-SRYYM          TO  SPA-GMN-SRYYM
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYM      TO  SGDAY-INDAY
           MOVE    "1"                TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"         USING
                                      ORCSGDAYAREA
           IF    ( SGDAY-RC           =   ZERO )
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-SRYYMDW
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYM
      ******** MOVE    SGDAY-SDAY          TO  SPA-SRYYMD
           ELSE
               MOVE    "0033"              TO  SPA-ERRCD
               GO  TO  4104-SRYYM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
      *
      *    入外区分
           IF    ( SPA-BEDSU       >   ZERO )
               MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
           ELSE
               IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
                   MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-IF
      *
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4104-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-LSTSEL-SEC       SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-GMN-SELBAN
                                           SPA-GMN-TBL-SEL
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-MAX )
      *
               IF    ( S02-SYULIST-SELECT (IDX1)   =   "T" )
                   MOVE    IDX1        TO  SPA-GMN-SELBAN
                                           SPA-GMN-TBL-SEL
                   MOVE    200         TO  IDX1
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
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-MAX
                                       SPA-NAI-TBL
                                       SPA-GMN-INPUT
      *
           MOVE    S02-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
      *
           IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
               MOVE    "0034"             TO  SPA-ERRCD
               GO  TO  4106-NYUGAIKBN-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       SPA-GMN-NYUGAIKBN-MAX )
               IF    ( SPA-GMN-NYUGAIKBN
                                   =   SPA-GMN-NYUGAIKBNL (IDX1) )
                   MOVE    ZERO        TO  FLG-ERR
                   MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  IDX1
               END-IF
           END-PERFORM
      *
           IF    ( FLG-ERR     NOT =   ZERO )
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO  4106-NYUGAIKBN-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
            OR   ( SPA-NAI-SRYYM       =   ZERO )
            OR   ( SPA-GMN-SRYKA       =   SPACE )
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO  4106-NYUGAIKBN-CHK-EXT
           END-IF
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           IF    ( SPA-GMN-MAX     >   ZERO )
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       4106-NYUGAIKBN-CHK-EXT.
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
           IF    ( SPA-MOTOPG2         =   "K02" OR "K02N"
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
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-SKYPRTFLG     =   "0" )
                   MOVE    "2003"      TO  SPA-SIDCD
               ELSE
                   PERFORM 420-SAIHAKKOU-SEC
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
           MOVE    "01"                TO  WRK-SYORINO
      *
      ***     MOVE    "S02"               TO  SPA-MOTOPG
      ***     MOVE    "S03"               TO  SPA-SAKIPG
      *
      ***     MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      ***     MOVE   "S03    "            TO  MCP-WINDOW.
      *
      ***     PERFORM 900-PUT-WINDOW.
      *
      ***     MOVE    1                   TO  FLG-END
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
      *
               MOVE    "01"            TO  WRK-SPA-PRT-FLG
               CALL    WRK-SEIKYU-PG   USING   ORCHCN03AREA
                                               WRK-SPA-AREA
               MOVE    SPACE           TO  WRK-SPA-PRT-FLG
      *
               MOVE   SPACE        TO  SPA-SRYYMD(7:2)
      *
           END-IF
      *
           .
       4201-NYU-SAIHAKKOU-EXT.
           EXIT.
      **************************************************************
      *    外来再発行処理
      **************************************************************
       4201-GAI-SAIHAKKOU-SEC          SECTION.
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
           MOVE    SPA-TERMID          TO  PARA-TERMID
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
           MOVE    SPA-TERMID          TO  PARA-TERMID
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
               GO  TO  4201-GAI-SAIHAKKOU-EXT
           END-IF
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
           MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
           MOVE    SPA-TERMID      TO      ORCHC03S02-TERMID
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
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    WRK-GAI-SEIKYU      TO  ORCHC03-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  ORCHC03-NYUKIN
           MOVE    1                   TO  ORCHC03-HAKKOFLG
           MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
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
           MOVE    SPA-TERMID          TO  ORCHC03-PARA-TERMID
           MOVE    "00102"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    "02"                TO  WRK-SPA-PRT-FLG
           CALL    WRK-SEIKYU-PG           USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "001%"          TO  WRK-FILEMEI-CD
               PERFORM 4801-DELETE-PARA-SEC
           END-IF
      *
           MOVE   SPACE        TO  SPA-SRYYMD(7:2)
      *
           .
       4201-GAI-SAIHAKKOU-EXT.
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
           MOVE    ZERO            TO  IDX-LST
      *
           PERFORM VARYING IDX10   FROM   1   BY  1
                   UNTIL ( IDX10   >   SPA-GMN-MAX )
                    OR   ( IDX-LST >   ZERO )
      *
               IF    ( SPA-NAI-TNYUGAIKBN (IDX10)  =   "2" )
                   MOVE    IDX10   TO  IDX-LST
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-LST         >   ZERO )
      *
      *        印刷制御情報取得サブ
               CALL    "ORCSONPRTSET"  USING   SPA-AREA
      *
               PERFORM 4901-GAI-TUKISKYPRT-SEC
      *
           END-IF
      *
           .
       490-GAI-TUKISKYPRT-EXT.
           EXIT.
      **************************************************************
      *    外来月まとめ請求書兼領収書発行処理
      **************************************************************
       4901-GAI-TUKISKYPRT-SEC         SECTION.
      *
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
           MOVE    WRK-SPA-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    WRK-SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM   (IDX-LST)
                                       TO  SYU-DENPNUM
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
           MOVE    4                   TO  SYUPARA01-SKYSUMFLG
           MOVE    SPA-SYSYMD          TO  SYUPARA01-DENPPRTYMD
      *
           IF    ( SYS-1039-TUKISKYMISYUPRTKBN =   "0" )
               MOVE    ZERO            TO  SYUPARA01-NYU-MISYU
                                           SYUPARA01-GAI-MISYU
                                           SYUPARA01-GAI-MISYU-P
                                           SYUPARA01-GAI-MISYU-M
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
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
           PERFORM 900-SYUNOU-KEY37-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               PERFORM 42011-SYUPARA02-HEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4901-GAI-TUKISKYPRT-EXT
               END-IF
      *
               PERFORM 900-SYUNOU-KEY37-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      ORCHC03S02AREA
           MOVE    SPA-HOSPNUM     TO      ORCHC03S02-HOSPNUM
           MOVE    "S02"           TO      ORCHC03S02-GYOUMUID
           MOVE    SPA-TERMID      TO      ORCHC03S02-TERMID
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
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-GAI-DENPNUM     TO  ORCHC03-DENPNUM
           MOVE    WRK-GAI-SEIKYU      TO  ORCHC03-SEIKYU
           MOVE    WRK-GAI-NYUKIN      TO  ORCHC03-NYUKIN
           MOVE    4                   TO  ORCHC03-HAKKOFLG
           MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
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
           MOVE    SPA-TERMID          TO  ORCHC03-PARA-TERMID
           MOVE    "00102"             TO  WRK-FILEMEI-CD
           MOVE    WRK-FILEMEI         TO  ORCHC03-PARA-FILEMEI
           MOVE    "03"                TO  WRK-SPA-PRT-FLG
           CALL    WRK-SEIKYU-PG           USING
                                           WRK-SPA-AREA
                                           ORCHC03AREA
           MOVE    SPACE               TO  WRK-SPA-PRT-FLG
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "001%"          TO  WRK-FILEMEI-CD
               PERFORM 4801-DELETE-PARA-SEC
           END-IF
      *
           .
       4901-GAI-TUKISKYPRT-EXT.
           EXIT.
      *************************************************************
      *    パラメタ削除処理
      *************************************************************
       4801-DELETE-PARA-SEC            SECTION.
      *
           INITIALIZE                  PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "S02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
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
      *    請求書
           MOVE    SPACE               TO  WRK-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1032-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHC03"           TO  WRK-SEIKYU-PG
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
      *    請求書
           MOVE    SPACE               TO  WRK-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHCN03"          TO  WRK-SEIKYU-PG
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
           MOVE    "02"                TO  WRK-SYORINO
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               GO  TO  430-TORIKESI-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELBAN      TO  IDX4
      *
      *    外来分の請求取消は不可
           IF    ( SPA-NAI-TNYUGAIKBN (IDX4)   =   "2" )
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO  430-TORIKESI-EXT
           END-IF
      *
      *    請求取消データの取消不可
           IF    ( SPA-NAI-TJYOTAIKBN (IDX4)   =   "3" )
               MOVE    "0003"          TO  SPA-ERRCD
               GO  TO  430-TORIKESI-EXT
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "2001"              TO  SPA-SIDCD
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
           MOVE    "03"                TO  WRK-SYORINO
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
           MOVE    ZERO                TO  FLG-HIT
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   SPA-GMN-MAX )
                    OR   ( FLG-HIT =   1 )
      *
               IF    ( SPA-NAI-TNYUGAIKBN (IDX3)   =   "2" )
                   MOVE    1           TO  FLG-HIT
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-HIT         =   ZERO )
               MOVE    "0060"          TO  SPA-ERRCD
               GO  TO  440-IKATUSAIKEI-CHK-EXT
           END-IF
      *
           MOVE    "2002"              TO  SPA-SIDCD
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
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           PERFORM 900-SYUNOU-KEY35-SEL-SEC
      *
           PERFORM   UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC     TO   SYUNOU-REC
      *
               MOVE    ZERO            TO   FLG-ARI
               PERFORM    VARYING   IDX3   FROM   1   BY   1
                       UNTIL  (IDX3                   >   15 )
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
               PERFORM 900-SYUNOU-KEY35-FET-SEC
      *
               IF    SPA-ERRCD   NOT =  SPACE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
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
           MOVE    SPA-NAI-SRYYM  TO  LNK-SYUACC-I-SRYYM
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
           MOVE    "04"                TO  WRK-SYORINO
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
           MOVE    "05"                TO  WRK-SYORINO
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
       470-NYUDEL-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      470-NYUDEL-EXT
           END-IF
      *
           MOVE    "06"                TO  WRK-SYORINO
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
             IF (SYU-DENPJTIKBN =  SPACE  OR "3"   )   OR
                (SYU-NYUKIN-TOTAL  =       ZERO    )
                  MOVE    "0002"     TO    SPA-ERRCD
             ELSE
                  MOVE    "2"        TO    SYU-DENPJTIKBN
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
      *****PERFORM    310-SPA-SET-SEC.
      *
           PERFORM 3101-MISYU-EDIT-SEC
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       470-NYUDEL-EXT.
           EXIT.
      *
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
      *    調整金
      *************************************************************
       480-CHOSEI-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      480-CHOSEI-EXT
           END-IF
      *
           MOVE    "07"                TO  WRK-SYORINO
      *
      *****MOVE    SPA-GMN-THKNCOMBI(SPA-GMN-SELBAN) TO
      *****                            SPA-HKNCOMBIMEI
      *****MOVE    SPA-GMN-TSRYKAMEI(SPA-GMN-SELBAN) TO
      *****                            SPA-SRYKAMEI
      *****MOVE    SPA-GMN-TDENPNUM(SPA-GMN-SELBAN)  TO
      *****                            SPA-S01-DENPNUM
      *
           INITIALIZE                  SPA-S01KYOUTU
           MOVE    "07"                TO  SPA-S01-SYORINO
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
       480-CHOSEI-EXT.
           EXIT.
      *************************************************************
      *    履歴修正
      *************************************************************
       490-RIREKI-SEC           SECTION.
      *
           IF      SPA-GMN-SELBAN      =   ZERO
               GO      TO      490-RIREKI-EXT
           END-IF
      *
           MOVE    "08"                TO  WRK-SYORINO
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
           MOVE    SPA-NAI-LOCK        TO  SPA-S01-LOCK
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
           IF      SPA-NAI-SRYYM       =   ZERO
               GO  TO  490-ZENGETU-EXT
           END-IF
      *
      *   前月の月算出
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN 
           CALL  "ORCSDAY"    USING    STS-AREA-DAY  LNK-DAY6-AREA
           IF      STS-DAY-RC1         NOT =   ZERO
              MOVE 0                   TO  LNK-DAY6-KEISAN
           END-IF
           MOVE    LNK-DAY6-KEISAN(1:6)    TO  SPA-NAI-SRYYM
           MOVE    LNK-DAY6-KEISAN         TO  SPA-SRYYMD
      *
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-DAY6-KEISAN     TO  LNK-DAY2-YMD
           CALL  "ORCSDAY"   USING    STS-AREA-DAY  LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1(1:6)   TO  SPA-GMN-SRYYM
               MOVE    LNK-DAY2-EDTYMD1        TO  SPA-SRYYMDW
           END-IF
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
               GO  TO  490-ZENGETU-EXT
           END-IF
      *
           IF    ( SPA-GMN-SRYKA       =   SPACE )
               MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           END-IF
      *
      *    入外区分
           IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
               IF    ( SPA-BEDSU       >   ZERO )
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
               ELSE
                   MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
                END-IF
           END-IF
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
      *
           INITIALIZE              SPA-GMN-INPUT
                                   SPA-GMN-TBL-SEL
      *
           .
       490-ZENGETU-EXT.
           EXIT.
      *
      **************************************************************
      *    次月表示処理
      **************************************************************
       490-JIGETU-SEC                 SECTION.
      *
           IF      SPA-NAI-SRYYM       =   ZERO
               GO  TO  490-JIGETU-EXT
           END-IF    
      *
      *   次月算出
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"  USING    STS-AREA-DAY  LNK-DAY6-AREA
           IF      STS-DAY-RC1         NOT =   ZERO
              MOVE 0                   TO  LNK-DAY6-KEISAN
           END-IF
           MOVE    LNK-DAY6-KEISAN(1:6)    TO  SPA-NAI-SRYYM
           MOVE    LNK-DAY6-KEISAN         TO  SPA-SRYYMD
      *
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-DAY6-KEISAN     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"  USING    STS-AREA-DAY  LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1(1:6)   TO  SPA-GMN-SRYYM
               MOVE    LNK-DAY2-EDTYMD1        TO  SPA-SRYYMDW
           END-IF
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
               GO  TO  490-JIGETU-EXT
           END-IF    
      *
           IF    ( SPA-GMN-SRYKA       =   SPACE )
               MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           END-IF
      *
      *    入外区分
           IF    ( SPA-GMN-NYUGAIKBN   =   SPACE )
               IF    ( SPA-BEDSU       >   ZERO )
                   MOVE    SPA-GMN-NYUGAIKBNLST (1)
                                       TO  SPA-GMN-NYUGAIKBN-G
               ELSE
                   MOVE    SPA-GMN-NYUGAIKBNLST (3)
                                       TO  SPA-GMN-NYUGAIKBN-G
                END-IF
           END-IF
      *
           PERFORM 300-SYUNOU-HENSYU-SEC
           INITIALIZE              SPA-GMN-INPUT
                                   SPA-GMN-TBL-SEL
      *
           .
       490-JIGETU-EXT.
           EXIT.
      *
      **************************************************************
      *    共通パラメタ更新・画面遷移処理
      **************************************************************
       4901-PARA-HEN-SEC                 SECTION.
      *
       4901-PARA-HEN-EXT.
           EXIT.
      *
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
           PERFORM VARYING IDX11   FROM    1   BY  1
                   UNTIL ( FLG-CHK =   1   )
                   OR    ( IDX11   >   100 )
               IF    ( WRK-KUMINO  =   SPA-NAI-HKNCOMBIL (IDX11))
                   MOVE    SPA-NAI-HKNCOMBIMEIL (IDX11)
                                       TO  WRK-HKNKUMI
                   MOVE    SPA-NAI-HKNNUML (IDX11)
                                       TO  WRK-HKNNUM
                   MOVE    SPA-NAI-RSI-HKNKBNL (IDX11)
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
       420-CLEA-SEC        SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-NAI-SRYYM      TO   WRK-NAI-SRYYM
      *
           INITIALIZE                  SPA-S02-AREA
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-NAI-SRYYM      TO  SGDAY-INDAY
           MOVE    "1"                TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"         USING
                                      ORCSGDAYAREA
           IF    ( SGDAY-RC           =   ZERO )
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-SRYYMDW
               MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYM
           ELSE
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
           END-IF
      *
           PERFORM 3101-SRYKA-SPA-SEC
           PERFORM 3102-NYUGAIKBN-SPA-SEC
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       420-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       430-MAEKPTNUM-SEC              SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      430-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  S02-PTNUM
           PERFORM 4102-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  430-MAEKPTNUM-EXT
           END-IF
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *****************************************************************
      *    未収検索画面へ
      *****************************************************************
       470-MISYUKEN-SEC                SECTION.
      *
           IF    ( SPA-GMN-PTID        =   ZERO )
               MOVE    "1003"          TO  SPA-ERRCD
               GO  TO  470-MISYUKEN-EXT
           END-IF
      *
           INITIALIZE                  SPA-S01KYOUTU
      *
           PERFORM 4901-SPAKYOTUKEY-SET-SEC
      *
           MOVE    SPA-S01KYOUTU       TO  SPA-WORK
      *
      *     MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "S02"               TO  SPA-MOTOPG
           MOVE    "S05"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
           MOVE    "S05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-MISYUKEN-EXT.
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
      *
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
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-S01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-S02
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
               WHEN    "0020"
                   MOVE    "対象の診療年月に収納はありません"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "診療科目入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "該当の患者が存在しません。"
                                               TO  SPA-ERRMSG
      *******  WHEN    "0032"
      ************ MOVE    "ＩＤ取得サブでエラー" 
      ********                                 TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "診療年月入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "入外区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0050"
                  MOVE "患者番号・診療科・診療年月の入力がありません" 
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
      *----(01.01.19)--ADD-START---
               WHEN    "1003"
                   MOVE    "患者が選択されていません"
                                               TO  SPA-ERRMSG
      *----(01.01.19)--ADD-END-----
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
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
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
           WHEN    "2003"
               MOVE    SPACE            TO  WRK-SIDMSG
               STRING  "請求書不要の設定がされています。"
                                       DELIMITED   BY  SIZE
                       "再発行を行いますか"
                                       DELIMITED   BY  SIZE
               INTO  WRK-SIDMSG
               END-STRING
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
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
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
       900-SYUNOU-KEY35-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY35-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY35-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY35-FET-EXT.
           EXIT.
      **************************************************************
      *    収納マスター検索処理
      **************************************************************
       900-SYUNOU-KEY37-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY37-SEL-EXT.
           EXIT.
      **************************************************************
      *    収納マスターFETCH処理
      **************************************************************
       900-SYUNOU-KEY37-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY37-FET-EXT.
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
