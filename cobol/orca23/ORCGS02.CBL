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
      *  01.00.38    NACL-門脇    04.12.17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
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
                           11  SPA-GMN-TNYUGAIKBN  PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(06).
                           11  SPA-GMN-THKNCOMBI   PIC X(50).
                           11  SPA-GMN-TFTNRATE    PIC X(08).
                           11  SPA-GMN-TSRYYMD     PIC X(15).
                           11  SPA-GMN-TSKYEDYMD   PIC X(06).
                           11  SPA-GMN-THAKYMD     PIC X(09).
                           11  SPA-GMN-TSEIKYU     PIC 9(10).
                           11  SPA-GMN-TMISYU      PIC S9(10).
                           11  SPA-GMN-TJYOTAI     PIC X(16).
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
           03  FLG-SINKOU          PIC 9(01).
           03  FLG-SINKAI          PIC 9(01).
           03  FLG-HKNMST          PIC 9(01).
           03  FLG-HKNKUMI         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTTEIKIRRK      PIC 9(01).
           03  FLG-TEIKIRRK        PIC 9(01).
           03  FLG-SENIN           PIC 9(01).
           03  FLG-SENIN-SYOSIN    PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-DBRC            PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYULIST-ROW     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX5                PIC 9(05).
           03  IDX6                PIC 9(05).
           03  IDX7                PIC 9(05).
           03  IDX8                PIC 9(05).
           03  IDX9                PIC 9(05).
           03  IDX10               PIC 9(05).
           03  IDX11               PIC 9(05).
           03  IDX12               PIC 9(05).
           03  SAI-IDX             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH          PIC X(20).
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
           03  WRK-SAIKEISAN-YMD.
               05  WRK-SAIKEISAN-YY PIC 9(04).
               05  WRK-SAIKEISAN-MM PIC 9(02).
               05  WRK-SAIKEISAN-DD PIC 9(02).
      *----(01.00.03) LINE ADD START -------------------------------
           03  WRK-TYK-HKNTEN       PIC S9(07).
      *----(01.00.03) LINE ADD END   -------------------------------
      *
      *    保険組合せ名称
           03  WRK-KUMINO          PIC X(04).
           03  WRK-HKNKUMI         PIC X(40).
           03  WRK-HKNKBN-MEI      PIC X(04).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-RSI-HKNKBN      PIC X(01).
      *
           03  WRK-ZAIKAISU        PIC 9(07).
      *
           03  WRK-MAE-SEIKYUGK    PIC 9(07).
           03  WRK-ATO-SEIKYUGK    PIC 9(07).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
           03  WRK-TEKEDYMD        PIC X(08).
      *
           03  WRK-SYORINO         PIC X(02).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
           03  WRK-SAGAKU          PIC S9(07).
      *     
           03  WRK-TBANGO-W        PIC ZZZ.
           03  WRK-TSEIKYU-W       PIC ZZ,ZZZ,ZZZ.
           03  WRK-TMISYU-W        PIC --,---,---.
           03  WRK-SELBAN-W        PIC ZZZZ.  
      *    請求書ＰＧ名
           03  WRK-SEIKYU-PG       PIC X(20).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-PTNUM           PIC X(20).
           03  WRK-MISYU           PIC S9(08).
           03  WRK-TBL-SEL-MOT     PIC 9(05).
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
      *
       01  WK-GMN-RITU.
           03  WK-GMN-FTNRITU      PIC ZZZ.
           03  WK-GMN-PA           PIC X(01).
           03  FILLER              PIC X(01).
      *
      *    再計算時の剤番号退避用テーブル
       01  ZAIBAN-TABLE.
           03  ZAIBAN-TBL          OCCURS 100.
               05  TBL-SRYKBN      PIC  X(02).
               05  TBL-ZAIBAN      PIC  9(08).
               05  TBL-ZAIKAISU    PIC  9(07).
      *----(01.00.03) LINE MOD START -------------------------------
      *        05  TBL-ZAITEN      PIC  9(08).
               05  TBL-ZAITEN      PIC  S9(08).
               05  TBL-SYUTEN      PIC  9(08).                         
      *----(01.00.03) LINE MOD END   -------------------------------
      *
      *----(01.00.07) LINE ADD START -------------------------------
      *    再計算時の保険適用外金額退避用テーブル
       01  HKNTEKGAI-TABLE.
           03  HKNTEKGAI-TBL       OCCURS 11.
               05  TBL-TGMONEY     PIC  9(08).
               05  TBL-TGMONEY-TAX PIC  9(08).
       01  JIHI-TABLE.
           03  JIHI-TBL            OCCURS 5.
               05  TBL-JIHI-TAXNASI     PIC  9(08).
               05  TBL-JIHI-TAXARI      PIC  9(08).
      *----(01.00.07) LINE ADD END   -------------------------------
      *
       01  RSI-TABLE.
           03  WK-RSISHOSHIN-TENSU PIC  9(08).
           03  WK-RSISAISHIN-TENSU PIC  9(08).
           03  WK-RSISDO-TENSU     PIC  9(08).
           03  WK-RSIETC-TENSU     PIC  9(08).
      *
           03  WK-RSISHOSHIN       PIC  9(08).                         
           03  WK-RSISAISHIN       PIC  9(08).                         
           03  WK-RSISDO           PIC  9(08).                         
           03  WK-RSIETC           PIC  9(08).                         
      *
           03  WK-SINDAN           PIC  9(08).                         
           03  WK-MEISAI           PIC  9(08).                         
      *
           03  WK-YOUBUKASAN       PIC  9(08).
           03  WK-KYOBUKASAN       PIC  9(08).
           03  WK-SOGAIKASAN       PIC  9(08).
           03  WK-RYOYOTEATE       PIC  9(08).
      *
           03  WK-SYUTEN-KEI       PIC  9(08).                         
           03  WRK-TEN             PIC  9(08).                         
      *     
      *    スパ領域 ワーク
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WRK-SPA-//.
      *
       01  CONST-AREA.
           03  CONST-ZENSRYKA-G.
               05  CONST-ZENSRYKA      PIC X(02)   VALUE "00".
               05                      PIC X(01)   VALUE SPACE.
               05  CONST-ZENSRYKAMEI   PIC X(16)   VALUE "全科".
      *
      *    収納明細マスターＷＫ
       01  WKSYUMEI-REC.
           COPY    "CPSYUMEI.INC"  REPLACING //SMEI-//
                                   BY        //WKSMEI-//.
      *
      *    定期請求履歴集計用ワーク
       01  SUMTEIKIRRK-REC.
         02    SUMTEIKIRRK-MAX         PIC 9(05).
         02    SUMTEIKIRRK-OCC     OCCURS 50.
           COPY  "CPTEIKIRRK.INC"  REPLACING  //TEIKIRRK-//
                                           BY //SUMTEIKIRRK-//.
      *
      *診療区分別給付外点数（福岡退避用）
       01  WRK40-KYUFUGAI.
               05  WRK40-KYUFUGAI-TBL        OCCURS   6.
      *    給付対象外点数
                   09  WRK40-KYUFUGAI-TEN    PIC  9(07).

      **************************************************************
      *    ファイルレイアウト
      **************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
           COPY    "CPSK1035.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *    収納明細マスター
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    受診履歴マスター
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    定期請求履歴
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
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
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    帳票サブ < 請求書兼領収書 >
           COPY    "CPORCHCN03.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険組み合せ取得
           COPY    "CPORCSHKNSET.INC".
      *
      *    福岡県地方公費給付対象外点数取得サブ
           COPY    "CPORCSSYU40.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *
       01  LNK-SYUNOU-REC.
           COPY "CPSYUNOU.INC" REPLACING  //SYU-// BY //LNK-SYU-//.
       01  WK-SYUNOU-REC-T.
           02 WK-SYUNOU-REC.
           COPY "CPSYUNOU.INC" REPLACING  //SYU-// BY //WK-SYU-//.
       01  WK-JYURRK-REC-T.
           02 WK-JYURRK-REC    OCCURS  100.
           COPY "CPJYURRK.INC" REPLACING  //JYURRK-// BY
                                                   //WK-JYURRK-//.
      *
           COPY "CPORCS01.INC" REPLACING  //LNK-// BY //WK-//.
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      **************************************************************
      *    連絡　領域
      **************************************************************
        LINKAGE                     SECTION.
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
           COPY    "SERR.INC".
           COPY    "SID1.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
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
           IF      SPA-MOTOPG          =   "SERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"
                                                      OR  "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-S02
                   MOVE    SPA-WORK        TO  SPA-S01KYOUTU
               END-IF
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG(1:3)     =   "Y01"
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
               WHEN    "S04"
               WHEN    "S06"
                   PERFORM 320-S03SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "S05"
                   PERFORM 320-S05SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "SID1"
                   PERFORM 330-SID1-SET-SEC
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
               IF      SPA-SIDCD            =   "2001"
                   PERFORM 430-TORIKESI-KOUSHIN-SEC
                   MOVE    SPA-ERRCD
                                   TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-SID1-FLG
           MOVE    SPACE           TO  SPA-SIDCD
           .
       330-SID1-SET-EXT.
           EXIT.
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
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  ORC-DBYMD
           MOVE    "01"                TO  ORC-DBYMD (7 : 2)
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY2"     TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM VARYING     IDX1   FROM    2    BY  1
                   UNTIL     ( IDX1           >    99   )
                   OR        ( FLG-SYSKANRI   =    1    )
      *
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO
                                             SPA-GMN-SRYKAL    (IDX1)
               MOVE    SYS-1005-SRYKANAME1   TO
                                             SPA-GMN-SRYKAMEIL (IDX1)
               MOVE    SYS-1005-SRYKANAME2   TO
                                             SPA-NAI-SRYKAMEIL (IDX1)
               MOVE    IDX1                  TO
                                             SPA-GMN-SRYKA-MAX
      *
               MOVE    "SYSKANRI-KEY2"       TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC        NOT =   ZERO )
                   MOVE    1                 TO  FLG-SYSKANRI
               END-IF
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "SYSKANRI-KEY2"           TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
      *
      *    患者情報設定処理
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "PTINF-KEY"         TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC    NOT     =   ZERO )
               MOVE    1               TO  FLG-PTINF
               MOVE    "0031"          TO  SPA-ERRCD
               GO  TO  320-PTINF-SET-EXT
           END-IF
      *
           MOVE    MCPDATA-REC         TO  PTINF-REC
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
      **************************************************************
      *   収納リスト編集処理
      **************************************************************
       3101-SYULIST-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-NAI-SRYYM       TO  SYU-SRYYMD (1:6)
           MOVE    "__"                TO  SYU-SRYYMD (7:)
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY14"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC TO  SYUNOU-REC
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
               MOVE    "SYUNOU-KEY14"  TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
           END-PERFORM
      *
           MOVE    "SYUNOU-KEY14"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
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
           MOVE    ZERO                TO  WRK-MISYU
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE  SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY13"      TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC        NOT =   ZERO )
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  =   1 )
      *
               MOVE    MCPDATA-REC TO  SYUNOU-REC
      *
               COMPUTE WRK-MISYU
                   =   WRK-MISYU
                   + ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
               MOVE    "SYUNOU-KEY13"  TO  WRK-DBPATH
               PERFORM 900-TBL-READ-SEC
               IF    ( FLG-DBRC    NOT =   ZERO )
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
           END-PERFORM
      *
           MOVE    "SYUNOU-KEY13"      TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           .
       3101-MISYU-SUM-EXT.
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
      *    入外区分
           MOVE    SYU-NYUGAIKBN       TO  SPA-NAI-TNYUGAIKBN  (IDX1)
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "入"            TO  SPA-GMN-TNYUGAIKBN  (IDX1)
           WHEN    "2"
               MOVE    "外"            TO  SPA-GMN-TNYUGAIKBN  (IDX1)
           END-EVALUATE
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
           PERFORM VARYING     IDX1     FROM    1   BY  1
                   UNTIL     ( IDX1     >       SPA-GMN-MAX )
               MOVE    SPA-GMN-TBANGO (IDX1)   TO  WRK-TBANGO-W
               MOVE    WRK-TBANGO-W            TO  S02-TNUM       (IDX1)
               MOVE    SPA-GMN-TDENPNUM(IDX1)  TO  S02-TDENPNUM   (IDX1)
               MOVE    SPA-GMN-TNYUGAIKBN(IDX1)
                                               TO  S02-TNYUGAI    (IDX1)
               MOVE    SPA-GMN-TSRYKA(IDX1)    TO  S02-TSRYKA    (IDX1)
               MOVE    SPA-GMN-THKNCOMBI(IDX1) TO  S02-THKN       (IDX1)
               MOVE    SPA-GMN-TFTNRATE(IDX1)  TO  S02-TFTNRATE   (IDX1)
               MOVE    SPA-GMN-THAKYMD (IDX1)  TO  S02-TSKYSTYMD  (IDX1)
               MOVE    SPA-GMN-TSRYYMD (IDX1)  TO  S02-TSRYYMD    (IDX1)
               MOVE    SPA-GMN-TSEIKYU(IDX1)   TO  WRK-TSEIKYU-W
               MOVE    WRK-TSEIKYU-W           TO  S02-TSKYMONEY  (IDX1)
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
           END-PERFORM
           MOVE    SPA-GMN-MAX                 TO  S02-SYULIST-COUNT
      *
           MOVE    SPA-GMN-SELBAN              TO  WRK-SELBAN-W
           MOVE    WRK-SELBAN-W                TO  S02-SELNUM
      *
           MOVE    SPA-GMN-GAI-MISYU           TO  S02-GAI-MISYU
      *
           MOVE    SPA-GMN-NYU-MISYU           TO  S02-NYU-MISYU
      *
           MOVE    WIDGET-NORMAL               TO  S02-PTNUM-STATE
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
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEA-SEC
      *    前回患者処理
           WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *
      *    再発行
           WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 420-SAIHAKKOU-SEC
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
           WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 500-SHIHARAI-SEC
      *----(01.01.19)--ADD-END-----
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
       420-SAIHAKKOU-SEC           SECTION.
      *
           IF    ( SPA-GMN-SELBAN      =   ZERO )
               GO  TO      420-SAIHAKKOU-EXT
           END-IF
      *
      *    収納マスタを読み込みし、診療日の取込み
      *    選択された伝票番号から再発行を行う
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "SYUNOU-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-SELECT-SEC
           IF    ( FLG-DBRC            =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "SYUNOU-KEY"        TO  WRK-DBPATH
           PERFORM 900-TBL-CLOSE-SEC
      *
           IF   (  FLG-SYUNOU          =   ZERO )
               IF    ( SYU-DENPJTIKBN      =   "3" )
                   MOVE    "0010"      TO  SPA-ERRCD
               ELSE
                   MOVE   SPA-AREA         TO  WRK-SPA-AREA
                   MOVE   SYU-SRYYMD       TO  WRK-SPA-SRYYMD
                   MOVE   SYU-NYUGAIKBN    TO  WRK-SPA-NYUGAIKBN
                   MOVE   SPA-GMN-PTID     TO  WRK-SPA-PTID
                   MOVE   SPA-GMN-PTNUM    TO  WRK-SPA-PTNUM
                   MOVE   SPA-GMN-NAME     TO  WRK-SPA-NAME
      *            MOVE   SPA-GMN-SRYKA    TO  WRK-SPA-SRYKA
      *            MOVE   SPA-GMN-SRYKAMEI TO  WRK-SPA-SRYKAMEI
      *
      *            労災判定
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
      *            帳票プログラム取得
                   IF    ( SYU-NYUGAIKBN   =   1 )
                       PERFORM 4201-NYU-PRINT-PG-SEC
                   ELSE
                       PERFORM 4201-PRINT-PG-SEC
                   END-IF
      *
      *
                   IF    ( SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN) =   "1" )
                       INITIALIZE      ORCHCN03AREA
      *
                       MOVE    WRK-SPA-HOSPID  TO  ORCHCN03-HEAD-HOSPID
                       MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
                       MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
                       MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
                       MOVE    1               TO  ORCHCN03-CNT
      *
                       MOVE    WRK-SPA-HOSPID  TO  ORCHCN03-HOSPID (1)
                       MOVE    WRK-SPA-PTID    TO  ORCHCN03-PTID   (1)
                       MOVE    WRK-SPA-PTNUM   TO  ORCHCN03-PTNUM  (1)
                       MOVE    "1"         TO  ORCHCN03-NYUGAIKBN (1)
      *
                       MOVE    SYU-SKYSTYMD
                                           TO  ORCHCN03-KJNYMD  (1)
                       MOVE    SYU-DENPNUM
                                           TO  ORCHCN03-DENPNUM (1)
                       MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  ORCHCN03-SEIKYU-TAX-SAI
                                                                (1)
                       MOVE    SYU-SKYMONEY
                                           TO  ORCHCN03-SEIKYU  (1)
                       MOVE    SYU-NYUKIN-TOTAL
                                           TO  ORCHCN03-NYUKIN  (1)
                       MOVE    1           TO  ORCHCN03-HAKKOFLG(1)
      *
                       CALL    WRK-SEIKYU-PG
                                           USING   ORCHCN03AREA
      *
                   ELSE
                       INITIALIZE      ORCHC03AREA
                       MOVE   SYU-DENPNUM  TO  ORCHC03-DENPNUM
                       MOVE   SYU-SKYMONEY TO  ORCHC03-SEIKYU
                       MOVE   SYU-SKYMONEY-TAX-SAI
                                           TO  ORCHC03-SEIKYU-TAX-SAI 
                       MOVE   SYU-NYUKIN-TOTAL
                                           TO  ORCHC03-NYUKIN
                       MOVE   1            TO  ORCHC03-HAKKOFLG
      *****************CALL  "ORCHC03"     USING   WRK-SPA-AREA
                       CALL  WRK-SEIKYU-PG     USING
                                               WRK-SPA-AREA
                                               ORCHC03AREA
                   END-IF
      *
                   MOVE   SPACE        TO  SPA-SRYYMD(7:2)
      *
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
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-NAI-TNYUGAIKBN (IDX4)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM   (IDX4)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
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
      *        患者定期請求履歴削除処理
               PERFORM 4301-PTTEIKIRRK-DEL-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  430-TORIKESI-KOUSHIN-EXT
               END-IF
      *
      *        定期請求履歴削除処理
               PERFORM 4301-TEIKIRRK-UPD-SEC
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
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
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
           MOVE    SPA-GMN-SELBAN      TO  IDX4
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
           MOVE    SPA-NAI-TNYUGAIKBN (IDX4)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM (IDX4)
                                       TO  SMEI-DENPNUM
           MOVE SYUMEI-REC             TO  MCPDATA-REC
           MOVE "DBSELECT"             TO  MCP-FUNC
           MOVE "SYUMEI-KEY2"          TO  ORC-DBPATH
           CALL "ORCMCPSUB"            USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( MCP-RC              =    ZERO )
               MOVE    "SYUMEI-KEY2"   TO   ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( FLG-SYUMEI  =   1 )
               MOVE "SYUMEI-KEY2" TO ORC-DBPATH
               PERFORM   900-SYUMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
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
           MOVE ZERO                   TO  FLG-SYUMEI
           MOVE SYUMEI-REC             TO  MCPDATA-REC
           MOVE "DBSELECT"             TO  MCP-FUNC
           MOVE "SYUMEI-KEY"           TO  ORC-DBPATH
           CALL "ORCMCPSUB"            USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( MCP-RC              =    ZERO )
               MOVE    "SYUMEI-KEY"    TO   ORC-DBPATH
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( FLG-SYUMEI          =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    WKSYUMEI-REC        TO  SYUMEI-REC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
      *    患者定期請求履歴削除処理
      **************************************************************
       4301-PTTEIKIRRK-DEL-SEC         SECTION.
      *
           INITIALIZE  PTTEIKIRRK-REC
           MOVE    SPA-NAI-SRYYM       TO  PTTEIKIRRK-SRYYM
           MOVE    SPA-HOSPID          TO  PTTEIKIRRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY6"   TO  ORC-DBPATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL "ORCMCPSUB"            USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( MCP-RC              =   ZERO )
               MOVE    "PTTEIKIRRK-KEY6"
                                       TO  ORC-DBPATH
               PERFORM 980-PTTEIKIRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTTEIKIRRK-KEY2"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( FLG-PTTEIKIRRK  =    1 )
               MOVE    "1004"      TO   SPA-ERRCD
               GO  TO  4301-PTTEIKIRRK-DEL-EXT
           END-IF
      *
      *    患者定期請求情報削除
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "PTTEIKIRRK-KEY"    TO  ORC-DBPATH
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "1004"      TO  SPA-ERRCD
               GO  TO  4301-PTTEIKIRRK-DEL-EXT
           END-IF
      *
           .
       4301-PTTEIKIRRK-DEL-EXT.
           EXIT.
      **************************************************************
      *    定期請求履歴更新処理
      **************************************************************
       4301-TEIKIRRK-UPD-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE  TEIKIRRK-REC
           MOVE    PTTEIKIRRK-SRYYM
                                   TO  TEIKIRRK-SRYYM
           MOVE    PTTEIKIRRK-SAKKBN
                                   TO  TEIKIRRK-SAKKBN
      *
           MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
           MOVE    "TEIKIRRK-KEY"  TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "TEIKIRRK-KEY"  TO  ORC-DBPATH
               PERFORM 980-TEIKIRRK-READ-SEC
           ELSE
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "TEIKIRRK-KEY"  TO  ORC-DBPATH
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF    ( FLG-PTTEIKIRRK  =   ZERO )
      *
      *    // 更新処理 //
      *
               COMPUTE TEIKIRRK-SKYTEN
                   =   TEIKIRRK-SKYTEN
                   + ( SYU-TOTAL-HKNTEN      *   -1 )
               COMPUTE TEIKIRRK-SKYMONEY
                   =   TEIKIRRK-SKYMONEY
                   + ( SYU-SKYMONEY          *   -1 )
      *
               MOVE    SPA-TERMID      TO  TEIKIRRK-TERMID
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  TEIKIRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  TEIKIRRK-UPHMS
      *
               MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
               MOVE    "TEIKIRRK-KEY"  TO  ORC-DBPATH
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1005"      TO  SPA-ERRCD
                   GO  TO  4301-TEIKIRRK-UPD-EXT
               END-IF
           ELSE
      *
      *    // 追加処理 //
      *
               INITIALIZE  TEIKIRRK-REC
               MOVE    PTTEIKIRRK-SRYYM    TO  TEIKIRRK-SRYYM
               MOVE    PTTEIKIRRK-SAKKBN   TO  TEIKIRRK-SAKKBN
               MOVE    PTTEIKIRRK-SKYSTYMD TO  TEIKIRRK-SKYSTYMD
               MOVE    PTTEIKIRRK-SKYEDYMD TO  TEIKIRRK-SKYEDYMD
               COMPUTE TEIKIRRK-SKYTEN
                   =   SYU-TOTAL-HKNTEN    *   -1
               COMPUTE TEIKIRRK-SKYMONEY
                   =   SYU-SKYMONEY        *   -1
               MOVE    SPA-TERMID      TO  TEIKIRRK-TERMID
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  TEIKIRRK-CREYMD
                                           TEIKIRRK-UPYMD
               MOVE    SMCNDATE-HMS    TO  TEIKIRRK-UPHMS
      *
               MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
               MOVE    "TEIKIRRK-KEY"  TO  ORC-DBPATH
               MOVE    "DBINSERT"      TO  MCP-FUNC
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1005"      TO  SPA-ERRCD
                   GO  TO  4301-TEIKIRRK-UPD-EXT
               END-IF
           END-IF
      *
           .
       4301-TEIKIRRK-UPD-EXT.
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
      *    福岡対応（給付対象外の算出）
           IF      SPA-PREFNUM         =   40
               INITIALIZE           ORCSSYU40AREA
               MOVE    "2"             TO  ORCS40-KBN
               MOVE    SPA-GMN-PTID    TO  SPA-PTID
               MOVE    "2"             TO  SPA-NYUGAIKBN
               MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  ORCS40-DENPNUM
               CALL    "ORCSSYU40"     USING   SPA-AREA
                                               ORCSSYU40AREA
                                               SYUNOU-REC
               PERFORM    VARYING   IDX   FROM  1   BY  1
                          UNTIL     IDX   >     6
                   MOVE   SYU-KYUFUGAI-TEN(IDX)  TO
                                            WRK40-KYUFUGAI-TEN(IDX)
               END-PERFORM
           END-IF
      *
      *------------------------------------------------------------
      *    診療日に該当する診療会計マスタ・診療行為マスタを検索し
      *    患者負担算定サブの必要項目をセットする
      *------------------------------------------------------------
      *
           MOVE     SPACE      TO      LNK-ORCS01-REC
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX2
      *
      *    収納マスタを読み込み診療日の再取込み
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
      *
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
      *
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
      *                                 
           IF      FLG-SYUNOU          =   1
                   MOVE   SPACE        TO  WRK-SAIKEISAN-YMD
                   MOVE   ZERO         TO  WRK-MAE-SEIKYUGK
           ELSE
                   MOVE   SYU-SRYYMD   TO  WRK-SAIKEISAN-YMD
                   MOVE   SYU-SKYMONEY TO  WRK-MAE-SEIKYUGK
           END-IF
           IF      SYU-DENPJTIKBN      =   "3"
                   MOVE    "0005"      TO  SPA-ERRCD
                   GO       TO             440-SAIKEI-EXT
           END-IF
      *
           MOVE    SPA-HOSPID          TO  JYURRK-HOSPID
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  JYURRK-SRYKA
           MOVE    SYU-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE      2       TO     SAI-IDX
               MOVE    "JYURRK-KEY4"   TO  ORC-DBPATH
               PERFORM   960-JYURRK-READ-SEC
               PERFORM   UNTIL   (FLG-JYURRK   =   1)    OR
                                 (JYURRK-DENPNUM = SYU-DENPNUM)
                   ADD   1       TO     SAI-IDX
                   MOVE    "JYURRK-KEY4"       TO  ORC-DBPATH
                   PERFORM   960-JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  FLG-SENIN                    
           INITIALIZE                  HKNCOMBI-REC                     
           MOVE    SPA-HOSPID          TO  COMB-HOSPID                  
           MOVE    SPA-GMN-PTID        TO  COMB-PTID                    
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM             
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC                  
           MOVE    "DBSELECT"          TO  MCP-FUNC                     
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH                   
           CALL    "ORCMCPSUB"          USING                           
                                        MCPAREA                         
                                        ORCMCPAREA                      
                                        MCPDATA-REC                     
           IF      MCP-RC              =   ZERO                         
               MOVE    "HKNCOMBI-KEY"  TO  ORC-DBPATH                   
               PERFORM 910-HKNCOMBI-READ-SEC                            
           ELSE                                                         
               MOVE    1               TO  FLG-HKNKUMI                  
           END-IF                                                       
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC                     
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH                   
           CALL    "ORCMCPSUB"          USING                           
                                        MCPAREA                         
                                        ORCMCPAREA                      
                                        MCPDATA-REC                     
      *
           IF     FLG-HKNKUMI         =   ZERO                          
              IF    COMB-HKNNUM           =    "002"                    
                 EVALUATE   COMB-HOJOKBN                                
                    WHEN   "3"                                          
                    WHEN   "C"                                          
                    WHEN   "F"                                          
                        MOVE     1     TO     FLG-SENIN                 
                 END-EVALUATE                                           
              END-IF                                                    
           END-IF                                                       
      *
      *    取り込んだ診療日から該当患者の診療会計マスタを検索し
      *    診療区分と剤番号を剤番号テーブルに退避する
      *   （保険組合せが同じデータに限る）
           MOVE    ZERO                TO  FLG-SINKAI
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPID          TO  ACCT-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                       TO  ACCT-SRYKA
           MOVE    SYU-SRYYMD (1:6)    TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SRYACCT-KEY32"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SRYACCT-KEY32" TO  ORC-DBPATH
               PERFORM SINKAI-NEXT-SEC
           ELSE
               MOVE    1               TO      FLG-SINKAI
           END-IF
           MOVE    1                   TO      IDX1
           MOVE    SPACE               TO      ZAIBAN-TABLE
           INITIALIZE                          ZAIBAN-TABLE
           MOVE    SPACE               TO      HKNTEKGAI-TABLE
           INITIALIZE                          HKNTEKGAI-TABLE
           MOVE    SPACE               TO      JIHI-TABLE
           INITIALIZE                          JIHI-TABLE
      *
           MOVE     ZERO               TO      WK-RSISHOSHIN           
                                               WK-RSISAISHIN           
                                               WK-RSISDO               
                                               WK-RSIETC               
                                               WK-SYUTEN-KEI           
      *
           MOVE     ZERO               TO      WK-SINDAN                
                                               WK-MEISAI                
      *
           MOVE     ZERO               TO      WK-YOUBUKASAN
                                               WK-KYOBUKASAN
                                               WK-SOGAIKASAN
                                               WK-RYOYOTEATE
      *
           MOVE     ZERO               TO      FLG-SENIN-SYOSIN         
      *
           PERFORM      4401-SINKAI-SET-SEC
               UNTIL   (FLG-SINKAI     =       1      ) OR
                       (IDX1            >       100    )
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SRYACCT-KEY32"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      * 剤番号テーブルに退避した内容から収納マスタのレイアウトに点数を
      * 再セットする（薬剤一部負担金の計算領域もセット）
           MOVE           SPACE               TO    SYU-SKY-NAIYOU
           INITIALIZE                               SYU-SKY-NAIYOU
           PERFORM VARYING IDX1 FROM    1   BY  1
                   UNTIL   IDX1 >       5
               MOVE           ZERO         TO    SYU-JIHI-TAXNASI(IDX1)
               MOVE           ZERO         TO    SYU-JIHI-TAXARI (IDX1)
           END-PERFORM    
           MOVE           ZERO                TO    SYU-JIHI-TOTAL
                                                    SYU-JIHI-TOTAL-TAX
                                                    SYU-JIHI-TAX
                                                    SYU-RJN-FTN
                                                    SYU-KOH-FTN
                                                    SYU-YKZ-FTN
                                                    SYU-HKNTEKMONEY
                                                    SYU-SKYMONEY
      *----(01.00.03) LINE ADD START -------------------------------
           MOVE           ZERO                TO    WRK-TYK-HKNTEN
      *----(01.00.03) LINE ADD END   -------------------------------
           PERFORM VARYING      IDX1    FROM    1    BY    1
                   UNTIL       (IDX1               >   100)
                   OR          (TBL-SRYKBN(IDX1)   =   SPACE)
                               
               EVALUATE    TBL-SRYKBN  (IDX1)
      *            診療
                   WHEN    "11"
                   WHEN    "12"
                    COMPUTE SYU-HKNTEN (01) =  SYU-HKNTEN (01)  +
                           (TBL-ZAITEN (IDX1) *   TBL-ZAIKAISU (IDX1))
                    IF   (TBL-SRYKBN  (IDX1)  =  "11")
                    AND  (FLG-SENIN          =   1)                     
                         MOVE    1     TO     FLG-SENIN-SYOSIN          
                    END-IF                                              
      *            指導
                   WHEN    "13"
                    COMPUTE SYU-HKNTEN (02) =  SYU-HKNTEN (02)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            在宅
                   WHEN    "14"
                    COMPUTE SYU-HKNTEN (03) =  SYU-HKNTEN (03)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            投薬
                   WHEN    "21"   THRU  "27"
      *----(01.00.03) LINE MOD START -------------------------------
      *             COMPUTE SYU-HKNTEN (04) =  SYU-HKNTEN (04)  +
      *                   (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
                    COMPUTE WRK-TYK-HKNTEN  =  WRK-TYK-HKNTEN   +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *----(01.00.03) LINE MOD END   -------------------------------
      *            注射
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                    COMPUTE SYU-HKNTEN (05) =  SYU-HKNTEN (05)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            処置
                   WHEN    "40"
                    COMPUTE SYU-HKNTEN (06)   =  SYU-HKNTEN (06)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            手術
                   WHEN    "50"
                   WHEN    "54"
                    COMPUTE SYU-HKNTEN (07)   =  SYU-HKNTEN (07)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            検査
                   WHEN    "60"
                    COMPUTE SYU-HKNTEN (08)   =  SYU-HKNTEN (08)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            Ｘ線
                   WHEN    "70"
                    COMPUTE SYU-HKNTEN (09)   =  SYU-HKNTEN (09)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
      *            その他
                   WHEN    "80"
                    COMPUTE SYU-HKNTEN (10)   =  SYU-HKNTEN (10)  +
                          (TBL-ZAITEN (IDX1)  *   TBL-ZAIKAISU (IDX1))
               END-EVALUATE
               IF  TBL-SRYKBN(IDX1)       =   "21"   OR  "22"  OR  "23"
                   MOVE    SPA-HOSPID         TO   SRY-HOSPID
      *************MOVE    SPA-NYUGAIKBN      TO   SRY-NYUGAIKBN
                   MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                              TO   SRY-NYUGAIKBN
                   MOVE    SPA-GMN-PTID       TO   SRY-PTID
                   MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                              TO   SRY-SRYKA
                   MOVE    WRK-SAIKEISAN-YMD(1:6)
                                              TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)    TO   SRY-ZAINUM
                   MOVE    SRYACT-REC         TO   MCPDATA-REC
                   MOVE    "DBSELECT"         TO   MCP-FUNC
                   MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC             =    ZERO
                     MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                     PERFORM SINKOU-READ-SEC
                   ELSE
                     MOVE    1                    TO  FLG-SINKOU
                   END-IF
      *----(01.00.04) LINE MOD START -------------------------------
      *            IF      FLG-SINKOU          =   SPACE
                   IF      FLG-SINKOU          =   ZERO
      *----(01.00.04) LINE MOD END   -------------------------------
                       PERFORM  UNTIL
                           (FLG-SINKOU         =  1)              OR
                           (SPA-HOSPID     NOT = SRY-HOSPID)      OR
      *********************(SPA-NYUGAIKBN  NOT = SRY-NYUGAIKBN) OR
                           (SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                           NOT = SRY-NYUGAIKBN) OR 
                           (SPA-GMN-PTID   NOT = SRY-PTID)        OR
                           (SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                           NOT = SRY-SRYKA)       OR
                           (TBL-SRYKBN(IDX1)   NOT = SRY-SRYKBN) OR
                           (TBL-ZAIBAN(IDX1)   NOT = SRY-ZAINUM)
      *                    薬剤セット
      *----(01.00.05) LINE MOD START -------------------------------
      ***                  PERFORM    4402-YAKUZAI-SET-SEC
                           EVALUATE   SYU-INGAISHOHOKBN
                               WHEN   "0"
                                   EVALUATE   SRY-SRYSYUKBN
                                       WHEN   "210"
                                       WHEN   "220"
                                       WHEN   "230"
                                       WHEN   "290"
                                       WHEN   "211"
                                       WHEN   "221"
                                       WHEN   "231"
                                       WHEN   "291"
                                       WHEN   "   "
                                          PERFORM  4402-YAKUZAI-SET-SEC
                                   END-EVALUATE       
                               WHEN   "1"
                                   EVALUATE   SRY-SRYSYUKBN
                                       WHEN   "211"
                                       WHEN   "221"
                                       WHEN   "231"
                                       WHEN   "291"
                                          PERFORM  4402-YAKUZAI-SET-SEC
                                   END-EVALUATE       
                           END-EVALUATE
      *----(01.00.05) LINE MOD END   -------------------------------
                           MOVE   "SRYACT-KEY2"  TO  MCP-FUNC
                           PERFORM    SINKOU-READ-SEC
                       END-PERFORM
                   END-IF
               END-IF
      *----(01.00.07) LINE ADD START -------------------------------
               IF  TBL-SRYKBN(IDX1)       =   "95"   OR  "96"
                   MOVE    SPA-HOSPID         TO   SRY-HOSPID
      *************MOVE    SPA-NYUGAIKBN      TO   SRY-NYUGAIKBN
                   MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                              TO   SRY-NYUGAIKBN
                   MOVE    SPA-GMN-PTID       TO   SRY-PTID
                   MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                              TO   SRY-SRYKA
                   MOVE    WRK-SAIKEISAN-YMD(1:6)
                                              TO   SRY-SRYYM
                   MOVE    TBL-ZAIBAN(IDX1)    TO   SRY-ZAINUM
                   MOVE    SRYACT-REC         TO   MCPDATA-REC
                   MOVE    "DBSELECT"         TO   MCP-FUNC
                   MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC             =    ZERO
                     MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH
                     PERFORM SINKOU-READ-SEC
                   ELSE
                     MOVE    1                    TO  FLG-SINKOU
                   END-IF
                   IF      FLG-SINKOU          =   ZERO
                       PERFORM  UNTIL
                           (FLG-SINKOU       =  1)                   OR
                           (SPA-HOSPID         NOT = SRY-HOSPID)     OR
      *********************(SPA-NYUGAIKBN  NOT = SRY-NYUGAIKBN) OR
                           (SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                               NOT = SRY-NYUGAIKBN)  OR 
                           (SPA-GMN-PTID       NOT = SRY-PTID)       OR
                           (SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                               NOT = SRY-SRYKA)      OR
                           (TBL-SRYKBN(IDX1)   NOT = SRY-SRYKBN)     OR
                           (TBL-ZAIBAN(IDX1)   NOT = SRY-ZAINUM)
      *                    保険適用外金額セット
                           PERFORM    4404-HKNTEKGAI-TBL-SEC
                           MOVE   "SRYACT-KEY2"  TO  ORC-DBPATH
                           PERFORM    SINKOU-READ-SEC
                       END-PERFORM
                   END-IF
               END-IF
      *
               IF  SYU-SYUHKNNUM       =   "971"  OR  "973"
                 IF  TBL-SRYKBN(IDX1)       =   "95"   OR  "96"        
                   CONTINUE                                            
                 ELSE                                                  
                   MOVE    SPA-HOSPID         TO   SRY-HOSPID          
      *************MOVE    SPA-NYUGAIKBN      TO   SRY-NYUGAIKBN       
                   MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                              TO   SRY-NYUGAIKBN
                   MOVE    SPA-GMN-PTID       TO   SRY-PTID            
                   MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                              TO   SRY-SRYKA           
                   MOVE    WRK-SAIKEISAN-YMD(1:6)                      
                                              TO   SRY-SRYYM           
                   MOVE    TBL-ZAIBAN(IDX1)    TO   SRY-ZAINUM         
                   MOVE    SRYACT-REC         TO   MCPDATA-REC         
                   MOVE    "DBSELECT"         TO   MCP-FUNC            
                   MOVE    "SRYACT-KEY2"      TO   ORC-DBPATH          
                   CALL    "ORCMCPSUB"         USING                   
                                               MCPAREA                 
                                               ORCMCPAREA              
                                               MCPDATA-REC             
                   IF      MCP-RC             =    ZERO                
                     MOVE    "SRYACT-KEY2"    TO   ORC-DBPATH          
                     PERFORM SINKOU-READ-SEC                           
                   ELSE                                                
                     MOVE    1                    TO  FLG-SINKOU       
                   END-IF                                              
                   IF      FLG-SINKOU          =   ZERO                
                       PERFORM  UNTIL                                  
                           (FLG-SINKOU       =  1)                   OR
                           (SPA-HOSPID         NOT = SRY-HOSPID)     OR
      *********************(SPA-NYUGAIKBN  NOT = SRY-NYUGAIKBN) OR     
                           (SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                               NOT = SRY-NYUGAIKBN)  OR 
                           (SPA-GMN-PTID       NOT = SRY-PTID)       OR
                           (SPA-NAI-TSRYKA (SPA-GMN-SELBAN)
                                               NOT = SRY-SRYKA)      OR
                           (TBL-SRYKBN(IDX1)   NOT = SRY-SRYKBN)     OR
                           (TBL-ZAIBAN(IDX1)   NOT = SRY-ZAINUM)       
      *                    手技点計算
                           PERFORM    4406-SYUTEN-KEISAN-SEC           
                           MOVE   "SRYACT-KEY2"  TO  MCP-FUNC          
                           PERFORM    SINKOU-READ-SEC                  
                       END-PERFORM                                     
                   END-IF                                              
                 END-IF                                                
               END-IF                                                  
      *----(01.00.07) LINE ADD END   -------------------------------
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               MOVE    "SRYACT-KEY2"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           END-PERFORM
      *----(01.00.07) LINE ADD START -------------------------------
      *    労災保険処理
           IF  SYU-SYUHKNNUM       =   "971"                           
             COMPUTE  SYU-RSISHOSHIN-MONEY  =  WK-RSISHOSHIN     * 10
             IF      SYU-RSISHOSHIN-MONEY     NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISHOSHIN-TENSU
                   COMPUTE WK-RSISHOSHIN-TENSU  =  SYU-RSISHOSHIN-MONEY
                                                   / 10
                   COMPUTE SYU-HKNTEN (01)      =  SYU-HKNTEN (01)   -
                                                   WK-RSISHOSHIN-TENSU
             END-IF
             COMPUTE  SYU-RSISAISHIN-MONEY  =  WK-RSISAISHIN     * 10
             IF      SYU-RSISAISHIN-MONEY     NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISAISHIN-TENSU
                   COMPUTE WK-RSISAISHIN-TENSU  =  SYU-RSISAISHIN-MONEY
                                                    / 10
                   COMPUTE SYU-HKNTEN (01)      =  SYU-HKNTEN (01)   -
                                                   WK-RSISAISHIN-TENSU
             END-IF
             COMPUTE  SYU-RSISDO-MONEY      =  WK-RSISDO         * 10
             IF      SYU-RSISDO-MONEY       NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSISDO-TENSU
                   COMPUTE WK-RSISDO-TENSU   =   SYU-RSISDO-MONEY / 10
                   COMPUTE SYU-HKNTEN (02)   =   SYU-HKNTEN (02)   -
                                              WK-RSISDO-TENSU
             END-IF
             COMPUTE  SYU-RSIETC-MONEY      =  WK-RSIETC         * 10
             IF      SYU-RSIETC-MONEY       NOT =    ZERO
                   MOVE    ZERO           TO  WK-RSIETC-TENSU
                   COMPUTE WK-RSIETC-TENSU   =   SYU-RSIETC-MONEY / 10
                   COMPUTE SYU-HKNTEN (10)   =   SYU-HKNTEN (10)   -
                                                 WK-RSIETC-TENSU
             END-IF
           END-IF                                                      
      *    自賠責保険処理
           IF  SYU-SYUHKNNUM       =   "973"                           
             COMPUTE  SYU-RSISHOSHIN-MONEY  =  WK-RSISHOSHIN     * 10  
             IF      SYU-RSISHOSHIN-MONEY     NOT =    ZERO            
                   MOVE    ZERO           TO  WK-RSISHOSHIN-TENSU      
                   COMPUTE WK-RSISHOSHIN-TENSU  =  SYU-RSISHOSHIN-MONEY
                                                   / 10                
                   COMPUTE SYU-HKNTEN (01)      =  SYU-HKNTEN (01)   - 
                                                   WK-RSISHOSHIN-TENSU 
             END-IF                                                    
             COMPUTE  SYU-RSISAISHIN-MONEY  =  WK-RSISAISHIN     * 10  
             IF      SYU-RSISAISHIN-MONEY     NOT =    ZERO            
                   MOVE    ZERO           TO  WK-RSISAISHIN-TENSU      
                   COMPUTE WK-RSISAISHIN-TENSU  =  SYU-RSISAISHIN-MONEY
                                                    / 10               
                   COMPUTE SYU-HKNTEN (01)      =  SYU-HKNTEN (01)   - 
                                                   WK-RSISAISHIN-TENSU 
             END-IF                                                    
             COMPUTE  SYU-RSISDO-MONEY      =  WK-RSISDO         * 10  
             IF      SYU-RSISDO-MONEY       NOT =    ZERO              
                   MOVE    ZERO           TO  WK-RSISDO-TENSU          
                   COMPUTE WK-RSISDO-TENSU   =   SYU-RSISDO-MONEY / 10 
                   COMPUTE SYU-HKNTEN (02)   =   SYU-HKNTEN (02)   -   
                                              WK-RSISDO-TENSU          
             END-IF                                                    
             COMPUTE  SYU-RSIETC-MONEY      =  WK-RSIETC         * 10  
             IF      SYU-RSIETC-MONEY       NOT =    ZERO              
                   MOVE    ZERO           TO  WK-RSIETC-TENSU          
                   COMPUTE WK-RSIETC-TENSU   =   SYU-RSIETC-MONEY / 10 
                   COMPUTE SYU-HKNTEN (10)   =   SYU-HKNTEN (10)   -   
                                                 WK-RSIETC-TENSU       
             END-IF                                                    
      *
             COMPUTE   LNK-S01-SYUTEN  =   WK-SYUTEN-KEI               
                                       -   WK-RSISHOSHIN               
                                       -   WK-RSISAISHIN               
                                       -   WK-RSISDO                   
                                       -   WK-RSIETC                   
                                       -   WK-YOUBUKASAN
                                       -   WK-KYOBUKASAN
                                       -   WK-SOGAIKASAN
                                       -   WK-RYOYOTEATE
      *
             COMPUTE   LNK-S01-SINDAN  =  WK-SINDAN  *  10
             COMPUTE   LNK-S01-MEISAI  =  WK-MEISAI  *  10
      *
             MOVE      WK-RYOYOTEATE   TO   LNK-S01-RYOYO
      *
           END-IF                                                      
      *----(01.00.03) LINE ADD END -------------------------------
      *
      *----(01.00.03) LINE ADD START -------------------------------
           MOVE    WRK-TYK-HKNTEN      TO  SYU-HKNTEN (04)
      *----(01.00.03) LINE ADD END -------------------------------
      *----(01.00.07) LINE ADD START -------------------------------
      *    保険適用外金額の集計テーブルから収納テーブルへのセット
           PERFORM    4405-HKNTEKGAI-SET-SEC
      *----(01.00.07) LINE ADD END   -------------------------------
           PERFORM   VARYING   IDX1     FROM    1   BY  1
                     UNTIL     IDX1         >   11
               COMPUTE   SYU-HKNTEN (12)  =   SYU-HKNTEN (12)
                                          +   SYU-HKNTEN (IDX1)
           END-PERFORM
      *
      *    福岡対応
           IF      SPA-PREFNUM         =   40
               PERFORM    VARYING   IDX   FROM  1   BY  1
                          UNTIL     IDX   >     6
                   MOVE   WRK40-KYUFUGAI-TEN(IDX)  TO
                                            SYU-KYUFUGAI-TEN(IDX)
               END-PERFORM
           END-IF
      *
      *    患者負担額計算処理
           MOVE    "1"                 TO  LNK-S01-SAIKEISAN-KBN
           MOVE    SYU-DENPNUM         TO  LNK-S01-SAIKEISAN-DENPYO
           IF      FLG-SENIN-SYOSIN   =   1                             
               MOVE       "1"          TO  LNK-S01-SENINFLG             
           END-IF                                                       
           MOVE    SYUNOU-REC          TO      LNK-SYUNOU-REC
      *    CALL    "ORCS01NEW"         USING   MCPAREA
           CALL    "ORCS01"            USING   MCPAREA
                                               LNK-ORCS01-REC
                                               LNK-SYUNOU-REC
           MOVE    LNK-SYUNOU-REC      TO      SYUNOU-REC
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID            TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
      *                                 
           MOVE    LNK-SYUNOU-REC      TO      SYUNOU-REC
           MOVE    SYU-SKYMONEY        TO      WRK-ATO-SEIKYUGK
      *    伝票状態区分の再セット
           IF      SYU-SKYMONEY        =       ZERO
                   MOVE    SPACE       TO      SYU-DENPJTIKBN
           ELSE
               IF      SYU-SKYMONEY    >       SYU-NYUKIN-TOTAL
                       MOVE    "2"         TO      SYU-DENPJTIKBN
               ELSE    
                       MOVE    "1"         TO      SYU-DENPJTIKBN
               END-IF
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
      *    会計変更区分の初期化
           MOVE    SPACE               TO  SYU-ACCT-UPDKBN
      *
      *    収納マスタの更新
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
      *    再計算分の収納明細マスタ作成
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID            TO  SMEI-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =    ZERO
                 MOVE    "SYUMEI-KEY2"      TO  ORC-DBPATH
                 PERFORM 900-SYUMEI-READ-SEC
           ELSE
                 MOVE    1                    TO  FLG-SYUMEI
           END-IF
           IF      FLG-SYUMEI    =     1
                 CONTINUE
           ELSE
                 PERFORM   VARYING  IDX1  FROM   1  BY   1
                     UNTIL (FLG-SYUMEI   =      1 )
                            MOVE      "SYUMEI-KEY2"  TO  ORC-DBPATH
                            PERFORM   900-SYUMEI-READ-SEC
                 END-PERFORM
                 PERFORM    4403-SMEI-SAKUSEI-SEC
                 MOVE    "DBCLOSE"           TO  MCP-FUNC
                 MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"          USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
           END-IF
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
      *    診療会計マスタの取込み
      **************************************************************
       4401-SINKAI-SET-SEC           SECTION.
      *
           IF    (ACCT-ZAITEN            NOT = ZERO  )           AND
                 (ACCT-SRYYM(1:6)    =   WRK-SAIKEISAN-YMD(1:6)) AND
                 (ACCT-HKNCOMBI      =  SYU-HKNCOMBINUM)         AND
                 (ACCT-DAY(SAI-IDX WRK-SAIKEISAN-DD)  NOT = ZERO)
      *
             MOVE    ACCT-SRYKBN   TO  TBL-SRYKBN(IDX1)
             MOVE    ACCT-ZAINUM   TO  TBL-ZAIBAN(IDX1)
             MOVE    ACCT-DAY(SAI-IDX WRK-SAIKEISAN-DD)
                                   TO  TBL-ZAIKAISU(IDX1)
      *----(01.00.03) LINE MOD START -------------------------------
      *      MOVE    ACCT-ZAITEN   TO  TBL-ZAITEN(IDX1)
             IF     (ACCT-SRYCDTOTAL   =    630010002)   AND
                    (ACCT-MEISAISU     =    1)
                 COMPUTE   TBL-ZAITEN(IDX1)  =  TBL-ZAITEN(IDX1) +
                                              (ACCT-ZAITEN * -1)
             ELSE
                 MOVE  ACCT-ZAITEN   TO  TBL-ZAITEN(IDX1)
             END-IF 
             IF   (   ACCT-SRYKBN       =   "95"  OR  "96"   )         
             OR   ((  ACCT-SRYCDTOTAL   =    630010002      )  AND     
                   (  ACCT-MEISAISU     =    1              ))         
                CONTINUE                                               
             ELSE                                                      
                MOVE   ACCT-SYUTEN1  TO  TBL-SYUTEN(IDX1)              
                IF  (SYU-SYUHKNNUM       =   "973")
                AND (ACCT-SRYKBN         =   "70")
                    COMPUTE  TBL-SYUTEN(IDX1) =  TBL-SYUTEN(IDX1)
                                              -  ACCT-SYUTEN2
                END-IF
             END-IF                                                    
      *----(01.00.03) LINE MOD END   -------------------------------
             ADD     1             TO  IDX1
      * 
           END-IF
           MOVE    "SRYACCT-KEY32"        TO  ORC-DBPATH
           PERFORM SINKAI-NEXT-SEC
           .
       4401-SINKAI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    薬剤コードのセット
      **************************************************************
       4402-YAKUZAI-SET-SEC           SECTION.
      *
           PERFORM     VARYING    IDX3    FROM    1     BY    1
                         UNTIL   (IDX3              >   5    )    OR
                                 (SRY-SRYCD(IDX3)   =   SPACE)
      *----(01.00.02) LINE MOD START -------------------------------
      *        MOVE    SRY-SRYKBN          TO  LNK-S01-SRYKBN (1 IDX2)
      *        MOVE    SRY-ZAINUM          TO  LNK-S01-ZAIBAN (1 IDX2)
      *        MOVE    SRY-SRYCD(IDX3)      TO  LNK-S01-SRYCD  (1 IDX2)
      *        MOVE    SRY-SRYSURYO(IDX3)   TO  LNK-S01-SURYOU (1 IDX2)
      *        MOVE    TBL-ZAIKAISU(IDX1)   TO  LNK-S01-KAISUU (1 IDX2)
      *        MOVE    TBL-ZAITEN(IDX1)     TO  LNK-S01-TENSUU (1 IDX2)
               IF     (SRY-AUTOKBN(IDX3)    =   SPACE)     AND
      *----(01.00.06) LINE ADD START -------------------------------
                      (SRY-SRYCD(IDX3)(1:1) =   "6"  )
      *----(01.00.06) LINE ADD END   -------------------------------
                   ADD     1              TO       IDX2
                   MOVE    SRY-SRYKBN         TO  LNK-S01-SRYKBN(1 IDX2)
                   MOVE    SRY-ZAINUM         TO  LNK-S01-ZAIBAN(1 IDX2)
                   MOVE    SRY-SRYCD(IDX3)    TO  LNK-S01-SRYCD (1 IDX2)
                   MOVE    SRY-SRYSURYO(IDX3) TO  LNK-S01-SURYOU(1 IDX2)
                   MOVE    TBL-ZAIKAISU(IDX1) TO  LNK-S01-KAISUU(1 IDX2)
                   MOVE    TBL-ZAITEN(IDX1)   TO  LNK-S01-TENSUU(1 IDX2)
               END-IF
      *----(01.00.02) LINE MOD END   -------------------------------
           END-PERFORM
           .
       4402-YAKUZAI-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    再計算処理（収納明細レコード作成）
      *************************************************************
       4403-SMEI-SAKUSEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SMEI-PTID
      *伝票番号
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
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
           IF      WRK-ATO-SEIKYUGK    NOT =   WRK-MAE-SEIKYUGK
                   COMPUTE   SMEI-SKYMONEY =   WRK-ATO-SEIKYUGK   -
                                               WRK-MAE-SEIKYUGK
           ELSE
                   MOVE    ZERO        TO  SMEI-SKYMONEY
           END-IF
      *入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    SPA-SYSYMD          TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "3"                 TO  SMEI-JOUTAIKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       4403-SMEI-SAKUSEI-EXT.
           EXIT.
      *
      **************************************************************
      *    保険適用外のテーブル領域セット
      **************************************************************
       4404-HKNTEKGAI-TBL-SEC          SECTION.
      *
           EVALUATE    SRY-SRYKBN
               WHEN    "95"
                   PERFORM     VARYING    IDX3    FROM    1     BY    1
                                 UNTIL   (IDX3         >   5    )   OR
       	                                  (SRY-SRYCD(IDX3)  =   SPACE)
                       IF    SRY-SRYCD (IDX3) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDX3) (4:2)    =   "00"
                                                     OR       "91"     
                                                     OR       "92"     
                                                     OR       "93"
                                                     OR       "94"
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                               MOVE    WRK-SAIKEISAN-YMD TO  ORC-DBYMD
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-GAITENTTLKBN
                                       WHEN    1
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXNASI(1)
                                          COMPUTE  TBL-JIHI-TAXNASI(1)
                                               =   TBL-JIHI-TAXNASI(1)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    2
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXNASI(2)
                                          COMPUTE  TBL-JIHI-TAXNASI(2)
                                               =   TBL-JIHI-TAXNASI(2)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    3
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXNASI(3)
                                          COMPUTE  TBL-JIHI-TAXNASI(3)
                                               =   TBL-JIHI-TAXNASI(3)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    4
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXNASI(4)
                                          COMPUTE  TBL-JIHI-TAXNASI(4)
                                               =   TBL-JIHI-TAXNASI(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    5
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXNASI(5)
                                          COMPUTE  TBL-JIHI-TAXNASI(5)
                                               =   TBL-JIHI-TAXNASI(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDX3) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(1)
                                       COMPUTE  TBL-TGMONEY(1)
                                            =   TBL-TGMONEY(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(2)
                                       COMPUTE  TBL-TGMONEY(2)
                                            =   TBL-TGMONEY(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(3)
                                       COMPUTE  TBL-TGMONEY(3)
                                            =   TBL-TGMONEY(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(4)
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(5)
                                       COMPUTE  TBL-TGMONEY(5)
                                            =   TBL-TGMONEY(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(6)
                                       COMPUTE  TBL-TGMONEY(6)
                                            =   TBL-TGMONEY(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                   WHEN   "54"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(7)
                                       COMPUTE  TBL-TGMONEY(7)
                                            =   TBL-TGMONEY(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(8)
                                       COMPUTE  TBL-TGMONEY(8)
                                            =   TBL-TGMONEY(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(9)
                                       COMPUTE  TBL-TGMONEY(9)
                                            =   TBL-TGMONEY(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(10)
                                       COMPUTE  TBL-TGMONEY(10)
                                            =   TBL-TGMONEY(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                           MOVE    WRK-SAIKEISAN-YMD   TO   ORC-DBYMD
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(1)
                                       COMPUTE  TBL-TGMONEY(1)
                                            =   TBL-TGMONEY(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(2)
                                       COMPUTE  TBL-TGMONEY(2)
                                            =   TBL-TGMONEY(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(3)
                                       COMPUTE  TBL-TGMONEY(3)
                                            =   TBL-TGMONEY(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(4)
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(5)
                                       COMPUTE  TBL-TGMONEY(5)
                                            =   TBL-TGMONEY(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(6)
                                       COMPUTE  TBL-TGMONEY(6)
                                            =   TBL-TGMONEY(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                   WHEN   "54"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(7)
                                       COMPUTE  TBL-TGMONEY(7)
                                            =   TBL-TGMONEY(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(8)
                                       COMPUTE  TBL-TGMONEY(8)
                                            =   TBL-TGMONEY(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(9)
                                       COMPUTE  TBL-TGMONEY(9)
                                            =   TBL-TGMONEY(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(10)
                                       COMPUTE  TBL-TGMONEY(10)
                                            =   TBL-TGMONEY(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(4)
                                       COMPUTE  TBL-TGMONEY(4)
                                            =   TBL-TGMONEY(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                            TO   TBL-TGMONEY(10)
                                       COMPUTE  TBL-TGMONEY(10)
                                            =   TBL-TGMONEY(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
               WHEN    "96"
                   PERFORM     VARYING    IDX3    FROM   1    BY   1
                                 UNTIL   (IDX3        >   5    )   OR
                                         (SRY-SRYCD(IDX3)  =   SPACE)
                       IF    SRY-SRYCD (IDX3) (1:3)   =   "095"
                                                     OR  "096"
                           IF    SRY-SRYCD (IDX3) (4:2)    =   "00"
                                                     OR       "91"     
                                                     OR       "92"     
                                                     OR       "93"
                                                     OR       "94"
                               MOVE    SPACE         TO   TNS-TENSU-REC
                               INITIALIZE                 TNS-TENSU-REC
                               MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                               MOVE    WRK-SAIKEISAN-YMD TO  ORC-DBYMD
                               PERFORM    TENSU-READ-SEC
                               IF      MCP-RC         =    ZERO
                                   EVALUATE    TNS-GAITENTTLKBN
                                       WHEN    1
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXARI(1)
                                          COMPUTE  TBL-JIHI-TAXARI(1)
                                               =   TBL-JIHI-TAXARI(1)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    2
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXARI(2)
                                          COMPUTE  TBL-JIHI-TAXARI(2)
                                               =   TBL-JIHI-TAXARI(2)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    3
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXARI(3)
                                          COMPUTE  TBL-JIHI-TAXARI(3)
                                               =   TBL-JIHI-TAXARI(3)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    4
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXARI(4)
                                          COMPUTE  TBL-JIHI-TAXARI(4)
                                               =   TBL-JIHI-TAXARI(4)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                       WHEN    5
      *                                    ADD   SRY-JIHIMONEY(IDX3)
      *                                       TO   TBL-JIHI-TAXARI(5)
                                          COMPUTE  TBL-JIHI-TAXARI(5)
                                               =   TBL-JIHI-TAXARI(5)
                                               +  (SRY-JIHIMONEY(IDX3)
                                                 * TBL-ZAIKAISU(IDX1)) 
                                   END-EVALUATE
                               END-IF
                           ELSE
                               EVALUATE   SRY-SRYCD (IDX3) (4:2)
                                   WHEN   "11"
                                   WHEN   "12"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(1)
                                       COMPUTE  TBL-TGMONEY-TAX(1)
                                            =   TBL-TGMONEY-TAX(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(2)
                                       COMPUTE  TBL-TGMONEY-TAX(2)
                                            =   TBL-TGMONEY-TAX(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(3)
                                       COMPUTE  TBL-TGMONEY-TAX(3)
                                            =   TBL-TGMONEY-TAX(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(4)
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(5)
                                       COMPUTE  TBL-TGMONEY-TAX(5)
                                            =   TBL-TGMONEY-TAX(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(6)
                                       COMPUTE  TBL-TGMONEY-TAX(6)
                                            =   TBL-TGMONEY-TAX(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                   WHEN   "54"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(7)
                                       COMPUTE  TBL-TGMONEY-TAX(7)
                                            =   TBL-TGMONEY-TAX(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(8)
                                       COMPUTE  TBL-TGMONEY-TAX(8)
                                            =   TBL-TGMONEY-TAX(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(9)
                                       COMPUTE  TBL-TGMONEY-TAX(9)
                                            =   TBL-TGMONEY-TAX(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO TBL-TGMONEY-TAX(10)
                                       COMPUTE  TBL-TGMONEY-TAX(10)
                                            =   TBL-TGMONEY-TAX(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                               END-EVALUATE
                           END-IF
                       ELSE
                           MOVE    SPACE          TO   TNS-TENSU-REC
                           INITIALIZE                  TNS-TENSU-REC
                           MOVE    SRY-SRYCD(IDX3) TO   TNS-SRYCD
                           MOVE    WRK-SAIKEISAN-YMD   TO   ORC-DBYMD
                           PERFORM    TENSU-READ-SEC
                           IF      MCP-RC         =    ZERO
                               EVALUATE   TNS-SRYKBN
                                   WHEN   "11"
                                   WHEN   "12"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(1)
                                       COMPUTE  TBL-TGMONEY-TAX(1)
                                            =   TBL-TGMONEY-TAX(1)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "13"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(2)
                                       COMPUTE  TBL-TGMONEY-TAX(2)
                                            =   TBL-TGMONEY-TAX(2)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "14"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(3)
                                       COMPUTE  TBL-TGMONEY-TAX(3)
                                            =   TBL-TGMONEY-TAX(3)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "21"   THRU  "27"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(4)
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "31"
                                   WHEN   "32"
                                   WHEN   "33"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(5)
                                       COMPUTE  TBL-TGMONEY-TAX(5)
                                            =   TBL-TGMONEY-TAX(5)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "40"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(6)
                                       COMPUTE  TBL-TGMONEY-TAX(6)
                                            =   TBL-TGMONEY-TAX(6)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "50"
                                   WHEN   "54"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(7)
                                       COMPUTE  TBL-TGMONEY-TAX(7)
                                            =   TBL-TGMONEY-TAX(7)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "60"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(8)
                                       COMPUTE  TBL-TGMONEY-TAX(8)
                                            =   TBL-TGMONEY-TAX(8)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "70"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO  TBL-TGMONEY-TAX(9)
                                       COMPUTE  TBL-TGMONEY-TAX(9)
                                            =   TBL-TGMONEY-TAX(9)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   "80"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                          TO TBL-TGMONEY-TAX(10)
                                       COMPUTE  TBL-TGMONEY-TAX(10)
                                            =   TBL-TGMONEY-TAX(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                   WHEN   OTHER
                                     IF   TNS-SRYCD(1:1)    =   "6"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                        TO   TBL-TGMONEY-TAX(4)
                                       COMPUTE  TBL-TGMONEY-TAX(4)
                                            =   TBL-TGMONEY-TAX(4)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                                     IF   TNS-SRYCD(1:1)    =   "7"
      *                                ADD    SRY-JIHIMONEY(IDX3)
      *                                        TO   TBL-TGMONEY-TAX(10)
                                       COMPUTE  TBL-TGMONEY-TAX(10)
                                            =   TBL-TGMONEY-TAX(10)
                                            +  (SRY-JIHIMONEY(IDX3)
                                              * TBL-ZAIKAISU(IDX1)) 
                                     END-IF
                               END-EVALUATE
                           END-IF
                       END-IF
                   END-PERFORM
           END-EVALUATE
           .
       4404-HKNTEKGAI-TBL-EXT.
           EXIT.
      *
      **************************************************************
      *    保険適用外金額の集計テーブルから収納テーブルへのセット
      **************************************************************
       4405-HKNTEKGAI-SET-SEC          SECTION.
      *
      *  保険適用外
           PERFORM     VARYING   IDX3    FROM   1    BY    1
                         UNTIL   IDX3    >      11
               MOVE   TBL-TGMONEY(IDX3)        TO  SYU-TGMONEY(IDX3)
               MOVE   TBL-TGMONEY-TAX(IDX3)    TO  SYU-TGMONEY-TAX(IDX3)
           END-PERFORM
      *
      *  自費１から５
           PERFORM     VARYING   IDX3    FROM    1    BY    1
                         UNTIL   IDX3    >     5
              MOVE   TBL-JIHI-TAXNASI(IDX3)    TO SYU-JIHI-TAXNASI(IDX3)
              MOVE   TBL-JIHI-TAXARI(IDX3)     TO SYU-JIHI-TAXARI(IDX3)
           END-PERFORM
           .
       4405-HKNTEKGAI-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    負担金額の手技点算出
      **************************************************************
       4406-SYUTEN-KEISAN-SEC          SECTION.
      *
           IF    SRY-RENNUM     =     1                                
      *                                                                
               ADD  TBL-SYUTEN(IDX1)    TO  WK-SYUTEN-KEI              
      *                                                                
           END-IF                                                      
      *                                                                
           PERFORM   VARYING   IDX8   FROM   1   BY   1                
                       UNTIL    (IDX8            >       5)  OR        
                                (SRY-SRYCD(IDX8) =   SPACE)            
      *                                                                
                    MOVE    SPACE             TO   TNS-TENSU-REC       
                    INITIALIZE                     TNS-TENSU-REC       
                    MOVE    SRY-SRYCD(IDX8)   TO   TNS-SRYCD           
                    MOVE    WRK-SAIKEISAN-YMD TO   ORC-DBYMD           
                    PERFORM    TENSU-READ-SEC                          
                    IF      MCP-RC         =    ZERO                   
      *                                                                
                         COMPUTE  WRK-TEN     =   TNS-TEN  /  10       
      *                                                                
                         IF   SRY-SRYCD(IDX8) =  "101110010"  OR       
                                                 "101110020"  OR       
                                                 "101110030"           
                           COMPUTE WK-RSISHOSHIN = WK-RSISHOSHIN +     
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))   
                         END-IF                                        
                         IF   SRY-SRYCD(IDX8) =  "101120030"  OR       
                                                 "101120040"  OR       
                                                 "101120050"  OR       
                                                 "101120060"           
                           COMPUTE WK-RSISAISHIN = WK-RSISAISHIN +     
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))   
                         END-IF                                        
                         IF   SRY-SRYCD(IDX8) =  "101130010"  OR       
                                                 "101130020"  OR       
                                                 "101130030"  OR       
                                                 "101130040"  OR       
                                                 "101130050"  OR       
                                                 "101130060"  OR       
                                                 "101130070"  OR       
                                                 "101130080"  OR       
                                                 "101130090"  OR       
                                                 "101130100"           
                           COMPUTE WK-RSISDO = WK-RSISDO +             
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))   
                         END-IF                                        
                         IF    SRY-SRYCD (IDX8)(1:5)   =   "09591"     
                                                     OR    "09592"     
                                                     OR    "09593"
                           COMPUTE WK-RSIETC = WK-RSIETC +             
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))   
                         END-IF                                        
                         IF    SRY-SRYCD (IDX8)(1:5)   =   "09591"      
                           COMPUTE WK-SINDAN = WK-SINDAN +              
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))    
                         END-IF                                         
                         IF    SRY-SRYCD (IDX8)(1:5)   =   "09592"      
                           COMPUTE WK-MEISAI = WK-MEISAI +              
                                  (WRK-TEN  *   TBL-ZAIKAISU (IDX1))    
                         END-IF                                         
      *
                         IF    SRY-SRYCD (IDX8)  =   "140037490"
                           COMPUTE WK-YOUBUKASAN = WK-YOUBUKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
                         IF    SRY-SRYCD (IDX8)  =   "140040110"
                           COMPUTE WK-KYOBUKASAN = WK-KYOBUKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
                         IF    SRY-SRYCD (IDX8)  =   "150266970"
                           COMPUTE WK-SOGAIKASAN = WK-SOGAIKASAN +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
      *
                         IF    SRY-SRYCD (IDX8)  =   "199000610"
                           COMPUTE WK-RYOYOTEATE = WK-RYOYOTEATE +
                                  (TNS-TEN  *   TBL-ZAIKAISU (IDX1))
                         END-IF
      *                                                                
                    END-IF                                             
      *                                                                
           END-PERFORM                                                 
           .                                                           
       4406-SYUTEN-KEISAN-EXT.
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
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC                            
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
                MOVE    "DBUPDATE"          TO  MCP-FUNC
                MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1002"              TO  SPA-ERRCD
                   GO  TO  450-HENKIN-EXT
                END-IF     
      *
                INITIALIZE                      SYUMEI-REC
                MOVE    SPA-HOSPID          TO  SMEI-HOSPID
      **********MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
                MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                            TO  SMEI-NYUGAIKBN
                MOVE    SPA-GMN-PTID        TO  SMEI-PTID
                MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                            TO  SMEI-DENPNUM
                MOVE    SYUMEI-REC          TO  MCPDATA-REC
                MOVE    "DBSELECT"          TO  MCP-FUNC
                MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
                IF      MCP-RC              =    ZERO
                    MOVE    "SYUMEI-KEY2"   TO   ORC-DBPATH
                    PERFORM 900-SYUMEI-READ-SEC
                ELSE
                    MOVE    1                    TO  FLG-SYUMEI
                END-IF
      *
                PERFORM   VARYING  IDX1  FROM   1  BY   1
                    UNTIL  (FLG-SYUMEI   =      1)
                            MOVE   "SYUMEI-KEY2"  TO  ORC-DBPATH  
                            PERFORM   900-SYUMEI-READ-SEC
                END-PERFORM
      *
                MOVE    "DBCLOSE"           TO  MCP-FUNC
                MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
                CALL    "ORCMCPSUB"          USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
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
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
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
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
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
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
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
                  MOVE    "DBUPDATE"          TO  MCP-FUNC
                  MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                  CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                  INITIALIZE                      SYUMEI-REC
                  MOVE    SPA-HOSPID          TO  SMEI-HOSPID
      **********  MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
                  MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                              TO  SMEI-NYUGAIKBN
                  MOVE    SPA-GMN-PTID        TO  SMEI-PTID
                  MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN)
                                              TO  SMEI-DENPNUM
                  MOVE    SYUMEI-REC          TO  MCPDATA-REC
                  MOVE    "DBSELECT"          TO  MCP-FUNC
                  MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
                  CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                  IF      MCP-RC              =    ZERO
                      MOVE    "SYUMEI-KEY2"   TO   ORC-DBPATH
                      PERFORM 900-SYUMEI-READ-SEC
                  ELSE
                      MOVE    1                    TO  FLG-SYUMEI
                  END-IF
      *
                  PERFORM   VARYING  IDX1  FROM   1  BY   1
                      UNTIL  (FLG-SYUMEI   =      1)
                              MOVE   "SYUMEI-KEY2"  TO  ORC-DBPATH  
                              PERFORM   900-SYUMEI-READ-SEC
                  END-PERFORM
      *
                  MOVE    "DBCLOSE"           TO  MCP-FUNC
                  MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
                  CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
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
           MOVE    SPA-HOSPID          TO  SMEI-HOSPID
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
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
      *収納請求額のチェックを行う
           MOVE    SPA-HOSPID          TO  SYU-HOSPID
      *****MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-TNYUGAIKBN (SPA-GMN-SELBAN)
                                       TO  SYU-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  SYU-PTID
           MOVE    SPA-GMN-TDENPNUM (SPA-GMN-SELBAN) (1:7)
                                       TO  SYU-DENPNUM
           PERFORM 900-SYUNOU-READ-SEC
      *     
           IF      FLG-SYUNOU          =   1
                   CONTINUE
           ELSE
      *収納請求額がゼロならばエラー
             IF    SYU-SKYMONEY      =  ZERO
                   MOVE    "0007"     TO    SPA-ERRCD
                   GO    TO    490-RIREKI-EXT
             END-IF
      *収納伝票状態のチェック（請求取消）はエラー
             IF    SYU-DENPJTIKBN     =     "3"
                   MOVE    "0007"     TO    SPA-ERRCD
                   GO    TO    490-RIREKI-EXT
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
           INITIALIZE                  SPA-S02-AREA
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-SRYYM
               MOVE    SPA-SYSYMD  (1:6)   TO  SPA-NAI-SRYYM
           ELSE
               MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SPA-SRYYMD (1:6)    TO  SPA-NAI-SRYYM
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
               WHEN    "0005"
                   MOVE    "請求取消済みの為、再計算はできません"
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
      **************************************************************
      *    患者マスター読込（主キー）
      **************************************************************
       900-PTINF-READ-SEC         SECTION.
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    診療行為マスター読込
      **************************************************************
       SINKOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SINKOU
           ELSE
               MOVE    1               TO  FLG-SINKOU
           END-IF
      *
           .
       SINKOU-READ-EXT.
           EXIT.
      **************************************************************
      *    診療会計マスター読込
      **************************************************************
       SINKAI-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SINKAI
           ELSE
               MOVE    1               TO  FLG-SINKAI
           END-IF
      *
           .
       SINKAI-NEXT-EXT.
           EXIT.
      *
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *     
      **************************************************************
      *    収納マスター読込（主キー）
      **************************************************************
       900-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =    ZERO
               MOVE    "SYUNOU-KEY"         TO  ORC-DBPATH
               PERFORM 900-SYUNOU-NEXT-SEC
           ELSE
               MOVE    1                    TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *     
      *
      **************************************************************
      *    収納マスター読込（ＮＥＸＴ）
      **************************************************************
       900-SYUNOU-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =  ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO FLG-SYUNOU
           ELSE
               MOVE    1               TO FLG-SYUNOU
           END-IF 
      *
           .
       900-SYUNOU-NEXT-EXT.
           EXIT.
      **************************************************************
      *    保険組合せマスター読込
      **************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNKUMI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNKUMI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    収納明細読込
      **************************************************************
       900-SYUMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
               MOVE    ZERO            TO  FLG-SYUMEI
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    保険番号マスター読込
      **************************************************************
       950-HKNMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNMST
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNMST
           END-IF
      *
           .
       950-HKNMST-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    受診履歴マスター読込
      **************************************************************
       960-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       960-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込（主キー）
      *****************************************************************
       TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴読込
      *****************************************************************
       980-PTTEIKIRRK-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    ZERO            TO  FLG-PTTEIKIRRK
           ELSE
               INITIALIZE                  PTTEIKIRRK-REC
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       980-PTTEIKIRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    定期請求履歴読込
      *****************************************************************
       980-TEIKIRRK-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TEIKIRRK-REC
               MOVE    ZERO            TO  FLG-TEIKIRRK
           ELSE
               INITIALIZE                  TEIKIRRK-REC
               MOVE    1               TO  FLG-TEIKIRRK
           END-IF
      *
           .
       980-TEIKIRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-TBL-SELECT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               MOVE    WRK-DBPATH  TO  ORC-DBPATH
               MOVE    "DBFETCH"   TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-DBRC
               END-IF
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       900-TBL-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル更新処理
      *****************************************************************
       900-TBL-UPDATE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル追加処理
      *****************************************************************
       900-TBL-INSERT-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル削除処理
      *****************************************************************
       900-TBL-DELETE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DBRC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-DBRC
           END-IF
      *
           .
       900-TBL-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-TBL-CLOSE-EXT.
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
      *
      **************************************************************
      *    ＰＵＴ　処理
      **************************************************************
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
