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
      ************************************************************
      *    労災入院会計作成用パラメタ　　　COPY (CPORCSROUNYUIN.INC)
      *                                使用プログラム (ORCSROUNYUIN)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      *      保険組合せ
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.01     NACL-門脇    10/01/18  包括点数の治験減点分追加
      ************************************************************
       01  ORCSROUNYUIN-AREA.
      *
      *医療機関ＩＤ
           03  LNK-SROUNYUIN-HOSPNUM                 PIC 9(2).
      *患者ＩＤ
           03  LNK-SROUNYUIN-PTID                    PIC  9(10).
      *算定年月
           03  LNK-SROUNYUIN-SANTEIYM                PIC  X(06).
      *保険組合せ
           03  LNK-SROUNYUIN-HKNCOMBI                PIC  9(04).
      *略称編集区分（空白：しない　１：する）
           03  LNK-SROUNYUIN-RYAKUKBN                PIC  X(01).
      *労災乗数区分（１：２週間以内　２：２週間越え）
           03  LNK-SROUNYUIN-JOUSU-KBN               PIC  X(01)
                                                     OCCURS 31.
      *返却項目
           03  LNK-SROUNYUIN-HENKYAKU-AREA.
      *返却テーブル
               05  LNK-SROUNYUIN-HENKYAKU-TBL        OCCURS  31.
      *労災入院フラグ（空白：労災入院なし　１：労災入院中）
                   07  LNK-SROUNYUIN-NYUIN-FLG       PIC  X(01).
      *外泊区分
                   07  LNK-SROUNYUIN-GAIHAKU         PIC  9(01).
      *入院基本料診療区分
                   07  LNK-SROUNYUIN-SRYKBN          PIC  X(02).
      *剤点数
                   07  LNK-SROUNYUIN-ZAITEN          PIC S9(08).
      *診療コード計
                   07  LNK-SROUNYUIN-SRYCDTOTAL      PIC  9(14).
      *明細数
                   07  LNK-SROUNYUIN-MEISAISU        PIC  9(07).
      *診療コード
                   07  LNK-SROUNYUIN-ROUSAI          OCCURS  20.
                       09  LNK-SROUNYUIN-SRYCD       PIC  X(09).
                       09  LNK-SROUNYUIN-SRYCD-SK    PIC  X(01).
      *略称（特定入院料）
                   07  LNK-SROUNYUIN-RYAKUSYO        PIC  X(14).
      *入院会計剤識別区分６の剤点数
                   07  LNK-SROUNYUIN-ZAITEN-NACCT    PIC  9(08).
      *診療会計９０・９２の剤点数
                   07  LNK-SROUNYUIN-ZAITEN-ACCT     PIC  9(08).
      *診療会計９０・９２の剤点数（その他）
                   07  LNK-SROUNYUIN-ZAITEN-ACCTETC  PIC  9(08).
      *包括点数の治験減点分
                   07  LNK-SROUNYUIN-GENTEN          PIC  9(08).
      *
      *略称（一般入院料・加算）
               05  LNK-SROUNYUIN-RYAKUSYO2           PIC  X(14)
                                                     OCCURS 20.
      *
      *注加算扱いの入院料加算
               05  LNK-SROUNYUIN-CHUKSNSRYCD         PIC  X(09)
                                                     OCCURS 50.
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー　３：労災入院なし）
           03  LNK-SROUNYUIN-RC                      PIC S9(9) BINARY.
      *
