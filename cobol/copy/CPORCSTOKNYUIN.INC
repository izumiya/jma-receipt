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
      *    特定入院料コード取得パラメタ　　COPY (CPORCSTOKNYUIN.INC)
      *                                使用プログラム (ORCSTOKNYUIN)
      *    設定項目
      *      特定入院料区分
      *      特定入院料
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTOKNYUIN-AREA.
      *
      *特定入院料区分（１：医療機関／２：病棟／３：病室）
           03  LNK-TOKNYUIN-KBN                       PIC  X(01).
      *特定入院料
           03  LNK-TOKNYUIN-NYUIN                     PIC  X(02).
      *算定年月
           03  LNK-TOKNYUIN-SANTEIYM                  PIC  X(06).
      *病棟番号
           03  LNK-TOKNYUIN-BTUNUM                    PIC  X(02).
      *返却項目
           03  LNK-TOKNYUIN-HENKYAKU-AREA.
      *特定入院料コード
               05  LNK-TOKNYUIN-SRYCD                 PIC  X(09).
      *復帰コード（０：特定入院料返却あり　１：特定入院料返却なし）
           03  LNK-TOKNYUIN-RC                     PIC S9(9) BINARY.
      *
