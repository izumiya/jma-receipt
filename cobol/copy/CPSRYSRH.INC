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
      *    診療行為照会テーブル      COPY (CPSRYSRH.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SRYSRH-REC.
      *医療機関番号
           03  SRYSRH-HOSPNUM      PIC 9(2).
      *端末ＩＤ
           03  SRYSRH-TERMID       PIC X(64).
      *患者ＩＤ
           03  SRYSRH-PTID         PIC 9(10).
      *入外区分
           03  SRYSRH-NYUGAIKBN    PIC X(1).
      *診療科
           03  SRYSRH-SRYKA        PIC X(2).
      *診療区分
           03  SRYSRH-SRYKBN       PIC X(2).
      *診療種別区分
           03  SRYSRH-SRYSYUKBN    PIC X(3).
      *診療日
           03  SRYSRH-SRYYMD       PIC X(8).
      *診療コード
           03  SRYSRH-SRYCD-G.
               05  SRYSRH-SRYCD    PIC X(9)
                                   OCCURS  5.
      *作成日
           03  SRYSRH-CREYMD       PIC X(8).
      *更新日
           03  SRYSRH-UPYMD        PIC X(8).
      *更新時間
           03  SRYSRH-UPHMS        PIC X(6).
      *開始診療日
           03  SRYSRH-STSRYYMD     PIC X(8).
      *終了診療日
           03  SRYSRH-EDSRYYMD     PIC X(8).
      *開始診療月
           03  SRYSRH-STSRYYM      PIC X(6).
      *終了診療月
           03  SRYSRH-EDSRYYM      PIC X(6).

