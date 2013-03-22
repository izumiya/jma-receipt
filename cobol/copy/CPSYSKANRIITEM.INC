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
      *    システム管理項目        COPY   (CPSYSKANRIITEM.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SYS-ITEM-REC.
      *キー
           03  SYS-ITEM-KEY.
      *管理コード
               05  SYS-ITEM-KANRICD    PIC X(04).
      *区分コード
               05  SYS-ITEM-KBNCD      PIC X(08).
      *項目コード
               05  SYS-ITEM-ITEMCD     PIC 9(05).
      *有効開始年月日
               05  SYS-ITEM-YUKOSTYMD  PIC X(08).
      *有効終了年月日
               05  SYS-ITEM-YUKOEDYMD  PIC X(08).
      *項目名称
           03  SYS-ITEM-ITEMNAME       PIC X(100).
      *データ形式
           03  SYS-ITEM-ITEMTYPE       PIC 9(01).
      *データ桁数
           03  SYS-ITEM-ITEMLENGTH     PIC 9(03).
      *データ精度
           03  SYS-ITEM-ITEMPRECISION  PIC 9(01).
      *更新年月日
           03  SYS-ITEM-UPYMD          PIC X(08).
