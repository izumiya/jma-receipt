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
      *    システム管理項目値選択肢    COPY   (CPSYSKANRICOMBO.INC) 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SYS-COMBO-REC.
      *キー
           03  SYS-COMBO-KEY.
      *管理コード
               05  SYS-COMBO-KANRICD   PIC X(04).
      *区分コード
               05  SYS-COMBO-KBNCD     PIC X(08).
      *項目コード
               05  SYS-COMBO-ITEMCD    PIC 9(05).
      *値
               05  SYS-COMBO-VALUE     PIC X(02).
      *有効開始年月日
               05  SYS-COMBO-YUKOSTYMD PIC X(08).
      *有効終了年月日
               05  SYS-COMBO-YUKOEDYMD PIC X(08).
      *値名称
           03  SYS-COMBO-VALUENAME     PIC X(100).
      *更新年月日
           03  SYS-COMBO-UPYMD         PIC X(08).
