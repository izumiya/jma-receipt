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
      *    システム管理項目値      COPY   (CPSYSKANRIVALUE.INC) 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  SYS-VALUE-REC.
      *キー
           03  SYS-VALUE-KEY.
      *医療機関識別番号
               05  SYS-VALUE-HOSPNUM   PIC 9(2).
      *管理コード
               05  SYS-VALUE-KANRICD   PIC X(04).
      *区分コード
               05  SYS-VALUE-KBNCD     PIC X(08).
      *項目コード
               05  SYS-VALUE-ITEMCD    PIC 9(05).
      *有効開始年月日
               05  SYS-VALUE-YUKOSTYMD PIC X(08).
      *有効終了年月日
               05  SYS-VALUE-YUKOEDYMD PIC X(08).
      *値
           03  SYS-VALUE-VALUE         PIC X(500).
      *オペーレータＩＤ
           03  SYS-VALUE-OPID          PIC X(16).
      *作成年月日
           03  SYS-VALUE-CREYMD        PIC X(08).
      *更新年月日
           03  SYS-VALUE-UPYMD         PIC X(08).
      *更新時間
           03  SYS-VALUE-UPHMS         PIC X(06).
      *主キー更新
           03  SYSUP-VALUE-KEY.
               05  SYSUP-VALUE-HOSPNUM     PIC 9(2).
               05  SYSUP-VALUE-KANRICD     PIC X(04).
               05  SYSUP-VALUE-KBNCD       PIC X(08).
               05  SYSUP-VALUE-ITEMCD      PIC 9(05).
               05  SYSUP-VALUE-YUKOSTYMD   PIC X(08).
               05  SYSUP-VALUE-YUKOEDYMD   PIC X(08).
