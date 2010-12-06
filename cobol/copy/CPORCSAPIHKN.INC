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
      *    ＡＰＩ保険組合せ処理パラメタ　　COPY (CPORCSHKNGET.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSAPIHKNAREA.
      *入力
      *処理区分
           03  ORCSAPIHKN-KBN              PIC  X(01).
      *診療科（前回受診履歴より）
           03  ORCSAPIHKN-SRYKA            PIC  X(02).
      *保険情報
           03  ORCSAPIHKN-IN-HKNAREA.
      *        保険種類
               05  ORCSAPIHKN-IN-HKNNUM            PIC X(03).
      *        保険者番号
               05  ORCSAPIHKN-IN-HKNJANUM          PIC X(08).
      *        保険種類名称
               05  ORCSAPIHKN-IN-HKNNUM-NAME       PIC X(10).
      *        記号
               05  ORCSAPIHKN-IN-MARK              PIC X(80).
      *        番号
               05  ORCSAPIHKN-IN-NUMBER            PIC X(80).
      *        有効期間
               05  ORCSAPIHKN-IN-TEKSTYMD          PIC X(08).
               05  ORCSAPIHKN-IN-TEKEDYMD          PIC X(08).
      *        公費情報
               05  ORCSAPIHKN-IN-INSURANCE-INFO
                                       OCCURS  4 TIMES.
                   07  ORCSAPIHKN-IN-KOHNUM            PIC X(04).
                   07  ORCSAPIHKN-IN-KOHNUM-NAME       PIC X(10).
                   07  ORCSAPIHKN-IN-FTNJANUM          PIC X(08).
                   07  ORCSAPIHKN-IN-JKYSNUM           PIC X(20).
      *    保険組合せ
           03  ORCSAPIHKN-IN-HKNCOMBI      PIC X(04).
      *
      *出力領域
           03  ORCSAPIHKN-OUT-AREA.
      *        対象保険組合せ
               05  ORCSAPIHKN-OT-HKNCOMBI      PIC X(04).
      *有効保険一覧
           03  ORCSAPIHKN-HKNCOMBI-AREA.
               05  ORCSAPIHKN-HKNCOMBI-TBL     OCCURS   20.
      *            保険組合せ
                   07  ORCSAPIHKN-THKNCOMBI        PIC  X(04).
      *            保険種別
                   07  ORCSAPIHKN-HKNNUM           PIC  X(03).
      *            保険種別名称
                   07  ORCSAPIHKN-HKNNUM-NAME      PIC  X(20).
      *            保険者番号
                   07  ORCSAPIHKN-HKNJANUM         PIC  X(08).
      *            記号
                   07  ORCSAPIHKN-KIGO             PIC  X(80).
      *            番号
                   07  ORCSAPIHKN-NUM              PIC  X(80).
      *            継続区分
                   07  ORCSAPIHKN-CONTKBN          PIC  X(01).
      *            補助区分
                   07  ORCSAPIHKN-HOJOKBN          PIC  X(01).
      *            本人家族区分
                   07  ORCSAPIHKN-HONKZKKBN        PIC  X(01).
      *            被保険者名称
                   07  ORCSAPIHKN-HIHKNJANAME      PIC  X(100).
      *            適用期間（表示）
                   07  ORCSAPIHKN-TEKSTYMD         PIC  X(10).
                   07  ORCSAPIHKN-TEKEDYMD         PIC  X(10).
      *            適用期間
                   07  ORCSAPIHKN-NAI-TEKSTYMD     PIC  X(08).
                   07  ORCSAPIHKN-NAI-TEKEDYMD     PIC  X(08).
      *            非表示区分
                   07  ORCSAPIHKN-HYOJIKBN         PIC  X(01).
      *            公費種類・公費ＩＤ
                   07  ORCSAPIHKN-KOH-G.
                       09  ORCSAPIHKN-KOH-OCC      OCCURS   4.
                           11  ORCSAPIHKN-KOHNUM           PIC  X(03).
                           11  ORCSAPIHKN-KOHNUM-NAME      PIC  X(20).
                           11  ORCSAPIHKN-FTNJANUM         PIC  X(08).
                           11  ORCSAPIHKN-JKYSNUM          PIC  X(20).
      *                    適用期間
                           11  ORCSAPIHKN-KOH-TEKSTYMD     PIC  X(10).
                           11  ORCSAPIHKN-KOH-TEKEDYMD     PIC  X(10).
      *                    入院−負担率
                           11  ORCSAPIHKN-RATE-ADMISSION   PIC X(05).
      *                    入院−固定額
                           11  ORCSAPIHKN-MONEY-ADMISSION  PIC X(06).
      *                    外来−負担率
                           11  ORCSAPIHKN-RATE-OUTPATIENT  PIC X(05).
      *                     外来−固定額
                           11  ORCSAPIHKN-MONEY-OUTPATIENT PIC X(06).
      *エラーコード
           03  ORCSAPIHKN-ERRCD                    PIC 9(01).
      *
      *
