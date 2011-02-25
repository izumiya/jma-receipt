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
      *    新保険別請求チェック表中間ファイル                    *
      *    COPY  (BGSF01.INC)                                  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  BGSF01-REC.
           03  BGSF01-KEY.
      *    保険
      *      1:社保/2:国保/3:後期高齢者/4:特別療養費/5:自費/
      *      6:治験/7:労災/8:自賠責/9:公害
               05  BGSF01-HOKEN        PIC 9(01).
      *    入外区分
               05  BGSF01-NYUGAIKBN    PIC X(01).
      *    診療科
               05  BGSF01-RECEKA       PIC X(02).
      *    レセプト種別(1)
      *    ※()の数字は項目を使用するデータ区分
      *      1:請求/2:公費/3:保険者
               05  BGSF01-RECESBT      PIC X(04).
      *    保険番号(1)
               05  BGSF01-HKNNUM       PIC X(03).
      *    公費区分    (2)
      *      1:レセプト請求分/2:公費請求書請求分
               05  BGSF01-KOHKBN       PIC X(01).
      *    公費の種類  (2)
               05  BGSF01-KOHNUM       PIC X(03).
      *    県内・県外区分 (3)
               05  BGSF01-PREFKBN      PIC X(01).
      *    保険者番号  (3)
               05  BGSF01-HKNJANUM     PIC X(08).
      *    件数
           03  BGSF01-KENSU            PIC 9(06).
      *    実日数
           03  BGSF01-NISSU            PIC 9(08).
      *    請求点
           03  BGSF01-TENSU            PIC 9(08).
      *    患者負担
           03  BGSF01-FTNMONEY         PIC 9(09).
      *    保険者負担
           03  BGSF01-HKNMONEY         PIC 9(09).
      *    公費負担
           03  BGSF01-KOHMONEY         PIC 9(09).
      *    合計(患者負担+保険者負担+公費負担)
           03  BGSF01-TTLMONEY         PIC 9(09).
      *    食事回数
           03  BGSF01-SKJKAISU         PIC 9(06).
      *    食事請求円
           03  BGSF01-SKJTTLMONEY      PIC 9(09).
      *    食事患者負担
           03  BGSF01-SKJFTNMONEY      PIC 9(09).
      *    食事保険者負担
           03  BGSF01-SKJHKNMONEY      PIC 9(09).
      *    食事公費負担
           03  BGSF01-SKJKOHMONEY      PIC 9(09).
      *    合計２（合計＋食事請求円）
           03  BGSF01-TTLTTLMONEY      PIC 9(09).
      *
      *    以下その他請求で使用
      *    自費
           03  BGSF01-JIHIMONEY        PIC S9(09).
      *    減免
           03  BGSF01-GENMEN           PIC S9(09).
      *    調整金
           03  BGSF01-CHOSEI1          PIC S9(09).
      *    調整金
           03  BGSF01-CHOSEI2          PIC S9(09).
      *    端数調整金
           03  BGSF01-HASU             PIC S9(09).
