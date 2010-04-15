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
      *    患者情報照会テーブル    COPY (CPPTSRH.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTSRH-REC.
      *医療機関番号
           03  PTSRH-HOSPNUM      PIC 9(2).
      *端末ＩＤ
           03  PTSRH-TERMID       PIC X(64).
      *患者ＩＤ
           03  PTSRH-PTID         PIC 9(10).
      *患者番号
           03  PTSRH-PTNUM        PIC X(20).
      *カナ氏名
           03  PTSRH-KANANAME     PIC X(100).
      *性別
           03  PTSRH-SEX          PIC X(1).
      *誕生日
           03  PTSRH-BIRTHDAY     PIC X(8).
      *受給者番号
           03  PTSRH-JKYSNUM      PIC X(20).
      *データ区分
           03  PTSRH-DATAKBN      PIC X(1).
      *作成日
           03  PTSRH-CREYMD       PIC X(8).
      *更新日
           03  PTSRH-UPYMD        PIC X(8).
      *更新時間
           03  PTSRH-UPHMS        PIC X(6).
      *以下検索用項目
      *データ区分
           03  PTSRH-UPDATAKBN    PIC X(1).
      *SQL
           03  PTSRH-SQL          PIC X(15000).
      *DBESCAPEENCODING
           03  PTSRH-DBSTR        PIC X(500).

