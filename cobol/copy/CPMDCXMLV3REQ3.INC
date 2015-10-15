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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエスト診療行為登録情報 (一体化)
      *                                       COPY (CPMDCXMLV3REQ3.INC)
      *         診療行為内容
      ******************************************************************
       01  XML-MEDICALV3REQ3.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(8).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       リクエスト番号
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       基準日付（収納入金日）
              05  MDCREQ-BASE-DATE         PIC X(10).
      *       患者番号
              05  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       診療時間
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID（オルカ）
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       自動発生剤削除区分（１：削除あり）
              05  MDCREQ-MDC-MODE          PIC X(01).
      *       自動発生剤削除連番
              05  MDCREQ-DEL-NUMBER-G.
                  07  MDCREQ-DEL-NUMBER        PIC X(02)
                                               OCCURS   30.
      *
      *       入金方法
              05  MDCREQ-IC-CODE           PIC X(02).
      *       入金金額
              05  MDCREQ-IC-MONEY          PIC X(10).
      *       調整金額１
              05  MDCREQ-AD-MONEY1         PIC X(10).
      *       調整金額２
              05  MDCREQ-AD-MONEY2         PIC X(10).
      *
      *
