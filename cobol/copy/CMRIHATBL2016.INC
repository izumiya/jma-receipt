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
      *----------------------------------------------------------------*
      *   リハビリコードテーブル（労災外） COPY (CMRIHATBL2016.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *    リハビリのコード
       01  TBL-RIHABIRI2016-AREA.
      *    心血管疾病リハビリ
           03  FILLER              PIC X(13)   VALUE   "1 1 180027410".
           03  FILLER              PIC X(13)   VALUE   "1 2 180027510".
      *    脳血管疾病リハビリ
           03  FILLER              PIC X(13)   VALUE   "2 1 180027610".
           03  FILLER              PIC X(13)   VALUE   "2 2 180027710".
           03  FILLER              PIC X(13)   VALUE   "2 3 180030810".
      *    脳血管疾病リハビリ（要介護・入院外）
           03  FILLER              PIC X(13)   VALUE   "211 180033910".
           03  FILLER              PIC X(13)   VALUE   "212 180034110".
           03  FILLER              PIC X(13)   VALUE   "213 180034310".
      *    脳血管疾病リハビリ（要介護）基準不適合
           03  FILLER              PIC X(13)   VALUE   "211 180043430".
           03  FILLER              PIC X(13)   VALUE   "212 180043630".
           03  FILLER              PIC X(13)   VALUE   "213 180043830".
      *    脳血管疾病リハビリ（要介護・入院）
           03  FILLER              PIC X(13)   VALUE   "211 180044310".
           03  FILLER              PIC X(13)   VALUE   "212 180044410".
           03  FILLER              PIC X(13)   VALUE   "213 180044510".
      *    運動器リハビリ
           03  FILLER              PIC X(13)   VALUE   "3 1 180032710".
           03  FILLER              PIC X(13)   VALUE   "3 2 180027810".
           03  FILLER              PIC X(13)   VALUE   "3 2 180027910".
      *    運動器リハビリ（要介護・入院外）
           03  FILLER              PIC X(13)   VALUE   "311 180034510".
           03  FILLER              PIC X(13)   VALUE   "312 180034610".
           03  FILLER              PIC X(13)   VALUE   "312 180034710".
      *    運動器リハビリ（要介護）基準不適合
           03  FILLER              PIC X(13)   VALUE   "311 180044030".
           03  FILLER              PIC X(13)   VALUE   "312 180044130".
           03  FILLER              PIC X(13)   VALUE   "312 180044230".
      *    運動器リハビリ（要介護・入院）
           03  FILLER              PIC X(13)   VALUE   "311 180045810".
           03  FILLER              PIC X(13)   VALUE   "312 180045910".
           03  FILLER              PIC X(13)   VALUE   "312 180046010".
      *    呼吸器リハビリ
           03  FILLER              PIC X(13)   VALUE   "4 1 180028010".
           03  FILLER              PIC X(13)   VALUE   "4 2 180028110".
      *    廃用症候群
           03  FILLER              PIC X(13)   VALUE   "9 1 180044610".
           03  FILLER              PIC X(13)   VALUE   "9 2 180044710".
           03  FILLER              PIC X(13)   VALUE   "9 2 180044810".
      *    廃用症候群（要介護・入院）
           03  FILLER              PIC X(13)   VALUE   "911 180044910".
           03  FILLER              PIC X(13)   VALUE   "912 180045110".
           03  FILLER              PIC X(13)   VALUE   "913 180045310".
      *    廃用症候群（要介護・入院外）
           03  FILLER              PIC X(13)   VALUE   "911 180045010".
           03  FILLER              PIC X(13)   VALUE   "912 180045210".
           03  FILLER              PIC X(13)   VALUE   "913 180045410".
      *    廃用症候群（要介護）基準不適合
           03  FILLER              PIC X(13)   VALUE   "911 180045530".
           03  FILLER              PIC X(13)   VALUE   "912 180045630".
           03  FILLER              PIC X(13)   VALUE   "913 180045730".
      *
       01  TBL-RIHABIRI2016-AREA-R REDEFINES   TBL-RIHABIRI2016-AREA.
           03  TBL-RIHABIRI2016-OCC    OCCURS  40   INDEXED   TBL-RIH16.
      *        リハビリ種類
               05  TBL-RIH16-KBN        PIC X(01).
      *        1:要介護
               05  TBL-RIH16-KIG        PIC X(01).
               05  TBL-RIH16-KBN2       PIC X(01).
               05  TBL-RIH16-YOBO       PIC X(01).
               05  TBL-RIH16-SRYCD      PIC X(09).
      *
       01  TBL-RIHA2016-MAX             PIC 9(02)  VALUE    40.
      *
