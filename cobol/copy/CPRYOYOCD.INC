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
      *         療養病棟入院基本料コード  (CPRYOYOCD.INC)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CONST-RYOYOCD-AREA.
           03  CONST-RYOYOCD-G.
               05  CONST-RYOYOCD-VAL.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190121310".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190121410".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190121510".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190121610".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190121710".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 6.
                   07                  PIC X(09)   VALUE "190131610".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 7.
                   07                  PIC X(09)   VALUE "190131810".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 8.
                   07                  PIC X(09)   VALUE "190132010".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 9.
                   07                  PIC X(09)   VALUE "190132210".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 10.
                   07                  PIC X(09)   VALUE "190121810".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190123710".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190123810".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190123910".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190124010".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190124110".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 6.
                   07                  PIC X(09)   VALUE "190131710".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 7.
                   07                  PIC X(09)   VALUE "190131910".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 8.
                   07                  PIC X(09)   VALUE "190132110".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 9.
                   07                  PIC X(09)   VALUE "190132310".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 10.
                   07                  PIC X(09)   VALUE "190124210".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190132410".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190132610".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190132810".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190133010".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190133210".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 6.
                   07                  PIC X(09)   VALUE "190133410".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 7.
                   07                  PIC X(09)   VALUE "190133610".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 8.
                   07                  PIC X(09)   VALUE "190133810".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 9.
                   07                  PIC X(09)   VALUE "190134010".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190132510".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190132710".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190132910".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190133110".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190133310".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 6.
                   07                  PIC X(09)   VALUE "190133510".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 7.
                   07                  PIC X(09)   VALUE "190133710".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 8.
                   07                  PIC X(09)   VALUE "190133910".
      *
                   07                  PIC 9(01)   VALUE 2.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 9.
                   07                  PIC X(09)   VALUE "190134110".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190122010".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190122110".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190122210".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190122310".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190122410".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 0.
                   07                  PIC 9(02)   VALUE 10.
                   07                  PIC X(09)   VALUE "190122510".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 1.
                   07                  PIC X(09)   VALUE "190124310".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 2.
                   07                  PIC X(09)   VALUE "190124410".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 3.
                   07                  PIC X(09)   VALUE "190124510".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 4.
                   07                  PIC X(09)   VALUE "190124610".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 5.
                   07                  PIC X(09)   VALUE "190124710".
      *
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(01)   VALUE 1.
                   07                  PIC 9(02)   VALUE 10.
                   07                  PIC X(09)   VALUE "190124810".
      *
               05  CONST-RYOYOCD-R REDEFINES   CONST-RYOYOCD-VAL.
                   07  CONST-RYOYOCD-OCC   OCCURS      50.
      *                入院基本料1or2(特別入院基本料は便宜上１)
                       09  CONST-RYOYOKHN      PIC 9(01).
      *                0:一般/1:生活
                       09  CONST-RYOYOLIFE PIC 9(01).
                       09  CONST-RYOYOCDMSGID
                                           PIC 9(02).
                       09  CONST-RYOYOCD   PIC X(09).
               05  CONST-RYOYOCD-MAX   PIC 9(05)   VALUE   50.
