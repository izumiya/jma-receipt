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
      ******************************************************************
      *    ＡＰＩレスポンスお薬手帳情報
      *    COPY (CPOKUSURIGETV2RES.INC)
      ******************************************************************
       01  XML-OKUSURIGETRES.
           03  OKUSURIGETRES-CONTEXT     PIC S9(9)   BINARY.
           03  OKUSURIGETRES-OBJECT      PIC X(8).
           03  OKUSURIGETRES-MODE        PIC S9(9)   BINARY.
           03  OKUSURIGETRES-RECORDNAME  PIC X(128).
      *
           03  OKUSURIGETRES-OKUSURIGETV2RES.
               05  OKUSURIGETRES-INFORMATION-DATE    PIC X(10).
               05  OKUSURIGETRES-INFORMATION-TIME    PIC X(08).
               05  OKUSURIGETRES-API-RESULT          PIC X(04).
               05  OKUSURIGETRES-API-RESULT-MSG      PIC X(100).
      *        基準日
               05  OKUSURIGETRES-BASE-DATE           PIC X(10).
      *        患者情報
               05  OKUSURIGETRES-PTINF-AREA.
                 07  OKUSURIGETRES-PTNUM             PIC X(20).
                 07  OKUSURIGETRES-NAME              PIC X(100).
                 07  OKUSURIGETRES-KANANAME          PIC X(100).
                 07  OKUSURIGETRES-BIRTHDAY          PIC X(10).
                 07  OKUSURIGETRES-SEX               PIC X(1).
      *        お薬手帳管理データテーブル（１０件まで）
               05  OKUSURIGETRES-INFOMATION  OCCURS  10.
      *        連番
                 07  OKUSURIGETRES-RENNUM            PIC X(04).
      *        グループ
                 07  OKUSURIGETRES-TBL-GROUP         PIC X(14).
      *        UUID
                 07  OKUSURIGETRES-TBL-UUID          PIC X(36).
      *        処理内連番
                 07  OKUSURIGETRES-SHORI-RENNUM      PIC X(04).
      *        お薬手帳データ
               05  OKUSURIGETRES-DATA.
                   07  OKUSURIGETRES-CSV-DATA        PIC X(10000).
