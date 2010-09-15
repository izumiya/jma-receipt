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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALRES.
            02  MDCRES-OBJECT              PIC X(8).
            02  MDCRES-MODE                PIC S9(9)   BINARY.
            02  MDCRES-MEDICALRES.
              03  MDCRES-INFORMATION-DATE  PIC X(10).
              03  MDCRES-INFORMATION-TIME  PIC X(8).
              03  MDCRES-API-RESULT        PIC X(2).
              03  MDCRES-API-RESULT-MSG    PIC X(100).
              03  MDCRES-RESKEY            PIC X(100).
              03  MDCRES-PERFORM-DATE      PIC X(10).
              03  MDCRES-PERFORM-TIME      PIC X(8).
              03  MDCRES-APPOINT-DEP-CODE  PIC X(2).
              03  MDCRES-APPOINT-DEP-NAME  PIC X(80).
              03  MDCRES-APPOINT-DR-CODE   PIC X(5).
              03  MDCRES-APPOINT-DR-NAME   PIC X(80).
              03  MDCRES-PATIENT-INFOMATION.
                04  MDCRES-PATIENTID       PIC X(20).
                04  MDCRES-NAME            PIC X(100).
                04  MDCRES-KANANAME        PIC X(100).
                04  MDCRES-BIRTHDAY        PIC X(10).
                04  MDCRES-SEX             PIC X(1).
