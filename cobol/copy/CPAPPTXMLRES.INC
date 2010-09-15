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
        01  XML-APPOINTRES.
            02  APNTRES-OBJECT         PIC X(8).
            02  APNTRES-MODE           PIC S9(9)   BINARY.
            02  APNTRES-APPOINTRES.
              03  APNTRES-INFORMATION-DATE    PIC X(10).
              03  APNTRES-INFORMATION-TIME    PIC X(8).
              03  APNTRES-API-RESULT   PIC X(2).
              03  APNTRES-API-RESULT-MSG      PIC X(100).
              03  APNTRES-RESKEY  PIC X(100).
              03  APNTRES-APPOINT-DATE        PIC X(10).
              03  APNTRES-APPOINT-TIME        PIC X(8).
              03  APNTRES-APPOINT-ID          PIC X(5).
              03  APNTRES-APPOINT-DEP-CODE    PIC X(2).
              03  APNTRES-APPOINT-DEP-NAME    PIC X(80).
              03  APNTRES-APPOINT-DR-CODE     PIC X(5).
              03  APNTRES-APPOINT-DR-NAME     PIC X(80).
              03  APNTRES-APPOINT-MEMO        PIC X(100).
              03  APNTRES-PATIENT-INFOMATION.
                04  APNTRES-PATIENTID         PIC X(20).
                04  APNTRES-NAME              PIC X(100).
                04  APNTRES-KANANAME          PIC X(100).
                04  APNTRES-BIRTHDAY          PIC X(10).
                04  APNTRES-SEX               PIC X(1).
                04  APNTRES-ADDRESS-INFO.
                  05  APNTRES-HOME-ZIP-CODE   PIC X(7).
                  05  APNTRES-HOME-ADDRESSES  PIC X(100).
                04  APNTRES-INSURANCE-INFO          OCCURS  4 TIMES.
                  05  APNTRES-MAIN-INSURANCE-CLASS    PIC X(10).
                  05  APNTRES-MAIN-INSURANCE-NUMBER   PIC X(10).
                  05  APNTRES-MAIN-INSURANCE-NAME PIC X(10).
                  05  APNTRES-MAIN-MARK       PIC X(80).
                  05  APNTRES-MAIN-NUMBER     PIC X(80).
                  05  APNTRES-MAIN-CONTINUATION   PIC X(10).
                  05  APNTRES-MAIN-ASSISTANCE PIC X(10).
                  05  APNTRES-MAIN-FAMILY     PIC X(10).
                  05  APNTRES-MAIN-POLICYHOLDER   PIC X(100).
                  05  APNTRES-MAIN-START-DATE PIC X(10).
                  05  APNTRES-MAIN-END-DATE   PIC X(10).
                  05  APNTRES-PUBLIC-INSURANCE-INFO 
                                              OCCURS  3 TIMES.
                    06  APNTRES-INSURANCE-NUMBER    PIC X(10).
                    06  APNTRES-INSURANCE-NAME  PIC X(10).
                    06  APNTRES-PROVIDER-NUMBER PIC X(10).
                    06  APNTRES-RECIPIENT-NUMBER    PIC X(10).
                    06  APNTRES-INSURANCE-START-DATE    PIC X(10).
                    06  APNTRES-INSURANCE-END-DATE  PIC X(10).
