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
        01  XML-MEDICALREQ.
            02  MDCREQ-OBJECT   PIC X(8).
            02  MDCREQ-MODE PIC S9(9)   BINARY.
            02  MDCREQ-MEDICALREQ.
              03  MDCREQ-INFORMATION-DATE PIC X(10).
              03  MDCREQ-INFORMATION-TIME PIC X(8).
              03  MDCREQ-OUTPATIENT-CLASS PIC X(1).
              03  MDCREQ-PATIENTID    PIC X(20).
              03  MDCREQ-PERFORM-DATE PIC X(10).
              03  MDCREQ-PERFORM-TIME PIC X(8).
              03  MDCREQ-MEDICAL-INFO     OCCURS  2 TIMES.
                04  MDCREQ-DEP-CODE PIC X(2).
                04  MDCREQ-DEP-NAME PIC X(80).
                04  MDCREQ-DR-CODE  PIC X(5).
                04  MDCREQ-DR-NAME  PIC X(80).
                04  MDCREQ-INSURANCE-INFO.
                  05  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(10).
                  05  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(10).
                  05  MDCREQ-MAIN-INSURANCE-NAME     PIC X(10).
                  05  MDCREQ-MAIN-MARK               PIC X(80).
                  05  MDCREQ-MAIN-NUMBER             PIC X(80).
                  05  MDCREQ-MAIN-CONTINUATION       PIC X(10).
                  05  MDCREQ-MAIN-ASSISTANCE         PIC X(10).
                  05  MDCREQ-MAIN-FAMILY             PIC X(10).
                  05  MDCREQ-MAIN-POLICYHOLDER       PIC X(100).
                  05  MDCREQ-MAIN-START-DATE         PIC X(10).
                  05  MDCREQ-MAIN-END-DATE           PIC X(10).
                  05  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  3 TIMES.
                    06  MDCREQ-INSURANCE-CLASS       PIC X(10).
                    06  MDCREQ-INSURANCE-NUMBER      PIC X(10).
                    06  MDCREQ-INSURANCE-NAME        PIC X(10).
                    06  MDCREQ-PROVIDER-NUMBER       PIC X(10).
                    06  MDCREQ-RECIPIENT-NUMBER      PIC X(10).
                    06  MDCREQ-INSURANCE-START-DATE  PIC X(10).
                    06  MDCREQ-INSURANCE-END-DATE    PIC X(10).
                04  MDCREQ-MDC-INFO         OCCURS  40 TIMES.
                  05  MDCREQ-MDC-CLASS               PIC X(3).
                  05  MDCREQ-MDC-CLASS-ID            PIC X(2).
                  05  MDCREQ-MDC-CLASS-NAME          PIC X(50).
                  05  MDCREQ-MDC-CLASS-NUMBER        PIC X(50).
                  05  MDCREQ-PRESCRIPTION-INFO        OCCURS  40 TIMES.
                    06  MDCREQ-PRSCRPT-CLASS         PIC X(3).
                    06  MDCREQ-PRSCRPT-CODE          PIC X(9).
                    06  MDCREQ-PRSCRPT-NAME          PIC X(50).
                    06  MDCREQ-PRSCRPT-NUMBER        PIC X(10).
                04  MDCREQ-DIAGNOSIS-INFO           OCCURS  50 TIMES.
                  05  MDCREQ-DIAGNOSIS-NUMBER        PIC X(50).
                  05  MDCREQ-DIAGNOSIS-NAME          PIC X(80).
                  05  MDCREQ-DIAGNOSIS-SINGLE       OCCURS  6  TIMES.
                    07 MDCREQ-DIAGNOSIS-SINGLE-NUMBER
                                                     PIC X(10). 
                    07 MDCREQ-DIAGNOSIS-SINGLE-NAME
                                                     PIC X(20). 
                  05  MDCREQ-DIAGNOSIS-CATEGORY      PIC X(2).
                  05  MDCREQ-DIAGNOSIS-SUSPECTEDFLAG PIC X(2).
                  05  MDCREQ-DIAGNOSIS-START-DATE    PIC X(10).
                  05  MDCREQ-DIAGNOSIS-END-DATE      PIC X(10).
                  05  MDCREQ-DIAGNOSIS-OUTCOME       PIC X(2).
