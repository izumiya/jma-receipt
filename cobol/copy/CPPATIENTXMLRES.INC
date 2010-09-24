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
        01  XML-PATIENTINFORES.
            02  PTRES-OBJECT           PIC X(8).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-PATIENTINFORES.
              03  PTRES-INFORMATION-DATE  PIC X(10).
              03  PTRES-INFORMATION-TIME  PIC X(8).
              03  PTRES-API-RESULT     PIC X(2).
              03  PTRES-API-RESULT-MSG PIC X(100).
              03  PTRES-RESKEY         PIC X(100).
              03  PTRES-PATIENT-INFOMATION.
                04  PTRES-ID           PIC X(20).
                04  PTRES-NAME         PIC X(100).
                04  PTRES-KANANAME     PIC X(100).
                04  PTRES-BIRTHDAY     PIC X(10).
                04  PTRES-SEX          PIC X(1).
                04  PTRES-ADDRESS-INFO.
                  05  PTRES-HOME-ZIP-CODE    PIC X(7).
                  05  PTRES-HOME-ADDRESSES1  PIC X(200).
                  05  PTRES-HOME-ADDRESSES2  PIC X(200).
                  05  PTRES-HOME-HPONE1      PIC X(15).
                  05  PTRES-HOME-HPONE2      PIC X(15).
                04  PTRES-INSURANCE-INFO     OCCURS  4 TIMES.
                  05  PTRES-MAIN-INSURANCE-CLASS   PIC X(10).
                  05  PTRES-MAIN-INSURANCE-NUMBER  PIC X(10).
                  05  PTRES-MAIN-INSURANCE-NAME    PIC X(10).
                  05  PTRES-MAIN-MARK  PIC X(80).
                  05  PTRES-MAIN-NUMBER  PIC X(80).
                  05  PTRES-MAIN-CONTINUATION PIC X(10).
                  05  PTRES-MAIN-ASSISTANCE   PIC X(10).
                  05  PTRES-MAIN-FAMILY   PIC X(10).
                  05  PTRES-MAIN-POLICYHOLDER PIC X(100).
                  05  PTRES-MAIN-START-DATE   PIC X(10).
                  05  PTRES-MAIN-END-DATE PIC X(10).
                  05  PTRES-PUBLIC-INSURANCE-INFO     OCCURS  3 TIMES.
                    06  PTRES-INSURANCE-NUMBER  PIC X(10).
                    06  PTRES-INSURANCE-NAME    PIC X(10).
                    06  PTRES-PROVIDER-NUMBER   PIC X(10).
                    06  PTRES-RECIPIENT-NUMBER  PIC X(20).
                    06  PTRES-INSURANCE-START-DATE  PIC X(10).
                    06  PTRES-INSURANCE-END-DATE    PIC X(10).
