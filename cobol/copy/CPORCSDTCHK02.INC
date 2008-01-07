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
       01  SDTCHK02AREA.
           03  SDTCHK02-IN.
               05  SDTCHK02-HOSPID             PIC X(24).
               05  SDTCHK02-PTID               PIC 9(10).
           03  SDTCHK02-OT.
               05  SDTCHK02-O-MAX              PIC 9(05).
               05  SDTCHK02-O-HKNCOMBIMEI      PIC X(50)
                                               OCCURS  1000.
           03  SDTCHK02-OT-HKNINF.
               05  SDTCHK02-O-HKNINF-MAX       PIC 9(05).
               05  SDTCHK02-O-HKNINF-OCC       OCCURS  50.
                   07  SDTCHK02-O-SYAHO        PIC 9(01).
                   07  SDTCHK02-O-KOKUHO       PIC 9(01).
                   07  SDTCHK02-O-ROSAI        PIC 9(01).
                   07  SDTCHK02-O-JIBAI        PIC 9(01).
                   07  SDTCHK02-O-JIHI         PIC 9(01).
                   07  SDTCHK02-O-HKNNUM       PIC X(03).
