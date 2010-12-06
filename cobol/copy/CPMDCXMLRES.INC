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
      ******************************************************************
      *    ＡＰＩレスポンス中途終了情報         COPY (CPMDCXMLRES.INC)
      ******************************************************************
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
      *         保険情報
                04  MDCRES-INSURANCE-G.
      *             保険の種類
                    06  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    06  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    06  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    06  MDCRES-MAIN-MARK              PIC X(80).
      *             番号
                    06  MDCRES-MAIN-NUMBER            PIC X(80).
      *             継続区分
                    06  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    06  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *             本人家族区分
                    06  MDCRES-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    06  MDCRES-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    06  MDCRES-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    06  MDCRES-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    06  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      07  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      07  MDCRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      07  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      07  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      07  MDCRES-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      07  MDCRES-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      07  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               外来−固定額
                      07  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      07  MDCRES-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      07  MDCRES-INSURANCE-END-DATE      PIC X(10).
      *
