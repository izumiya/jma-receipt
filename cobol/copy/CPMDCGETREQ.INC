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
      ******************************************************************
      *    ＡＰＩリクエスト診療行為情報取得      COPY (CPMDCGETREQ.INC)
      ******************************************************************
        01  XML-MEDICALGETREQ.
            02  MDCREQ-OBJECT       PIC X(8).
            02  MDCREQ-MODE         PIC S9(9)   BINARY.
            02  MDCREQ-MEDICALREQ.
      *       患者番号
              03  MDCREQ-PATIENTID         PIC X(20).
      *       診療日付
              03  MDCREQ-PERFORM-DATE      PIC X(10).
      *       入外区分（１：入院）
      ****    03  MDCREQ-OUTPATIENT-CLASS  PIC X(1).
              03  MDCREQ-MEDICAL-INFO.
      *         診療科
                04  MDCREQ-DEP-CODE                PIC X(02).
      *         連番
                04  MDCREQ-NUMBER                  PIC X(01).
      *         保険組合せ番号
                04  MDCREQ-COMBINATION-NUMBER      PIC X(04).
      *         保険組合せ情報
                04  MDCREQ-INSURANCE-INFO.
      *           保険の種類
                  05  MDCREQ-MAIN-INSURANCE-CLASS    PIC X(03).
      *           保険の種類
                  05  MDCREQ-MAIN-INSURANCE-NAME     PIC X(20).
      *           保険者番号
                  05  MDCREQ-MAIN-INSURANCE-NUMBER   PIC X(08).
      *           記号
                  05  MDCREQ-MAIN-MARK               PIC X(80).
      *           番号
                  05  MDCREQ-MAIN-NUMBER             PIC X(80).
      *           公費情報
                  05  MDCREQ-PUBLIC-INSURANCE-INF  OCCURS  4 TIMES.
      *             公費の種類
                    06  MDCREQ-INSURANCE-CLASS       PIC X(03).
      *             公費の種類
                    06  MDCREQ-INSURANCE-NAME        PIC X(20).
      *             負担者番号
                    06  MDCREQ-PROVIDER-NUMBER       PIC X(08).
      *             受給者番号
                    06  MDCREQ-RECIPIENT-NUMBER      PIC X(20).
      *
