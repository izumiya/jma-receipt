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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SKYSRH-REC.
      *    医療機関番号
           03  SKYSRH-HOSPNUM          PIC 9(02).
      *    端末ＩＤ
           03  SKYSRH-TERMID           PIC X(64).
      *    患者ＩＤ
           03  SKYSRH-PTID             PIC 9(10).
      *    入外区分
           03  SKYSRH-NYUGAIKBN        PIC X(1).
      *    診療年月
           03  SKYSRH-SRYYM            PIC X(6).
      *    保険ＩＤ
           03  SKYSRH-HKNID            PIC 9(10).
      *    保険番号
           03  SKYSRH-HKNNUM           PIC X(3).
      *    公費番号１
           03  SKYSRH-KOHNUM1          PIC X(3).
      *    公費番号２
           03  SKYSRH-KOHNUM2          PIC X(3).
      *    保険点数
           03  SKYSRH-TOTALTEN         PIC 9(10).
      *    特記事項番号
           03  SKYSRH-TOKKI-G.
               05  SKYSRH-TOKKI        PIC X(2)  OCCURS 5.
      *    特記事項名称
           03  SKYSRH-TOKKINM-G.
               05  SKYSRH-TOKKINM      PIC X(12) OCCURS 5.
      *    記載外公費−薬剤一部負担金情報
           03  SKYSRH-ETC-YKZFTN1      PIC  9(05).
      *    請求区分（レセ電対象）
           03  SKYSRH-SKYKBN-RECEDEN   PIC X(1).
      *    開始診療年月
           03  SKYSRH-STSRYYM          PIC X(6).
      *    終了診療年月
           03  SKYSRH-EDSRYYM          PIC X(6).
