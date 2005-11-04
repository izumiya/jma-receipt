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
      *    レセプト明細書     COPY (CPRCF046.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.03  剤番号桁修正
      ************************************************************
      *01  RECE46-REC.
           03  RECE46-KEY.
      *医療機関ＩＤ
               05  RECE46-HOSPID          PIC  X(24).
      *診療年月
               05  RECE46-SRYYM           PIC  9(06).
      *入外区分
               05  RECE46-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE46-PTID            PIC  9(10).
      *レセプト科
               05  RECE46-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE46-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE46-TEKSTYMD        PIC  X(08). 
      *剤番号
               05  RECE46-ZAINUM          PIC  9(08).
      *連番号
               05  RECE46-LINENUM         PIC  9(05).
      *剤明細数
           03  RECE46-ZAIMEISAISU         PIC  9(03).
      *患者番号
           03  RECE46-PTNUM               PIC  X(20).
      *診療科
           03  RECE46-SRYKA               PIC  X(02).
      *診療区分
           03  RECE46-SRYKBN              PIC  X(02).
      *適要欄情報
           03  RECE46-TEKIYO-INF.
      *剤区切（＊）
               05  RECE46-ASTER           PIC  X(02).
      *摘要
               05  RECE46-TEKIYO          PIC  X(100).
      *剤点数
           03  RECE46-ZAITEN              PIC  9(08).
      *検査実施区分
           03  RECE46-KNSJISKBN           PIC  9(01).
      *表用番号
           03  RECE46-CDKBN-SYO           PIC  9(01).
           03  RECE46-CDKBN-BU            PIC  9(02).
           03  RECE46-CDKBN-KBN           PIC  X(01).
           03  RECE46-CDKBN-KBNNUM        PIC  9(03).
           03  RECE46-CDKBN-KBNNUM-EDA    PIC  9(02).
           03  RECE46-CDKBN-KOUBAN        PIC  9(02).
           03  RECE46-KOUHYOJUNNUM        PIC  9(09).
      *（サ）フラグ
           03  RECE46-SAFLG               PIC  X(01).
