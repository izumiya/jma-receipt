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
      *    患者自己負担金レセプト記載判定パラメタ                 *
      *                         COPY  (CPORCSFTNJGNCHK.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/10/08  都道府県番号追加
      ************************************************************
       01  ORCSFTNJGNCHKAREA.
      *入力値
      *    医療機関ＩＤ
           03  LNK-FTNJGNCHK-HOSPNUM         PIC 9(2).
      *    患者ＩＤ
           03  LNK-FTNJGNCHK-PTID            PIC 9(10).
      *    入外区分
           03  LNK-FTNJGNCHK-NYUGAIKBN       PIC X(01).
      *    診療年月
           03  LNK-FTNJGNCHK-SRYYM           PIC X(06).
      *    公費番号
           03  LNK-FTNJGNCHK-KOHNUM          PIC X(03).
      *    公費ＩＤ
           03  LNK-FTNJGNCHK-KOHID           PIC 9(10).
      *    都道府県番号
           03  LNK-FTNJGNCHK-PREFNUM         PIC 9(02).
      *復帰コード
      *    00:レセプト記載あり
      *    01:レセプト記載なし
      *    10:パラメタエラー
           03  LNK-FTNJGNCHK-RC              PIC 9(02).
