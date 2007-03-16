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
      *    高齢者の一部負担金額記載に関するチェックパラメタ      *
      *                         COPY  (CPORCSKOREIFTNCHK.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-門脇    06/08/10  高齢者経過措置フラグ追加
      *                                     主保険単独の患者負担額追加
      *                                     公費を含む場合の患者負担額追加
      *  03.03.01    NACL-門脇    06/11/02  減額・免除・支払猶予対応
      *  03.04.01    NACL-門脇    06/11/20  高額４回目以降対応
      *  03.04.02    NACL-門脇    07/03/16  全国公費有り無しフラグ追加
      *                                     公費を含む場合の主保険の一部負
      *                                     担額２追加
      ************************************************************
       01  ORCSKOREIFTNCHKAREA.
      *
      *入力値
      *    医療機関ＩＤ
           03  LNK-KOREIFTNCHK-HOSPID       PIC X(24).
      *    患者ＩＤ
           03  LNK-KOREIFTNCHK-PTID         PIC 9(10).
      *    入外区分
           03  LNK-KOREIFTNCHK-NYUGAIKBN    PIC X(01).
      *    診療年月
           03  LNK-KOREIFTNCHK-SRYYM        PIC X(06).
      *    主保険−保険番号
           03  LNK-KOREIFTNCHK-HKNNUM       PIC X(03).
      *    主保険−保険ＩＤ
           03  LNK-KOREIFTNCHK-HKNID        PIC 9(10).
      *    低所得フラグ
           03  LNK-KOREIFTNCHK-TEISYOTOKU   PIC X(01).
      *    レセプト科
           03  LNK-KOREIFTNCHK-RECEKA       PIC X(02).
      *    高齢者経過措置フラグ
           03  LNK-KOREIFTNCHK-KEIKASOCHI   PIC X(01).
      *    減額区分
           03  LNK-KOREIFTNCHK-GENGAKUKBN   PIC 9(01).
      *    減額（割合）
           03  LNK-KOREIFTNCHK-GENGAKU-RITU PIC 9(03).
      *    減額（金額）
           03  LNK-KOREIFTNCHK-GENGAKU-KIN  PIC 9(07).
      *    高額４回目以降
           03  LNK-KOREIFTNCHK-KOHYONKAIME  PIC X(01).
      *    全国公費有り無しフラグ
           03  LNK-KOREIFTNCHK-ZENKOHARI    PIC X(01).
      *
      *返却値
           03  LNK-KOREIFTNCHK-OUT-AREA.
      *主保険単独の患者負担額
             05 LNK-KOREIFTNCHK-FTNGAKU     PIC 9(07).
      *公費を含む場合の患者負担額
             05 LNK-KOREIFTNCHK-KOHFTNGAKU  PIC 9(07).
      *公費を含む場合の主保険の一部負担額
             05 LNK-KOREIFTNCHK-ICHIBUGAKU  PIC 9(07).
      *公費を含む場合の主保険の一部負担額２
             05 LNK-KOREIFTNCHK-ICHIBUGAKU2 PIC 9(07).
      *入院：高齢者２割＋地方公費の請求金額
             05 LNK-KOREIFTNCHK-CHIHOUGAKU  PIC 9(07).
      *
      *復帰コード
           03  LNK-KOREIFTNCHK-RC           PIC 9(02).
