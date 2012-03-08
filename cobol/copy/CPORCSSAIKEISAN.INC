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
      *    負担金再計算用パラメタ　　　　　 COPY (CPORCSSAIKEISAN.INC)
      *                                使用プログラム  (ORCSSAIKEISAN)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-門脇    09/06/16  一括再計算順、一括再計算方法追加
      * 01.00.02     NACL-門脇    12/03/09  現物給付計算フラグ追加
      ************************************************************
       01  LNK-ORCSSAIKEISAN-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSSKS-HOSPNUM         PIC 9(2).
      *入外区分
           03  LNK-ORCSSKS-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSSKS-PTID            PIC  9(10).
      *対象伝票番号
           03  LNK-ORCSSKS-DENPNUM         PIC  9(07).
      *診療科
           03  LNK-ORCSSKS-SRYKA           PIC  X(02).
      *一括再計算順
           03  LNK-ORCSSKS-IKTSORTKBN      PIC  X(01).
      *一括再計算方法
           03  LNK-ORCSSKS-IKTKEISANKBN    PIC  X(01).
      *一括再計算；現物給付計算フラグ
           03  LNK-ORCSSKS-IKTGENKEIFLG    PIC  X(01).
      *
      *返却値
      *
           03  LNK-ORCSSKS-OUT-AREA.
      *計算伝票番号
               05  LNK-ORCSSKS-DENPNUM-END    PIC  9(07)
                                           OCCURS  15.
      *
