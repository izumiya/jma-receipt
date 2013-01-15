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
      *    データチェック確認項目        COPY (CPDTCHKKOMOKU.INC)*
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHKKOMOKU-AREA.
           03  DTCHKKOMOKU-MAX             PIC 9(03)   VALUE   19.
           03  DTCHKKOMOKU-VAL.
               05                          PIC X(02)   VALUE
               "01".
               05                          PIC X(40)   VALUE
               "保険者番号".
               05                          PIC X(02)   VALUE
               "02".
               05                          PIC X(40)   VALUE
               "被保険者記号・番号".
               05                          PIC X(02)   VALUE
               "03".
               05                          PIC X(40)   VALUE
               "公費負担者番号・受給者番号".
               05                          PIC X(02)   VALUE
               "04".
               05                          PIC X(40)   VALUE
               "保険適応年齢".
               05                          PIC X(02)   VALUE
               "05".
               05                          PIC X(40)   VALUE
               "患者病名".
               05                          PIC X(02)   VALUE
               "06".
               05                          PIC X(40)   VALUE
               "診療開始日".
               05                          PIC X(02)   VALUE
               "07".
               05                          PIC X(40)   VALUE
               "疑い病名・急性病名".
               05                          PIC X(02)   VALUE
               "08".
               05                          PIC X(40)   VALUE
               "薬剤と病名".
               05                          PIC X(02)   VALUE
               "09".
               05                          PIC X(40)   VALUE
               "診療行為と病名".
               05                          PIC X(02)   VALUE
               "10".
               05                          PIC X(40)   VALUE
               "薬剤と併用禁忌".
               05                          PIC X(02)   VALUE
               "11".
               05                          PIC X(40)   VALUE
               "診療行為の併用算定".
               05                          PIC X(02)   VALUE
               "12".
               05                          PIC X(40)   VALUE
               "実日数".
               05                          PIC X(02)   VALUE
               "13".
               05                          PIC X(40)   VALUE
               "患者氏名".
               05                          PIC X(02)   VALUE
               "14".
               05                          PIC X(40)   VALUE
               "保険組合せ有効期間".
               05                          PIC X(02)   VALUE
               "15".
               05                          PIC X(40)   VALUE
               "投与禁忌薬剤と病名".
               05                          PIC X(02)   VALUE
               "16".
               05                          PIC X(40)   VALUE
               "診療行為どうしの算定漏れ".
               05                          PIC X(02)   VALUE
               "17".
               05                          PIC X(40)   VALUE
               "病名と薬剤".
               05                          PIC X(02)   VALUE
               "18".
               05                          PIC X(40)   VALUE
               "病名と診療行為".
               05                          PIC X(02)   VALUE
               "19".
               05                          PIC X(40)   VALUE
               "薬剤の月上限回数・総投与量".
           03  DTCHKKOMOKU-R   REDEFINES   DTCHKKOMOKU-VAL.
               05  DTCHKKOMOKU-CHKKOMOKU-OCC    OCCURS  19.
                   07  DTCHKKOMOKU-CHKNUM       PIC X(02).
                   07  DTCHKKOMOKU-CHKKOMOKU    PIC X(40).
