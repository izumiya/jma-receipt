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
      *         入院料取得サブ      COPY   (CPORCSNYUINRYO.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SNYUINRYO-AREA.
      *    診療日
           03  SNYUINRYO-SRYYMD            PIC X(08).
      *    処理区分 (01:入院）
           03  SNYUINRYO-SYORIKBN          PIC X(02).
      *    病棟番号
           03  SNYUINRYO-BTUNUM            PIC X(02).
      *    病室番号
           03  SNYUINRYO-BRMNUM            PIC X(06).
      *    保険組合せ
           03  SNYUINRYO-HKNCOMBI          PIC 9(04).
      *
      *    入院料件数
           03  SNYUINRYO-MAX               PIC 9(05).
           03  SNYUINRYO-OCC               OCCURS  100.
      *
      *        連番
               05  SNYUINRYO-NO            PIC X(02).
      *        名称
               05  SNYUINRYO-NAME          PIC X(60).
      *        内部コード
               05  SNYUINRYO-NYUINRYO-G.
                   07  SNYUINRYO-NYUINRYO-KBN
                                           PIC X(01).
                   07  SNYUINRYO-NYUINRYO-CD
                                           PIC X(02).
      *        入院料コード
               05  SNYUINRYO-SRYCD         PIC X(09).
      *    診療コード合計
           03  SNYUINRYO-SRYCDTTL          PIC 9(14).
      *    病棟設定入院料コード
           03  SNYUINRYO-BTU-KHNSRYCD      PIC X(09).
