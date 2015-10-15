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
      *    ＡＰＩ診療内容編集サブパラメタ（一体化）
      *                                         (CPORAPI021SUB2V3.INC)
      ******************************************************************
       01  ORAPI021SUB2V3AREA.
      *    処理区分
           03  ORAPISUB2-SYORIKBN      PIC X(01).
      *    テーブル件数
           03  ORAPISUB2-WKSRYACT-MAX  PIC 9(04).
      *
      *返却
           03  ORAPISUB2-HENSYU-AREA.
      *        診療内容返却
               05  ORAPISUB2-MEDICAL-INF-G.
                 07  ORAPISUB2-MDC-INFO          OCCURS   120.
      *             診療区分
                    09  ORAPISUB2-MDC-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  ORAPISUB2-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  ORAPISUB2-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    09  ORAPISUB2-MDC-CLASS-NUMBER      PIC X(03).
      *             剤点数
                    09  ORAPISUB2-MDC-POINT             PIC X(08).
      *             剤金額
                    09  ORAPISUB2-MDC-MOENY             PIC X(08).
      *             剤区分（１：包括剤）
                    09  ORAPISUB2-MDC-CODE              PIC X(01).
      *
      *             剤削除可能連番
                    09  ORAPISUB2-DEL-NUMBER            PIC X(02).
      *             剤削除区分
                    09  ORAPISUB2-DEL-INF               PIC X(01).
      *
                    09  ORAPISUB2-PRSCRPT-INFO-G.
                      10  ORAPISUB2-PRSCRPT-INFO    OCCURS   50.
      *                 診療コード
                        11  ORAPISUB2-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  ORAPISUB2-PRSCRPT-NAME          PIC X(80).
      *                 数量
                        11  ORAPISUB2-PRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        11  ORAPISUB2-PRSCRPT-MOENY         PIC X(07).
      *                 コメント埋め込み値
                        11  ORAPISUB2-PRSCRPT-ATAI-G.
                            13  ORAPISUB2-PRSCRPT-ATAI      PIC X(08)
                                                         OCCURS   5.
      *                 一般名記載区分
                        11  ORAPISUB2-PRSCRPT-GENERIC       PIC X(01).
      *                 一般名
                        11  ORAPISUB2-PRSCRPT-GENNAME       PIC X(200).
      *                 継続指示区分
                        11  ORAPISUB2-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  ORAPISUB2-PRSCRPT-NAIFKBN       PIC X(01).
      *                 自動区分
                        11  ORAPISUB2-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *    エラー区分
           03  ORAPISUB2-ERRCD         PIC X(04).
           03  ORAPISUB2-ERRMSG        PIC X(100).
      *
