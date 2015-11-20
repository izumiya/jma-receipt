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
      * 04.08.00     NACL-多々納  15/11/XX  エラー診療コード追加
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ中途終了データ編集サブパラメタ（一体化）
      *                                         (CPORAPI021SUB1V3.INC)
      ******************************************************************
       01  ORAPI021SUB1V3AREA.
      *    処理区分
           03  ORAPISUB01-SYORIKBN      PIC X(03).
      *    時間外加算区分
           03  ORAPISUB01-TIMEFLG       PIC X(01).
      *    保険情報
           03  ORAPISUB01-HKNCOMBI-AREA.
      *        保険組合せ番号
               05  ORAPISUB01-HKNCOMBI     PIC X(04).
      *
      *    診療内容
           03  ORAPISUB01-SRYACT-AREA.
               05  ORAPISUB01-MDC-INFO         OCCURS  110 TIMES.
      *            診療種別区分
                   07  ORAPISUB01-MDC-CLASS-KBN    PIC X(2).
      *            診療種別区分
                   07  ORAPISUB01-MDC-CLASS        PIC X(3).
      *            診療区分名称
                   07  ORAPISUB01-MDC-CLASS-NAME   PIC X(40).
      *            回数
                   07  ORAPISUB01-MDC-CLASS-NUMBER PIC X(3).
      *            剤内容
                   07  ORAPISUB01-PRESCRIPTION-INFO    OCCURS  50 TIMES.
      *                診療コード
                       09  ORAPISUB01-PRSCRPT-CODE     PIC X(9).
      *                診療名称（コメント）
                       09  ORAPISUB01-PRSCRPT-NAME     PIC X(80).
      *                数量
                       09  ORAPISUB01-PRSCRPT-NUMBER   PIC X(11).
      *                自費金額
                       09  ORAPISUB01-PRSCRPT-MONEY    PIC X(07).
      *                コメント埋め込み値
                       09  ORAPISUB01-ATAI-G.
                           11  ORAPISUB01-ATAI         PIC X(08)
                                                       OCCURS   5.
      *                継続指示区分
                       09  ORAPISUB01-CONTKBN          PIC X(01).
      *                内服種類数指示区分
                       09  ORAPISUB01-NAIFKBN          PIC X(01).
      *                自動区分
                       09  ORAPISUB01-AUTOKBN          PIC X(01).
      *
      *    エラー区分
           03  ORAPISUB01-ERRCD        PIC X(02).
           03  ORAPISUB01-ERRMSG       PIC X(100).
      *    コード毎のエラー
           03  ORAPISUB01-INP-ERR-G.
               05  ORAPISUB01-IN-ERR-TBL   OCCURS   200.
                   07  ORAPISUB01-IN-ERR       PIC X(03).
                   07  ORAPISUB01-IN-ERRMSG    PIC X(100).
      *            エラー剤番
                   07  ORAPISUB01-IN-POS       PIC 9(03).
      *            エラー行番
                   07  ORAPISUB01-IN-LINE      PIC 9(02).
      *            エラー診療コード
                   07  ORAPISUB01-IN-SRYCD     PIC X(09).
      *
