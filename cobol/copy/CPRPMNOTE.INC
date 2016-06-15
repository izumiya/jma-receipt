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
       01  RPMNOTE-REC.
           03  RPMNOTE-CONTEXT                     PIC S9(9)   BINARY.
           03  RPMNOTE-OBJECT                      PIC X(8).
           03  RPMNOTE-MODE                        PIC S9(9)   BINARY.
           03  RPMNOTE-RECORDNAME                  PIC X(128).
           03  RPMNOTE-DATA.
      *            帳票ＩＤ
               05  RPMNOTE-REPORTID                PIC X(30).
      *            プリンタ名
               05  RPMNOTE-PRINTER                 PIC X(20).
      *            診療日 YYYY-MM-DD
               05  RPMNOTE-SRYYMD                  PIC X(10).
      *            伝票番号
               05  RPMNOTE-DENPNUM                 PIC X(7).
      *            患者情報
               05  RPMNOTE-PTINF-AREA.
      *                患者番号
                   07  RPMNOTE-PTNUM               PIC X(20).
      *                氏名
                   07  RPMNOTE-NAME                PIC X(50).
      *                カナ氏名
                   07  RPMNOTE-KANANAME            PIC X(50).
      *                誕生日
                   07  RPMNOTE-BIRTHDAY            PIC X(10).
      *                性別区分
                   07  RPMNOTE-SEX                 PIC X(1).
      *            医療機関情報
               05  RPMNOTE-HOSP-AREA.
      *                県番
                   07  RPMNOTE-PREFNUM             PIC X(2).
      *                名称
                   07  RPMNOTE-HOSP-NAME-G.
                       09  RPMNOTE-HOSP-NAME       PIC X(120)
                                                   OCCURS   3.
      *                住所
                   07  RPMNOTE-HOSP-ADRS-G.
                       09  RPMNOTE-HOSP-ADRS       PIC X(200)
                                                   OCCURS   3.
      *                電話番号
                   07  RPMNOTE-HOSP-TEL            PIC X(15).
               05  RPMNOTE-DR-AREA.
      *                医師コード
                   07  RPMNOTE-DR-CODE             PIC X(5).
      *                医師名
                   07  RPMNOTE-DR-NAME             PIC X(50).
               05  RPMNOTE-ZAI-AREA                OCCURS  50 TIMES.
      *                診療区分
                   07  RPMNOTE-SRYKBN              PIC X(2).
      *                剤回数
                   07  RPMNOTE-ZAIKAISU            PIC X(3).
      *                剤単位名（回数／回分）
                   07  RPMNOTE-ZAITANINAME         PIC X(12).
                   07  RPMNOTE-MEDICATION          OCCURS  50 TIMES.
      *                薬剤名
                       09  RPMNOTE-YKZNAME         PIC X(80).
      *                数量(ZZZZ9.99999)
                       09  RPMNOTE-SURYO           PIC X(12).
      *                単位名
                       09  RPMNOTE-TANINAME        PIC X(12).
      *                診療コード
                       09  RPMNOTE-SRYCD           PIC X(9).
      *        備考
               05  RPMNOTE-MEMO    PIC X(80)       OCCURS  10 TIMES.
