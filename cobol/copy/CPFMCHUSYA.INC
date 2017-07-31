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
      ******************************************************************
      *    ＡＰＩ　入院注射指示箋帳票情報   COPY (CPFMCHUSYA.INC)
      ******************************************************************
        01  FMCHUSYA-REC.
            03  FMCHUSYA-CONTEXT                PIC S9(9)   BINARY.
            03  FMCHUSYA-OBJECT                 PIC X(8).
            03  FMCHUSYA-MODE                   PIC S9(9)   BINARY.
            03  FMCHUSYA-RECORDNAME             PIC X(128).
            03  FMCHUSYA-DATA.
              05  FMCHUSYA-FORM-ID              PIC X(30).
              05  FMCHUSYA-CUSTOM-ID            PIC X(30).
              05  FMCHUSYA-PRINTER              PIC X(20).
      *       処方日
              05  FMCHUSYA-SYSYMD               PIC X(10).
      *       処方時刻
              05  FMCHUSYA-SYSTIME              PIC X(5).
      *       指示日
              05  FMCHUSYA-SRYYMD               PIC X(10).
      *       診療科（日レセ）
              05  FMCHUSYA-SRYKA                PIC X(2).
              05  FMCHUSYA-SRYKANAME            PIC X(40).
      *       病棟番号
              05  FMCHUSYA-BTUNUM               PIC X(2).
              05  FMCHUSYA-BTUNAME              PIC X(20).
              05  FMCHUSYA-BRMNUM               PIC X(6).
      *       患者情報
              05  FMCHUSYA-PTINF.
                07  FMCHUSYA-PTNUM              PIC X(20).
                07  FMCHUSYA-NAME               PIC X(50).
                07  FMCHUSYA-KANANAME           PIC X(50).
                07  FMCHUSYA-BIRTHDAY           PIC X(10).
                07  FMCHUSYA-SEX                PIC X(1).
      *         年齢（全角）
                07  FMCHUSYA-NENREI             PIC X(08).
      *       ドクター
              05  FMCHUSYA-DOCTOR-AREA.
                07  FMCHUSYA-DR-CODE             PIC X(5).
                07  FMCHUSYA-DR-NAME             PIC X(50).
                07  FMCHUSYA-DR-KANANAME         PIC X(50).
                07  FMCHUSYA-DR-MAYAKULICENSE    PIC X(15).
      *
      *       帳票タイトル
              05  FMCHUSYA-TITLE                 PIC X(30).
      *       1:麻薬を記載した指示せん
              05  FMCHUSYA-MAYAKU                PIC X(1).
      *       回数連番（２回以上の時のみ）
              05  FMCHUSYA-KAI-RENNUM            PIC X(2).
      *       回数編集（Ｎ回目）
              05  FMCHUSYA-KAI-HENSYU            PIC X(10).
      *
              05  FMCHUSYA-ZAI-AREA.
                07  FMCHUSYA-SRYKBN              PIC X(2).
      *         診療種別区分
                07  FMCHUSYA-SRYSYUKBN           PIC X(3).
      *         注射名称
                07  FMCHUSYA-CHUSYA-NAME         PIC X(100).
      *         手技料コード（ある場合のみ）
                07  FMCHUSYA-CHUSYA-SRYCD        PIC X(9).
      *
                07  FMCHUSYA-MEDICATION          OCCURS  50 TIMES.
      *           薬剤名
                  09  FMCHUSYA-YKZNAME           PIC X(200).
      *           数量
                  09  FMCHUSYA-SURYO             PIC X(12).
      *           単位名称
                  09  FMCHUSYA-TANINAME          PIC X(12).
      *           コード
                  09  FMCHUSYA-SRYCD             PIC X(9).
              05  FMCHUSYA-MEMO                  PIC X(110)
                                                 OCCURS  3  TIMES.
              05  FMCHUSYA-OPID                  PIC X(16).
