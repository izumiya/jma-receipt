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
      *-------------------------------------------------*
      *    ワーク診療行為マスタ    COPY (CPWKSRYACT.INC)
      *
      *-------------------------------------------------*
      *01  WKSRYACT-REC.
           03  WKSRY-KEY.
      *医療機関ＩＤ
               05  WKSRY-HOSPID         PIC  X(24).
      *入外区分
               05  WKSRY-NYUGAIKBN      PIC  X(01).
      *患者ＩＤ
               05  WKSRY-PTID           PIC  9(10).
      *診療科
               05  WKSRY-SRYKA          PIC  X(02).
      *診療日
               05  WKSRY-SRYYMD         PIC  X(08).
      *保険組合せ
               05  WKSRY-HKNCOMBI       PIC  9(04).
      *剤番号
               05  WKSRY-ZAINUM         PIC  9(08).
      *連番号
               05  WKSRY-RENNUM         PIC  9(02).
      *ドクターコード
           03  WKSRY-DRCD               PIC  X(05).
      *診療種別区分
           03  WKSRY-SRYSYUKBN          PIC  X(03).
      *診療区分
           03  WKSRY-SRYKBN             PIC  X(02).
      *自費金額
           03  WKSRY-JIHIMONEYTOTAL     PIC  9(07).
      *診療行為テーブル
           03  WKSRY-SINRYO-TBL-G.
             04    WKSRY-SINRYO-TBL      OCCURS   5.
               05  WKSRY-SRYCD           PIC  X(09).
               05  WKSRY-SRYSURYO        PIC  9(05)V9(03).
               05  WKSRY-SRYKAISU        PIC  9(03).
               05  WKSRY-ZAIKAISU        PIC  9(08).
      **自動算定区分
               05  WKSRY-AUTOKBN        PIC  X(01).
      **入力コメント番号
               05  WKSRY-INPUTNUM       PIC  9(03).
      **入力コメント
               05  WKSRY-INPUTCOMENT    PIC  X(80).
      **入力値
               05  WKSRY-INPUTCHI-G.
                   07  WKSRY-INPUTCHI   PIC  X(08)   OCCURS  4.
      *入力コード
               05  WKSRY-INPUTCD        PIC  X(22).
      *自費金額
               05  WKSRY-JIHIMONEY      PIC  9(07).
      *剤点数計
           03  WKSRY-ZAITENKEI          PIC  9(08).
      *剤回数計
           03  WKSRY-ZAIKAIKEI          PIC  9(07).
      *手技点数１
           03  WKSRY-SYUTEN1             PIC  9(07).
      *手技点数２
           03  WKSRY-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  WKSRY-YKZTEN              PIC  9(07).
      *器材点数
           03  WKSRY-KIZAITEN            PIC  9(07).
      *同時診療伝票番号
           03  WKSRY-DOUJI-DENPNUM      PIC  9(07).
      *同時診療連番
           03  WKSRY-DOUJI-RENNUM       PIC  9(01).
      *継続区分
           03  WKSRY-CONTKBN            PIC  X(01).
      *更新中フラグ
           03  WKSRY-MOD-FLG            PIC  9(01).
      *電子カルテフラグ
           03  WKSRY-KARTE-FLG          PIC  9(01).
      *電子カルテキー
           03  WKSRY-KARTE-KEY          PIC  X(36).
      *端末ＩＤ
           03  WKSRY-TERMID             PIC  X(16).
      *オペレータＩＤ
           03  WKSRY-OPID               PIC  X(16).
      *作成年月日
           03  WKSRY-CREYMD             PIC  X(08).
      *更新年月日
           03  WKSRY-UPYMD              PIC  X(08).
      *更新時間
           03  WKSRY-UPHMS              PIC  X(06).
      *
