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
      *    一括再計算該当者チェック用パラメタ   COPY (CPORCSSYUNOUCHK.INC)
      *                    使用プログラム            (ORCSSYUNOUCHK)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  LNK-ORCSSYUNOUCHK-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSSYUNOUCHK-HOSPNUM        PIC 9(2).
      *入外区分
           03  LNK-ORCSSYUNOUCHK-NYUGAIKBN      PIC X(01).
      *診療年月
           03  LNK-ORCSSYUNOUCHK-SRYYM          PIC X(06).
      *
      *返却値
      *
           03  LNK-ORCSSYUNOUCHK-OUT-AREA.
             05  LNK-ORCSSYUNOUCHK-OUT-OCC      OCCURS 10000.
      *患者ＩＤ
               07  LNK-ORCSSYUNOUCHK-PTID       PIC 9(10).
      *一括再計算フラグ（※９は自動一括再計算あり）
      *　　０：患者請求額の変更がある方
      *　　９：患者請求額の変更がない方（収納内部項目の変更あり）
               07  LNK-ORCSSYUNOUCHK-IKTFLG     PIC 9(01).
      *該当区分
      *　　１：第三者行為現物給付：通常診療分を優先した計算が必要
      *　　２：７０歳未満現物給付：公費併用分を優先した計算が必要
      *　　３：高齢者現物給付：公費併用分を優先した計算が必要
      *　　４：該当者チェック抽出分：（要因）請求点数の増減
      *　　５：該当者チェック抽出分：（要因）患者情報や公費情報の設定変更等
      *
      *　　６：該当者チェック抽出分：（要因）請求点数の増減
      *　　７：該当者チェック抽出分：（要因）患者情報や公費情報の設定変更等
               07  LNK-ORCSSYUNOUCHK-GAITOKBN   PIC 9(02).
      *
      *復帰コード
      *
           03  LNK-ORCSSYUNOUCHK-RC             PIC 9(02).
      *
