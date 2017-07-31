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
        01  FMSEIKYUN-REC.
            03  FMSEIKYUN-CONTEXT              PIC S9(9)   BINARY.
            03  FMSEIKYUN-OBJECT               PIC X(8).
            03  FMSEIKYUN-MODE                 PIC S9(9)   BINARY.
            03  FMSEIKYUN-RECORDNAME           PIC X(128).
            03  FMSEIKYUN-DATA.
              05  FMSEIKYUN-FORM-ID            PIC X(30).
              05  FMSEIKYUN-CUSTOM-ID          PIC X(30).
              05  FMSEIKYUN-PRINTER            PIC X(20).
      *
              05  FMSEIKYUN-MOTOID             PIC X(02).
              05  FMSEIKYUN-KARIKEISANKBN      PIC X(01).
      *
      *       患者番号
              05  FMSEIKYUN-PTNUM              PIC X(20).
      *       患者氏名
              05  FMSEIKYUN-NAME               PIC X(50).
      *       患者カナ氏名
              05  FMSEIKYUN-KANANAME           PIC X(50).
      *
      *       医療機関情報
              05  FMSEIKYUN-HOSP-AREA.
      *         医療機関名称
                07  FMSEIKYUN-HOSP-NAME-G.
                  09  FMSEIKYUN-HOSP-NAME      PIC X(120)
                                               OCCURS   3.
      *         医療機関郵便番号
                07  FMSEIKYUN-HOSP-POST        PIC X(7).
      *         医療機関住所
                07  FMSEIKYUN-HOSP-ADRS-G.
                  09  FMSEIKYUN-HOSP-ADRS      PIC X(200)
                                               OCCURS   3.
      *         医療機関電話番号
                07  FMSEIKYUN-HOSP-TEL         PIC X(15).
      *
      *       実日数
              05  FMSEIKYUN-JNISSU             PIC X(03).
      *       診療期間区分
              05  FMSEIKYUN-SRYKIKANKBN        PIC X(01).
      *       診療年月
              05  FMSEIKYUN-SRYYM              PIC X(07).
      *       開始診療日
              05  FMSEIKYUN-STSRYYMD           PIC X(10).
      *       終了診療日
              05  FMSEIKYUN-EDSRYYMD           PIC X(10).
      *       伝票発行日
              05  FMSEIKYUN-DENPPRTYMD         PIC X(10).
      *       病棟番号
              05  FMSEIKYUN-BTUNUM             PIC X(2).
      *       病棟名称
              05  FMSEIKYUN-BTUNAME            PIC X(20).
      *       病室番号
              05  FMSEIKYUN-BRMNUM             PIC X(6).
      *
      *       診療科
              05  FMSEIKYUN-SRYKANAME          PIC X(30).
      *       保険情報
              05  FMSEIKYUN-HOKENNAME          PIC X(40).
              05  FMSEIKYUN-FUTAN-G.
      *         負担割合
                07  FMSEIKYUN-FUTAN            PIC X(04).
      *         負担割合
                07  FMSEIKYUN-FUTANMEI         PIC X(02).
      *       伝票番号（一括以外）
              05  FMSEIKYUN-DENPNUM            PIC X(07).
      *       訂正表示区分
              05  FMSEIKYUN-TEISEI-KBN         PIC X(01).
      *       請求項目
              05  FMSEIKYUN-MEISAI-OCC         OCCURS 16.
      *           請求項目区分
                  07  FMSEIKYUN-HKNTEN-KBN     PIC X(03).
      *           請求項目名
                  07  FMSEIKYUN-HKNTEN-NAME    PIC X(20).
      *           請求点数
                  07  FMSEIKYUN-HKNTEN         PIC X(08).
      *           請求金額（保険適用外）
                  07  FMSEIKYUN-TGMONEY        PIC X(08).
      *       点数合計
              05  FMSEIKYUN-TOTAL-TEN          PIC X(08).
      *       患者一部負担金
              05  FMSEIKYUN-HKNTEKMONEY                 PIC X(08).
      *       患者一部負担金ラベル（労災診療料等再掲）
              05  FMSEIKYUN-LBL-RSI-TOTAL               PIC X(20).
      *       患者一部負担金（労災診療料等再掲）
              05  FMSEIKYUN-RSI-TOTAL                   PIC X(08).
      *       食事・生活負担金
              05  FMSEIKYUN-SKJ-FTN                     PIC X(08).
      *       公費一部負担金
              05  FMSEIKYUN-KOH-FTN                     PIC X(08).
      *       保険適用外合計
              05  FMSEIKYUN-TOTAL-TGMONEY               PIC X(08).
      *       保険適用外合計消費税再掲
              05  FMSEIKYUN-TOTAL-TGMONEY-TAX           PIC X(08).
      *       その他自費
              05  FMSEIKYUN-JIHI-OCC                    OCCURS  10.
      *             その他自費名称
                07  FMSEIKYUN-JIHI-NAME                 PIC X(20).
      *             その他自費金額
                07  FMSEIKYUN-JIHI                      PIC X(08).
      *       その他自費合計金額
              05  FMSEIKYUN-JIHI-TOTAL                  PIC X(08).
      *       その他自費合計金額消費税再掲
              05  FMSEIKYUN-JIHI-TOTAL-TAX              PIC X(08).
      *       室料差額
              05  FMSEIKYUN-RMSAGAKU                    PIC X(08).
      *       調整金１
              05  FMSEIKYUN-CHOSEI1                     PIC X(08).
      *       調整金２
              05  FMSEIKYUN-CHOSEI2                     PIC X(08).
      *       今回請求額
              05  FMSEIKYUN-SKYMONEY                    PIC X(08).
      *       今回請求額消費税再掲
              05  FMSEIKYUN-SKYMONEY-TAX                PIC X(08).
      *       今回入金額
              05  FMSEIKYUN-NYUKIN                      PIC X(08).
      *       入院未収額
              05  FMSEIKYUN-NYU-MISYU                   PIC X(08).
      *       外来未収額
              05  FMSEIKYUN-GAI-MISYU                   PIC X(08).
      *
      *       注意書き（請求書部分）
              05  FMSEIKYUN-SKY-CAUTION                 PIC X(100).
      *
      *       メモ
              05  FMSEIKYUN-MEMO                        PIC X(80)
                                                        OCCURS 10.
