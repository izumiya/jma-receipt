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
      *    ＡＰＩレスポンス患者情報取得       COPY (CPPATIENTXMLRES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  12/10/01  禁忌〜コメント等追加
      *****************************************************************
        01  XML-PATIENTINFORES.
            02  PTRES-OBJECT           PIC X(8).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-PATIENTINFORES.
      *       実施日
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  PTRES-API-RESULT         PIC X(2).
      *       返却メッセージ
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *       患者情報
              03  PTRES-PATIENT-INFOMATION.
      *           患者番号
                  04  PTRES-PATIENTID          PIC X(20).
      *           漢字氏名
                  04  PTRES-NAME               PIC X(100).
      *           カナ氏名
                  04  PTRES-KANANAME           PIC X(100).
      *           生年月日
                  04  PTRES-BIRTHDAY           PIC X(10).
      *           性別
                  04  PTRES-SEX                PIC X(1).
      *!!
      *           世帯主名
                  04  PTRES-SETAI-NAME         PIC X(100).
      *           続柄
                  04  PTRES-ZOKUGARA           PIC X(60).
      *!!
      *           患者自宅情報
                  04  PTRES-ADDRESS-INFO.
      *               郵便番号
                      06  PTRES-HOME-ZIP-CODE      PIC X(7).
      *               住所
                      06  PTRES-HOME-ADDRESSES1    PIC X(200).
      *               番地番号
                      06  PTRES-HOME-ADDRESSES2    PIC X(200).
      *               自宅電話番号（昼）
                      06  PTRES-HOME-HPONE1        PIC X(15).
      *               自宅電話番号（昼）
                      06  PTRES-HOME-HPONE2        PIC X(15).
      *!!
      *           禁忌１
                  04  PTRES-TABOO-1              PIC  X(200).
      *           禁忌２
                  04  PTRES-TABOO-2              PIC  X(200).
      *           アレルギー１
                  04  PTRES-ALLERGY-1            PIC  X(200).
      *           アレルギー２
                  04  PTRES-ALLERGY-2            PIC  X(200).
      *           感染症１
                  04  PTRES-KANSENSYO-1          PIC  X(200).
      *           感染症２
                  04  PTRES-KANSENSYO-2          PIC  X(200).
      *           コメント１
                  04  PTRES-COMMENT-1            PIC  X(200).
      *           コメント２
                  04  PTRES-COMMENT-2            PIC  X(200).
      *!!
      *           保険情報
                  04  PTRES-INSURANCE-INFO     OCCURS  20 TIMES.
      *               保険組合せ番号
                      06  PTRES-COMBINATION-NUMBER     PIC X(04).
      *               保険の種類
                      06  PTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険者番号
                      06  PTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               保険の種類名称
                      06  PTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               記号
                      06  PTRES-MAIN-MARK              PIC X(80).
      *               番号
                      06  PTRES-MAIN-NUMBER            PIC X(80).
      *               継続区分
                      06  PTRES-MAIN-CONTINUATION      PIC X(01).
      *               補助区分
                      06  PTRES-MAIN-ASSISTANCE        PIC X(01).
      *               本人家族区分
                      06  PTRES-MAIN-FAMILY            PIC X(01).
      *               被保険者名
                      06  PTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               有効開始日
                      06  PTRES-MAIN-START-DATE        PIC X(10).
      *               有効終了日
                      06  PTRES-MAIN-END-DATE          PIC X(10).
      *               公費情報
                      06  PTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 公費の種類
                        07  PTRES-INSURANCE-CLASS         PIC X(03).
      *                 公費の種類名称
                        07  PTRES-INSURANCE-NAME          PIC X(20).
      *                 負担者番号
                        07  PTRES-PROVIDER-NUMBER         PIC X(08).
      *                 受給者番号
                        07  PTRES-RECIPIENT-NUMBER        PIC X(20).
      *                 入院−負担率（割）
                        07  PTRES-RATE-ADMISSION          PIC X(04).
      *                 入院−固定額
                        07  PTRES-MONEY-ADMISSION         PIC X(06).
      *                 外来−負担率（割）
                        07  PTRES-RATE-OUTPATIENT         PIC X(04).
      *                 外来−固定額
                        07  PTRES-MONEY-OUTPATIENT        PIC X(06).
      *                 有効開始日
                        07  PTRES-INSURANCE-START-DATE    PIC X(10).
      *                 有効終了日
                        07  PTRES-INSURANCE-END-DATE      PIC X(10).
      *
