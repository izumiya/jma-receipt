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
      *    データチェックエラーメッセージ COPY (CPDTCHKERRMES.INC)*
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ERRMES-AREA.
      *
         03  ERRMES-VAL.
      *
      *    ** 000xxx 基本チェック
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "000001".
           05 PIC  X(30)   VALUE   "患者情報が参照できません      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 010xxx 保険者番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "010001".
           05 PIC  X(30)   VALUE   "保険者番号が未入力です        ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 020xxx 被保険者記号・番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020001".
           05 PIC  X(30)   VALUE   "保険情報の記号・番号が未入力で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020002".
           05 PIC  X(30)   VALUE   "保険情報の記号に全角文字以外が".
           05 PIC  X(30)   VALUE   "入力されています              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020003".
           05 PIC  X(30)   VALUE   "保険情報の番号に全角文字以外が".
           05 PIC  X(30)   VALUE   "入力されています              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020004".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、保険情報".
           05 PIC  X(30)   VALUE   "の記号・番号の文字数の和が１９".
           05 PIC  X(30)   VALUE   "文字を超えています            ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020005".
           05 PIC  X(30)   VALUE   "保険情報の番号が未入力です    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020006".
           05 PIC  X(30)   VALUE   "保険情報の記号に外字が入力され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020007".
           05 PIC  X(30)   VALUE   "保険情報の番号に外字が入力され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "020008".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、保険情報".
           05 PIC  X(30)   VALUE   "の記号に空白が入力されています".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
      *
      *    ** 030xxx 公費負担者番号・受給者番号抜け
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030001".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号が未入力で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030002".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号が未入力で".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030003".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に全角文字".
           05 PIC  X(30)   VALUE   "と半角文字が混在しています    ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030004".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に全角文字".
           05 PIC  X(30)   VALUE   "と半角文字が混在しています    ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030005".
           05 PIC  X(30)   VALUE   "公費情報の負担者番号に外字が入".
           05 PIC  X(30)   VALUE   "力されています                ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "030006".
           05 PIC  X(30)   VALUE   "公費情報の受給者番号に外字が入".
           05 PIC  X(30)   VALUE   "力されています                ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 040xxx 保険適応年齢
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040001".
           05 PIC  X(30)   VALUE   "１５歳未満ですが本人で登録され".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040002".
           05 PIC  X(30)   VALUE   "老人保健の適応年齢誤りです    ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040003".
           05 PIC  X(30)   VALUE   "老人保健が未設定です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "040004".
           05 PIC  X(30)   VALUE   "老人の負担割合が２割です。３割".
           05 PIC  X(30)   VALUE   "に登録し直して下さい          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 050xxx 患者病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050001".
           05 PIC  X(30)   VALUE   "病名が未登録です              ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050002".
           05 PIC  X(30)   VALUE   "診療年月に有効な入院病名が存在".
           05 PIC  X(30)   VALUE   "しません                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050003".
           05 PIC  X(30)   VALUE   "診療年月に有効な外来病名が存在".
           05 PIC  X(30)   VALUE   "しません                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050004".
           05 PIC  X(30)   VALUE   "患者病名情報テーブルの病名が未".
           05 PIC  X(30)   VALUE   "設定です                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050005".
           05 PIC  X(30)   VALUE   "病名に全角文字以外が入力されて".
           05 PIC  X(30)   VALUE   "います                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050006".
           05 PIC  X(30)   VALUE   "患者病名情報テーブルの病名文字".
           05 PIC  X(30)   VALUE   "数が未設定です                ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050007".
           05 PIC  X(30)   VALUE   "病名に対応する病名コードが入力".
           05 PIC  X(30)   VALUE   "されていません                ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050008".
           05 PIC  X(30)   VALUE   "転帰区分（転帰日）が設定されて".
           05 PIC  X(30)   VALUE   "いる場合、転帰日（転帰区分）も".
           05 PIC  X(30)   VALUE   "設定されていなければなりません".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050009".
           05 PIC  X(30)   VALUE   "カルテ病名に全角文字以外が入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050010".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、病名文字".
           05 PIC  X(30)   VALUE   "数が１７文字を超えています    ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "050011".
           05 PIC  X(30)   VALUE   "レセ電算データの場合、病名文字".
           05 PIC  X(30)   VALUE   "数が２０文字を超えています    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 060xxx 診療開始日
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "060001".
           05 PIC  X(30)   VALUE   "初診算定日から開始になる病名が".
           05 PIC  X(30)   VALUE   "存在しません                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060002".
           05 PIC  X(30)   VALUE   "初診算定日以前より継続中の病名".
           05 PIC  X(30)   VALUE   "が存在します                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "060003".
           05 PIC  X(30)   VALUE   "初診料が算定可能か確認してくだ".
           05 PIC  X(30)   VALUE   "さい                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 070xxx 疑い病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "070001".
           05 PIC  X(30)   VALUE   "疑い病名が長期間継続しています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 080xxx 薬剤と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080001".
           05 PIC  X(30)   VALUE   "薬剤の適応病名未入力です      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080002".
           05 PIC  X(30)   VALUE   "適応病名が外来の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080003".
           05 PIC  X(30)   VALUE   "適応病名が入院の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "080004".
           05 PIC  X(30)   VALUE   "適応病名が有効な期間で登録され".
           05 PIC  X(30)   VALUE   "ていません                    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 090xxx 診療行為と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090001".
           05 PIC  X(30)   VALUE   "診療行為の適応病名未入力です  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090002".
           05 PIC  X(30)   VALUE   "適応病名が外来の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090003".
           05 PIC  X(30)   VALUE   "適応病名が入院の病名として入力".
           05 PIC  X(30)   VALUE   "されています                  ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "090004".
           05 PIC  X(30)   VALUE   "適応病名が有効な期間で登録され".
           05 PIC  X(30)   VALUE   "ていません                    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 010xxx 併用禁忌の薬剤
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "100001".
           05 PIC  X(30)   VALUE   "併用禁忌の薬剤が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 110xxx 併用算定の診療行為
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "110001".
           05 PIC  X(30)   VALUE   "診療行為が併用算定の誤りです  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 120xxx 実日数
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120001".
           05 PIC  X(30)   VALUE   "実日数を越えた回数を算定してい".
           05 PIC  X(30)   VALUE   "ます                          ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120002".
           05 PIC  X(30)   VALUE   "入院していない日に入院診療があ".
           05 PIC  X(30)   VALUE   "ります                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "120003".
           05 PIC  X(30)   VALUE   "外泊のときに入院調剤料が発生し".
           05 PIC  X(30)   VALUE   "ています                      ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 130xxx 患者氏名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130001".
           05 PIC  X(30)   VALUE   "漢字氏名に全角文字以外が入力さ".
           05 PIC  X(30)   VALUE   "れています          ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130002".
           05 PIC  X(30)   VALUE   "カナ氏名に全角カタカナ以外の文".
           05 PIC  X(30)   VALUE   "字が入力されています          ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130003".
           05 PIC  X(30)   VALUE   "漢字氏名が未入力です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130004".
           05 PIC  X(30)   VALUE   "カナ氏名が未入力です          ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130005".
           05 PIC  X(30)   VALUE   "漢字氏名に外字が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "130006".
           05 PIC  X(30)   VALUE   "カナ氏名に外字が入力されていま".
           05 PIC  X(30)   VALUE   "す                            ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 140xxx 保険組合せ有効期間
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "140001".
           05 PIC  X(30)   VALUE   "保険組合せが存在しません      ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "140002".
           05 PIC  X(30)   VALUE   "保険の適用が切れている期間があ".
           05 PIC  X(30)   VALUE   "ります                        ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "140003".
           05 PIC  X(30)   VALUE   "地方公費のみの保険組合せです  ".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 150xxx 投与禁忌薬剤と病名
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "150001".
           05 PIC  X(30)   VALUE   "投与禁忌薬剤が入力されています".
           05 PIC  X(30)   VALUE   "                              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
      *    ** 160xxx 診療行為どうしの算定漏れ
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "160001".
           05 PIC  X(30)   VALUE   "該当の診療行為に対して同日の算".
           05 PIC  X(30)   VALUE   "定漏れがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "160002".
           05 PIC  X(30)   VALUE   "該当の診療行為に対して同月の算".
           05 PIC  X(30)   VALUE   "定漏れがあります              ".
           05 PIC  X(30)   VALUE   "                              ".
      *    ** 990xxx その他必須チェック
      *                            +---------+---------+---------+.
           05 PIC  X(06)   VALUE   "990001".
           05 PIC  X(30)   VALUE   "システム管理と診療行為より入力".
           05 PIC  X(30)   VALUE   "された減算のコードが重複してい".
           05 PIC  X(30)   VALUE   "ます                          ".
      *
           05 PIC  X(06)   VALUE   "990002".
           05 PIC  X(30)   VALUE   "入院基本料と異なる保険組合せで".
           05 PIC  X(30)   VALUE   "入院料加算が登録されています  ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990003".
           05 PIC  X(30)   VALUE   "内分泌負荷試験の算定点数に誤り".
           05 PIC  X(30)   VALUE   "があります（上限オーバー）    ".
           05 PIC  X(30)   VALUE   "                              ".
      *
           05 PIC  X(06)   VALUE   "990004".
           05 PIC  X(30)   VALUE   "内分泌負荷試験の算定点数に誤り".
           05 PIC  X(30)   VALUE   "があります（過誤の逓減）      ".
           05 PIC  X(30)   VALUE   "                              ".
      *
         03  ERRMES-RED    REDEFINES   ERRMES-VAL.
           05 ERRMES-OCC   OCCURS  64.
              07  ERRMES-CD            PIC X(06).
              07  ERRMES-MESSAGE       PIC X(90).
         03  ERRMES-MAX                PIC 9(05)   VALUE   64.
