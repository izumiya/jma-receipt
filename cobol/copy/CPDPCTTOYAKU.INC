      *****************************************************************
      *    ＤＰＣ  退院時投薬コメントテーブル
      *****************************************************************
       01  TBL-TAITOYAKU-AREA.
           03  TBL-TAITOYAKU-CONST.
      *        この行は変更しないでください。
               05  FILLER              PIC X(09) VALUE  "840000006".
      *
      *
      *        ユーザーが作成した退院時投薬のコメントコードは
      *        ２つまで登録できます。
      *        以下の２行のうち上側から順番に設定してください。
      *        "000000000"の部分に９桁のコードを編集します。
      *
      *
               05  FILLER              PIC X(09) VALUE  "000000000".
               05  FILLER              PIC X(09) VALUE  "000000000".
      *
      *
      *
      *    これより下は変更しないでください。
           03  TBL-TAITOYAKU-CONST-R   REDEFINES TBL-TAITOYAKU-CONST.
               05  TBL-TAITOYAKU-OCC   OCCURS    3
                                       INDEXED   TBL-TAITOYAKU-IDX.
                   07  TBL-TAITOYAKU   PIC X(09).
      *
           03  TBL-TAITOYAKU-MAX       PIC 9(02) VALUE  3.
      *
