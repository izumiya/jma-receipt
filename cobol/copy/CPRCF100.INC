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
      *    公害レセプト・基本情報         COPY (CPRCF100.INC) 
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-門脇    10/02/16  予備項目追加
      *
      *  04.06.01    NACL-門脇    10/07/02  小児特定疾患カウンセリング料
      *                                     （１回目）算定フラグ追加
      ******************************************************************
      *01  RECE100-REC.
           03  RECE100-KEY.
      *医療機関番号
               05  RECE100-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE100-SRYYM           PIC 9(06).
      *入外区分
               05  RECE100-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               05  RECE100-PTID            PIC 9(10).
      *レセプト診療科
               05  RECE100-RECEKA          PIC X(02).
      *公費ＩＤ
               05  RECE100-HKNID           PIC 9(10).
      *適用開始年月日
               05  RECE100-TEKSTYMD        PIC X(08). 
      *
      *レセプト種別
           03  RECE100-RECESYUBETU         PIC 9(04).
      *
      *レセプト対象年月日情報
           03  RECE100-TEKYMD.
      *適用開始年月日
               05  RECE100-TEKSTYMD-X      PIC X(08). 
      *適用終了年月日
               05  RECE100-TEKEDYMD-X      PIC X(08).
      *
      *患者情報
           03  RECE100-PTINF.
      *患者番号  
               05  RECE100-PTNUM           PIC X(20).
      *カナ氏名
               05  RECE100-KANANAME        PIC X(50).
      *漢字氏名
               05  RECE100-NAME            PIC X(100).
      *性別
               05  RECE100-SEX             PIC X(01).
      *生年月日
               05  RECE100-BIRTHDAY        PIC 9(08).
      *死亡区分
               05  RECE100-DEATHKBN        PIC X(01).
      *年齢
               05  RECE100-AGE             PIC 9(03).
      *テスト患者番号区分
               05  RECE100-TSTPTNUMKBN     PIC X(01).
      *
      *主保険情報
           03  RECE100-HKNINF.
      *主保険−保険番号
               05  RECE100-HKNNUM          PIC X(03).
      *主保険−保険者番号
               05  RECE100-HKNJANUM        PIC X(08).
      *主保険−記号
               05  RECE100-KIGO            PIC X(80).
      *主保険−番号
               05  RECE100-NUM             PIC X(80).
      *
      *診療科情報
           03  RECE100-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE100-SRYKA           PIC X(02).
      *
      *保険組合せ番号
           03  RECE100-HKNCOMBI            PIC 9(04).
      *
      *実日数
           03  RECE100-JNISSU              PIC 9(03).
      *
      *診療区分別点数情報（４１：小計）
           03  RECE100-SRYKBNINF           OCCURS  41.
      *基本点数
               05  RECE100-KHNTEN          PIC 9(08).
      *回数
               05  RECE100-KAISU           PIC 9(05).
      *点数
               05  RECE100-TENSU           PIC 9(08)   OCCURS  2.
      *
      *合計情報
           03  RECE100-TOTALTENINF-AREA.
               05  RECE100-TOTALTENINF     OCCURS  4.
      *合計点数
                   07  RECE100-TOTALTEN    PIC 9(08).
      *
      *実日数テーブル情報
           03  RECE100-JNISSUTBLINF        OCCURS  31.
      *実日数テーブル
               05  RECE100-JNISSUTBL       PIC X(01).
      *乳幼児・幼児のコメントフラグ
           03  RECE100-NYUYOJIFLG          PIC X(01).
      *小児特定疾患カウンセリング料（１回目）算定フラグ
           03  RECE100-SYOUNIFLG           PIC X(01).
      *
      *初診時間区分
           03  RECE100-SHOSHINTIME         PIC X(01).
      *外来迅速検体検査加算フラグ
           03  RECE100-KENTAIKASANFLG      PIC X(01).
      *コメントカウントフラグ
           03  RECE100-COMCNTFLG           PIC X(01).
      *印刷順
           03  RECE100-PRTSEQ              PIC 9(03).
      *未請求フラグ
           03  RECE100-MISEIKYU-FLG        PIC X(02).
      *未訂正区分
           03  RECE100-TEISEIKBN           PIC X(01).
      *レセプト種別不明エラー情報
           03  RECE100-ERR-AREA.
               05  RECE100-ERR-ERRKBN      PIC 9(01).
               05  RECE100-ERR-DAY         PIC 9(02).
               05  RECE100-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE100-ERR-TEKSTYMD    PIC 9(08).
               05  RECE100-ERR-TEKEDYMD    PIC 9(08).
      *
      *湿布薬制限枚数解除及び院外湿布薬７０枚超理由コメント情報
           03  RECE100-SIPPUCHKFLG         PIC 9(01).
           03  RECE100-SIPPUCHKINF.
             04  RECE100-SIPPUCHK-OCC      OCCURS  10.
               05  RECE100-SIPPUCHK-OCC2   OCCURS  31.
                   07  RECE100-SIPPUCHK    PIC 9(03).
      *
      *予備
           03  RECE100-YOBIETC-AREA.
               05  RECE100-YOBIETCINF      OCCURS 10.
                   07  RECE100-YOBIETC     PIC 9(09).
      *
