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
      *    公害レセプト（入院）・基本情報     COPY (CPRCF200.INC) 
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.05.01    NACL-門脇    10/02/16  予備項目追加
      *  04.05.02    NACL-藤原    11/11/22  同日再入院対応
      ********************************************************************
      *01  RECE200-REC.
           03  RECE200-KEY.
      *医療機関番号
               05  RECE200-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE200-SRYYM           PIC 9(06).
      *入外区分
               05  RECE200-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               05  RECE200-PTID            PIC 9(10).
      *レセプト診療科
               05  RECE200-RECEKA          PIC X(02).
      *公費ＩＤ
               05  RECE200-HKNID           PIC 9(10).
      *適用開始年月日
               05  RECE200-TEKSTYMD        PIC X(08). 
      *
      *レセプト種別
           03  RECE200-RECESYUBETU         PIC 9(04).
      *
      *レセプト対象年月日情報
           03  RECE200-TEKYMD.
      *適用開始年月日
               05  RECE200-TEKSTYMD-X      PIC X(08). 
      *適用終了年月日
               05  RECE200-TEKEDYMD-X      PIC X(08).
      *
      *患者情報
           03  RECE200-PTINF.
      *患者番号  
               05  RECE200-PTNUM           PIC X(20).
      *カナ氏名
               05  RECE200-KANANAME        PIC X(50).
      *漢字氏名
               05  RECE200-NAME            PIC X(100).
      *性別
               05  RECE200-SEX             PIC X(01).
      *生年月日
               05  RECE200-BIRTHDAY        PIC 9(08).
      *死亡区分
               05  RECE200-DEATHKBN        PIC X(01).
      *年齢
               05  RECE200-AGE             PIC 9(03).
      *テスト患者番号区分
               05  RECE200-TSTPTNUMKBN     PIC X(01).
      *
      *主保険情報
           03  RECE200-HKNINF.
      *主保険−保険番号
               05  RECE200-HKNNUM          PIC X(03).
      *主保険−保険者番号
               05  RECE200-HKNJANUM        PIC X(08).
      *主保険−記号
               05  RECE200-KIGO            PIC X(80).
      *主保険−番号
               05  RECE200-NUM             PIC X(80).
      *
      *診療科情報
           03  RECE200-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE200-SRYKA           PIC X(02).
      *
      *保険組合せ番号
           03  RECE200-HKNCOMBI            PIC 9(04).
      *
      *実日数
           03  RECE200-JNISSU              PIC 9(03).
      *
      *診療区分別点数情報
           03  RECE200-SRYKBNINF           OCCURS  21.
      *基本点数
               05  RECE200-KHNTEN          PIC 9(08).
      *回数
               05  RECE200-KAISU           PIC 9(05).
      *点数
               05  RECE200-TENSU           PIC 9(08)   OCCURS  2.
      *
      *小計情報
           03  RECE200-SYOUKEIINF-AREA.
               05  RECE200-SYOUKEIINF      OCCURS  3.
      *小計点数
                   07  RECE200-SYOUKEI     PIC 9(08).
      *
      *合計情報
           03  RECE200-TOTALTENINF-AREA.
               05  RECE200-TOTALTENINF     OCCURS  4.
      *合計点数
                   07  RECE200-TOTALTEN    PIC 9(08).
      *
      *入院基本料種別情報
           03  RECE200-NYUSYUINF           OCCURS  20.
      *入院基本料種別
               05  RECE200-NYUSYUBETU      PIC X(20).
      *
      *入院基本料情報
           03  RECE200-NYUKHNINF           OCCURS  10.
      *入院基本料・加算
               05  RECE200-NYUKHNRYO       PIC 9(09).
      *入院日数
               05  RECE200-NYUKHNNISSU     PIC 9(02).
      *合計金額
               05  RECE200-NYUKHNKEI       PIC 9(09).
      *
      *入院基本料情報２
           03  RECE200-NYUKHNINF2.
      *合計金額２
               05  RECE200-NYUKHNKEI2      PIC S9(09).
      *
      *特定入院基本料情報
           03  RECE200-NYUTOKINF           OCCURS  10.
      *特定入院料・その他（点）
               05  RECE200-NYUTOKRYO       PIC 9(09).
      *特定入院料・その他（略称）
               05  RECE200-NYUTOKRYO-RYAKU PIC X(20).
      *入院日数
               05  RECE200-NYUTOKNISSU     PIC 9(02).
      *合計金額
               05  RECE200-NYUTOKKEI       PIC 9(09).
      *
      *特定入院基本料情報２
           03  RECE200-NYUTOKINF2.
      *合計金額２
               05  RECE200-NYUTOKKEI2      PIC S9(09).
      *
      *公害入院療養指導
           03  RECE200-KOGAISIDOINF        OCCURS   3.
      *基本料
               05  RECE200-KOGAISIDORYO    PIC 9(09).
      *日数
               05  RECE200-KOGAISIDONISSU  PIC 9(02).
      *合計金額
               05  RECE200-KOGAISIDOKEI    PIC 9(09).
      *
      *清浄空気室管理料
           03  RECE200-SEIKANINF.
      *基本料
               05  RECE200-SEIKANRYO       PIC 9(09).
      *日数
               05  RECE200-SEIKANNISSU     PIC 9(02).
      *合計金額
               05  RECE200-SEIKANKEI       PIC 9(09).
      *
      *食事種別情報‐基本
           03  RECE200-SHOKUJISYUBETU-KHN  PIC X(02).
      *
      *食事種別情報
           03  RECE200-SHOKUJISYUINF       OCCURS   3.
      *食事種別
               05  RECE200-SHOKUJISYUBETU  PIC X(04).
      *
      *食事基本料情報
           03  RECE200-SHOKUJIKHNINF       OCCURS   4.
      *食事基本料
               05  RECE200-SHOKUJIKHNRYO   PIC 9(09).
      *食事日数
               05  RECE200-SHOKUJIKHNNISSU PIC 9(02).
      *
      *他保険入院料算定済みフラグ
           03  RECE200-NYUINRYOFLG         PIC X(01).
      *
      *最終診療科
           03  RECE200-LASTSRYKA           PIC X(02).
      *最終病棟番号
           03  RECE200-LASTBTUNUM          PIC X(02).
      *最終病室番号
           03  RECE200-LASTBRMNUM          PIC X(08).
      *
      *入院年月日
           03  RECE200-NYUINYMD            PIC X(08).
      *
      *医療機関種別
           03  RECE200-HOSPSBT             PIC X(01).
      *病棟区分情報
           03  RECE200-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  RECE200-BYOTOKBN        PIC X(02).
      *病棟種別情報
           03  RECE200-BYOTOSYUBETUINF     OCCURS 31.
      *病棟番号
               05  RECE200-BYOTONUM        PIC X(02).
      *病棟種別
               05  RECE200-BYOTOSYUBETU    PIC X(02).
      *
      *診療回数テーブル
           03  RECE200-DAY-AREA.
               05  RECE200-DAY             PIC 9(03)
                                           OCCURS  31.
      *
      *実日数テーブル情報
           03  RECE200-JNISSUTBLINF        OCCURS  31.
      *実日数テーブル
               05  RECE200-JNISSUTBL       PIC X(01).
      *
      *入院料情報取得フラグ
           03  RECE200-DOUJITSU-FLG        PIC 9(01).
      *入院料情報
           03  RECE200-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE200-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE200-DOUJITSU-SKJKBN
                                          PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE200-NYUINDAY-TBL.
                   07  RECE200-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *初診時間区分
           03  RECE200-SHOSHINTIME         PIC X(01).
      *外来迅速検体検査加算フラグ
           03  RECE200-KENTAIKASANFLG      PIC X(01).
      *コメントカウントフラグ
           03  RECE200-COMCNTFLG           PIC X(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE200-BEDSUFLG            PIC X(01).
      *特別な関係にある他院歴ありフラグ
           03  RECE200-TAINFLG             PIC X(01).
      *印刷順
           03  RECE200-PRTSEQ              PIC 9(03).
      *未請求フラグ
           03  RECE200-MISEIKYU-FLG        PIC X(02).
      *未訂正区分
           03  RECE200-TEISEIKBN           PIC X(01).
      *レセプト種別不明エラー情報
           03  RECE200-ERR-AREA.
               05  RECE200-ERR-ERRKBN      PIC 9(01).
               05  RECE200-ERR-DAY         PIC 9(02).
               05  RECE200-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE200-ERR-TEKSTYMD    PIC 9(08).
               05  RECE200-ERR-TEKEDYMD    PIC 9(08).
      *
      *予備
           03  RECE200-YOBIETC-AREA.
               05  RECE200-YOBIETCINF      OCCURS 10.
                   07  RECE200-YOBIETC     PIC 9(09).
      *
