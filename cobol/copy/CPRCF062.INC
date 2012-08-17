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
      *    レセプト明細書     COPY (CPRCF062.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.02.06  コメントカウントフラグの追加
      *  01.00.02    NACL-藤原    03.02.12  テスト患者番号区分の追加
      *  01.00.03    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.04    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *  01.00.05    NACL-藤原    03.03.11  レセプト種別不明情報の追加
      *  01.00.06    NACL-門脇    03.04.16  船員保険（被保険者）＋通勤災害
      *                                     フラグの追加
      *  01.00.07    NACL-門脇    03.05.28  病棟番号・種別の追加
      *  01.00.08    NACL-門脇    03.06.19  実日数テーブルの追加
      *  01.00.09    NACL-門脇    04.01.16  公費分離区分最小値の追加
      *  01.00.10    NACL-門脇    04.01.19  病床数編集フラグの追加
      *  01.00.11    NACL-門脇    04.01.20  自費レセ摘要欄編集レコードフラグ
      *                                     の追加
      *  01.00.12    NACL-門脇    04.03.17  福祉編集フラグの追加
      *                                     福祉編集フラグ（食事療養）の追加
      *                                     合計点数情報（ぶん点・公費分離
      *                                     区分編集用）の追加
      *  01.00.13    NACL-藤原    04.06.08  適用終了日の追加
      *  01.00.14    NACL-藤原    04.06.15  印刷順の追加（個別作成時のみ）
      *  01.00.15    NACL-藤原    04.07.06  組合せ公費保険ＩＤの追加
      *  01.00.16    NACL-藤原    04.09.13  ＯＣＲエリアを追加
      *  01.00.17    NACL-藤原    04.10.31  県番号の追加
      *  01.00.18    NACL-門脇    04.12.15  公費一部負担金レセ記載フラグ
      *                                     の追加
      *                                     公費一部負担金０円記載フラグ
      *                                     の追加
      *  01.00.19    NACL-門脇    05.03.11  負担金額情報に項目追加
      *                                     レセプト記載外公費情報に項目追加
      *  01.00.20    NACL-藤原    05.03.24  レセプト特記事項編集情報の追加
      *  01.00.21    NACL-小豆沢  05.06.17  福岡公費の給付外点数の追加
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-藤原    05.07.13  老人保険−支払区分
      *                                     本人−入院−内容
      *                                     保険組合せ公費フラグの追加
      *  02.06.02    NACL-藤原    05.07.28  レセプト請求しない公費の追加
      *  02.06.03    NACL-藤原    05.08.05  記載外公費−負担者番号の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *                                     レセプト記載用レセプト種別の追加
      *  02.07.02    NACL-門脇    06/02/07  食事基本料情報（その他）追加
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *                                     主科追加 
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *                                     外来迅速検体検査加算フラグ追加
      *  03.03.01    NACL-門脇    06/10/10  入院時生活療養費食事対応
      *
      *  03.04.01    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.02    NACL-門脇    07/02/19  公費の一部負担金再掲追加
      *                                     公費の一部負担金再掲フラグ追加
      *                                     公費−本人−入院−回−負担割合追加
      *  03.04.03    NACL-門脇    07/03/27  高齢者一部負担金再掲２追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/19  最終病棟・病室番号追加
      *  03.05.03    NACL-門脇    07/08/01  所得情報取扱い機能強化対応
      *  03.05.04    NACL-門脇    07/08/01  標準負担額減額認定証設定対応
      *  03.05.05    NACL-門脇    07/08/01  月途中の所得情報変更対応
      *  04.01.01    NACL-門脇    07/12/19  入院料未算定日対応
      *
      *  04.01.01    NACL-藤原    08/01/10  レセプト特記事項編集情報
      *                                     適用欄コメント対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *                                     就学前区分の追加
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-門脇    08/07/07  実日数・請求点記載フラグ追加
      *  04.03.03    NACL-藤原    08/08/05  患者登録特記事項入力対応
      *                                     患者特記事項追加
      *  04.03.04    NACL-門脇    08/09/05  レセ電用負担区分編集フラグ追加
      *                                     予備追加
      *  04.03.05    NACL-門脇    08/10/16  公費−低２・低１項目追加
      *
      *  04.04.01    NACL-門脇    09/02/16  地方公費のレセ記載対応
      *  04.04.02    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.04.03    NACL-藤原    09/03/25  生保まとめフラグ追加
      *
      *  04.05.01    NACL-門脇    09/12/04  食事療養費情報（ぶん点・
      *                                     公費分離区分編集用）追加
      *  04.05.02    NACL-門脇    10/02/08  全国公費併用分請求点集計追加
      *                                     福祉編集フラグにインデックス追加
      *  04.05.03    NACL-門脇    10/12/13  請求額情報の追加
      *  04.05.04    NACL-藤原    11/02/22  レセ電データの記号番号区切り
      *                                     文字設定対応
      *  04.05.05    NACL-藤原    11/03/25  災害対応
      *                                     連絡先情報追加
      *  04.05.06    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.07    NACL-門脇    12/04/09  入院略称の文字数拡張
      *  04.05.08    NACL-門脇    12/08/16  レセ電用負担区分編集フラグ（食事）
      *                                     追加
      *                                     予備を２０に変更
      ************************************************************
      *01  RECE62-REC.
           03  RECE62-KEY.
      *医療機関ＩＤ
               05  RECE62-HOSPNUM         PIC  9(2).
      *診療年月
               05  RECE62-SRYYM           PIC  9(06).
      *入外区分
               05  RECE62-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE62-PTID            PIC  9(10).
      *レセプト科
               05  RECE62-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE62-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE62-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE62-HKNJANUM-KEY    PIC  X(08).
      *補助区分
               05  RECE62-HOJOKBN-KEY     PIC  X(01).
      *適用開始年月日
               05  RECE62-TEKSTYMD        PIC  X(08). 
      *患者番号
           03  RECE62-PTNUM               PIC  X(20).
      *診療科情報
           03  RECE62-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE62-SRYKA           PIC  X(02).
      *カナ氏名
           03  RECE62-KANANAME            PIC  X(50).
      *漢字氏名
           03  RECE62-NAME                PIC  X(100).
      *性別
           03  RECE62-SEX                 PIC  X(01).
      *生年月日
           03  RECE62-BIRTHDAY            PIC  9(08).
      *年齢
           03  RECE62-AGE                 PIC  9(03).
      *就学前区分
           03  RECE62-SYUGAKUKBN          PIC 9(01).
      *死亡区分
           03  RECE62-DEATHKBN            PIC  X(01).
      *自宅−郵便番号
           03  RECE62-HOME-POST           PIC X(07).
      *自宅−住所
           03  RECE62-HOME-ADRS           PIC X(200).
      *自宅−番地方書
           03  RECE62-HOME-BANTI          PIC X(200).
      *連絡先−名称
           03  RECE62-RENRAKU-NAME.
               05  RECE62-RENRAKU-NAME-R  PIC X(50)
                                              OCCURS  2.
      *連絡先−郵便番号
           03  RECE62-RENRAKU-POST        PIC X(07).
      *連絡先−住所
           03  RECE62-RENRAKU-ADRS        PIC X(200).
      *連絡先−番地方書
           03  RECE62-RENRAKU-BANTI       PIC X(200).
      *連絡先−昼電話番号
           03  RECE62-RENRAKU-TEL1        PIC X(15).
      *連絡先−夜電話番号
           03  RECE62-RENRAKU-TEL2        PIC X(15).
      *自宅−編集郵便番号
           03  RECE62-HOME-POST1          PIC X(14).
      *自宅−編集住所（住所＋番地）
           03  RECE62-HOME-ADRS1.
               05  RECE62-HOME-ADRS-R     PIC X(50)
                                              OCCURS  4.
      *連絡先−編集郵便番号
           03  RECE62-RENRAKU-POST1       PIC X(14).
      *連絡先−編集住所（住所＋番地）
           03  RECE62-RENRAKU-ADRS1.
               05  RECE62-RENRAKU-ADRS-R  PIC X(50)
                                              OCCURS  4.
      *連絡先−編集電話番号（昼電話番号全角）
           03  RECE62-TEL                 PIC X(30).
      *レセプト対象年月日情報
           03  RECE62-TEKYMD.
      *適用開始年月日
               05  RECE62-TEKSTYMD-X      PIC  X(08). 
      *適用終了年月日
               05  RECE62-TEKEDYMD-X      PIC  X(08). 
      *記載用レセプト種別（特別療養費）
           03  RECE62-RECESYUBETU-X       PIC  X(04).
      *記載用レセプト種別（紙レセプト）
           03  RECE62-RECESYUBETU-Y       PIC  X(04).
      *主科情報
           03  RECE62-SYUKA-AREA.
      *主科
               05  RECE62-SYUKA           PIC  X(02).
      *主科（レセ電算診療科）
               05  RECE62-RECESYUKA       PIC  X(02).
      *主科診療科名
               05  RECE62-SYUKANAME       PIC  X(80).
      *医師別出力情報
           03  RECE62-DRCD-AREA.
      *医師コード
               05  RECE62-DRCD            PIC  X(05).
      *医師別診療科
               05  RECE62-DRCD-SRYKA      PIC  X(02).
      *主保険情報
           03  RECE62-HKN.
      *主保険−保険ＩＤ
               05  RECE62-HKNID           PIC  9(10).
      *主保険−保険番号
               05  RECE62-HKNNUM          PIC  X(03).
      *主保険−法別番号
               05  RECE62-HKNHBTNUM       PIC  X(02).
      *主保険−保険者番号
               05  RECE62-HKNJANUM        PIC  X(08).
      *主保険−本人家族区分
               05  RECE62-HONKZKKBN       PIC  X(01).
      *主保険−補助区分
               05  RECE62-HOJOKBN         PIC  X(01).
      *主保険−継続区分
               05  RECE62-CONTKBN         PIC  X(01).
      *主保険−記号
               05  RECE62-KIGO            PIC  X(80).
      *主保険−番号
               05  RECE62-NUM             PIC  X(80).
      *主保険−給付割合
               05  RECE62-KYURATE         PIC  9(03).
      *主保険−証明書番号
               05  RECE62-CERTNUM         PIC  X(03).
      *老人保健情報
           03  RECE62-RJNHKN.
      *老人保険−保険ＩＤ
               05  RECE62-RJNHKNID        PIC  9(10).
      *老人保険−保険番号
               05  RECE62-RJNHKNNUM       PIC  X(03).
      *老人保険−法別番号
               05  RECE62-RJNHBTNUM       PIC  X(02).
      *老人保険−市町村番号
               05  RECE62-RJNFTNJANUM     PIC  X(08).
      *老人保険−受給者番号
               05  RECE62-RJNJKYSNUM      PIC  X(07).
      *老人保険−支払区分
               05  RECE62-RJNPAYKBN       PIC  X(02).
      *
      *公費情報
           03  RECE62-KOHINF-AREA.
               05  RECE62-KOHINF          OCCURS  4.
      *公費−保険ＩＤ
                   07  RECE62-KOHID       PIC  9(10).
      *公費−保険番号
                   07  RECE62-KOHNUM      PIC  X(03).
      *公費−法別番号
                   07  RECE62-KOHHBTNUM   PIC  X(02).
      *公費−負担者番号
                   07  RECE62-KOHFTNJANUM PIC  X(08).
      *公費−受給者番号
                   07  RECE62-KOHJKYSNUM  PIC  X(20).
      *公費−任意給付区分
                   07  RECE62-KOHNINIKYUKBN
                                          PIC  X(01).
      *公費−件数
                   07  RECE62-KOHKENSU    PIC  9(03).
      *公費−レセプト請求区分
                   07  RECE62-RECESKYKBN  PIC  X(01).
      *公費−適用開始年月日
                   07  RECE62-KOHTEKSTYMD PIC  X(08).
      *公費−適用終了年月日
                   07  RECE62-KOHTEKEDYMD PIC  X(08).
      *
      *公費−本人−入院−負担区分
                   07  RECE62-HON-NYUFTNKBN     PIC  X(01).
      *公費−本人−入院−回−負担割合
                   07  RECE62-HON-NYUKFTNRATE   PIC  9(03).
      *公費−本人−入院−日−上限額
                   07  RECE62-HON-NYUDJGNGAK    PIC  9(06).
      *公費−本人−入院−月−上限額
                   07  RECE62-HON-NYUMJGNGAK    PIC  9(06).
      *公費−本人−入院−月−上限回数
                   07  RECE62-HON-NYUMJGNCNT    PIC  9(02).
      *公費−本人−入院−食事療養区分
                   07  RECE62-HON-NYUSKJRYYKBN  PIC  X(01).
      *
      *公費−低２−入院−負担区分
                   07  RECE62-TSY-NYUFTNKBN     PIC  X(01).
      *公費−低２−入院−回−負担割合
                   07  RECE62-TSY-NYUKFTNRATE   PIC  9(03).
      *公費−低２−入院−日−上限額
                   07  RECE62-TSY-NYUDJGNGAK    PIC  9(06).
      *公費−低２−入院−月−上限額
                   07  RECE62-TSY-NYUMJGNGAK    PIC  9(06).
      *公費−低２−入院−月−上限回数
                   07  RECE62-TSY-NYUMJGNCNT    PIC  9(02).
      *公費−低２−入院−食事療養区分
                   07  RECE62-TSY-NYUSKJRYYKBN  PIC  X(01).
      *
      *公費−低１−入院−負担区分
                   07  RECE62-TNK-NYUFTNKBN     PIC  X(01).
      *公費−低１−入院−回−負担割合
                   07  RECE62-TNK-NYUKFTNRATE   PIC  9(03).
      *公費−低１−入院−日−上限額
                   07  RECE62-TNK-NYUDJGNGAK    PIC  9(06).
      *公費−低１−入院−月−上限額
                   07  RECE62-TNK-NYUMJGNGAK    PIC  9(06).
      *公費−低１−入院−月−上限回数
                   07  RECE62-TNK-NYUMJGNCNT    PIC  9(02).
      *公費−低１−入院−食事療養区分
                   07  RECE62-TNK-NYUSKJRYYKBN  PIC  X(01).
      *
      *保険組合せ番号情報
           03  RECE62-HKNCOMBIINF-AREA.
               05  RECE62-HKNCOMBIINF     OCCURS  10.
      *保険組合せ番号
                   07  RECE62-HKNCOMBI    PIC  9(04).
      *主保険番号
                   07  RECE62-HKNNUMHC    PIC  X(03).
      *組合せ公費保険番号情報
                   07  RECE62-KOHNUMHCINF OCCURS  4.
      *組合せ公費保険番号
                       09  RECE62-KOHNUMHC
                                          PIC  X(03).
      *組合せ公費保険ＩＤ
                       09  RECE62-KOHIDHC PIC  9(10).
      *全国公費併用分請求点集計区分
                       09  RECE62-SYUKEIKBN
                                          PIC  X(01).
      *全国公費併用分請求点集計対象区分
                       09  RECE62-SYUKEI-TAIKBN
                                          PIC  X(01).
      *レセ電用負担区分編集フラグ
                       09  RECE62-FTNKBNFLG
                                          PIC  9(01).
      *レセ電用負担区分編集フラグ（食事）
                       09  RECE62-FTNKBNFLG-SKJ
                                          PIC  9(01).
      *保険組合せ公費フラグ（１：全国公費あり）
                   07  RECE62-HKNCOMBIFLG PIC  9(01).
      *
      *診療区分別点数情報
           03  RECE62-SRYKBNINF           OCCURS  30.
      *基本点数
               05  RECE62-KHNTEN          PIC  9(08).
      *回数
               05  RECE62-KAISU           PIC  9(05).
      *明細点数情報
               05  RECE62-TENSUINF-AREA.
                   07  RECE62-TENSUINF    OCCURS  5.
      *明細点数
                       09  RECE62-TENSU   PIC  9(08).
      *
      *合計点数情報
           03  RECE62-TOTALTENINF-AREA.
               05  RECE62-TOTALTENINF     OCCURS  5.
      *合計点数
                   07  RECE62-TOTALTEN    PIC  9(08).
      *
      *負担金額情報
           03  RECE62-FTNMONEYINF-AREA.
               05  RECE62-FTNMONEYINF     OCCURS  5.
      *負担金額
                   07  RECE62-FTNMONEY    PIC  9(09).
      *負担金額（一部負担額）
                   07  RECE62-FTNMONEY-FTN    PIC  9(09).
      *負担金額（控除額）
                   07  RECE62-FTNMONEY-KOJ    PIC  9(09).
      *
      *実日数情報
           03  RECE62-JNISSUINF-AREA.
               05  RECE62-JNISSUINF       OCCURS  5.
      *実日数
                   07  RECE62-JNISSU      PIC  9(02).
      *
      *入院基本料種別情報
           03  RECE62-NYUSYUINF           OCCURS  20.
      *入院基本料種別
               05  RECE62-NYUSYUBETU      PIC  X(20).
      *
      *入院基本料情報
           03  RECE62-NYUKHNINF           OCCURS  10.
      *入院基本料・加算
               05  RECE62-NYUKHNRYO       PIC  9(09).
      *入院日数
               05  RECE62-NYUKHNNISSU     PIC  9(02).
      *合計金額情報
               05  RECE62-NYUKHNKEIINF-AREA.
                   07  RECE62-NYUKHNKEIINF    OCCURS  5.
      *合計金額
                       09  RECE62-NYUKHNKEI   PIC  9(09).
      *入院基本料情報２
           03  RECE62-NYUKHNINF2.
      *合計金額情報２
               05  RECE62-NYUKHNKEIINF2   OCCURS  5.
      *合計金額２
                   07  RECE62-NYUKHNKEI2  PIC S9(09).
      *
      *特定入院基本料情報
           03  RECE62-NYUTOKINF           OCCURS  10.
      *特定入院料・その他（点）
               05  RECE62-NYUTOKRYO       PIC  9(09).
      *特定入院料・その他（略称）
               05  RECE62-NYUTOKRYO-RYAKU PIC  X(20).
      *入院日数
               05  RECE62-NYUTOKNISSU     PIC  9(02).
      *合計金額情報
               05  RECE62-NYUTOKKEIINF-AREA.
                   07  RECE62-NYUTOKKEIINF    OCCURS  5.
      *合計金額
                       09  RECE62-NYUTOKKEI   PIC  9(09).
      *
      *特定入院基本料情報２
           03  RECE62-NYUTOKINF2.
      *合計金額情報２
               05  RECE62-NYUTOKKEIINF2   OCCURS  5.
      *合計金額２
                   07  RECE62-NYUTOKKEI2  PIC S9(09).
      *
      *食事療養費情報
           03  RECE62-SHOKUJIINF-AREA.
               05  RECE62-SHOKUJIINF      OCCURS  5.
      *食事日数
                   07  RECE62-SHOKUJINISSU
                                          PIC  9(02).
      *食事療養費
                   07  RECE62-SHOKUJIRYOYOHI
                                          PIC  9(09).
      *標準負担額
                   07  RECE62-SHOKUJIFTN  PIC  9(09).
      *
      *一部負担金・食事療養費標準負担額区分
           03  RECE62-SHOKUJIFTNKBN       PIC  X(01).
      *一部負担金・食事療養費標準負担額区分２（地方公費判定用）
           03  RECE62-SHOKUJIFTNKBN2-INF.
               05  RECE62-SHOKUJIFTNKBN2  PIC  X(01)
                                          OCCURS  4.
      *
      *食事種別情報‐基本
           03  RECE62-SHOKUJISYUBETU-KHN  PIC  X(02).
      *
      *食事種別情報
           03  RECE62-SHOKUJISYUINF       OCCURS   3.
      *食事種別
               05  RECE62-SHOKUJISYUBETU  PIC  X(04).
      *
      *食事基本料情報
           03  RECE62-SHOKUJIKHNINF       OCCURS   4.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU PIC  9(02).
      *
      *食事種別情報‐基本（生活）
           03  RECE62-SHOKUJISYUBETU-KHNSEI   PIC  X(02).
      *
      *食事基本料情報（生活）
           03  RECE62-SHOKUJIKHNINF-SEI   OCCURS   2.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO-SEI   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU-SEI PIC  9(02).
      *
      *食事基本料情報（その他）
           03  RECE62-SHOKUJIKHNINF-ETC       OCCURS   3.
      *食事基本料
               05  RECE62-SHOKUJIKHNRYO-ETC   PIC  9(09).
      *食事日数
               05  RECE62-SHOKUJIKHNNISSU-ETC PIC  9(02).
      *
      *入院時生活療養費に係る生活療養食事算定フラグ
           03  RECE62-SEIKATURYOYO        PIC  X(01).
      *
      *最終診療科
           03  RECE62-LASTSRYKA           PIC  X(02).
      *最終病棟番号
           03  RECE62-LASTBTUNUM          PIC  X(02).
      *最終病室番号
           03  RECE62-LASTBRMNUM          PIC  X(08).
      *
      *入院年月日
           03  RECE62-NYUINYMD            PIC  X(08).
      *
      *医療機関種別
           03  RECE62-HOSPSBT             PIC  X(01).
      *特記事項情報
           03  RECE62-TOKKIINF            OCCURS  5.
      *特記事項
               05  RECE62-TOKKI           PIC  X(02).
      *病棟区分情報
           03  RECE62-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  RECE62-BYOTOKBN        PIC  X(02).
      *病棟種別情報
           03  RECE62-BYOTOSYUBETUINF     OCCURS 31.
      *病棟番号
               05  RECE62-BYOTONUM        PIC  X(02).
      *病棟種別
               05  RECE62-BYOTOSYUBETU    PIC  X(02).
      *レセプトＯＣＲ１
           03  RECE62-OCR1                PIC  X(67).
      *レセプトＯＣＲ２
           03  RECE62-OCR2                PIC  X(67).
      *レセプトＯＣＲ３
           03  RECE62-OCR3                PIC  X(67).
      *
      *レセプト記載外公費情報
           03  RECE62-ETC-KOH-G.
               05  RECE62-ETC-KOHINF      OCCURS  4.
      *記載外公費−保険番号
                   07  RECE62-ETC-KOHNUM  PIC  X(03).
      *記載外公費−負担者番号
                   07  RECE62-ETC-KOHFTNJANUM
                                          PIC  X(08).
      *記載外公費−実日数
                   07  RECE62-ETC-JNISSU  PIC  9(02).
      *記載外公費−合計点数
                   07  RECE62-ETC-TOTALTEN
                                          PIC  9(08).
      *記載外公費−負担金額
                   07  RECE62-ETC-FTNMONEY
                                          PIC  9(09).
      *記載外公費−負担金額（一部負担額）
                   07  RECE62-ETC-FTNMONEY-FTN
                                          PIC  9(09).
      *記載外公費−負担金額（控除額）
                   07  RECE62-ETC-FTNMONEY-KOJ
                                          PIC  9(09).
      *記載外公費−食事日数
                   07  RECE62-ETC-SHOKUJINISSU
                                          PIC  9(02).
      *記載外公費−食事療養費
                   07  RECE62-ETC-SHOKUJIRYOYOHI
                                          PIC  9(09).
      *記載外公費−標準負担額
                   07  RECE62-ETC-SHOKUJIFTN
                                          PIC  9(09).
      *
      *レセプト請求しない公費領域
           03 RECE62-KOHSKY-KOHI-AREA.
               05  RECE62-KOHSKY-KOHI-TBL OCCURS  4.
      *    公費ＩＤ
                   07  RECE62-KOHSKY-KOHID
                                          PIC  9(10).
      *    公費保険番号
                   07  RECE62-KOHSKY-KOHNUM
                                          PIC  X(03).
      *    公費負担者番号
                   07  RECE62-KOHSKY-KOHFTNJANUM
                                          PIC  X(08).
      *低所得フラグ
           03  RECE62-TEISYOTOKU          PIC  X(01).
      *低所得フラグ２（地方公費判定用）
           03  RECE62-TEISYOTOKU2-INF.
               05  RECE62-TEISYOTOKU2     PIC  X(01)
                                          OCCURS  4.
      *低所得フラグ３（食事判定用）
           03  RECE62-TEISYOTOKU3         PIC  X(01).
      *高齢者フラグ
           03  RECE62-KOUREI              PIC  X(01).
      *高齢者一部負担金再掲
           03  RECE62-KOUREI-SAIKEI       PIC  9(09).
      *高齢者一部負担金再掲２
           03  RECE62-KOUREI-SAIKEI2      PIC  9(09).
      *公費の一部負担金再掲
           03  RECE62-KOH-SAIKEI          PIC  9(09).
      *公費の一部負担金再掲フラグ
           03  RECE62-KOH-SAIKEIFLG       PIC  9(01).
      *船員保険（被保険者）＋通勤災害フラグ
           03  RECE62-SENIN-TUUSAI        PIC  X(01).
      *初診時間区分
           03  RECE62-SHOSHINTIME         PIC  X(01).
      *老人減免区分（高齢者経過措置フラグとして使用）
           03  RECE62-RJNGENMENKBN        PIC  X(01).
      *老人外来金額（請求点数合計として使用）
           03  RECE62-RJNGAIFTN           PIC  9(09).
      *老人入院金額（未使用）
           03  RECE62-RJNNYUFTN           PIC  9(09).
      *減免区分
           03  RECE62-GENMENKBN           PIC  9(01).
      *減額割合
           03  RECE62-GENGAKURATE         PIC  9(03).
      *減額金額
           03  RECE62-GENGAKU             PIC  9(07).
      *割引点数単価（７０歳未満現物給付の所得区分として使用）
           03  RECE62-DISCOUNTTENTNK      PIC  X(02).
      *老人公費負担金額情報
           03  RECE62-RJNKOHFTNINF        OCCURS  3.
      *老人外来公費負担額（（１）（２）（３）使用）
               05  RECE62-RJNGAIKOHFTN    PIC  9(09).
      *老人入院公費負担額（（１）（２）（３）使用）
               05  RECE62-RJNNYUKOHFTN    PIC  9(09).
      *コメントカウントフラグ
           03  RECE62-COMCNTFLG           PIC  X(01).
      *テスト患者番号区分
           03  RECE62-TSTPTNUMKBN         PIC  X(01).
      *レセプト種別不明エラー情報
           03  RECE62-ERR-AREA.
               05  RECE62-ERR-ERRKBN      PIC 9(01).
               05  RECE62-ERR-DAY         PIC 9(02).
               05  RECE62-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE62-ERR-TEKSTYMD    PIC 9(08).
               05  RECE62-ERR-TEKEDYMD    PIC 9(08).
      *他保険入院料算定済みフラグ
           03  RECE62-NYUINRYOFLG         PIC  X(01).
      *
      *実日数テーブル情報
           03  RECE62-JNISSUTBLINF        OCCURS 31.
      *実日数テーブル
               05  RECE62-JNISSUTBL       PIC  X(01).
      *
      *公費実日数テーブル情報
           03  RECE62-KOHJNISSUTBLINF.
      *公費実日数テーブル
               05  RECE62-KOHJNISSUTBL    PIC  X(01)
                                          OCCURS 31.
      *県番号
           03  RECE62-PREFNUM             PIC  9(02).
      *県内・県外区分（１：県内　２：県外　）
           03  RECE62-PREFKBN             PIC  X(01).
      *県番号（保険者）
           03  RECE62-HKNJANUM-PREFNUM    PIC  X(02).
      *公費分離区分最小値
           03  RECE62-KOHBUNRI-MIN        PIC  9(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE62-BEDSUFLG            PIC  X(01).
      *自費レセ摘要欄編集レコードフラグ
           03  RECE62-JIHTEKFLG           PIC  X(01).
      *外来迅速検体検査加算フラグ
           03  RECE62-KENTAIKASANFLG      PIC  X(01).
      *福祉編集フラグ
           03  RECE62-FUKUSHIFLGINF-AREA.
               05  RECE62-FUKUSHIFLG      PIC  X(01)   OCCURS 5.
      *特別な関係にある他院歴ありフラグ
           03  RECE62-TAINFLG             PIC  X(01).
      *生保まとめフラグ
           03  RECE62-SEIHOFLG            PIC  X(01).
      *
      *レセプト用：食事療養編集情報
           03  RECE62-FUKUSHIFLG-SKJINF-AREA.
               05  RECE62-FUKUSHIFLG-SKJ  PIC  X(01)   OCCURS 4.
      *
      *レセ電用：食事療養（一般）編集情報
           03  RECE62-FUKUSHIFLG-SKJIPAINF-AREA.
               05  RECE62-FUKUSHIFLG-SKJIPA
                                          PIC  X(01)   OCCURS 4.
      *レセ電用：食事療養（生活）編集情報
           03  RECE62-FUKUSHIFLG-SKJSEIINF-AREA.
               05  RECE62-FUKUSHIFLG-SKJSEI
                                          PIC  X(01)   OCCURS 4.
      *
      *合計点数情報（ぶん点・公費分離区分編集用）
           03  RECE62-HENTOTALTENINF-AREA.
               05  RECE62-HENTOTALTENINF  OCCURS  5.
      *合計点数（ぶん点・公費分離区分編集用）
                   07  RECE62-HENTOTALTEN PIC  9(08).
      *
      *食事療養費情報（ぶん点・公費分離区分編集用）
           03  RECE62-HENSHOKUJIINF-AREA.
               05  RECE62-HENSHOKUJIINF   OCCURS  5.
      *食事療養費（ぶん点・公費分離区分編集用）
                   07  RECE62-HENSHOKUJIRYOYOHI
                                          PIC  9(09).
      *
      *印刷順
           03  RECE62-PRTSEQ              PIC  9(03).
      *
      *公費一部負担金レセ記載フラグ
           03  RECE62-KISAIFLGINF-AREA.
               05  RECE62-KISAIFLGINF     OCCURS  5.
                   07  RECE62-KISAIFLG    PIC  X(01).
      *公費一部負担金０円記載フラグ
           03  RECE62-ZEROKISAIFLGINF-AREA.
               05  RECE62-ZEROKISAIFLGINF OCCURS  5.
                   07  RECE62-ZEROKISAIFLG
                                          PIC  X(01).
      *その他記載フラグ
           03  RECE62-ETCKISAIFLGINF-AREA.
               05  RECE62-ETCKISAIFLGINF  OCCURS  5.
                   07  RECE62-EKF-JNISSUFLG    PIC  X(01).
                   07  RECE62-EKF-TOTALTENFLG  PIC  X(01).
                   07  RECE62-EKF-SKJFLG       PIC  X(01).
                   07  RECE62-EKF-OCRFLG       PIC  X(01).
      *
      *実日数・請求点記載フラグ
           03  RECE62-JSKISAIFLG          PIC  X(01).
      *未請求フラグ
           03  RECE62-MISEIKYU-FLG        PIC X(02).
      *レセプト特記事項編集情報
           03  RECE62-2006-AREA.
      *        特記事項
               05  RECE62-2006-TOKKIINF.
                   07  RECE62-2006-TOKKI  PIC  X(12)
                                               OCCURS  4.
      *        給付割合
               05  RECE62-2006-KYURATE    PIC  9(03).
      *        レセ電：特記事項
               05  RECE62-2006-RECEDEN-TOKKIINF.
                   07  RECE62-2006-RECEDEN-TOKKI
                                          PIC  X(02)
                                               OCCURS  5.
      *        レセ電：給付割合
               05  RECE62-2006-RECEDEN-KYURATE
                                          PIC  9(03).
      *        公費情報
               05  RECE62-2006-KOHINF.
                   07  RECE62-2006-KOH-OCC     OCCURS  4. 
      *                負担者番号記載区分
                       09  RECE62-2006-FTNJANUMKBN
                                          PIC  X(02).
      *                負担者番号
                       09  RECE62-2006-FTNJANUM
                                          PIC  X(08).
      *                受給者番号記載区分
                       09  RECE62-2006-JKYSNUMKBN
                                          PIC  X(02).
      *                受給者番号
                       09  RECE62-2006-JKYSNUM
                                          PIC  X(07).
      *                レセ電：公費レコード区分
                       09  RECE62-2006-RECEDEN-KORECKBN
                                          PIC  X(01).
      *                レセ電：負担者番号記載区分
                       09  RECE62-2006-RECEDEN-FTNJANUMKBN
                                          PIC  X(02).
      *                レセ電：負担者番号
                       09  RECE62-2006-RECEDEN-FTNJANUM
                                          PIC  X(08).
      *                レセ電：受給者番号記載区分
                       09  RECE62-2006-RECEDEN-JKYSNUMKBN
                                          PIC  X(02).
      *                レセ電：受給者番号
                       09  RECE62-2006-RECEDEN-JKYSNUM
                                          PIC  X(07).
      *        適用欄コメント情報
               05  RECE62-2006-TEKIYOU-COMMENTINF.
                   07  RECE62-2006-TEKIYOU-COMMENT-OCC OCCURS  5. 
      *                適用欄上段コメント
                       09  RECE62-2006-TEKIYOU-COMMENT1
                                          PIC  X(52).
      *                適用欄下段コメント
                       09  RECE62-2006-TEKIYOU-COMMENT2
                                          PIC  X(52).
      *                適用欄上段コメントレセ電編集区分
                       09  RECE62-2006-TEKIYOU-COMMENT1KBN
                                          PIC  X(01).
      *                適用欄下段コメントレセ電編集区分
                       09  RECE62-2006-TEKIYOU-COMMENT2KBN
                                          PIC  X(01).
      *
      *福岡県公費給付外点数
           03  RECE62-KYUFUGAITEN         PIC  9(06).
      *
      *患者特記事項情報
           03  RECE62-PTTOKKI-AREA.
               05  RECE62-PTTOKKINF           OCCURS  5.
                   07  RECE62-PTTOKKI-NAIYO
                                          PIC  X(12).
                   07  RECE62-PTTOKKI-CD  PIC  X(02).
      *
      *請求額情報（計算用）
           03  RECE62-FTNMONEY-SKY             PIC 9(09).
      *
      *請求額情報（医療費）
           03  RECE62-SKYIRYO-INF.
      *実日数
               05  RECE62-SKYIRYO-NISSU        PIC 9(02).
      *請求点
               05  RECE62-SKYIRYO-TOTALTEN     PIC 9(08).
      *患者負担
               05  RECE62-SKYIRYO-ICHIBUFTN    PIC 9(09).
      *保険者負担
               05  RECE62-SKYIRYO-HOKENJA      PIC 9(09).
      *公費負担
               05  RECE62-SKYIRYO-KOHFTN       PIC 9(09).
      *
      *請求額情報（食事）
           03  RECE62-SKYSKJ-INF.
      *回数
               05  RECE62-SKYSKJ-KAISU         PIC 9(02).
      *請求円
               05  RECE62-SKYSKJ-RYOYOHI       PIC 9(09).
      *患者負担
               05  RECE62-SKYSKJ-ICHIBUFTN     PIC 9(09).
      *保険者負担
               05  RECE62-SKYSKJ-HOKENJA       PIC 9(09).
      *公費負担
               05  RECE62-SKYSKJ-KOHFTN        PIC 9(09).
      *
      *入院料情報
           03  RECE62-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE62-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE62-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE62-NYUINDAY-TBL.
                   07  RECE62-NYUINDAY-KBN
                                          PIC X(01)   OCCURS  31.
      *
      *予備
           03  RECE62-YOBIETC-AREA.
               05  RECE62-YOBIETCINF          OCCURS 20.
                   07  RECE62-YOBIETC     PIC  9(09).
      *
