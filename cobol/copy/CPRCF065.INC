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
      *    レセプト明細書     COPY (CPRCF065.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.01.17  データ区分の追加
      *  01.00.02    NACL-門脇    03.09.29  包括対象検査情報の追加
      *                                     検査等実施判断情報の追加
      *                                     各種番号等情報の追加
      *  01.00.03    NACL-門脇    04.06.07  繰り返しを集団項目へ変更
      *
      *  01.00.04    NACL-藤原    04/07/27  診療種別区分
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.00.05    NACL-門脇    05/01/17  その他情報の追加
      *
      *  02.06.01    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  02.09.01    NACL-藤原    06/04/13  検査等実施判断グループの追加
      *  03.04.01    NACL-門脇    07/02/19  レセプト対象外フラグの追加
      *  03.04.02    NACL-藤原    07/03/27  特定器材種別１、酸素等区分の追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  04.01.01    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.02.01    NACL-藤原    09/04/21  告示等識別区分２、通則加算対
      *                                     象外区分の追加
      ************************************************************
      *01  RECE65-REC.
           03  RECE65-KEY.
      *医療機関ＩＤ
               05  RECE65-HOSPNUM         PIC  9(2).
      *診療年月
               05  RECE65-SRYYM           PIC  9(06).
      *入外区分
               05  RECE65-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE65-PTID            PIC  9(10).
      *レセプト科
               05  RECE65-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE65-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE65-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE65-HKNJANUM        PIC  X(08).
      *補助区分
               05  RECE65-HOJOKBN         PIC  X(01).
      *適用開始年月日
               05  RECE65-TEKSTYMD        PIC  X(08).
      *剤番号
               05  RECE65-ZAINUM          PIC  9(08).
      *剤連番
               05  RECE65-ZAIREN          PIC  9(02).
      *連番号
               05  RECE65-RENNUM          PIC  9(02).
      *公費分離区分
           03  RECE65-KOHBUNRI            PIC  9(01).
      *患者番号
           03  RECE65-PTNUM               PIC  X(20).
      *診療科
           03  RECE65-SRYKA               PIC  X(02).
      *診療区分
           03  RECE65-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE65-SRYSYUKBN           PIC  X(03).
      *自費金額
           03  RECE65-JIHIMONEY           PIC  9(07).
      *
           03  RECE65-SRYACT-TBL          OCCURS  5.
      *診療行為情報
               05  RECE65-SRYACTINF.
      *診療コード
                   07  RECE65-SRYCD           PIC  X(09).
      *診療種別区分
                   07  RECE65-SRYSYUKBN-X     PIC  X(03).
      *数量
                   07  RECE65-SRYSURYO        PIC  9(05)V9(03).
      *回数
                   07  RECE65-SRYKAISU        PIC  9(03).
      *単位コード
                   07  RECE65-TANICD          PIC  9(03).
      *単位名称有効桁数
                   07  RECE65-TANIMOJISU      PIC  9(01).
      *単位名称
                   07  RECE65-TANINAME        PIC  X(24).
      *点数識別区分
                   07  RECE65-TENSIKIBETU     PIC  9(01).
      *点数
                   07  RECE65-TEN             PIC  9(09)V9(02).
      *明細請求フラグ
                   07  RECE65-MEISKYFLG       PIC  X(01).
      *自動算定区分
                   07  RECE65-AUTOKBN         PIC  X(01).
      *名称入力番号
                   07  RECE65-INPUTNUM        PIC  9(03).
      *点数欄集計先識別
                   07  RECE65-TENTTLKBN       PIC  9(03).
      *データ区分
                   07  RECE65-DATAKBN         PIC  9(01).
      *包括対象検査
                   07  RECE65-HOUKNSKBN       PIC  9(02).
      *検査等実施判断
                   07  RECE65-KNSJISKBN       PIC  9(01).
      *検査等実施判断グループ
                   07  RECE65-KNSJISGRPKBN    PIC  9(02).
      *各種番号等
                   07  RECE65-CDKBNINF.
                       09  RECE65-CDKBN-KBN        PIC  X(01).
                       09  RECE65-CDKBN-SYO        PIC  9(01).
                       09  RECE65-CDKBN-BU         PIC  9(02).
                       09  RECE65-CDKBN-KBNNUM     PIC  9(03).
                       09  RECE65-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  RECE65-CDKBN-KOUBAN     PIC  9(02).
                       09  RECE65-KOUHYOJYUNNUM    PIC  9(09).
      *告示等識別区分１
                   07  RECE65-KOKUJISKBKBN1   PIC  9(01).
      *告示等識別区分２
                   07  RECE65-KOKUJISKBKBN2   PIC  9(01).
      *通則加算対象外区分
                   07  RECE65-TUSOKUGAIKBN    PIC  9(01).
      *きざみ値計算識別
                   07  RECE65-KZMCOMPSIKIBETU PIC  9(01).
      *注加算コード
                   07  RECE65-CHUKSNINF.
      *コード
                       09  RECE65-CHUKSNCD     PIC  X(04).
      *通番
                       09  RECE65-CHUKSNTUBAN  PIC  X(01).
      *検体検査コメント
                   07  RECE65-KENTAICOMMENT    PIC  9(01).
      *入力コメント
                   07  RECE65-INPUTCOMENT      PIC  X(80).
      *名称情報
               05  RECE65-NAMEINF.
      *漢字有効桁数
                   07  RECE65-YUKOKETA         PIC  9(03).
      *漢字名称
                   07  RECE65-NAME             PIC  X(200).
      *特定器材種別１
               05  RECE65-TOKUKIZAISBT1        PIC  9(01).
      *酸素等区分  
               05  RECE65-SANSOKBN             PIC  9(01).
      *その他情報
               05  RECE65-SONOTAINF.
      *レセプト電算対象外フラグ
                   07  RECE65-RECEDENFLG       PIC  X(01).
      *レセプト対象外フラグ
                   07  RECE65-RECEPTFLG        PIC  X(01).
      *レセ電用特定器材商品名
                   07  RECE65-RECEDENNAME      PIC  X(300).
      *レセ用特定器材商品名編集フラグ
                   07  RECE65-RECEHENFLG       PIC  X(01).
      *
