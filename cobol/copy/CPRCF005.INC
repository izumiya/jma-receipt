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
      *    レセプト明細書     COPY (CPRCF05.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    NACL-藤原    03.01.17  データ区分の追加
      *  01.00.03    NACL-藤原    03/03/12  補助区分の追加
      *  01.00.04    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.05    NACL-門脇    03.09.29  包括対象検査情報の追加
      *                                     検査等実施判断情報の追加
      *                                     各種番号等情報の追加
      *
      *  01.01.01    NACL-藤原    04/05/26  レイアウト変更
      *                                     診療種別区分
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.01.02    NACL-門脇    05/01/17  その他情報の追加
      *
      *  02.06.01    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  02.09.01    NACL-藤原    06/04/13  検査等実施判断グループの追加
      *
      *  03.03.01    NACL-藤原    07/03/27  特定器材種別１、酸素等区分の追加
      ************************************************************
      *01  RECE05-REC.
           03  RECE05-KEY.
      *医療機関ＩＤ
               05  RECE05-HOSPID          PIC  X(24).
      *診療年月
               05  RECE05-SRYYM           PIC  9(06).
      *入外区分
               05  RECE05-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE05-PTID            PIC  9(10).
      *レセプト科
               05  RECE05-RECEKA          PIC  X(02).
      *審査支払機関
               05  RECE05-TEISYUTUSAKI    PIC  9(01).
      *レセプト種別
               05  RECE05-RECESYUBETU     PIC  9(04).
      *保険者番号        
               05  RECE05-HKNJANUM        PIC  X(08).
      *補助区分
               05  RECE05-HOJOKBN         PIC  X(01).
      *適用開始年月日
               05  RECE05-TEKSTYMD        PIC  X(08). 
      *剤番号
               05  RECE05-ZAINUM          PIC  9(08).
      *連番号
               05  RECE05-RENNUM          PIC  9(02).
      *公費分離区分
           03  RECE05-KOHBUNRI            PIC  9(01).
      *患者番号
           03  RECE05-PTNUM               PIC  X(20).
      *診療科
           03  RECE05-SRYKA               PIC  X(02).
      *診療区分
           03  RECE05-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE05-SRYSYUKBN           PIC  X(03).
      *自費金額
           03  RECE05-JIHIMONEY           PIC  9(07).
      *
           03  RECE05-SRYACT-TBL          OCCURS  5.
      *診療行為情報
               05  RECE05-SRYACTINF.
      *診療コード
                   07  RECE05-SRYCD           PIC  X(09).
      *診療種別区分
                   07  RECE05-SRYSYUKBN-X     PIC  X(03).
      *数量
                   07  RECE05-SRYSURYO        PIC  9(05)V9(03).
      *回数
                   07  RECE05-SRYKAISU        PIC  9(03).
      *単位コード
                   07  RECE05-TANICD          PIC  9(03).
      *単位名称有効桁数
                   07  RECE05-TANIMOJISU      PIC  9(01).
      *単位名称
                   07  RECE05-TANINAME        PIC  X(24).
      *点数識別区分
                   07  RECE05-TENSIKIBETU     PIC  9(01).
      *点数
                   07  RECE05-TEN             PIC  9(09)V9(02).
      *明細請求フラグ
                   07  RECE05-MEISKYFLG       PIC  X(01).
      *自動算定区分
                   07  RECE05-AUTOKBN         PIC  X(01).
      *名称入力番号
                   07  RECE05-INPUTNUM        PIC  9(03).
      *点数欄集計先識別
                   07  RECE05-TENTTLKBN       PIC  9(03).
      *データ区分
                   07  RECE05-DATAKBN         PIC  9(01).
      *包括対象検査
                   07  RECE05-HOUKNSKBN       PIC  9(02).
      *検査等実施判断
                   07  RECE05-KNSJISKBN       PIC  9(01).
      *検査等実施判断グループ
                   07  RECE05-KNSJISGRPKBN    PIC  9(02).
      *各種番号等
                   07  RECE05-CDKBNINF.
                       09  RECE05-CDKBN-KBN        PIC  X(01).
                       09  RECE05-CDKBN-SYO        PIC  9(01).
                       09  RECE05-CDKBN-BU         PIC  9(02).
                       09  RECE05-CDKBN-KBNNUM     PIC  9(03).
                       09  RECE05-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  RECE05-CDKBN-KOUBAN     PIC  9(02).
                       09  RECE05-KOUHYOJYUNNUM    PIC  9(09).
      *告示等識別区分１
                   07  RECE05-KOKUJISKBKBN1   PIC  9(01).
      *きざみ値計算識別
                   07  RECE05-KZMCOMPSIKIBETU PIC  9(01).
      *注加算コード
                   07  RECE05-CHUKSNINF.
      *コード
                       09  RECE05-CHUKSNCD     PIC  X(04).
      *通番
                       09  RECE05-CHUKSNTUBAN  PIC  X(01).
      *検体検査コメント
                   07  RECE05-KENTAICOMMENT    PIC  9(01).
      *入力コメント
                   07  RECE05-INPUTCOMENT      PIC  X(80).
      *名称情報
               05  RECE05-NAMEINF.
      *漢字有効桁数
                   07  RECE05-YUKOKETA         PIC  9(02).
      *漢字名称
                   07  RECE05-NAME             PIC  X(200).
      *特定器材種別１
               05  RECE05-TOKUKIZAISBT1        PIC  9(01).
      *酸素等区分
               05  RECE05-SANSOKBN             PIC  9(01).
      *その他情報
               05  RECE05-SONOTAINF.
      *レセプト電算対象外フラグ
                   07  RECE05-RECEDENFLG       PIC  X(01).
      *
