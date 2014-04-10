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
      ************************************************************
      *    レセプト明細書（労災）     COPY (CPRCF0841.INC)
      *    (レセプト電算処理システムのレコード並び替え用コピー句）
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-門脇    14/04/07  改正対応項目追加
      ************************************************************
      *01  RECE841-REC.
           03  RECE841-KEY.
               05  RECE841-KEY1.
      *医療機関ＩＤ
                   07  RECE841-HOSPNUM     PIC  9(02).
      *診療年月
                   07  RECE841-SRYYM       PIC  9(06).
      *入外区分
                   07  RECE841-NYUGAIKBN   PIC  X(01).
      *患者ＩＤ
                   07  RECE841-PTID        PIC  9(10).
      *レセプト科
                   07  RECE841-RECEKA      PIC  X(02).
      *保険ＩＤ        
                   07  RECE841-HKNID       PIC  9(10).
      *適用開始年月日
                   07  RECE841-TEKSTYMD    PIC  X(08).
      *診療区分
               05  RECE841-SRYKBN          PIC  X(02).
      *コード表用番号−区分番号
               05  RECE841-CDKBN-KBNNUM    PIC  9(03).
      *剤番号
               05  RECE841-ZAINUM          PIC  9(08).
      *
           03  RECE841-DATA-AREA.
      *診療種別区分
               05  RECE841-SRYSYUKBN       PIC  X(03).
      *剤識別区分
               05  RECE841-ZAISKBKBN       PIC  X(01).
      *患者番号
               05  RECE841-PTNUM           PIC  X(20).
      *診療科
               05  RECE841-SRYKA           PIC  X(02).
      *保険組合せ番号
               05  RECE841-HKNCOMBI        PIC  9(04).
      *自保区分
               05  RECE841-JIHOKBN         PIC  X(01).
      *剤点数
               05  RECE841-ZAITEN          PIC  9(08).
      *診療コード計
               05  RECE841-SRYCDTOTAL      PIC  9(14).
      *数量計
               05  RECE841-SURYOUTOTAL     PIC  9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE841-RUIKEITOTAL     PIC  9(13)V9(05).
      *明細数
               05  RECE841-MEISAISU        PIC  9(04).
      *剤回数
               05  RECE841-ZAIKAISU        PIC  9(04).
      *手技点数１
               05  RECE841-SYUTEN1         PIC  9(07).
      *手技点数２
               05  RECE841-SYUTEN2         PIC  9(07).
      *薬剤点数
               05  RECE841-YKZTEN          PIC  9(07).
      *器材点数
               05  RECE841-KIZAITEN        PIC  9(07).
      *ドクターコード
               05  RECE841-DRCD            PIC  X(05).
      *剤請求フラグ
               05  RECE841-ZAIREQFLG       PIC  X(01).
      *逓減率
               05  RECE841-TEIGENRATE      PIC  X(01).
      *コメントコードフラグ
               05  RECE841-COMCDFLG        PIC  X(01).
      *（サ）フラグ
               05  RECE841-SAFLG           PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
               05  RECE841-TOYAKUFLG       PIC  X(01).
      *診療区分置き換えフラグ（自賠責リハ用）
               05  RECE841-OKIKAEFLG       PIC  X(01).
      *療養病棟入院料剤点記載明細数
               05  RECE841-RYOTENKISAISU   PIC  9(04).
      *自賠責集計等取扱
               05  RECE841-JIBAISYUKEIKBN  PIC  9(01).
      *関係コメント存在フラグ
               05  RECE841-SETCOMFLG       PIC  X(01).
      *剤区分
               05  RECE841-ZAIKBN          PIC  9(03).
      *画像診断（２回目以降減算あり）
               05  RECE841-GAZOGEN         PIC  X(01).
      *
      *リハビリ
               05  RECE841-RIHABIRI-COMMENT    PIC  X(01).
      *リハビリ（心大血管疾患）
               05  RECE841-SINDAI-COMMENT      PIC  X(01).
      *リハビリ（呼吸器）
               05  RECE841-KOKYUKI-COMMENT     PIC  X(01).
      *リハビリ（脳血管疾患等）
               05  RECE841-NOUKE-COMMENT       PIC  X(01).
      *リハビリ（運動器）
               05  RECE841-UNDOKI-COMMENT      PIC  X(01).
      *リハビリ（難病患者）
               05  RECE841-NANBYO-COMMENT      PIC  X(01).
      *リハビリ（障害児（者））
               05  RECE841-SYOGAI-COMMENT      PIC  X(01).
      *リハビリ（摂食）
               05  RECE841-SESSYOKU-COMMENT    PIC  X(01).
      *介達牽引
               05  RECE841-KAITATU-COMMENT     PIC  X(01).
      *
      *薬剤管理指導料
               05  RECE841-FLG-YAKUKAN-COMMENT PIC  X(01).
      *入院栄養食事指導料
               05  RECE841-FLG-NYUEIYO-COMMENT PIC  X(01).
      *退院前訪問指導料
               05  RECE841-FLG-HOUMON-COMMENT  PIC  X(01).
      *傷病手当金意見書交付料
               05  RECE841-FLG-SYOBYO-COMMENT  PIC  X(01).
      *療養費同意書交付料
               05  RECE841-FLG-RYOYO-COMMENT   PIC  X(01).
      *退院時服薬指導加算を算定時の薬剤管理指導料
               05  RECE841-FLG-TAIIN-COMMENT   PIC  X(01).
      *訪問看護指示料
               05  RECE841-FLG-HOUSIJI-COMMENT PIC  X(01).
      *手術（輸血は除く）
               05  RECE841-FLG-SHUJUTU-COMMENT PIC  X(01).
      *ＣＴ・ＭＲＩ
               05  RECE841-FLG-CTMRI-COMMENT   PIC  X(01).
      *精神科退院前訪問指導料
               05  RECE841-FLG-HOUSEI-COMMENT  PIC  X(01).
      *精神科隔離病室管理加算（入院料加算）
               05  RECE841-FLG-KAKURI-COMMENT  PIC  X(01).
      *労災治療計画加算（入院料加算）
               05  RECE841-FLG-CHIRYO-COMMENT  PIC  X(01).
      *在宅血液透析頻回指導管理料
               05  RECE841-FLG-HINKAI-COMMENT  PIC  X(01).
      *酸素
               05  RECE841-FLG-SANSO-COMMENT   PIC  X(01).
      *初診（同日複数診療科）
               05  RECE841-FLG-SYODOU-COMMENT  PIC  X(01).
               05  RECE841-SYODOU-SRYKA-AREA.
                   07  RECE841-SYODOU-SRYKA    PIC  X(02)  OCCURS 10.
      *検査等実施判断
               05  RECE841-KNSJISKBN           PIC  9(01).
      *検査等実施判断グループ
               05  RECE841-KNSJISGRPKBN        PIC  9(02).
      *往診・特別往診
               05  RECE841-FLG-OSIN-COMMENT    PIC  X(01).
      *在支援に関する診療
               05  RECE841-FLG-ZAISIEN-COMMENT PIC  X(01).
      *レセ電用入院料加算フラグ
               05  RECE841-NYUKSNFLG           PIC  9(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
               05  RECE841-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *介護支援連携指導料
               05  RECE841-FLG-KAISIEN-COMMENT PIC  X(01).
      *集団栄養食事指導料
               05  RECE841-FLG-SYUEIYO-COMMENT PIC  X(01).
      *人工腎臓等
               05  RECE841-FLG-JINZOU-COMMENT  PIC  X(01).
      *局所陰圧閉鎖処置
               05  RECE841-FLG-KYOKUIN-COMMENT PIC  X(01).
      *麻酔
               05  RECE841-FLG-MASUI-COMMENT   PIC  X(01).
      *栄養サポートチーム加算
               05  RECE841-FLG-EISAPO-COMMENT  PIC  X(01).
      *呼吸ケアチーム加算
               05  RECE841-FLG-KOKKEA-COMMENT  PIC  X(01).
      *重症者加算（精神療養病棟入院料）
               05  RECE841-FLG-JYUSYO-COMMENT  PIC  X(01).
      *
      *画像診断フイルム
               05  RECE841-FLG-FILM-COMMENT    PIC  X(01).
      *
      *介護職員等喀痰吸引等指示料
               05  RECE841-FLG-KAISYOKU-COMMENT
                                               PIC  X(01).
      *精神科ショート・ケア・精神科デイ・ケア
               05  RECE841-FLG-SEIKEA-COMMENT  PIC  X(01).
      *精神科リエゾンチーム加算
               05  RECE841-FLG-SEIRIE-COMMENT  PIC  X(01).
      *病棟薬剤業務実施加算
               05  RECE841-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *在宅持続陽圧呼吸療法指導管理料
               05  RECE841-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（着用型自動除細動器）
      *心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
               05  RECE841-FLG-SINPESUR-COMMENT  PIC  X(01).
      *がん患者指導管理料２・３
               05  RECE841-FLG-GANSIDO-COMMENT   PIC  X(01).
      *在宅患者共同診療料
               05  RECE841-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *磁気による膀胱等刺激法
               05  RECE841-FLG-BOKOHO-COMMENT    PIC  X(01).
      *超重症児（者）入院診療加算・準超重症児（者）入院診療加算
               05  RECE841-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *
      *手技毎の内容（手術・画像診断・処置）
               05  RECE841-ACCTP-SYUGI-TBL-G.
                   07  RECE841-ACCTP-SYUGI-TBL     OCCURS  5.
      *手技開始コード
                       09  RECE841-ACCTP-SRYCD     PIC X(09).
      *手技点数
                       09  RECE841-ACCTP-SYUTEN    PIC 9(07).
      *薬剤点数
                       09  RECE841-ACCTP-YKZTEN    PIC 9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                       09  RECE841-ACCTP-KIZAITEN  PIC 9(07).
      *フィルム点数
                       09  RECE841-ACCTP-FIRTEN    PIC 9(07).
      *酸素点数
               05  RECE841-ACCTP-SANSOTEN          PIC 9(07).
      *窒素点数
               05  RECE841-ACCTP-CHISOTEN          PIC 9(07).
      *
      *剤（点数）
               05  RECE841-ZAI-TENSU               PIC 9(09).
      *剤（金額）
               05  RECE841-ZAI-KINGAKU             PIC 9(09).
      *
      *日数回数テーブル
               05  RECE841-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
                   07  RECE841-DAY-TBL             OCCURS  4.
                       09  RECE841-KAISUINF        OCCURS  31.
                           11  RECE841-KAISU       PIC 9(03).
      *レセプト種別
               05  RECE84-RECESYUBETU          PIC 9(04).
