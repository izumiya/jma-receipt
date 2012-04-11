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
      *    レセプト明細書     COPY (CPRCF044.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.09.20  日数のテーブルを変更
      *  01.00.02    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.03    NACL-門脇    03.09.24  コメント編集フラグ追加
      *  01.00.04    NACL-門脇    04.02.05  診療コード＊数量の累計
      *                                     追加
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料
      *                                     の追加
      *  01.01.02    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指
      *                                     導料算定時の点滴注射の追加
      *  01.01.03    NACL-門脇    05.01.17  酸素の追加
      *  01.01.04    NACL-門脇    05.02.28  （サ）フラグの追加
      *  02.09.01    NACL-門脇    06.04.17  ２００６年４月改正対応
      *  03.00.01    NACL-門脇    06.06.22  往診・特別往診、
      *                                     在宅患者訪問診療料の追加
      *  03.01.01    NACL-門脇    06.07.11  在支援に関する診療の追加
      *  03.01.02    NACL-門脇    06.07.14  診療区分置き換えフラグの追加
      *  03.04.01    NACL-門脇    06.12.01  自賠責健保準拠対応
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/28  小児特定疾患カウンセリングの追加
      *  04.03.01    NACL-門脇    08/07/14  投薬剤点０フラグの追加
      *  04.03.02    NACL-門脇    08/09/25  自賠責集計等取扱の追加
      *
      *  04.04.01    NACL-門脇    09/02/04  検査（ＢＮＰ等）の追加
      *  04.04.02    NACL-門脇    10/03/23  同日電話再診フラグの追加
      *  04.04.03    NACL-門脇    10/03/29  画像診断（２回目以降減算あり）
      *                                     の追加
      *  04.04.04    NACL-門脇    10/04/06  改正対応項目追加
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    10/01/19  剤区分追加
      *  04.05.03    NACL-門脇    12/04/10  改正対応項目追加
      ************************************************************
      *01  RECE44-REC.
           03  RECE44-KEY.
      *医療機関ＩＤ
               05  RECE44-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE44-SRYYM           PIC  9(06).
      *入外区分
               05  RECE44-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE44-PTID            PIC  9(10).
      *レセプト科
               05  RECE44-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE44-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE44-TEKSTYMD        PIC  X(08).
      *剤番号
               05  RECE44-ZAINUM          PIC  9(08).
      *診療区分
           03  RECE44-SRYKBN              PIC  X(02).
      *患者番号
           03  RECE44-PTNUM               PIC  X(20).
      *診療科
           03  RECE44-SRYKA               PIC  X(02).
      *保険組合せ番号
           03  RECE44-HKNCOMBI            PIC  9(04).
      *自保区分
           03  RECE44-JIHOKBN             PIC  X(01).
      *剤点数
           03  RECE44-ZAITEN              PIC  9(08).
      *診療コード計
           03  RECE44-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  RECE44-SURYOUTOTAL         PIC  9(07)V9(05).
      *診療コード＊数量の累計
           03  RECE44-RUIKEITOTAL         PIC  9(13)V9(05).
      *明細数
           03  RECE44-MEISAISU            PIC  9(04).
      *剤回数
           03  RECE44-ZAIKAISU            PIC  9(04).
      *手技点数１
           03  RECE44-SYUTEN1             PIC  9(07).
      *手技点数２
           03  RECE44-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  RECE44-YKZTEN              PIC  9(07).
      *器材点数
           03  RECE44-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  RECE44-DRCD                PIC  X(05).
      *剤請求フラグ
           03  RECE44-ZAIREQFLG           PIC  X(01).
      *逓減率
           03  RECE44-TEIGENRATE          PIC  X(01).
      *コメントコードフラグ
           03  RECE44-COMCDFLG            PIC  X(01).
      *（サ）フラグ
           03  RECE44-SAFLG               PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
           03  RECE44-TOYAKUFLG           PIC  X(01).
      *診療区分置き換えフラグ（自賠責リハ用）
           03  RECE44-OKIKAEFLG           PIC  X(01).
      *自賠責集計等取扱
           03  RECE44-JIBAISYUKEIKBN      PIC  9(01).
      *剤区分
           03  RECE44-ZAIKBN              PIC  9(03).
      *画像診断（２回目以降減算あり）
           03  RECE44-GAZOGEN             PIC  X(01).
      *
      *初診料算定科：
           03  RECE44-SYOSANTEIKA-COMMENT PIC  X(01).
      *再診料算定科：
           03  RECE44-SAISANTEIKA-COMMENT PIC  X(01).
      *電話再診
           03  RECE44-DENWASAI-COMMENT    PIC  X(01).
      *同日再診
           03  RECE44-DOUJITUSAI-COMMENT  PIC  X(01).
      *同日電話再診
           03  RECE44-DOUDENSAI-COMMENT   PIC  X(01).
      *リハビリ
           03  RECE44-RIHABIRI-COMMENT    PIC  X(01).
      *リハビリ（心大血管疾患）
           03  RECE44-SINDAI-COMMENT      PIC  X(01).
      *リハビリ（呼吸器）
           03  RECE44-KOKYUKI-COMMENT     PIC  X(01).
      *リハビリ（脳血管疾患等）
           03  RECE44-NOUKE-COMMENT       PIC  X(01).
      *リハビリ（運動器）
           03  RECE44-UNDOKI-COMMENT      PIC  X(01).
      *リハビリ（難病患者）
           03  RECE44-NANBYO-COMMENT      PIC  X(01).
      *リハビリ（障害児（者））
           03  RECE44-SYOGAI-COMMENT      PIC  X(01).
      *リハビリ（摂食）
           03  RECE44-SESSYOKU-COMMENT    PIC  X(01).
      *介達牽引
           03  RECE44-KAITATU-COMMENT     PIC  X(01).
      *
      *慢性疼痛疾患管理料
           03  RECE44-FLG-SHIDOU-COMMENT  PIC  X(01).
      *傷病手当金意見書交付料
           03  RECE44-FLG-SYOBYO-COMMENT  PIC  X(01).
      *療養費同意書交付料
           03  RECE44-FLG-RYOYO-COMMENT   PIC  X(01).
      *訪問看護指示料
           03  RECE44-FLG-HOUSIJI-COMMENT PIC  X(01).
      *手術（輸血は除く）
           03  RECE44-FLG-SHUJUTU-COMMENT PIC  X(01).
      *ＣＴ・ＭＲＩ
           03  RECE44-FLG-CTMRI-COMMENT   PIC  X(01).
      *在宅血液透析頻回指導管理料
           03  RECE44-FLG-HINKAI-COMMENT  PIC  X(01).
      *在宅患者訪問点滴注射管理指導料算定時の点滴注射
           03  RECE44-FLG-HOUTEN-COMMENT  PIC  X(01).
      *酸素
           03  RECE44-FLG-SANSO-COMMENT   PIC  X(01).
      *初診（同日複数診療科）
           03  RECE44-FLG-SYODOU-COMMENT  PIC  X(01).
           03  RECE44-SYODOU-SRYKA-AREA.
               05  RECE44-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *再診（同一日２科目）
           03  RECE44-FLG-SAIDOU-COMMENT  PIC  X(01).
           03  RECE44-SAIDOU-SRYKA-AREA.
               05  RECE44-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
      *ニコチン依存症管理料
           03  RECE44-FLG-NIKOCHIN-COMMENT PIC X(01).
      *重度喘息患者治療管理加算
           03  RECE44-FLG-ZENSOKU-COMMENT PIC  X(01).
      *検査等実施判断グループ
           03  RECE44-KNSJISGRPKBN        PIC  9(02).
      *往診・特別往診
           03  RECE44-FLG-OSIN-COMMENT    PIC  X(01).
      *在宅患者訪問診療料
           03  RECE44-FLG-ZAIHOU-COMMENT  PIC  X(01).
      *在支援に関する診療
           03  RECE44-FLG-ZAISIEN-COMMENT PIC  X(01).
      *小児特定疾患カウンセリング
           03  RECE44-FLG-SYOUNI-COMMENT  PIC  X(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
           03  RECE44-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（遠隔モニタリング）
           03  RECE44-FLG-SINPESU-COMMENT PIC  X(01).
      *集団栄養食事指導料
           03  RECE44-FLG-SYUEIYO-COMMENT PIC  X(01).
      *人工腎臓等
           03  RECE44-FLG-JINZOU-COMMENT  PIC  X(01).
      *局所陰圧閉鎖処置
           03  RECE44-FLG-KYOKUIN-COMMENT PIC  X(01).
      *麻酔
           03  RECE44-FLG-MASUI-COMMENT   PIC  X(01).
      *認知療法・認知行動療法
           03  RECE44-FLG-NINRYO-COMMENT  PIC  X(01).
      *
      *認知症専門診断管理料２
           03  RECE44-FLG-NINSEN-COMMENT  PIC  X(01).
      *退院時共同指導料１（１以外）
           03  RECE44-FLG-TAKYO-COMMENT   PIC  X(01).
      *退院時共同指導料特別管理指導加算
           03  RECE44-FLG-TAKYOKSN-COMMENT
                                          PIC  X(01).
      *外来リハビリテーション診療料・外来放射線照射診療料
           03  RECE44-FLG-GAIRIHA-COMMENT PIC  X(01).
      *介護職員等喀痰吸引等指示料
           03  RECE44-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *
      *日数回数テーブル
           03  RECE44-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
             04  RECE44-DAY-TBL           OCCURS  4.
                 05  RECE44-KAISUINF      OCCURS  31.
                     07  RECE44-KAISU     PIC  9(03).
