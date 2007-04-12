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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 自動算定情報（Ｗ０７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/06/05  薬剤情報提供料の追加
      *  01.00.02    MCC-藤原     01/09/11  削除処理の修正 
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/11/26  項目をコンボボックスに変更
      *  01.02.01    MCC-森脇     02/05/23  チェック機能制御情報の追加
      *  01.02.02    NACL-多々納  02/10/10  外来管理加算チェック追加
      *  01.02.03    NACL-多々納  02/10/31  外来管理加算チェック区分追加
      *  01.02.04    NACL-多々納  02/11/13  一括入力 
      *  01.02.05    NACL-多々納  02/11/20  チェック機能制御初期設定変更
      *  01.02.05    NACL-多々納  03/02/19  療養担当手当て追加
      *  01.02.06    NACL-多々納  03/03/14  特定薬剤治療管理料追加
      *  02.01.01    NACL-森脇    03/10/17  画面変更につき一括変更
      *  02.01.02    NACL-多々納  03/11/07  入院初診料追加
      *  02.01.03    NACL-多々納  04/01/09  包括登録追加
      *  02.01.04    NACL-多々納  04/01/15  数量ゼロ入力追加
      *  02.01.05    NACL-太田    04/01/16  診療行為機能情報タグ追加
      *  02.01.06    NACL-多々納  04/01/23  入院調基チェック追加
      *  02.01.07    NACL-太田    04/01/23  収納機能情報タグ追加
      *  02.01.08    NACL-多々納  04/03/08  時間外加算特例乳幼児他追加
      *  02.01.09    NACL-藤原    04/03/08  生保受給者番号の追加
      *  02.01.10    NACL-多々納  04/03/10  時間外加算乳幼児チェック追加
      *  02.01.11    NACL-藤原    04/03/08  生保受給者番号の機能停止
      *  02.01.12    NACL-多々納  04/07/05  最終受診日から初診の期間追加
      *                                     請求書発行方法他追加
      *  02.01.13    NACL-藤原    04/07/08  生保受給者番号を削除
      *  02.01.14    NACL-多々納  04/10/06  同姓同名者、二重登録疑い追加
      *  02.01.15    NACL-藤原    04/11/15  病名機能情報を追加
      *  02.01.16    NACL-多々納  04/12/06  診療行為クリア確認追加
      *  02.01.17    NACL-太田    04/01/07  収納機能情報−請求書発行
      *  02.01.18    NACL-藤原    05/04/20  移行病名警告メッセージ表示
      *                                     有無区分
      *                                     廃止年月日警告メッセージ表示
      *                                     有無区分
      *                                     診療科初期表示区分
      *                                     表示対象期間初期表示区分
      *                                     同一病名警告メッセージ表示
      *                                     有無区分
      *                                     の追加
      *  02.01.19    NACL-多々納  05/06/06  中途終了展開時の自動発生
      *                                     薬剤情報提供料チェック追加
      *  02.01.20    NACL-多々納  05/07/06  訂正時の自動発生追加
      *  02.01.21    NACL-太田    05/07/26  収納明細登録時刻入力欄追加
      *  02.01.22    NACL-多々納  05/08/19  会計照会情報追加
      *  02.01.21    NACL-太田    05/09/09  収納明細登録仮締め時刻入力欄追加
      *  02.01.22    NACL-多々納  05/12/21  病名表示区分
      *                                     剤登録順区分追加
      *                                     MONFUNC 対応
      *  02.01.23    NACL-多々納  06/01/20  会計照会 診療科初期表示追加
      *  02.01.24    NACL-多々納  06/02/08  地方公費単独チェックの追加
      *  02.01.25    NACL-多々納  06/05/19  患者削除機能チェックの追加
      *  02.01.26    NACL-太田    06/08/16  収納機能情報
      *                                     請求書兼領収書発行区分追加
      *  03.02.00    NACL-多々納  06/08/29  レセコメント詳記区分の追加
      *  03.03.00    NACL-太田    06/11/02  パッチ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W07.
           03  SPA-W07-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
                   07  SPA-GMN-PAGENO      PIC 9(01).
      *
                   07  SPA-GMN-PAGE1-AREA.
      *                自動算定チェック
                       09  SPA-GMN-DATA-TBL            OCCURS  7.
                           11  SPA-GMN-DATA-G.
                               13  SPA-GMN-DATA        PIC X(01).
                               13  SPA-GMN-F1          PIC X(01).
                               13  SPA-GMN-DATAMEI     PIC X(20).
      *
      *                自動算定科
                       09  SPA-GMN-SRYKACD             PIC X(02).
                       09  SPA-GMN-SRYKAMEI            PIC X(20).
      *                入院初診料
                       09  SPA-GMN-NYUSYOSIN-G.
                           11  SPA-GMN-NYUSYOSIN       PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
                           11  SPA-GMN-NYUSYOSINMEI    PIC X(10).
      *
      *                時間外加算特例乳幼児
                       09  SPA-GMN-JIKANTOKU-NYU-G.
                           11  SPA-GMN-JIKANTOKU-NYU   PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
                           11  SPA-GMN-JIKANTOKU-NYUMEI    PIC X(10).
      *                画像診断管理加算１
                       09  SPA-GMN-GAZOUKSN1-G.
                           11  SPA-GMN-GAZOUKSN1       PIC X(01).
                           11  SPA-GMN-FN              PIC X(01).
                           11  SPA-GMN-GAZOUKSN1MEI    PIC X(10).
      *
      *                相互作用チェック期間
                       09  SPA-GMN-INTERACT-KIKAN      PIC 9(02).
      *
      *                中途終了展開時の自動発生
                       09  SPA-GMN-CHUTOTENKAI-G.
                           11  SPA-GMN-CHUTOTENKAI       PIC X(01).
                           11  SPA-GMN-CHUTO             PIC X(01).
                           11  SPA-GMN-CHUTOTENKAIMEI    PIC X(10).
      *                訂正時の自動発生
                       09  SPA-GMN-TEISEIAUTO-G.
                           11  SPA-GMN-TEISEIAUTO       PIC X(01).
                           11  SPA-GMN-TEISEI           PIC X(01).
                           11  SPA-GMN-TEISEIAUTOMEI    PIC X(10).
      *                訂正時の自動発生（特定疾患）
                       09  SPA-GMN-TEISEITOKU-G.
                           11  SPA-GMN-TEISEITOKU       PIC X(01).
                           11  SPA-GMN-TEISEIH1         PIC X(01).
                           11  SPA-GMN-TEISEITOKUMEI    PIC X(10).
      *
      *                外来管理加算チェック
                       09  SPA-GMN-GAIKANRIKBN-G.
                           11  SPA-GMN-GAIKANRIKBN-CHK     PIC X(01).
                           11  SPA-GMN-GAIKANRIKBN-H       PIC X(01).
                           11  SPA-GMN-GAIKANRIKBN-MEI     PIC X(20).
                       09  SPA-GMN-GAIKANRIKBN-COMB.
                           11  SPA-GMN-GAIKANRIKBN-LIST    PIC X(22)
                                                           OCCURS  3.
      *                外来カーソル移動
                       09  SPA-GMN-GAIMOVE-G.
                           11  SPA-GMN-GAIMOVE             PIC X(01).
                           11  SPA-GMN-GAIMOVE-F1          PIC X(01).
                           11  SPA-GMN-GAIMOVEMEI          PIC X(20).
                       09  SPA-GMN-GAIMOVE-COMB.
                           11  SPA-GMN-GAIMOVE-LIST        PIC X(22)
                                                           OCCURS  3.
      *                時間外加算チェック
                       09  SPA-GMN-JIKANTOKU-CHK-G.
                           11  SPA-GMN-JIKANTOKU-CHK       PIC X(01).
                           11  SPA-GMN-JIKANTOKU-CHK-F1    PIC X(01).
                           11  SPA-GMN-JIKANTOKU-CHKMEI    PIC X(20).
                       09  SPA-GMN-JIKANTOKU-CHK-COMB.
                           11  SPA-GMN-JIKANTOKU-CHK-LIST  PIC X(22)
                                                           OCCURS  2.
      *                チェック除外時間
                       09  SPA-GMN-JIKANCHK-WEEK           OCCURS   2.
                           11  SPA-GMN-JIKANCHK-WST.
                               13  SPA-GMN-JIKANWST-HH      PIC X(02).
                               13  SPA-GMN-JIKANWST-MM      PIC X(02).
                           11  SPA-GMN-JIKANCHK-WED.
                               13  SPA-GMN-JIKANWED-HH      PIC X(02).
                               13  SPA-GMN-JIKANWED-MM      PIC X(02).
                       09  SPA-GMN-JIKANCHK-SAT            OCCURS   2.
                           11  SPA-GMN-JIKANCHK-SST.
                               13  SPA-GMN-JIKANSST-HH      PIC X(02).
                               13  SPA-GMN-JIKANSST-MM      PIC X(02).
                           11  SPA-GMN-JIKANCHK-SED.
                               13  SPA-GMN-JIKANSED-HH      PIC X(02).
                               13  SPA-GMN-JIKANSED-MM      PIC X(02).
      *
      *                包括まとめ
                       09  SPA-GMN-HOUKATU-G.
                           11  SPA-GMN-HOUKATU             PIC X(01).
                           11  SPA-GMN-HOUKATU-F1          PIC X(01).
                           11  SPA-GMN-HOUKATUMEI          PIC X(28).
                       09  SPA-GMN-HOUKATU-COMB.
                           11  SPA-GMN-HOUKATU-LIST        PIC X(30)
                                                           OCCURS  2.
      *                数量ゼロ入力
                       09  SPA-GMN-SURYOZERO-G.
                           11  SPA-GMN-SURYOZERO           PIC X(01).
                           11  SPA-GMN-SURYOZERO-F1        PIC X(01).
                           11  SPA-GMN-SURYOZEROMEI        PIC X(20).
                       09  SPA-GMN-SURYOZERO-COMB.
                           11  SPA-GMN-SURYOZERO-LIST      PIC X(22)
                                                           OCCURS  2.
      *                入院調基チェック
                       09  SPA-GMN-CHOKICHK-G.
                           11  SPA-GMN-CHOKICHK            PIC X(01).
                           11  SPA-GMN-CHOKICHK-F1         PIC X(01).
                           11  SPA-GMN-CHOKICHKMEI         PIC X(20).
                       09  SPA-GMN-CHOKICHK-COMB.
                           11  SPA-GMN-CHOKICHK-LIST       PIC X(20)
                                                           OCCURS  2.
      *                相互作用チェック期間
                       09  SPA-GMN-LAST-KIKAN              PIC 9(02).
      *                診療行為クリア確認チェック
                       09  SPA-GMN-CLEARCHK-G.
                           11  SPA-GMN-CLEARCHK            PIC X(01).
                           11  SPA-GMN-CLEARCHK-F1         PIC X(01).
                           11  SPA-GMN-CLEARCHK-MEI        PIC X(20).
                       09  SPA-GMN-CLEARCHK-COMB.
                           11  SPA-GMN-CLEARCHK-LIST       PIC X(20)
                                                           OCCURS  2.
      *                薬剤情報提供料チェック
                       09  SPA-GMN-YAKJYOCHK-G.
                           11  SPA-GMN-YAKJYOCHK           PIC X(01).
                           11  SPA-GMN-YAKJYOCHK-F1        PIC X(01).
                           11  SPA-GMN-YAKJYOCHK-MEI       PIC X(20).
                       09  SPA-GMN-YAKJYOCHK-COMB.
                           11  SPA-GMN-YAKJYOCHK-LIST      PIC X(22)
                                                           OCCURS  2.
      *                病名表示区分
                       09  SPA-GMN-BYOHYOKBN-G.
                           11  SPA-GMN-BYOHYOKBN           PIC X(01).
                           11  SPA-GMN-BYOHYOKBN-F1        PIC X(01).
                           11  SPA-GMN-BYOHYOKBN-MEI       PIC X(10).
                       09  SPA-GMN-BYOHYOKBN-COMB.
                           11  SPA-GMN-BYOHYOKBN-LIST      PIC X(12)
                                                           OCCURS  2.
      *                剤登録順区分
                       09  SPA-GMN-ZAIJUNKBN-G.
                           11  SPA-GMN-ZAIJUNKBN           PIC X(01).
                           11  SPA-GMN-ZAIJUNKBN-F1        PIC X(01).
                           11  SPA-GMN-ZAIJUNKBN-MEI       PIC X(14).
                       09  SPA-GMN-ZAIJUNKBN-COMB.
                           11  SPA-GMN-ZAIJUNKBN-LIST      PIC X(16)
                                                           OCCURS  2.
      *                検体検査加算自動発生
                       09  SPA-GMN-KENSAAUTO-G.
                           11  SPA-GMN-KENSAAUTO           PIC X(01).
                           11  SPA-GMN-KENSAAUTO-F1        PIC X(01).
                           11  SPA-GMN-KENSAAUTO-MEI       PIC X(10).
                       09  SPA-GMN-KENSAAUTO-COMB.
                           11  SPA-GMN-KENSAAUTO-LIST      PIC X(12)
                                                           OCCURS  2.
      *
                   07  SPA-GMN-PAGE2-AREA.
      *                カルテの病名対象
                       09  SPA-GMN-BYOMEIKBN-G.
                           11  SPA-GMN-BYOMEIKBN           PIC X(01).
                           11  SPA-GMN-BYOMEIKBN-F1        PIC X(01).
                           11  SPA-GMN-BYOMEIKBNMEI        PIC X(26).
                       09  SPA-GMN-BYOMEIKBN-COMB.
                           11  SPA-GMN-BYOMEIKBN-LIST      PIC X(28)
                                                           OCCURS  3.
      *                人名辞書使用
                       09  SPA-GMN-JISYOKBN-G.
                           11  SPA-GMN-JISYOKBN            PIC X(01).
                           11  SPA-GMN-JISYOKBN-F1         PIC X(01).
                           11  SPA-GMN-JISYOKBNMEI         PIC X(26).
                       09  SPA-GMN-JISYOKBN-COMB.
                           11  SPA-GMN-JISYOKBN-LIST       PIC X(28)
                                                           OCCURS  2.
      *                患者登録のカルテ発行
                       09  SPA-GMN-HAKKOKBN-G.
                           11  SPA-GMN-HAKKOKBN            PIC X(01).
                           11  SPA-GMN-HAKKOKBN-F1         PIC X(01).
                           11  SPA-GMN-HAKKOKBNMEI         PIC X(26).
                       09  SPA-GMN-HAKKOKBN-COMB.
                           11  SPA-GMN-HAKKOKBN-LIST       PIC X(28)
                                                           OCCURS  3.
      *                自費保険の補助区分
                       09  SPA-GMN-HOJYOKBN-G.
                           11  SPA-GMN-HOJYOKBN            PIC X(01).
                           11  SPA-GMN-HOJYOKBN-F1         PIC X(01).
                           11  SPA-GMN-HOJYOKBNMEI         PIC X(26).
                       09  SPA-GMN-HOJYOKBN-COMB.
                           11  SPA-GMN-HOJYOKBN-LIST       PIC X(28)
                                                           OCCURS  2.
      *                同姓同名リスト
                       09  SPA-GMN-DOUMEI-CHKFLG-G.
                           11  SPA-GMN-DOUMEI-CHKFLG       PIC X(01).
                           11  SPA-GMN-DOUMEI-F1           PIC X(01).
                           11  SPA-GMN-DOUMEI-MEI          PIC X(26).
                       09  SPA-GMN-DOUMEI-CHKFLG-COMB.
                           11  SPA-GMN-DOUMEI-CHKFLG-LIST  PIC X(28)
                                                           OCCURS  2.
      *                二重登録疑い区分
                       09  SPA-GMN-JYUFUKU-CHKKBN-G.
                           11  SPA-GMN-JYUFUKU-CHKKBN      PIC X(01).
                           11  SPA-GMN-JYUFUKU-F1          PIC X(01).
                           11  SPA-GMN-JYUFUKU-MEI         PIC X(58).
                       09  SPA-GMN-JYUFUKU-CHKKBN-COMB.
                           11  SPA-GMN-JYUFUKU-CHKKBN-LIST PIC X(60)
                                                           OCCURS  5.
                       09  SPA-GMN-JYUFUKU-MAX             PIC 9(02).
      *                地方公費単独チェック
                       09  SPA-GMN-KOHONLY-CHKFLG-G.
                           11  SPA-GMN-KOHONLY-CHKFLG      PIC X(01).
                           11  SPA-GMN-KOHONLY-F1          PIC X(01).
                           11  SPA-GMN-KOHONLY-MEI         PIC X(26).
                       09  SPA-GMN-KOHONLY-CHKFLG-COMB.
                           11  SPA-GMN-KOHONLY-CHKFLG-LIST PIC X(28)
                                                           OCCURS  2.
                       09  SPA-GMN-KOHONLY-MAX             PIC 9(02).
      *                患者情報削除機能
                       09  SPA-GMN-PTNUMDEL-FLG-G.
                           11  SPA-GMN-PTNUMDEL-FLG         PIC X(01).
                           11  SPA-GMN-PTNUMDEL-F1          PIC X(01).
                           11  SPA-GMN-PTNUMDEL-MEI         PIC X(26).
                       09  SPA-GMN-PTNUMDEL-FLG-COMB.
                           11  SPA-GMN-PTNUMDEL-FLG-LIST PIC X(28)
                                                           OCCURS  2.
                       09  SPA-GMN-PTNUMDEL-MAX             PIC 9(02).
                   07  SPA-GMN-PAGE3-AREA.
      *                入金管理情報（診療行為機能情報）
                       09  SPA-GMN-SRYNYKNKNRKBN-G.
                           11  SPA-GMN-SRYNYKNKNRKBN       PIC X(01).
                           11  SPA-GMN-SRYNYKNKNRKBN-F1    PIC X(01).
                           11  SPA-GMN-SRYNYKNKNRKBNMEI    PIC X(38).
                       09  SPA-GMN-SRYNYKNKNRKBNLST-G.
                           11  SPA-GMN-SRYNYKNKNRKBN-MAX   PIC 9(03).
                           11  SPA-GMN-SRYNYKNKNRKBNLST    OCCURS  5.
                               13  SPA-GMN-SRYNYKNKNRKBNL  PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SRYNYKNKNRKBNMEIL
                                                           PIC X(38).
      *                複数科まとめ集計
                       09  SPA-GMN-KAGRPFLG-G.
                           11  SPA-GMN-KAGRPFLG       PIC X(01).
                           11  SPA-GMN-KAGRPFLG-F1    PIC X(01).
                           11  SPA-GMN-KAGRPFLG-MEI   PIC X(38).
                       09  SPA-GMN-KAGRPFLGLST-G.
                           11  SPA-GMN-KAGRPFLG-MAX   PIC 9(03).
                           11  SPA-GMN-KAGRPFLG-LST    OCCURS  5.
                               13  SPA-GMN-KAGRPFLGL  PIC X(01).
                               13                      PIC X(01).
                               13  SPA-GMN-KAGRPFLGMEIL
                                                       PIC X(38).
      *                請求書発行方法
                       09  SPA-GMN-HAKHOUFLG-G.
                           11  SPA-GMN-HAKHOUFLG       PIC X(01).
                           11  SPA-GMN-HAKHOUFLG-F1    PIC X(01).
                           11  SPA-GMN-HAKHOUFLG-MEI   PIC X(38).
                       09  SPA-GMN-HAKHOUFLGLST-G.
                           11  SPA-GMN-HAKHOUFLG-MAX   PIC 9(03).
                           11  SPA-GMN-HAKHOUFLG-LST    OCCURS  5.
                               13  SPA-GMN-HAKHOUFLGL  PIC X(01).
                               13                      PIC X(01).
                               13  SPA-GMN-HAKHOUFLGMEIL
                                                       PIC X(38).
      *                ＤＯ初期表示
                       09  SPA-GMN-DOSRYKBNFLG-G.
                           11  SPA-GMN-DOSRYKBNFLG       PIC X(02).
                           11  SPA-GMN-DOSRYKBNFLG-F1    PIC X(01).
                           11  SPA-GMN-DOSRYKBNFLG-MEI   PIC X(36).
                       09  SPA-GMN-DOSRYKBNFLGLST-G.
                           11  SPA-GMN-DOSRYKBNFLG-LST   PIC X(40)
                                                       OCCURS  20.
                           11  SPA-GMN-DOSRYKBNFLG-MAX   PIC 9(03).
      *                中途表示一覧
                       09  SPA-GMN-WKSRYHYOFLG-G.
                           11  SPA-GMN-WKSRYHYOFLG       PIC X(01).
                           11  SPA-GMN-WKSRYHYOFLG-F1    PIC X(01).
                           11  SPA-GMN-WKSRYHYOFLG-MEI   PIC X(38).
                       09  SPA-GMN-WKSRYHYOFLGLST-G.
                           11  SPA-GMN-WKSRYHYOFLG-LST   PIC X(40)
                                                       OCCURS  5.
                           11  SPA-GMN-WKSRYHYOFLG-MAX   PIC 9(03).
                   07  SPA-GMN-PAGE4-AREA.
      *                入金管理情報（収納機能情報）
                       09  SPA-GMN-SYUNYKNKNRKBN-G.
                           11  SPA-GMN-SYUNYKNKNRKBN       PIC X(01).
                           11  SPA-GMN-SYUNYKNKNRKBN-F1    PIC X(01).
                           11  SPA-GMN-SYUNYKNKNRKBNMEI    PIC X(38).
      *                請求書兼領収書発行（外来）
                       09  SPA-GMN-SYUSKYPRTG-G.
                           11  SPA-GMN-SYUSKYPRTG          PIC X(01).
                           11  SPA-GMN-SYUSKYPRTG-F1       PIC X(01).
                           11  SPA-GMN-SYUSKYPRTGMEI       PIC X(22).
      *                診療費明細書発行（外来）
                       09  SPA-GMN-SYUSRYPRTG-G.
                           11  SPA-GMN-SYUSRYPRTG          PIC X(01).
                           11  SPA-GMN-SYUSRYPRTG-F1       PIC X(01).
                           11  SPA-GMN-SYUSRYPRTGMEI       PIC X(22).
      *                請求書兼領収書発行（入院）
                       09  SPA-GMN-SYUSKYPRTN-G.
                           11  SPA-GMN-SYUSKYPRTN          PIC X(01).
                           11  SPA-GMN-SYUSKYPRTN-F1       PIC X(01).
                           11  SPA-GMN-SYUSKYPRTNMEI       PIC X(22).
      *                診療費明細書発行（入院）
                       09  SPA-GMN-SYUSRYPRTN-G.
                           11  SPA-GMN-SYUSRYPRTN          PIC X(01).
                           11  SPA-GMN-SYUSRYPRTN-F1       PIC X(01).
                           11  SPA-GMN-SYUSRYPRTNMEI       PIC X(22).
      *                一括入金画面請求書兼領収書発行
                       09  SPA-GMN-S06SKYPRT-G.
                           11  SPA-GMN-S06SKYPRT           PIC X(01).
                           11  SPA-GMN-S06SKYPRT-F1        PIC X(01).
                           11  SPA-GMN-S06SKYPRTMEI        PIC X(22).
      *                一括入金画面外来分発行方法
                       09  SPA-GMN-S06SKYSUM-G.
                           11  SPA-GMN-S06SKYSUM           PIC X(01).
                           11  SPA-GMN-S06SKYSUM-F1        PIC X(01).
                           11  SPA-GMN-S06SKYSUMMEI        PIC X(30).
      *                請求一覧画面外来請求書兼領収書発行方法
                       09  SPA-GMN-S02SKYSAI-G.
                           11  SPA-GMN-S02SKYSAI           PIC X(01).
                           11  SPA-GMN-S02SKYSAI-F1        PIC X(01).
                           11  SPA-GMN-S02SKYSAIMEI        PIC X(40).
                       09  SPA-GMN-SYUSTIME.
                           11  SPA-GMN-SYUSTIME-HH         PIC 9(02).
                           11  SPA-GMN-SYUSTIME-MM         PIC 9(02).
                       09  SPA-GMN-SYUKARITIME-G.
                           11  SPA-GMN-SYUKARITIME-OCC1    OCCURS  2.
                               13  SPA-GMN-SYUKARIDAY-G.
                                   15  SPA-GMN-SYUKARIDAY  PIC X(01)
                                                           OCCURS  7.
                               13  SPA-GMN-SYUKARITIME-OCC2
                                                           OCCURS  2.
                                   15  SPA-GMN-SYUKARISTTIME.
                                       17  SPA-GMN-SYUKARIST-HH
                                                           PIC 9(02).
                                       17  SPA-GMN-SYUKARIST-MM
                                                           PIC 9(02).
                                   15  SPA-GMN-SYUKARIEDTIME.
                                       17  SPA-GMN-SYUKARIED-HH
                                                           PIC 9(02).
                                       17  SPA-GMN-SYUKARIED-MM
                                                           PIC 9(02).
      *
                       09  SPA-GMN-SYUNYKNKNRKBNLST-G.
                           11  SPA-GMN-SYUNYKNKNRKBN-MAX   PIC 9(03).
                           11  SPA-GMN-SYUNYKNKNRKBNLST    OCCURS  5.
                               13  SPA-GMN-SYUNYKNKNRKBNL  PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYUNYKNKNRKBNMEIL
                                                           PIC X(38).
                       09  SPA-GMN-SYUSKYPRTGLST-G.
                           11  SPA-GMN-SYUSKYPRTG-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSKYPRTGLST       OCCURS  2.
                               13  SPA-GMN-SYUSKYPRTGL     PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYUSKYPRTGMEIL
                                                           PIC X(22).
                       09  SPA-GMN-SYUSRYPRTGLST-G.
                           11  SPA-GMN-SYUSRYPRTG-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSRYPRTGLST       OCCURS  2.
                               13  SPA-GMN-SYUSRYPRTGL     PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYUSRYPRTGMEIL
                                                           PIC X(22).
                       09  SPA-GMN-SYUSKYPRTNLST-G.
                           11  SPA-GMN-SYUSKYPRTN-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSKYPRTNLST       OCCURS  2.
                               13  SPA-GMN-SYUSKYPRTNL     PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYUSKYPRTNMEIL
                                                           PIC X(22).
                       09  SPA-GMN-SYUSRYPRTNLST-G.
                           11  SPA-GMN-SYUSRYPRTN-MAX      PIC 9(03).
                           11  SPA-GMN-SYUSRYPRTNLST       OCCURS  2.
                               13  SPA-GMN-SYUSRYPRTNL     PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYUSRYPRTNMEIL
                                                           PIC X(22).
                       09  SPA-GMN-S06SKYPRTLST-G.
                           11  SPA-GMN-S06SKYPRT-MAX       PIC 9(03).
                           11  SPA-GMN-S06SKYPRTLST        OCCURS  2.
                               13  SPA-GMN-S06SKYPRTL      PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-S06SKYPRTMEIL
                                                           PIC X(22).
                       09  SPA-GMN-S06SKYSUMLST-G.
                           11  SPA-GMN-S06SKYSUM-MAX       PIC 9(03).
                           11  SPA-GMN-S06SKYSUMLST        OCCURS  5.
                               13  SPA-GMN-S06SKYSUML      PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-S06SKYSUMMEIL
                                                           PIC X(40).
                       09  SPA-GMN-S02SKYSAILST-G.
                           11  SPA-GMN-S02SKYSAI-MAX       PIC 9(03).
                           11  SPA-GMN-S02SKYSAILST        OCCURS  5.
                               13  SPA-GMN-S02SKYSAIL      PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-S02SKYSAIMEIL
                                                           PIC X(30).
                   07  SPA-GMN-PAGE5-AREA.
      *                病名機能情報
                       09  SPA-GMN-MSG1-DSPKBN-G.
                           11  SPA-GMN-MSG1-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG1-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG1-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG1-DSPKBNLST-G.
                           11  SPA-GMN-MSG1-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG1-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG1-DSPKBNL    PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-MSG1-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG2-DSPKBN-G.
                           11  SPA-GMN-MSG2-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG2-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG2-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG2-DSPKBNLST-G.
                           11  SPA-GMN-MSG2-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG2-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG2-DSPKBNL    PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-MSG2-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG3-DSPKBN-G.
                           11  SPA-GMN-MSG3-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG3-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG3-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG3-DSPKBNLST-G.
                           11  SPA-GMN-MSG3-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG3-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG3-DSPKBNL    PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-MSG3-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-MSG4-DSPKBN-G.
                           11  SPA-GMN-MSG4-DSPKBN         PIC X(01).
                           11  SPA-GMN-MSG4-DSPKBN-F1      PIC X(01).
                           11  SPA-GMN-MSG4-DSPKBNMEI      PIC X(10).
      *
                       09  SPA-GMN-MSG4-DSPKBNLST-G.
                           11  SPA-GMN-MSG4-DSPKBN-MAX     PIC 9(03).
                           11  SPA-GMN-MSG4-DSPKBNLST      OCCURS  2.
                               13  SPA-GMN-MSG4-DSPKBNL    PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-MSG4-DSPKBNMEIL PIC X(10).
      *
                       09  SPA-GMN-SRYKA-DSPKBN-G.
                           11  SPA-GMN-SRYKA-DSPKBN        PIC X(01).
                           11  SPA-GMN-SRYKA-DSPKBN-F1     PIC X(01).
                           11  SPA-GMN-SRYKA-DSPKBNMEI     PIC X(38).
      *
                       09  SPA-GMN-SRYKA-DSPKBNLST-G.
                           11  SPA-GMN-SRYKA-DSPKBN-MAX    PIC 9(02).
                           11  SPA-GMN-SRYKA-DSPKBNLST     OCCURS  3.
                               13  SPA-GMN-SRYKA-DSPKBNL   PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SRYKA-DSPKBNMEIL
                                                           PIC X(38).
      *
                       09  SPA-GMN-SYOKBN-DSPKBN-G.
                           11  SPA-GMN-SYOKBN-DSPKBN       PIC X(01).
                           11  SPA-GMN-SYOKBN-DSPKBN-F1    PIC X(01).
                           11  SPA-GMN-SYOKBN-DSPKBNMEI    PIC X(10).
      *
                       09  SPA-GMN-SYOKBN-DSPKBNLST-G.
                           11  SPA-GMN-SYOKBN-DSPKBN-MAX   PIC 9(02).
                           11  SPA-GMN-SYOKBN-DSPKBNLST    OCCURS  3.
                               13  SPA-GMN-SYOKBN-DSPKBNL  PIC X(01).
                               13                          PIC X(01).
                               13  SPA-GMN-SYOKBN-DSPKBNMEIL
                                                           PIC X(10).
      *                詳記区分初期設定
                       09  SPA-GMN-SJKBN-G.
                           11  SPA-GMN-SJKBN               PIC X(02).
                           11  SPA-GMN-SJKBN-F1            PIC X(01).
                           11  SPA-GMN-SJKBN-MEI           PIC X(50).
                       09  SPA-GMN-SJKBN-LST-G.
                           11  SPA-GMN-SJKBN-MAX           PIC 9(02).
                           11  SPA-GMN-SJKBNLST            PIC X(53)
                                                           OCCURS  20.
      *
      *            会計照会情報
                   07  SPA-GMN-PAGE6-AREA.
      *                表示順
                       09  SPA-GMN-SRYKBN-FLG-G.
                           11  SPA-GMN-SRYKBN-FLG          PIC X(01).
                           11  SPA-GMN-SRYKBN-F1           PIC X(01).
                           11  SPA-GMN-SRYKBN-MEI          PIC X(10).
                       09  SPA-GMN-SRYKBN-COMB.
                           11  SPA-GMN-SRYKBN-LIST         PIC X(12)
                                                           OCCURS  2.
                       09  SPA-GMN-SRYKBN-MAX              PIC 9(04).
      *                初期表示
                       09  SPA-GMN-SRYKA-FLG-G.
                           11  SPA-GMN-SRYKA-FLG          PIC X(01).
                           11  SPA-GMN-SRYKA-F1           PIC X(01).
                           11  SPA-GMN-SRYKA-MEI          PIC X(18).
                       09  SPA-GMN-SRYKA-COMB.
                           11  SPA-GMN-SRYKA-LIST         PIC X(20)
                                                           OCCURS  5.
                       09  SPA-GMN-SRYKA-MAX              PIC 9(04).
      *
                   07  SPA-GMN-DATALST        OCCURS  2.
                       09  SPA-GMN-DATAL      PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-DATAMEIL   PIC X(10).
                   07  SPA-GMN-DATA-MAX       PIC 9(02).
      *
                   07  SPA-GMN-DATALST2       OCCURS  3.
                       09  SPA-GMN-DATAL2     PIC X(01).
                       09  SPA-GMN-F1L2       PIC X(01).
                       09  SPA-GMN-DATAMEIL2  PIC X(20).
                   07  SPA-GMN-DATA-MAX2      PIC 9(02).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
      *
      *                チェック除外時間
                   07  SPA-NAI-JIKAN-TIME.
                       09  SPA-NAI-JIKANCHK-WEEK           OCCURS   2.
                           11  SPA-NAI-JIKANCHK-WST.
                               13  SPA-NAI-JIKANWST-HH      PIC 9(02).
                               13  SPA-NAI-JIKANWST-MM      PIC 9(02).
                           11  SPA-NAI-JIKANCHK-WED.
                               13  SPA-NAI-JIKANWED-HH      PIC 9(02).
                               13  SPA-NAI-JIKANWED-MM      PIC 9(02).
                       09  SPA-NAI-JIKANCHK-SAT             OCCURS   2.
                           11  SPA-NAI-JIKANCHK-SST.
                               13  SPA-NAI-JIKANSST-HH      PIC 9(02).
                               13  SPA-NAI-JIKANSST-MM      PIC 9(02).
                           11  SPA-NAI-JIKANCHK-SED.
                               13  SPA-NAI-JIKANSED-HH      PIC 9(02).
                               13  SPA-NAI-JIKANSED-MM      PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDT                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-ERR-CUR         PIC 9(03).
           03  WRK-KARIDAY         PIC X(01).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(04).
               05  WRK-WIDGET2     PIC X(02).
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-YMD             PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  PAT-1039-REC.
           03  PAT-1039-KEY.
      *管理コード
               05  PAT-1039-KANRICD     PIC  X(04).
      *区分コード
               05  PAT-1039-KBNCD       PIC  X(08).
      *有効開始年月日
               05  PAT-1039-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  PAT-1039-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  PAT-1039-TBL.
      *入金管理区分
               05  PAT-1039-NYKNKNRKBN  PIC  X(01).
      *請求書兼領収書発行区分（外来）
               05  PAT-1039-SKYPRTGKBN  PIC  X(01).
      *請求書兼領収書発行区分（入院）
               05  PAT-1039-SKYPRTNKBN  PIC  X(01).
      *統計時間
               05  PAT-1039-STIME.
                   07  PAT-1039-STIME-HH   PIC  X(02).
                   07  PAT-1039-STIME-MM   PIC  X(02).
      *仮締時間
               05  PAT-1039-KARITIME.
                   07  PAT-1039-KARITIME-OCC1          OCCURS  2.
                       09  PAT-1039-KARIDAY    PIC X   OCCURS  7.
                       09  PAT-1039-KARITIME-OCC2      OCCURS  2.
                           11  PAT-1039-KARISTTIME.
                               13  PAT-1039-KARIST-HH  PIC X(02).
                               13  PAT-1039-KARIST-MM  PIC X(02).
                           11  PAT-1039-KARIEDTIME.
                               13  PAT-1039-KARIED-HH  PIC X(02).
                               13  PAT-1039-KARIED-MM  PIC X(02).
      *診療費明細書発行区分（外来）
               05  PAT-1039-SRYPRTGKBN  PIC  X(01).
      *診療費明細書発行区分（入院）
               05  PAT-1039-SRYPRTNKBN  PIC  X(01).
      *一括入金画面請求書兼領収書発行区分（入院）
               05  PAT-1039-S06SKYPRTNKBN
                                        PIC  X(01).
      *一括入金画面請求書兼領収書発行区分（外来）
               05  PAT-1039-S06SKYPRTGKBN
                                        PIC  X(01).
      *一括入金画面外来分発行方法
               05  PAT-1039-S06SKYSUMKBN
                                        PIC  X(01).
      *請求一覧画面外来請求書兼領収書発行方法
               05  PAT-1039-S02SKYSAIKBN
                                        PIC  X(01).
      *請求一覧画面外来請求書兼領収書未収印字区分
               05  PAT-1039-S02SKYMISYUPRTKBN
                                        PIC  X(01).
      *請求確認画面外来請求書兼領収書未収印字区分
               05  PAT-1039-S03SKYMISYUPRTKBN
                                        PIC  X(01).
      *予備
               05                       PIC  X(439).
      *端末ＩＤ
           03  PAT-1039-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  PAT-1039-OPID            PIC  X(16).
      *作成年月日
           03  PAT-1039-CREYMD          PIC  X(08).
      *更新年月日
           03  PAT-1039-UPYMD           PIC  X(08).
      *更新時間
           03  PAT-1039-UPHMS           PIC  X(06).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
           COPY  "CPSK1001.INC".
           COPY  "CPSK5000.INC".
      *
      *
      *    自動算定情報
           COPY  "CPSK1007.INC".
      *    チェック機能制御
           COPY  "CPSK1008.INC".
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *    診療科情報
           COPY  "CPSK1005.INC".
      *    診療行為機能情報
           COPY  "CPSK1038.INC".
      *    収納機能情報
           COPY  "CPSK1039.INC".
      *    病名機能情報
           COPY  "CPSK1042.INC".
      *    会計照会機能情報
           COPY  "CPSK1043.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "W01.INC".
           COPY    "W02.INC".
           COPY    "W03.INC".
           COPY    "W04.INC".
           COPY    "W05.INC".
           COPY    "W06.INC".
           COPY    "W07.INC".
           COPY    "W08.INC".
           COPY    "W09.INC".
           COPY    "W10.INC".
           COPY    "W11.INC".
           COPY    "W111.INC".
           COPY    "W12.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W26.INC".
           COPY    "W27.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W98.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W07
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W07         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 510-GSRAUTH-SEC
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE                TO  LINKAREA
      *
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "W07"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               EVALUATE      SPA-WIDCD
                   WHEN   "1001"
      *                自動算定情報の更新
                       PERFORM  491-KOUSHIN-1007-SEC
                       PERFORM  491-KOUSHIN-1008-SEC
                   WHEN   "1002"
      *                自動算定情報の削除
                       PERFORM  4032-DEL-1007-SEC
                       PERFORM  4032-DEL-1008-SEC
                   WHEN   "1003"
      *                患者登録情報の更新
                       PERFORM  491-KOUSHIN-1017-SEC
                   WHEN   "1004"
      *                患者登録情報の削除
                       PERFORM  4032-DEL-1017-SEC
                   WHEN   "1005"
      *                診療行為情報の更新
                       PERFORM  491-KOUSHIN-1038-SEC
                   WHEN   "1006"
      *                診療行為情報の削除
                       PERFORM  4032-DEL-1038-SEC
                   WHEN   "1007"
      *                収納情報の更新
                       PERFORM  491-KOUSHIN-1039-SEC
                   WHEN   "1008"
      *                収納情報の削除
                       PERFORM  4032-DEL-1039-SEC
                   WHEN   "1009"
      *                病名情報の更新
                       PERFORM  491-KOUSHIN-1042-SEC
                   WHEN   "1010"
      *                病名情報の更新
                       PERFORM  4032-DEL-1042-SEC
                   WHEN   "1011"
      *                会計照会情報の更新
                       PERFORM  493-KOUSHIN-1043-SEC
                   WHEN   "1012"
      *                会計照会情報の削除
                       PERFORM  4033-DEL-1043-SEC
               END-EVALUATE
           ELSE
               EVALUATE    SPA-W01-KANRICD
                   WHEN    "1007"
                       MOVE    2            TO  SPA-GMN-CUR
                   WHEN    "1017"
                       MOVE    20           TO  SPA-GMN-CUR
                   WHEN    "1038"
                       MOVE    30           TO  SPA-GMN-CUR
                   WHEN    "1043"
                       MOVE    100          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W07
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           MOVE    "0"                 TO  SPA-GMN-DATAL(1)
           MOVE    "算定しない"        TO  SPA-GMN-DATAMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-DATAL(2)
           MOVE    "算定する"          TO  SPA-GMN-DATAMEIL(2)
           MOVE    2                   TO  SPA-GMN-DATA-MAX
      *    薬剤情報提供料
           MOVE    "0"                 TO  SPA-GMN-DATAL2(1)
           MOVE    "算定しない"        TO  SPA-GMN-DATAMEIL2(1)
           MOVE    "1"                 TO  SPA-GMN-DATAL2(2)
           MOVE    "算定する（月１回）"
                                       TO  SPA-GMN-DATAMEIL2(2)
           MOVE    "2"                 TO  SPA-GMN-DATAL2(3)
           MOVE    "算定する（毎回）" 
                                       TO  SPA-GMN-DATAMEIL2(3)
           MOVE    3                   TO  SPA-GMN-DATA-MAX2
      *
           MOVE    "0 チェックなし"    TO  SPA-GMN-GAIKANRIKBN-LIST(1)
           MOVE    "1 チェックあり"    TO  SPA-GMN-GAIKANRIKBN-LIST(2)
           MOVE    "2 自動発生なし"    TO  SPA-GMN-GAIKANRIKBN-LIST(3)
      *
           MOVE    "0 標準"            TO  SPA-GMN-GAIMOVE-LIST(1)
           MOVE    "1 保険組合せ"      TO  SPA-GMN-GAIMOVE-LIST(2)
           MOVE    "2 診療科"          TO  SPA-GMN-GAIMOVE-LIST(3)
      *
      *
           MOVE    "0 包括分入力をしない"
                                       TO  SPA-GMN-HOUKATU-LIST(1)
           MOVE    "1 包括分入力をする"
                                       TO  SPA-GMN-HOUKATU-LIST(2)
      *
      *
           MOVE    "0 標準（行削除する）"
                                       TO  SPA-GMN-SURYOZERO-LIST(1)
           MOVE    "1 入力エラーとする"
                                       TO  SPA-GMN-SURYOZERO-LIST(2)
      *    時間外加算チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-JIKANTOKU-CHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-JIKANTOKU-CHK-LIST(2)
      *
      *    入院調基チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-CHOKICHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-CHOKICHK-LIST(2)
      *    クリア確認
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-CLEARCHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-CLEARCHK-LIST(2)
      *    薬剤情報提供料チェック
           MOVE    "0 チェックしない"
                                       TO  SPA-GMN-YAKJYOCHK-LIST(1)
           MOVE    "1 チェックする"
                                       TO  SPA-GMN-YAKJYOCHK-LIST(2)
      *    病名表示区分
           MOVE    "0 表示しない"      TO  SPA-GMN-BYOHYOKBN-LIST(1)
           MOVE    "1 表示する"        TO  SPA-GMN-BYOHYOKBN-LIST(2)
      *    剤登録順区分
           MOVE    "0 診療区分順"      TO  SPA-GMN-ZAIJUNKBN-LIST(1)
           MOVE    "1 入力順"          TO  SPA-GMN-ZAIJUNKBN-LIST(2)
      *    検体検査加算自動発生
           MOVE    "0 算定しない"      TO  SPA-GMN-KENSAAUTO-LIST(1)
           MOVE    "1 算定する"        TO  SPA-GMN-KENSAAUTO-LIST(2)
      *
           MOVE    "0 すべての病名"    TO  SPA-GMN-BYOMEIKBN-LIST(1)
           MOVE    "1 継続病名のみ"    TO  SPA-GMN-BYOMEIKBN-LIST(2)
           MOVE    "2 病名記載なし"    TO  SPA-GMN-BYOMEIKBN-LIST(3)
      *
           MOVE    "0 使用する"        TO  SPA-GMN-JISYOKBN-LIST(1)
           MOVE    "1 使用しない"      TO  SPA-GMN-JISYOKBN-LIST(2)
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-HAKKOKBN-LIST(1)
           MOVE    "1 発行する（保険組合せ指示）"
                                       TO  SPA-GMN-HAKKOKBN-LIST(2)
           MOVE    "2 発行する（保険組合せ自動）"
                                       TO  SPA-GMN-HAKKOKBN-LIST(3)
      *
           MOVE    "0 課税"            TO  SPA-GMN-HOJYOKBN-LIST(1)
           MOVE    "1 非課税"          TO  SPA-GMN-HOJYOKBN-LIST(2)
      *
           MOVE    "0 表示する"        TO  SPA-GMN-DOUMEI-CHKFLG-LIST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-DOUMEI-CHKFLG-LIST(2)
      *
           MOVE    "0 カナ氏名＋性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (1)
           MOVE    "1 カナ氏名＋漢字氏名＋性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (2)
           MOVE    "2 性別＋生年月日" 
                                   TO  SPA-GMN-JYUFUKU-CHKKBN-LIST (3)
           MOVE    3                   TO  SPA-GMN-JYUFUKU-MAX 
      *
           MOVE    "0 チェックしない" 
                                   TO  SPA-GMN-KOHONLY-CHKFLG-LIST (1)
           MOVE    "1 チェックする" 
                                   TO  SPA-GMN-KOHONLY-CHKFLG-LIST (2)
           MOVE    2               TO  SPA-GMN-KOHONLY-MAX 
      *
           MOVE    "0 削除機能あり" 
                                   TO  SPA-GMN-PTNUMDEL-FLG-LIST (1)
           MOVE    "1 削除機能なし" 
                                   TO  SPA-GMN-PTNUMDEL-FLG-LIST (2)
           MOVE    2               TO  SPA-GMN-PTNUMDEL-MAX 
      *
           MOVE    "1 今回請求分のみ入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(1)
           MOVE    "2 今回分・伝票の古い未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(2)
           MOVE    "3 今回分・伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(3)
           MOVE    "4 伝票の古い未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(4)
           MOVE    "5 伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SRYNYKNKNRKBNLST(5)
           MOVE    5                   TO  SPA-GMN-SRYNYKNKNRKBN-MAX
      *
           MOVE    "0 複数科まとめ集計をする"
                                       TO  SPA-GMN-KAGRPFLG-LST(1)
           MOVE    "1 複数科まとめ集計をしない"
                                       TO  SPA-GMN-KAGRPFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-KAGRPFLG-MAX
      *
      *
           MOVE    "1 診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(1)
           MOVE    "2 保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(2)
           MOVE    "3 診療科別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(3)
           MOVE    "4 全体をまとめて発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(4)
           MOVE    4                   TO  SPA-GMN-HAKHOUFLG-MAX
      *
           MOVE    "1 伝票の古い未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(1)
           MOVE    "2 伝票の新しい未収順に入金"
                                       TO  SPA-GMN-SYUNYKNKNRKBNLST(2)
           MOVE    2                   TO  SPA-GMN-SYUNYKNKNRKBN-MAX
      *    ＤＯ初期表示
           MOVE    "00 全体"           TO  SPA-GMN-DOSRYKBNFLG-LST (01)
           MOVE    "10 医学管理・在宅" TO  SPA-GMN-DOSRYKBNFLG-LST (02)
           MOVE    "20 投薬"           TO  SPA-GMN-DOSRYKBNFLG-LST (03)
           MOVE    "30 注射"           TO  SPA-GMN-DOSRYKBNFLG-LST (04)
           MOVE    "40 処置"           TO  SPA-GMN-DOSRYKBNFLG-LST (05)
           MOVE    "50 手術・麻酔"     TO  SPA-GMN-DOSRYKBNFLG-LST (06)
           MOVE    "60 検査"           TO  SPA-GMN-DOSRYKBNFLG-LST (07)
           MOVE    "70 画像診断"       TO  SPA-GMN-DOSRYKBNFLG-LST (08)
           MOVE    "80 リハビリ・その他"
                                       TO  SPA-GMN-DOSRYKBNFLG-LST (09)
           MOVE    "90 自費・その他"   TO  SPA-GMN-DOSRYKBNFLG-LST (10)
           MOVE    10                  TO  SPA-GMN-DOSRYKBNFLG-MAX
      *    中途一覧表示順
           MOVE    "0 患者番号順"      TO  SPA-GMN-WKSRYHYOFLG-LST(1)
           MOVE    "1 登録順"          TO  SPA-GMN-WKSRYHYOFLG-LST(2)
           MOVE    2                   TO  SPA-GMN-WKSRYHYOFLG-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSKYPRTGLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSKYPRTGLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSKYPRTG-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSRYPRTGLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSRYPRTGLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSRYPRTG-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSKYPRTNLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSKYPRTNLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSKYPRTN-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-SYUSRYPRTNLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-SYUSRYPRTNLST(2)
           MOVE    2                   TO  SPA-GMN-SYUSRYPRTN-MAX
      *
           MOVE    "0 発行しない"      TO  SPA-GMN-S06SKYPRTLST(1)
           MOVE    "1 発行する"        TO  SPA-GMN-S06SKYPRTLST(2)
           MOVE    2                   TO  SPA-GMN-S06SKYPRT-MAX
      *
           MOVE    "0 個別に発行"      TO  SPA-GMN-S06SKYSUMLST(1)
           MOVE    "1 月毎に診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(2)
           MOVE    "2 月毎に保険組合せ別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(3)
           MOVE    "3 月毎に診療科別に発行"
                                       TO  SPA-GMN-S06SKYSUMLST(4)
           MOVE    "4 月毎にまとめて発行"
                                       TO  SPA-GMN-S06SKYSUMLST(5)
           MOVE    5                   TO  SPA-GMN-S06SKYSUM-MAX
      *
           MOVE    "0 個別に発行"      TO  SPA-GMN-S02SKYSAILST(1)
           MOVE    "1 診療行為で指定した方法で発行"
                                       TO  SPA-GMN-S02SKYSAILST(2)
           MOVE    "2 個別に（前回請求印字なし）"
                                       TO  SPA-GMN-S02SKYSAILST(3)
           MOVE    "3 診療行為で（前回請求印字なし）"
                                       TO  SPA-GMN-S02SKYSAILST(4)
           MOVE    4                   TO  SPA-GMN-S02SKYSAI-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG1-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG1-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG1-DSPKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG2-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG2-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG2-DSPKBN-MAX
      *
           MOVE    "0 表示する"        TO  SPA-GMN-MSG3-DSPKBNLST(1)
           MOVE    "1 表示しない"      TO  SPA-GMN-MSG3-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG3-DSPKBN-MAX
      *
           MOVE    "0 表示しない"      TO  SPA-GMN-MSG4-DSPKBNLST(1)
           MOVE    "1 表示する"        TO  SPA-GMN-MSG4-DSPKBNLST(2)
           MOVE    2                   TO  SPA-GMN-MSG4-DSPKBN-MAX
      *
           MOVE    "0 標準（最終受診科、遷移時は選択診療科）"
                                       TO  SPA-GMN-SRYKA-DSPKBNLST(1)
           MOVE    "1 全科"            TO  SPA-GMN-SRYKA-DSPKBNLST(2)
           MOVE    "2 全科（遷移時は選択診療科）"
                                       TO  SPA-GMN-SRYKA-DSPKBNLST(3)
           MOVE    3                   TO  SPA-GMN-SRYKA-DSPKBN-MAX
      *
           MOVE    "0 当月"            TO  SPA-GMN-SYOKBN-DSPKBNLST(1)
           MOVE    "1 前月"            TO  SPA-GMN-SYOKBN-DSPKBNLST(2)
           MOVE    "2 全体"            TO  SPA-GMN-SYOKBN-DSPKBNLST(3)
           MOVE    3                   TO  SPA-GMN-SYOKBN-DSPKBN-MAX
      *    コメント症状詳記区分初期
           MOVE    "01 主たる疾患の臨床症状"
                                       TO  SPA-GMN-SJKBNLST (01)
           MOVE    "02 主たる疾患の診療・検査所見"
                                       TO  SPA-GMN-SJKBNLST (02)
           MOVE    "03 主な治療行為の必要性"
                                       TO  SPA-GMN-SJKBNLST (03)
           MOVE    "04 主な治療行為の経過"
                                       TO  SPA-GMN-SJKBNLST (04)
           MOVE    "05 １００万点以上の薬剤に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (05)
           MOVE    "06 １００万点以上の処置に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (06)
           MOVE    "07 その他（１）"
                                       TO  SPA-GMN-SJKBNLST (07)
           MOVE    "08 その他（２）"
                                       TO  SPA-GMN-SJKBNLST (08)
           MOVE    "09 その他（３）"
                                       TO  SPA-GMN-SJKBNLST (09)
      *    Ｈ１９．４からの追加
           MOVE    "50 治験概要"
                                       TO  SPA-GMN-SJKBNLST (10)
           MOVE    "51 疾患別リハビリテーションに係る治療継続理由"
                                       TO  SPA-GMN-SJKBNLST (11)
           MOVE    "99 レセプト摘要欄コメント"
                                       TO  SPA-GMN-SJKBNLST (12)
           MOVE    12                  TO  SPA-GMN-SJKBN-MAX
      *
      *    会計照会情報
           MOVE    "0 診療科順"        TO  SPA-GMN-SRYKBN-LIST(1)
           MOVE    "1 診療区分順"      TO  SPA-GMN-SRYKBN-LIST(2)
           MOVE    2                   TO  SPA-GMN-SRYKBN-MAX
      *    診療科初期表示
           MOVE    "0 標準"            TO  SPA-GMN-SRYKA-LIST(1)
           MOVE    "1 前回表示科"      TO  SPA-GMN-SRYKA-LIST(2)
           MOVE    "2 全科"            TO  SPA-GMN-SRYKA-LIST(3)
           MOVE    3                   TO  SPA-GMN-SRYKA-MAX
      *
           EVALUATE    SPA-W01-KANRICD
      *        診療行為情報
               WHEN    "1007"
                   PERFORM 310-SPASET-1007-SEC
      *        患者登録機能制御情報
               WHEN    "1017"
                   PERFORM 310-SPASET-1017-SEC
      *        診療行為機能情報
               WHEN    "1038"
                   PERFORM 310-SPASET-1038-SEC
      *        収納機能情報
               WHEN    "1039"
                   PERFORM 310-SPASET-1039-SEC
      *        病名機能情報
               WHEN    "1042"
                   PERFORM 310-SPASET-1042-SEC
      *        会計照会機能情報
               WHEN    "1043"
                   PERFORM 310-SPASET-1043-SEC
           END-EVALUATE
      *
           . 
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1007-SEC         SECTION.
      *
      *    自動算定情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1007"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1007-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       6
                   MOVE    SYS-1007-SANTEI(IDX:1)
                                               TO  SPA-GMN-DATA(IDX)
                   IF      IDX             =   2   OR  3  OR  4
      *            薬剤情報提供料
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >   SPA-GMN-DATA-MAX2
                           IF      SPA-GMN-DATA(IDX)
                                               =   SPA-GMN-DATAL2(IDY)
                               MOVE    SPA-GMN-DATALST2(IDY)
                                               TO SPA-GMN-DATA-G(IDX)
                               MOVE    SPA-GMN-DATA-MAX2   TO  IDY
                           END-IF
                       END-PERFORM
                   ELSE
      *            薬剤情報提供料以外
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >   SPA-GMN-DATA-MAX
                           IF      SPA-GMN-DATA(IDX)
                                               =   SPA-GMN-DATAL(IDY)
                               MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-DATA-G(IDX)
                               MOVE    SPA-GMN-DATA-MAX   TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
      *    チェック機能制御割り当て情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1008"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1008-REC
      *
      *        育児自動算定
               MOVE    SYS-1008-IKUJIEIYO-FLG
                                       TO  SPA-GMN-DATA(7)
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >   SPA-GMN-DATA-MAX
                   IF      SPA-GMN-DATA(7)
                                       =   SPA-GMN-DATAL(IDY)
                       MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-DATA-G(7)
                       MOVE    SPA-GMN-DATA-MAX   TO  IDY
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-DATA(7)     =   "1"
      *            育児自動算定診療科
                   MOVE    SYS-1008-IKUJIEIYO-SRYKA
                                           TO  SPA-GMN-SRYKACD
               END-IF
      *
               MOVE    SYS-1008-INTERACT-KIKAN
                                           TO  SPA-GMN-INTERACT-KIKAN
      *        外来管理加算チェック
               MOVE    SYS-1008-GAIKANRIKBN-CHK
                                           TO  SPA-GMN-GAIKANRIKBN-CHK
      *        外来カーソル移動
               MOVE    SYS-1008-GAI-CURSOR-SET-1
                                           TO  SPA-GMN-GAIMOVE
      *        数量ゼロ入力
               MOVE    SYS-1008-SURYOZERO-FLG
                                           TO  SPA-GMN-SURYOZERO
      *        入院調基チェック
               MOVE    SYS-1008-CHOKICHK-FLG
                                           TO  SPA-GMN-CHOKICHK
      *
      *        入院初診料自動発生
               MOVE    SYS-1008-NYU-SYOSIN
                                       TO  SPA-GMN-NYUSYOSIN
      *
      *        包括まとめ
               MOVE    SYS-1008-HOUKATU-FLG
                                       TO  SPA-GMN-HOUKATU
      *        クリア確認
               MOVE    SYS-1008-CLEARCHK-FLG
                                       TO  SPA-GMN-CLEARCHK
      *        薬剤情報提供料チェック
               MOVE    SYS-1008-YAKJYOCHK-FLG
                                       TO  SPA-GMN-YAKJYOCHK
      *        最終受診日からの期間
               IF      SYS-1008-LAST-KIKAN-X =   SPACE
                   MOVE    ZERO            TO  SYS-1008-LAST-KIKAN
               END-IF
               MOVE    SYS-1008-LAST-KIKAN
                                       TO  SPA-GMN-LAST-KIKAN
      *        病名表示区分
               IF     SYS-1008-BYOHYOKBN-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-GMN-BYOHYOKBN
               ELSE
                   MOVE    SYS-1008-BYOHYOKBN-FLG
                                           TO  SPA-GMN-BYOHYOKBN
               END-IF
      *        剤登録順区分
               IF     SYS-1008-ZAIJUNKBN-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-ZAIJUNKBN
               ELSE
                   MOVE    SYS-1008-ZAIJUNKBN-FLG
                                           TO  SPA-GMN-ZAIJUNKBN
               END-IF
      *        検体検査加算自動発生
               IF     SYS-1008-KENSAAUTO-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-KENSAAUTO
               ELSE
                   MOVE    SYS-1008-KENSAAUTO-FLG
                                           TO  SPA-GMN-KENSAAUTO
               END-IF
      *
      *        時間外加算特例乳幼児
               MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                       TO  SPA-GMN-JIKANTOKU-NYU
      *        画像診断管理加算１
               MOVE    SYS-1008-GAZOUKSN1-FLG
                                       TO  SPA-GMN-GAZOUKSN1
      *        中途終了展開時の自動発生
               MOVE    SYS-1008-CHUTOTENKAI-FLG
                                       TO  SPA-GMN-CHUTOTENKAI
      *        訂正時の自動発生
               MOVE    SYS-1008-TEISEIAUTO-FLG
                                       TO  SPA-GMN-TEISEIAUTO
      *        訂正時の自動発生（特定疾患処方管理）
               MOVE    SYS-1008-TEISEITOKU-FLG
                                       TO  SPA-GMN-TEISEITOKU
      *
      *        時間外加算チェック
               MOVE    SYS-1008-JIKANTOKU-CHK
                                       TO  SPA-GMN-JIKANTOKU-CHK
      *        チェック除外時間
               MOVE    SYS-1008-JIKANTOKU-CHK
                                       TO  SPA-GMN-JIKANTOKU-CHK
      *        平日
               IF      SYS-1008-JIKANCHK-WEEK-G    NOT NUMERIC
                   MOVE    ZERO            TO  SYS-1008-JIKANCHK-WEEK-G
               END-IF
               MOVE      SYS-1008-JIKANCHK-WST(1)
                                           TO  SPA-NAI-JIKANCHK-WST(1)
               MOVE      SYS-1008-JIKANCHK-WST(2)
                                           TO  SPA-NAI-JIKANCHK-WST(2)
               MOVE      SYS-1008-JIKANCHK-WED(1)
                                           TO  SPA-NAI-JIKANCHK-WED(1)
               MOVE      SYS-1008-JIKANCHK-WED(2)
                                           TO  SPA-NAI-JIKANCHK-WED(2)
      *        土曜
               IF      SYS-1008-JIKANCHK-SAT-G    NOT NUMERIC
                   MOVE    ZERO            TO  SYS-1008-JIKANCHK-SAT-G
               END-IF
               MOVE      SYS-1008-JIKANCHK-SST(1)
                                           TO  SPA-NAI-JIKANCHK-SST(1)
               MOVE      SYS-1008-JIKANCHK-SST(2)
                                           TO  SPA-NAI-JIKANCHK-SST(2)
               MOVE      SYS-1008-JIKANCHK-SED(1)
                                           TO  SPA-NAI-JIKANCHK-SED(1)
               MOVE      SYS-1008-JIKANCHK-SED(2)
                                           TO  SPA-NAI-JIKANCHK-SED(2)
           END-IF
      *    チェック除外時間
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL       IDT     >   2
               MOVE    SPA-NAI-JIKANWST-HH(IDT)
                                      TO  SPA-GMN-JIKANWST-HH(IDT)
               MOVE    SPA-NAI-JIKANWST-MM(IDT)
                                      TO  SPA-GMN-JIKANWST-MM(IDT)
               MOVE    SPA-NAI-JIKANWED-HH(IDT)
                                      TO  SPA-GMN-JIKANWED-HH(IDT)
               MOVE    SPA-NAI-JIKANWED-MM(IDT)
                                      TO  SPA-GMN-JIKANWED-MM(IDT)
               MOVE    SPA-NAI-JIKANSST-HH(IDT)
                                      TO  SPA-GMN-JIKANSST-HH(IDT)
               MOVE    SPA-NAI-JIKANSST-MM(IDT)
                                      TO  SPA-GMN-JIKANSST-MM(IDT)
               MOVE    SPA-NAI-JIKANSED-HH(IDT)
                                      TO  SPA-GMN-JIKANSED-HH(IDT)
               MOVE    SPA-NAI-JIKANSED-MM(IDT)
                                      TO  SPA-GMN-JIKANSED-MM(IDT)
           END-PERFORM
      *
      *    入院初診料
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUSYOSIN
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                               TO SPA-GMN-NYUSYOSIN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(01) TO SPA-GMN-NYUSYOSIN-G
           END-IF
      *
      *    初期設定
           PERFORM VARYING IDX    FROM     1   BY  1
                   UNTIL   IDX    >   7
               IF      IDX             =   2   OR  3  OR  4
                   IF     SPA-GMN-DATA (IDX)   =   "0"  OR  "1"
                                                        OR  "2"
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-DATALST2(01)
                                       TO  SPA-GMN-DATA-G(IDX)
                   END-IF
               ELSE
                   IF     SPA-GMN-DATA (IDX)   =   "0"  OR  "1"
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-DATALST(01)
                                       TO  SPA-GMN-DATA-G(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    SPA-GMN-GAIMOVE
                WHEN    "1"
                    MOVE    SPA-GMN-GAIMOVE-LIST(2)
                                               TO  SPA-GMN-GAIMOVE-G
                WHEN    "2"
                    MOVE    SPA-GMN-GAIMOVE-LIST(3)
                                               TO  SPA-GMN-GAIMOVE-G
                WHEN    OTHER
                    MOVE    SPA-GMN-GAIMOVE-LIST(1)
                                               TO  SPA-GMN-GAIMOVE-G
            END-EVALUATE
      *
           IF      SPA-GMN-GAIKANRIKBN-CHK   =   SPACE
                MOVE    "1"     TO  SPA-GMN-GAIKANRIKBN-CHK
           END-IF
           EVALUATE    SPA-GMN-GAIKANRIKBN-CHK
                WHEN    "1"
                    MOVE    SPA-GMN-GAIKANRIKBN-LIST(2)
                                               TO  SPA-GMN-GAIKANRIKBN-G
                WHEN    "2"
                    MOVE    SPA-GMN-GAIKANRIKBN-LIST(3)
                                               TO  SPA-GMN-GAIKANRIKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-GAIKANRIKBN-LIST(1)
                                               TO  SPA-GMN-GAIKANRIKBN-G
            END-EVALUATE
      *
      *    時間外加算特例乳幼児
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-JIKANTOKU-NYU
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-JIKANTOKU-NYU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(01) TO SPA-GMN-JIKANTOKU-NYU-G
           END-IF
      *
      *    画像診断管理加算
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-GAZOUKSN1
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-GAZOUKSN1-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-GAZOUKSN1-G
           END-IF
      *
      *    中途終了展開時の自動発生
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-CHUTOTENKAI
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-CHUTOTENKAI-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-CHUTOTENKAI-G
           END-IF
      *    訂正時の自動発生
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEIAUTO
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-TEISEIAUTO-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’１’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(02) TO SPA-GMN-TEISEIAUTO-G
           END-IF
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEITOKU
                                       =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST(IDY)
                                           TO SPA-GMN-TEISEITOKU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-DATA-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    初期値は’０’とする
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DATALST(01) TO SPA-GMN-TEISEITOKU-G
           END-IF
      *
      *    包括まとめ
           IF      SPA-GMN-HOUKATU    =   SPACE
               MOVE    "0"            TO  SPA-GMN-HOUKATU
           END-IF
           EVALUATE    SPA-GMN-HOUKATU
                WHEN    "0"
                    MOVE    SPA-GMN-HOUKATU-LIST(1)
                                               TO  SPA-GMN-HOUKATU-G
                WHEN    "1"
                    MOVE    SPA-GMN-HOUKATU-LIST(2)
                                               TO  SPA-GMN-HOUKATU-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HOUKATU-LIST(1)
                                               TO  SPA-GMN-HOUKATU-G
            END-EVALUATE
      *
      *    数量ゼロ入力
           IF      SPA-GMN-SURYOZERO  =   SPACE
               MOVE    "0"            TO  SPA-GMN-SURYOZERO
           END-IF
           EVALUATE    SPA-GMN-SURYOZERO
                WHEN    "0"
                    MOVE    SPA-GMN-SURYOZERO-LIST(1)
                                               TO  SPA-GMN-SURYOZERO-G
                WHEN    "1"
                    MOVE    SPA-GMN-SURYOZERO-LIST(2)
                                               TO  SPA-GMN-SURYOZERO-G
                WHEN    OTHER
                    MOVE    SPA-GMN-SURYOZERO-LIST(1)
                                               TO  SPA-GMN-SURYOZERO-G
            END-EVALUATE
      *
      *    入院調基チェック
           IF      SPA-GMN-CHOKICHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-CHOKICHK
           END-IF
           EVALUATE    SPA-GMN-CHOKICHK
                WHEN    "0"
                    MOVE    SPA-GMN-CHOKICHK-LIST(1)
                                               TO  SPA-GMN-CHOKICHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-CHOKICHK-LIST(2)
                                               TO  SPA-GMN-CHOKICHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-CHOKICHK-LIST(1)
                                               TO  SPA-GMN-CHOKICHK-G
            END-EVALUATE
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-JIKANTOKU-CHK
           END-IF
           EVALUATE    SPA-GMN-JIKANTOKU-CHK
                WHEN    "0"
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(1)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(2)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(1)
                                           TO  SPA-GMN-JIKANTOKU-CHK-G
           END-EVALUATE
      *    クリア確認
           IF      SPA-GMN-CLEARCHK   =   SPACE
               MOVE    "0"            TO  SPA-GMN-CLEARCHK
           END-IF
           EVALUATE    SPA-GMN-CLEARCHK
                WHEN    "0"
                    MOVE    SPA-GMN-CLEARCHK-LIST(1)
                                               TO  SPA-GMN-CLEARCHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-CLEARCHK-LIST(2)
                                               TO  SPA-GMN-CLEARCHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-CLEARCHK-LIST(1)
                                               TO  SPA-GMN-CLEARCHK-G
            END-EVALUATE
      *    薬剤情報提供料チェック
           IF      SPA-GMN-YAKJYOCHK  =   SPACE
               MOVE    "1"            TO  SPA-GMN-YAKJYOCHK
           END-IF
           EVALUATE    SPA-GMN-YAKJYOCHK
                WHEN    "0"
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(1)
                                               TO  SPA-GMN-YAKJYOCHK-G
                WHEN    "1"
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(2)
                                               TO  SPA-GMN-YAKJYOCHK-G
                WHEN    OTHER
                    MOVE    SPA-GMN-YAKJYOCHK-LIST(2)
                                               TO  SPA-GMN-YAKJYOCHK-G
            END-EVALUATE
      *
      *     診療科コードセット
            PERFORM 4102-SRYKACD-CHK-SEC
      *    病名表示区分
           IF     SPA-GMN-BYOHYOKBN   =   SPACE
               MOVE    "1"             TO  SPA-GMN-BYOHYOKBN
           END-IF
           EVALUATE    SPA-GMN-BYOHYOKBN
                WHEN    "0"
                    MOVE    SPA-GMN-BYOHYOKBN-LIST(1)
                                               TO  SPA-GMN-BYOHYOKBN-G
                WHEN    "1"
                    MOVE    SPA-GMN-BYOHYOKBN-LIST(2)
                                               TO  SPA-GMN-BYOHYOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-BYOHYOKBN-LIST(2)
                                               TO  SPA-GMN-BYOHYOKBN-G
           END-EVALUATE
      *    剤登録順区分
           IF     SPA-GMN-ZAIJUNKBN   =   SPACE
               MOVE    "0"             TO  SPA-GMN-ZAIJUNKBN
           END-IF
           EVALUATE    SPA-GMN-ZAIJUNKBN
                WHEN    "0"
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(1)
                                               TO  SPA-GMN-ZAIJUNKBN-G
                WHEN    "1"
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(2)
                                               TO  SPA-GMN-ZAIJUNKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-ZAIJUNKBN-LIST(1)
                                               TO  SPA-GMN-ZAIJUNKBN-G
            END-EVALUATE
      *    検体検査加算自動発生
           EVALUATE    SPA-GMN-KENSAAUTO
                WHEN    "0"
                    MOVE    SPA-GMN-KENSAAUTO-LIST(1)
                                               TO  SPA-GMN-KENSAAUTO-G
                WHEN    "1"
                    MOVE    SPA-GMN-KENSAAUTO-LIST(2)
                                               TO  SPA-GMN-KENSAAUTO-G
                WHEN    OTHER
                    MOVE    SPA-GMN-KENSAAUTO-LIST(1)
                                               TO  SPA-GMN-KENSAAUTO-G
            END-EVALUATE
      *
            MOVE    2                  TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1007-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1017-SEC         SECTION.
      *
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1017"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1017-REC
      *        カルテの病名対象
               MOVE    SYS-1017-KARUTE-BYOMEI
                                       TO  SPA-GMN-BYOMEIKBN
      *        人名辞書使用
               MOVE    SYS-1017-JINMEI-USE
                                       TO  SPA-GMN-JISYOKBN
      *        患者登録のカルテ発行
               MOVE    SYS-1017-KARUTE-HAKKO
                                       TO  SPA-GMN-HAKKOKBN
      *        自費保険の補助区分
               MOVE    SYS-1017-JIHIHKN-TAX
                                       TO  SPA-GMN-HOJYOKBN
      *        同姓同名リスト
               MOVE    SYS-1017-DOUMEI-CHKFLG
                                       TO  SPA-GMN-DOUMEI-CHKFLG
      *        二重登録疑い区分
               MOVE    SYS-1017-JYUFUKU-CHKKBN
                                       TO  SPA-GMN-JYUFUKU-CHKKBN
      *        地方公費単独チェック
               MOVE    SYS-1017-KOHONLY-CHKFLG
                                       TO  SPA-GMN-KOHONLY-CHKFLG
      *        患者情報削除機能
               MOVE    SYS-1017-PTNUMDEL-FLG
                                       TO  SPA-GMN-PTNUMDEL-FLG
           END-IF
      *
      *    初期設定
           EVALUATE    SPA-GMN-BYOMEIKBN
                WHEN    "1"
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(2)
                                               TO  SPA-GMN-BYOMEIKBN-G
                WHEN    "2"
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(3)
                                               TO  SPA-GMN-BYOMEIKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-BYOMEIKBN-LIST(1)
                                               TO  SPA-GMN-BYOMEIKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-JISYOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-JISYOKBN-LIST(2)
                                               TO  SPA-GMN-JISYOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-JISYOKBN-LIST(1)
                                               TO  SPA-GMN-JISYOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-HAKKOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-HAKKOKBN-LIST(2)
                                               TO  SPA-GMN-HAKKOKBN-G
                WHEN    "2"
                    MOVE    SPA-GMN-HAKKOKBN-LIST(3)
                                               TO  SPA-GMN-HAKKOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HAKKOKBN-LIST(1)
                                               TO  SPA-GMN-HAKKOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-HOJYOKBN
                WHEN    "1"
                    MOVE    SPA-GMN-HOJYOKBN-LIST(2)
                                               TO  SPA-GMN-HOJYOKBN-G
                WHEN    OTHER
                    MOVE    SPA-GMN-HOJYOKBN-LIST(1)
                                               TO  SPA-GMN-HOJYOKBN-G
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-DOUMEI-CHKFLG
               WHEN    "1"
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(2)
                                           TO  SPA-GMN-DOUMEI-CHKFLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(1)
                                           TO  SPA-GMN-DOUMEI-CHKFLG-G
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-JYUFUKU-MAX ) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-JYUFUKU-CHKKBN
                                       =   SPA-GMN-JYUFUKU-CHKKBN-LIST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(IDX)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(1)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
           END-IF
      *    地方公費単独チェック
           EVALUATE    SPA-GMN-KOHONLY-CHKFLG
               WHEN    "0"
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(1)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(2)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(2)
                                           TO  SPA-GMN-KOHONLY-CHKFLG-G
           END-EVALUATE
      *    患者情報削除機能
           EVALUATE    SPA-GMN-PTNUMDEL-FLG
               WHEN    "0"
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(1)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(2)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(1)
                                           TO  SPA-GMN-PTNUMDEL-FLG-G
           END-EVALUATE
      *
           MOVE    20                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1017-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1038-SEC         SECTION.
      *
      *    診療行為機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1038"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1038-REC
           ELSE
               INITIALIZE  SYS-1038-REC
           END-IF
      *
           MOVE    SYS-1038-NYKNKNRKBN TO  SPA-GMN-SRYNYKNKNRKBN
           MOVE    SYS-1038-KAGRPFLG   TO  SPA-GMN-KAGRPFLG
           MOVE    SYS-1038-HAKHOUFLG  TO  SPA-GMN-HAKHOUFLG
           MOVE    SYS-1038-DOSRYKBNFLG    TO  SPA-GMN-DOSRYKBNFLG
           MOVE    SYS-1038-WKSRYHYOFLG    TO  SPA-GMN-WKSRYHYOFLG
      *
      *    入金管理区分
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYNYKNKNRKBN-MAX
               IF      SPA-GMN-SRYNYKNKNRKBN
                                       =   SPA-GMN-SRYNYKNKNRKBNL (IDY)
                   MOVE    SPA-GMN-SRYNYKNKNRKBNLST(IDY)
                                       TO  SPA-GMN-SRYNYKNKNRKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SRYNYKNKNRKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYNYKNKNRKBNLST (1)
                                       TO  SPA-GMN-SRYNYKNKNRKBN-G
           END-IF
      *    複数科まとめ集計
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-KAGRPFLG-MAX
               IF      SPA-GMN-KAGRPFLG
                                       =   SPA-GMN-KAGRPFLGL (IDY)
                   MOVE    SPA-GMN-KAGRPFLG-LST (IDY)
                                       TO  SPA-GMN-KAGRPFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-KAGRPFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KAGRPFLG-LST (1)
                                       TO  SPA-GMN-KAGRPFLG-G
           END-IF
      *    請求書発行方法
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG
                                       =   SPA-GMN-HAKHOUFLGL (IDY)
                   MOVE    SPA-GMN-HAKHOUFLG-LST (IDY)
                                       TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-HAKHOUFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-HAKHOUFLG-LST (1)
                                       TO  SPA-GMN-HAKHOUFLG-G
           END-IF
      *    ＤＯ初期表示
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-DOSRYKBNFLG-MAX
               IF      SPA-GMN-DOSRYKBNFLG
                                       =   SPA-GMN-DOSRYKBNFLG-LST (IDY)
                                                             (1:2)
                   MOVE    SPA-GMN-DOSRYKBNFLG-LST (IDY)
                                       TO  SPA-GMN-DOSRYKBNFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-DOSRYKBNFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-DOSRYKBNFLG-LST (1)
                                       TO  SPA-GMN-DOSRYKBNFLG-G
           END-IF
      *    中途表示一覧
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-WKSRYHYOFLG-MAX
               IF      SPA-GMN-WKSRYHYOFLG
                                       =   SPA-GMN-WKSRYHYOFLG-LST (IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-WKSRYHYOFLG-LST (IDY)
                                       TO  SPA-GMN-WKSRYHYOFLG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-WKSRYHYOFLG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-WKSRYHYOFLG-LST (1)
                                       TO  SPA-GMN-WKSRYHYOFLG-G
           END-IF
      *
           MOVE    33                  TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1038-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1039-SEC         SECTION.
      *
      *    収納機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1039"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1039-REC
           ELSE
               INITIALIZE  SYS-1039-REC
           END-IF
      *
           MOVE    SYS-1039-REC        TO  PAT-1039-REC
      *
      *    入金管理区分
           MOVE    SYS-1039-NYKNKNRKBN TO  SPA-GMN-SYUNYKNKNRKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUNYKNKNRKBN-MAX
               IF      SPA-GMN-SYUNYKNKNRKBN
                                       =   SPA-GMN-SYUNYKNKNRKBNL (IDY)
                   MOVE    SPA-GMN-SYUNYKNKNRKBNLST(IDY)
                                       TO  SPA-GMN-SYUNYKNKNRKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUNYKNKNRKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUNYKNKNRKBNLST (1)
                                       TO  SPA-GMN-SYUNYKNKNRKBN-G
           END-IF
      *
      *    請求書兼領収書発行区分（外来）
           MOVE    SYS-1039-SKYPRTGKBN TO  SPA-GMN-SYUSKYPRTG
           PERFORM 3101-SYUSKYPRTG-SYOKI-SEC
      *
      *    診療費明細書発行区分（外来）
           MOVE    SYS-1039-SRYPRTGKBN TO  SPA-GMN-SYUSRYPRTG
           PERFORM 3101-SYUSRYPRTG-SYOKI-SEC
      *
      *    請求書兼領収書発行区分（入院）
           MOVE    SYS-1039-SKYPRTNKBN TO  SPA-GMN-SYUSKYPRTN
           PERFORM 3101-SYUSKYPRTN-SYOKI-SEC
      *
      *    診療費明細書発行区分（入院）
           MOVE    SYS-1039-SRYPRTNKBN TO  SPA-GMN-SYUSRYPRTN
           PERFORM 3101-SYUSRYPRTN-SYOKI-SEC
      *
      *    収納一括入金画面請求書兼領収書発行区分
           MOVE    SYS-1039-S06SKYPRTGKBN
                                       TO  SPA-GMN-S06SKYPRT
           PERFORM 3101-S06SKYPRT-SYOKI-SEC
      *
      *    収納一括入金画面外来分発行方法
           MOVE    SYS-1039-S06SKYSUMKBN
                                       TO  SPA-GMN-S06SKYSUM
           PERFORM 3101-S06SKYSUM-SYOKI-SEC
      *
      *    収納請求一覧画面外来請求書兼領収書発行方法
           PERFORM 3101-S02SKYSAI-SYOKI-SEC
      *
           IF    ( SYS-1039-STIME          IS  NUMERIC )
               MOVE    SYS-1039-STIME      TO  SPA-GMN-SYUSTIME
           ELSE
               MOVE    ZERO                TO  SPA-GMN-SYUSTIME
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   IF    ( SYS-1039-KARIDAY (IDX1 IDX2)    =   "1" )
                       MOVE    "T"         TO  SPA-GMN-SYUKARIDAY
                                                        (IDX1 IDX2)
                   ELSE
                       MOVE    "F"         TO  SPA-GMN-SYUKARIDAY
                                                        (IDX1 IDX2)
                   END-IF
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
      *
                   IF    ( SYS-1039-KARISTTIME (IDX1 IDX2) IS  NUMERIC )
                       MOVE    SYS-1039-KARISTTIME (IDX1 IDX2)
                                           TO  SPA-GMN-SYUKARISTTIME
                                                             (IDX1 IDX2)
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-SYUKARISTTIME
                                                             (IDX1 IDX2)
                   END-IF
      *
                   IF    ( SYS-1039-KARIEDTIME (IDX1 IDX2) IS  NUMERIC )
                       MOVE    SYS-1039-KARIEDTIME (IDX1 IDX2)
                                           TO  SPA-GMN-SYUKARIEDTIME
                                                             (IDX1 IDX2)
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-SYUKARIEDTIME
                                                             (IDX1 IDX2)
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           MOVE    49                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1039-EXT.
           EXIT.
      *****************************************************************
      *    収納請求書兼領収書外来初期値設定処理
      *****************************************************************
       3101-SYUSKYPRTG-SYOKI-SEC       SECTION.
      *
           IF    ( SPA-GMN-SYUSKYPRTG  =   SPACE )
               INITIALIZE                      SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1001-EDYUKYMD
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
                   IF    ( SYS-1001-SKYPRTFLG  =   "1" OR  "2" )
                       MOVE    "1"         TO  SPA-GMN-SYUSKYPRTG
                   ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTG
                   END-IF
               ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTG
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSKYPRTG-MAX
               IF      SPA-GMN-SYUSKYPRTG
                                       =   SPA-GMN-SYUSKYPRTGL (IDY)
                   MOVE    SPA-GMN-SYUSKYPRTGLST(IDY)
                                       TO  SPA-GMN-SYUSKYPRTG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSKYPRTG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTGLST (1)
                                       TO  SPA-GMN-SYUSKYPRTG-G
           END-IF
      *
           . 
       3101-SYUSKYPRTG-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納診療費明細書外来初期値設定処理
      *****************************************************************
       3101-SYUSRYPRTG-SYOKI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSRYPRTG-MAX
               IF      SPA-GMN-SYUSRYPRTG
                                       =   SPA-GMN-SYUSRYPRTGL (IDY)
                   MOVE    SPA-GMN-SYUSRYPRTGLST(IDY)
                                       TO  SPA-GMN-SYUSRYPRTG-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSRYPRTG-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTGLST (1)
                                       TO  SPA-GMN-SYUSRYPRTG-G
           END-IF
      *
           . 
       3101-SYUSRYPRTG-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納請求書兼領収書入院初期値設定処理
      *****************************************************************
       3101-SYUSKYPRTN-SYOKI-SEC       SECTION.
      *
           IF    ( SPA-GMN-SYUSKYPRTN  =   SPACE )
               INITIALIZE                      SYS-5000-REC
               MOVE    "5000"              TO  SYS-5000-KANRICD
               MOVE    "*"                 TO  SYS-5000-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-5000-EDYUKYMD
               MOVE    SYS-5000-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    MCPDATA-REC     TO  SYS-5000-REC
                   IF    ( SYS-5000-SKYPRTKBN  =   "1" OR  "2" )
                       MOVE    "1"         TO  SPA-GMN-SYUSKYPRTN
                   ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTN
                   END-IF
               ELSE
                       MOVE    "0"         TO  SPA-GMN-SYUSKYPRTN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSKYPRTN-MAX
               IF      SPA-GMN-SYUSKYPRTN
                                       =   SPA-GMN-SYUSKYPRTNL (IDY)
                   MOVE    SPA-GMN-SYUSKYPRTNLST(IDY)
                                       TO  SPA-GMN-SYUSKYPRTN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSKYPRTN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSKYPRTNLST (1)
                                       TO  SPA-GMN-SYUSKYPRTN-G
           END-IF
      *
           . 
       3101-SYUSKYPRTN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納診療費明細書入院初期値設定処理
      *****************************************************************
       3101-SYUSRYPRTN-SYOKI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYUSRYPRTN-MAX
               IF      SPA-GMN-SYUSRYPRTN
                                       =   SPA-GMN-SYUSRYPRTNL (IDY)
                   MOVE    SPA-GMN-SYUSRYPRTNLST(IDY)
                                       TO  SPA-GMN-SYUSRYPRTN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYUSRYPRTN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYUSRYPRTNLST (1)
                                       TO  SPA-GMN-SYUSRYPRTN-G
           END-IF
      *
           . 
       3101-SYUSRYPRTN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面請求書兼領収書発行区分初期値設定処理
      *****************************************************************
       3101-S06SKYPRT-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S06SKYPRT-MAX
               IF      SPA-GMN-S06SKYPRT
                                       =   SPA-GMN-S06SKYPRTL (IDY)
                   MOVE    SPA-GMN-S06SKYPRTLST(IDY)
                                       TO  SPA-GMN-S06SKYPRT-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S06SKYPRT-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYPRTLST (1)
                                       TO  SPA-GMN-S06SKYPRT-G
           END-IF
      *
           . 
       3101-S06SKYPRT-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面外来分発行方法初期値設定処理
      *****************************************************************
       3101-S06SKYSUM-SYOKI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S06SKYSUM-MAX
               IF      SPA-GMN-S06SKYSUM
                                       =   SPA-GMN-S06SKYSUML (IDY)
                   MOVE    SPA-GMN-S06SKYSUMLST(IDY)
                                       TO  SPA-GMN-S06SKYSUM-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S06SKYSUM-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S06SKYSUMLST (1)
                                       TO  SPA-GMN-S06SKYSUM-G
           END-IF
      *
           . 
       3101-S06SKYSUM-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    収納一括入金画面外来分発行方法初期値設定処理
      *****************************************************************
       3101-S02SKYSAI-SYOKI-SEC        SECTION.
      *
           EVALUATE    SYS-1039-S02SKYSAIKBN
                                  ALSO PAT-1039-S02SKYMISYUPRTKBN
           WHEN    "0"            ALSO " "
           WHEN    "0"            ALSO "1"
               MOVE    "0"        TO  SPA-GMN-S02SKYSAI
           WHEN    "0"            ALSO "0"
               MOVE    "2"        TO  SPA-GMN-S02SKYSAI
           WHEN    "1"            ALSO " "
           WHEN    "1"            ALSO "1"
               MOVE    "1"        TO  SPA-GMN-S02SKYSAI
           WHEN    "1"            ALSO "0"
               MOVE    "3"        TO  SPA-GMN-S02SKYSAI
           WHEN    OTHER
               MOVE    "0"        TO  SPA-GMN-S02SKYSAI
           END-EVALUATE
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-S02SKYSAI-MAX
               IF      SPA-GMN-S02SKYSAI
                                       =   SPA-GMN-S02SKYSAIL (IDY)
                   MOVE    SPA-GMN-S02SKYSAILST(IDY)
                                       TO  SPA-GMN-S02SKYSAI-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-S02SKYSAI-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-S02SKYSAILST (1)
                                       TO  SPA-GMN-S02SKYSAI-G
           END-IF
      *
           . 
       3101-S02SKYSAI-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-1042-SEC         SECTION.
      *
      *    病名機能情報
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1042"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1042-REC
           ELSE
               INITIALIZE  SYS-1042-REC
           END-IF
      *
      *    主病名確認メッセージ表示有無
           MOVE    SYS-1042-MSG1-DSPKBN
                                       TO  SPA-GMN-MSG1-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG1-DSPKBN-MAX
               IF      SPA-GMN-MSG1-DSPKBN
                                       =   SPA-GMN-MSG1-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG1-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG1-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG1-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG1-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG1-DSPKBN-G
           END-IF
      *
      *    移行病名警告メッセージ表示有無
           MOVE    SYS-1042-MSG2-DSPKBN
                                       TO  SPA-GMN-MSG2-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG2-DSPKBN-MAX
               IF      SPA-GMN-MSG2-DSPKBN
                                       =   SPA-GMN-MSG2-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG2-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG2-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG2-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG2-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG2-DSPKBN-G
           END-IF
      *
      *    廃止年月日警告メッセージ表示有無
           MOVE    SYS-1042-MSG3-DSPKBN
                                       TO  SPA-GMN-MSG3-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG3-DSPKBN-MAX
               IF      SPA-GMN-MSG3-DSPKBN
                                       =   SPA-GMN-MSG3-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG3-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG3-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG3-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG3-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG3-DSPKBN-G
           END-IF
      *
      *    同一病名警告メッセージ表示有無
           MOVE    SYS-1042-MSG4-DSPKBN
                                       TO  SPA-GMN-MSG4-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-MSG4-DSPKBN-MAX
               IF      SPA-GMN-MSG4-DSPKBN
                                       =   SPA-GMN-MSG4-DSPKBNL (IDY)
                   MOVE    SPA-GMN-MSG4-DSPKBNLST(IDY)
                                       TO  SPA-GMN-MSG4-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-MSG4-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-MSG4-DSPKBNLST (1)
                                       TO  SPA-GMN-MSG4-DSPKBN-G
           END-IF
      *
      *    診療科初期表示区分
           MOVE    SYS-1042-SRYKA-DSPKBN
                                       TO  SPA-GMN-SRYKA-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKA-DSPKBN-MAX
               IF      SPA-GMN-SRYKA-DSPKBN
                                       =   SPA-GMN-SRYKA-DSPKBNL (IDY)
                   MOVE    SPA-GMN-SRYKA-DSPKBNLST(IDY)
                                       TO  SPA-GMN-SRYKA-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SRYKA-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKA-DSPKBNLST (1)
                                           TO  SPA-GMN-SRYKA-DSPKBN-G
           END-IF
      *
      *    表示対象期間初期表示区分
           MOVE    SYS-1042-SYOKBN-DSPKBN
                                       TO  SPA-GMN-SYOKBN-DSPKBN
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SYOKBN-DSPKBN-MAX
               IF      SPA-GMN-SYOKBN-DSPKBN
                                       =   SPA-GMN-SYOKBN-DSPKBNL (IDY)
                   MOVE    SPA-GMN-SYOKBN-DSPKBNLST(IDY)
                                       TO  SPA-GMN-SYOKBN-DSPKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SYOKBN-DSPKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SYOKBN-DSPKBNLST (1)
                                       TO  SPA-GMN-SYOKBN-DSPKBN-G
           END-IF
      *
      *    コメント症状詳記区分
           MOVE    SYS-1042-SJKBN-SET  TO  SPA-GMN-SJKBN
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SJKBN-MAX
               IF      SPA-GMN-SJKBN   =   SPA-GMN-SJKBNLST (IDY)(1:2)
                   MOVE    SPA-GMN-SJKBNLST(IDY)
                                       TO  SPA-GMN-SJKBN-G
                   MOVE    1           TO  FLG-OK
                   MOVE    SPA-GMN-SJKBN-MAX
                                       TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SJKBNLST (1)
                                       TO  SPA-GMN-SJKBN-G
           END-IF
      *
           MOVE    70                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1042-EXT.
           EXIT.
      *****************************************************************
      *    会計照会スパ初期編集処理
      *****************************************************************
       310-SPASET-1043-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1043"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1043-REC
           ELSE
               INITIALIZE  SYS-1043-REC
           END-IF
      *
      *    全科の表示順区分
           MOVE    SYS-1043-SRYKBN-FLG
                                       TO  SPA-GMN-SRYKBN-FLG
      *    診療科の初期表示
           MOVE    SYS-1043-SRYKA-FLG
                                       TO  SPA-GMN-SRYKA-FLG
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKBN-MAX
               IF      SPA-GMN-SRYKBN-FLG
                                       =   SPA-GMN-SRYKBN-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKBN-LIST(IDY)
                                           TO  SPA-GMN-SRYKBN-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-SRYKBN-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKBN-LIST (1)
                                       TO  SPA-GMN-SRYKBN-FLG-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA-FLG
                                       =   SPA-GMN-SRYKA-LIST(IDY)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKA-LIST(IDY)
                                           TO  SPA-GMN-SRYKA-FLG-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-GMN-SRYKA-MAX
                                           TO  IDY
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-SRYKA-LIST (1)
                                       TO  SPA-GMN-SRYKA-FLG-G
           END-IF
      *
           MOVE    100                 TO  SPA-GMN-CUR
      *
           . 
       310-SPASET-1043-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY1"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY2"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY3"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY4"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY5"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY6"
               WHEN    "CLICKED"       ALSO    "SYUKARI1DAY7"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY1"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY2"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY3"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY4"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY5"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY6"
               WHEN    "CLICKED"       ALSO    "SYUKARI2DAY7"
                   MOVE    MCP-WIDGET (8:1)
                                       TO  WRK-KARIDAY
                   PERFORM 4110-SYUKARIDAY-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
      *    自動算定情報
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   7
      *        MOVE    W07-DATA (IDX)      TO  SPA-GMN-DATA-G (IDX)
      *    END-PERFORM
           MOVE    W07-DATA01          TO  SPA-GMN-DATA-G (01)
           MOVE    W07-DATA02          TO  SPA-GMN-DATA-G (02)
           MOVE    W07-DATA03          TO  SPA-GMN-DATA-G (03)
           MOVE    W07-DATA04          TO  SPA-GMN-DATA-G (04)
           MOVE    W07-DATA05          TO  SPA-GMN-DATA-G (05)
           MOVE    W07-DATA06          TO  SPA-GMN-DATA-G (06)
           MOVE    W07-DATA07          TO  SPA-GMN-DATA-G (07)
      *    入院初診料自動算定
           MOVE    W07-NYUSYOSIN       TO  SPA-GMN-NYUSYOSIN-G
      *    時間外加算特例乳幼児
           MOVE    W07-JIKANTOKUNYU    TO  SPA-GMN-JIKANTOKU-NYU-G
           MOVE    W07-GAZOUKSN1       TO  SPA-GMN-GAZOUKSN1-G
      *    中途終了展開時の自動発生
           MOVE    W07-CHUTOTENKAI     TO  SPA-GMN-CHUTOTENKAI-G
      *    訂正時の自動発生
           MOVE    W07-TEISEI          TO  SPA-GMN-TEISEIAUTO-G
      *    訂正時の自動発生
           MOVE    W07-TEISEITOKU      TO  SPA-GMN-TEISEITOKU-G
      *
           MOVE    W07-LASTKIKAN       TO  SPA-GMN-LAST-KIKAN
      *
           MOVE    W07-SRYKACD         TO  SPA-GMN-SRYKACD
           MOVE    W07-SRYKAMEI        TO  SPA-GMN-SRYKAMEI
      *    時間外加算乳幼児チェック
           MOVE    W07-JIKANTOKU-CHK   TO  SPA-GMN-JIKANTOKU-CHK-G
      *    チェック除外時間
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    W07-JIKANW1-HH(IDX) TO  SPA-GMN-JIKANWST-HH (IDX)
               MOVE    W07-JIKANW1-MM(IDX) TO  SPA-GMN-JIKANWST-MM (IDX)
               MOVE    W07-JIKANW2-HH(IDX) TO  SPA-GMN-JIKANWED-HH (IDX)
               MOVE    W07-JIKANW2-MM(IDX) TO  SPA-GMN-JIKANWED-MM (IDX)
      *
               MOVE    W07-JIKANS1-HH(IDX) TO  SPA-GMN-JIKANSST-HH (IDX)
               MOVE    W07-JIKANS1-MM(IDX) TO  SPA-GMN-JIKANSST-MM (IDX)
               MOVE    W07-JIKANS2-HH(IDX) TO  SPA-GMN-JIKANSED-HH (IDX)
               MOVE    W07-JIKANS2-MM(IDX) TO  SPA-GMN-JIKANSED-MM (IDX)
           END-PERFORM
      *
      *    チェック制御
           MOVE    W07-KIKAN           TO  SPA-GMN-INTERACT-KIKAN
           MOVE    W07-GAIKANKBN       TO  SPA-GMN-GAIKANRIKBN-G
           MOVE    W07-GAIMOVE         TO  SPA-GMN-GAIMOVE-G
           MOVE    W07-SURYOZERO       TO  SPA-GMN-SURYOZERO-G
           MOVE    W07-CHOKICHK        TO  SPA-GMN-CHOKICHK-G
           MOVE    W07-CLEARCHK        TO  SPA-GMN-CLEARCHK-G
           MOVE    W07-YAKJYOCHK       TO  SPA-GMN-YAKJYOCHK-G
      *
           MOVE    W07-HOUKATU         TO  SPA-GMN-HOUKATU-G
           MOVE    W07-BYOHYOKBN       TO  SPA-GMN-BYOHYOKBN-G
           MOVE    W07-ZAIJUNKBN       TO  SPA-GMN-ZAIJUNKBN-G
           MOVE    W07-KENSAAUTO       TO  SPA-GMN-KENSAAUTO-G
      *
      *    患者登録情報
           MOVE    W07-BYOMEIKBN       TO  SPA-GMN-BYOMEIKBN-G
           MOVE    W07-JISYOKBN        TO  SPA-GMN-JISYOKBN-G
           MOVE    W07-HAKKOKBN        TO  SPA-GMN-HAKKOKBN-G
           MOVE    W07-HOJYOKBN        TO  SPA-GMN-HOJYOKBN-G
           MOVE    W07-DOUMEI-CHKFLG   TO  SPA-GMN-DOUMEI-CHKFLG-G
           MOVE    W07-JYUFUKU-CHKKBN  TO  SPA-GMN-JYUFUKU-CHKKBN-G
           MOVE    W07-KOHONLY-CHKFLG  TO  SPA-GMN-KOHONLY-CHKFLG-G
           MOVE    W07-PTNUMDEL-FLG    TO  SPA-GMN-PTNUMDEL-FLG-G
      *
      *    診療行為機能情報
           MOVE    W07-SRYNYKNKNRKBN   TO  SPA-GMN-SRYNYKNKNRKBN-G
           MOVE    W07-KAGRPFLG        TO  SPA-GMN-KAGRPFLG-G
           MOVE    W07-HAKHOUFLG       TO  SPA-GMN-HAKHOUFLG-G
           MOVE    W07-DOSRYKBN-FLG    TO  SPA-GMN-DOSRYKBNFLG-G
           MOVE    W07-WKSRYHYO-FLG    TO  SPA-GMN-WKSRYHYOFLG-G
      *
      *    収納機能情報
           MOVE    W07-SYUNYKNKNRKBN   TO  SPA-GMN-SYUNYKNKNRKBN-G
           MOVE    W07-SYUSKYPRTG      TO  SPA-GMN-SYUSKYPRTG-G
           MOVE    W07-SYUSRYPRTG      TO  SPA-GMN-SYUSRYPRTG-G
           MOVE    W07-SYUSKYPRTN      TO  SPA-GMN-SYUSKYPRTN-G
           MOVE    W07-SYUSRYPRTN      TO  SPA-GMN-SYUSRYPRTN-G
           MOVE    W07-S06SKYPRT       TO  SPA-GMN-S06SKYPRT-G
           MOVE    W07-S06SKYSUM       TO  SPA-GMN-S06SKYSUM-G
           MOVE    W07-S02SKYSAI       TO  SPA-GMN-S02SKYSAI-G
           MOVE    W07-SYUSTIME-HH     TO  SPA-GMN-SYUSTIME-HH
           MOVE    W07-SYUSTIME-MM     TO  SPA-GMN-SYUSTIME-MM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                   MOVE    W07-SYUKARISTTIME (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARISTTIME
                                                       (IDX1 IDX2)
                   MOVE    W07-SYUKARIEDTIME (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARIEDTIME
                                                       (IDX1 IDX2)
      *
               END-PERFORM
           END-PERFORM
      *
      *    病名機能情報
           MOVE    W07-MSG1-DSPKBN     TO  SPA-GMN-MSG1-DSPKBN-G
           MOVE    W07-MSG2-DSPKBN     TO  SPA-GMN-MSG2-DSPKBN-G
           MOVE    W07-MSG3-DSPKBN     TO  SPA-GMN-MSG3-DSPKBN-G
           MOVE    W07-MSG4-DSPKBN     TO  SPA-GMN-MSG4-DSPKBN-G
           MOVE    W07-SRYKA-DSPKBN    TO  SPA-GMN-SRYKA-DSPKBN-G
           MOVE    W07-SYOKBN-DSPKBN   TO  SPA-GMN-SYOKBN-DSPKBN-G
           MOVE    W07-SJKBN           TO  SPA-GMN-SJKBN-G
      *
      *    会計照会機能情報 
           MOVE    W07-SRYKBN-FLG      TO  SPA-GMN-SRYKBN-FLG-G
           MOVE    W07-SRYKA-FLG       TO  SPA-GMN-SRYKA-FLG-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
      *            調剤技術基本料チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-DATA-CHK-SEC
                   END-IF
      *            診療科チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4102-SRYKACD-CHK-SEC
                   END-IF
      *            入院初診料自動算定チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-NYUSYOSIN-CHK-SEC
                   END-IF
      *            時間外加算特例乳幼児
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANTOKU-CHK-SEC
                   END-IF
      *            画像診断管理加算１
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-GAZOUKSN1-CHK-SEC
                   END-IF
      *            相互作用チェック期間
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4100-KIKAN-CHK2-SEC
                   END-IF
      *            中途終了展開時の自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CHUTOTENKAI-CHK-SEC
                   END-IF
      *            訂正時の自動発生
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEISEIAUTO-CHK-SEC
                   END-IF
      *            訂正時の自動発生（特定疾患処方管理）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-TEISEITOKU-CHK-SEC
                   END-IF
      *            検体検査加算自動発生）
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-KENSAAUTO-CHK-SEC
                   END-IF
      *            外来管理加算チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4101-GAIKANRIKBN-CHK2-SEC
                   END-IF
      *            外来カーソル移動チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-GAIMOVE-CHK-SEC
                   END-IF
      *            数量ゼロチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-SURYOZERO-CHK-SEC
                   END-IF
      *            入院調基チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CHOKICHK-CHK-SEC
                   END-IF
      *            時間外加算乳幼児チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANTOKU-CHK-CHK-SEC
                   END-IF
      *            チェック除外時間
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-JIKANCHK-TIME-SEC
                   END-IF
      *            クリア確認チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-CLERACHK-CHK-SEC
                   END-IF
      *            薬剤情報提供料チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-YAKJYOCHK-CHK-SEC
                   END-IF
      *            包括入力チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-HOUKATU-CHK-SEC
                   END-IF
      *            病名表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-BYOHYOKBN-CHK-SEC
                   END-IF
      *            剤登録順区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4103-ZAIJUNKBN-CHK-SEC
                   END-IF
               WHEN    "1017"
      *            カルテの病名対象チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4104-BYOMEI-CHK-SEC
                   END-IF
      *            人名辞書使用チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4105-JISYO-CHK-SEC
                   END-IF
      *            患者登録のカルテ発行チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4106-HAKKO-CHK-SEC
                   END-IF
      *            自費保険の補助区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-HOJYO-CHK-SEC
                   END-IF
      *            同姓同名リスト区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-DOUMEI-CHK-SEC
                   END-IF
      *            二重登録疑い区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-JYUFUKU-CHK-SEC
                   END-IF
      *            地方公費単独チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-KOHONLY-CHKFLG-SEC
                   END-IF
      *            患者情報削除機能
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4107-PTNUMDEL-FLG-SEC
                   END-IF
               WHEN    "1038"
      *            ＤＯ初期表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-DOSRYKBNFLG-CHK-SEC
                   END-IF
      *            中途表示一覧
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-WKSRYHYOFLG-CHK-SEC
                   END-IF
      *            入金管理区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-SRYNYKNKNRKBN-CHK-SEC
                   END-IF
      *            複数科まとめチェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-KAGRPFLG-CHK-SEC
                   END-IF
      *            請求書発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4108-HAKHOUFLG-CHK-SEC
                   END-IF
               WHEN    "1039"
      *            請求書兼領収書発行区分（外来）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSKYPRTG-CHK-SEC
                   END-IF
      *            診療費明細書発行区分（外来）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSRYPRTG-CHK-SEC
                   END-IF
      *            請求書兼領収書発行区分（入院）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSKYPRTN-CHK-SEC
                   END-IF
      *            診療費明細書発行区分（入院）チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSRYPRTN-CHK-SEC
                   END-IF
      *            入金管理区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-SYUNYKNKNRKBN-CHK-SEC
                   END-IF
      *            一括入金画面請求書兼領収書発行区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S06SKYPRT-CHK-SEC
                   END-IF
      *            一括入金画面外来分発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S06SKYSUM-CHK-SEC
                   END-IF
      *            請求一覧画面外来分再発行方法チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4109-S02SKYSAI-CHK-SEC
                   END-IF
      *            登録時刻チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUSTIME-CHK-SEC
                   END-IF
      *            仮締め時間帯チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4110-SYUKARITIME-CHK-SEC
                   END-IF
               WHEN    "1042"
      *            主病名確認メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG1-DSPKBN-CHK-SEC
                   END-IF
      *            移行病名警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG2-DSPKBN-CHK-SEC
                   END-IF
      *            廃止年月日警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG3-DSPKBN-CHK-SEC
                   END-IF
      *            同一病名警告メッセージ表示有無チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-MSG4-DSPKBN-CHK-SEC
                   END-IF
      *            診療科初期表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SRYKA-DSPKBN-CHK-SEC
                   END-IF
      *            表示対象期間初期表示区分チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SYOKBN-DSPKBN-CHK-SEC
                   END-IF
      *            症状詳記区分初期表示チェック
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4111-SJKBN-CHK-SEC
                   END-IF
               WHEN    "1043"
      *            会計照会全科表示順
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-SRYKBN-FLG-CHK-SEC
                   END-IF
      *            診療科の初期表示
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 4311-SRYKA-FLG-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤技術基本料チェック
      *****************************************************************
       4100-DATA-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   7   )
                   OR         (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-OK
      *
               IF      IDX                 =   2
                                           OR  3
                                           OR  4
      *            薬剤情報提供料
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-GMN-DATA-MAX2
                       IF      SPA-GMN-DATA (IDX)
                                               =   SPA-GMN-DATAL2(IDY)
                           MOVE    SPA-GMN-DATALST2 (IDY)
                                               TO  SPA-GMN-DATA-G(IDX)
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-GMN-DATA-MAX
                       IF      SPA-GMN-DATA (IDX)
                                               =   SPA-GMN-DATAL(IDY)
                           MOVE    SPA-GMN-DATALST (IDY)
                                               TO  SPA-GMN-DATA-G(IDX)
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      FLG-OK             =   ZERO
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    "0001"             TO SPA-ERRCD
                       WHEN    2
                           MOVE    "0002"             TO SPA-ERRCD
                       WHEN    3
                           MOVE    "0003"             TO SPA-ERRCD
                       WHEN    4
                           MOVE    "0004"             TO SPA-ERRCD
                       WHEN    5
                           MOVE    "0008"             TO SPA-ERRCD
                       WHEN    6
                           MOVE    "0009"             TO SPA-ERRCD
                       WHEN    7
                           MOVE    "0010"             TO SPA-ERRCD
                   END-EVALUATE
                   MOVE    IDX             TO  SPA-GMN-CUR
                   ADD     1               TO  SPA-GMN-CUR
               END-IF
           END-PERFORM
           .
       4100-DATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    相互作用チェック期間チェック処理
      *****************************************************************
       4100-KIKAN-CHK2-SEC       SECTION.
      *
          .
       4100-KIKAN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院初診料自動算定チェック処理
      *****************************************************************
       4103-NYUSYOSIN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-NYUSYOSIN   =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-NYUSYOSIN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-NYUSYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像診断管理加算１チェック処理
      *****************************************************************
       4103-GAZOUKSN1-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-GAZOUKSN1   =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-GAZOUKSN1-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-GAZOUKSN1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途終了展開時の自動発生チェック処理
      *****************************************************************
       4103-CHUTOTENKAI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-CHUTOTENKAI  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-CHUTOTENKAI-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    68                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-CHUTOTENKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の自動発生チェック処理
      *****************************************************************
       4103-TEISEIAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEIAUTO  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-TEISEIAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0080"              TO  SPA-ERRCD
               MOVE    80                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-TEISEIAUTO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の自動発生（特定疾患処方管理）チェック処理
      *****************************************************************
       4103-TEISEITOKU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-TEISEITOKU  =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-TEISEITOKU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0081"              TO  SPA-ERRCD
               MOVE    81                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-TEISEITOKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検体検査加算自動発生チェック処理
      *****************************************************************
       4103-KENSAAUTO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   2
               IF      SPA-GMN-KENSAAUTO  =    SPA-GMN-KENSAAUTO-LIST
                                                         (IDY)(1:1)
                   MOVE    SPA-GMN-KENSAAUTO-LIST(IDY)
                                           TO  SPA-GMN-KENSAAUTO-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0082"              TO  SPA-ERRCD
               MOVE    84                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-KENSAAUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算特例乳幼児算定チェック処理
      *****************************************************************
       4103-JIKANTOKU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-DATA-MAX
               IF      SPA-GMN-JIKANTOKU-NYU   =   SPA-GMN-DATAL(IDY)
                   MOVE    SPA-GMN-DATALST (IDY)
                                           TO  SPA-GMN-JIKANTOKU-NYU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
      *
          .
       4103-JIKANTOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算チェック処理
      *****************************************************************
       4101-GAIKANRIKBN-CHK2-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-GAIKANRIKBN-CHK
                                      =   SPA-GMN-GAIKANRIKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-GAIKANRIKBN-LIST(IDX)
                                          TO  SPA-GMN-GAIKANRIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    11              TO  SPA-GMN-CUR
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
      *
          .
       4101-GAIKANRIKBN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定科チェック処理
      *****************************************************************
       4102-SRYKACD-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-DATA (7)  =   "1"
               IF      ( SPA-GMN-SRYKACD   =   "0" )
                   OR  ( SPA-GMN-SRYKACD   =   "00" )
                   OR  ( SPA-GMN-SRYKACD   =   SPACE )
                   MOVE    "00"            TO  SPA-GMN-SRYKACD
                   MOVE    "全科"          TO  SPA-GMN-SRYKAMEI
               ELSE
      *            診療科名
                   MOVE    SPACE           TO  SYSKANRI-REC 
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    SPA-GMN-SRYKACD TO  SYS-KBNCD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-EDYUKYMD
                   MOVE    SYSKANRI-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI            =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1005-REC
                       MOVE    SYS-1005-SRYKANAME1
                                               TO  SPA-GMN-SRYKAMEI
                   ELSE
                       MOVE    9               TO  SPA-GMN-CUR
                       MOVE    SPACE           TO  SPA-GMN-SRYKAMEI
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-SRYKACD
               MOVE    SPACE           TO  SPA-GMN-SRYKAMEI
           END-IF
           .
       4102-SRYKACD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来カーソル移動チェック処理
      *****************************************************************
       4103-GAIMOVE-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-GAIMOVE
                                      =   SPA-GMN-GAIMOVE-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-GAIMOVE-LIST(IDX)
                                          TO  SPA-GMN-GAIMOVE-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    12              TO  SPA-GMN-CUR
               MOVE    "0012"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-GAIMOVE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量ゼロ入力チェック処理
      *****************************************************************
       4103-SURYOZERO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-SURYOZERO
                                      =   SPA-GMN-SURYOZERO-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-SURYOZERO-LIST(IDX)
                                          TO  SPA-GMN-SURYOZERO-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    14              TO  SPA-GMN-CUR
               MOVE    "0015"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-SURYOZERO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院調基チェック入力チェック処理
      *****************************************************************
       4103-CHOKICHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-CHOKICHK
                                      =   SPA-GMN-CHOKICHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-CHOKICHK-LIST(IDX)
                                          TO  SPA-GMN-CHOKICHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    15              TO  SPA-GMN-CUR
               MOVE    "0016"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-CHOKICHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア確認チェック処理
      *****************************************************************
       4103-CLERACHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-CLEARCHK
                                      =   SPA-GMN-CLEARCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-CLEARCHK-LIST(IDX)
                                          TO  SPA-GMN-CLEARCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    67              TO  SPA-GMN-CUR
               MOVE    "0034"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-CLERACHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報提供料チェック処理
      *****************************************************************
       4103-YAKJYOCHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-YAKJYOCHK
                                      =   SPA-GMN-YAKJYOCHK-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-YAKJYOCHK-LIST(IDX)
                                          TO  SPA-GMN-YAKJYOCHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    69              TO  SPA-GMN-CUR
               MOVE    "0037"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-YAKJYOCHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外加算乳幼児入力チェック処理
      *****************************************************************
       4103-JIKANTOKU-CHK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JIKANTOKU-CHK
                                      =   SPA-GMN-JIKANTOKU-CHK-LIST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-JIKANTOKU-CHK-LIST(IDX)
                                          TO  SPA-GMN-JIKANTOKU-CHK-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    50              TO  SPA-GMN-CUR
               MOVE    "0030"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-JIKANTOKU-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック除外時間チェック処理
      *****************************************************************
       4103-JIKANCHK-TIME-SEC          SECTION.
      *
      *    時間外チェックなしの時、ゼロとする
           IF      SPA-GMN-JIKANTOKU-CHK   NOT =   "1"
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-WEEK(1)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-WEEK(2)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-SAT (1)
               MOVE    SPACE           TO  SPA-GMN-JIKANCHK-SAT (2)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-WEEK(1)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-WEEK(2)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-SAT (1)
               MOVE    ZERO            TO  SPA-NAI-JIKANCHK-SAT (2)
           ELSE
      *        時間判定
               PERFORM 41031-JIKANCHK-TIMECHK-SEC
           END-IF
      *
          .
       4103-JIKANCHK-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間判定処理
      *****************************************************************
       41031-JIKANCHK-TIMECHK-SEC       SECTION.
      *
      *    時間判定
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWST-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWST-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWST-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    52              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWST-MM
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWED-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    53              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWED-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANWED-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    54              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANWED-MM
                                                                (IDT)
               END-IF
      *
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSST-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    59              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSST-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSST-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    60              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSST-MM
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSED-HH(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    61              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSED-HH
                                                                (IDT)
               END-IF
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JIKANSED-MM(IDT)    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    "0031"          TO  SPA-ERRCD
                       MOVE    62              TO  SPA-GMN-CUR
                       COMPUTE SPA-GMN-CUR     =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    SNUM-OUTNUM         TO  SPA-NAI-JIKANSED-MM
                                                                (IDT)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPA-NAI-JIKANWST-HH(IDT)
                                      TO  SPA-GMN-JIKANWST-HH(IDT)
               MOVE    SPA-NAI-JIKANWST-MM(IDT)
                                      TO  SPA-GMN-JIKANWST-MM(IDT)
               MOVE    SPA-NAI-JIKANWED-HH(IDT)
                                      TO  SPA-GMN-JIKANWED-HH(IDT)
               MOVE    SPA-NAI-JIKANWED-MM(IDT)
                                      TO  SPA-GMN-JIKANWED-MM(IDT)
               MOVE    SPA-NAI-JIKANSST-HH(IDT)
                                      TO  SPA-GMN-JIKANSST-HH(IDT)
               MOVE    SPA-NAI-JIKANSST-MM(IDT)
                                      TO  SPA-GMN-JIKANSST-MM(IDT)
               MOVE    SPA-NAI-JIKANSED-HH(IDT)
                                      TO  SPA-GMN-JIKANSED-HH(IDT)
               MOVE    SPA-NAI-JIKANSED-MM(IDT)
                                      TO  SPA-GMN-JIKANSED-MM(IDT)
           END-PERFORM
      *
      *    時間判定
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-NAI-JIKANWST-HH(IDT)    >   23
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWST-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    52                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWED-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    53                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANWED-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    54                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSST-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSST-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    60                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSED-HH(IDT)    >   23
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    61                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JIKANSED-MM(IDT)    >   59
                 AND  (SPA-ERRCD           =   SPACE )
                   MOVE    "0031"              TO  SPA-ERRCD
                   MOVE    62                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-NAI-JIKANCHK-WED(IDT)   >   ZERO  ) AND
                      (SPA-NAI-JIKANCHK-WST(IDT)
                                           >  SPA-NAI-JIKANCHK-WED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF     (SPA-NAI-JIKANCHK-SED(IDT)   >   ZERO  )  AND
                      (SPA-NAI-JIKANCHK-SST(IDT)
                                           >  SPA-NAI-JIKANCHK-SED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
          .
       41031-JIKANCHK-TIMECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括まとめ入力チェック処理
      *****************************************************************
       4103-HOUKATU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HOUKATU
                                      =   SPA-GMN-HOUKATU-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HOUKATU-LIST(IDX)
                                          TO  SPA-GMN-HOUKATU-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    16              TO  SPA-GMN-CUR
               MOVE    "0014"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-HOUKATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名表示区分入力チェック処理
      *****************************************************************
       4103-BYOHYOKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-BYOHYOKBN
                                      =   SPA-GMN-BYOHYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-BYOHYOKBN-LIST(IDX)
                                          TO  SPA-GMN-BYOHYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    82              TO  SPA-GMN-CUR
               MOVE    "0038"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-BYOHYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤登録順区分チェック処理
      *****************************************************************
       4103-ZAIJUNKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-ZAIJUNKBN
                                      =   SPA-GMN-ZAIJUNKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-ZAIJUNKBN-LIST(IDX)
                                          TO  SPA-GMN-ZAIJUNKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    83              TO  SPA-GMN-CUR
               MOVE    "0039"          TO  SPA-ERRCD
           END-IF
      *
          .
       4103-ZAIJUNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテの病名対象チェック処理
      *****************************************************************
       4104-BYOMEI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-BYOMEIKBN
                                      =   SPA-GMN-BYOMEIKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-BYOMEIKBN-LIST(IDX)
                                          TO  SPA-GMN-BYOMEIKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    20              TO  SPA-GMN-CUR
               MOVE    "1001"          TO  SPA-ERRCD
           END-IF
      *
          .
       4104-BYOMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    人名辞書使用チェック処理
      *****************************************************************
       4105-JISYO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JISYOKBN
                                      =   SPA-GMN-JISYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-JISYOKBN-LIST(IDX)
                                          TO  SPA-GMN-JISYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    21              TO  SPA-GMN-CUR
               MOVE    "1002"          TO  SPA-ERRCD
           END-IF
      *
          .
       4105-JISYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録のカルテ発行チェック処理
      *****************************************************************
       4106-HAKKO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-HAKKOKBN
                                      =   SPA-GMN-HAKKOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HAKKOKBN-LIST(IDX)
                                          TO  SPA-GMN-HAKKOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    22              TO  SPA-GMN-CUR
               MOVE    "1003"          TO  SPA-ERRCD
           END-IF
      *
          .
       4106-HAKKO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費保険の補助区分チェック処理
      *****************************************************************
       4107-HOJYO-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-HOJYOKBN
                                       =   SPA-GMN-HOJYOKBN-LIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HOJYOKBN-LIST(IDX)
                                          TO  SPA-GMN-HOJYOKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    23              TO  SPA-GMN-CUR
               MOVE    "1004"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-HOJYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同姓同名リスト区分チェック処理
      *****************************************************************
       4107-DOUMEI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-DOUMEI-CHKFLG
                                       =   SPA-GMN-DOUMEI-CHKFLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST(IDX)
                                          TO  SPA-GMN-DOUMEI-CHKFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    24              TO  SPA-GMN-CUR
               MOVE    "1005"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-DOUMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同姓同名リスト区分チェック処理
      *****************************************************************
       4107-JYUFUKU-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-JYUFUKU-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-JYUFUKU-CHKKBN
                                       =   SPA-GMN-JYUFUKU-CHKKBN-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST(IDX)
                                          TO  SPA-GMN-JYUFUKU-CHKKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    25              TO  SPA-GMN-CUR
               MOVE    "1006"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地方公費単独チェック処理
      *****************************************************************
       4107-KOHONLY-CHKFLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-KOHONLY-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-KOHONLY-CHKFLG
                                       =   SPA-GMN-KOHONLY-CHKFLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST(IDX)
                                          TO  SPA-GMN-KOHONLY-CHKFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    26              TO  SPA-GMN-CUR
               MOVE    "1007"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-KOHONLY-CHKFLG-EXT.
           EXIT.
      *****************************************************************
      *    地方公費単独チェック処理
      *****************************************************************
       4107-PTNUMDEL-FLG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-PTNUMDEL-MAX) OR
                              (FLG-OK  =   1    )
               IF      SPA-GMN-PTNUMDEL-FLG
                                       =   SPA-GMN-PTNUMDEL-FLG-LIST
                                                          (IDX)(1:1)
                   MOVE    SPA-GMN-PTNUMDEL-FLG-LIST(IDX)
                                          TO  SPA-GMN-PTNUMDEL-FLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    27              TO  SPA-GMN-CUR
               MOVE    "1008"          TO  SPA-ERRCD
           END-IF
      *
          .
       4107-PTNUMDEL-FLG-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分チェック処理（診療行為機能情報）
      *****************************************************************
       4108-SRYNYKNKNRKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYNYKNKNRKBN-MAX
               IF      SPA-GMN-SRYNYKNKNRKBN
                                       =   SPA-GMN-SRYNYKNKNRKBNL (IDX)
                   MOVE    SPA-GMN-SRYNYKNKNRKBNLST(IDX)
                                          TO  SPA-GMN-SRYNYKNKNRKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    30              TO  SPA-GMN-CUR
               MOVE    "2001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-SRYNYKNKNRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行方法チェック処理（診療行為機能情報）
      *****************************************************************
       4108-KAGRPFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KAGRPFLG-MAX
               IF      SPA-GMN-KAGRPFLG
                                       =   SPA-GMN-KAGRPFLGL (IDX)
                   MOVE    SPA-GMN-KAGRPFLG-LST(IDX)
                                          TO  SPA-GMN-KAGRPFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    31              TO  SPA-GMN-CUR
               MOVE    "2002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-KAGRPFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書発行方法チェック処理（診療行為機能情報）
      *****************************************************************
       4108-HAKHOUFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG
                                       =   SPA-GMN-HAKHOUFLGL (IDX)
                   MOVE    SPA-GMN-HAKHOUFLG-LST(IDX)
                                          TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    32              TO  SPA-GMN-CUR
               MOVE    "2003"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-HAKHOUFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＤＯ初期表示チェック処理（診療行為機能情報）
      *****************************************************************
       4108-DOSRYKBNFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-DOSRYKBNFLG-MAX
               IF      SPA-GMN-DOSRYKBNFLG
                                       =   SPA-GMN-DOSRYKBNFLG-LST (IDX)
                                                          (1:2)
                   MOVE    SPA-GMN-DOSRYKBNFLG-LST(IDX)
                                          TO  SPA-GMN-DOSRYKBNFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    33              TO  SPA-GMN-CUR
               MOVE    "2004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-DOSRYKBNFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途表示一覧チェック処理（診療行為機能情報）
      *****************************************************************
       4108-WKSRYHYOFLG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-WKSRYHYOFLG-MAX
               IF      SPA-GMN-WKSRYHYOFLG
                                       =   SPA-GMN-WKSRYHYOFLG-LST (IDX)
                                                          (1:1)
                   MOVE    SPA-GMN-WKSRYHYOFLG-LST(IDX)
                                          TO  SPA-GMN-WKSRYHYOFLG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    34              TO  SPA-GMN-CUR
               MOVE    "2005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4108-WKSRYHYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金管理区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-SYUNYKNKNRKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUNYKNKNRKBN-MAX
               IF      SPA-GMN-SYUNYKNKNRKBN
                                       =   SPA-GMN-SYUNYKNKNRKBNL (IDX)
                   MOVE    SPA-GMN-SYUNYKNKNRKBNLST(IDX)
                                          TO  SPA-GMN-SYUNYKNKNRKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    42              TO  SPA-GMN-CUR
               MOVE    "3002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4109-SYUNYKNKNRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金画面請求書兼領収書発行区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S06SKYPRT-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYPRT-MAX
               IF      SPA-GMN-S06SKYPRT
                                       =   SPA-GMN-S06SKYPRTL (IDX)
                   MOVE    SPA-GMN-S06SKYPRTLST(IDX)
                                          TO  SPA-GMN-S06SKYPRT-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    47              TO  SPA-GMN-CUR
               MOVE    "3006"          TO  SPA-ERRCD
           END-IF
      *
           .
       4109-S06SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入金画面請求書兼領収書発行区分チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S06SKYSUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYSUM-MAX
               IF      SPA-GMN-S06SKYSUM
                                       =   SPA-GMN-S06SKYSUML (IDX)
                   MOVE    SPA-GMN-S06SKYSUMLST(IDX)
                                          TO  SPA-GMN-S06SKYSUM-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    48              TO  SPA-GMN-CUR
               MOVE    "3007"          TO  SPA-ERRCD
           END-IF
      *
           .
       4109-S06SKYSUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求一覧画面請求書兼領収書再発行方法チェック処理（収納行為機能情報）
      *****************************************************************
       4109-S02SKYSAI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYSAI-MAX
               IF      SPA-GMN-S02SKYSAI
                                       =   SPA-GMN-S02SKYSAIL (IDX)
                   MOVE    SPA-GMN-S02SKYSAILST(IDX)
                                          TO  SPA-GMN-S02SKYSAI-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    49              TO  SPA-GMN-CUR
               MOVE    "3008"          TO  SPA-ERRCD
           END-IF
      *
           .
       4109-S02SKYSAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行区分（外来）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSKYPRTG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTG-MAX
               IF      SPA-GMN-SYUSKYPRTG
                                       =   SPA-GMN-SYUSKYPRTGL (IDX)
                   MOVE    SPA-GMN-SYUSKYPRTGLST(IDX)
                                          TO  SPA-GMN-SYUSKYPRTG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    40              TO  SPA-GMN-CUR
               MOVE    "3001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4110-SYUSKYPRTG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分（外来）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSRYPRTG-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTG-MAX
               IF      SPA-GMN-SYUSRYPRTG
                                       =   SPA-GMN-SYUSRYPRTGL (IDX)
                   MOVE    SPA-GMN-SYUSRYPRTGLST(IDX)
                                          TO  SPA-GMN-SYUSRYPRTG-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    45              TO  SPA-GMN-CUR
               MOVE    "3005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4110-SYUSRYPRTG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書兼領収書発行区分（入院）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSKYPRTN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTN-MAX
               IF      SPA-GMN-SYUSKYPRTN
                                       =   SPA-GMN-SYUSKYPRTNL (IDX)
                   MOVE    SPA-GMN-SYUSKYPRTNLST(IDX)
                                          TO  SPA-GMN-SYUSKYPRTN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    41              TO  SPA-GMN-CUR
               MOVE    "3001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4110-SYUSKYPRTN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行区分（入院）チェック処理（収納行為機能情報）
      *****************************************************************
       4110-SYUSRYPRTN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTN-MAX
               IF      SPA-GMN-SYUSRYPRTN
                                       =   SPA-GMN-SYUSRYPRTNL (IDX)
                   MOVE    SPA-GMN-SYUSRYPRTNLST(IDX)
                                          TO  SPA-GMN-SYUSRYPRTN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    46              TO  SPA-GMN-CUR
               MOVE    "3005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4110-SYUSRYPRTN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    登録時刻チェック
      *****************************************************************
       4110-SYUSTIME-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-SYUSTIME-HH     >=  24 )
               MOVE    "3003"          TO  SPA-ERRCD
               MOVE    43              TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SYUSTIME-MM     >=  60 )
                   MOVE    "3003"      TO  SPA-ERRCD
                   MOVE    44          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4110-SYUSTIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    仮締め時間帯チェック
      *****************************************************************
       4110-SYUKARITIME-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE WRK-ERR-CUR
                       =   400
                       + ( IDX1    -   1 ) *  8
                       + ( IDX2    -   1 ) *  4
      *
                   IF    ( SPA-GMN-SYUKARIST-HH (IDX1 IDX2)    >=  24 )
                       MOVE    "3004"          TO  SPA-ERRCD
                       MOVE    WRK-ERR-CUR     TO  SPA-GMN-CUR
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIST-MM(IDX1 IDX2) >=  60 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   1
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIED-HH(IDX1 IDX2) >=  24 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   2
                       END-IF
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       IF    ( SPA-GMN-SYUKARIED-MM(IDX1 IDX2) >=  60 )
                           MOVE    "3004"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR
                               =   WRK-ERR-CUR +   3
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4110-SYUKARITIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    仮締め時間帯曜日チェック
      *****************************************************************
       4110-SYUKARIDAY-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   MOVE    W07-SYUKARIDAY (IDX1 IDX2)
                                       TO  SPA-GMN-SYUKARIDAY
                                                       (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           IF    ( WRK-KARIDAY     =   "1" )
               MOVE    1           TO  IDX1
               MOVE    2           TO  IDX2
           ELSE
               MOVE    2           TO  IDX1
               MOVE    1           TO  IDX2
           END-IF
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   7 )
      *
               IF    ( SPA-GMN-SYUKARIDAY (IDX1 IDX3)  =   "T" )
                   MOVE    "F"     TO  SPA-GMN-SYUKARIDAY (IDX2 IDX3)
               END-IF
      *
           END-PERFORM
      *
           .
       4110-SYUKARIDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名確認メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG1-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG1-DSPKBN-MAX
               IF      SPA-GMN-MSG1-DSPKBN
                                       =   SPA-GMN-MSG1-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG1-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG1-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    70              TO  SPA-GMN-CUR
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG1-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    移行病名警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG2-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG2-DSPKBN-MAX
               IF      SPA-GMN-MSG2-DSPKBN
                                       =   SPA-GMN-MSG2-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG2-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG2-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    71              TO  SPA-GMN-CUR
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG2-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    廃止年月日警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG3-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG3-DSPKBN-MAX
               IF      SPA-GMN-MSG3-DSPKBN
                                       =   SPA-GMN-MSG3-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG3-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG3-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    72              TO  SPA-GMN-CUR
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG3-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同一病名警告メッセージ表示有無チェック処理（病名機能情報）
      *****************************************************************
       4111-MSG4-DSPKBN-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG4-DSPKBN-MAX
               IF      SPA-GMN-MSG4-DSPKBN
                                       =   SPA-GMN-MSG4-DSPKBNL (IDX)
                   MOVE    SPA-GMN-MSG4-DSPKBNLST(IDX)
                                          TO  SPA-GMN-MSG4-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    73              TO  SPA-GMN-CUR
               MOVE    "4006"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-MSG4-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科初期表示区分チェック処理（病名機能情報）
      *****************************************************************
       4111-SRYKA-DSPKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-DSPKBN-MAX
               IF      SPA-GMN-SRYKA-DSPKBN
                                       =   SPA-GMN-SRYKA-DSPKBNL (IDX)
                   MOVE    SPA-GMN-SRYKA-DSPKBNLST(IDX)
                                          TO  SPA-GMN-SRYKA-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    74              TO  SPA-GMN-CUR
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-SRYKA-DSPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    表示対象期間初期表示区分チェック処理（病名機能情報）
      *****************************************************************
       4111-SYOKBN-DSPKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYOKBN-DSPKBN-MAX
               IF      SPA-GMN-SYOKBN-DSPKBN
                                       =   SPA-GMN-SYOKBN-DSPKBNL (IDX)
                   MOVE    SPA-GMN-SYOKBN-DSPKBNLST(IDX)
                                          TO  SPA-GMN-SYOKBN-DSPKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    75              TO  SPA-GMN-CUR
               MOVE    "4005"          TO  SPA-ERRCD
           END-IF
      *
           .
       4111-SYOKBN-DSPKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分初期表示チェック処理（病名機能情報）
      *****************************************************************
       4111-SJKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SJKBN-MAX
               IF      SPA-GMN-SJKBN   =   SPA-GMN-SJKBNLST (IDX)(1:2)
                   MOVE    SPA-GMN-SJKBNLST(IDX)
                                          TO  SPA-GMN-SJKBN-G
                   MOVE    1              TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    76              TO  SPA-GMN-CUR
               MOVE    "4007"          TO  SPA-ERRCD
           END-IF
           .
       4111-SJKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全科表示順チェック処理（会計照会機能情報）
      *****************************************************************
       4311-SRYKBN-FLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               IF      SPA-GMN-SRYKBN-FLG
                                       =   SPA-GMN-SRYKBN-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKBN-LIST(IDX)
                                           TO  SPA-GMN-SRYKBN-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    100             TO  SPA-GMN-CUR
               MOVE    "5001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-SRYKBN-FLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科の初期表示チェック処理（会計照会機能情報）
      *****************************************************************
       4311-SRYKA-FLG-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA-FLG
                                       =   SPA-GMN-SRYKA-LIST(IDX)
                                                             (1:1)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    101             TO  SPA-GMN-CUR
               MOVE    "5002"          TO  SPA-ERRCD
           END-IF
      *
           .
       4311-SRYKA-FLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W07"           TO  SPA-MOTOPG
      *
           INITIALIZE                  SPA-W01KYOUTU
      *
      ***  MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                 SECTION.
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
                   MOVE    "1002"              TO  SPA-WIDCD
               WHEN    "1017"
                   MOVE    "1004"              TO  SPA-WIDCD
               WHEN    "1038"
                   MOVE    "1006"              TO  SPA-WIDCD
               WHEN    "1039"
                   MOVE    "1008"              TO  SPA-WIDCD
               WHEN    "1042"
                   MOVE    "1010"              TO  SPA-WIDCD
               WHEN    "1043"
                   MOVE    "1012"              TO  SPA-WIDCD
           END-EVALUATE
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１００７
      *****************************************************************
       4032-DEL-1007-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                      SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1007-EDYUKYMD
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1007-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1007-EXT
           END-IF
      *
      *     MOVE   "JOIN"               TO  MCP-PUTTYPE
      *     PERFORM 210-BACK
           .
       4032-DEL-1007-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１００８
      *****************************************************************
       4032-DEL-1008-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                      SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1008-EDYUKYMD
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1008-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1008-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1008-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１０１７
      *****************************************************************
       4032-DEL-1017-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1017-EDYUKYMD
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1017-KEY
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1017-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1017-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０３８
      *****************************************************************
       4032-DEL-1038-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1038-EDYUKYMD
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1038-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1038-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1038-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０３９
      *****************************************************************
       4032-DEL-1039-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1039-REC
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1039-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1039-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1039-EXT.
           EXIT.
      *****************************************************************
      *    削除処理１０４２
      *****************************************************************
       4032-DEL-1042-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1042-REC
           INITIALIZE                      SYS-1042-REC
           MOVE    "1042"              TO  SYS-1042-KANRICD
           MOVE    "*"                 TO  SYS-1042-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1042-EDYUKYMD
           MOVE    SYS-1042-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1042-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4032-DEL-1042-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4032-DEL-1042-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理１０４３
      *****************************************************************
       4033-DEL-1043-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1043-REC
           INITIALIZE                      SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1043-EDYUKYMD
           MOVE    SYS-1043-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "9001"              TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1043-KEY 
                       ",MCP-RC:" MCP-RC 
               GO  TO  4033-DEL-1043-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4033-DEL-1043-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-W01-KANRICD
               WHEN    "1007"
                   PERFORM 4900-KANRENCHK-SEC
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1001"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1017"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1003"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1038"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1005"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1039"
                   PERFORM 4904-KANRENCHK-SEC
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1007"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1042"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1009"           TO  SPA-WIDCD
                   END-IF
               WHEN    "1043"
                   IF      SPA-ERRCD        =   SPACE
                       MOVE    "1011"           TO  SPA-WIDCD
                   END-IF
              END-EVALUATE
          END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1007-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                      SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1007-EDYUKYMD
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1007-REC
           ELSE
               MOVE    SPACE               TO  SYS-1007-REC
               INITIALIZE                      SYS-1007-REC
               MOVE    "1007"              TO  SYS-1007-KANRICD
               MOVE    "*"                 TO  SYS-1007-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1007-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1007-EDYUKYMD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               MOVE    SPA-GMN-DATA (IDX)  TO  SYS-1007-SANTEI (IDX:1)
           END-PERFORM 
           MOVE    SYS-1007-REC            TO  SYSKANRI-REC
      *     
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-1007-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-1007-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
      *     MOVE   "JOIN"               TO  MCP-PUTTYPE
      *     PERFORM 210-BACK 
           .
       491-KOUSHIN-1007-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1008-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                      SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1008-EDYUKYMD
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                      SYS-1008-REC
               MOVE    "1008"              TO  SYS-1008-KANRICD
               MOVE    "*"                 TO  SYS-1008-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1008-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1008-EDYUKYMD
           MOVE    SPA-GMN-INTERACT-KIKAN  TO  SYS-1008-INTERACT-KIKAN
           MOVE    SPA-GMN-GAIKANRIKBN-CHK TO  SYS-1008-GAIKANRIKBN-CHK
           MOVE    SPA-GMN-DATA(7)         TO  SYS-1008-IKUJIEIYO-FLG
           MOVE    SPA-GMN-SRYKACD         TO  SYS-1008-IKUJIEIYO-SRYKA
           MOVE    SPA-GMN-GAIMOVE         TO  SYS-1008-GAI-CURSOR-SET-1
      *    入院初診料自動発生
           MOVE    SPA-GMN-NYUSYOSIN       TO  SYS-1008-NYU-SYOSIN
      *    包括入力
           MOVE    SPA-GMN-HOUKATU         TO  SYS-1008-HOUKATU-FLG
      *    数量ゼロ入力
           MOVE    SPA-GMN-SURYOZERO       TO  SYS-1008-SURYOZERO-FLG
      *    入院調基チェック
           MOVE    SPA-GMN-CHOKICHK        TO  SYS-1008-CHOKICHK-FLG
      *    クリア確認チェック
           MOVE    SPA-GMN-CLEARCHK        TO  SYS-1008-CLEARCHK-FLG
      *    薬剤情報提供料チェック
           MOVE    SPA-GMN-YAKJYOCHK       TO  SYS-1008-YAKJYOCHK-FLG
      *
      *    時間外加算特例乳幼児
           MOVE    SPA-GMN-JIKANTOKU-NYU   TO
                                           SYS-1008-JIKANTOKU-NYU-FLG
      *    画像診断管理加算１（写真診断）
           MOVE    SPA-GMN-GAZOUKSN1       TO  SYS-1008-GAZOUKSN1-FLG
      *    最終来院日からの期間
           MOVE    SPA-GMN-LAST-KIKAN      TO  SYS-1008-LAST-KIKAN
      *    中途終了展開時の自動発生
           MOVE    SPA-GMN-CHUTOTENKAI     TO  SYS-1008-CHUTOTENKAI-FLG
      *    訂正時の自動発生
           MOVE    SPA-GMN-TEISEIAUTO      TO  SYS-1008-TEISEIAUTO-FLG
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    SPA-GMN-TEISEITOKU      TO  SYS-1008-TEISEITOKU-FLG
      *    病名表示区分
           MOVE    SPA-GMN-BYOHYOKBN       TO  SYS-1008-BYOHYOKBN-FLG
      *    剤登録順区分
           MOVE    SPA-GMN-ZAIJUNKBN       TO  SYS-1008-ZAIJUNKBN-FLG
      *    検体検査加算自動発生
           MOVE    SPA-GMN-KENSAAUTO       TO  SYS-1008-KENSAAUTO-FLG
      *
      *    時間外加算乳幼児チェック
           MOVE    SPA-GMN-JIKANTOKU-CHK   TO
                                           SYS-1008-JIKANTOKU-CHK
      *    チェック除外時間
           MOVE    SPA-NAI-JIKANCHK-WST(1) TO
                                           SYS-1008-JIKANCHK-WST(1)
           MOVE    SPA-NAI-JIKANCHK-WST(2) TO
                                           SYS-1008-JIKANCHK-WST(2)
           MOVE    SPA-NAI-JIKANCHK-WED(1) TO
                                           SYS-1008-JIKANCHK-WED(1)
           MOVE    SPA-NAI-JIKANCHK-WED(2) TO
                                           SYS-1008-JIKANCHK-WED(2)
           MOVE    SPA-NAI-JIKANCHK-SST(1) TO
                                           SYS-1008-JIKANCHK-SST(1)
           MOVE    SPA-NAI-JIKANCHK-SST(2) TO
                                           SYS-1008-JIKANCHK-SST(2)
           MOVE    SPA-NAI-JIKANCHK-SED(1) TO
                                           SYS-1008-JIKANCHK-SED(1)
           MOVE    SPA-NAI-JIKANCHK-SED(2) TO
                                           SYS-1008-JIKANCHK-SED(2)
      *
           MOVE    SYS-1008-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-1008-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-1008-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-1008-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1017-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                      SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1017-EDYUKYMD
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
           ELSE
               MOVE    SPACE               TO  SYS-1017-REC
               INITIALIZE                      SYS-1017-REC
               MOVE    "1017"              TO  SYS-1017-KANRICD
               MOVE    "*"                 TO  SYS-1017-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1017-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1017-EDYUKYMD
           MOVE    SPA-GMN-BYOMEIKBN       TO  SYS-1017-KARUTE-BYOMEI
           MOVE    SPA-GMN-JISYOKBN        TO  SYS-1017-JINMEI-USE
           MOVE    SPA-GMN-HAKKOKBN        TO  SYS-1017-KARUTE-HAKKO
           MOVE    SPA-GMN-HOJYOKBN        TO  SYS-1017-JIHIHKN-TAX
           MOVE    SPA-GMN-DOUMEI-CHKFLG   TO  SYS-1017-DOUMEI-CHKFLG
           MOVE    SPA-GMN-JYUFUKU-CHKKBN  TO  SYS-1017-JYUFUKU-CHKKBN
           MOVE    SPA-GMN-KOHONLY-CHKFLG  TO  SYS-1017-KOHONLY-CHKFLG
           MOVE    SPA-GMN-PTNUMDEL-FLG    TO  SYS-1017-PTNUMDEL-FLG
      *
           MOVE    SYS-1017-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-1017-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-1017-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1017-EXT.
           EXIT.
      *
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1038-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1038-EDYUKYMD
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1038-REC
           ELSE
               MOVE    SPACE               TO  SYS-1038-REC
               INITIALIZE                      SYS-1038-REC
               MOVE    "1038"              TO  SYS-1038-KANRICD
               MOVE    "*"                 TO  SYS-1038-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1038-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1038-EDYUKYMD
           MOVE    SPA-GMN-SRYNYKNKNRKBN   TO  SYS-1038-NYKNKNRKBN
           MOVE    SPA-GMN-KAGRPFLG        TO  SYS-1038-KAGRPFLG
           MOVE    SPA-GMN-HAKHOUFLG       TO  SYS-1038-HAKHOUFLG
           MOVE    SPA-GMN-DOSRYKBNFLG     TO  SYS-1038-DOSRYKBNFLG
           MOVE    SPA-GMN-WKSRYHYOFLG     TO  SYS-1038-WKSRYHYOFLG
      *
           MOVE    SYS-1038-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1038-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1039-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1039-REC
           INITIALIZE                      SYS-1039-REC
           MOVE    "1039"              TO  SYS-1039-KANRICD
           MOVE    "*"                 TO  SYS-1039-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1039-EDYUKYMD
           MOVE    SYS-1039-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1039-REC
           ELSE
               MOVE    SPACE               TO  SYS-1039-REC
               INITIALIZE                      SYS-1039-REC
               MOVE    "1039"              TO  SYS-1039-KANRICD
               MOVE    "*"                 TO  SYS-1039-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1039-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1039-EDYUKYMD
           MOVE    SPA-GMN-SYUNYKNKNRKBN   TO  SYS-1039-NYKNKNRKBN
           MOVE    SPA-GMN-S06SKYPRT       TO  SYS-1039-S06SKYPRTGKBN
                                               SYS-1039-S06SKYPRTNKBN
           MOVE    SPA-GMN-S06SKYSUM       TO  SYS-1039-S06SKYSUMKBN
           MOVE    SPA-GMN-SYUSKYPRTG      TO  SYS-1039-SKYPRTGKBN
           MOVE    SPA-GMN-SYUSKYPRTN      TO  SYS-1039-SKYPRTNKBN
           MOVE    SPA-GMN-SYUSRYPRTG      TO  SYS-1039-SRYPRTGKBN
           MOVE    SPA-GMN-SYUSRYPRTN      TO  SYS-1039-SRYPRTNKBN
           MOVE    SPA-GMN-SYUSTIME        TO  SYS-1039-STIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
                   IF    ( SPA-GMN-SYUKARIDAY (IDX1 IDX2)  =   "T" )
                       MOVE    "1"         TO  SYS-1039-KARIDAY
                                                        (IDX1 IDX2)
                   ELSE
                       MOVE    SPACE       TO  SYS-1039-KARIDAY
                                                        (IDX1 IDX2)
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                       MOVE    SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                                           TO  SYS-1039-KARISTTIME
                                                             (IDX1 IDX2)
                       MOVE    SPA-GMN-SYUKARIEDTIME (IDX1 IDX2)
                                           TO  SYS-1039-KARIEDTIME
                                                             (IDX1 IDX2)
               END-PERFORM
           END-PERFORM
      *
           MOVE    SYS-1039-REC            TO  PAT-1039-REC
           EVALUATE    SPA-GMN-S02SKYSAI
           WHEN    "0"
               MOVE    "0"             TO  PAT-1039-S02SKYSAIKBN
               MOVE    "1"             TO  PAT-1039-S02SKYMISYUPRTKBN
           WHEN    "1"
               MOVE    "1"             TO  PAT-1039-S02SKYSAIKBN
               MOVE    "1"             TO  PAT-1039-S02SKYMISYUPRTKBN
           WHEN    "2"
               MOVE    "0"             TO  PAT-1039-S02SKYSAIKBN
               MOVE    "0"             TO  PAT-1039-S02SKYMISYUPRTKBN
           WHEN    "3"
               MOVE    "1"             TO  PAT-1039-S02SKYSAIKBN
               MOVE    "0"             TO  PAT-1039-S02SKYMISYUPRTKBN
           END-EVALUATE
      *
           MOVE    PAT-1039-S02SKYMISYUPRTKBN
                                       TO  PAT-1039-S03SKYMISYUPRTKBN
      *
           MOVE    PAT-1039-REC        TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07  INSERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       490-KOUSHIN-1039-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-1042-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1042-REC
           INITIALIZE                      SYS-1042-REC
           MOVE    "1042"              TO  SYS-1042-KANRICD
           MOVE    "*"                 TO  SYS-1042-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1042-EDYUKYMD
           MOVE    SYS-1042-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1042-REC
           ELSE
               MOVE    SPACE               TO  SYS-1042-REC
               INITIALIZE                      SYS-1042-REC
               MOVE    "1042"              TO  SYS-1042-KANRICD
               MOVE    "*"                 TO  SYS-1042-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1042-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1042-EDYUKYMD
           MOVE    SPA-GMN-MSG1-DSPKBN     TO  SYS-1042-MSG1-DSPKBN
           MOVE    SPA-GMN-MSG2-DSPKBN     TO  SYS-1042-MSG2-DSPKBN
           MOVE    SPA-GMN-MSG3-DSPKBN     TO  SYS-1042-MSG3-DSPKBN
           MOVE    SPA-GMN-MSG4-DSPKBN     TO  SYS-1042-MSG4-DSPKBN
           MOVE    SPA-GMN-SRYKA-DSPKBN    TO  SYS-1042-SRYKA-DSPKBN
           MOVE    SPA-GMN-SYOKBN-DSPKBN   TO  SYS-1042-SYOKBN-DSPKBN
           MOVE    SPA-GMN-SJKBN           TO  SYS-1042-SJKBN-SET
      *
           MOVE    SYS-1042-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       491-KOUSHIN-1042-EXT.
           EXIT.
      ******************************************************************
      *       登録処理
      *****************************************************************
       493-KOUSHIN-1043-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYS-1043-REC
           INITIALIZE                      SYS-1043-REC
           MOVE    "1043"              TO  SYS-1043-KANRICD
           MOVE    "*"                 TO  SYS-1043-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1043-EDYUKYMD
           MOVE    SYS-1043-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1043-REC
           ELSE
               MOVE    SPACE               TO  SYS-1043-REC
               INITIALIZE                      SYS-1043-REC
               MOVE    "1043"              TO  SYS-1043-KANRICD
               MOVE    "*"                 TO  SYS-1043-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD        TO  SYS-1043-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD        TO  SYS-1043-EDYUKYMD
           MOVE    SPA-GMN-SRYKBN-FLG      TO  SYS-1043-SRYKBN-FLG
           MOVE    SPA-GMN-SRYKA-FLG       TO  SYS-1043-SRYKA-FLG
      *
           MOVE    SYS-1043-REC            TO  SYSKANRI-REC
      *
           IF      FLG-SYSKANRI            =   ZERO
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    "DBUPDATE"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 UPD ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           ELSE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
      *
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC            NOT =   ZERO 
                   MOVE    "9002"                  TO  SPA-ERRCD
                   DISPLAY "W07 INS ERR SYSKANRI-KEY =>" SYS-KEY
                           ",MCP-RC:" MCP-RC 
               END-IF 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       493-KOUSHIN-1043-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
           MOVE    SPA-GMN-CUR          TO  WRK-GMN-CUR
      *
           PERFORM 4100-DATA-CHK-SEC
      *
           IF      SPA-ERRCD            =   SPACE
               IF     (SPA-GMN-DATA (3)     =   "1"  OR  "2" )
               AND    (SPA-GMN-DATA (4)     =   "1"  OR  "2" )
                   MOVE    "0005"               TO  SPA-ERRCD
                   MOVE    4                    TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    時間外チェックありの時、ゼロはエラー
           IF     (SPA-ERRCD               =   SPACE  )  AND
                  (SPA-GMN-JIKANTOKU-CHK   =   "1"    )
               IF     (SPA-NAI-JIKANCHK-WEEK(1)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-WEEK(2)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-SAT (1)    =   ZERO )  AND
                      (SPA-NAI-JIKANCHK-SAT (2)    =   ZERO )
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           PERFORM VARYING     IDT     FROM    1   BY  1
                   UNTIL      (IDT     >   2   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF    ((SPA-NAI-JIKANCHK-WST(IDT)   >   ZERO  ) OR
                      (SPA-NAI-JIKANCHK-WED(IDT)   >   ZERO  )) AND
                      (SPA-NAI-JIKANCHK-WST(IDT)
                                           >  SPA-NAI-JIKANCHK-WED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    51                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
               IF    ((SPA-NAI-JIKANCHK-SST(IDT)   >   ZERO  ) OR
                      (SPA-NAI-JIKANCHK-SED(IDT)   >   ZERO  )) AND
                      (SPA-NAI-JIKANCHK-SST(IDT)
                                           >  SPA-NAI-JIKANCHK-SED(IDT))
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    59                  TO  SPA-GMN-CUR
                   COMPUTE SPA-GMN-CUR         =  ((IDT  -   1  )  *  4)
                                               +   SPA-GMN-CUR
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-GMN-CUR          TO  SPA-GMN-CUR
           END-IF
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理（収納機能情報）
      *****************************************************************
       4904-KANRENCHK-SEC          SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   COMPUTE WRK-ERR-CUR
                       =   400
                       + ( IDX1    -   1 ) *  8
                       + ( IDX2    -   1 ) *  4
      *
                   IF    ( SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                           >       SPA-GMN-SYUKARIEDTIME (IDX1 IDX2) )
      *
                       MOVE    WRK-ERR-CUR TO  SPA-GMN-CUR
                       MOVE    "3101"      TO  SPA-ERRCD
      *
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       4904-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W07    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W07
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W07-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W07-YUKOEDYMD
      *
      **** PERFORM VARYING IDX     FROM    1   BY  1
      *            UNTIL   IDX     >       7 
      *        MOVE    SPA-GMN-DATA-G(IDX)     TO  W07-DATA(IDX)
      *        PERFORM VARYING IDY     FROM    1   BY  1
      *                UNTIL   IDY     >       2
      *            MOVE    SPA-GMN-DATALST(IDY)
      *                                TO  W07-DATALST(IDX IDY)
      *        END-PERFORM
      *        MOVE    SPA-GMN-DATA-MAX    TO  W07-COUNT(IDX)
      **** END-PERFORM
           MOVE    SPA-GMN-DATA-G(01)      TO  W07-DATA01
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA01-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA01-COUNT
      *
           MOVE    SPA-GMN-DATA-G(02)      TO  W07-DATA02
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA02-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA02-COUNT
      *
           MOVE    SPA-GMN-DATA-G(03)      TO  W07-DATA03
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA03-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA03-COUNT
      *
           MOVE    SPA-GMN-DATA-G(04)      TO  W07-DATA04
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX2
               MOVE    SPA-GMN-DATALST2(IDY)
                                       TO  W07-DATA04-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX2   TO  W07-DATA04-COUNT
      *
           MOVE    SPA-GMN-DATA-G(05)      TO  W07-DATA05
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA05-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA05-COUNT
      *
           MOVE    SPA-GMN-DATA-G(06)      TO  W07-DATA06
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA06-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA06-COUNT
      *
           MOVE    SPA-GMN-DATA-G(07)      TO  W07-DATA07
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-DATA07-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-DATA07-COUNT
      *
           MOVE    SPA-GMN-SRYKACD     TO  W07-SRYKACD
           MOVE    SPA-GMN-SRYKAMEI    TO  W07-SRYKAMEI
      *    入院初診料
           MOVE    SPA-GMN-NYUSYOSIN-G TO  W07-NYUSYOSIN
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-NYUSYOSIN-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-NYUSYOSIN-COUNT
      *
      *    時間外加算特例乳幼児
           MOVE    SPA-GMN-JIKANTOKU-NYU-G TO  W07-JIKANTOKUNYU
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-JIKANTOKUNYU-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-JIKANTOKUNYU-COUNT
      *
      *    画像診断管理加算１（写真診断）
           MOVE    SPA-GMN-GAZOUKSN1-G TO  W07-GAZOUKSN1
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-GAZOUKSN1-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-GAZOUKSN1-COUNT
      *
      *    中途終了展開時の自動発生
           MOVE    SPA-GMN-CHUTOTENKAI-G   TO  W07-CHUTOTENKAI
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-CHUTOTENKAI-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-CHUTOTENKAI-COUNT
      *
      *    訂正時の自動発生
           MOVE    SPA-GMN-TEISEIAUTO-G    TO  W07-TEISEI
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-TEISEI-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-TEISEI-COUNT
      *
      *    訂正時の自動発生（特定疾患処方管理）
           MOVE    SPA-GMN-TEISEITOKU-G    TO  W07-TEISEITOKU
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-DATA-MAX
               MOVE    SPA-GMN-DATALST(IDY)
                                       TO  W07-TEISEITOKU-ITEM(IDY)
           END-PERFORM
           MOVE    SPA-GMN-DATA-MAX    TO  W07-TEISEITOKU-COUNT
      *
           MOVE  SPA-GMN-LAST-KIKAN    TO  W07-LASTKIKAN
      *
           MOVE  SPA-GMN-INTERACT-KIKAN    TO  W07-KIKAN
      *    外来管理加算チェック
           MOVE    SPA-GMN-GAIKANRIKBN-G   TO  W07-GAIKANKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-GAIKANRIKBN-LIST (IDX)
                                       TO  W07-GAIKANKBN-ITEM(IDX)
           END-PERFORM
           MOVE    3                   TO  W07-GAIKANKBN-COUNT
      *
      *    外来カーソル移動
           MOVE    SPA-GMN-GAIMOVE-G       TO  W07-GAIMOVE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-GAIMOVE-LIST (IDX)
                                       TO  W07-GAIMOVE-ITEM(IDX)
           END-PERFORM
           MOVE    3                   TO  W07-GAIMOVE-COUNT
      *
      *    数量ゼロ入力
           MOVE    SPA-GMN-SURYOZERO-G TO  W07-SURYOZERO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-SURYOZERO-LIST (IDX)
                                       TO  W07-SURYOZERO-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-SURYOZERO-COUNT
      *
      *    入院調基チェック
           MOVE    SPA-GMN-CHOKICHK-G  TO  W07-CHOKICHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-CHOKICHK-LIST (IDX)
                                       TO  W07-CHOKICHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-CHOKICHK-COUNT
      *
      *    クリア確認チェック
           MOVE    SPA-GMN-CLEARCHK-G  TO  W07-CLEARCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-CLEARCHK-LIST (IDX)
                                       TO  W07-CLEARCHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-CLEARCHK-COUNT
      *    薬剤情報提供料チェック
           MOVE    SPA-GMN-YAKJYOCHK-G  TO  W07-YAKJYOCHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-YAKJYOCHK-LIST (IDX)
                                       TO  W07-YAKJYOCHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-YAKJYOCHK-COUNT
      *
      *    包括入力
           MOVE    SPA-GMN-HOUKATU-G       TO  W07-HOUKATU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-HOUKATU-LIST (IDX)
                                       TO  W07-HOUKATU-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-HOUKATU-COUNT
      *    病名表示区分
           MOVE    SPA-GMN-BYOHYOKBN-G TO  W07-BYOHYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-BYOHYOKBN-LIST (IDX)
                                       TO  W07-BYOHYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-BYOHYOKBN-COUNT
      *    剤登録順区分
           MOVE    SPA-GMN-ZAIJUNKBN-G TO  W07-ZAIJUNKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-ZAIJUNKBN-LIST (IDX)
                                       TO  W07-ZAIJUNKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-ZAIJUNKBN-COUNT
      *
      *    検体検査加算自動発生
           MOVE    SPA-GMN-KENSAAUTO-G TO  W07-KENSAAUTO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-KENSAAUTO-LIST (IDX)
                                       TO  W07-KENSAAUTO-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-KENSAAUTO-COUNT
      *
      *    時間外加算チェック
           MOVE    SPA-GMN-JIKANTOKU-CHK-G TO  W07-JIKANTOKU-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-JIKANTOKU-CHK-LIST (IDX)
                                       TO  W07-JIKANTOKU-CHK-ITEM(IDX)
           END-PERFORM
           MOVE    2                   TO  W07-JIKANTOKU-CHK-COUNT
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   "0"
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                        UNTIL       IDX     >   2
                   MOVE    SPA-GMN-JIKANWST-HH(IDX)
                                           TO  W07-JIKANW1-HH (IDX)
                   MOVE    SPA-GMN-JIKANWST-MM(IDX)
                                           TO  W07-JIKANW1-MM (IDX)
                   MOVE    SPA-GMN-JIKANWED-HH(IDX)
                                           TO  W07-JIKANW2-HH (IDX)
                   MOVE    SPA-GMN-JIKANWED-MM(IDX)
                                           TO  W07-JIKANW2-MM (IDX)
      *
                   MOVE    SPA-GMN-JIKANSST-HH(IDX)
                                           TO  W07-JIKANS1-HH (IDX)
                   MOVE    SPA-GMN-JIKANSST-MM(IDX)
                                           TO  W07-JIKANS1-MM (IDX)
                   MOVE    SPA-GMN-JIKANSED-HH(IDX)
                                           TO  W07-JIKANS2-HH (IDX)
                   MOVE    SPA-GMN-JIKANSED-MM(IDX)
                                           TO  W07-JIKANS2-MM (IDX)
               END-PERFORM
           END-IF
      *
      *    カルテの病名対象
           MOVE    SPA-GMN-BYOMEIKBN-G     TO  W07-BYOMEIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-BYOMEIKBN-LIST (IDX)
                                       TO  W07-BYOMEIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    3               TO  W07-BYOMEIKBN-COUNT
      *
      *    人名辞書使用
           MOVE    SPA-GMN-JISYOKBN-G      TO  W07-JISYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-JISYOKBN-LIST (IDX)
                                       TO  W07-JISYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-JISYOKBN-COUNT
      *
      *    患者登録のカルテ発行
           MOVE    SPA-GMN-HAKKOKBN-G      TO  W07-HAKKOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    SPA-GMN-HAKKOKBN-LIST (IDX)
                                       TO  W07-HAKKOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    3               TO  W07-HAKKOKBN-COUNT
      *
      *    自費保険の補助区分
           MOVE    SPA-GMN-HOJYOKBN-G      TO  W07-HOJYOKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-HOJYOKBN-LIST (IDX)
                                       TO  W07-HOJYOKBN-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-HOJYOKBN-COUNT
      *
      *    同姓同名チェック
           MOVE    SPA-GMN-DOUMEI-CHKFLG-G     TO  W07-DOUMEI-CHKFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-DOUMEI-CHKFLG-LIST (IDX)
                                       TO  W07-DOUMEI-CHKFLG-ITEM(IDX)
           END-PERFORM
           MOVE    2               TO  W07-DOUMEI-CHKFLG-COUNT
      *
      *    二重登録疑いチェック
           MOVE    SPA-GMN-JYUFUKU-CHKKBN-G    TO  W07-JYUFUKU-CHKKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-JYUFUKU-MAX
               MOVE    SPA-GMN-JYUFUKU-CHKKBN-LIST (IDX)
                                       TO  W07-JYUFUKU-CHKKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-JYUFUKU-MAX TO  W07-JYUFUKU-CHKKBN-COUNT
      *    地方公費単独チェック
           MOVE    SPA-GMN-KOHONLY-CHKFLG-G    TO  W07-KOHONLY-CHKFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHONLY-MAX
               MOVE    SPA-GMN-KOHONLY-CHKFLG-LIST (IDX)
                                       TO  W07-KOHONLY-CHKFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOHONLY-MAX TO  W07-KOHONLY-CHKFLG-COUNT
      *    患者情報削除機能
           MOVE    SPA-GMN-PTNUMDEL-FLG-G  TO  W07-PTNUMDEL-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-PTNUMDEL-MAX
               MOVE    SPA-GMN-PTNUMDEL-FLG-LIST (IDX)
                                       TO  W07-PTNUMDEL-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-PTNUMDEL-MAX TO  W07-PTNUMDEL-FLG-COUNT
      *
      *    入金管理区分（診療行為機能情報）
           MOVE    SPA-GMN-SRYNYKNKNRKBN-G TO  W07-SRYNYKNKNRKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYNYKNKNRKBN-MAX
               MOVE    SPA-GMN-SRYNYKNKNRKBNLST (IDX)
                                       TO  W07-SRYNYKNKNRKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYNYKNKNRKBN-MAX
                                       TO  W07-SRYNYKNKNRKBN-COUNT
      *
      *    請求書発行方法（診療行為機能情報）
           MOVE    SPA-GMN-HAKHOUFLG-G TO  W07-HAKHOUFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HAKHOUFLG-MAX
               MOVE    SPA-GMN-HAKHOUFLG-LST (IDX)
                                       TO  W07-HAKHOUFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-HAKHOUFLG-MAX
                                       TO  W07-HAKHOUFLG-COUNT
      *    ＤＯ初期表示（診療行為機能情報）
           MOVE    SPA-GMN-DOSRYKBNFLG-G   TO  W07-DOSRYKBN-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-DOSRYKBNFLG-MAX
               MOVE    SPA-GMN-DOSRYKBNFLG-LST (IDX)
                                       TO  W07-DOSRYKBN-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-DOSRYKBNFLG-MAX
                                       TO  W07-DOSRYKBN-FLG-COUNT
      *    中途表示一覧（診療行為機能情報）
           MOVE    SPA-GMN-WKSRYHYOFLG-G   TO  W07-WKSRYHYO-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-WKSRYHYOFLG-MAX
               MOVE    SPA-GMN-WKSRYHYOFLG-LST (IDX)
                                       TO  W07-WKSRYHYO-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-WKSRYHYOFLG-MAX
                                       TO  W07-WKSRYHYO-FLG-COUNT
      *
      *    複数科まとめ（診療行為機能情報）
           MOVE    SPA-GMN-KAGRPFLG-G  TO  W07-KAGRPFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KAGRPFLG-MAX
               MOVE    SPA-GMN-KAGRPFLG-LST (IDX)
                                       TO  W07-KAGRPFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-KAGRPFLG-MAX
                                       TO  W07-KAGRPFLG-COUNT
      *
      *    入金管理区分（収納機能情報）
           MOVE    SPA-GMN-SYUNYKNKNRKBN-G TO  W07-SYUNYKNKNRKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUNYKNKNRKBN-MAX
               MOVE    SPA-GMN-SYUNYKNKNRKBNLST (IDX)
                                       TO  W07-SYUNYKNKNRKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUNYKNKNRKBN-MAX
                                       TO  W07-SYUNYKNKNRKBN-COUNT
      *
      *    一括入金画面請求書兼領収書発行区分（収納機能情報）
           MOVE    SPA-GMN-S06SKYPRT-G TO  W07-S06SKYPRT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYPRT-MAX
               MOVE    SPA-GMN-S06SKYPRTLST (IDX)
                                       TO  W07-S06SKYPRT-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S06SKYPRT-MAX
                                       TO  W07-S06SKYPRT-COUNT
      *
      *    一括入金画面外来分発行方法（収納機能情報）
           MOVE    SPA-GMN-S06SKYSUM-G TO  W07-S06SKYSUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S06SKYSUM-MAX
               MOVE    SPA-GMN-S06SKYSUMLST (IDX)
                                       TO  W07-S06SKYSUM-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S06SKYSUM-MAX
                                       TO  W07-S06SKYSUM-COUNT
      *
      *    請求一覧画面外来分再発行方法（収納機能情報）
           MOVE    SPA-GMN-S02SKYSAI-G TO  W07-S02SKYSAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-S02SKYSAI-MAX
               MOVE    SPA-GMN-S02SKYSAILST (IDX)
                                       TO  W07-S02SKYSAI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-S02SKYSAI-MAX
                                       TO  W07-S02SKYSAI-COUNT
      *
      *    請求書兼領収書発行区分（外来）
           MOVE    SPA-GMN-SYUSKYPRTG-G TO  W07-SYUSKYPRTG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTG-MAX
               MOVE    SPA-GMN-SYUSKYPRTGLST (IDX)
                                       TO  W07-SYUSKYPRTG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSKYPRTG-MAX
                                       TO  W07-SYUSKYPRTG-COUNT
      *
      *    診療費明細書発行区分（外来）
           MOVE    SPA-GMN-SYUSRYPRTG-G TO  W07-SYUSRYPRTG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTG-MAX
               MOVE    SPA-GMN-SYUSRYPRTGLST (IDX)
                                       TO  W07-SYUSRYPRTG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSRYPRTG-MAX
                                       TO  W07-SYUSRYPRTG-COUNT
      *
      *    請求書兼領収書発行区分（入院）
           MOVE    SPA-GMN-SYUSKYPRTN-G TO  W07-SYUSKYPRTN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSKYPRTN-MAX
               MOVE    SPA-GMN-SYUSKYPRTNLST (IDX)
                                       TO  W07-SYUSKYPRTN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSKYPRTN-MAX
                                       TO  W07-SYUSKYPRTN-COUNT
      *
      *    診療費明細書発行区分（入院）
           MOVE    SPA-GMN-SYUSRYPRTN-G TO  W07-SYUSRYPRTN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYUSRYPRTN-MAX
               MOVE    SPA-GMN-SYUSRYPRTNLST (IDX)
                                       TO  W07-SYUSRYPRTN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYUSRYPRTN-MAX
                                       TO  W07-SYUSRYPRTN-COUNT
      *
      *    過去日分の登録時刻
           MOVE    SPA-GMN-SYUSTIME-HH TO  W07-SYUSTIME-HH
           MOVE    SPA-GMN-SYUSTIME-MM TO  W07-SYUSTIME-MM
      *
      *    仮締め時間帯
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   7 )
      *
                   MOVE    SPA-GMN-SYUKARIDAY (IDX1 IDX2)
                                       TO  W07-SYUKARIDAY
                                                       (IDX1 IDX2)
               END-PERFORM
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   2 )
      *
                   MOVE    SPA-GMN-SYUKARISTTIME (IDX1 IDX2)
                                       TO  W07-SYUKARISTTIME
                                                       (IDX1 IDX2)
                   MOVE    SPA-GMN-SYUKARIEDTIME (IDX1 IDX2)
                                       TO  W07-SYUKARIEDTIME
                                                       (IDX1 IDX2)
      *
               END-PERFORM
           END-PERFORM
      *
      *    主病名確認メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG1-DSPKBN-G   TO  W07-MSG1-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG1-DSPKBN-MAX
               MOVE    SPA-GMN-MSG1-DSPKBNLST (IDX)
                                       TO  W07-MSG1-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG1-DSPKBN-MAX
                                       TO  W07-MSG1-DSPKBN-COUNT
      *
      *    移行病名警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG2-DSPKBN-G   TO  W07-MSG2-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG2-DSPKBN-MAX
               MOVE    SPA-GMN-MSG2-DSPKBNLST (IDX)
                                       TO  W07-MSG2-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG2-DSPKBN-MAX
                                       TO  W07-MSG2-DSPKBN-COUNT
      *
      *    廃止年月日警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG3-DSPKBN-G   TO  W07-MSG3-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG3-DSPKBN-MAX
               MOVE    SPA-GMN-MSG3-DSPKBNLST (IDX)
                                       TO  W07-MSG3-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG3-DSPKBN-MAX
                                       TO  W07-MSG3-DSPKBN-COUNT
      *
      *    同一病名警告メッセージ表示有無区分（病名機能情報）
           MOVE    SPA-GMN-MSG4-DSPKBN-G   TO  W07-MSG4-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MSG4-DSPKBN-MAX
               MOVE    SPA-GMN-MSG4-DSPKBNLST (IDX)
                                       TO  W07-MSG4-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MSG4-DSPKBN-MAX
                                       TO  W07-MSG4-DSPKBN-COUNT
      *
      *    診療科初期表示区分（病名機能情報）
           MOVE    SPA-GMN-SRYKA-DSPKBN-G   TO  W07-SRYKA-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-DSPKBN-MAX
               MOVE    SPA-GMN-SRYKA-DSPKBNLST (IDX)
                                       TO  W07-SRYKA-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-DSPKBN-MAX
                                       TO  W07-SRYKA-DSPKBN-COUNT
      *
      *    表示対象期間初期表示区分（病名機能情報）
           MOVE    SPA-GMN-SYOKBN-DSPKBN-G   TO  W07-SYOKBN-DSPKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SYOKBN-DSPKBN-MAX
               MOVE    SPA-GMN-SYOKBN-DSPKBNLST (IDX)
                                       TO  W07-SYOKBN-DSPKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYOKBN-DSPKBN-MAX
                                       TO  W07-SYOKBN-DSPKBN-COUNT
      *
      *    表示対象期間初期表示区分（病名機能情報）
           MOVE    SPA-GMN-SJKBN-G    TO  W07-SJKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SJKBN-MAX
               MOVE    SPA-GMN-SJKBNLST (IDX)
                                       TO  W07-SJKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SJKBN-MAX
                                       TO  W07-SJKBN-COUNT
      *
      *    全科の表示順区分（会計照会機能情報）
           MOVE    SPA-GMN-SRYKBN-FLG-G    TO  W07-SRYKBN-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBN-LIST (IDX)
                                       TO  W07-SRYKBN-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKBN-MAX  TO  W07-SRYKBN-FLG-COUNT
      *    診療科の初期表示（会計照会機能情報）
           MOVE    SPA-GMN-SRYKA-FLG-G TO  W07-SRYKA-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                       TO  W07-SRYKA-FLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-MAX   TO  W07-SRYKA-FLG-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE  TO  W07-B03-STATE
                                           W07-TAGNAME1-STATE
                                           W07-TAGNAME2-STATE
                                           W07-TAGNAME3-STATE
                                           W07-TAGNAME4-STATE
                                           W07-TAGNAME5-STATE
                                           W07-TAGNAME6-STATE
                                           W07-SRYKACD-STATE
      *
           IF      SPA-W01-SELNUM          >   ZERO
               MOVE    WIDGET-NORMAL   TO  W07-B03-STATE
           END-IF 
      *
           EVALUATE    SPA-W01-KANRICD
               WHEN    "1007" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME1-STATE
               WHEN    "1017" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME2-STATE
               WHEN    "1038" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME3-STATE
               WHEN    "1039" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME4-STATE
               WHEN    "1042" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME5-STATE
               WHEN    "1043" 
                   MOVE    WIDGET-NORMAL   TO  W07-TAGNAME6-STATE
           END-EVALUATE
      *
           IF      SPA-GMN-DATA(7)         =   "1"
               MOVE    WIDGET-NORMAL   TO  W07-SRYKACD-STATE
           END-IF
      *    チェック除外時間
           IF      SPA-GMN-JIKANTOKU-CHK   =   "1"
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   2
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW1-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW1-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW2-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANW2-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS1-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS1-MM-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS2-HH-STATE(IDX)
                   MOVE    WIDGET-NORMAL   TO  W07-JIKANS2-MM-STATE(IDX)
               END-PERFORM
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   2
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW1-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW1-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW2-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANW2-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS1-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS1-MM-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS2-HH-STATE
                                                                  (IDX)
                   MOVE    WIDGET-INSENSITIVE  TO  W07-JIKANS2-MM-STATE
                                                                  (IDX)
               END-PERFORM
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "調剤技術基本料入力エラー"  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "薬剤情報提供料（一般）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                      "薬剤情報提供料（老人手帳記載あり）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                       "薬剤情報提供料（老人手帳記載なし）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE
                 "薬剤情報提供料（老人手帳記載）は両方の選択できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "外来管理加算チェック区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE
                       "療養担当手当（北海道）入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE
                       "特定薬剤治療管理料入力エラー"  TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                       "育児栄養加算自動算定入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "自動算定科がありません。"  TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "外来カーソル移動区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入院初診料入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "包括登録入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "数量ゼロ入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "入院調剤技術基本料チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "時間外加算特例乳幼児チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "画像診断管理加算チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "中途終了展開時の自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "訂正時の自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0081"
                   MOVE    "訂正時の自動発生（特定疾患処方管理）"
                                                    TO  SPA-ERRMSG
                   MOVE    "チェックエラー"
                                                    TO  SPA-ERRMSG(37:)
               WHEN    "0082"
                   MOVE    "検体検査加算自動発生チェックエラー"
                                                    TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "時間外加算（乳幼児）チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "チェック除外時間が違います。"
                                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "チェック除外開始時間＜終了時間で"
                                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい"
                                               TO  SPA-ERRMSG(35:)
               WHEN    "0033"
                   MOVE    "チェック除外時間が設定されていません。"
                                                       TO  SPA-ERRMSG
                   MOVE    "入力して下さい"
                                               TO  SPA-ERRMSG(39:)
               WHEN    "0034"
                   MOVE    "診療行為クリア確認チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "薬剤情報提供料チェックエラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "病名表示エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "剤登録順エラー"
                                                       TO  SPA-ERRMSG
      *        
               WHEN    "1001"
                   MOVE    "カルテの病名対象区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "人名辞書使用区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "患者登録のカルテ発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "自費保険の補助区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "同姓同名者リスト区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "二重登録疑いリスト区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "地方公費単独チェック区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "患者削除機能区分がありません。"
                                                       TO  SPA-ERRMSG
      *        診療行為機能情報
               WHEN    "2001"
                   MOVE    "入金管理区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "複数科まとめ集計区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2003"
                   MOVE    "請求書発行方法区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2004"
                   MOVE    "ＤＯ初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "2005"
                   MOVE    "中途終了一覧区分がありません。"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "3001"
                   MOVE    "請求書兼領収書発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "3002"
                   MOVE    "入金管理区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "3003"
                   MOVE    "登録時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3004"
                   MOVE    "仮締め時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3005"
                   MOVE    "仮締め時刻入力エラー"
                                                       TO  SPA-ERRMSG
               WHEN    "3006"
                   MOVE    "請求書兼領収書発行区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "3007"
                   MOVE    "外来分発行方法がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "3008"
                   MOVE    "再発行方法がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "3101"
                   MOVE    "期間の指定に誤りがあります"
                                                       TO  SPA-ERRMSG
      *    病名機能情報
               WHEN    "4001"
                   MOVE
                       "主病名確認メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4002"
                   MOVE
                   "移行病名警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4003"
                   MOVE
                   "廃止年月日警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4004"
                   MOVE    "診療科初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4005"
                   MOVE    "表示対象期間初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4006"
                   MOVE
                   "同一病名警告メッセージ表示有無区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "4007"
                   MOVE    "詳記区分初期表示がありません。"
                                                       TO  SPA-ERRMSG
      *        会計照会機能情報
               WHEN    "5001"
                   MOVE    "全科の表示順区分がありません。"
                                                       TO  SPA-ERRMSG
               WHEN    "5002"
                   MOVE    "診療科の初期表示区分がありません。"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "0099"
                   MOVE    "未入力の項目があります"    TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除ができませんでした"    TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新ができませんでした"    TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W07"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "自動算定・チェック機能制御情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "自動算定・チェック機能制御情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "患者登録機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1004"
                   MOVE    "患者登録機能情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1005"
                   MOVE    "診療行為機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1006"
                   MOVE    "診療行為機能情報を削除します"
                                                       TO  WRK-WIDMSG
               WHEN    "1007"
                   MOVE    "収納機能情報を登録します"
                                                       TO  WRK-WIDMSG
               WHEN    "1008"
                   MOVE    "収納機能情報を削除します"
                                                       TO  WRK-WIDMSG
      *
               WHEN    "1009"
                   MOVE    "病名機能情報を登録します"  TO  WRK-WIDMSG
               WHEN    "1010"
                   MOVE    "病名機能情報を削除します"  TO  WRK-WIDMSG
      *
               WHEN    "1011"
                   MOVE    "会計照会機能情報を登録します"
                                                   TO  WRK-WIDMSG
               WHEN    "1012"
                   MOVE    "会計照会機能情報を削除します"
                                                   TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W07"               TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2
                   MOVE  "DATA01"          TO   MCP-WIDGET
               WHEN    3
                   MOVE  "DATA02"          TO   MCP-WIDGET
               WHEN    4
                   MOVE  "DATA03"          TO   MCP-WIDGET
               WHEN    5
                   MOVE  "DATA04"          TO   MCP-WIDGET
               WHEN    6
                   MOVE  "DATA05"          TO   MCP-WIDGET
               WHEN    7
                   MOVE  "DATA06"          TO   MCP-WIDGET
               WHEN    8
                   MOVE  "DATA07"          TO   MCP-WIDGET
               WHEN    9
                   MOVE  "SRYKACD"         TO   MCP-WIDGET
               WHEN    13
                   MOVE  "NYUSYOSIN"       TO   MCP-WIDGET
               WHEN    10
                   MOVE  "KIKAN"           TO   MCP-WIDGET
               WHEN    11
                   MOVE  "GAIKANKBN"       TO   MCP-WIDGET
               WHEN    12
                   MOVE  "GAIMOVE"         TO   MCP-WIDGET
               WHEN    14
                   MOVE  "SURYOZERO"       TO   MCP-WIDGET
               WHEN    15
                   MOVE  "CHOKICHK"        TO   MCP-WIDGET
               WHEN    16
                   MOVE  "HOUKATU"         TO   MCP-WIDGET
               WHEN    17
                   MOVE  "JIKANTOKUNYU"    TO   MCP-WIDGET
               WHEN    18
                   MOVE  "GAZOUKSN1"       TO   MCP-WIDGET
               WHEN    19
                   MOVE  "LASTKIKAN"       TO   MCP-WIDGET
               WHEN    67
                   MOVE  "CLEARCHK"        TO   MCP-WIDGET
               WHEN    68
                   MOVE  "CHUTOTENKAI"     TO   MCP-WIDGET
               WHEN    69
                   MOVE  "YAKJYOCHK"       TO   MCP-WIDGET
               WHEN    80
                   MOVE  "TEISEI"          TO   MCP-WIDGET
               WHEN    81
                   MOVE  "TEISEITOKU"      TO   MCP-WIDGET
               WHEN    82
                   MOVE  "BYOHYOKBN"       TO   MCP-WIDGET
               WHEN    83
                   MOVE  "ZAIJUNKBN"       TO   MCP-WIDGET
               WHEN    84
                   MOVE  "KENSAAUTO"       TO   MCP-WIDGET
      *        時間外加算チェック
               WHEN    50
                   MOVE  "JIKANTOKU_CHK"   TO   MCP-WIDGET
               WHEN    51
                   MOVE  "JIKANW1_HH1"     TO   MCP-WIDGET
               WHEN    52
                   MOVE  "JIKANW1_MM1"     TO   MCP-WIDGET
               WHEN    53
                   MOVE  "JIKANW1_HH2"     TO   MCP-WIDGET
               WHEN    54
                   MOVE  "JIKANW1_MM2"     TO   MCP-WIDGET
               WHEN    55
                   MOVE  "JIKANW2_HH1"     TO   MCP-WIDGET
               WHEN    56
                   MOVE  "JIKANW2_MM1"     TO   MCP-WIDGET
               WHEN    57
                   MOVE  "JIKANW2_HH2"     TO   MCP-WIDGET
               WHEN    58
                   MOVE  "JIKANW2_MM2"     TO   MCP-WIDGET
               WHEN    59
                   MOVE  "JIKANS1_HH1"     TO   MCP-WIDGET
               WHEN    60
                   MOVE  "JIKANS1_MM1"     TO   MCP-WIDGET
               WHEN    61
                   MOVE  "JIKANS1_HH2"     TO   MCP-WIDGET
               WHEN    62
                   MOVE  "JIKANS1_MM2"     TO   MCP-WIDGET
               WHEN    63
                   MOVE  "JIKANS2_HH1"     TO   MCP-WIDGET
               WHEN    64
                   MOVE  "JIKANS2_MM1"     TO   MCP-WIDGET
               WHEN    65
                   MOVE  "JIKANS2_HH2"     TO   MCP-WIDGET
               WHEN    66
                   MOVE  "JIKANS2_MM2"     TO   MCP-WIDGET
      *
               WHEN    20
                   MOVE  "BYOMEIKBN"       TO   MCP-WIDGET
               WHEN    21
                   MOVE  "JISYOKBN"        TO   MCP-WIDGET
               WHEN    22
                   MOVE  "HAKKOKBN"        TO   MCP-WIDGET
               WHEN    23
                   MOVE  "HOJYOKBN"        TO   MCP-WIDGET
               WHEN    24
                   MOVE  "DOUMEI_CHKFLG"   TO   MCP-WIDGET
               WHEN    25
                   MOVE  "JYUFUKU_CHKKBN"  TO   MCP-WIDGET
               WHEN    26
                   MOVE  "KOHONLY_CHKFLG"  TO   MCP-WIDGET
               WHEN    27
                   MOVE  "PTNUMDEL_FLG"    TO   MCP-WIDGET
      *
               WHEN    30
                   MOVE  "SRYNYKNKNRKBN"   TO   MCP-WIDGET
               WHEN    31
                   MOVE  "KAGRPFLG"        TO   MCP-WIDGET
               WHEN    32
                   MOVE  "HAKHOUFLG"       TO   MCP-WIDGET
               WHEN    33
                   MOVE  "DOSRYKBN_FLG"    TO   MCP-WIDGET
               WHEN    34
                   MOVE  "WKSRYHYO_FLG"    TO   MCP-WIDGET
      *
               WHEN    40
                   MOVE  "SYUSKYPRTG"      TO   MCP-WIDGET
               WHEN    45
                   MOVE  "SYUSRYPRTG"      TO   MCP-WIDGET
               WHEN    41
                   MOVE  "SYUSKYPRTN"      TO   MCP-WIDGET
               WHEN    46
                   MOVE  "SYUSRYPRTN"      TO   MCP-WIDGET
               WHEN    42
                   MOVE  "SYUNYKNKNRKBN"   TO   MCP-WIDGET
               WHEN    47
                   MOVE  "S06SKYPRT"       TO   MCP-WIDGET
               WHEN    48
                   MOVE  "S06SKYSUM"       TO   MCP-WIDGET
               WHEN    49
                   MOVE  "S02SKYSAI"       TO   MCP-WIDGET
               WHEN    43
                   MOVE  "SYUSTIME_HH"     TO   MCP-WIDGET
               WHEN    44
                   MOVE  "SYUSTIME_MM"     TO   MCP-WIDGET
               WHEN    400
                   MOVE  "SYUKARIST11_HH"  TO   MCP-WIDGET
               WHEN    401
                   MOVE  "SYUKARIST11_MM"  TO   MCP-WIDGET
               WHEN    402
                   MOVE  "SYUKARIED11_HH"  TO   MCP-WIDGET
               WHEN    403
                   MOVE  "SYUKARIED11_MM"  TO   MCP-WIDGET
               WHEN    404
                   MOVE  "SYUKARIST12_HH"  TO   MCP-WIDGET
               WHEN    405
                   MOVE  "SYUKARIST12_MM"  TO   MCP-WIDGET
               WHEN    406
                   MOVE  "SYUKARIED12_HH"  TO   MCP-WIDGET
               WHEN    407
                   MOVE  "SYUKARIED12_MM"  TO   MCP-WIDGET
               WHEN    408
                   MOVE  "SYUKARIST21_HH"  TO   MCP-WIDGET
               WHEN    409
                   MOVE  "SYUKARIST21_MM"  TO   MCP-WIDGET
               WHEN    410
                   MOVE  "SYUKARIED21_HH"  TO   MCP-WIDGET
               WHEN    411
                   MOVE  "SYUKARIED21_MM"  TO   MCP-WIDGET
               WHEN    412
                   MOVE  "SYUKARIST22_HH"  TO   MCP-WIDGET
               WHEN    413
                   MOVE  "SYUKARIST22_MM"  TO   MCP-WIDGET
               WHEN    414
                   MOVE  "SYUKARIED22_HH"  TO   MCP-WIDGET
               WHEN    415
                   MOVE  "SYUKARIED22_MM"  TO   MCP-WIDGET
      *
               WHEN    70
                   MOVE  "MSG1_DSPKBN"     TO   MCP-WIDGET
               WHEN    71
                   MOVE  "MSG2_DSPKBN"     TO   MCP-WIDGET
               WHEN    72
                   MOVE  "MSG3_DSPKBN"     TO   MCP-WIDGET
               WHEN    73
                   MOVE  "MSG4_DSPKBN"     TO   MCP-WIDGET
               WHEN    74
                   MOVE  "SRYKA_DSPKBN"    TO   MCP-WIDGET
               WHEN    75
                   MOVE  "SYOKBN_DSPKBN"   TO   MCP-WIDGET
               WHEN    76
                   MOVE  "SJKBN"           TO   MCP-WIDGET
      *
               WHEN    100
                   MOVE  "SRYKBN_FLG"      TO   MCP-WIDGET
               WHEN    101
                   MOVE  "SRYKA_FLG"       TO   MCP-WIDGET
      *
               WHEN    99
                   MOVE  "B12"             TO   MCP-WIDGET
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2   THRU    19
               WHEN    50  THRU    69
               WHEN    80  THRU    89
                   MOVE    0               TO  W07-PAGENO
                   MOVE    0               TO  SPA-GMN-PAGENO
               WHEN    20  THRU    29
                   MOVE    1               TO  W07-PAGENO
                   MOVE    1               TO  SPA-GMN-PAGENO
               WHEN    30  THRU    39
                   MOVE    2               TO  W07-PAGENO
                   MOVE    2               TO  SPA-GMN-PAGENO
               WHEN    40  THRU    49
               WHEN    400 THRU    499
                   MOVE    3               TO  W07-PAGENO
                   MOVE    3               TO  SPA-GMN-PAGENO
               WHEN    70  THRU    79
                   MOVE    4               TO  W07-PAGENO
                   MOVE    4               TO  SPA-GMN-PAGENO
               WHEN    100 THRU    100
                   MOVE    5               TO  W07-PAGENO
                   MOVE    5               TO  SPA-GMN-PAGENO
               WHEN    OTHER
                   MOVE    SPA-GMN-PAGENO  TO  W07-PAGENO
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "DATA01"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "DATA02"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "DATA03"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "DATA04"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "DATA05"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "DATA06"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "DATA07"
                   IF      SPA-GMN-DATA(7)     =   "1"
                       MOVE    9           TO  SPA-GMN-CUR
                   ELSE
                       MOVE    13          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "SRYKACD"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "NYUSYOSIN"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "JIKANTOKUNYU"
                   MOVE    18              TO  SPA-GMN-CUR
               WHEN    "GAZOUKSN1"
                   MOVE    19              TO  SPA-GMN-CUR
               WHEN    "LASTKIKAN"
      ************ MOVE    10              TO  SPA-GMN-CUR
                   MOVE    68              TO  SPA-GMN-CUR
               WHEN    "CHUTOTENKAI"
                   MOVE    80              TO  SPA-GMN-CUR
               WHEN    "TEISEI"
                   MOVE    81              TO  SPA-GMN-CUR
               WHEN    "TEISEITOKU"
                   MOVE    84              TO  SPA-GMN-CUR
               WHEN    "KENSAAUTO"
                   MOVE    10              TO  SPA-GMN-CUR
      *
               WHEN    "KIKAN"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "GAIKANKBN"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "GAIMOVE"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN    "SURYOZERO"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "CHOKICHK"
      ************ MOVE    16              TO  SPA-GMN-CUR
                   MOVE    50              TO  SPA-GMN-CUR
               WHEN    "CLEARCHK"
      ************ MOVE    16              TO  SPA-GMN-CUR
                   MOVE    69              TO  SPA-GMN-CUR
               WHEN    "YAKJYOCHK"
                   MOVE    16              TO  SPA-GMN-CUR
      *        時間外加算チェック
               WHEN    "JIKANTOKU_CHK"
                   IF      SPA-GMN-JIKANTOKU-CHK   =   "1"
                       MOVE    51          TO  SPA-GMN-CUR
                   ELSE
      *****************MOVE    16          TO  SPA-GMN-CUR
                       MOVE    67          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "JIKANW1_HH1"
                   MOVE    52              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_MM1"
                   MOVE    53              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_HH2"
                   MOVE    54              TO  SPA-GMN-CUR
               WHEN    "JIKANW1_MM2"
                   MOVE    55              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_HH1"
                   MOVE    56              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_MM1"
                   MOVE    57              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_HH2"
                   MOVE    58              TO  SPA-GMN-CUR
               WHEN    "JIKANW2_MM2"
                   MOVE    59              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_HH1"
                   MOVE    60              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_MM1"
                   MOVE    61              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_HH2"
                   MOVE    62              TO  SPA-GMN-CUR
               WHEN    "JIKANS1_MM2"
                   MOVE    63              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_HH1"
                   MOVE    64              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_MM1"
                   MOVE    65              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_HH2"
                   MOVE    66              TO  SPA-GMN-CUR
               WHEN    "JIKANS2_MM2"
      ************ MOVE    16              TO  SPA-GMN-CUR
                   MOVE    67              TO  SPA-GMN-CUR
      *
      ******   WHEN    "CLEARCHK"
      *******      MOVE    17              TO  SPA-GMN-CUR
               WHEN    "HOUKATU"
      *********    MOVE    99              TO  SPA-GMN-CUR
                   MOVE    82              TO  SPA-GMN-CUR
               WHEN    "BYOHYOKBN"
                   MOVE    83              TO  SPA-GMN-CUR
               WHEN    "ZAIJUNKBN"
                   MOVE    99              TO  SPA-GMN-CUR
      *        患者登録情報
               WHEN    "BYOMEIKBN"
                   MOVE    21              TO  SPA-GMN-CUR
               WHEN    "JISYOKBN"
                   MOVE    22              TO  SPA-GMN-CUR
               WHEN    "HAKKOKBN"
                   MOVE    23              TO  SPA-GMN-CUR
               WHEN    "HOJYOKBN"
                   MOVE    24              TO  SPA-GMN-CUR
               WHEN    "DOUMEI_CHKFLG"
                   MOVE    25              TO  SPA-GMN-CUR
               WHEN    "JYUFUKU_CHKKBN"
      **********   MOVE    99              TO  SPA-GMN-CUR
                   MOVE    26              TO  SPA-GMN-CUR
               WHEN    "KOHONLY_CHKFLG"
                   MOVE    27              TO  SPA-GMN-CUR
               WHEN    "PTNUMDEL_FLG"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN    "DOSRYKBN_FLG"
                   MOVE    34              TO  SPA-GMN-CUR
               WHEN    "WKSRYHYO_FLG"
                   MOVE    30              TO  SPA-GMN-CUR
               WHEN    "SRYNYKNKNRKBN"
                   MOVE    31              TO  SPA-GMN-CUR
               WHEN    "KAGRPFLG"
                   MOVE    32              TO  SPA-GMN-CUR
               WHEN    "HAKHOUFLG"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN    "S02SKYSAI"
                   MOVE    40              TO  SPA-GMN-CUR
               WHEN    "SYUSKYPRTG"
                   MOVE    45              TO  SPA-GMN-CUR
               WHEN    "SYUSRYPRTG"
                   MOVE    41              TO  SPA-GMN-CUR
               WHEN    "SYUSKYPRTN"
                   MOVE    46              TO  SPA-GMN-CUR
               WHEN    "SYUSRYPRTN"
                   MOVE    42              TO  SPA-GMN-CUR
               WHEN    "SYUNYKNKNRKBN"
                   MOVE    47              TO  SPA-GMN-CUR
               WHEN    "S06SKYPRT"
                   MOVE    48              TO  SPA-GMN-CUR
               WHEN    "S06SKYSUM"
                   MOVE    43              TO  SPA-GMN-CUR
               WHEN    "SYUSTIME_HH"
                   MOVE    44              TO  SPA-GMN-CUR
               WHEN    "SYUSTIME_MM"
                   MOVE    400             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST11_HH"
                   MOVE    401             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST11_MM"
                   MOVE    402             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED11_HH"
                   MOVE    403             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED11_MM"
                   MOVE    404             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST12_HH"
                   MOVE    405             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST12_MM"
                   MOVE    406             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED12_HH"
                   MOVE    407             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED12_MM"
                   MOVE    408             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST21_HH"
                   MOVE    409             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST21_MM"
                   MOVE    410             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED21_HH"
                   MOVE    411             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED21_MM"
                   MOVE    412             TO  SPA-GMN-CUR
               WHEN    "SYUKARIST22_HH"
                   MOVE    413              TO  SPA-GMN-CUR
               WHEN    "SYUKARIST22_MM"
                   MOVE    414              TO  SPA-GMN-CUR
               WHEN    "SYUKARIED22_HH"
                   MOVE    415             TO  SPA-GMN-CUR
               WHEN    "SYUKARIED22_MM"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN    "MSG1_DSPKBN"
                   MOVE    71              TO  SPA-GMN-CUR
               WHEN    "MSG2_DSPKBN"
                   MOVE    72              TO  SPA-GMN-CUR
               WHEN    "MSG3_DSPKBN"
                   MOVE    73              TO  SPA-GMN-CUR
               WHEN    "MSG4_DSPKBN"
                   MOVE    74              TO  SPA-GMN-CUR
               WHEN    "SRYKA_DSPKBN"
                   MOVE    75              TO  SPA-GMN-CUR
               WHEN    "SYOKBN_DSPKBN"
                   MOVE    76              TO  SPA-GMN-CUR
               WHEN    "SJKBN"
                   MOVE    99              TO  SPA-GMN-CUR
      *
               WHEN    "SRYKBN_FLG"
                   MOVE    101             TO  SPA-GMN-CUR
               WHEN    "SRYKA_FLG"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
