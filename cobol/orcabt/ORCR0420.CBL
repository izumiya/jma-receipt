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
       PROGRAM-ID.             ORCR0420.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      診療区分別点数集計（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/06/18    MCC−門脇　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    02.09.12  自賠責対応　　　　
      *  01.00.02    NACL-藤原    02.09.20  検査の編集のために回数を
      *                                     全て編集する
      *  01.00.03    NACL-門脇    02.10.02  摘要出力項目変更の為の編集
      *  01.00.04    NACL-門脇    02.11.06  約束処方時の点数欄表示修正
      *  01.00.05    NACL-門脇    02.11.21  自賠責（ポリネック等の対応）
      *  01.00.06    NACL-門脇    02.11.26  検診コードを編集できるよう修正
      *  01.00.07    NACL-藤原    02.12.02  酸素の時単価を表示するように追加
      *  01.00.08    NACL-藤原    03.01.22  データ区分の追加
      *  01.00.09    NACL-門脇    03.02.06  コメントカウントフラグの編集追加
      *  01.00.10    NACL-門脇    03.03.10  診療科１０から２０へ
      *  01.00.11    NACL-藤原    03.04.09  特定器材（画像診断のフィルム以外）
      *                                     の単価を表示するように修正
      *  01.00.12    NACL-門脇    03.04.14  自賠責の小数点以下金額計算
      *                                     四捨五入に変更
      *  01.00.13    NACL-門脇    03.05.08  名称有効桁のセット
      *  01.00.14    NACL-門脇    03.05.13  自賠責：腰部固定帯加算対応
      *  01.00.15    NACL-門脇    03.05.20  自賠責：療養担当手当て対応
      *  01.00.16    NACL-門脇    03.05.22  残量廃棄の数量セット
      *  01.00.17    NACL-門脇    03.06.03  外来診療料対応
      *                                     （平成１５年６月以降）
      *  01.00.18    NACL-門脇    03.09.24  コメント編集フラグのセット
      *  01.00.19    NACL-門脇    03.09.29  包括対象検査情報のセット
      *                                     検査等実施判断情報のセット
      *                                     各種番号等情報のセット
      *  01.00.20    NACL-門脇    03.10.06  点数マスタを読み込まないよう
      *                                     修正
      *  01.00.21    NACL-門脇    04.01.06  診療行為コードの１桁目の判定
      *                                     修正
      *  01.00.22    NACL-門脇    04.01.21  労災の特定器材(09593)対応
      *  01.00.23    NACL-門脇    04.02.05  診療コード＊数量の累計編集
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.01  長期投薬加算（処方料、処方せん
      *                                     料）追加対応
      *  01.01.02    NACL-門脇    04.04.01  リハ言語聴覚療法（３）追加対応
      *  01.01.03    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料の
      *                                     コメント対応
      *  01.01.04    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.01.05    NACL-門脇    04.04.26  ユーザーコメント追加対応
      *  01.01.06    NACL-小豆沢  04.06.08  ＣＴ・ＭＲＩ（２回目以降）の
      *                                     摘要欄記載順修正
      *  01.01.07    NACL-門脇    04.06.11  皮下筋肉注射で、薬剤点・器材点
      *                                     が無いの場合の摘要欄記載対応
      *  01.01.08    NACL-門脇    04.07.28  投薬料の器材点の集計修正
      *  01.01.09    NACL-門脇    04.07.28  休業証明フラグ、休業期間終了月
      *                                     日の編集
      *  01.01.10    NACL-門脇    04.10.15  自賠責特定器材等消費税対応
      *  中止処理追加
      *  01.02.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.02.02    NACL-門脇    04/12/17  胸部固定帯加算、創外固定器
      *                                     使用加算を薬剤等へ
      *  01.02.03    NACL-門脇    05/01/18  酸素計算式コメント対応
      *  01.02.04    NACL-門脇    05/01/26  システム管理４００１に関する修正
      *  01.02.05    NACL-門脇    05/01/27  腰・胸部固定帯加算、創外固定器
      *                                     使用加算の技術・薬剤等の選択対応
      *  01.02.06    NACL-門脇    05/01/28  文書料のコメント対応
      *  01.02.07    NACL-門脇    05/02/28  （サ）記載対応
      *  01.02.08    NACL-門脇    05/03/03  特記事項の編集修正
      *  01.02.09    NACL-門脇    05/04/20  内服薬剤逓減編集対応
      *  01.02.10    NACL-門脇    05/05/19  点数マスタの単位文字数がゼロ
      *                                     の場合の編集
      *  01.02.11    NACL-門脇    05/07/15  自賠責従来様式対応
      *  01.02.12    NACL-門脇    05/08/22  リハビリ実施回数差し込み修正対応
      *  01.02.13    NACL-藤原    05/08/22  検体検査コメントの追加
      *  01.02.14    NACL-門脇    05/08/29  ０９５９３のコメント対応
      *  01.02.15    NACL-門脇    05/10/06  診区９５９６対応
      *  01.02.16    NACL-小豆沢  05/10/28  特記事項　先進、制超　対応
      *  01.02.17    NACL-門脇    05/11/01  （検選）等コメント対応
      *  01.02.18    NACL-門脇    05/11/11  コメントがある場合の編集修正
      *  01.02.19    NACL-門脇    05/11/15  傷病手当金、療養費同意書の自動
      *                                     記載フラグ編集修正
      *  01.02.20    NACL-門脇    06/01/05  アフターケア在宅料対応
      *  01.02.21    NACL-門脇    06/02/01  ０９９２０００で始まる投薬用
      *                                     コメントの編集対応
      *
      *  ２００６年４月改正対応
      *  02.09.01    NACL-門脇    06/04/05  累計トータル編集修正
      *  02.09.02    NACL-門脇    06/04/13  ２００６年４月改正対応
      *  02.09.03    NACL-藤原    06/04/14  きざみ値計算識別の追加
      *  02.09.04    NACL-門脇    06/04/20  外来迅速検体検査加算編集
      *  02.09.05    NACL-門脇    06/04/28  在宅編集修正
      *  02.09.06    NACL-門脇    06/06/09  労災（加算なし）を剤に含む
      *                                     場合の編集修正（自賠責）
      *  03.00.01    NACL-門脇    06/06/22  往診、在宅患者訪問診療料の
      *                                     コメント自動記載対応
      *  03.01.01    NACL-門脇    06/07/10  在支援コメント自動記載対応
      *  03.01.02    NACL-門脇    06/07/14  診療区分置き換えフラグの編集
      *  03.02.01    NACL-門脇    06/08/10  高齢者上限額経過措置対応
      *  03.02.02    NACL-門脇    06/09/08  フイルムの単価編集修正
      *  03.03.01    NACL-門脇    06/09/27  特記事項１６長２対応
      *  03.03.02    NACL-門脇    06/11/10  往診のコメント自動記載条件修正
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.04.02    NACL-門脇    07/01/23  注射加算の名称編集修正
      *  03.04.03    NACL-門脇    07/03/12  包括診療対応
      *  03.04.04    NACL-門脇    07/04/01  現物給付対応
      *  03.04.05    NACL-門脇    07/04/13  リハビリ対応
      *  03.04.06    NACL-門脇    07/05/11  労災・自賠責単価１０円未満対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/06/26  自賠責平成１９年４月改正様式対応
      *  03.05.04    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  03.05.05    NACL-門脇    07/07/26  レセに記載しないコメント対応
      *  03.05.06    NACL-門脇    07/09/13  皮下筋肉、静脈注射の記載変更対応
      *  03.05.07    NACL-門脇    07/09/18  画像診断の加算（データ区分：手技）
      *                                     の記載順変更対応
      *  03.01.01    NACL-門脇    07/10/24  自賠責特定器材等消費税対象外設定
      *                                     項目数拡張対応
      *  04.01.02    NACL-門脇    07/10/31  公務災害対応
      *  04.01.03    NACL-門脇    07/11/26  特定器材商品名記載対応
      *  04.01.04    NACL-門脇    07/11/30  他保険にて診察料算定済の名称置換
      *  04.01.05    NACL-門脇    08/02/12  処方料等カウント修正
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （特記事項追加対応：２０）
      *                                     （脳血管リハ（３）コード追加）
      *                                     （介達牽引発症日記載廃止）
      *                                     （初診（夜間・早朝等）加算追加）
      *                                     （診療区分６４：病理診断対応）
      *                                     （再就労：円変更対応）
      *                                     （リハ：専用コード追加）
      *                                     （在宅コード追加）
      *                                     （剤支援記載対象コード修正）
      *                                     （小児特定疾患カウンセリング料
      *                                       の自動記載対応）
      *                                     （在支病自動記載対応）
      *  04.02.02    NACL-門脇    08/06/20  在宅編集修正
      *  04.02.03    NACL-門脇    08/11/06  皮下筋肉、静脈注射の記載変更対応
      *
      *  04.03.01    NACL-門脇    08/07/08  診区１４のコメントのみの剤
      *                                     編集対応
      *  04.03.02    NACL-門脇    08/07/11  診断書明細書料の摘要欄記載対応
      *  04.03.03    NACL-門脇    08/07/14  仮レセ作成（院外処方含む）対応
      *                                     投薬剤点０（院外等）記載
      *  04.03.04    NACL-門脇    08/09/25  自賠責０９５９３の集計先対応
      *  04.03.05    NACL-門脇    08/12/01  在宅（院外分）編集修正
      *  04.03.06    NACL-門脇    08/12/16  平成２１年１月改正対応
      *  04.03.07    NACL-門脇    09/01/16  処方欄金額編集修正
      *
      *  04.04.01    NACL-門脇    08/12/25  包括対応
      *  04.04.02    NACL-門脇    09/02/04  検査のコメント自動記載対応
      *  04.04.03    NACL-門脇    09/04/13  平成２１年５月改正対応
      *  04.04.04    NACL-門脇    09/11/19  自賠責健保準拠療養担当手当対応
      *  04.04.05    NACL-門脇    09/12/04  剤まとめ項目値の計算修正
      *  04.04.06    NACL-門脇    09/12/09  出来高算定（剤）の対応
      *  04.04.07    NACL-門脇    10/01/13  剤まとめ項目値の計算修正
      *  04.04.08    NACL-門脇    10/01/29  内服薬剤逓減編集対応
      *  04.04.09    NACL-門脇    10/03/12  内服薬剤逓減編集対応
      *  04.04.10    NACL-門脇    10/03/23  同日電話再診の回数記載対応
      *  04.04.11    NACL-門脇    10/03/25  改正対応
      *                                     （地域医療貢献加算記載編集）
      *                                     （明細書発行体制等加算記載編集）
      *  04.04.12    NACL-門脇    10/03/25  改正対応
      *                                     （予約コード読み飛ばし編集）
      *  04.04.13    NACL-門脇    10/03/29  改正対応
      *                                     （画像診断２回目以降減算編集）
      *                                     （抗悪性腫瘍剤処方管理加算編集）
      *                                     （コード追加）
      *  04.04.14    NACL-門脇    10/03/31  改正対応
      *                                     （頸部固定帯加算の対応）
      *  04.04.15    NACL-門脇    10/04/06  改正対応
      *                                     （コメント自動記載）
      *  04.04.16    NACL-門脇    10/08/06  アフターケア再診欄回数記載修正
      *  04.04.17    NACL-門脇    10/12/10  剤まとめ項目値の計算修正
      *
      *  04.05.01    NACL-門脇    09/06/29  療養期間記載方法選択機能
      *                                     （休業証明請求時）
      *  04.05.02    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.03    NACL-門脇    09/08/17  小数点以下５桁対応
      *  04.05.04    NACL-門脇    09/09/24  レセプト記載しないコメント対応
      *  04.05.05    NACL-門脇    09/11/19  薬剤算定なし時の摘要欄同一剤
      *                                     まとめ記載対応
      *  04.05.06    NACL-門脇    10/01/21  治験対応
      *  04.05.07    NACL-門脇    11/06/21  画像診断・電子画像管理加算の
      *                                     表用番号編集
      *  04.05.08    NACL-門脇    11/12/16  器材の金額と単位の対応
      *  04.05.09    NACL-門脇    12/02/13  回数欄編集修正（処方、麻毒、調基）
      *  04.05.10    NACL-門脇    12/04/02  平成２４年４月改正
      *                                     「再診」の対応
      *                                     「在宅」の対応
      *                                     「処置」の対応
      *                                     「画像診断」の対応
      *                                     「その他」の対応
      *                                     労災再診（同一日２科目）対応
      *                                     コメント差込
      *  04.05.11    NACL-門脇    12/05/01  平成２４年４月改正
      *                                     自賠責
      *                                     再診の点数集計と回数編集対応
      *  04.05.12    NACL-門脇    12/05/23  再就労療養指導管理料の集計先変更
      *  04.05.13    NACL-門脇    12/05/23  保険欄−一部負担金未記載対応
      *  04.05.14    NACL-門脇    12/08/10  ８０その他欄の点数集計修正
      *
      *  04.06.01    NACL-門脇    10/07/01  自賠責文書料レセプト摘要欄記載
      *                                     （１通時）対応
      *  04.06.02    NACL-門脇    10/07/02  小児特定疾患カウンセリング料の
      *                                     レセプトコメント自動記載（重複時）
      *                                     対応
      *  04.06.03    NACL-門脇    10/12/06  労災・自賠責の金額フリー入力
      *                                     コード対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE444-FILE    ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE444-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE4A-FILE     ASSIGN  RECE4APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4A-KEY
                                   FILE    STATUS  IS  STS-RECE4A.
           SELECT  RECE4A4-FILE    ASSIGN  RECE4APARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4A4-KEY
                                   FILE    STATUS  IS  STS-RECE4A.
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE455-FILE    ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE455-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE4B-FILE     ASSIGN  RECE4BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE4B-KEY
                                   FILE    STATUS  IS  STS-RECE4B.
           SELECT  RECE4B5-FILE    ASSIGN  RECE4BPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE4B5-KEY
                                   FILE    STATUS  IS  STS-RECE4B.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC. 
           COPY    "CPRCF044.INC".
      *
       FD  RECE444-FILE.
       01  RECE444-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE444//.
      *
       FD  RECE4A-FILE.
       01  RECE4A-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE4A//.
      *
       FD  RECE4A4-FILE.
       01  RECE4A4-REC. 
           COPY    "CPRCF044.INC"  REPLACING  //RECE44//
                                   BY         //RECE4A4//.
      *
       FD  RECE45-FILE.
       01  RECE45-REC. 
           COPY    "CPRCF045.INC".
      *
       FD  RECE455-FILE.
       01  RECE455-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE455//.
      *
       FD  RECE4B-FILE.
       01  RECE4B-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE4B//.
      *
       FD  RECE4B5-FILE.
       01  RECE4B5-REC. 
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //RECE4B5//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE4A//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE4B//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41                              PIC X(02).
           03  STS-RECE44                              PIC X(02).
           03  STS-RECE45                              PIC X(02).
           03  STS-RECE4A                              PIC X(02).
           03  STS-RECE4B                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-DAY                                 PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-FORMAL-KISAI                        PIC 9(01).
           03  FLG-RECE45                              PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-OSIN-COM                            PIC 9(01).
           03  FLG-ZAIHOU-COM                          PIC 9(01).
           03  FLG-SYOUNI-COM                          PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-SYOBYO-COM                          PIC 9(01).
           03  FLG-RYOYO-COM                           PIC 9(01).
           03  FLG-SYODOU-COM                          PIC 9(01).
           03  FLG-SAIDOU-COM                          PIC 9(01).
           03  FLG-NIKOCHIN-COM                        PIC 9(01).
           03  FLG-ZENSOKU-COM                         PIC 9(01).
           03  FLG-KENSA-COM                           PIC 9(01).
           03  FLG-TAIGAI                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-TAISYO                              PIC 9(01).
           03  FLG-SRYACCTPLUS                         PIC 9(01).
           03  FLG-WRITE45                             PIC 9(01).
           03  FLG-WRITE45X                            PIC 9(01).
           03  FLG-TAHOKEN                             PIC 9(01).
           03  FLG-SRYKBN-HEN                          PIC 9(01).
           03  FLG-COUNT-ARI                           PIC 9(01).
      *
      *    基本点数の非表示判定用フラグ
       01  FLG-KHNTEN-AREA.
           03  FLG-KHNTEN2                             PIC 9(01).
           03  FLG-KHNTEN3                             PIC 9(01).
           03  FLG-KHNTEN4                             PIC 9(01).
           03  FLG-KHNTEN5                             PIC 9(01).
           03  FLG-KHNTEN6                             PIC 9(01).
           03  FLG-KHNTEN15                            PIC 9(01).
           03  FLG-KHNTEN18                            PIC 9(01).
           03  FLG-KHNTEN19                            PIC 9(01).
      *
      *    基本点数の非表示判定用フラグ（自賠責用）
       01  FLG-KHNTEN-JI-AREA.                                          
           03  FLG-KHNTEN10-JI                         PIC 9(01).       
           03  FLG-KHNTEN13-JI                         PIC 9(01).       
           03  FLG-KHNTEN14-JI                         PIC 9(01).       
      *
      *    基本点数の非表示判定用フラグ（自賠責用：従来様式用）
       01  FLG-KHNTEN-JI2-AREA.                                          
           03  FLG-KHNTEN2-JI2                         PIC 9(01).
           03  FLG-KHNTEN3-JI2                         PIC 9(01).
           03  FLG-KHNTEN4-JI2                         PIC 9(01).
           03  FLG-KHNTEN5-JI2                         PIC 9(01).
           03  FLG-KHNTEN6-JI2                         PIC 9(01).
           03  FLG-KHNTEN11-JI2                        PIC 9(01).       
           03  FLG-KHNTEN14-JI2                        PIC 9(01).       
           03  FLG-KHNTEN15-JI2                        PIC 9(01).       
      *
      *    基本点数の非表示判定用フラグ（公務災害）
       01  FLG-KHNTEN-KO-AREA.
           03  FLG-KHNTEN2-KO                          PIC 9(01).
           03  FLG-KHNTEN3-KO                          PIC 9(01).
           03  FLG-KHNTEN4-KO                          PIC 9(01).
           03  FLG-KHNTEN5-KO                          PIC 9(01).
           03  FLG-KHNTEN6-KO                          PIC 9(01).
           03  FLG-KHNTEN7-KO                          PIC 9(01).
           03  FLG-KHNTEN8-KO                          PIC 9(01).
           03  FLG-KHNTEN17-KO                         PIC 9(01).
           03  FLG-KHNTEN19-KO                         PIC 9(01).
           03  FLG-KHNTEN21-KO                         PIC 9(01).
           03  FLG-KHNTEN22-KO                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-OUT3                                PIC 9(06).
           03  CNT-OUT4                                PIC 9(06).
           03  CNT-OUT5                                PIC 9(06).
           03  CNT-NYUYOJI                             PIC 9(02).
           03  CNT-SAISIN-DOUJITU                      PIC 9(02).
           03  CNT-SAISIN-ETC                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX5                                    PIC 9(03).
           03  IDXA                                    PIC 9(01).
           03  IDXB                                    PIC 9(03).
           03  ID1                                     PIC 9(03). 
           03  IDY                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW3                                    PIC 9(03).
           03  IDW4                                    PIC 9(03).
           03  IDW5                                    PIC 9(03).
           03  IDW6                                    PIC 9(03).
           03  IDV                                     PIC 9(03).
           03  IDU                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-IDX2                             PIC 9(03).
           03  FUKA-IDX                                PIC 9(03).
      *
      *    剤情報退避領域
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *    施設基準情報退避用領域
       01  TBL-SYS1006-T.
           03  TBL-SYS1006-OCC                         OCCURS  100.
               05  TBL-SYS10062                        PIC X(610).
               05  TBL-SYS10067                        PIC X(610).
      *
      *    点数テーブル検索用対象日領域
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
      *    点数マスタ情報退避領域
       01  WRK-TNS-TAIHI-AREA.
           03  WRK-TNS-TAIHI-TABLE                     OCCURS  5.
               05  WRK-TNS-CHUKSNTUBAN                 PIC X(01).
               05  WRK-TNS-DATAKBN                     PIC 9(01).
               05  WRK-TNS-TIMEKSNKBN                  PIC 9(01).
               05  WRK-TNS-TEN                         PIC 9(09)V99.    
               05  WRK-TNS-GAITENTTLKBN                PIC 9(03).
               05  WRK-TNS-OSINKBN                     PIC 9(01).
               05  WRK-TNS-KNSJISKBN                   PIC 9(01).
               05  WRK-TNS-SRYCD                       PIC X(09).
               05  WRK-TNS-JIBAISYUKEIKBN              PIC 9(01).
      *
      *    手技点・薬剤等点数用領域
       01  TBL-TENSUG-JI.
           03  TBL-TENSU-JI                            OCCURS   50.
               05  TBL-SYUTEN-JI                       PIC 9(09).
               05  TBL-YKZTEN-JI                       PIC 9(09).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-RECEKBN                    PIC X(01).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC X(01).
           03  WRK-PARA-PRTKBN                         PIC X(01).
           03  WRK-PARA-HOSPNUM                        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-KEISAN                              PIC 9(09).
           03  WRK-TEN                                 PIC 9(09).
           03  WRK-MEISAI-CNT                          PIC 9(04).
           03  WRK-SRYCDTOTAL                          PIC 9(14).
           03  WRK-SURYOUTOTAL                         PIC 9(07)V9(05).
           03  WRK-KSNRATE                             PIC 9V99.    
           03  WRK-SRYCD                               PIC 9(09).   
           03  WRK-YOUBUKASAN                          PIC 9(07).
           03  WRK-KYOBUKASAN                          PIC 9(07).
           03  WRK-KEIBUKASAN                          PIC 9(07).
           03  WRK-SOGAIKASAN                          PIC 9(07).
           03  WRK-RYOYOTEATE                          PIC 9(07).
           03  WRK-RYOYOTEATE2                         PIC 9(07).
      *
           03  WRK-KYUGYOFLG                           PIC X(01).
      *
           03  WRK-DAY                                 PIC 9(02).
      *
           03  WRK-ZAIKAISU-XXX                        PIC 9(03).
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(05).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-KZI058-TEN                          PIC 9(09)V99.
           03  WRK-KZI058-TANICD                       PIC 9(03).
           03  WRK-KZI058-TANIMOJISU                   PIC 9(01).
           03  WRK-KZI058-TANINAME                     PIC X(24).
           03  WRK-RENNUM                              PIC 9(02).
           03  WRK-RENNUM2                             PIC 9(02).
      *
           03  WRK-TEN-KZI                             PIC 9(09).
           03  WRK-TEN-BUN                             PIC 9(09).
      *
           03  WRK-SYUTEN-JI                           PIC 9(09).
           03  WRK-YKZTEN-JI                           PIC 9(09).
           03  WRK-KINGAKU-JI                          PIC 9(09).
      *
      *    消費税端数区分
           03  WRK-TAX1001-KBN                         PIC X(01).
      *
           03  WRK-SISETU613                           PIC 9(01).
           03  WRK-SISETU695                           PIC 9(01).
           03  WRK-SISETU3055                          PIC 9(01).
           03  WRK-SISETU3056                          PIC 9(01).
           03  WRK-SISETU3057                          PIC 9(01).
           03  WRK-SISETU3058                          PIC 9(01).
           03  WRK-SISETU3168                          PIC 9(01).
           03  WRK-SISETU3169                          PIC 9(01).
      *
      *    数字半角全角変換用     
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(20).
               05  WRK-CHAR-N                  PIC X(40).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *     
      *    単価編集用     
           03  WRK-TANKA-9                     PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA-9.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      ***  03  WRK-FUKA-KINGAKU                PIC 9(09).
           03  WRK-FUKA-KINGAKU                PIC 9(13)V9(05).
           03  WRK-FUKA-VALUE                  PIC 9(01).
      *
           03  WRK-ZAITENNASHI                 PIC 9(09).
      *
           03  WRK-SURYOU-COUNT                PIC 9(09).
      *
      *---------------------------------------------------------------
      *  回数減算用退避領域
      *
      *    特定疾患処方管理加算（処方料）
           03  WRK-TOKUTEI-SKAN                        PIC 9(03).
      *    処方料（乳幼児）加算
           03  WRK-NYUYOUJI-SKAN                       PIC 9(03).
      *    特定疾患処方管理加算（処方せん料）
           03  WRK-TOKUTEI-SKAN2                       PIC 9(03).
      *    処方せん（乳幼児）加算
           03  WRK-NYUYOUJI-SHOKAN                     PIC 9(03).
      *    処方料（麻・向・毒・覚）
           03  WRK-MADOKU-SHOHO                        PIC 9(03).
      *    長期投薬加算（処方料）
           03  WRK-CYOKI-SKAN                          PIC 9(03).
      *    長期投薬加算（処方せん料）
           03  WRK-CYOKI-SKAN2                         PIC 9(03).
      *    抗悪性腫瘍剤処方管理加算（処方料）
           03  WRK-AKUSEI-SKAN                         PIC 9(03).
      *    抗悪性腫瘍剤処方管理加算（処方せん料）
           03  WRK-AKUSEI-SKAN2                        PIC 9(03).
      *    一般名処方加算（処方せん料）
           03  WRK-IPPAN-SKAN2                         PIC 9(03).
      *---------------------------------------------------------------
      *
      *    固定値
       01  CONST-AREA.
      *    継続管理加算
           03  CONS-KEIZOKU1   PIC X(09)     VALUE "112007170".
           03  CONS-KEIZOKU2   PIC X(09)     VALUE "112702270".
      *    外来診療料（１回目）
           03  CONS-GAIRAIRYO1 PIC X(09)     VALUE "112011310".
           03  CONS-GAIRAIRYO2 PIC X(09)     VALUE "112705510".
           03  CONS-GAIRAIRYO3 PIC X(09)     VALUE "112011710".
      *    処方せん料
           03  CONS-SHOHOSEN1 PIC X(09)      VALUE "120002710".
           03  CONS-SHOHOSEN2 PIC X(09)      VALUE "120002810".
           03  CONS-SHOHOSEN3 PIC X(09)      VALUE "120002910".
           03  CONS-SHOHOSEN4 PIC X(09)      VALUE "120003010".
      *    処方せん（乳幼児）加算
           03  CONS-SHOHOKSN1  PIC X(09)     VALUE "120002470".
           03  CONS-SHOHOKSN2  PIC X(09)     VALUE "120002570".
      *    長期投薬加算（処方せん料）
           03  CONS-SHOHOKSN3  PIC X(09)     VALUE "120003270".
      *    抗悪性腫瘍剤処方管理加算（処方せん料）
           03  CONS-SHOHOKSN4  PIC X(09)     VALUE "120003470".
      *    一般名処方加算（処方せん料）
           03  CONS-SHOHOKSN5  PIC X(09)     VALUE "120003570".
      *    薬剤料逓減（９０／１００）（内服薬）
           03  CONS-YAKTEIGEN  PIC X(09)     VALUE "630010002".
      *    粉砕（コメント）
           03  CONS-FUNSAI-COMMENT PIC X(09) VALUE "099209901".
      *    採血（Ｂ−Ａ）
           03  CONS-SAIKETSU1      PIC X(09) VALUE "160101210".
      *    採血（Ｂ−Ｖ）
           03  CONS-SAIKETSU2      PIC X(09) VALUE "160095710".
      *    採血（Ｂ−Ｃ）
           03  CONS-SAIKETSU3      PIC X(09) VALUE "160095810".
      *    乳幼児加算
           03  CONS-NYUKSN1        PIC X(09) VALUE "111000370".         
           03  CONS-NYUKSN2        PIC X(09) VALUE "111011470".
           03  CONS-NYUKSN3        PIC X(09) VALUE "111011970".
           03  CONS-NYUKSN4        PIC X(09) VALUE "111012070".
           03  CONS-NYUKSN5        PIC X(09) VALUE "111012170".
           03  CONS-NYUKSN6        PIC X(09) VALUE "111012270".
           03  CONS-NYUKSN7        PIC X(09) VALUE "111011570".
           03  CONS-NYUKSN8        PIC X(09) VALUE "111011670".
           03  CONS-NYUKSN9        PIC X(09) VALUE "111011770".
      *    再診（３月超）（５回超）など
      *    （４６５に関係あり）
           03  CONS-SAISHIN-A1     PIC X(09) VALUE "101120060".
      **   03  CONS-SAISHIN-A2     PIC X(09) VALUE "112001210".
      **   03  CONS-SAISHIN-A3     PIC X(09) VALUE "112701110".
      **   03  CONS-SAISHIN-A4     PIC X(09) VALUE "112000970".
      **   03  CONS-SAISHIN-A5     PIC X(09) VALUE "112001070".
      **   03  CONS-SAISHIN-A6     PIC X(09) VALUE "112006970".
      **   03  CONS-SAISHIN-A7     PIC X(09) VALUE "112007070".
      **   03  CONS-SAISHIN-A8     PIC X(09) VALUE "112001310".
      **   03  CONS-SAISHIN-A9     PIC X(09) VALUE "112701210".
      **   03  CONS-SAISHIN-A10    PIC X(09) VALUE "112001110".
      **   03  CONS-SAISHIN-A11    PIC X(09) VALUE "112701010".
      **   03  CONS-SAISHIN-A12    PIC X(09) VALUE "112001410".
      **   03  CONS-SAISHIN-A13    PIC X(09) VALUE "112701310".
      *    外来管理加算（３月超）（５回超）
      *    （４６０に関係あり）
           03  CONS-GAIRAI         PIC X(09) VALUE "101120090".
      *    労災自賠責関係
           03  CONS-ROUSAI1    PIC X(09)   VALUE   "101110010".
           03  CONS-ROUSAI2    PIC X(09)   VALUE   "101110020".
           03  CONS-ROUSAI3    PIC X(09)   VALUE   "101110030".
           03  CONS-ROUSAI4    PIC X(09)   VALUE   "101120030".
           03  CONS-ROUSAI5    PIC X(09)   VALUE   "101120040".
           03  CONS-ROUSAI6    PIC X(09)   VALUE   "101120050".
           03  CONS-ROUSAI7    PIC X(09)   VALUE   "101120060".
           03  CONS-ROUSAI8    PIC X(09)   VALUE   "101130010".
           03  CONS-ROUSAI9    PIC X(09)   VALUE   "101130020".
           03  CONS-ROUSAI10   PIC X(09)   VALUE   "101130030".
           03  CONS-ROUSAI11   PIC X(09)   VALUE   "101130040".
           03  CONS-ROUSAI12   PIC X(09)   VALUE   "101130050".
           03  CONS-ROUSAI13   PIC X(09)   VALUE   "101130060".
           03  CONS-ROUSAI14   PIC X(09)   VALUE   "101130070".
           03  CONS-ROUSAI15   PIC X(09)   VALUE   "101130080".
           03  CONS-ROUSAI16   PIC X(09)   VALUE   "101130090".
           03  CONS-ROUSAI17   PIC X(09)   VALUE   "101130100".
           03  CONS-ROUSAI18   PIC X(09)   VALUE   "101110040".
           03  CONS-ROUSAI19   PIC X(09)   VALUE   "101120100".
      *
           03  CONS-ROUCOM1    PIC X(09)   VALUE   "840800001".
           03  CONS-ROUCOM2    PIC X(09)   VALUE   "840800002". 
      *
      *    腰部固定帯加算
           03  CONS-YOUBUKASAN PIC X(09)   VALUE   "140037490".
      *    胸部固定帯加算
           03  CONS-KYOBUKASAN PIC X(09)   VALUE   "140040110".
      *    頸部固定帯加算
           03  CONS-KEIBUKASAN PIC X(09)   VALUE   "140052610".
      *    創外固定器使用加算
           03  CONS-SOGAIKASAN PIC X(09)   VALUE   "150266970".
      *    療養担当手当て
           03  CONS-RYOYOTEATE PIC X(09)   VALUE   "199000610".
      *
      *    フィルム料（乳幼児）加算
           03  CONS-FILM-YOUJI-KASAN  
                               PIC X(09)   VALUE   "799990070".
      *    酸素補正率
           03  CONS-SANSO      PIC X(09)   VALUE   "770020070".
      *    高気圧酸素加算
           03  CONS-SANSO2     PIC X(09)   VALUE   "770030070".
      *    残量廃棄
           03  CONS-ZANHAIKI   PIC X(09)   VALUE   "099309901".
      *
      *
      *    慢性とう痛疾患
           03  CONS-MANSEI           PIC X(09)  VALUE  "113006510".
      *    傷病手当金意見書交付料
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    傷病手当金意見書交付年月日
           03  CONS-SYOBYO2-COMMENT  PIC X(09)  VALUE  "840000011".
      *    （相続）傷病手当金意見書交付年月日
           03  CONS-SYOBYO3-COMMENT  PIC X(09)  VALUE  "840000012".
      *    療養費同意書交付料
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
      *    療養費同意書交付年月日
           03  CONS-RYOYO2-COMMENT   PIC X(09)  VALUE  "840000078".
      *    訪問看護指示料
           03  CONS-HOUSIJI-COMMENT  PIC X(09)  VALUE  "114008010".
      *    訪問看護指示料算定日
           03  CONS-HOUSIJI2-COMMENT PIC X(09)  VALUE  "840000073".
      *    手術日コメント
           03  CONS-SHUJUTU-COMMENT  PIC X(09)  VALUE  "840000042".
      *    頭部単純ＣＴ撮影
           03  CONS-CTMRI-COMMENT1   PIC X(09)  VALUE  "170011710".
      *    躯幹単純ＣＴ撮影
           03  CONS-CTMRI-COMMENT2   PIC X(09)  VALUE  "170011810".
      *    四肢単純ＣＴ撮影
           03  CONS-CTMRI-COMMENT3   PIC X(09)  VALUE  "170011910".
      *    脳槽ＣＴ造影
           03  CONS-CTMRI-COMMENT4   PIC X(09)  VALUE  "170012110".
      *    頸部単純ＣＴ撮影
           03  CONS-CTMRI-COMMENT5   PIC X(09)  VALUE  "170015550".
      *    頭部単純ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT6   PIC X(09)  VALUE  "170015210".
      *    躯幹単純ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT7   PIC X(09)  VALUE  "170020110".
      *    四肢単純ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT8   PIC X(09)  VALUE  "170022110".
      *    ＣＴ、ＭＲＩ（頭部）（２回目以降）
           03  CONS-CTMRI-COMMENT9   PIC X(09)  VALUE  "170022290".
      *    ＣＴ、ＭＲＩ（躯幹）（２回目以降）
           03  CONS-CTMRI-COMMENT10  PIC X(09)  VALUE  "170022390".
      *    ＣＴ、ＭＲＩ（四肢）（２回目以降）
           03  CONS-CTMRI-COMMENT11  PIC X(09)  VALUE  "170022490".
      *    頭部特殊ＣＴ撮影
           03  CONS-CTMRI-COMMENT12  PIC X(09)  VALUE  "170023010".
      *    躯幹特殊ＣＴ撮影
           03  CONS-CTMRI-COMMENT13  PIC X(09)  VALUE  "170023110".
      *    四肢特殊ＣＴ撮影
           03  CONS-CTMRI-COMMENT14  PIC X(09)  VALUE  "170023210".
      *    頭部特殊ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT15  PIC X(09)  VALUE  "170023310".
      *    躯幹特殊ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT16  PIC X(09)  VALUE  "170023410".
      *    四肢特殊ＭＲＩ撮影
           03  CONS-CTMRI-COMMENT17  PIC X(09)  VALUE  "170023510".
      *    ＣＴ撮影（１６列未満マルチスライス型機器）
           03  CONS-CTMRI-COMMENT18  PIC X(09)  VALUE  "170028610".
      *    ＣＴ撮影（６４列以上マルチスライス型機器）
           03  CONS-CTMRI-COMMENT19  PIC X(09)  VALUE  "170033410".
      *    ＭＲＩ撮影（３テスラ以上の機器）
           03  CONS-CTMRI-COMMENT20  PIC X(09)  VALUE  "170033510".
      *    在宅血液透析頻回指導管理料
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *    初診（同日複数診療科）
           03  CONS-SYODOU-COMMENT   PIC X(09)  VALUE  "111011810".
      *    複初算定診療科：
           03  CONS-SYODOU2-COMMENT  PIC X(09)  VALUE  "830000024".
      *    ニコチン依存症管理料（初回）
           03  CONS-NIKOCHIN-COMMENT PIC X(09)  VALUE  "113008310".
      *    ニコチン依存症管理料（２回目から４回目まで）
           03  CONS-NIKOCHIN2-COMMENT PIC X(09) VALUE  "113008410".
      *    ニコチン依存症管理料（５回目）
           03  CONS-NIKOCHIN3-COMMENT PIC X(09) VALUE  "113008510".
      *    初回算定　　月　日
           03  CONS-NIKOCHIN4-COMMENT PIC X(09) VALUE  "840000097".
      *    重度喘息患者治療管理加算（１月目）
           03  CONS-ZENSOKU-COMMENT  PIC X(09)  VALUE  "113008070".
      *    重度喘息患者治療管理加算（２月目以降６月目まで）
           03  CONS-ZENSOKU2-COMMENT PIC X(09)  VALUE  "113008170".
      *    第１回目治療管理　　月　日
           03  CONS-ZENSOKU3-COMMENT PIC X(09)  VALUE  "840000098".
      *    心大血管疾患リハビリテーション料（１）
           03  CONS-SINDAI1-COMMENT  PIC X(09)  VALUE  "180027410".
      *    心大血管疾患リハビリテーション料（２）
           03  CONS-SINDAI2-COMMENT  PIC X(09)  VALUE  "180027510".
      **   心大血管疾患リハビリテーション料（１）（１２０日超）
           03  CONS-SINDAI3-COMMENT  PIC X(09)  VALUE  "180029110".
      **   心大血管疾患リハビリテーション料（２）（１２０日超）
           03  CONS-SINDAI4-COMMENT  PIC X(09)  VALUE  "180029210".
      **   心大血管疾患リハビリテーション医学管理料（１）
           03  CONS-SINDAI5-COMMENT  PIC X(09)  VALUE  "180029910".
      **   心大血管疾患リハビリテーション医学管理料（２）
           03  CONS-SINDAI6-COMMENT  PIC X(09)  VALUE  "180030010".
      *!   心大血管疾患リハビリテーション料（１）
           03  CONS-SINDAI7-COMMENT  PIC X(09)  VALUE  "101800080".
      *!   心大血管疾患リハビリテーション料（２）
           03  CONS-SINDAI8-COMMENT  PIC X(09)  VALUE  "101800090".
      *    呼吸器リハビリテーション料（１）
           03  CONS-KOKYUKI1-COMMENT PIC X(09)  VALUE  "180028010".
      *    呼吸器リハビリテーション料（２）
           03  CONS-KOKYUKI2-COMMENT PIC X(09)  VALUE  "180028110".
      **   呼吸器リハビリテーション料（１）（８０日超）
           03  CONS-KOKYUKI3-COMMENT PIC X(09)  VALUE  "180029710".
      **   呼吸器リハビリテーション料（２）（８０日超）
           03  CONS-KOKYUKI4-COMMENT PIC X(09)  VALUE  "180029810".
      **   呼吸器リハビリテーション医学管理料（１）
           03  CONS-KOKYUKI5-COMMENT PIC X(09)  VALUE  "180030510".
      **   呼吸器リハビリテーション医学管理料（２）
           03  CONS-KOKYUKI6-COMMENT PIC X(09)  VALUE  "180030610".
      *!   呼吸器リハビリテーション料（１）
           03  CONS-KOKYUKI7-COMMENT PIC X(09)  VALUE  "101800150".
      *!   呼吸器リハビリテーション料（２）
           03  CONS-KOKYUKI8-COMMENT PIC X(09)  VALUE  "101800160".
      *    脳血管疾患等リハビリテーション料（１）
           03  CONS-NOUKE1-COMMENT   PIC X(09)  VALUE  "180027610".
      *    脳血管疾患等リハビリテーション料（２）
           03  CONS-NOUKE2-COMMENT   PIC X(09)  VALUE  "180027710".
      **   脳血管疾患等リハビリテーション料（１）（１４０日超）
           03  CONS-NOUKE3-COMMENT   PIC X(09)  VALUE  "180029310".
      **   脳血管疾患等リハビリテーション料（２）（１４０日超）
           03  CONS-NOUKE4-COMMENT   PIC X(09)  VALUE  "180029410".
      **   脳血管疾患等リハビリテーション医学管理料（１）
           03  CONS-NOUKE5-COMMENT   PIC X(09)  VALUE  "180030110".
      **   脳血管疾患等リハビリテーション医学管理料（２）
           03  CONS-NOUKE6-COMMENT   PIC X(09)  VALUE  "180030210".
      *    脳血管疾患等リハビリテーション料（３）
           03  CONS-NOUKE7-COMMENT   PIC X(09)  VALUE  "180030810".
      *!   脳血管疾患等リハビリテーション料（１）
           03  CONS-NOUKE8-COMMENT   PIC X(09)  VALUE  "101800100".
      *!   脳血管疾患等リハビリテーション料（２）
           03  CONS-NOUKE9-COMMENT   PIC X(09)  VALUE  "101800110".
      *!   脳血管疾患等リハビリテーション料（３）
           03  CONS-NOUKE10-COMMENT  PIC X(09)  VALUE  "101800120".
      *    脳血管疾患等リハビリテーション料（１）（廃用症候群）
           03  CONS-NOUKE11-COMMENT  PIC X(09)  VALUE  "180032410".
      *    脳血管疾患等リハビリテーション料（２）（廃用症候群）
           03  CONS-NOUKE12-COMMENT  PIC X(09)  VALUE  "180032510".
      *    脳血管疾患等リハビリテーション料（３）（廃用症候群）
           03  CONS-NOUKE13-COMMENT  PIC X(09)  VALUE  "180032610".
      *    脳血管疾患等リハビリテーション料（１）（要介護・廃用症候群以外）
           03  CONS-NOUKE14-COMMENT  PIC X(09)  VALUE  "180033910".
      *    脳血管疾患等リハビリテーション料（１）（要介護・廃用症候群）
           03  CONS-NOUKE15-COMMENT  PIC X(09)  VALUE  "180034010".
      *    脳血管疾患等リハビリテーション料（２）（要介護・廃用症候群以外）
           03  CONS-NOUKE16-COMMENT  PIC X(09)  VALUE  "180034110".
      *    脳血管疾患等リハビリテーション料（２）（要介護・廃用症候群）
           03  CONS-NOUKE17-COMMENT  PIC X(09)  VALUE  "180034210".
      *    脳血管疾患等リハビリテーション料（３）（要介護・廃用症候群以外）
           03  CONS-NOUKE18-COMMENT  PIC X(09)  VALUE  "180034310".
      *    脳血管疾患等リハビリテーション料（３）（要介護・廃用症候群）
           03  CONS-NOUKE19-COMMENT  PIC X(09)  VALUE  "180034410".
      *    運動器リハビリテーション料（１）
           03  CONS-UNDOKI1-COMMENT  PIC X(09)  VALUE  "180027810".
      *    運動器リハビリテーション料（２）
           03  CONS-UNDOKI2-COMMENT  PIC X(09)  VALUE  "180027910".
      **   運動器リハビリテーション料（１）（１２０日超）
           03  CONS-UNDOKI3-COMMENT  PIC X(09)  VALUE  "180029510".
      **   運動器リハビリテーション料（２）（１２０日超）
           03  CONS-UNDOKI4-COMMENT  PIC X(09)  VALUE  "180029610".
      **   運動器リハビリテーション医学管理料（１）
           03  CONS-UNDOKI5-COMMENT  PIC X(09)  VALUE  "180030310".
      **   運動器リハビリテーション医学管理料（２）
           03  CONS-UNDOKI6-COMMENT  PIC X(09)  VALUE  "180030410".
      *!   運動器リハビリテーション料（１）
           03  CONS-UNDOKI7-COMMENT  PIC X(09)  VALUE  "101800130".
      *!   運動器リハビリテーション料（２）
           03  CONS-UNDOKI8-COMMENT  PIC X(09)  VALUE  "101800140".
      *    運動器リハビリテーション料（１）
           03  CONS-UNDOKI9-COMMENT  PIC X(09)  VALUE  "180032710".
      *!   運動器リハビリテーション料（１）
           03  CONS-UNDOKI10-COMMENT PIC X(09)  VALUE  "101800125".
      *    運動器リハビリテーション料（１）（要介護）
           03  CONS-UNDOKI11-COMMENT  PIC X(09)  VALUE  "180034510".
      *    運動器リハビリテーション料（２）（要介護）
           03  CONS-UNDOKI12-COMMENT  PIC X(09)  VALUE  "180034610".
      *    運動器リハビリテーション料（３）（要介護）
           03  CONS-UNDOKI13-COMMENT PIC X(09)  VALUE  "180034710".
      *    難病患者リハビリテーション料
           03  CONS-NANBYO-COMMENT   PIC X(09)  VALUE  "180017910".
      *    障害児（者）リハビリテーション料（６歳未満）
           03  CONS-SYOGAI1-COMMENT  PIC X(09)  VALUE  "180028210".
      *    障害児（者）リハビリテーション料（６歳以上１８歳未満）
           03  CONS-SYOGAI2-COMMENT  PIC X(09)  VALUE  "180028310".
      *    障害児（者）リハビリテーション料（１８歳以上）
           03  CONS-SYOGAI3-COMMENT  PIC X(09)  VALUE  "180028410".
      *    摂食機能療法
           03  CONS-SESSYOKU-COMMENT PIC X(09)  VALUE  "180016610".
      *    がん患者リハビリテーション料
           03  CONS-GANKNJ-COMMENT   PIC X(09)  VALUE  "180033110".
      *    介達牽引
           03  CONS-KAITATU-COMMENT  PIC X(09)  VALUE  "140048010".
      *    往診
           03  CONS-OSIN-COMMENT     PIC X(09)  VALUE  "114000110".
      *    特別往診
           03  CONS-OSIN2-COMMENT    PIC X(09)  VALUE  "114001610".
      *    往診日コメント
           03  CONS-OSIN3-COMMENT    PIC X(09)  VALUE  "840000039".
      *    在宅患者訪問診療料
           03  CONS-ZAIHOU-COMMENT   PIC X(09)  VALUE  "114001110".
      *    訪問診療日コメント
           03  CONS-ZAIHOU2-COMMENT  PIC X(09)  VALUE  "840000066".
      *    在宅患者訪問診療料（居住系）
           03  CONS-ZAIHOU3-COMMENT  PIC X(09)  VALUE  "114012910".
      *    在宅患者訪問診療料（同一建物居住者）（特定施設等入居者）
           03  CONS-ZAIHOU4-COMMENT  PIC X(09)  VALUE  "114018010".
      *
      *    在宅時医学総合管理料１（院外処方せん）
           03  CONS-ZAIIKAN1-COMMENT PIC X(09)  VALUE  "114012210".
      *    在宅時医学総合管理料１（院内処方せん）
           03  CONS-ZAIIKAN2-COMMENT PIC X(09)  VALUE  "114012310".
      *    在宅時医学総合管理料２（院内処方せん）
           03  CONS-ZAIIKAN3-COMMENT PIC X(09)  VALUE  "114012410".
      *    在宅時医学総合管理料２（院外処方せん）
           03  CONS-ZAIIKAN4-COMMENT PIC X(09)  VALUE  "114007510".
      *    特定施設入居時等医学総合管理料１（処方せんを交付）
           03  CONS-ZAIIKAN5-COMMENT PIC X(09)  VALUE  "114013010".
      *    特定施設入居時等医学総合管理料１（処方せんを交付しない）
           03  CONS-ZAIIKAN6-COMMENT PIC X(09)  VALUE  "114013110".
      *    特定施設入居時等医学総合管理料２（処方せんを交付）
           03  CONS-ZAIIKAN7-COMMENT PIC X(09)  VALUE  "114013210".
      *    特定施設入居時等医学総合管理料２（処方せんを交付しない）
           03  CONS-ZAIIKAN8-COMMENT PIC X(09)  VALUE  "114013310".
      *    在医総管（機能強化した在支診等）（病床あり）（処方せん）
           03  CONS-ZAIIKAN9-COMMENT  PIC X(09) VALUE  "114018710".
      *    在医総管（機能強化した在支診等）（病床あり）（処方せんなし）
           03  CONS-ZAIIKAN10-COMMENT PIC X(09) VALUE  "114018810".
      *    在医総管（機能強化した在支診等）（病床なし）（処方せん）
           03  CONS-ZAIIKAN11-COMMENT PIC X(09) VALUE  "114018910".
      *    在医総管（機能強化した在支診等）（病床なし）（処方せんなし）
           03  CONS-ZAIIKAN12-COMMENT PIC X(09) VALUE  "114019010".
      *    特医総管（機能強化した在支診等）（病床あり）（処方せん）
           03  CONS-ZAIIKAN13-COMMENT PIC X(09) VALUE  "114019110".
      *    特医総管（機能強化した在支診等）（病床あり）（処方せんなし）
           03  CONS-ZAIIKAN14-COMMENT PIC X(09) VALUE  "114019210".
      *    特医総管（機能強化した在支診等）（病床なし）（処方せん）
           03  CONS-ZAIIKAN15-COMMENT PIC X(09) VALUE  "114019310".
      *    特医総管（機能強化した在支診等）（病床なし）（処方せんなし）
           03  CONS-ZAIIKAN16-COMMENT PIC X(09) VALUE  "114019410".
      *
      *    小児特定疾患カウンセリング料（１回目）
           03  CONS-SYOUNI1-COMMENT  PIC X(09)  VALUE  "113000810".
      *    小児特定疾患カウンセリング料（２回目）
           03  CONS-SYOUNI2-COMMENT  PIC X(09)  VALUE  "113009910".
      *    小児特定第１回目カウンセリングコメント
           03  CONS-SYOUNI3-COMMENT  PIC X(09)  VALUE  "840000104".
      *
      *    ＢＮＰ精密
           03  CONS-KENSA1-COMMENT   PIC X(09)  VALUE  "160162350".
      *    ＮＴ−ｐｒｏＢＮＰ精密
           03  CONS-KENSA2-COMMENT   PIC X(09)  VALUE  "160181250".
      *    ＨＡＮＰ精密
           03  CONS-KENSA3-COMMENT   PIC X(09)  VALUE  "160116310".
      *
      *    心臓ペースメーカー指導管理料（遠隔モニタリング）
           03  CONS-SINPESU-COMMENT  PIC X(09)  VALUE  "113001510".
      *    集団栄養食事指導料
           03  CONS-SYUEIYO-COMMENT  PIC X(09)  VALUE  "113003410".
      *    人工腎臓（その他）
           03  CONS-JINZOU1-COMMENT  PIC X(09)  VALUE  "140007710".
      *    人工腎臓（慢性維持透析）（４時間未満）
           03  CONS-JINZOU2-COMMENT  PIC X(09)  VALUE  "140036710".
      *    人工腎臓（慢性維持透析）（４時間以上５時間未満）
           03  CONS-JINZOU3-COMMENT  PIC X(09)  VALUE  "140051010".
      *    人工腎臓（慢性維持透析）（５時間以上）
           03  CONS-JINZOU4-COMMENT  PIC X(09)  VALUE  "140051110".
      *    連続携行式腹膜かん流
           03  CONS-JINZOU5-COMMENT  PIC X(09)  VALUE  "140008510".
      *    持続緩徐式血液濾過
           03  CONS-JINZOU6-COMMENT  PIC X(09)  VALUE  "140029850".
      *    血漿交換療法
           03  CONS-JINZOU7-COMMENT  PIC X(09)  VALUE  "140008210".
      *    血球成分除去療法
           03  CONS-JINZOU8-COMMENT  PIC X(09)  VALUE  "140039210".
      *    人工腎臓（慢性維持透析濾過）（複雑）
           03  CONS-JINZOU9-COMMENT  PIC X(09)  VALUE  "140052810".
      *    局所陰圧閉鎖処置（その他）
           03  CONS-KYOKUIN1-COMMENT PIC X(09)  VALUE  "140052410".
      *    局所陰圧閉鎖処置（被覆材貼付）（１００ｃｍ２未満）
           03  CONS-KYOKUIN2-COMMENT PIC X(09)  VALUE  "140051810".
      *    局所陰圧閉鎖処置（被覆材貼付）（１００ｃｍ２〜２００ｃｍ２未満）
           03  CONS-KYOKUIN3-COMMENT PIC X(09)  VALUE  "140051910".
      *    局所陰圧閉鎖処置（被覆材貼付）（２００ｃｍ２以上）
           03  CONS-KYOKUIN4-COMMENT PIC X(09)  VALUE  "140052010".
      *    局所陰圧閉鎖処置初回加算（１００ｃｍ２未満）
           03  CONS-KYOKUIN5-COMMENT PIC X(09)  VALUE  "140052170".
      *    局所陰圧閉鎖処置初回加算（１００ｃｍ２以上２００ｃｍ２未満）
           03  CONS-KYOKUIN6-COMMENT PIC X(09)  VALUE  "140052270".
      *    局所陰圧閉鎖処置初回加算（２００ｃｍ２以上）
           03  CONS-KYOKUIN7-COMMENT PIC X(09)  VALUE  "140052370".
      *    認知療法・認知行動療法
           03  CONS-NINRYO-COMMENT   PIC X(09)  VALUE  "180033210".
      *    認知療法・認知行動療法（精神科救急体制の精神保健指定医）
           03  CONS-NINRYO2-COMMENT  PIC X(09)  VALUE  "180035910".
      *
      *    認知症専門診断管理料２
           03  CONS-NINSEN-COMMENT   PIC X(09)  VALUE  "113014610".
      *    退院時共同指導料１（１以外）
           03  CONS-TAKYO-COMMENT    PIC X(09)  VALUE  "113008710".
      *    退院時共同指導料特別管理指導加算
           03  CONS-TAKYOKSN-COMMENT PIC X(09)  VALUE  "113014370".
      *    外来リハビリテーション診療料１
           03  CONS-GAIRIHA1-COMMENT PIC X(09)  VALUE  "113013910".
      *    外来リハビリテーション診療料２
           03  CONS-GAIRIHA2-COMMENT PIC X(09)  VALUE  "113014010".
      *    外来放射線照射診療料
           03  CONS-GAIHOU-COMMENT   PIC X(09)  VALUE  "113014110".
      *    介護職員等喀痰吸引等指示料
           03  CONS-KAISYOKU-COMMENT PIC X(09)  VALUE  "114020910".
      *
      *    外来迅速検体検査加算
           03  CONS-GAIJINKEN        PIC X(09)  VALUE  "160177770".
      *
      *    皮下筋肉注射
           03  CONS-HIKAKIN          PIC X(09)  VALUE  "130000510".
      *    生物学的製剤注射
           03  CONS-SEIBUTU          PIC X(09)  VALUE  "130000110".
      *    麻薬注射
           03  CONS-MAYAKU           PIC X(09)  VALUE  "130000310".
      *    静脈内注射
           03  CONS-JYOMYAKU         PIC X(09)  VALUE  "130003510".
      *    静脈内注射（乳幼児）加算
           03  CONS-JYOMYAKUKASAN    PIC X(09)  VALUE  "130003670".
      *
      *    在支援フラグ編集の為のコード
           03  CONS-ZAISIEN1         PIC X(09)  VALUE  "114011570".
           03  CONS-ZAISIEN2         PIC X(09)  VALUE  "114011670".
           03  CONS-ZAISIEN3         PIC X(09)  VALUE  "114011770".
      *
      *    再診料（同一日２科目）
           03  CONS-DOUJITU-SAI      PIC X(09)  VALUE  "112015810".
      *    電話等再診（同一日２科目）
           03  CONS-DOUJITU-DEN      PIC X(09)  VALUE  "112015950".
      *    外来診療料（同一日２科目）
           03  CONS-DOUJITU-GAI      PIC X(09)  VALUE  "112016210".
      *
      *    複再算定診療科：
           03  CONS-DOUJITU-COMMENT  PIC X(09)  VALUE  "830000051".
      *
      *    時間外対応加算１
           03  CONS-JIKANTAI1        PIC X(09)  VALUE  "112016070".
      *    時間外対応加算２
           03  CONS-JIKANTAI2        PIC X(09)  VALUE  "112015670".
      *    時間外対応加算３
           03  CONS-JIKANTAI3        PIC X(09)  VALUE  "112016170".
      *    明細書発行体制等加算
           03  CONS-MESAIKSN         PIC X(09)  VALUE  "112015770".
      *
      *    剤の診療内容チェックテーブル
       01  TBL-ZAICHECK-AREA.
      *    （サ）
           03  FILLER          PIC X(09)   VALUE    "820000046".
      *    （検選）
           03  FILLER          PIC X(09)   VALUE    "820000053".
      *    （リハ選）
           03  FILLER          PIC X(09)   VALUE    "820000054".
      *    （精選）
           03  FILLER          PIC X(09)   VALUE    "820000055".
      *
       01  TBL-ZAICHECK-R      REDEFINES   TBL-ZAICHECK-AREA.
           03  TBL-ZAICHECK    PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAICHECK-MAX    PIC 9(02)   VALUE     4.
      *
      *    酸素 
           COPY    "CMSANSOTBL.INC".
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
       01  WRK-RECE41G-REC. 
           COPY    "CPRCF041.INC"  REPLACING  //RECE41//
                                   BY         //WRK-RECE41G//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計付加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本
           COPY    "CPSK1001.INC".
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK4003.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE440"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
           MOVE    "RECE450"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    "RECE441"           TO  RECE4APARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4APARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE4APARA-HOSPNUM
           MOVE    "RECE451"           TO  RECE4BPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE4BPARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE4BPARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0420"      TO  JOB-PGID
               MOVE    "診療区分別点数集計（労災・自賠責）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報−基本
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
              MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO  SYS-1001-REC
                  MOVE    SYS-1001-TAX-HASU-KBN
                                          TO  WRK-TAX1001-KBN
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                              TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1                   TO  FLG-END
              END-IF
      *       施設基準情報
              MOVE     ZERO     TO     WRK-SISETU613
              MOVE     ZERO     TO     WRK-SISETU695
              MOVE     ZERO     TO     WRK-SISETU3055
              MOVE     ZERO     TO     WRK-SISETU3056
              MOVE     ZERO     TO     WRK-SISETU3057
              MOVE     ZERO     TO     WRK-SISETU3058
              MOVE     ZERO     TO     WRK-SISETU3168
              MOVE     ZERO     TO     WRK-SISETU3169
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 INITIALIZE                    SYS-1006-REC
                 MOVE    "1006"            TO  SYS-1006-KANRICD
                 MOVE    IDXA              TO  SYS-1006-KBNCD
                 MOVE    WRK-PARA-SRYYM    TO  SYS-1006-STYUKYMD(1:6)
                 MOVE    "01"              TO  SYS-1006-STYUKYMD(7:2)
                 MOVE    SYS-1006-STYUKYMD TO  SYS-1006-EDYUKYMD
grpsys           MOVE    SPA-HOSPNUM       TO  SYS-1006-HOSPNUM
                 MOVE    SYS-1006-REC      TO  MCPDATA-REC
                 PERFORM 800-SYSKANRI-READ-SEC
                 IF      FLG-SYSKANRI     =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1006-REC
                   EVALUATE  IDXA
                     WHEN     2
                       MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                       MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
                     WHEN     7
                       MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                       MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                       MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                       MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                       MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                       MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
                   END-EVALUATE
                 END-IF
               END-IF
              END-PERFORM
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM 810-SYSKANRI-READ-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
                    PERFORM 810-SYSKANRI-READ-SEC
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO  FLG-END
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              MOVE     SPACE              TO  TBL-SYS1006-T
              INITIALIZE                      TBL-SYS1006-T
      *
              PERFORM  VARYING  IDXA   FROM   1   BY  1
                        UNTIL   IDXA     >    7
               IF   IDXA  =  1 OR 3 OR 4 OR 5 OR 6
                 CONTINUE
               ELSE
                 MOVE     ZERO            TO  SYSKAN-IDX2
                 INITIALIZE                   SYS-1006-REC
                 MOVE    "1006"           TO  SYS-1006-KANRICD
                 MOVE    IDXA             TO  SYS-1006-KBNCD
grpsys           MOVE    SPA-HOSPNUM      TO  SYS-1006-HOSPNUM
                 MOVE    SYS-1006-REC     TO  MCPDATA-REC
                 MOVE    "tbl_syskanri"   TO  MCP-TABLE
                 MOVE    "key8"           TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC           =   ZERO
                     MOVE    "tbl_syskanri"   TO  MCP-TABLE
                     MOVE    "key8"           TO  MCP-PATHNAME
                     PERFORM 810-SYSKANRI-READ-SEC
                     PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                               OR    (SYSKAN-IDX2  >=  100)
      *
                       MOVE    MCPDATA-REC    TO  SYS-1006-REC
                       EVALUATE  IDXA
                         WHEN     2
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10062(SYSKAN-IDX2)
                         WHEN     7
                           ADD     1     TO  SYSKAN-IDX2
                           MOVE    SYS-1006-REC
                                         TO  TBL-SYS10067(SYSKAN-IDX2)
                       END-EVALUATE
      *
                       MOVE    "tbl_syskanri"   TO  MCP-TABLE
                       MOVE    "key8"           TO  MCP-PATHNAME
                       PERFORM 810-SYSKANRI-READ-SEC
                     END-PERFORM
                 END-IF
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key8"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
               END-IF
              END-PERFORM
      *
           END-IF
      *                             
      *    レセプト処理
           OPEN    OUTPUT              RECE444-FILE
           CLOSE                       RECE444-FILE
      *
           OPEN    OUTPUT              RECE455-FILE
           CLOSE                       RECE455-FILE
      *
           OPEN    OUTPUT              RECE4A4-FILE
           CLOSE                       RECE4A4-FILE
      *
           OPEN    OUTPUT              RECE4B5-FILE
           CLOSE                       RECE4B5-FILE
      *
           OPEN    OUTPUT              RECE44-FILE
      ***                              RECE45-FILE
           OPEN    I-O                 RECE45-FILE
      *
           OPEN    OUTPUT              RECE4A-FILE
                                       RECE4B-FILE
      
      *
           OPEN    I-O                 RECE41-FILE
      *
           PERFORM     900-RECE41-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *      
           PERFORM     900-RECE41-READ-SEC
           IF      CNT-IN-COMMIT   =   300
               MOVE  ZERO          TO      CNT-IN-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
      *
      *    処理中止設定処理
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                   WRK-RECE41G-REC
      *
           MOVE       ZERO       TO     WRK-ZAITENNASHI
      *
           MOVE       ZERO       TO     WRK-SURYOU-COUNT
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
      *
              INITIALIZE                   FLG-KHNTEN-AREA
              INITIALIZE                   FLG-KHNTEN-JI-AREA           
              INITIALIZE                   FLG-KHNTEN-JI2-AREA
              INITIALIZE                   FLG-KHNTEN-KO-AREA
              INITIALIZE                   TBL-TENSUG-JI
      *
              MOVE       ZERO       TO     CNT-NYUYOJI
      *
              MOVE       ZERO       TO     WRK-KINGAKU-JI
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2101-RECE44-HENSYU-SEC
              END-PERFORM
      *
              EVALUATE   RECE41-HKNKBN
                WHEN       "1"
                WHEN       "2"
                  PERFORM     2102-KHNTEN-CLEAR-SEC
                WHEN       "4"
                  PERFORM     2103-KHNTEN-JI-CLEAR-SEC                       
                WHEN       "5"
                  PERFORM     2106-KHNTEN-KO-CLEAR-SEC
              END-EVALUATE
      *
           ELSE
      *
              PERFORM    VARYING    IDX    FROM   1   BY   1
                         UNTIL     (IDX      >   20)  OR
                                   (RECE41-SRYKA(IDX)   =   SPACE)
                  PERFORM     2104-RECE44-HENSYU2-SEC
              END-PERFORM
      *
           END-IF
      *
           PERFORM     2107-GENTEN-HENSYU-SEC
      *
           PERFORM     2105-RECE41-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成
      *****************************************************************
       2101-RECE44-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key24"          TO      MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                 MOVE   ZERO          TO  FLG-RINYAK
      *
                 IF    ACCT-SRYKBN       =  "95"  OR  "96"
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  21015-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                        (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                        (ACCT-JIHOKBN       =    SPACE))
                   OR  ( FLG-TAISYO         =      1   )
                               MOVE    ZERO    TO      WRK-TEN
                                                       WRK-MEISAI-CNT
                                                       WRK-SRYCDTOTAL
                                                       WRK-SURYOUTOTAL
                                                       WRK-YOUBUKASAN
                                                       WRK-KYOBUKASAN
                                                       WRK-KEIBUKASAN
                                                       WRK-SOGAIKASAN
                                                       WRK-RYOYOTEATE
      *
                               MOVE    ZERO    TO      WRK-SRYSURYO
                                                       WRK-SRYSURYO2
                                                       WRK-SRYSURYO3
                                                       WRK-TANICD
                                                       WRK-TANIMOJISU
                               MOVE    SPACE   TO      WRK-TANINAME
                               MOVE    ZERO    TO      FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
                                                       FLG-SYOBYO-COM
                                                       FLG-RYOYO-COM
                                                       FLG-SYODOU-COM
                                                       FLG-SAIDOU-COM
                                                       FLG-NIKOCHIN-COM
                                                       FLG-ZENSOKU-COM
                                                       FLG-OSIN-COM
                                                       FLG-ZAIHOU-COM
                                                       FLG-TAHOKEN
                                                       FLG-SYOUNI-COM
                                                       FLG-SRYKBN-HEN
                                                       FLG-KENSA-COM
      *
                               MOVE    SPACE   TO      WRK-KYUGYOFLG
                               MOVE    ZERO    TO      FLG-COUNT-ARI
      *
                               MOVE    ZERO    TO
                                               WRK-KZI058-TEN
                                               WRK-KZI058-TANICD
                                               WRK-KZI058-TANIMOJISU
                               MOVE    SPACE   TO
                                               WRK-KZI058-TANINAME
      *
                               MOVE    ZERO    TO      FLG-WRITE45
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               IF    FLG-WRITE45  =   1
                                 PERFORM 21013-RECE44-WRITE-SEC
                               END-IF
                   ELSE
                             IF  (ACCT-ZAIKAISU  NOT =    ZERO)  AND
                                 (ACCT-JIHOKBN       =   SPACE)
                               IF ( ACCT-SRYKBN  =  "11" OR "12" OR
                                                    "99"          )
                               OR ((RECE41-SRYYM >= "200804"     ) AND
                                   (ACCT-SRYKBN  =  "14"         ))
                               OR ((RECE41-SRYYM >= "200804"     ) AND
                                   (ACCT-SRYKBN  =  "21" OR "22" OR
                                                    "23"         ) AND
                                   (WRK-PARA-PRTKBN  =  "2"      ))
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               ELSE
                                 PERFORM   21014-SINRYO-CHECK-SEC
                                 IF   FLG-RINYAK  NOT =  ZERO
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                             END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_sryacct"    TO      MCP-TABLE
                 MOVE    "key24"          TO      MCP-PATHNAME
                 PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2101-RECE44-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       21011-RECE44-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE44-REC
           INITIALIZE                           RECE44-REC
           MOVE    RECE41-HOSPNUM       TO      RECE44-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE44-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE44-PTID
           MOVE    RECE41-RECEKA        TO      RECE44-RECEKA
           MOVE    RECE41-HKNID         TO      RECE44-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE44-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE44-PTNUM 
           MOVE    ACCT-ZAINUM          TO      RECE44-ZAINUM
           MOVE    ACCT-SRYKBN          TO      RECE44-SRYKBN
           MOVE    ACCT-SRYKA           TO      RECE44-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE44-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE44-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE44-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE44-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE44-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE44-MEISAISU
           IF      RECE41-HKNKBN   NOT  =   "3"
              MOVE    ACCT-ZAIKAISU     TO      RECE44-ZAIKAISU
           ELSE
              MOVE    WRK-ZAIKAISU-XXX  TO      RECE44-ZAIKAISU
           END-IF
           MOVE    ACCT-SYUTEN1         TO      RECE44-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE44-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE44-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE44-KIZAITEN
           IF   RECE41-HKNKBN     =  "4"
              IF  ACCT-SRYKBN     =  "70"
                  COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                            -  RECE44-SYUTEN2
                  COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                            +  RECE44-SYUTEN2
              END-IF
           END-IF
           MOVE    ACCT-DRCD            TO      RECE44-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE44-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE44-TEIGENRATE
      *
           IF      RECE41-HKNKBN   NOT  =   "3"
               MOVE        ZERO             TO      WK-TAIHI-DAY
               MOVE        ZERO             TO      WK-TAIHI-FLG
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >       4
                   PERFORM     VARYING   IDX2   FROM    1    BY   1
                               UNTIL     IDX2   >       31
                       MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE44-KAISU (IDX3 IDX2)
                       IF     (WK-TAIHI-FLG            =   ZERO)   AND
                              (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                           MOVE     IDX2    TO      WK-TAIHI-DAY
                           MOVE     1       TO      WK-TAIHI-FLG
                       END-IF    
                   END-PERFORM
               END-PERFORM
           ELSE
               PERFORM     VARYING   IDX3   FROM    1    BY   1
                           UNTIL     IDX3   >       4
                  MOVE    ACCT-DAY(IDX3 WRK-DAY)  
                                       TO    RECE44-KAISU(IDX3 WRK-DAY)
              END-PERFORM                         
              MOVE    WRK-DAY          TO    WK-TAIHI-DAY
           END-IF
      *
           IF    FLG-RINYAK    NOT =    ZERO
                MOVE    FLG-RINYAK     TO    RECE44-SAFLG
           END-IF
      *
           .
       21011-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成
      *****************************************************************
       21012-RECE45-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO      SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "tbl_sryact"         TO      MCP-TABLE
           MOVE    "key2"               TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"     TO      MCP-TABLE
               MOVE    "key2"           TO      MCP-PATHNAME
               PERFORM  800-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                                     FLG-FORMAL-KISAI
                   MOVE   ZERO   TO  WRK-RENNUM
                                     WRK-RENNUM2
                   MOVE   ZERO   TO  FLG-WRITE45X
                   PERFORM   210121-RECE45-TENSOU-SEC
                   PERFORM   210122-RECE41-HENSYU-SEC
                   IF  FLG-WRITE45X  =  1
                      PERFORM  210123-RECE45-WRITE-SEC
                      MOVE  1    TO  FLG-WRITE45
                   END-IF
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  210124-RECE45-REWRITE-SEC
                   END-IF
                   IF  FLG-FORMAL-KISAI  =  1
                      PERFORM  210124-RECE45-REWRITE2-SEC
                   END-IF
                   MOVE     "tbl_sryact"   TO   MCP-TABLE
                   MOVE     "key2"         TO   MCP-PATHNAME
                   PERFORM   800-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryact"     TO      MCP-TABLE
           MOVE    "key2"           TO      MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       21012-RECE45-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       210121-RECE45-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO      RECE45-REC
           INITIALIZE                           RECE45-REC
           MOVE    RECE41-HOSPNUM       TO      RECE45-HOSPNUM
           MOVE    RECE41-SRYYM         TO      RECE45-SRYYM
           MOVE    RECE41-NYUGAIKBN     TO      RECE45-NYUGAIKBN
           MOVE    RECE41-PTID          TO      RECE45-PTID
           MOVE    RECE41-RECEKA        TO      RECE45-RECEKA
           MOVE    RECE41-HKNID         TO      RECE45-HKNID
           MOVE    RECE41-TEKSTYMD      TO      RECE45-TEKSTYMD
           MOVE    RECE41-PTNUM         TO      RECE45-PTNUM
           MOVE    SRY-ZAINUM           TO      RECE45-ZAINUM
           MOVE    SRY-RENNUM           TO      RECE45-RENNUM
           MOVE    SRY-SRYKA            TO      RECE45-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE45-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE45-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE45-JIHIMONEY
      * 
           MOVE    ZERO                 TO      FLG-OSINKBN
                                                IDX4
      *
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *
             PERFORM    800-TENSU-TBL-SEC
      **     点数マスタ情報退避
             MOVE    TNS-CHUKSNTUBAN    TO  WRK-TNS-CHUKSNTUBAN(IDX2)
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
      *
             IF     (RECE41-HKNKBN        =  "4"               )
             AND    (ACCT-SRYKBN          =  "80"              )
             AND    (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                     OR "09593")
               IF    SRY-SRYCD(IDX2)(1:5) =  "09593"
                 PERFORM   2101213-JIBAI-KIZAI-TENSU-HENSYU-SEC
                 MOVE   WRK-TEN-KZI     TO  WRK-TNS-TEN(IDX2)
               ELSE
                 PERFORM   2101214-JIBAI-BUNSYO-TENSU-HENSYU-SEC
                 MOVE   WRK-TEN-BUN     TO  WRK-TNS-TEN(IDX2)
               END-IF
             ELSE
               IF   (ACCT-SRYKBN          =  "80"              )
               AND  (SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                     OR "09593")
                 IF  SRY-SRYSYUKBN = "809"
                   MOVE SRY-JIHIMONEY(IDX2)
                                        TO  WRK-TNS-TEN(IDX2)
                 ELSE
                   MOVE TNS-TEN         TO  WRK-TNS-TEN(IDX2)
                 END-IF
               ELSE
                 MOVE   TNS-TEN         TO  WRK-TNS-TEN(IDX2)
               END-IF
             END-IF
      *
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE44-MEISAISU  =  RECE44-MEISAISU
                                             -  1
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
             OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
             OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
             OR     ( SRY-SRYCD(IDX2)       =  "099209903"
                                            OR "099209905"
                                            OR "099209906"
                                            OR "099209907"
                                            OR "099209908"  )
             OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                            OR "0086"       )
             OR     ( SRY-SRYCD(IDX2)       =  "099999903"
                                            OR "099999904"
                                            OR "099999905"
                                            OR "099999906"
                                            OR "099999907"
                                            OR "099999909"  )
             OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                    "60" OR "64" OR "70" OR
                                    "80"                   ) AND
                     (ACCT-ZAITEN       NOT =  ZERO        ) AND
                     (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                     (ACCT-JIHOKBN          =  SPACE       ) AND
                     (ACCT-YKZTEN           =  ZERO        ) AND
                     (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                  COMPUTE   RECE44-MEISAISU    =  RECE44-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL
                                               -  WRK-SRYCD
                  IF ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
                     COMPUTE   RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL
                                                  -  SRY-SRYSURYO(IDX2)
                  ELSE
                     COMPUTE   RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL
                                                  -  1
                  END-IF
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)(1:7)  =  "0999901"    )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
              OR     ( SRY-SRYCD(IDX2)(1:7)  =  "0992000"    )
              OR     ( SRY-SRYCD(IDX2)       =  "099209903"
                                             OR "099209905"
                                             OR "099209906"
                                             OR "099209907"
                                             OR "099209908"  )
              OR     ( SRY-SRYCD(IDX2)(1:4)  =  "0085"
                                             OR "0086"       )
              OR     ( SRY-SRYCD(IDX2)       =  "099999903"
                                             OR "099999904"
                                             OR "099999905"
                                             OR "099999906"
                                             OR "099999907"
                                             OR "099999909"  )
              OR     ((ACCT-SRYKBN = "40" OR "50" OR "54" OR
                                     "60" OR "64" OR "70" OR
                                     "80"                   ) AND
                      (ACCT-ZAITEN       NOT =  ZERO        ) AND
                      (ACCT-ZAIKAISU     NOT =  ZERO        ) AND
                      (ACCT-JIHOKBN          =  SPACE       ) AND
                      (ACCT-YKZTEN           =  ZERO        ) AND
                      (SRY-SRYCD(IDX2)(1:1)  =  "6"         ))
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4
               MOVE    SRY-SRYCD(IDX2)    TO    RECE45-SRYCD(IDX4)
      *
               IF      SRY-SRYCD(IDX2)    =     CONS-ROUSAI10
                        MOVE   "1"        TO    RECE41-KYUGYOFLG
                                                WRK-KYUGYOFLG
               END-IF
      *
               IF      SRY-SRYKBN         =     "12"
                   IF   SRY-SRYCD(IDX2)   =     CONS-KEIZOKU1    OR
                                                CONS-KEIZOKU2
                        MOVE   "1"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-GAIRAIRYO1  OR
                                                CONS-GAIRAIRYO2  OR
                                                CONS-GAIRAIRYO3  OR
                                                CONS-DOUJITU-GAI
                        MOVE   "2"      TO    RECE41-KEIZOKU-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-SAISHIN-A1   
      **                                  OR    CONS-SAISHIN-A2   
      **                                  OR    CONS-SAISHIN-A3   
      **                                  OR    CONS-SAISHIN-A4   
      **                                  OR    CONS-SAISHIN-A5   
      **                                  OR    CONS-SAISHIN-A6   
      **                                  OR    CONS-SAISHIN-A7   
      **                                  OR    CONS-SAISHIN-A8   
      **                                  OR    CONS-SAISHIN-A9   
      **                                  OR    CONS-SAISHIN-A10  
      **                                  OR    CONS-SAISHIN-A11  
      **                                  OR    CONS-SAISHIN-A12  
      **                                  OR    CONS-SAISHIN-A13  
                        MOVE   "1"      TO    RECE41-SAISHIN-PRTKBN2
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-GAIRAI 
                        MOVE   "1"      TO    RECE41-GAIRAIKSN-PRTKBN
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)      =    CONS-GAIJINKEN
                  MOVE   "1"              TO    RECE41-KENTAIKASANFLG
                  MOVE   99               TO    RECE44-KNSJISGRPKBN
               END-IF
               IF     RECE44-KNSJISGRPKBN  =    ZERO
                 IF    TNS-KNSJISGRPKBN = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                  MOVE  TNS-KNSJISGRPKBN  TO    RECE44-KNSJISGRPKBN
                 END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "14" OR "21" OR "22" OR "23" )
               AND   ( RECE44-ZAITEN     =     ZERO                 )
               AND   ( RECE44-SAFLG      =     SPACE                )
               AND   ( ACCT-ZAIKBN   NOT =     1                    )
                  IF   SRY-SRYCD(IDX2)   =     "820000046"
                     MOVE     "1"         TO    RECE44-SAFLG
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN   =  "21" OR "22" OR "23" )
               AND   ( RECE44-ZAITEN     =     ZERO         )
               AND   ( ACCT-ZAIKBN   NOT =     1            )
                  IF     RECE44-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE44-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                      IF   RECE44-TOYAKUFLG = SPACE
                        MOVE    "2"       TO    RECE44-TOYAKUFLG
                      END-IF
                    ELSE
                        MOVE    "1"       TO    RECE44-TOYAKUFLG
                    END-IF
                  END-IF
               END-IF
      *
               IF    ( SRY-SRYKBN        =     "14" )
               AND   ( RECE44-ZAITEN     =     ZERO )
               AND   ( ACCT-ZAIKBN   NOT =     1    )
                  IF     RECE44-SAFLG   NOT =  SPACE
                        MOVE   SPACE      TO    RECE44-TOYAKUFLG
                  ELSE
                    IF  (SRY-SRYCD(IDX2)(1:1)  =  "8"  )
                    OR  (SRY-SRYCD(IDX2)(1:3)  =  "008")
                        IF   SRY-SRYSYUKBN  =  "148" OR "149"
                          EVALUATE  WRK-PARA-PRTKBN
                           WHEN      "1"
                             MOVE   "9"  TO    RECE44-TOYAKUFLG
                           WHEN      "2"
                             IF   RECE44-TOYAKUFLG = SPACE
                               MOVE "2"  TO    RECE44-TOYAKUFLG
                             END-IF
                          END-EVALUATE
                        END-IF
                    ELSE
                        EVALUATE  WRK-PARA-PRTKBN
                          WHEN      "1"
                            MOVE  "9"    TO    RECE44-TOYAKUFLG
                          WHEN      "2"
                            MOVE  "1"    TO    RECE44-TOYAKUFLG
                        END-EVALUATE
                    END-IF
                  END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
      *          IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)   TO  WRK-SRYCD
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >    9
                     MOVE   SRY-SRYCD(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  RECE44-RUIKEITOTAL  =
                                RECE44-RUIKEITOTAL  +
                              ( WRK-FUKA-VALUE    * WRK-SRYCD)
                       IF  (ACCT-SRYKBN          =  "80" )
                       AND (SRY-SRYCD(IDX2)(1:5) =
                            "09591" OR "09592" OR "09593")
                       AND (SRY-SRYSYUKBN        =  "809")
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-JIHIMONEY(IDX2))
                       ELSE
                        COMPUTE WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX
                                                  * SRY-SRYSURYO(IDX2))
                       END-IF
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE44-RUIKEITOTAL  =
                            RECE44-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *
                   MOVE     ZERO    TO    WRK-FUKA-KINGAKU
                   PERFORM  VARYING  FUKA-IDX  FROM  1  BY  1
                             UNTIL   FUKA-IDX   >   10
                     MOVE   SRY-SRYSURYO(IDX2)(FUKA-IDX:1)
                                             TO  WRK-FUKA-VALUE
                     IF  WRK-FUKA-VALUE  =  ZERO
                       CONTINUE
                     ELSE
                       COMPUTE  WRK-FUKA-KINGAKU  =
                                WRK-FUKA-KINGAKU  +
                              ((WRK-FUKA-VALUE^4) * FUKA-IDX)
                     END-IF
                   END-PERFORM
                   COMPUTE  RECE44-RUIKEITOTAL  =
                            RECE44-RUIKEITOTAL  +
                            WRK-FUKA-KINGAKU
      *          END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE45-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE45-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE45-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE45-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE45-INPUTNUM(IDX4)
      **
               IF   RECE41-HKNKBN    NOT  =  "4"                        
                  IF ( SRY-SRYCD(IDX2)  =  CONS-ROUSAI2   OR
                                           CONS-ROUSAI3   OR
                                           CONS-ROUSAI4   OR
                                           CONS-ROUSAI9   OR
                                           CONS-ROUSAI10  OR
                                           CONS-ROUSAI11  OR
                                           CONS-ROUSAI12  OR
                                           CONS-ROUSAI13  OR
                                           CONS-ROUSAI14  OR
                                           CONS-ROUSAI15  OR
                                           CONS-ROUSAI16  OR
                                           CONS-ROUSAI17    )
                  OR ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI8    ) AND
                      (RECE41-SRYYM     <  "200804"        ))
                    IF     SRY-SRYCD(IDX2)   =   CONS-ROUSAI4
                          MOVE   "13"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                    ELSE
                          MOVE   "80"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                    END-IF
                  ELSE
                    IF      TNS-SRYKBN         =     "80"
                    OR      RECE44-SRYKBN      =     "80"
                      IF  TNS-GAITENTTLKBN   =     130
                          MOVE   "13"        TO    RECE44-SRYKBN
                                                   RECE45-SRYKBN
                      END-IF
                    END-IF
                  END-IF
               ELSE                                                     
                  IF      SRY-SRYCD(IDX2)    =     CONS-YOUBUKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-YOUBUKASAN   =  WRK-YOUBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KYOBUKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KYOBUKASAN   =  WRK-KYOBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-KEIBUKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-KEIBUKASAN   =  WRK-KEIBUKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
                  IF      SRY-SRYCD(IDX2)    =     CONS-SOGAIKASAN
                   IF   RECE41-KOTEIKBN  =  SPACE  OR  "2"
                    COMPUTE  WRK-SOGAIKASAN   =  WRK-SOGAIKASAN
                                              +  TNS-TEN
                    COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                              -  TNS-TEN
                    COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                              +  TNS-TEN
                   END-IF
                  END-IF
      *           自賠責・療養担当手当用特例処理
      *           （自賠責の場合、７点を８４円に読み替える）
      *           （療養担当手当：１月１回の算定）
                  IF      SRY-SRYCD(IDX2)    =     CONS-RYOYOTEATE
                    EVALUATE  RECE41-JIBAIKBN
                      WHEN      "1"
                        MOVE     84     TO     WRK-RYOYOTEATE2
                        COMPUTE  WRK-RYOYOTEATE   =
                                 WRK-RYOYOTEATE   +
                                 WRK-RYOYOTEATE2
                      WHEN      "2"
                        COMPUTE  WRK-RYOYOTEATE   =  WRK-RYOYOTEATE
                                                  +  TNS-TEN
                        COMPUTE  RECE44-SYUTEN1   =  RECE44-SYUTEN1
                                                  -  TNS-TEN
                        COMPUTE  RECE44-KIZAITEN  =  RECE44-KIZAITEN
                                                  +  TNS-TEN
                    END-EVALUATE
                  END-IF
      *
                  IF ( ACCT-SRYKBN          =  "80"               ) AND
                     ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592" )
      ***            EVALUATE  SRY-SRYCD(IDX2)(1:5)
      ***             WHEN      "09591"
      ***               IF  RECE41-SINDAN-TUU < 2
      ***                 MOVE   "95"   TO    RECE44-SRYKBN
      ***                                     RECE45-SRYKBN
      ***               END-IF
      ***             WHEN      "09592"
      ***               IF  RECE41-MEISAI-TUU < 2
      ***                 MOVE   "95"   TO    RECE44-SRYKBN
      ***                                     RECE45-SRYKBN
      ***               END-IF
      ***            END-EVALUATE
                     MOVE    1          TO    FLG-SRYKBN-HEN
                  ELSE
                   IF ( SRY-SRYCD(IDX2)  =  CONS-ROUSAI2   OR
                                            CONS-ROUSAI3   OR
                                            CONS-ROUSAI4   OR
                                            CONS-ROUSAI9   OR
                                            CONS-ROUSAI10  OR
                                            CONS-ROUSAI11  OR
                                            CONS-ROUSAI12  OR
                                            CONS-ROUSAI13  OR
                                            CONS-ROUSAI14  OR
                                            CONS-ROUSAI15  OR
                                            CONS-ROUSAI16  OR
                                            CONS-ROUSAI17    )
                   OR ( SRY-SRYCD(IDX2)(1:5) = "09593"       )
                   OR ((SRY-SRYCD(IDX2)  =  CONS-ROUSAI8    ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                   OR ((RECE41-JIBAIKBN  =  "1"             ) AND
                       (SRY-SRYCD(IDX2)  =  CONS-RYOYOTEATE ))
                     IF  SRY-SRYCD(IDX2)(1:5) = "09593"
                       MOVE  TNS-NAMECHGKBN
                                      TO  WRK-TNS-JIBAISYUKEIKBN(IDX2)
                                          RECE44-JIBAISYUKEIKBN
                       EVALUATE  TNS-NAMECHGKBN
                         WHEN      0
                         WHEN      4
                         WHEN      5
                            MOVE   "80"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      1
                            MOVE   "11"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      2
                            MOVE   "12"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                         WHEN      3
                            MOVE   "13"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-EVALUATE
                     ELSE
                       IF     SRY-SRYCD(IDX2)   =   CONS-ROUSAI4
                            MOVE   "13"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       ELSE
                            MOVE   "80"   TO    RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-IF
                     END-IF
                     MOVE    1          TO    FLG-SRYKBN-HEN
                   ELSE                                                 
                     IF      TNS-SRYKBN         =   "80"
                     OR      RECE44-SRYKBN      =   "80"
                         IF  TNS-GAITENTTLKBN   =   130
                             MOVE   "13"    TO  RECE44-SRYKBN
                                                RECE45-SRYKBN
                         END-IF
                     END-IF
                     IF  (WRK-PARA-JIBAIPRTKBN =  "0" )
                     AND (FLG-SRYKBN-HEN       =  ZERO)
                      IF (( RECE44-SRYKBN     =  "80" ) AND
                          ( RECE44-OKIKAEFLG  =  SPACE))
                      OR (  RECE44-OKIKAEFLG  =  "1"   )
                       IF   SRY-SRYCD(IDX2)   =     CONS-SHOHOSEN1 OR   
                                                    CONS-SHOHOSEN2 OR   
                                                    CONS-SHOHOSEN3 OR   
                                                    CONS-SHOHOSEN4 OR   
                                                    CONS-SHOHOKSN1 OR   
                                                    CONS-SHOHOKSN2 OR   
                                                    CONS-SHOHOKSN3 OR
                                                    CONS-SHOHOKSN4 OR
                                                    CONS-SHOHOKSN5 OR
                                                    CONS-RYOYOTEATE
                              CONTINUE                                  
                       ELSE                                             
                              MOVE   "1"    TO  RECE44-OKIKAEFLG
                              MOVE   "40"   TO  RECE44-SRYKBN
                                                RECE45-SRYKBN
                       END-IF                                           
                      END-IF
                     END-IF
                   END-IF                                               
                  END-IF                                                
      *
               END-IF                                                   
      *
               PERFORM  2101211-RECE45-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                IF   ACCT-ZAIKBN  =  1
                  MOVE    "1"             TO  RECE45-RECEPTFLG(IDX4)
                  MOVE    ZERO            TO  RECE45-SRYSURYO(IDX4)
                ELSE
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.99999                   
                  COMPUTE RECE45-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE45-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE45-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE45-TANINAME(IDX4)   
                    IF    RECE45-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE45-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE45-SRYSURYO(IDX4 - 1) 
                    END-IF
                  ELSE
                    MOVE  "1"             TO  RECE45-RECEPTFLG(IDX4)
                  END-IF                                                
                END-IF
               ELSE                                                     
                  IF      TNS-TANIMOJISU    =   ZERO
                  AND     TNS-TANINAME  NOT =   SPACE
                     PERFORM   VARYING   IDW   FROM   24   BY  -1
                                UNTIL    IDW    <      1
                        IF   TNS-TANINAME(IDW:1)  NOT =   SPACE
                            COMPUTE  TNS-TANIMOJISU   =   IDW   /   2
                            MOVE    1     TO    IDW
                        END-IF
                     END-PERFORM
                  END-IF
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO    RECE45-TANICD(IDX4)
                                                WRK-TANICD
                  END-IF
                  MOVE    TNS-TANIMOJISU  TO    RECE45-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE45-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
               IF ( ACCT-SRYKBN          =  "80"              ) AND     
                  ( SRY-SRYCD(IDX2)(1:5) =  "09591" OR "09592"
                                                    OR "09593")         
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
      ***>          MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)          
                    MOVE  WRK-TNS-TEN(IDX2)  TO  RECE45-TEN(IDX4)
               ELSE                                                     
                 IF   RECE41-HKNKBN     =  "4"
                 AND  RECE41-JIBAIKBN   =  "1"
                 AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
                    MOVE  1               TO  RECE45-TENSIKIBETU(IDX4)  
                    MOVE  WRK-RYOYOTEATE2 TO  RECE45-TEN(IDX4)
                 ELSE
                    MOVE  TNS-TENSIKIBETU TO  RECE45-TENSIKIBETU(IDX4)
                    MOVE  TNS-TEN         TO  RECE45-TEN(IDX4)
                 END-IF
               END-IF                                                   
               IF      RECE41-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE45-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE45-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE45-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-KENTAICOMMENT  TO    RECE45-KENTAICOMMENT
                                                               (IDX4)
               MOVE    TNS-DATAKBN        TO    RECE45-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE45-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE45-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE45-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE45-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE45-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE45-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE45-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE45-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE45-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE45-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-KNSJISGRPKBN
                                      TO RECE45-KNSJISGRPKBN   (IDX4)
      *
               IF    (TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490")
               AND   (RECE41-SRYYM   <=  "201003"   )
                      MOVE   202      TO  RECE45-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE45-CDKBN-KOUBAN(IDX4)
               END-IF
               IF     TNS-SRYCD       =  "170016010"  OR
                                         "170020270"  OR  "170024470" OR
                                         "170024910"  OR  "170025010" OR
                                         "170025210"  OR  "170025310" OR
                                         "170025410"  OR  "170025510" OR
                                         "170025610"  OR  "170025710" OR
                                         "170025810"  OR  "170025910" OR
                                         "170026010"  OR  "170026110" OR
                                         "170026210"  OR  "170026310" OR
                                         "170026810"  OR
                                         "170000210"  OR  "170016910" OR
                                         "170017010"  OR  "170026710" OR
                                         "170028810"
                      MOVE   203      TO  RECE45-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE45-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    (TNS-SRYCD       =  "170022290")
               AND   (RECE41-SRYYM   >=  "201004"   )
                      MOVE   "1"      TO  RECE44-GAZOGEN
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPNUM          TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD (IDX2)     TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)   TO  PTCOM-RENNUM
                   PERFORM 800-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO    RECE45-NAME(IDX4)
      *
      ***          IF    RECE44-COMCDFLG   NOT =   "3"
      ***             MOVE   "1"           TO    RECE44-COMCDFLG
      ***          END-IF
      *
                   IF      WRK-KYUGYOFLG   =   "1"
                   AND     SRY-SRYCD(IDX2) = CONS-ROUCOM1
                        MOVE   PTCOM-INPUTCHI1(1:2)
                                            TO  RECE41-KYUGYO-STR(1:2)
                        MOVE   PTCOM-INPUTCHI2(1:2)
                                            TO  RECE41-KYUGYO-STR(3:2)
                        MOVE   PTCOM-INPUTCHI3(1:2)
                                            TO  RECE41-KYUGYO-END(1:2)
                        MOVE   PTCOM-INPUTCHI4(1:2)
                                            TO  RECE41-KYUGYO-END(3:2)
                        MOVE   SPACE        TO  WRK-KYUGYOFLG
                   END-IF
      *
               ELSE
                   MOVE    TNS-NAME            TO    RECE45-NAME(IDX4)
      *
      *            IF   RECE41-HKNKBN   =  "4"  
      *            AND  RECE41-SRYYM   >=  "200302"
      *                 IF   SRY-SRYCD(IDX2)  =  "101120040"  OR
      *                                          "101120050"  OR
      *                                          "101120060"
      *                    MOVE   "６"   TO    RECE45-NAME(IDX4)(9:2)
      *                 END-IF
      *                 IF   SRY-SRYCD(IDX2)  =  "101120070"  OR
      *                                          "101120080"  OR
      *                                          "101120090"
      *                    MOVE   "６"   TO    RECE45-NAME(IDX4)(31:2)
      *                 END-IF
      *            END-IF
      *
               END-IF
      *
               IF          SRY-SRYKBN       =  "21"
                    IF   SRY-SRYCD(IDX2)  =  "820000047" OR 
                                             "630010002"
                        MOVE     "3"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
               IF    (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                    IF    RECE44-COMCDFLG   NOT =   "3"
                        MOVE     "1"       TO    RECE44-COMCDFLG
                    END-IF
               END-IF
      *
               IF    RECE44-COMCDFLG       =   "3"
                 IF  SRY-SRYCD(IDX2)  =  "820000047" OR
                                         "630010002"
                   CONTINUE
                 ELSE
                   ADD   999               TO  WRK-SURYOU-COUNT
                   ADD   WRK-SURYOU-COUNT  TO  RECE44-SURYOUTOTAL
                 END-IF
               END-IF
      *
               IF          SRY-SRYKBN       =     "31" OR "32"
                 IF        SRY-SRYCD(IDX2)  =    CONS-HIKAKIN
      ***                                  OR    CONS-SEIBUTU
      ***                                  OR    CONS-MAYAKU
                                           OR    CONS-JYOMYAKU
      ***                                  OR    CONS-JYOMYAKUKASAN
                   MOVE    SPACE           TO    RECE45-NAME(IDX4)
                 END-IF
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   200 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE45-NAME(IDX4)(IDV:2)   =   "訪"
                         MOVE   SPACE  TO    RECE44-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  ( SRY-SRYKBN     =  "60" OR "64" )
               AND ((SRY-SRYCD(IDX2)(1:1)  =  "8"  ) OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"))
               AND ( FLG-KENSA-COM  =   ZERO        )
                 IF  SRY-SRYCD(IDX2) = "840000085" OR "840000087"
                    CONTINUE
                 ELSE
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                              UNTIL  (IDV            >   200)
                               OR    (FLG-KENSA-COM  =    1 )
                      IF  RECE45-NAME(IDX4)(IDV:2)   =  "日"
                        MOVE   SPACE  TO  RECE44-FLG-KENSA-COMMENT
                        MOVE     1    TO  FLG-KENSA-COM
                      END-IF
                    END-PERFORM
                 END-IF
               END-IF
      *
               IF  SRY-SRYCD(IDX2)(1:3)     =   "058"
                 MOVE  TNS-FORMALYUKOKETA TO  RECE45-YUKOKETA(IDX4)
                 MOVE  TNS-FORMALNAME     TO  RECE45-NAME(IDX4)
      *
                 MOVE     TNS-TEN         TO  WRK-KZI058-TEN
                 IF   TNS-TANICD  NOT = SPACE AND "000"
                    MOVE  TNS-TANICD      TO  WRK-KZI058-TANICD
                    MOVE  TNS-TANIMOJISU  TO  WRK-KZI058-TANIMOJISU
                    MOVE  TNS-TANINAME    TO  WRK-KZI058-TANINAME
                 END-IF
                 MOVE     ZERO            TO  RECE45-TEN(IDX4)
                 MOVE     ZERO            TO  RECE45-TANICD(IDX4)
                 MOVE     ZERO            TO  RECE45-TANIMOJISU(IDX4)
                 MOVE     SPACE           TO  RECE45-TANINAME(IDX4)
               END-IF
               IF  (SRY-SRYCD(IDX2)(1:1)    =   "7")
               AND (SRY-MEISKYFLG(IDX2)     =   "1")
                 IF   (SRY-SRYKBN  =   "70")
                 AND  (TNS-DATAKBN =   "3" )
                    IF    RECE45-RENNUM  >  1
                    AND   IDX4           =  1
                      MOVE    1           TO  FLG-FORMAL-KISAI
                      COMPUTE  WRK-RENNUM2 =  RECE45-RENNUM  -  1
                    ELSE
                      MOVE  "1"           TO  RECE45-RECEPTFLG(IDX4 - 1)
                    END-IF
                 ELSE
                    MOVE  "1"             TO  RECE45-RECEHENFLG(IDX4)
                    IF    WRK-KZI058-TEN  NOT = ZERO
                       MOVE   WRK-KZI058-TEN
                                          TO  RECE45-TEN(IDX4)
                    END-IF
                    IF   (RECE45-TANICD(IDX4)    = ZERO)
                    AND  (WRK-KZI058-TANICD  NOT = ZERO)
                       MOVE   WRK-KZI058-TANICD
                                          TO  RECE45-TANICD(IDX4)
                       MOVE   WRK-KZI058-TANIMOJISU
                                          TO  RECE45-TANIMOJISU(IDX4)
                       MOVE   WRK-KZI058-TANINAME
                                          TO  RECE45-TANINAME(IDX4)
                    END-IF
                 END-IF
               END-IF
      *
               IF  (SRY-SRYKBN         =  "99"       )
               AND (SRY-RENNUM         =   1         )
               AND (IDX2               =   1         )
               AND (SRY-SRYCD(IDX2)    =  "099999902")
                    MOVE      1         TO  FLG-TAHOKEN
               END-IF
               IF  (FLG-TAHOKEN           =   1              )
               AND (SRY-RENNUM            =   1              )
               AND (IDX2                  =   2              )
               AND (SRY-SRYCD(IDX2)(1:4)  =  "0082"          )
               AND (TNS-NAME(1:14)        =  "保険名称変更：")
                  MOVE     "1"          TO  RECE45-RECEPTFLG(IDX4)
      *
                  MOVE      ZERO        TO  IDW3
                  PERFORM   VARYING   IDW   FROM   200   BY  -1
                             UNTIL    IDW     <    15
                       IF   TNS-NAME(IDW:1)  NOT =   SPACE
                           COMPUTE IDW6       =  (IDW / 2) - 7
                           MOVE    IDW        TO  IDW3
                           MOVE    1          TO  IDW
                       END-IF
                  END-PERFORM
                  IF     IDW3    NOT =  ZERO
                    MOVE  SPACE TO RECE45-NAME(IDX4 - 1)
                    COMPUTE   IDW4  =  IDW3  -  14
                    MOVE  TNS-NAME(15:IDW4)
                                TO RECE45-NAME(IDX4 - 1)(1:IDW4)
                    COMPUTE   IDW5  =  IDW4  +   1
                    MOVE  "にて診察料算定済"
                                TO RECE45-NAME(IDX4 - 1)(IDW5:16)
                    COMPUTE   RECE45-YUKOKETA(IDX4 - 1) =
                              RECE45-YUKOKETA(IDX4 - 1) -
                              3                         +
                              IDW6
                  END-IF
               END-IF
      *
               IF  RECE45-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE45-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE45-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF (  SRY-SRYCD(IDX2) = CONS-ROUSAI1  OR
                                       CONS-ROUSAI2  OR
                                       CONS-ROUSAI3  OR
                                       CONS-ROUSAI4  OR
                                       CONS-ROUSAI5  OR
                                       CONS-ROUSAI6  OR
                                       CONS-ROUSAI7  OR
                                       CONS-ROUSAI9  OR
                                       CONS-ROUSAI10 OR
                                       CONS-ROUSAI11 OR
                                       CONS-ROUSAI12 OR
                                       CONS-ROUSAI13 OR
                                       CONS-ROUSAI14 OR
                                       CONS-ROUSAI15 OR
                                       CONS-ROUSAI16 OR
                                       CONS-ROUSAI17 OR
                                       CONS-ROUSAI18 OR
                                       CONS-ROUSAI19   )
               OR (( ACCT-SRYKBN          =  "80"     )   AND           
                   ( SRY-SRYCD(IDX2)(1:5) =  "09591"
                                          OR "09592"
                                          OR "09593"  ))                
               OR (( SRY-SRYCD(IDX2) = CONS-ROUSAI8   )   AND
                   ( RECE41-SRYYM    < "200804"       ))
      *
      *           IF     TNS-TENSIKIBETU   =   1
                     ADD      1          TO  WRK-MEISAI-CNT
                     IF  SRY-SRYSYUKBN = "809"
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (SRY-JIHIMONEY(IDX2) / 10)
                     ELSE
                        COMPUTE  WRK-TEN = WRK-TEN +
                                          (TNS-TEN / 10)
                     END-IF
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *           END-IF
      *
                  MOVE     "2"     TO    RECE45-SIKFLG(IDX4)
      *
               ELSE
      *
                  IF   RECE41-HKNKBN     =  "4"
                  AND  RECE41-JIBAIKBN   =  "1"
                  AND  SRY-SRYCD(IDX2)   =  CONS-RYOYOTEATE
      *
                     ADD      1          TO  WRK-MEISAI-CNT
                     COMPUTE  WRK-TEN    =   WRK-TEN   +
                                             TNS-TEN
      *
                     ADD      1          TO  WRK-SURYOUTOTAL
                     MOVE  SRY-SRYCD(IDX2) 
                                         TO  WRK-SRYCD
                     ADD   WRK-SRYCD     TO  WRK-SRYCDTOTAL
      *
                     MOVE     "2"        TO  RECE45-SIKFLG(IDX4)
      *
                  ELSE
      *
                    IF  ((RECE41-HKNKBN      =   "4") AND
                         (RECE41-JIBAIKBN    =   "2"))
                    OR  ((RECE41-HKNKBN      =   "5") AND
                         (RECE41-KOMUKBN     =   "2"))
                      IF (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE45-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE44-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE44-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                      END-IF
                    ELSE
                      IF  ( SRY-SRYCD(IDX2) = CONS-ROUCOM1  OR
                                              CONS-ROUCOM2      )
                      OR  ( SRY-SRYCD(1)    = CONS-ROUSAI2  OR
                                              CONS-ROUSAI3      )
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000020"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "11"              ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "12"              ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ) AND
                          ( SRY-SRYCD(IDX2) NOT = "830000021"   ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "12"              ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ) AND
                          ( ACCT-ZAIKBN     NOT =  1 AND 2      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12 OR
                                               CONS-ROUSAI13 OR
                                               CONS-ROUSAI14 OR
                                               CONS-ROUSAI15 OR
                                               CONS-ROUSAI16 OR
                                               CONS-ROUSAI17    ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI9  OR
                                               CONS-ROUSAI10 OR
                                               CONS-ROUSAI11 OR
                                               CONS-ROUSAI12 OR
                                               CONS-ROUSAI13 OR
                                               CONS-ROUSAI14 OR
                                               CONS-ROUSAI15 OR
                                               CONS-ROUSAI16 OR
                                               CONS-ROUSAI17    ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN     = "13"              ) AND
                          ( SRY-SRYCD(1)    =  CONS-ROUSAI8     ) AND
                          ((SRY-SRYCD(IDX2)(1:3)   =   "008") OR
                           (SRY-SRYCD(IDX2)(1:1)   =   "8"  )   ) AND
                          ( RECE41-SRYYM    <  "200804"         ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:1)   =   "8"      ))
                      OR (( ACCT-SRYKBN            =  "80"      ) AND
                          ( SRY-SRYCD(1)(1:5)      =  "09591"
                                                   OR "09592"
                                                   OR "09593"   ) AND
                          ( SRY-SRYCD(IDX2)(1:3)   =   "008"    ))
      *
                         ADD    1      TO    WRK-MEISAI-CNT             
                         MOVE  "2"     TO    RECE45-SIKFLG(IDX4)
      *
                         ADD    1      TO    WRK-SURYOUTOTAL
                         MOVE  SRY-SRYCD(IDX2) 
                                       TO    WRK-SRYCD
                         ADD   WRK-SRYCD 
                                       TO    WRK-SRYCDTOTAL
                         IF  ( RECE44-COMCDFLG       =   "1"   )
                         AND ((SRY-SRYCD(IDX2)(1:1)  =   "8"  ) OR
                              (SRY-SRYCD(IDX2)(1:3)  =   "008"))
                            MOVE  "2"  TO    RECE44-COMCDFLG
                         END-IF
                      ELSE
                         MOVE  "1"     TO    RECE45-SIKFLG(IDX4)
                      END-IF
                    END-IF
      *
                  END-IF
      *
               END-IF
      *
      *        特定器材（画像診断のフィルム以外）のとき単価を編集
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
      ***          IF   (( SRY-SRYKBN               =   "70" )  AND
      ***                ( TNS-DATAKBN              =   "3"  ))
                   IF   (  TNS-DATAKBN              =   "3"   )
                   OR   (  SRY-SRYCD (IDX2)         =
                                    CONS-FILM-YOUJI-KASAN  OR  
                                    CONS-SANSO             OR
                                    CONS-SANSO2               )
                       CONTINUE
                   ELSE
      **               SET     TBL-SANSO           TO  1
      **               SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
      **                   AT  END
      **                       MOVE    ZERO            TO  FLG-SANSO-OK
      **                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
      **                                                        (TBL-SANSO)
      **                       MOVE    1               TO  FLG-SANSO-OK
      **               END-SEARCH
      **               IF      FLG-SANSO-OK        =   "1"
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2101212-RECE45-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE45-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE45-NAME(IDX4)(IDW:)   
                       COMPUTE RECE45-YUKOKETA(IDX4)
                                               =   RECE45-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *       
      *----------------------------------------------------------------
      *        在宅部分のレセプト編集用に往診（特別往診）・夜間・
      *        深夜・緊急の剤判定を追加
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
      *
                 IF (SRY-SRYCD(IDX2) =  CONS-ZAIHOU-COMMENT )
                 OR (SRY-SRYCD(IDX2) =  CONS-ZAIHOU3-COMMENT)
                 OR (SRY-SRYCD(IDX2) =  CONS-ZAIHOU4-COMMENT)
                    MOVE        9         TO    FLG-OSINKBN
                 END-IF
      *
                 EVALUATE    SRY-SRYCD(IDX2)
      *往診（特別往診）
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   WHEN    "114002470"
                   WHEN    "114002970"
                   IF  FLG-OSINKBN        NOT = 2  AND  3  AND  9
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *夜間
                   WHEN    "114000470"
                   WHEN    "114001970"
                   WHEN    "114011670"
                   WHEN    "114011970"
                   WHEN    "114017570"
                   WHEN    "114017870"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     2         TO    FLG-OSINKBN
                   END-IF
      *深夜・緊急
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                   WHEN    "114011570"
                   WHEN    "114011770"
                   WHEN    "114011870"
                   WHEN    "114012070"
                   WHEN    "114017470"
                   WHEN    "114017770"
                   WHEN    "114017670"
                   WHEN    "114017970"
                   IF  FLG-OSINKBN        NOT = 9
                       MOVE     3         TO    FLG-OSINKBN
                   END-IF
                 END-EVALUATE
               END-IF
      *
               MOVE    ACCT-ZAIKBN  TO  RECE44-ZAIKBN
      *
               IF      ACCT-ZAIKBN       =     1
                 MOVE   ZERO        TO  RECE45-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE45-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE45-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE45-TANINAME(IDX4)
                 IF  (SRY-SRYCD(IDX2)(1:1) = "6" OR "7")
                 OR  (SRY-SRYCD(IDX2)(1:3) = "059"     )
                 OR  (SRY-SRYCD(IDX2)(1:2) = "63"      )
                     MOVE  "1"          TO  RECE44-COMCDFLG
                     MOVE  "810000001"  TO  RECE45-SRYCD(IDX4)
                 END-IF
               END-IF
               IF      ACCT-ZAIKBN       =     2
                 MOVE   ZERO        TO  RECE45-SRYSURYO(IDX4)
                 MOVE   ZERO        TO  RECE45-TANICD(IDX4)
                 MOVE   ZERO        TO  RECE45-TANIMOJISU(IDX4)
                 MOVE   SPACE       TO  RECE45-TANINAME(IDX4)
               END-IF
      *
              END-IF
             END-IF  
           END-PERFORM
      *
           IF    IDX4    NOT =   ZERO
               MOVE    1     TO    FLG-WRITE45X
           END-IF
      *
           .
       210121-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントフラグ編集処理
      *****************************************************************
       2101211-RECE45-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE44-SRYKBN      =     "11"
      *       初診料算定科：
              IF ( SRY-SRYCD(IDX2)    =     "830000020"         )
              OR ((SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT) AND
                  (RECE41-SRYYM      >=     "201204"           ))
              OR ((SRY-SRYCD(IDX2)    =     CONS-ROUSAI18      ) AND
                  (RECE41-SRYYM      >=     "201204"           ))
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SYOSANTEIKA-COMMENT
              END-IF
      *       初診（同日複数診療科）
              IF  (SRY-SRYCD(IDX2)    =     CONS-SYODOU-COMMENT
                                      OR    CONS-ROUSAI18       )
              AND (FLG-SYODOU-COM     =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-SYODOU-COMMENT
                   MOVE   SRY-SRYKA   TO    RECE44-SYODOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYODOU2-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-SYODOU-COMMENT
                   MOVE  SPACE  TO    RECE44-SYODOU-SRYKA(1)
                   MOVE    1    TO    FLG-SYODOU-COM
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "12"
      *       再診料算定科：
              IF   SRY-SRYCD(IDX2)    =     "830000021"
                                      OR    CONS-DOUJITU-SAI
                                      OR    CONS-DOUJITU-DEN
                                      OR    CONS-DOUJITU-GAI
                                      OR    CONS-ROUSAI19
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SAISANTEIKA-COMMENT
              END-IF
      *       電話再診
              IF  (SRY-SRYCD(IDX2)    =     "112009750"   OR
                                            "112009850"   OR
                                            "112009950"   OR
                                            "112010050"   OR
                                            "112012750"   OR
                                            "112704150"   OR
                                            "112704250"   OR
                                            "112704350"   OR
                                            "112706750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112007950"   OR
                                            "112008050"   OR
                                            "112008150"   OR
                                            "112008250"   OR
                                            "112012450"   OR
                                            "112702750"   OR
                                            "112702850"   OR
                                            "112702950"   OR
                                            "112706450"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"     )
              AND (RECE41-SRYYM       <     "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DENWASAI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "112007950"     )
              AND (RECE41-SRYYM       >=    "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DENWASAI-COMMENT
              END-IF
      *       同日再診
              IF  (SRY-SRYCD(IDX2)    =     "112010150"   OR
                                            "112010250"   OR
                                            "112010350"   OR
                                            "112010450"   OR
                                            "112010550"   OR
                                            "112704450"   OR
                                            "112704550"   OR
                                            "112704650"   OR
                                            "112704750"   OR
                                            "112010650"   OR
                                            "112010750"   OR
                                            "112010850"   OR
                                            "112010950"   OR
                                            "112012850"   OR
                                            "112704850"   OR
                                            "112704950"   OR
                                            "112705050"   OR
                                            "112706850"   OR
                                            "112008850"   OR
                                            "112008950"   OR
                                            "112009050"   OR
                                            "112009150"   OR
                                            "112012550"   OR
                                            "112703450"   OR
                                            "112703550"   OR
                                            "112703650"   OR
                                            "112706550"   OR
                                            "112008350"   OR
                                            "112008450"   OR
                                            "112008550"   OR
                                            "112008650"   OR
                                            "112008750"   OR
                                            "112703050"   OR
                                            "112703150"   OR
                                            "112703250"   OR
                                            "112703350"   OR
      *
                                            "112011710"   OR
                                            "112705850"     )
              AND (RECE41-SRYYM       <     "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUJITUSAI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     "112008350"   OR
                                            "112011710"     )
              AND (RECE41-SRYYM       >=    "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUJITUSAI-COMMENT
              END-IF
      *       同日電話再診
              IF  (SRY-SRYCD(IDX2)    =     "112008850"     )
              AND (RECE41-SRYYM       >=    "201004"        )
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-DOUDENSAI-COMMENT
              END-IF
      *
      *       再診（同一日２科目）
              IF  (SRY-SRYCD(IDX2)    =     CONS-DOUJITU-SAI
                                      OR    CONS-DOUJITU-DEN
                                      OR    CONS-DOUJITU-GAI
                                      OR    CONS-ROUSAI19   )
              AND (FLG-SAIDOU-COM     =     ZERO            )
                 MOVE   "1"        TO   RECE44-FLG-SAIDOU-COMMENT
                 MOVE   SRY-SRYKA  TO   RECE44-SAIDOU-SRYKA(1)
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-DOUJITU-COMMENT
                 MOVE   SPACE      TO   RECE44-FLG-SAIDOU-COMMENT
                 MOVE   SPACE      TO   RECE44-SAIDOU-SRYKA(1)
                 MOVE    1         TO   FLG-SAIDOU-COM
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "40"
      *       介達牽引（発症日記載）
              IF   RECE41-SRYYM      >=    "200604"
              AND  RECE41-SRYYM      <=    "200803"
                IF   SRY-SRYCD(IDX2)  =     CONS-KAITATU-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KAITATU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201004"
      *         人工腎臓等
                IF   SRY-SRYCD(IDX2)  =     CONS-JINZOU1-COMMENT
                                        OR  CONS-JINZOU2-COMMENT
                                        OR  CONS-JINZOU3-COMMENT
                                        OR  CONS-JINZOU4-COMMENT
                                        OR  CONS-JINZOU5-COMMENT
                                        OR  CONS-JINZOU6-COMMENT
                                        OR  CONS-JINZOU7-COMMENT
                                        OR  CONS-JINZOU8-COMMENT
                                        OR  CONS-JINZOU9-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-JINZOU-COMMENT
                END-IF
      *         局所陰圧閉鎖処置
                IF   RECE41-SRYYM      >=    "201204"
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN5-COMMENT
                                          OR  CONS-KYOKUIN6-COMMENT
                                          OR  CONS-KYOKUIN7-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-KYOKUIN-COMMENT
                  END-IF
                ELSE
                  IF   SRY-SRYCD(IDX2)  =     CONS-KYOKUIN1-COMMENT
                                          OR  CONS-KYOKUIN2-COMMENT
                                          OR  CONS-KYOKUIN3-COMMENT
                                          OR  CONS-KYOKUIN4-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-KYOKUIN-COMMENT
                  END-IF
                END-IF
              END-IF
           END-IF
      *
           IF    ((RECE41-HKNKBN    NOT  =  "4"         ) AND
                  (RECE44-SRYKBN         =  "80"        ))
           OR    ((RECE41-HKNKBN         =  "4"         ) AND
                  (RECE44-SRYKBN         =  "40" OR "80"))
      *       リハビリ
              IF   SRY-SRYCD(IDX2)    =     "180706910"   OR
                                            "180022210"   OR
                                            "180706810"   OR
                                            "180022110"   OR
                                            "180707110"   OR
                                            "180022410"   OR
                                            "180707010"   OR
                                            "180022310"   OR
                                            "180707310"   OR
                                            "180022610"   OR
                                            "180707210"   OR
                                            "180022510"   OR
                                            "180707510"   OR
                                            "180022810"   OR
                                            "180707410"   OR
                                            "180022710"   OR
                                            "180708210"   OR
                                            "180023710"   OR
                                            "180708410"   OR
                                            "180023910"   OR
                                            "180708110"   OR
                                            "180023610"   OR
                                            "180708310"   OR
                                            "180023810"   OR
                                            "180024910"   OR
                                            "180024810"   OR
                                            "180025110"   OR
                                            "180025010"   OR
                                            "180025510"   OR
                                            "180025410"
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SINDAI1-COMMENT  OR
                                            CONS-SINDAI2-COMMENT  OR
                                            CONS-SINDAI3-COMMENT  OR
                                            CONS-SINDAI4-COMMENT  OR
                                            CONS-SINDAI7-COMMENT  OR
                                            CONS-SINDAI8-COMMENT  OR
                                            CONS-KOKYUKI1-COMMENT OR
                                            CONS-KOKYUKI2-COMMENT OR
                                            CONS-KOKYUKI3-COMMENT OR
                                            CONS-KOKYUKI4-COMMENT OR
                                            CONS-KOKYUKI7-COMMENT OR
                                            CONS-KOKYUKI8-COMMENT OR
                                            CONS-NOUKE1-COMMENT   OR
                                            CONS-NOUKE2-COMMENT   OR
                                            CONS-NOUKE3-COMMENT   OR
                                            CONS-NOUKE4-COMMENT   OR
                                            CONS-NOUKE7-COMMENT   OR
                                            CONS-NOUKE8-COMMENT   OR
                                            CONS-NOUKE9-COMMENT   OR
                                            CONS-NOUKE10-COMMENT  OR
                                            CONS-NOUKE11-COMMENT  OR
                                            CONS-NOUKE12-COMMENT  OR
                                            CONS-NOUKE13-COMMENT  OR
                                            CONS-NOUKE14-COMMENT  OR
                                            CONS-NOUKE15-COMMENT  OR
                                            CONS-NOUKE16-COMMENT  OR
                                            CONS-NOUKE17-COMMENT  OR
                                            CONS-NOUKE18-COMMENT  OR
                                            CONS-NOUKE19-COMMENT  OR
                                            CONS-UNDOKI1-COMMENT  OR
                                            CONS-UNDOKI2-COMMENT  OR
                                            CONS-UNDOKI3-COMMENT  OR
                                            CONS-UNDOKI4-COMMENT  OR
                                            CONS-UNDOKI7-COMMENT  OR
                                            CONS-UNDOKI8-COMMENT  OR
                                            CONS-UNDOKI9-COMMENT  OR
                                            CONS-UNDOKI10-COMMENT OR
                                            CONS-UNDOKI11-COMMENT OR
                                            CONS-UNDOKI12-COMMENT OR
                                            CONS-UNDOKI13-COMMENT OR
                                            CONS-SYOGAI1-COMMENT  OR
                                            CONS-SYOGAI2-COMMENT  OR
                                            CONS-SYOGAI3-COMMENT  OR
                                            CONS-GANKNJ-COMMENT
      *****             MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-RIHABIRI-COMMENT
              END-IF
              EVALUATE      SRY-SRYCD(IDX2)
                WHEN     CONS-SINDAI5-COMMENT
                        MOVE   "2"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-SINDAI6-COMMENT
                        MOVE   "3"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI5-COMMENT
                        MOVE   "4"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-KOKYUKI6-COMMENT
                        MOVE   "5"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE5-COMMENT
                        MOVE   "6"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-NOUKE6-COMMENT
                        MOVE   "7"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI5-COMMENT
                        MOVE   "8"    TO    RECE44-RIHABIRI-COMMENT
                WHEN     CONS-UNDOKI6-COMMENT
                        MOVE   "9"    TO    RECE44-RIHABIRI-COMMENT
              END-EVALUATE
      *       リハビリ（発症日記載）
              IF   RECE41-SRYYM      >=    "200604"
                IF   SRY-SRYCD(IDX2)  =     CONS-SINDAI1-COMMENT
                                      OR    CONS-SINDAI2-COMMENT
                                      OR    CONS-SINDAI3-COMMENT
                                      OR    CONS-SINDAI4-COMMENT
                                      OR    CONS-SINDAI5-COMMENT
                                      OR    CONS-SINDAI6-COMMENT
                                      OR    CONS-SINDAI7-COMMENT
                                      OR    CONS-SINDAI8-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SINDAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-KOKYUKI1-COMMENT
                                      OR    CONS-KOKYUKI2-COMMENT
                                      OR    CONS-KOKYUKI3-COMMENT
                                      OR    CONS-KOKYUKI4-COMMENT
                                      OR    CONS-KOKYUKI5-COMMENT
                                      OR    CONS-KOKYUKI6-COMMENT
                                      OR    CONS-KOKYUKI7-COMMENT
                                      OR    CONS-KOKYUKI8-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-KOKYUKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NOUKE1-COMMENT
                                      OR    CONS-NOUKE2-COMMENT
                                      OR    CONS-NOUKE3-COMMENT
                                      OR    CONS-NOUKE4-COMMENT
                                      OR    CONS-NOUKE5-COMMENT
                                      OR    CONS-NOUKE6-COMMENT
                                      OR    CONS-NOUKE7-COMMENT
                                      OR    CONS-NOUKE8-COMMENT
                                      OR    CONS-NOUKE9-COMMENT
                                      OR    CONS-NOUKE10-COMMENT
                                      OR    CONS-NOUKE11-COMMENT
                                      OR    CONS-NOUKE12-COMMENT
                                      OR    CONS-NOUKE13-COMMENT
                                      OR    CONS-NOUKE14-COMMENT
                                      OR    CONS-NOUKE15-COMMENT
                                      OR    CONS-NOUKE16-COMMENT
                                      OR    CONS-NOUKE17-COMMENT
                                      OR    CONS-NOUKE18-COMMENT
                                      OR    CONS-NOUKE19-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NOUKE-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-UNDOKI1-COMMENT
                                      OR    CONS-UNDOKI2-COMMENT
                                      OR    CONS-UNDOKI3-COMMENT
                                      OR    CONS-UNDOKI4-COMMENT
                                      OR    CONS-UNDOKI5-COMMENT
                                      OR    CONS-UNDOKI6-COMMENT
                                      OR    CONS-UNDOKI7-COMMENT
                                      OR    CONS-UNDOKI8-COMMENT
                                      OR    CONS-UNDOKI9-COMMENT
                                      OR    CONS-UNDOKI10-COMMENT
                                      OR    CONS-UNDOKI11-COMMENT
                                      OR    CONS-UNDOKI12-COMMENT
                                      OR    CONS-UNDOKI13-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-UNDOKI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NANBYO-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-NANBYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SYOGAI1-COMMENT
                                      OR    CONS-SYOGAI2-COMMENT
                                      OR    CONS-SYOGAI3-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SYOGAI-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-SESSYOKU-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-SESSYOKU-COMMENT
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201004"
      *         認知療法・認知行動療法
                IF   SRY-SRYCD(IDX2)  =     CONS-NINRYO-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-NINRYO2-COMMENT
                        MOVE   "2"    TO    RECE44-FLG-NINRYO-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-MANSEI
                   MOVE   "1"   TO    RECE44-FLG-SHIDOU-COMMENT
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
              AND  FLG-SYOBYO-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO2-COMMENT
                                      OR    CONS-SYOBYO3-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-SYOBYO-COMMENT
                   MOVE    1    TO    FLG-SYOBYO-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
              AND  FLG-RYOYO-COM      =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-RYOYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO2-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-RYOYO-COMMENT
                   MOVE    1    TO    FLG-RYOYO-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-NIKOCHIN-COMMENT
                                      OR    CONS-NIKOCHIN2-COMMENT
                                      OR    CONS-NIKOCHIN3-COMMENT)
              AND (FLG-NIKOCHIN-COM   =     ZERO                  )
                   MOVE   "1"   TO    RECE44-FLG-NIKOCHIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-NIKOCHIN4-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-NIKOCHIN-COMMENT
                   MOVE    1    TO    FLG-NIKOCHIN-COM
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-ZENSOKU-COMMENT
                                      OR    CONS-ZENSOKU2-COMMENT)
              AND (FLG-ZENSOKU-COM    =     ZERO                 )
                   MOVE   "1"   TO    RECE44-FLG-ZENSOKU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-ZENSOKU3-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-ZENSOKU-COMMENT
                   MOVE    1    TO    FLG-ZENSOKU-COM
              END-IF
      *
              IF   RECE41-SRYYM    >=     "200804"
                IF   (SRY-SRYCD(IDX2)   =     CONS-SYOUNI1-COMMENT)
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-SYOUNI-COMMENT
                   MOVE   "1"   TO    RECE41-SYOUNIFLG
                END-IF
                IF   (SRY-SRYCD(IDX2)   =     CONS-SYOUNI2-COMMENT)
                AND  (FLG-SYOUNI-COM    =     ZERO                )
                   MOVE   "2"   TO    RECE44-FLG-SYOUNI-COMMENT
                END-IF
                IF    SRY-SRYCD(IDX2)   =     CONS-SYOUNI3-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-SYOUNI-COMMENT
                   MOVE    1    TO    FLG-SYOUNI-COM
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201004"
      *         心臓ペースメーカー指導管理料（遠隔モニタリング）
                IF   SRY-SRYCD(IDX2)  =     CONS-SINPESU-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-SINPESU-COMMENT
                END-IF
      *         集団栄養食事指導料
                IF   SRY-SRYCD(IDX2)  =     CONS-SYUEIYO-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-SYUEIYO-COMMENT
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM      >=    "201204"
                IF   SRY-SRYCD(IDX2)  =     CONS-NINSEN-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-NINSEN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-TAKYO-COMMENT
                        MOVE   "1"    TO    RECE41-COMCNTFLG
                        MOVE   "1"    TO    RECE44-FLG-TAKYO-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-TAKYOKSN-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-TAKYOKSN-COMMENT
                END-IF
                IF   SRY-SRYCD(IDX2)  =     CONS-GAIRIHA1-COMMENT
                                      OR    CONS-GAIRIHA2-COMMENT
                                      OR    CONS-GAIHOU-COMMENT
                        MOVE   "1"    TO    RECE44-FLG-GAIRIHA-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
      *
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-HINKAI-COMMENT
              END-IF
      *
              IF  (SRY-SRYCD(IDX2)    =     CONS-OSIN-COMMENT
                                      OR    CONS-OSIN2-COMMENT)
              AND (FLG-OSIN-COM       =     ZERO              )
                   MOVE   "1"   TO    RECE44-FLG-OSIN-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-OSIN3-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-OSIN-COMMENT
                   MOVE    1    TO    FLG-OSIN-COM
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-ZAIIKAN1-COMMENT
                                      OR    CONS-ZAIIKAN2-COMMENT
                                      OR    CONS-ZAIIKAN3-COMMENT
                                      OR    CONS-ZAIIKAN4-COMMENT
                                      OR    CONS-ZAIIKAN5-COMMENT
                                      OR    CONS-ZAIIKAN6-COMMENT
                                      OR    CONS-ZAIIKAN7-COMMENT
                                      OR    CONS-ZAIIKAN8-COMMENT
                                      OR    CONS-ZAIIKAN9-COMMENT
                                      OR    CONS-ZAIIKAN10-COMMENT
                                      OR    CONS-ZAIIKAN11-COMMENT
                                      OR    CONS-ZAIIKAN12-COMMENT
                                      OR    CONS-ZAIIKAN13-COMMENT
                                      OR    CONS-ZAIIKAN14-COMMENT
                                      OR    CONS-ZAIIKAN15-COMMENT
                                      OR    CONS-ZAIIKAN16-COMMENT
                                      OR    CONS-ZAIHOU-COMMENT
                                      OR    CONS-ZAIHOU3-COMMENT
                                      OR    CONS-ZAIHOU4-COMMENT
                   MOVE   "1"   TO    RECE41-COMCNTFLG
                   MOVE   "2"   TO    RECE44-FLG-OSIN-COMMENT
              END-IF
      *
              IF   (SRY-SRYCD(IDX2)   =     CONS-ZAIHOU-COMMENT
                                      OR    CONS-ZAIHOU3-COMMENT
                                      OR    CONS-ZAIHOU4-COMMENT)
              AND  (FLG-ZAIHOU-COM    =     ZERO                )
                   MOVE   "1"   TO    RECE44-FLG-ZAIHOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-ZAIHOU2-COMMENT
                   MOVE  SPACE  TO    RECE44-FLG-ZAIHOU-COMMENT
                   MOVE    1    TO    FLG-ZAIHOU-COM
              END-IF
      *
              IF   RECE41-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-OSIN-COMMENT
                                    OR  CONS-OSIN2-COMMENT
                   IF    (WRK-SISETU3168  =  1)
                   OR    (WRK-SISETU3055  =  1)
                   OR    (WRK-SISETU3056  =  1)
                       MOVE   "1"   TO    RECE41-COMCNTFLG
                       MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                   END-IF
                   IF  RECE44-FLG-ZAISIEN-COMMENT = SPACE
                     IF  (WRK-SISETU3169  =  1)
                     OR  (WRK-SISETU3057  =  1)
                     OR  (WRK-SISETU3058  =  1)
                       MOVE   "1"   TO    RECE41-COMCNTFLG
                       MOVE   "2"   TO    RECE44-FLG-ZAISIEN-COMMENT
                     END-IF
                   END-IF
                END-IF
              ELSE
                IF   WRK-SISETU613   =  1
                  IF   RECE41-SRYYM   >=  "200604"
                  AND  RECE41-SRYYM   <=  "200803"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAIHOU-COMMENT OR
                                            CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                  IF   RECE41-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "1"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                  END-IF
                ELSE
                  IF   WRK-SISETU695   =  1
                    IF   RECE41-SRYYM   >=  "200804"
                      IF SRY-SRYCD(IDX2) =  CONS-ZAISIEN1       OR
                                            CONS-ZAISIEN2       OR
                                            CONS-ZAISIEN3
                        MOVE   "1"   TO    RECE41-COMCNTFLG
                        MOVE   "2"   TO    RECE44-FLG-ZAISIEN-COMMENT
                      END-IF
                    END-IF
                  END-IF
                END-IF
              END-IF
      *
              IF   RECE41-SRYYM    >=     "201204"
                IF   SRY-SRYCD(IDX2) =  CONS-KAISYOKU-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-KAISYOKU-COMMENT
                END-IF
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "50"
              IF   RECE44-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE44-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF   RECE41-SRYYM      >=    "201004"
            IF     RECE44-SRYKBN      =     "54"
              IF   RECE44-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE44-FLG-MASUI-COMMENT
              END-IF
            END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "70"
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT3  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT5  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT8  OR
                                            CONS-CTMRI-COMMENT9  OR
                                            CONS-CTMRI-COMMENT10 OR
                                            CONS-CTMRI-COMMENT11 OR
                                            CONS-CTMRI-COMMENT12 OR
                                            CONS-CTMRI-COMMENT13 OR
                                            CONS-CTMRI-COMMENT14 OR
                                            CONS-CTMRI-COMMENT15 OR
                                            CONS-CTMRI-COMMENT16 OR
                                            CONS-CTMRI-COMMENT17)
              AND (RECE41-SRYYM       <     "201004"            )
                   MOVE   "1"   TO    RECE44-FLG-CTMRI-COMMENT
              END-IF
              IF  (SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
                                            CONS-CTMRI-COMMENT2  OR
                                            CONS-CTMRI-COMMENT4  OR
                                            CONS-CTMRI-COMMENT6  OR
                                            CONS-CTMRI-COMMENT7  OR
                                            CONS-CTMRI-COMMENT18 OR
                                            CONS-CTMRI-COMMENT19 OR
                                            CONS-CTMRI-COMMENT20)
              AND (RECE41-SRYYM       >=    "201004"            )
                   MOVE   "1"   TO    RECE44-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN      =     "60" OR "64"
              IF   (SRY-SRYCD(IDX2)   =     CONS-KENSA1-COMMENT
                                      OR    CONS-KENSA2-COMMENT
                                      OR    CONS-KENSA3-COMMENT)
              AND  (FLG-KENSA-COM     =     ZERO               )
                   MOVE   "1"   TO    RECE44-FLG-KENSA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE44-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                   OR "60" OR "70" OR "80" OR "64"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE44-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2101211-RECE45-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の単価編集処理
      *****************************************************************
       2101212-RECE45-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  2101212-RECE45-TENSOU-TANKA-HEN-EXT  
      ***  END-IF
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    数字表示用編集（半角）
           MOVE    TNS-TEN         TO  WRK-TANKA-9
           MOVE    WRK-TANKA-S1    TO  WRK-TANKA-Z1
           MOVE    ZERO            TO  FLG-SP
           PERFORM   VARYING   IDY   FROM    2   BY  -1
                     UNTIL     IDY           =   ZERO
             IF  WRK-TANKA-S2(IDY:1) NOT =   ZERO
                 MOVE    1               TO  FLG-SP
             END-IF
             IF  FLG-SP                  =   1
                 MOVE    WRK-TANKA-S2(IDY:1)
                                         TO  WRK-TANKA-Z3(IDY:1)
             END-IF
           END-PERFORM
           IF  WRK-TANKA-Z3          NOT =   SPACE
               MOVE    "."               TO  WRK-TANKA-Z2
           END-IF
      *        
           INITIALIZE                      WRK-CHAR-HENKAN-AREA
           MOVE    WRK-TANKA-X         TO  WRK-CHAR-X
           MOVE    12                  TO  WRK-CHAR-CNT
           MOVE    1                   TO  WRK-CHAR-IDX
           MOVE    ZERO                TO  WRK-CHAR-N-CNT
      *     
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   WRK-CHAR-CNT
               IF      WRK-CHAR-X (IDW:1)  =   SPACE
                   CONTINUE
               ELSE        
                   IF    WRK-CHAR-X (IDW:1)  IS  NUMERIC
                       IF      WRK-CHAR-X (IDW:1)  =   "0"
                           MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE
                           MOVE    WRK-CHAR-X (IDW:1)  TO  NUM-IDX
                           MOVE    TBL-SUJI (NUM-IDX)
                                       TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF                    
                   ELSE
                       IF    WRK-CHAR-X (IDW:1)  =   "."
                           MOVE    "．"   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       ELSE    
                           MOVE    "  "   TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                       END-IF
                   END-IF    
                   ADD     2           TO  WRK-CHAR-IDX
                                           WRK-CHAR-N-CNT
               END-IF    
           END-PERFORM
      *
           IF      WRK-CHAR-N-CNT      >   ZERO
                   MOVE    "円"        TO  WRK-CHAR-N (WRK-CHAR-IDX:)
                   ADD     2           TO  WRK-CHAR-N-CNT
           END-IF                             
      *
           .
       2101212-RECE45-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責特定器材点数編集処理
      *****************************************************************
       2101213-JIBAI-KIZAI-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE     ZERO                   TO    WRK-TEN-KZI
           MOVE     ZERO                   TO    FLG-TAIGAI
      *
           IF   RECE41-TAXKBN-KZI   =   "2"
      *
      *       システム管理マスタ（４００３）にて消費税対象外コードか判別
              INITIALIZE                      SYS-4003-REC
              MOVE    "4003"              TO  SYS-4003-KANRICD
              MOVE    "*"                 TO  SYS-4003-KBNCD
              MOVE    RECE41-SRYYM        TO  SYS-4003-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-4003-STYUKYMD(7:2)
              MOVE    SYS-4003-STYUKYMD   TO  SYS-4003-EDYUKYMD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-4003-HOSPNUM
              MOVE    SYS-4003-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO  SYS-4003-REC
                  PERFORM   VARYING   IDX5    FROM   1   BY   1
                             UNTIL   (IDX5                 >    50 )
                               OR    (SYS-4003-SRYCD(IDX5) =  SPACE)
                               OR    (FLG-TAIGAI           =     1 )
                      IF   SYS-4003-SRYCD(IDX5)  =  SRY-SRYCD(IDX2)
                          MOVE     1       TO    FLG-TAIGAI
                      END-IF
                  END-PERFORM
              END-IF
              IF      TNS-NAMECHGKBN  =  ZERO OR 5
                  CONTINUE
              ELSE
                  MOVE     1       TO    FLG-TAIGAI
              END-IF
      *
      *       特定器材等消費税計算
              IF   FLG-TAIGAI  =  ZERO
                   IF    WRK-TAX1001-KBN  =  "1"
                    IF  SRY-SRYSYUKBN = "809"
                      COMPUTE  WRK-TEN-KZI  =
                              (SRY-JIHIMONEY(IDX2)
                                        *  1.05)  +  0.5
                    ELSE
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)  +  0.5
                    END-IF
                   ELSE
                    IF  SRY-SRYSYUKBN = "809"
                      COMPUTE  WRK-TEN-KZI  =
                              (SRY-JIHIMONEY(IDX2)
                                        *  1.05)
                    ELSE
                      COMPUTE  WRK-TEN-KZI  =
                              (TNS-TEN  *  1.05)
                    END-IF
                   END-IF
              ELSE
                   IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
                   ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
                   END-IF
              END-IF
           ELSE
              IF  SRY-SRYSYUKBN = "809"
                      MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-KZI
              ELSE
                      MOVE  TNS-TEN              TO   WRK-TEN-KZI
              END-IF
           END-IF
      *
           .
       2101213-JIBAI-KIZAI-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責文書料点数編集処理
      *****************************************************************
       2101214-JIBAI-BUNSYO-TENSU-HENSYU-SEC     SECTION.
      *
           MOVE        ZERO          TO    WRK-TEN-BUN
      *
           IF   RECE41-TAXKBN   =   "2"
              IF    WRK-TAX1001-KBN  =  "1"
               IF  SRY-SRYSYUKBN = "809"
                  COMPUTE  WRK-TEN-BUN  =
                          (SRY-JIHIMONEY(IDX2)
                                    *  1.05)  +  0.5
               ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)  +  0.5
               END-IF
              ELSE
               IF  SRY-SRYSYUKBN = "809"
                  COMPUTE  WRK-TEN-BUN  =
                          (SRY-JIHIMONEY(IDX2)
                                    *  1.05)
               ELSE
                  COMPUTE  WRK-TEN-BUN  =
                          (TNS-TEN  *  1.05)
               END-IF
              END-IF
           ELSE
               IF  SRY-SRYSYUKBN = "809"
                  MOVE  SRY-JIHIMONEY(IDX2)  TO   WRK-TEN-BUN
               ELSE
                  MOVE  TNS-TEN              TO   WRK-TEN-BUN
               END-IF
           END-IF
      *
           .
       2101214-JIBAI-BUNSYO-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       210122-RECE41-HENSYU-SEC               SECTION.
      *
      *    特定疾患処方管理加算の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-TOKUTEI-SKAN
           MOVE   ZERO              TO   WRK-TOKUTEI-SKAN2
      *    処方の乳幼児加算の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-NYUYOUJI-SKAN
      *    処方せん(乳幼児加算)の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-NYUYOUJI-SHOKAN
      *    処方料（麻・向・毒・覚）の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-MADOKU-SHOHO
      *    長期投薬加算の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-CYOKI-SKAN
           MOVE   ZERO              TO   WRK-CYOKI-SKAN2
      *    抗悪性腫瘍剤処方管理加算の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-AKUSEI-SKAN
                                         WRK-AKUSEI-SKAN2
      *    一般名処方加算の算定回数減算用領域の初期化
           MOVE   ZERO              TO   WRK-IPPAN-SKAN2
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       IF      RECE41-HKNKBN   NOT  =   "3"
                          MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       ELSE
                          MOVE  WRK-ZAIKAISU-XXX  TO   WK-ZAIKAISU
                       END-IF 
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF   RECE41-HKNKBN    NOT  =  "4"                
                         IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                              "40" OR "50" OR "54" OR
                                              "60" OR "70" OR "80" OR
                                              "64"
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                  COMPUTE   WK-YKZTEN =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                         END-IF
                       ELSE                                             
                         IF    SRY-SRYKBN  =  "31" OR "32" OR "33" OR
                                              "40" OR "50" OR "54" OR
                                              "60" OR "70" OR "80" OR
                                              "64"
                                  MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN  
                                  COMPUTE   WK-YKZTEN =                 
                                                      ACCT-KIZAITEN  +  
                                                      ACCT-YKZTEN       
                                  IF  SRY-SRYKBN  =  "70"
                                     COMPUTE  WK-SYUTEN  =  WK-SYUTEN
                                                         -  ACCT-SYUTEN2
                                     COMPUTE  WK-YKZTEN  =  WK-YKZTEN
                                                         +  ACCT-SYUTEN2
                                  END-IF
                                  IF  WRK-YOUBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-YOUBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-YOUBUKASAN
                                  END-IF
                                  IF  WRK-KYOBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KYOBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KYOBUKASAN
                                  END-IF
                                  IF  WRK-KEIBUKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-KEIBUKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-KEIBUKASAN
                                  END-IF
                                  IF  WRK-SOGAIKASAN  NOT =  ZERO
                                   COMPUTE  WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-SOGAIKASAN
                                   COMPUTE  WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-SOGAIKASAN
                                  END-IF
                                  IF  WRK-RYOYOTEATE  NOT =  ZERO
                                   IF  RECE41-JIBAIKBN    =  "2"
                                    COMPUTE WK-SYUTEN  = WK-SYUTEN
                                                       - WRK-RYOYOTEATE
                                    COMPUTE WK-YKZTEN  = WK-YKZTEN
                                                       + WRK-RYOYOTEATE
                                   END-IF
                                  END-IF
                         END-IF
                       END-IF                                           
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                           IF   RECE41-HKNKBN   =  "4"
                             IF  WRK-YOUBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-YOUBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-YOUBUKASAN
                             END-IF
                             IF  WRK-KYOBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KYOBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KYOBUKASAN
                             END-IF
                             IF  WRK-KEIBUKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-KEIBUKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-KEIBUKASAN
                             END-IF
                             IF  WRK-SOGAIKASAN  NOT =  ZERO
                                COMPUTE  WK-SYUTEN   =  WK-SYUTEN
                                                     -  WRK-SOGAIKASAN
                                COMPUTE  WK-KIZAITEN =  WK-KIZAITEN
                                                     +  WRK-SOGAIKASAN
                             END-IF
                           END-IF
                       END-IF
                   ELSE
                       IF ( SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"  OR  "64" )
                       OR ((ACCT-ZAIKBN  =   2  ) AND
                           (SRY-SRYKBN   =  "12"))
                           MOVE  ZERO          TO   WK-ZAIKAISU
                           MOVE  ZERO          TO   WK-ZAITEN
                           MOVE  ZERO          TO   WK-SYUTEN
                           MOVE  ZERO          TO   WK-KIZAITEN
                           MOVE  ZERO          TO   WK-YKZTEN
                           MOVE  ZERO          TO   FLG-OSINKBN
                       END-IF
                   END-IF
               ELSE
                     MOVE    ZERO     TO    WK-ZAIKAISU
                     MOVE    ZERO     TO    WK-ZAITEN
                     MOVE    ZERO     TO    WK-SYUTEN
                     MOVE    ZERO     TO    WK-KIZAITEN
                     MOVE    ZERO     TO    WK-YKZTEN
                     MOVE    ZERO     TO    FLG-OSINKBN
               END-IF
               IF  ((ACCT-ZAITEN    NOT =    ZERO) AND
                    (ACCT-ZAIKAISU  NOT =    ZERO))
               OR  ( FLG-TAISYO         =     1   )
                  EVALUATE   RECE41-HKNKBN
                    WHEN       "1"
                    WHEN       "2"
                        PERFORM   2101221-RECE-LEFT-SET-SEC
                    WHEN       "3"
                        PERFORM   2101222-RECE-LEFT-SET2-SEC
                    WHEN       "4"
                        EVALUATE  WRK-PARA-RECEKBN
                          WHEN      "4"
      ********************* （新様式）
                            PERFORM   2101223-RECE-LEFT-SET3-SEC
                          WHEN      "5"
      ********************* （従来様式）
                            PERFORM   2101224-RECE-LEFT-SET4-SEC
                        END-EVALUATE
                    WHEN       "5"
                        PERFORM   2101225-RECE-LEFT-SET5-SEC
                  END-EVALUATE
               END-IF
               IF  ( SRY-SRYKBN   =  "21"   OR   "22"   OR   "23"   OR
                                     "31"   OR   "32"   OR   "33"   OR
                                     "40"   OR   "50"   OR   "54"   OR
                                     "60"   OR   "70"   OR   "14"   OR
                                     "64"          )
               OR  ((ACCT-ZAIKBN  =    2          ) AND
                    (SRY-SRYKBN   =   "12" OR "13"))
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
           EVALUATE   RECE41-HKNKBN
             WHEN       "1"
             WHEN       "2"
      *
      *         処方の算定回数から特定疾患処方管理加算の減算
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -
                                                   WRK-TOKUTEI-SKAN
      *
      *         処方の算定回数から処方(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -
                                                   WRK-NYUYOUJI-SKAN           
      *
      *         処方の算定回数から長期投薬加算（処方料)の減算
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         処方の算定回数から抗悪性腫瘍剤処方管理加算（処方料）の減算
                COMPUTE    RECE41-KAISU(19)   =    RECE41-KAISU(19)  -
                                                   WRK-AKUSEI-SKAN
      *
      *         処方の算定回数から特定疾患処方管理加算(処方料)の減算
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-TOKUTEI-SKAN2           
      *
      *         処方せんの算定回数から処方せん(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-NYUYOUJI-SHOKAN      
      *
      *         処方せんの算定回数から長期投薬加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         処方せんの算定回数から抗悪性腫瘍剤処方管理加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-AKUSEI-SKAN2
      *
      *         処方せんの算定回数から一般名処方加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(33)   =    RECE41-KAISU(33)  -
                                                   WRK-IPPAN-SKAN2
      *
      *         麻毒の算定回数から処方料（麻・向・毒・覚）の減算
                COMPUTE    RECE41-KAISU(20)   =    RECE41-KAISU(20)  -
                                                   WRK-MADOKU-SHOHO  
      *
             WHEN       "3"
      *
      *         処方の算定回数から特定疾患処方管理加算の減算
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -
                                                   WRK-TOKUTEI-SKAN
      *
      *         処方の算定回数から処方(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -
                                                   WRK-NYUYOUJI-SKAN           
      *
      *         処方の算定回数から長期投薬加算（処方料)の減算
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         処方の算定回数から抗悪性腫瘍剤処方管理加算（処方料）の減算
                COMPUTE    RECE41-KAISU(12)   =    RECE41-KAISU(12)  -
                                                   WRK-AKUSEI-SKAN
      *
      *         処方の算定回数から特定疾患処方管理加算(処方料)の減算
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-TOKUTEI-SKAN2           
      *
      *         処方せんの算定回数から処方せん(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-NYUYOUJI-SHOKAN      
      *
      *         処方せんの算定回数から長期投薬加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         処方せんの算定回数から抗悪性腫瘍剤処方管理加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-AKUSEI-SKAN2
      *
      *         処方せんの算定回数から一般名処方加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(30)   =    RECE41-KAISU(30)  -
                                                   WRK-IPPAN-SKAN2
      *
      *         麻毒の算定回数から処方料（麻・向・毒・覚）の減算
                COMPUTE    RECE41-KAISU(13)   =    RECE41-KAISU(13)  -
                                                   WRK-MADOKU-SHOHO  
      *
             WHEN       "4"
      *
                EVALUATE  WRK-PARA-RECEKBN
                  WHEN      "4"
      **************（新様式）
      *             処方の算定回数から特定疾患処方管理加算の減算
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)  -  
                                                    WRK-TOKUTEI-SKAN     
      *             処方の算定回数から処方(乳幼児)加算の減算
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)  -  
                                                    WRK-NYUYOUJI-SKAN    
      *             処方の算定回数から長期投薬加算（処方料)の減算
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)  -  
                                                    WRK-CYOKI-SKAN       
      *             処方の算定回数から抗悪性腫瘍剤処方管理加算（処方料）の減算
                    COMPUTE    RECE41-KAISU(14)  =  RECE41-KAISU(14)  -
                                                    WRK-AKUSEI-SKAN
      *             処方の算定回数から特定疾患処方管理加算(処方料)の減算
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)  -  
                                                    WRK-TOKUTEI-SKAN2    
      *             処方せんの算定回数から処方せん(乳幼児)加算の減算
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)  -  
                                                    WRK-NYUYOUJI-SHOKAN  
      *             処方せんの算定回数から長期投薬加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)  -  
                                                    WRK-CYOKI-SKAN2      
      *             処方せんの算定回数から抗悪性腫瘍剤処方管理加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)  -
                                                    WRK-AKUSEI-SKAN2
      *             処方せんの算定回数から一般名処方加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(33)  =  RECE41-KAISU(33)  -
                                                    WRK-IPPAN-SKAN2
      *             麻毒の算定回数から処方料（麻・向・毒・覚）の減算
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)  -  
                                                    WRK-MADOKU-SHOHO     
                  WHEN      "5"
      **************（従来様式）
      *             処方の算定回数から特定疾患処方管理加算の減算
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)  -  
                                                    WRK-TOKUTEI-SKAN     
      *             処方の算定回数から処方(乳幼児)加算の減算
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)  -  
                                                    WRK-NYUYOUJI-SKAN    
      *             処方の算定回数から長期投薬加算（処方料)の減算
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)  -  
                                                    WRK-CYOKI-SKAN       
      *             処方の算定回数から抗悪性腫瘍剤処方管理加算（処方料）の減算
                    COMPUTE    RECE41-KAISU(15)  =  RECE41-KAISU(15)  -
                                                    WRK-AKUSEI-SKAN
      *             処方の算定回数から特定疾患処方管理加算(処方料)の減算
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)  -  
                                                    WRK-TOKUTEI-SKAN2    
      *             処方せんの算定回数から処方せん(乳幼児)加算の減算
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)  -  
                                                    WRK-NYUYOUJI-SHOKAN  
      *             処方せんの算定回数から長期投薬加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)  -  
                                                    WRK-CYOKI-SKAN2      
      *             処方せんの算定回数から抗悪性腫瘍剤処方管理加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)  -
                                                    WRK-AKUSEI-SKAN2
      *             処方せんの算定回数から一般名処方加算(処方せん料)の減算
                    COMPUTE    RECE41-KAISU(35)  =  RECE41-KAISU(35)  -
                                                    WRK-IPPAN-SKAN2
      *             麻毒の算定回数から処方料（麻・向・毒・覚）の減算
                    COMPUTE    RECE41-KAISU(16)  =  RECE41-KAISU(16)  -  
                                                    WRK-MADOKU-SHOHO     
                END-EVALUATE
      *
             WHEN       "5"
      *
      *         処方の算定回数から特定疾患処方管理加算の減算
                COMPUTE    RECE41-KAISU(22)   =    RECE41-KAISU(22)  -
                                                   WRK-TOKUTEI-SKAN
      *
      *         処方の算定回数から処方(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(22)   =    RECE41-KAISU(22)  -
                                                   WRK-NYUYOUJI-SKAN           
      *
      *         処方の算定回数から長期投薬加算（処方料)の減算
                COMPUTE    RECE41-KAISU(22)   =    RECE41-KAISU(22)  -  
                                                   WRK-CYOKI-SKAN       
      *
      *         処方の算定回数から抗悪性腫瘍剤処方管理加算（処方料）の減算
                COMPUTE    RECE41-KAISU(22)   =    RECE41-KAISU(22)  -
                                                   WRK-AKUSEI-SKAN
      *
      *         処方の算定回数から特定疾患処方管理加算(処方料)の減算
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)  -
                                                   WRK-TOKUTEI-SKAN2           
      *
      *         処方せんの算定回数から処方せん(乳幼児)加算の減算
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)  -
                                                   WRK-NYUYOUJI-SHOKAN      
      *
      *         処方せんの算定回数から長期投薬加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)  -  
                                                   WRK-CYOKI-SKAN2      
      *
      *         処方せんの算定回数から抗悪性腫瘍剤処方管理加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)  -
                                                   WRK-AKUSEI-SKAN2
      *
      *         処方せんの算定回数から一般名処方加算(処方せん料)の減算
                COMPUTE    RECE41-KAISU(36)   =    RECE41-KAISU(36)  -
                                                   WRK-IPPAN-SKAN2
      *
      *         麻毒の算定回数から処方料（麻・向・毒・覚）の減算
                COMPUTE    RECE41-KAISU(23)   =    RECE41-KAISU(23)  -
                                                   WRK-MADOKU-SHOHO  
      *
           END-EVALUATE
      *
           .                                           
      *
       210122-RECE41-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       2101221-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1 OR
                                          CONS-ROUSAI18
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2 OR
                                          CONS-ROUSAI3    
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  OR
                                             CONS-ROUSAI19
                         ADD   WK-ZAIKAISU      TO   RECE41-KAISU2(2)
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                         IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                         ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO  RECE41-KHNTEN(2)
                               MOVE   1    TO  FLG-KHNTEN2
                            END-IF
                         END-IF
                         IF   FLG-COUNT-ARI  =  ZERO
                           ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                           MOVE   1             TO   FLG-COUNT-ARI
                         END-IF
                         COMPUTE    RECE41-TENSU(2)  =
                                    RECE41-TENSU(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      （外来管理加算）
                   WHEN  122
                        IF    RECE41-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                        ELSE
                           IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE41-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE    RECE41-TENSU(3)  =
                                   RECE41-TENSU(3)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （時間外）
                   WHEN  123
                        IF    RECE41-KHNTEN(4)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                        ELSE
                           IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(4)
                              MOVE   1    TO  FLG-KHNTEN4
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （休日）
                   WHEN  124
                        IF    RECE41-KHNTEN(5)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                        ELSE
                           IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(5)
                              MOVE   1     TO   FLG-KHNTEN5
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU   TO
                                              RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       （深夜）
                   WHEN  125
                        IF    RECE41-KHNTEN(6)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                        ELSE
                           IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO
                                            RECE41-KHNTEN(6)
                              MOVE   1       TO   FLG-KHNTEN6
                           END-IF
                        END-IF
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6) =
                                 RECE41-TENSU(6) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       （その他）
                   WHEN  OTHER 
                        IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI4  
                           ADD   WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO         TO   RECE41-KHNTEN(2)
                           MOVE   1            TO   FLG-KHNTEN2
                           COMPUTE    WRK-RECE41G-TENSU(2)  =
                                      WRK-RECE41G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17    )
                   OR ((SRY-SRYCD(IDX3)  =  CONS-ROUSAI8    ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(7)  =
                                    WRK-RECE41G-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE41G-TENSU(12)  =
                              WRK-RECE41G-TENSU(12)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU(9)  =
                                 RECE41-TENSU(9)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(12)  =
                                 RECE41-TENSU(12)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            薬剤
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)  =
                                 RECE41-TENSU(14)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(15)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(15)
                       ELSE
                          IF   RECE41-KHNTEN(15) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(15)
                               MOVE   1       TO    FLG-KHNTEN15
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(15)
                       COMPUTE   RECE41-TENSU(15)  =
                                 RECE41-TENSU(15)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(17)
                       COMPUTE   RECE41-TENSU(17)  =
                                 RECE41-TENSU(17)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(18)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(18)
                       ELSE
                          IF   RECE41-KHNTEN(18) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(18)
                               MOVE   1       TO    FLG-KHNTEN18
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                       COMPUTE   RECE41-TENSU(18)  =
                                 RECE41-TENSU(18)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)   =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
      ***          IF    RECE41-KHNTEN(19)   =   ZERO
      ***             MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(19)
      ***          ELSE
      ***             IF   RECE41-KHNTEN(19) NOT = WRK-TNS-TEN(IDX3)
      ***             AND  WRK-TNS-TEN(IDX3) NOT = ZERO
      ***                  MOVE   ZERO    TO    RECE41-KHNTEN(19)
      ***                  MOVE   1       TO    FLG-KHNTEN19
      ***             END-IF
      ***          END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(19)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(19)
                     ELSE
                      IF   RECE41-KHNTEN(19) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(19)
                            MOVE   1      TO   FLG-KHNTEN19
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(19)
                   END-IF
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(20)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(22)
                   COMPUTE   RECE41-TENSU(22)  =
                             RECE41-TENSU(22)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(23)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(24)
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(28)  =
                             RECE41-TENSU(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(30)  =
                             RECE41-TENSU(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(32)  =
                             RECE41-TENSU(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )  AND         
                      ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       COMPUTE    RECE41-KINGAKU2(4)  =                 
                                  RECE41-KINGAKU2(4)  +                 
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)  
                       IF   FLG-TAISYO   =   1
                          COMPUTE WRK-ZAITENNASHI  =
                                  WRK-ZAITENNASHI  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                   ELSE               
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                     COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2 +
                                                   ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                     COMPUTE WRK-NYUYOUJI-SHOKAN = WRK-NYUYOUJI-SHOKAN +
                                                  ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3         
                     COMPUTE  WRK-CYOKI-SKAN2  =   WRK-CYOKI-SKAN2   +  
                                                   ACCT-ZAIKAISU
                    END-IF                                              
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN4
                     COMPUTE  WRK-AKUSEI-SKAN2 =   WRK-AKUSEI-SKAN2  +
                                                   ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN5
                     COMPUTE  WRK-IPPAN-SKAN2  =   WRK-IPPAN-SKAN2   +
                                                   ACCT-ZAIKAISU
                    END-IF
                    COMPUTE   RECE41-TENSU(34)  =
                              RECE41-TENSU(34)  +
                             (WK-SYUTEN   *   WK-ZAIKAISU)
                    IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                              CONS-SHOHOSEN2 OR
                                              CONS-SHOHOSEN3 OR
                                              CONS-SHOHOSEN4 OR
                                              CONS-SHOHOKSN1 OR
                                              CONS-SHOHOKSN2 OR
                                              CONS-SHOHOKSN3 OR
                                              CONS-SHOHOKSN4 OR
                                              CONS-SHOHOKSN5
                         ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(33)
                         COMPUTE   RECE41-TENSU(33)  =
                                   RECE41-TENSU(33)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE   RECE41-TENSU(34)  =
                                   RECE41-TENSU(34)  -
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                    ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(7)  =
                                     RECE41-TENSU(7)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           COMPUTE   RECE41-TENSU(34) =
                                     RECE41-TENSU(34) -
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(35)  =
                              RECE41-TENSU(35)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101221-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       2101222-RECE-LEFT-SET2-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     IF   SRY-SRYCD(IDX3) = "111012370"
                       CONTINUE
                     ELSE
                       ADD   WK-ZAIKAISU   TO   RECE41-KAISU(1)
                     END-IF
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1 OR
                                          CONS-ROUSAI18
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                        MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(1)
                   END-IF  
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  OR
                                             CONS-ROUSAI19
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         MOVE    RECE41-TEKSTYMD    
                                           TO   RECE41-SYOSAIYMD2(2)
                     ELSE
                         IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                            IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(3)
                              MOVE  1            TO  FLG-COUNT-ARI
                            END-IF
                            COMPUTE    RECE41-TENSU(3)  =
                                       RECE41-TENSU(3)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         ELSE
                            IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                            END-IF
                            COMPUTE    RECE41-TENSU(2)  =
                                       RECE41-TENSU(2)  +
                                 (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         END-IF
                     END-IF
      *      （時間外）
                   WHEN  123
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "1"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      （休日）
                   WHEN  124
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "2"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      （深夜）
                   WHEN  125
                     IF      RECE41-KEIZOKU-PRTKBN     =   "2"
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        ADD   WK-ZAIKAISU  TO
                                            RECE41-KAISU(4)
                        COMPUTE RECE41-TENSU(4) =
                                RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     ELSE
                        MOVE  "3"     TO   RECE41-SAISHINTIME 
                        IF   FLG-COUNT-ARI  =  ZERO
                              ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                              MOVE  1            TO  FLG-COUNT-ARI
                        END-IF
                        COMPUTE RECE41-TENSU(2) =
                                RECE41-TENSU(2) +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF
      *      （その他）
                   WHEN  OTHER 
                     IF   ACCT-ZAIKBN     =  2
                        COMPUTE WRK-RECE41G-TENSU(3)  =
                                WRK-RECE41G-TENSU(3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                   IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(5)  =
                                    WRK-RECE41G-TENSU(5)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                         COMPUTE    RECE41-TENSU(5)  =
                                    RECE41-TENSU(5)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE41G-TENSU(6)  =
                              WRK-RECE41G-TENSU(6)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(6)  =
                                 RECE41-TENSU(6)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            薬剤
                   COMPUTE   RECE41-TENSU(6)  =
                             RECE41-TENSU(6)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(7)
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(8)
                       COMPUTE   RECE41-TENSU(8)  =
                                 RECE41-TENSU(8)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(9)
                   COMPUTE   RECE41-TENSU(9)  =
                             RECE41-TENSU(9)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)  =
                                 RECE41-TENSU(11)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)   =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(12)
                   END-IF
                   COMPUTE   RECE41-TENSU(12)  =
                             RECE41-TENSU(12)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(13)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(13)  =
                             RECE41-TENSU(13)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(15)
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(16)
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   COMPUTE   RECE41-TENSU(17)  =
                             RECE41-TENSU(17)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                   END-IF
                   COMPUTE   RECE41-TENSU(18)  =
                             RECE41-TENSU(18)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(21)
                   END-IF
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(23)  =
                             RECE41-TENSU(23)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(24)
                   END-IF
                   COMPUTE   RECE41-TENSU(24)  =
                             RECE41-TENSU(24)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(24)  =
                                   RECE41-KAISU(24)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(24)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(24)  =
                                        RECE41-KAISU(24)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )  AND         
                      ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
      ***              CONTINUE
                       IF   FLG-TAISYO   =   1
                          COMPUTE  WRK-ZAITENNASHI  =
                                   WRK-ZAITENNASHI  +
                                  (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                       END-IF
                   ELSE               
                    COMPUTE   RECE41-TENSU(31)  =
                              RECE41-TENSU(31)  +
                             (WK-SYUTEN   *   WK-ZAIKAISU)
                    IF   SRY-SRYCD(IDX3)   =      CONS-SHOHOSEN1 OR
                                                  CONS-SHOHOSEN2 OR
                                                  CONS-SHOHOSEN3 OR
                                                  CONS-SHOHOSEN4 OR
                                                  CONS-SHOHOKSN1 OR
                                                  CONS-SHOHOKSN2 OR
                                                  CONS-SHOHOKSN3 OR
                                                  CONS-SHOHOKSN4 OR
                                                  CONS-SHOHOKSN5
                         COMPUTE   RECE41-TENSU(30)  =
                                   RECE41-TENSU(30)  +
                                  (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         COMPUTE   RECE41-TENSU(31)  =
                                   RECE41-TENSU(31)  -
                                  (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                    ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(5)  =
                                     RECE41-TENSU(5)  +
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                           COMPUTE   RECE41-TENSU(31) =
                                     RECE41-TENSU(31) -
                                (WRK-TNS-TEN(IDX3) * WRK-ZAIKAISU-XXX)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(32)  =
                              RECE41-TENSU(32)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101222-RECE-LEFT-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       2101223-RECE-LEFT-SET3-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)  =
                                RECE41-TENSU(1)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1 OR
                                          CONS-ROUSAI18
                        COMPUTE    RECE41-KINGAKU2(1)  =
                                   RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2 OR
                                          CONS-ROUSAI3    
                        COMPUTE    RECE41-KINGAKU2(4)  =
                                   RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF  
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                   WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  OR
                                             CONS-ROUSAI19
                         IF  SRY-SRYCD(IDX3) = CONS-ROUSAI19
                           CONTINUE
                         ELSE
                           ADD    WK-ZAIKAISU   TO   RECE41-KAISU2(2)
                         END-IF
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)  =
                                    RECE41-TENSU(7)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF
      *
      *      （外来管理加算）
                   WHEN  122
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(2)
                        COMPUTE    RECE41-TENSU(2)  =
                                   RECE41-TENSU(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （時間外）
                   WHEN  123
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE RECE41-TENSU(3) =
                                RECE41-TENSU(3) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （休日）
                   WHEN  124
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4) =
                                 RECE41-TENSU(4) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       （深夜）
                   WHEN  125
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5) =
                                 RECE41-TENSU(5) +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *       （その他）
                   WHEN  OTHER 
                        IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI4  
                           ADD   WK-ZAIKAISU TO   RECE41-KAISU2(3)
                           COMPUTE    RECE41-KINGAKU2(3)  =
                                      RECE41-KINGAKU2(3)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           COMPUTE    WRK-RECE41G-TENSU(7)  =
                                      WRK-RECE41G-TENSU(7)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17    )
                   OR ((SRY-SRYCD(IDX3)  =  CONS-ROUSAI8    ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(6)  =
                                    WRK-RECE41G-TENSU(6)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(6)  =
                                    RECE41-TENSU(6)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE41G-TENSU(7)  =
                              WRK-RECE41G-TENSU(7)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(7)  =
                                 RECE41-TENSU(7)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            薬剤
                   COMPUTE   RECE41-TENSU-YAK(7)  =
                             RECE41-TENSU-YAK(7)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(9)
                       COMPUTE   RECE41-TENSU-YAK(9)  =
                                 RECE41-TENSU-YAK(9)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(10)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(10)
                       ELSE
                          IF   RECE41-KHNTEN(10) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(10)
                               MOVE   1       TO    FLG-KHNTEN10-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)  =
                                 RECE41-TENSU(10)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                   COMPUTE   RECE41-TENSU-YAK(11)  =
                             RECE41-TENSU-YAK(11)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                       COMPUTE   RECE41-TENSU-YAK(12)  =
                                 RECE41-TENSU-YAK(12)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(13)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(13)
                       ELSE
                          IF   RECE41-KHNTEN(13) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(13)
                               MOVE   1       TO    FLG-KHNTEN13-JI
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)  =
                                 RECE41-TENSU(13)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)   =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
      ***          IF    RECE41-KHNTEN(14)   =   ZERO
      ***             MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(14)
      ***          ELSE
      ***             IF   RECE41-KHNTEN(14) NOT = WRK-TNS-TEN(IDX3)
      ***             AND  WRK-TNS-TEN(IDX3) NOT = ZERO
      ***                  MOVE   ZERO    TO    RECE41-KHNTEN(14)
      ***                  MOVE   1       TO    FLG-KHNTEN14-JI
      ***             END-IF
      ***          END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(14)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(14)
                     ELSE
                      IF   RECE41-KHNTEN(14) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(14)
                            MOVE   1      TO   FLG-KHNTEN14-JI
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(14)
                   END-IF
                   COMPUTE   RECE41-TENSU(14)  =
                             RECE41-TENSU(14)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(15)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(15)  =
                             RECE41-TENSU(15)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(16)
                   END-IF
                   COMPUTE   RECE41-TENSU(16)  =
                             RECE41-TENSU(16)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)  =
                             RECE41-TENSU(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)  =
                             RECE41-TENSU(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(21)
                   COMPUTE   RECE41-TENSU(21)  =
                             RECE41-TENSU(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(22)  =
                             RECE41-TENSU-YAK(22)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(25)
                   END-IF
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(26)  =
                             RECE41-TENSU-YAK(26)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(27)
                   END-IF
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(28)  =
                             RECE41-TENSU-YAK(28)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)  =
                             RECE41-TENSU(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU-YAK(30)  =
                             RECE41-TENSU-YAK(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(31)
                   END-IF
                   COMPUTE   RECE41-TENSU(31)  =
                             RECE41-TENSU(31)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU-YAK(32)  =
                             RECE41-TENSU-YAK(32)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ( ACCT-SRYKBN          =  "80"    )  AND         
                       ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                     EVALUATE  WRK-TNS-JIBAISYUKEIKBN(IDX3)
                       WHEN       0
                       WHEN       5
                         COMPUTE    RECE41-KINGAKU2(6)  =
                                    RECE41-KINGAKU2(6)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       1
                         COMPUTE    RECE41-KINGAKU2(1)  =
                                    RECE41-KINGAKU2(1)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       2
                         COMPUTE    RECE41-KINGAKU2(2)  =
                                    RECE41-KINGAKU2(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       3
                         ADD    WK-ZAIKAISU   TO   RECE41-KAISU2(3)
                         COMPUTE    RECE41-KINGAKU2(3)  =
                                    RECE41-KINGAKU2(3)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       WHEN       4
                         COMPUTE    RECE41-KINGAKU2(4)  =
                                    RECE41-KINGAKU2(4)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-EVALUATE
                    ELSE                                                
                     IF  SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE41-KINGAKU2(6)  =                 
                                  RECE41-KINGAKU2(6)  +                 
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                       COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2
                                                  +  ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                       COMPUTE  WRK-NYUYOUJI-SHOKAN = 
                                                    WRK-NYUYOUJI-SHOKAN
                                                  + ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3       
                       COMPUTE  WRK-CYOKI-SKAN2  =  WRK-CYOKI-SKAN2     
                                                 +  ACCT-ZAIKAISU
                      END-IF                                            
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN4
                       COMPUTE  WRK-AKUSEI-SKAN2 =  WRK-AKUSEI-SKAN2
                                                 +  ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN5
                       COMPUTE  WRK-IPPAN-SKAN2  =  WRK-IPPAN-SKAN2
                                                 +  ACCT-ZAIKAISU
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
      **>                     IF    WK-SYUTEN       NOT =   ZERO
      **>                       ADD  WK-ZAIKAISU  TO  RECE41-KAISU(25)
      **>                     END-IF
                              COMPUTE   RECE41-TENSU(25)  =
                                        RECE41-TENSU(25)  +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(34)  =
                                        RECE41-TENSU(34)  +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF   SRY-SRYCD(IDX3)    =    CONS-SHOHOSEN1 OR
                                                   CONS-SHOHOSEN2 OR
                                                   CONS-SHOHOSEN3 OR
                                                   CONS-SHOHOSEN4 OR
                                                   CONS-SHOHOKSN1 OR
                                                   CONS-SHOHOKSN2 OR
                                                   CONS-SHOHOKSN3 OR
                                                   CONS-SHOHOKSN4 OR
                                                   CONS-SHOHOKSN5
                          ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(33)
                          COMPUTE   RECE41-TENSU(33)  =
                                    RECE41-TENSU(33)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
                              COMPUTE   RECE41-TENSU(25)  =
                                        RECE41-TENSU(25)  -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(34)  =
                                        RECE41-TENSU(34)  -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          END-EVALUATE
                      ELSE
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(6)  =
                                      RECE41-TENSU(6)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
                              COMPUTE   RECE41-TENSU(25)  =
                                        RECE41-TENSU(25)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(34)  =
                                        RECE41-TENSU(34)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            END-EVALUATE
                          END-IF
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
                          COMPUTE   RECE41-TENSU-YAK(26)  =
                                    RECE41-TENSU-YAK(26)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                       WHEN      "1"
                          COMPUTE   RECE41-TENSU-YAK(35)  =
                                    RECE41-TENSU-YAK(35)  +
                                   (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101223-RECE-LEFT-SET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       2101224-RECE-LEFT-SET4-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                 EVALUATE   RECE41-JIBAIKBN
                  WHEN       "1"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     COMPUTE    RECE41-TENSU(1)    =
                                RECE41-TENSU(1)    +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     COMPUTE    TBL-SYUTEN-JI(1)   =
                                TBL-SYUTEN-JI(1)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1 OR
                                          CONS-ROUSAI18
      ****              COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                         RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                   END-IF
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI2 OR
                                          CONS-ROUSAI3
      ****              COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                         RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                        COMPUTE    WRK-KINGAKU-JI   =
                                   WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)   * ACCT-ZAIKAISU)
                   END-IF
                  WHEN       "2"
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                         WHEN    6
                             MOVE  "1"     TO   RECE41-SHOSHINTIME
                         WHEN    3
                             MOVE  "2"     TO   RECE41-SHOSHINTIME
                         WHEN    4
                             MOVE  "3"     TO   RECE41-SHOSHINTIME
                     END-EVALUATE
                     IF  SRY-SRYCD(IDX3)  =  CONS-NYUKSN1  OR
                                             CONS-NYUKSN2  OR
                                             CONS-NYUKSN3  OR
                                             CONS-NYUKSN4  OR
                                             CONS-NYUKSN5  OR
                                             CONS-NYUKSN6  OR
                                             CONS-NYUKSN7  OR
                                             CONS-NYUKSN8  OR
                                             CONS-NYUKSN9
                             MOVE  "1"     TO   RECE41-SHOSHIN-NYU
                     END-IF
                   END-IF
                   COMPUTE    RECE41-TENSU(1)    =
                              RECE41-TENSU(1)    +
                              (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE    TBL-SYUTEN-JI(1)   =
                              TBL-SYUTEN-JI(1)   +
                              (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                 END-EVALUATE
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE   RECE41-JIBAIKBN
                  WHEN       "1"
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                    WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  OR
                                             CONS-ROUSAI19
      ****               ADD   WK-ZAIKAISU      TO   RECE41-KAISU(2)
      ****               COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                          RECE41-SONOTAKIN-JYU  +
      ****                     (WRK-TNS-TEN(IDX3)    * ACCT-ZAIKAISU)
                         COMPUTE    WRK-KINGAKU-JI   =
                                    WRK-KINGAKU-JI   +
                               (WRK-TNS-TEN(IDX3)    * ACCT-ZAIKAISU)
                     ELSE
                      IF  (SRY-SRYCD(IDX3) =  CONS-JIKANTAI1  OR
                                              CONS-JIKANTAI2  OR
                                              CONS-JIKANTAI3  OR
                                              CONS-MESAIKSN     )
                      AND (RECE41-SRYYM   >=  "201204"          )
                         COMPUTE    RECE41-TENSU(8)   =
                                    RECE41-TENSU(8)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(8)  =
                                    TBL-SYUTEN-JI(8)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      ELSE
      **                 IF ( SRY-SRYCD(IDX3)     =   CONS-KEIZOKU1  OR
      **                                              CONS-KEIZOKU2 )
      **                 OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"  )
      **                     CONTINUE
      **                 ELSE
                             IF   FLG-COUNT-ARI  =  ZERO
                               ADD   WK-ZAIKAISU  TO  RECE41-KAISU(2)
                               MOVE  1            TO  FLG-COUNT-ARI
                             END-IF
      **                 END-IF
                         IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                          ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                            END-IF
                          END-IF
                         END-IF
                         COMPUTE    RECE41-TENSU(2)   =
                                    RECE41-TENSU(2)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(2)  =
                                    TBL-SYUTEN-JI(2)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      END-IF
                     END-IF
      *      （外来管理加算）
                    WHEN  122
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(3)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                          ELSE
                            IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(3)
                              MOVE   1       TO    FLG-KHNTEN3-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE  RECE41-TENSU(3)   =
                                 RECE41-TENSU(3)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(3)  =
                                 TBL-SYUTEN-JI(3)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （時間外）
                    WHEN  123
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(4)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                          ELSE
                            IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(4)
                              MOVE   1       TO    FLG-KHNTEN4-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4)   =
                                 RECE41-TENSU(4)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(4)  =
                                 TBL-SYUTEN-JI(4)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （休日）
                    WHEN  124
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(5)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                          ELSE
                            IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(5)
                              MOVE   1       TO    FLG-KHNTEN5-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(5)  =
                                 TBL-SYUTEN-JI(5)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （深夜）
                    WHEN  125
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(6)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                          ELSE
                            IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(6)
                              MOVE   1       TO    FLG-KHNTEN6-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(6)  =
                                 TBL-SYUTEN-JI(6)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （その他）
                    WHEN  OTHER
                        IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI4
      ****                 COMPUTE    RECE41-SONOTAKIN-JYU  =
      ****                            RECE41-SONOTAKIN-JYU  +
      ****                       (WRK-TNS-TEN(IDX3)   *  ACCT-ZAIKAISU)
                           COMPUTE    WRK-KINGAKU-JI  =
                                      WRK-KINGAKU-JI  +
                                 (WRK-TNS-TEN(IDX3)   *  ACCT-ZAIKAISU)
                        END-IF
                        IF   ACCT-ZAIKBN     =  2
                           IF  WRK-PARA-JIBAIPRTKBN = "1"
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                           END-IF
                           COMPUTE    WRK-RECE41G-TENSU(2)  =
                                      WRK-RECE41G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
                  WHEN       "2"
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                    WHEN  120
                      IF    SRY-SRYCD(IDX3) =  CONS-DOUJITU-SAI OR
                                               CONS-DOUJITU-DEN OR
                                               CONS-DOUJITU-GAI
                        ADD   WK-ZAIKAISU  TO  CNT-SAISIN-DOUJITU
                        COMPUTE    RECE41-TENSU(2)   =
                                   RECE41-TENSU(2)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                      ELSE
                       IF  (SRY-SRYCD(IDX3) =  CONS-JIKANTAI1   OR
                                               CONS-JIKANTAI2   OR
                                               CONS-JIKANTAI3   OR
                                               CONS-MESAIKSN      )
                       AND (RECE41-SRYYM   >=  "201204"           )
                        COMPUTE    RECE41-TENSU(8)   =
                                   RECE41-TENSU(8)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(8)  =
                                   TBL-SYUTEN-JI(8)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                        IF   RECE41-SRYYM  >= "200604"
                          IF  SRY-SRYCD(IDX3)  =  "112000970" OR
                                                  "112006270"
                            COMPUTE  CNT-NYUYOJI =  CNT-NYUYOJI
                                                 +  WK-ZAIKAISU
                          END-IF
                        END-IF
                        IF ( SRY-SRYCD(IDX3)     =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                        OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"  )
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(2)
                            ADD   WK-ZAIKAISU  TO   CNT-SAISIN-ETC
                        END-IF
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(2)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                          ELSE
                            IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                            END-IF
                          END-IF
                        END-IF
                        COMPUTE    RECE41-TENSU(2)   =
                                   RECE41-TENSU(2)   +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE    TBL-SYUTEN-JI(2)  =
                                   TBL-SYUTEN-JI(2)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                      END-IF
      *      （外来管理加算）
                    WHEN  122
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(3)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                          ELSE
                            IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(3)
                              MOVE   1       TO    FLG-KHNTEN3-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(3)
                        COMPUTE  RECE41-TENSU(3)   =
                                 RECE41-TENSU(3)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(3)  =
                                 TBL-SYUTEN-JI(3)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （時間外）
                    WHEN  123
                        PERFORM   2101225-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(4)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(4)
                          ELSE
                            IF RECE41-KHNTEN(4) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(4)
                              MOVE   1       TO    FLG-KHNTEN4-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(4)
                        COMPUTE  RECE41-TENSU(4)   =
                                 RECE41-TENSU(4)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(4)  =
                                 TBL-SYUTEN-JI(4)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （休日）
                    WHEN  124
                        PERFORM   2101225-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(5)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                          ELSE
                            IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(5)
                              MOVE   1       TO    FLG-KHNTEN5-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(5)  =
                                 TBL-SYUTEN-JI(5)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （深夜）
                    WHEN  125
                        PERFORM   2101225-NYUYOJI-COUNT-SEC
                        IF  WRK-PARA-JIBAIPRTKBN = "1"
                          IF    RECE41-KHNTEN(6)   =   ZERO
                            MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                          ELSE
                            IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO    RECE41-KHNTEN(6)
                              MOVE   1       TO    FLG-KHNTEN6-JI2
                            END-IF
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                        COMPUTE  TBL-SYUTEN-JI(6)  =
                                 TBL-SYUTEN-JI(6)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （その他）
                    WHEN  OTHER
                        IF   ACCT-ZAIKBN     =  2
                           IF  WRK-PARA-JIBAIPRTKBN = "1"
                              MOVE   ZERO    TO    RECE41-KHNTEN(2)
                              MOVE   1       TO    FLG-KHNTEN2-JI2
                           END-IF
                           COMPUTE    WRK-RECE41G-TENSU(2)  =
                                      WRK-RECE41G-TENSU(2)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                 EVALUATE   RECE41-JIBAIKBN
                  WHEN       "1"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17    )
                   OR ((SRY-SRYCD(IDX3)  =  CONS-ROUSAI8    ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         COMPUTE    RECE41-SONOTAKIN-JYU  =
                                    RECE41-SONOTAKIN-JYU  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(7)  =
                                    WRK-RECE41G-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)   =
                                    RECE41-TENSU(7)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(7)  =
                                    TBL-SYUTEN-JI(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
                  WHEN       "2"
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(7)  =
                                    WRK-RECE41G-TENSU(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(7)   =
                                    RECE41-TENSU(7)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                         COMPUTE    TBL-SYUTEN-JI(7)  =
                                    TBL-SYUTEN-JI(7)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                 END-EVALUATE
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE41G-TENSU(8)  =
                              WRK-RECE41G-TENSU(8)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(8)  =
                                 TBL-SYUTEN-JI(8)  +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            薬剤
                   COMPUTE   RECE41-TENSU(8)   =
                             RECE41-TENSU(8)   +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(8)  =
                             TBL-YKZTEN-JI(8)  +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(10)  =
                                 TBL-YKZTEN-JI(10)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(11)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(11)
                       ELSE
                          IF   RECE41-KHNTEN(11) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(11)
                               MOVE   1       TO    FLG-KHNTEN11-JI2
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)   =
                                 RECE41-TENSU(11)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(11)  =
                                 TBL-SYUTEN-JI(11)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                   COMPUTE   RECE41-TENSU(12)   =
                             RECE41-TENSU(12)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(12)  =
                             TBL-YKZTEN-JI(12)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)   =
                                 RECE41-TENSU(13)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                       COMPUTE   TBL-YKZTEN-JI(13)  =
                                 TBL-YKZTEN-JI(13)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(14)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(14)
                       ELSE
                          IF   RECE41-KHNTEN(14) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(14)
                               MOVE   1       TO    FLG-KHNTEN14-JI2
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(14)
                       COMPUTE   RECE41-TENSU(14)   =
                                 RECE41-TENSU(14)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       COMPUTE   TBL-SYUTEN-JI(14)  =
                                 TBL-SYUTEN-JI(14)  +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)   =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
      ***          IF    RECE41-KHNTEN(15)   =   ZERO
      ***             MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(15)
      ***          ELSE
      ***             IF   RECE41-KHNTEN(15) NOT = WRK-TNS-TEN(IDX3)
      ***             AND  WRK-TNS-TEN(IDX3) NOT = ZERO
      ***                  MOVE   ZERO    TO    RECE41-KHNTEN(15)
      ***                  MOVE   1       TO    FLG-KHNTEN15-JI2
      ***             END-IF
      ***          END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(15)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(15)
                     ELSE
                      IF   RECE41-KHNTEN(15) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(15)
                            MOVE   1      TO   FLG-KHNTEN15-JI2
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(15)
                   END-IF
                   COMPUTE   RECE41-TENSU(15)   =
                             RECE41-TENSU(15)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(15)  =
                             TBL-SYUTEN-JI(15)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(16)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(16)   =
                             RECE41-TENSU(16)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(16)  =
                             TBL-SYUTEN-JI(16)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(17)
                   END-IF
                   COMPUTE   RECE41-TENSU(17)   =
                             RECE41-TENSU(17)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(17)  =
                             TBL-SYUTEN-JI(17)  +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(19)
                   COMPUTE   RECE41-TENSU(19)   =
                             RECE41-TENSU(19)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(19)  =
                             TBL-SYUTEN-JI(19)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(19)   =
                             RECE41-TENSU(19)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(19)  =
                             TBL-YKZTEN-JI(19)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(20)
                   COMPUTE   RECE41-TENSU(20)   =
                             RECE41-TENSU(20)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(20)  =
                             TBL-SYUTEN-JI(20)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(20)   =
                             RECE41-TENSU(20)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(20)  =
                             TBL-YKZTEN-JI(20)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(21)
                   COMPUTE   RECE41-TENSU(21)   =
                             RECE41-TENSU(21)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(21)  =
                             TBL-SYUTEN-JI(21)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(21)   =
                             RECE41-TENSU(21)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(21)  =
                             TBL-YKZTEN-JI(21)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(23)
                   END-IF
                   COMPUTE   RECE41-TENSU(23)   =
                             RECE41-TENSU(23)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(23)  =
                             TBL-SYUTEN-JI(23)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(24)   =
                             RECE41-TENSU(24)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(24)  =
                             TBL-YKZTEN-JI(24)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(26)
                   END-IF
                   COMPUTE   RECE41-TENSU(26)   =
                             RECE41-TENSU(26)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(26)  =
                             TBL-SYUTEN-JI(26)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(27)   =
                             RECE41-TENSU(27)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(27)  =
                             TBL-YKZTEN-JI(27)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(29)
                   END-IF
                   COMPUTE   RECE41-TENSU(29)   =
                             RECE41-TENSU(29)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(29)  =
                             TBL-SYUTEN-JI(29)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(29)  =
                                   RECE41-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(29)  =
                                        RECE41-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(30)   =
                             RECE41-TENSU(30)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(30)  =
                             TBL-YKZTEN-JI(30)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(32)
                   END-IF
                   COMPUTE   RECE41-TENSU(32)   =
                             RECE41-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-SYUTEN-JI(32)  =
                             TBL-SYUTEN-JI(32)  +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(33)   =
                             RECE41-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
                   COMPUTE   TBL-YKZTEN-JI(33)  =
                             TBL-YKZTEN-JI(33)  +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )   AND
                      ( SRY-SRYCD(IDX3)(1:5) =  "09591" OR 
                                                "09592" )
                        CONTINUE
                   ELSE
                    IF ( ACCT-SRYKBN          =  "80"    )  AND         
                       ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                     EVALUATE   RECE41-JIBAIKBN
                       WHEN       "1"
                         EVALUATE  WRK-TNS-JIBAISYUKEIKBN(IDX3)
                           WHEN       0
                           WHEN       5
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           WHEN       1
                           WHEN       2
                           WHEN       3
                           WHEN       4
                             COMPUTE  WRK-KINGAKU-JI   =
                                      WRK-KINGAKU-JI   +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         END-EVALUATE
                       WHEN       "2"
                             COMPUTE  RECE41-SONOTAKIN-JYU  =
                                      RECE41-SONOTAKIN-JYU  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-EVALUATE
                    ELSE                                                
                     IF  RECE41-JIBAIKBN      =    "1"
                     AND SRY-SRYCD(IDX3)      =    CONS-RYOYOTEATE
                       COMPUTE    RECE41-SONOTAKIN-JYU  =               
                                  RECE41-SONOTAKIN-JYU  +               
                                 (WRK-RYOYOTEATE  *   ACCT-ZAIKAISU)    
                     ELSE
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                       COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2
                                                  +  ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                       COMPUTE  WRK-NYUYOUJI-SHOKAN = 
                                                    WRK-NYUYOUJI-SHOKAN
                                                  + ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3       
                       COMPUTE  WRK-CYOKI-SKAN2  =  WRK-CYOKI-SKAN2     
                                                 +  ACCT-ZAIKAISU
                      END-IF                                            
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN4
                       COMPUTE  WRK-AKUSEI-SKAN2 =  WRK-AKUSEI-SKAN2
                                                 +  ACCT-ZAIKAISU
                      END-IF
                      IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN5
                       COMPUTE  WRK-IPPAN-SKAN2  =  WRK-IPPAN-SKAN2
                                                 +  ACCT-ZAIKAISU
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                            WHEN      "0"
      **>                     IF    WK-SYUTEN       NOT =   ZERO
      **>                       ADD  WK-ZAIKAISU  TO  RECE41-KAISU(23)
      **>                     END-IF
                              COMPUTE   RECE41-TENSU(23)   =
                                        RECE41-TENSU(23)   +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(23)  =
                                        TBL-SYUTEN-JI(23)  +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                            WHEN      "1"
                              COMPUTE   RECE41-TENSU(36)   =
                                        RECE41-TENSU(36)   +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(36)  =
                                        TBL-SYUTEN-JI(36)  +
                                       (WK-SYUTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                      IF   SRY-SRYCD(IDX3)    =    CONS-SHOHOSEN1 OR
                                                   CONS-SHOHOSEN2 OR
                                                   CONS-SHOHOSEN3 OR
                                                   CONS-SHOHOSEN4 OR
                                                   CONS-SHOHOKSN1 OR
                                                   CONS-SHOHOKSN2 OR
                                                   CONS-SHOHOKSN3 OR
                                                   CONS-SHOHOKSN4 OR
                                                   CONS-SHOHOKSN5
                          ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(35)
                          COMPUTE   RECE41-TENSU(35)   =
                                    RECE41-TENSU(35)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          COMPUTE   TBL-SYUTEN-JI(35)  =
                                    TBL-SYUTEN-JI(35)  +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          EVALUATE  WRK-PARA-JIBAIPRTKBN
                            WHEN      "0"
                              COMPUTE   RECE41-TENSU(23)   =
                                        RECE41-TENSU(23)   -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(23)  =
                                        TBL-SYUTEN-JI(23)  -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            WHEN      "1"
                              COMPUTE   RECE41-TENSU(36)   =
                                        RECE41-TENSU(36)   -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(36)  =
                                        TBL-SYUTEN-JI(36)  -
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                          END-EVALUATE
                      ELSE
                          IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                            COMPUTE   RECE41-TENSU(7)   =
                                      RECE41-TENSU(7)   +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            COMPUTE   TBL-SYUTEN-JI(7)  =
                                      TBL-SYUTEN-JI(7)  +
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            EVALUATE  WRK-PARA-JIBAIPRTKBN
                             WHEN      "0"
                              COMPUTE   RECE41-TENSU(23)   =
                                        RECE41-TENSU(23)   -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(23)  =
                                        TBL-SYUTEN-JI(23)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                             WHEN      "1"
                              COMPUTE   RECE41-TENSU(36)   =
                                        RECE41-TENSU(36)   -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                              COMPUTE   TBL-SYUTEN-JI(36)  =
                                        TBL-SYUTEN-JI(36)  -
                                     (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                            END-EVALUATE
                          END-IF
                      END-IF
                      EVALUATE  WRK-PARA-JIBAIPRTKBN
                       WHEN      "0"
                        IF  SRY-SRYCD(IDX3) = CONS-RYOYOTEATE
                         COMPUTE   RECE41-TENSU(37)   =
                                   RECE41-TENSU(37)   +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(37)  =
                                   TBL-YKZTEN-JI(37)  +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                        ELSE
                         COMPUTE   RECE41-TENSU(24)   =
                                   RECE41-TENSU(24)   +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(24)  =
                                   TBL-YKZTEN-JI(24)  +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                        END-IF
                       WHEN      "1"
                         COMPUTE   RECE41-TENSU(37)   =
                                   RECE41-TENSU(37)   +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                         COMPUTE   TBL-YKZTEN-JI(37)  =
                                   TBL-YKZTEN-JI(37)  +
                                  (WK-YKZTEN   *   WK-ZAIKAISU)
                      END-EVALUATE
                     END-IF
                    END-IF                                              
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101224-RECE-LEFT-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送
      *****************************************************************
       2101225-RECE-LEFT-SET5-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   SRY-SRYCD(IDX3) = CONS-ROUSAI1  OR
                                          CONS-ROUSAI18 OR
                                          CONS-ROUSAI2  OR
                                          CONS-ROUSAI3
                      CONTINUE
                   ELSE
                      IF   WRK-TNS-DATAKBN(IDX3)  =   2
                        EVALUATE  WRK-TNS-TIMEKSNKBN(IDX3)
                          WHEN    1
                          WHEN    5
                          WHEN    6
                              MOVE  "1"     TO   RECE41-SHOSHINTIME
                          WHEN    3
                              MOVE  "2"     TO   RECE41-SHOSHINTIME
                          WHEN    4
                              MOVE  "3"     TO   RECE41-SHOSHINTIME
                        END-EVALUATE
                      END-IF
                      IF    RECE41-KOMUKBN  =   "2"
                        IF  WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"
                            CONTINUE
                        ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(1)
                        END-IF
                      ELSE
                        IF  WRK-TNS-DATAKBN(IDX3)  =   2
                          IF   SRY-SRYCD(IDX3) = "111012370"
                            CONTINUE
                          ELSE
                            ADD   WK-ZAIKAISU  TO   RECE41-KAISU(1)
                          END-IF
                        END-IF
                      END-IF
                      COMPUTE    RECE41-TENSU(1)    =
                                 RECE41-TENSU(1)    +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                   EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                    WHEN  120
                     IF   SRY-SRYCD(IDX3) =  CONS-ROUSAI5  OR
                                             CONS-ROUSAI6  OR
                                             CONS-ROUSAI7  OR
                                             CONS-ROUSAI19
                         CONTINUE
                     ELSE
                       IF SRY-SRYCD(IDX3) =  CONS-GAIRAIRYO1  OR
                                             CONS-GAIRAIRYO3  OR
                                             CONS-DOUJITU-GAI
                         IF    RECE41-KHNTEN(3)   =   ZERO
                           MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(3)
                         ELSE
                           IF RECE41-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO    TO    RECE41-KHNTEN(3)
                             MOVE   1       TO    FLG-KHNTEN3-KO
                           END-IF
                         END-IF
                         IF  WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"
                             CONTINUE
                         ELSE
                             ADD   WK-ZAIKAISU  TO   RECE41-KAISU(3)
                         END-IF
                         COMPUTE    RECE41-TENSU(3)   =
                                    RECE41-TENSU(3)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       ELSE
                         IF    RECE41-KHNTEN(2)   =   ZERO
                           MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(2)
                         ELSE
                           IF RECE41-KHNTEN(2) NOT = WRK-TNS-TEN(IDX3)
                             MOVE   ZERO    TO    RECE41-KHNTEN(2)
                             MOVE   1       TO    FLG-KHNTEN2-KO
                           END-IF
                         END-IF
                         IF    RECE41-KOMUKBN  =   "2"
                           IF  WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"
                               CONTINUE
                           ELSE
                               ADD   WK-ZAIKAISU  TO   RECE41-KAISU(2)
                           END-IF
                         ELSE
                           IF   FLG-COUNT-ARI  =  ZERO
                               ADD   WK-ZAIKAISU  TO   RECE41-KAISU(2)
                               MOVE  1            TO   FLG-COUNT-ARI
                           END-IF
                         END-IF
                         COMPUTE    RECE41-TENSU(2)   =
                                    RECE41-TENSU(2)   +
                                   (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                     END-IF
      *      （外来管理加算）
                    WHEN  122
                        IF    RECE41-KHNTEN(5)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(5)
                        ELSE
                          IF RECE41-KHNTEN(5) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(5)
                            MOVE   1       TO    FLG-KHNTEN5-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(5)
                        COMPUTE  RECE41-TENSU(5)   =
                                 RECE41-TENSU(5)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （時間外）
                    WHEN  123
                        IF    RECE41-KHNTEN(6)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(6)
                        ELSE
                          IF RECE41-KHNTEN(6) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(6)
                            MOVE   1       TO    FLG-KHNTEN6-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(6)
                        COMPUTE  RECE41-TENSU(6)   =
                                 RECE41-TENSU(6)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （休日）
                    WHEN  124
                        IF    RECE41-KHNTEN(7)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(7)
                        ELSE
                          IF RECE41-KHNTEN(7) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(7)
                            MOVE   1       TO    FLG-KHNTEN7-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(7)
                        COMPUTE  RECE41-TENSU(7)   =
                                 RECE41-TENSU(7)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （深夜）
                    WHEN  125
                        IF    RECE41-KHNTEN(8)   =   ZERO
                          MOVE WRK-TNS-TEN(IDX3) TO RECE41-KHNTEN(8)
                        ELSE
                          IF RECE41-KHNTEN(8) NOT = WRK-TNS-TEN(IDX3)
                            MOVE   ZERO    TO    RECE41-KHNTEN(8)
                            MOVE   1       TO    FLG-KHNTEN8-KO
                          END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE41-KAISU(8)
                        COMPUTE  RECE41-TENSU(8)   =
                                 RECE41-TENSU(8)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *      （その他）
                    WHEN  OTHER 
                        IF   ACCT-ZAIKBN     =  2
                           MOVE   ZERO    TO    RECE41-KHNTEN(3)
                           MOVE   1       TO    FLG-KHNTEN3-KO
                           COMPUTE    WRK-RECE41G-TENSU(3)  =
                                      WRK-RECE41G-TENSU(3)  +
                                     (WK-ZAITEN   *   WK-ZAIKAISU)
                        END-IF
                   END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                   IF ( SRY-SRYCD(IDX3) =   CONS-ROUSAI9  OR
                                            CONS-ROUSAI10 OR
                                            CONS-ROUSAI11 OR
                                            CONS-ROUSAI12 OR
                                            CONS-ROUSAI13 OR
                                            CONS-ROUSAI14 OR
                                            CONS-ROUSAI15 OR
                                            CONS-ROUSAI16 OR
                                            CONS-ROUSAI17    )
                   OR ((SRY-SRYCD(IDX3)  =  CONS-ROUSAI8    ) AND
                       (RECE41-SRYYM     <  "200804"        ))
                         CONTINUE
                   ELSE
                     IF   ACCT-ZAIKBN     =  2
                         COMPUTE    WRK-RECE41G-TENSU(9)  =
                                    WRK-RECE41G-TENSU(9)  +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                         COMPUTE    RECE41-TENSU(9)   =
                                    RECE41-TENSU(9)   +
                                   (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   END-IF                   
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                 IF  ACCT-ZAIKBN   =    2
                   COMPUTE    WRK-RECE41G-TENSU(14)  =
                              WRK-RECE41G-TENSU(14)  +
                             (WK-ZAITEN   *   WK-ZAIKAISU)
                 ELSE
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(10)
                       COMPUTE   RECE41-TENSU(10)   =
                                 RECE41-TENSU(10)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(11)
                       COMPUTE   RECE41-TENSU(11)   =
                                 RECE41-TENSU(11)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(12)
                       COMPUTE   RECE41-TENSU(12)   =
                                 RECE41-TENSU(12)   +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
      *            在宅患者訪問診療
                   IF (( WRK-TNS-OSINKBN(IDX3) =     1          )  AND
                       ((SRY-SRYCD(IDX3)   NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011970")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114002070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114011870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114012070")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017470")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017570")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017670")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017770")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017870")   AND
                        (SRY-SRYCD(IDX3)   NOT =    "114017970")))
                   OR (  FLG-OSINKBN           =     9           )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU-COMMENT  )
      ***          OR (  SRY-SRYCD(IDX3) =  CONS-ZAIHOU3-COMMENT )
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(13)
                       COMPUTE   RECE41-TENSU(13)   =
                                 RECE41-TENSU(13)   +
                                (WK-SYUTEN         * WK-ZAIKAISU)
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       COMPUTE   RECE41-TENSU(14)   =
                                 RECE41-TENSU(14)   +
                                (WK-SYUTEN     *   WK-ZAIKAISU)
                   END-IF
      *            薬剤
                   COMPUTE   RECE41-TENSU(15)   =
                             RECE41-TENSU(15)   +
                             (WK-KIZAITEN *   WK-ZAIKAISU)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                 END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   -
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(16)
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)   =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE41-KAISU(16)
                       COMPUTE   RECE41-TENSU(16)   =
                                 RECE41-TENSU(16)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                     ELSE   
                       IF    RECE41-KHNTEN(17)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(17)
                       ELSE
                          IF   RECE41-KHNTEN(17) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(17)
                               MOVE   1       TO    FLG-KHNTEN17-KO
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(17)
                       COMPUTE   RECE41-TENSU(17)   =
                                 RECE41-TENSU(17)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE41-KAISU(18)
                   COMPUTE   RECE41-TENSU(18)   =
                             RECE41-TENSU(18)   +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                   OR    (SRY-SRYCD(IDX3) (1:7)  = "0992000"        )
                   OR    (SRY-SRYCD(IDX3)        = "099999903"      )
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(20)
                       COMPUTE   RECE41-TENSU(20)   =
                                 RECE41-TENSU(20)   +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   ELSE
                       IF    RECE41-KHNTEN(21)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(21)
                       ELSE
                          IF   RECE41-KHNTEN(21) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE41-KHNTEN(21)
                               MOVE   1       TO    FLG-KHNTEN21-KO
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(21)
                       COMPUTE   RECE41-TENSU(21)   =
                                 RECE41-TENSU(21)   +
                                (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)   =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  = WRK-TOKUTEI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN = WRK-NYUYOUJI-SKAN +
                                                   WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)   =    "120003170"                
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)   =    "120003370"
                      COMPUTE  WRK-AKUSEI-SKAN =  WRK-AKUSEI-SKAN +
                                                  WK-ZAIKAISU
                   END-IF
      ***          IF    RECE41-KHNTEN(22)   =   ZERO
      ***             MOVE  WRK-TNS-TEN(IDX3)  TO  RECE41-KHNTEN(22)
      ***          ELSE
      ***             IF   RECE41-KHNTEN(22) NOT = WRK-TNS-TEN(IDX3)
      ***             AND  WRK-TNS-TEN(IDX3) NOT = ZERO
      ***                  MOVE   ZERO    TO    RECE41-KHNTEN(22)
      ***                  MOVE   1       TO    FLG-KHNTEN22-KO
      ***             END-IF
      ***          END-IF
                   IF    WK-ZAITEN   NOT =  ZERO
                     IF    RECE41-KHNTEN(22)     = ZERO
                      MOVE  WK-ZAITEN     TO   RECE41-KHNTEN(22)
                     ELSE
                      IF   RECE41-KHNTEN(22) NOT = WK-ZAITEN
                            MOVE   ZERO   TO   RECE41-KHNTEN(22)
                            MOVE   1      TO   FLG-KHNTEN22-KO
                      END-IF
                     END-IF
                   END-IF
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(22)
                   END-IF
                   COMPUTE   RECE41-TENSU(22)   =
                             RECE41-TENSU(22)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   IF  (SRY-SRYCD(IDX3)(1:1)   = "S" OR "8" )
                   OR  (SRY-SRYCD(IDX3)(1:3)   = "008"      )
                   OR  (SRY-SRYCD(IDX3)        = "099999903")
                      CONTINUE
                   ELSE
                      ADD    WK-ZAIKAISU     TO    RECE41-KAISU(23)
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   COMPUTE   RECE41-TENSU(23)   =
                             RECE41-TENSU(23)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   COMPUTE   RECE41-TENSU(24)   =
                             RECE41-TENSU(24)   +
                            (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(25)
                   COMPUTE   RECE41-TENSU(25)  =
                             RECE41-TENSU(25)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(26)
                   COMPUTE   RECE41-TENSU(26)  =
                             RECE41-TENSU(26)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE41-KAISU(27)
                   COMPUTE   RECE41-TENSU(27)  =
                             RECE41-TENSU(27)  +
                            (WK-ZAITEN   *   WK-ZAIKAISU)
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(28)
                   END-IF
                   COMPUTE   RECE41-TENSU(28)   =
                             RECE41-TENSU(28)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(29)   =
                             RECE41-TENSU(29)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(30)
                   END-IF
                   COMPUTE   RECE41-TENSU(30)   =
                             RECE41-TENSU(30)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(31)   =
                             RECE41-TENSU(31)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    検査のレセ点数欄の編集
               WHEN    "60"
               WHEN    "64"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(32)
                   END-IF
                   COMPUTE   RECE41-TENSU(32)   =
                             RECE41-TENSU(32)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE41-KAISU(32)  =
                                   RECE41-KAISU(32)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE41-KAISU(32)  NOT =  ZERO
                              COMPUTE   RECE41-KAISU(32)  =
                                        RECE41-KAISU(32)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   COMPUTE   RECE41-TENSU(33)   =
                             RECE41-TENSU(33)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE41-KAISU(34)
                   END-IF
                   COMPUTE   RECE41-TENSU(34)   =
                             RECE41-TENSU(34)   +
                            (WK-SYUTEN   *   WK-ZAIKAISU)
                   COMPUTE   RECE41-TENSU(35)   =
                             RECE41-TENSU(35)   +
                            (WK-YKZTEN   *   WK-ZAIKAISU)
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF ( ACCT-SRYKBN          =  "80"    )  AND         
                      ( SRY-SRYCD(IDX3)(1:5) =  "09593" )              
                       IF   FLG-TAISYO   =   1
                          COMPUTE WRK-ZAITENNASHI  =
                                  WRK-ZAITENNASHI  +
                                 (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                       END-IF
                   ELSE               
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                     COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2 +
                                                   ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                     COMPUTE WRK-NYUYOUJI-SHOKAN = WRK-NYUYOUJI-SHOKAN +
                                                  ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3         
                     COMPUTE  WRK-CYOKI-SKAN2  =   WRK-CYOKI-SKAN2   +  
                                                   ACCT-ZAIKAISU
                    END-IF                                              
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN4
                     COMPUTE  WRK-AKUSEI-SKAN2 =   WRK-AKUSEI-SKAN2  +
                                                   ACCT-ZAIKAISU
                    END-IF
                    IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN5
                     COMPUTE  WRK-IPPAN-SKAN2  =   WRK-IPPAN-SKAN2   +
                                                   ACCT-ZAIKAISU
                    END-IF
                    COMPUTE   RECE41-TENSU(37)  =
                              RECE41-TENSU(37)  +
                             (WK-SYUTEN   *   WK-ZAIKAISU)
                    IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                              CONS-SHOHOSEN2 OR
                                              CONS-SHOHOSEN3 OR
                                              CONS-SHOHOSEN4 OR
                                              CONS-SHOHOKSN1 OR
                                              CONS-SHOHOKSN2 OR
                                              CONS-SHOHOKSN3 OR
                                              CONS-SHOHOKSN4 OR
                                              CONS-SHOHOKSN5
                         ADD  ACCT-ZAIKAISU  TO  RECE41-KAISU(36)
                         COMPUTE   RECE41-TENSU(36)  =
                                   RECE41-TENSU(36)  +
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         COMPUTE   RECE41-TENSU(37)  =
                                   RECE41-TENSU(37)  -
                                  (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                    ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE41-TENSU(9)  =
                                     RECE41-TENSU(9)  +
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                           COMPUTE   RECE41-TENSU(37) =
                                     RECE41-TENSU(37) -
                                    (WRK-TNS-TEN(IDX3) * ACCT-ZAIKAISU)
                         END-IF
                    END-IF
                    COMPUTE   RECE41-TENSU(38)  =
                              RECE41-TENSU(38)  +
                             (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
      *
           .
      *
       2101225-RECE-LEFT-SET5-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳幼児加算のカウント
      *****************************************************************
       2101225-NYUYOJI-COUNT-SEC               SECTION.
      *
           IF   RECE41-SRYYM  >= "200604"
              IF  SRY-SRYCD(IDX3) = "112015170" OR
                                    "112015270" OR
                                    "112015370" OR
                                    "112015470" OR
                                    "112014470" OR
                                    "112014570" OR
                                    "112014670" OR
                                    "112014770" OR
                                    "112014870" OR
                                    "112014970" OR
                                    "112015070" OR
                                    "112014170" OR
                                    "112014270" OR
                                    "112014370"
                     COMPUTE  CNT-NYUYOJI  =  CNT-NYUYOJI
                                           +  WK-ZAIKAISU
              END-IF
           END-IF
           .
      *
       2101225-NYUYOJI-COUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       210123-RECE45-WRITE-SEC                SECTION.
      *
           MOVE    RECE45-REC          TO   RECE4B-REC      
      *
           WRITE   RECE45-REC
           ADD     1                   TO   CNT-OUT2
      *
           WRITE   RECE4B-REC
           ADD     1                   TO   CNT-OUT5
      *
           .
       210123-RECE45-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       210124-RECE45-REWRITE-SEC              SECTION.                        
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
           END-START
           PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45-RENNUM   =   WRK-RENNUM
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45-SRYCD(IDXB)  NOT = SPACE
                       MOVE WRK-SRYSURYO3 TO RECE45-SRYSURYO(IDXB)
                       MOVE 1             TO IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45-REC
                END-IF
                READ    RECE45-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
           END-PERFORM
      *
           .
       210124-RECE45-REWRITE-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       210124-RECE45-REWRITE2-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-REWRITE
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1         TO  FLG-RECE45
                 NOT INVALID
                     MOVE    ZERO      TO  FLG-RECE45
                     READ    RECE45-FILE   NEXT
                       AT  END
                         MOVE    1     TO  FLG-RECE45
                     END-READ
           END-START
           PERFORM   UNTIL
               (RECE44-HOSPNUM      NOT =  RECE45-HOSPNUM)     OR
               (RECE44-SRYYM        NOT =  RECE45-SRYYM)       OR
               (RECE44-NYUGAIKBN    NOT =  RECE45-NYUGAIKBN)   OR
               (RECE44-PTID         NOT =  RECE45-PTID)        OR
               (RECE44-RECEKA       NOT =  RECE45-RECEKA)      OR
               (RECE44-HKNID        NOT =  RECE45-HKNID)       OR
               (RECE44-TEKSTYMD     NOT =  RECE45-TEKSTYMD)    OR
               (RECE44-ZAINUM       NOT =  RECE45-ZAINUM)      OR
               (FLG-RECE45                =      1        )    OR
               (FLG-REWRITE               =      1        )
                IF    RECE45-RENNUM   =   WRK-RENNUM2
                   MOVE    1              TO  FLG-REWRITE
                   PERFORM  VARYING  IDXB  FROM  5   BY  -1
                             UNTIL   IDXB   <    1
                     IF  RECE45-SRYCD(IDXB)  NOT = SPACE
                       MOVE   "1"  TO   RECE45-RECEPTFLG(IDXB)
                       MOVE    1   TO   IDXB
                     END-IF
                   END-PERFORM
                   REWRITE   RECE45-REC
                END-IF
                READ    RECE45-FILE   NEXT
                  AT  END
                    MOVE    1     TO  FLG-RECE45
                END-READ
           END-PERFORM
      *
           .
       210124-RECE45-REWRITE2-EXT.
           EXIT.                                                       
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       21013-RECE44-WRITE-SEC                SECTION.
      *
           MOVE    RECE44-REC          TO  RECE4A-REC
      *
           COMPUTE  RECE44-ZAITEN      =  RECE44-ZAITEN     -  
                                          WRK-TEN
           COMPUTE  RECE44-MEISAISU    =  RECE44-MEISAISU   -
                                          WRK-MEISAI-CNT
           COMPUTE  RECE44-SYUTEN1     =  RECE44-SYUTEN1    -
                                          WRK-TEN
           COMPUTE  RECE44-SRYCDTOTAL  =  RECE44-SRYCDTOTAL -
                                          WRK-SRYCDTOTAL
           COMPUTE  RECE44-SURYOUTOTAL =  RECE44-SURYOUTOTAL -
                                          WRK-SURYOUTOTAL     
      *
           MOVE     WRK-TEN         TO  RECE4A-ZAITEN
           MOVE     WRK-MEISAI-CNT  TO  RECE4A-MEISAISU
           MOVE     WRK-TEN         TO  RECE4A-SYUTEN1  
           MOVE     WRK-SRYCDTOTAL  TO  RECE4A-SRYCDTOTAL
           MOVE     WRK-SURYOUTOTAL TO  RECE4A-SURYOUTOTAL
      *
           WRITE   RECE44-REC
           ADD     1                TO  CNT-OUT1
      *
           WRITE   RECE4A-REC
           ADD     1                TO  CNT-OUT4
      *
           .
       21013-RECE44-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤の診療内容チェック
      *****************************************************************
       21014-SINRYO-CHECK-SEC                 SECTION.
      *
           PERFORM   VARYING   IDU   FROM   1   BY   1
                      UNTIL  ( IDU   >   TBL-ZAICHECK-MAX )
                       OR    ( FLG-RINYAK  NOT =  ZERO    )
      *
              IF  ( IDU  =  1                                  )
              OR  ((IDU  =  2) AND (ACCT-SRYKBN = "60" OR "64"))
              OR  ((IDU  =  3) AND (ACCT-SRYKBN = "80"        ))
              OR  ((IDU  =  4) AND (ACCT-SRYKBN = "80"        ))
      *
                 INITIALIZE                  SRYACT-REC
                 MOVE RECE41-HOSPNUM     TO  SRY-HOSPNUM
                 MOVE RECE41-NYUGAIKBN   TO  SRY-NYUGAIKBN
                 MOVE RECE41-PTID        TO  SRY-PTID
                 MOVE RECE41-SRYYM       TO  SRY-SRYYM
                 MOVE ACCT-ZAINUM        TO  SRY-ZAINUM
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(1)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(2)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(3)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(4)
                 MOVE TBL-ZAICHECK(IDU)  TO  SRY-SRYCD(5)
                 MOVE SRYACT-REC         TO  MCPDATA-REC
                 MOVE "tbl_sryact"       TO  MCP-TABLE
                 MOVE "key8"             TO  MCP-PATHNAME
grpsys           PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC        =  ZERO
                     MOVE "tbl_sryact"   TO  MCP-TABLE
                     MOVE "key8"         TO  MCP-PATHNAME
                     PERFORM   800-SRYACT-READ-SEC
                     IF     FLG-SRYACT   =  ZERO
                         EVALUATE   IDU
                           WHEN      1
                             MOVE   1   TO  FLG-RINYAK
                           WHEN      2
                           WHEN      3
                           WHEN      4
                             MOVE   2   TO  FLG-RINYAK
                         END-EVALUATE
                     END-IF
                 END-IF
                 MOVE "tbl_sryact"   TO  MCP-TABLE
                 MOVE "key8"         TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
      *
              END-IF
      *
           END-PERFORM
      *
           .
       21014-SINRYO-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療会計付加情報検索
      **************************************************************
       21015-SRYACCTPLUS-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM       TO   ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN     TO   ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID          TO   ACCTP-PTID
           MOVE    ACCT-SRYKA         TO   ACCTP-SRYKA
           MOVE    ACCT-SRYYM         TO   ACCTP-SRYYM
           MOVE    ACCT-ZAINUM        TO   ACCTP-ZAINUM
           MOVE    1                  TO   ACCTP-RENNUM
           MOVE    SRYACCTPLUS-REC    TO   MCPDATA-REC
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
               MOVE    "key"              TO   MCP-PATHNAME
               PERFORM  800-SRYACCTPLUS-READ-SEC
               IF        FLG-SRYACCTPLUS   =   ZERO
                 IF   ACCTP-RSIKINGAKU  NOT = ZERO
                    MOVE    1   TO   FLG-TAISYO
                 END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_sryacctplus"  TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       21015-SRYACCTPLUS-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの基本点数部分のクリア処理
      *****************************************************************
       2102-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN15        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(15)
           END-IF
           IF        FLG-KHNTEN18        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(18)
           END-IF
           IF        FLG-KHNTEN19        =    1
               MOVE    ZERO             TO   RECE41-KHNTEN(19)
           END-IF
           .
       2102-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの基本点数部分のクリア処理（自賠責）
      *****************************************************************
       2103-KHNTEN-JI-CLEAR-SEC               SECTION.
      *
           EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      **********（新様式）
                IF        FLG-KHNTEN10-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(10)
                END-IF
                IF        FLG-KHNTEN13-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(13)
                END-IF
                IF        FLG-KHNTEN14-JI    =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(14)
                END-IF
              WHEN      "5"
      **********（従来様式）
                IF        FLG-KHNTEN2-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(2)
                END-IF
                IF        FLG-KHNTEN3-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(3)
                END-IF
                IF        FLG-KHNTEN4-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(4)
                END-IF
                IF        FLG-KHNTEN5-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(5)
                END-IF
                IF        FLG-KHNTEN6-JI2  =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(6)
                END-IF
                IF        FLG-KHNTEN11-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(11)
                END-IF
                IF        FLG-KHNTEN14-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(14)
                END-IF
                IF        FLG-KHNTEN15-JI2   =    1
                  MOVE    ZERO             TO   RECE41-KHNTEN(15)
                END-IF
      **>       IF   RECE41-JIBAIKBN    =   "2"
      **>        IF   RECE41-SRYYM  >= "200604"
      **>          IF    (CNT-NYUYOJI      NOT =  ZERO        )
      ***          AND   (RECE41-KAISU(2)  NOT =  CNT-NYUYOJI )
      **>          AND   (CNT-SAISIN-ETC   NOT =  CNT-NYUYOJI )
      **>             MOVE    "1"    TO   RECE41-NYUYOJIFLG
      **>          END-IF
      **>        END-IF
      **>       END-IF
           END-EVALUATE
           .
       2103-KHNTEN-JI-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成
      *****************************************************************
       2104-RECE44-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE41-HOSPNUM       TO      ACCT-HOSPNUM
           MOVE    RECE41-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE41-PTID          TO      ACCT-PTID
           MOVE    RECE41-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE41-SRYYM         TO      ACCT-SRYYM
           MOVE    RECE41-HKNCOMBI      TO      ACCT-HKNCOMBI
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "tbl_sryacct"        TO      MCP-TABLE
           MOVE    "key24"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryacct"    TO      MCP-TABLE
               MOVE    "key24"          TO      MCP-PATHNAME
               PERFORM  800-SRYACCT-READ-SEC
               PERFORM  UNTIL     FLG-SRYACCT   =   1 
      *
                 MOVE   ZERO          TO  FLG-RINYAK
      *
                 IF    ACCT-SRYKBN       =  "95"  OR  "96"
                   CONTINUE
                 ELSE
      *
                   MOVE     ZERO    TO    FLG-TAISYO
                   IF   (ACCT-SRYKBN         =   "80" )
                   AND  (ACCT-ZAITEN         =   ZERO )
                   AND  (ACCT-ZAIKAISU   NOT =   ZERO )
                   AND  (ACCT-JIHOKBN        =   SPACE)
                     PERFORM  21015-SRYACCTPLUS-KENSAKU-SEC
                   END-IF
      *
                   MOVE   RECE41-TEKSTYMD(7:2)   TO   WRK-DAY
                   IF  ACCT-DAY(1 WRK-DAY)   NOT =    ZERO
                        MOVE    1       TO   FLG-DAY
                   ELSE
                        MOVE    ZERO    TO   FLG-DAY  
                   END-IF
      *
                   IF      FLG-DAY         =   1
                           IF  ((ACCT-ZAITEN    NOT =    ZERO ) AND
                                (ACCT-ZAIKAISU  NOT =    ZERO ) AND
                                (ACCT-JIHOKBN       =    SPACE))
                           OR  ( FLG-TAISYO         =      1   )
                               MOVE  ACCT-DAY(1 WRK-DAY)  
                                               TO    WRK-ZAIKAISU-XXX
                               MOVE    ZERO    TO    WRK-TEN
                                                     WRK-MEISAI-CNT
                                                     WRK-SRYCDTOTAL
                                                     WRK-SURYOUTOTAL
                                                     WRK-YOUBUKASAN
                                                     WRK-KYOBUKASAN
                                                     WRK-KEIBUKASAN
                                                     WRK-SOGAIKASAN
                                                     WRK-RYOYOTEATE
      *
                               MOVE    ZERO    TO    WRK-SRYSURYO
                                                     WRK-SRYSURYO2
                                                     WRK-SRYSURYO3
                                                     WRK-TANICD
                                                     WRK-TANIMOJISU
                               MOVE    SPACE   TO    WRK-TANINAME
                               MOVE    ZERO    TO    FLG-HOUSIJI-COM
                                                     FLG-HOUTEN-COM
                                                     FLG-SYOBYO-COM
                                                     FLG-RYOYO-COM
                                                     FLG-SYODOU-COM
                                                     FLG-SAIDOU-COM
                                                     FLG-NIKOCHIN-COM
                                                     FLG-ZENSOKU-COM
                                                     FLG-OSIN-COM
                                                     FLG-ZAIHOU-COM
                                                     FLG-TAHOKEN
                                                     FLG-SYOUNI-COM
                                                     FLG-SRYKBN-HEN
                                                     FLG-KENSA-COM
      *
                               MOVE    SPACE   TO    WRK-KYUGYOFLG
                               MOVE    ZERO    TO    FLG-COUNT-ARI
      *
                               MOVE    ZERO    TO
                                               WRK-KZI058-TEN
                                               WRK-KZI058-TANICD
                                               WRK-KZI058-TANIMOJISU
                               MOVE    SPACE   TO
                                               WRK-KZI058-TANINAME
      *
                               MOVE    ZERO    TO    FLG-WRITE45
                               PERFORM   21011-RECE44-TENSOU-SEC
                               PERFORM   21012-RECE45-HENSYU-SEC
                               IF    FLG-WRITE45  =   1
                                 PERFORM 21013-RECE44-WRITE-SEC
                               END-IF
                           ELSE
                             IF  (ACCT-ZAIKAISU  NOT =    ZERO)  AND
                                 (ACCT-JIHOKBN       =   SPACE)
                               IF ( ACCT-SRYKBN  =  "11" OR "12" OR
                                                    "99"          )
                               OR ((RECE41-SRYYM >= "200804"     ) AND
                                   (ACCT-SRYKBN  =  "14"         ))
                               OR ((RECE41-SRYYM >= "200804"     ) AND
                                   (ACCT-SRYKBN  =  "21" OR "22" OR
                                                    "23"         ) AND
                                   (WRK-PARA-PRTKBN  =  "2"      ))
                               OR ( ACCT-ZAIKBN  =   1            )
                                 IF  (ACCT-SRYCDTOTAL(1:12)  =
                                                    "000000999901")
                                 AND (ACCT-MEISAISU      =   1    )
                                      CONTINUE
                                 ELSE
                                      MOVE  ACCT-DAY(1 WRK-DAY)  
                                                  TO  WRK-ZAIKAISU-XXX
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               ELSE
                                 PERFORM   21014-SINRYO-CHECK-SEC
                                 IF   FLG-RINYAK  NOT =  ZERO
                                      MOVE  ACCT-DAY(1 WRK-DAY)  
                                                  TO  WRK-ZAIKAISU-XXX
                                      MOVE  ZERO  TO  WRK-TEN
                                                      WRK-MEISAI-CNT
                                                      WRK-SRYCDTOTAL
                                                      WRK-SURYOUTOTAL
                                                      WRK-YOUBUKASAN
                                                      WRK-KYOBUKASAN
                                                      WRK-KEIBUKASAN
                                                      WRK-SOGAIKASAN
                                                      WRK-RYOYOTEATE
      *
                                      MOVE  ZERO  TO  WRK-SRYSURYO
                                                      WRK-SRYSURYO2
                                                      WRK-SRYSURYO3
                                                      WRK-TANICD
                                                      WRK-TANIMOJISU
                                      MOVE  SPACE TO  WRK-TANINAME
                                      MOVE  ZERO  TO  FLG-HOUSIJI-COM
                                                      FLG-HOUTEN-COM
                                                      FLG-SYOBYO-COM
                                                      FLG-RYOYO-COM
                                                      FLG-SYODOU-COM
                                                      FLG-SAIDOU-COM
                                                      FLG-NIKOCHIN-COM
                                                      FLG-ZENSOKU-COM
                                                      FLG-OSIN-COM
                                                      FLG-ZAIHOU-COM
                                                      FLG-TAHOKEN
                                                      FLG-SYOUNI-COM
                                                      FLG-SRYKBN-HEN
                                                      FLG-KENSA-COM
      *
                                      MOVE  SPACE TO  WRK-KYUGYOFLG
                                      MOVE  ZERO  TO  FLG-COUNT-ARI
      *
                                      MOVE  ZERO  TO
                                                  WRK-KZI058-TEN
                                                  WRK-KZI058-TANICD
                                                  WRK-KZI058-TANIMOJISU
                                      MOVE  SPACE TO
                                                  WRK-KZI058-TANINAME
      *
                                      MOVE  ZERO  TO  FLG-WRITE45
                                      PERFORM   21011-RECE44-TENSOU-SEC
                                      PERFORM   21012-RECE45-HENSYU-SEC
                                      IF    FLG-WRITE45  =   1
                                        PERFORM 21013-RECE44-WRITE-SEC
                                      END-IF
                                 END-IF
                               END-IF
                             END-IF
                           END-IF
                   END-IF
                 END-IF
                 MOVE     "tbl_sryacct"    TO      MCP-TABLE
                 MOVE     "key24"          TO      MCP-PATHNAME
                 PERFORM   800-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2104-RECE44-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括点数の治験減点分編集
      *****************************************************************
       2107-GENTEN-HENSYU-SEC      SECTION.
      *
           IF   RECE41-TENSU(2)  >=  WRK-RECE41G-TENSU(2)
             COMPUTE  RECE41-TENSU(2)  =
                      RECE41-TENSU(2)  -  WRK-RECE41G-TENSU(2)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(2)
           END-IF
           IF   RECE41-TENSU(3)  >=  WRK-RECE41G-TENSU(3)
             COMPUTE  RECE41-TENSU(3)  =
                      RECE41-TENSU(3)  -  WRK-RECE41G-TENSU(3)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(3)
           END-IF
           IF   RECE41-TENSU(5)  >=  WRK-RECE41G-TENSU(5)
             COMPUTE  RECE41-TENSU(5)  =
                      RECE41-TENSU(5)  -  WRK-RECE41G-TENSU(5)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(5)
           END-IF
           IF   RECE41-TENSU(6)  >=  WRK-RECE41G-TENSU(6)
             COMPUTE  RECE41-TENSU(6)  =
                      RECE41-TENSU(6)  -  WRK-RECE41G-TENSU(6)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(6)
           END-IF
           IF   RECE41-TENSU(7)  >=  WRK-RECE41G-TENSU(7)
             COMPUTE  RECE41-TENSU(7)  =
                      RECE41-TENSU(7)  -  WRK-RECE41G-TENSU(7)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(7)
           END-IF
           IF   RECE41-TENSU(8)  >=  WRK-RECE41G-TENSU(8)
             COMPUTE  RECE41-TENSU(8)  =
                      RECE41-TENSU(8)  -  WRK-RECE41G-TENSU(8)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(8)
           END-IF
           IF   RECE41-TENSU(9)  >=  WRK-RECE41G-TENSU(9)
             COMPUTE  RECE41-TENSU(9)  =
                      RECE41-TENSU(9)  -  WRK-RECE41G-TENSU(9)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(9)
           END-IF
           IF   RECE41-TENSU(12) >=  WRK-RECE41G-TENSU(12)
             COMPUTE  RECE41-TENSU(12) =
                      RECE41-TENSU(12) -  WRK-RECE41G-TENSU(12)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(12)
           END-IF
           IF   RECE41-TENSU(14) >=  WRK-RECE41G-TENSU(14)
             COMPUTE  RECE41-TENSU(14) =
                      RECE41-TENSU(14) -  WRK-RECE41G-TENSU(14)
           ELSE
             MOVE     ZERO    TO   RECE41-TENSU(14)
           END-IF
      *
      *    合計点数の編集
           IF   RECE41-HKNKBN     NOT  =   "4"
              COMPUTE  RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                        -  WRK-RECE41G-TENSU(2)
                                        -  WRK-RECE41G-TENSU(3)
                                        -  WRK-RECE41G-TENSU(5)
                                        -  WRK-RECE41G-TENSU(6)
                                        -  WRK-RECE41G-TENSU(7)
                                        -  WRK-RECE41G-TENSU(8)
                                        -  WRK-RECE41G-TENSU(9)
                                        -  WRK-RECE41G-TENSU(12)
                                        -  WRK-RECE41G-TENSU(14)
           END-IF
      *
           .
       2107-GENTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理
      *****************************************************************
       2105-RECE41-REWRITE-SEC                SECTION.
      *
           IF   RECE41-HKNKBN     NOT  =   "4"
             COMPUTE  RECE41-TOTALKIN2  =  RECE41-TOTALKIN2
                                        +  WRK-ZAITENNASHI
             IF    RECE41-HKNKBN   =   "5"
             AND   RECE41-KOMUKBN  =   "2"
               COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                              RECE41-KOMUTENTANKA
             ELSE
               COMPUTE  RECE41-TOTALKIN   =   RECE41-TOTALTEN   *
                                              RECE41-TANKA
             END-IF
           END-IF
      *
           IF   RECE41-HKNKBN     =   "4"                               
      *
             EVALUATE  WRK-PARA-RECEKBN
              WHEN      "4"
      ********（新様式）
      *
               COMPUTE   RECE41-TENSU(8)      =  RECE41-TENSU(1)        
                                              +  RECE41-TENSU(2)        
                                              +  RECE41-TENSU(3)        
                                              +  RECE41-TENSU(4)        
                                              +  RECE41-TENSU(5)        
                                              +  RECE41-TENSU(6)        
                                              +  RECE41-TENSU(7)        
      *
               COMPUTE   RECE41-TENSU-YAK(8)  =  RECE41-TENSU-YAK(7)    
      *
               COMPUTE   RECE41-TENSU(17)     =  RECE41-TENSU(10)       
                                              +  RECE41-TENSU(13)       
                                              +  RECE41-TENSU(14)       
                                              +  RECE41-TENSU(15)       
                                              +  RECE41-TENSU(16)       
      *
               COMPUTE   RECE41-TENSU-YAK(18) =  RECE41-TENSU-YAK(9)    
                                              +  RECE41-TENSU-YAK(11)   
                                              +  RECE41-TENSU-YAK(12)   
      *
               COMPUTE   RECE41-TENSU(23)     =  RECE41-TENSU(19)       
                                              +  RECE41-TENSU(20)       
                                              +  RECE41-TENSU(21)       
      *
               COMPUTE   RECE41-TENSU-YAK(24) =  RECE41-TENSU-YAK(22)   
      *
               COMPUTE   WRK-KSNRATE          =  RECE41-KSNRATE / 100   
      *
               PERFORM    VARYING    ID1    FROM    1   BY   1          
                       UNTIL      ID1    >     31                    
      *              
                  ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                  ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN-YAK
      *
               END-PERFORM
      *
      *        減点編集
               COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                          -  WRK-RECE41G-TENSU(2)
                                          -  WRK-RECE41G-TENSU(3)
                                          -  WRK-RECE41G-TENSU(5)
                                          -  WRK-RECE41G-TENSU(6)
                                          -  WRK-RECE41G-TENSU(7)
                                          -  WRK-RECE41G-TENSU(8)
                                          -  WRK-RECE41G-TENSU(9)
                                          -  WRK-RECE41G-TENSU(12)
                                          -  WRK-RECE41G-TENSU(14)
      *
               COMPUTE   RECE41-TOTALKIN   =  
                  (RECE41-TOTALTEN  *  RECE41-TANKA  *  WRK-KSNRATE) 
                                                  +  0.5
      *
               COMPUTE   RECE41-TOTALKIN-YAK =  
                  (RECE41-TOTALTEN-YAK  *  RECE41-TANKA)  
                                                  +  0.5
      *
               COMPUTE   RECE41-KINGAKU2(5)   =  RECE41-KINGAKU2(1)     
                                              +  RECE41-KINGAKU2(2)     
                                              +  RECE41-KINGAKU2(3)     
                                              +  RECE41-KINGAKU2(4)     
      *
      ***      COMPUTE   RECE41-TOTALKIN2     =  RECE41-KINGAKU2(5)     
      ***                                     *  WRK-KSNRATE            
      *
               COMPUTE   RECE41-TOTALKIN2     =  
                  (RECE41-KINGAKU2(5)   *  WRK-KSNRATE)  +  0.5         
      *
               COMPUTE   RECE41-TOTALKIN2-ETC =  RECE41-KINGAKU2(6)     
                                              +  RECE41-SINDAN-KIN      
                                              +  RECE41-MEISAI-KIN      
      *
              WHEN      "5"
      ********（従来様式）
      *
               COMPUTE   RECE41-TENSU(9)      =  RECE41-TENSU(1)
                                              +  RECE41-TENSU(2)
                                              +  RECE41-TENSU(3)
                                              +  RECE41-TENSU(4)
                                              +  RECE41-TENSU(5)
                                              +  RECE41-TENSU(6)
                                              +  RECE41-TENSU(7)
                                              +  RECE41-TENSU(8)
      *
               COMPUTE   RECE41-TENSU(18)     =  RECE41-TENSU(10)
                                              +  RECE41-TENSU(11)
                                              +  RECE41-TENSU(12)
                                              +  RECE41-TENSU(13)
                                              +  RECE41-TENSU(14)
                                              +  RECE41-TENSU(15)
                                              +  RECE41-TENSU(16)
                                              +  RECE41-TENSU(17)
      *
               COMPUTE   RECE41-TENSU(22)     =  RECE41-TENSU(19)
                                              +  RECE41-TENSU(20)
                                              +  RECE41-TENSU(21)
      *
               COMPUTE   RECE41-TENSU(25)     =  RECE41-TENSU(23)
                                              +  RECE41-TENSU(24)
      *
               COMPUTE   RECE41-TENSU(28)     =  RECE41-TENSU(26)
                                              +  RECE41-TENSU(27)
      *
               COMPUTE   RECE41-TENSU(31)     =  RECE41-TENSU(29)
                                              +  RECE41-TENSU(30)
      *
               COMPUTE   RECE41-TENSU(34)     =  RECE41-TENSU(32)
                                              +  RECE41-TENSU(33)
      *
               COMPUTE   RECE41-TENSU(38)     =  RECE41-TENSU(35)
                                              +  RECE41-TENSU(36)
                                              +  RECE41-TENSU(37)
      *
      ***      COMPUTE   RECE41-TOTALTEN      =  RECE41-TENSU(9)
      ***                                     +  RECE41-TENSU(18)
      ***                                     +  RECE41-TENSU(22)
      ***                                     +  RECE41-TENSU(25)
      ***                                     +  RECE41-TENSU(28)
      ***                                     +  RECE41-TENSU(31)
      ***                                     +  RECE41-TENSU(34)
      ***                                     +  RECE41-TENSU(38)
      *
               MOVE       ZERO         TO        WRK-SYUTEN-JI
                                                 WRK-YKZTEN-JI
               PERFORM    VARYING    ID1    FROM    1   BY   1
                           UNTIL     ID1     >     31
                  ADD   RECE41-SYUTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-SYUTEN-JI
                  ADD   RECE41-YKZTEN-JI(ID1)  TO  RECE41-TOTALTEN
                                                   WRK-YKZTEN-JI
               END-PERFORM
      *        減点編集
               COMPUTE   RECE41-TOTALTEN  =  RECE41-TOTALTEN
                                          -  WRK-RECE41G-TENSU(2)
                                          -  WRK-RECE41G-TENSU(3)
                                          -  WRK-RECE41G-TENSU(5)
                                          -  WRK-RECE41G-TENSU(6)
                                          -  WRK-RECE41G-TENSU(7)
                                          -  WRK-RECE41G-TENSU(8)
                                          -  WRK-RECE41G-TENSU(9)
                                          -  WRK-RECE41G-TENSU(12)
                                          -  WRK-RECE41G-TENSU(14)
               COMPUTE   WRK-SYUTEN-JI    =  WRK-SYUTEN-JI
                                          -  WRK-RECE41G-TENSU(2)
                                          -  WRK-RECE41G-TENSU(3)
                                          -  WRK-RECE41G-TENSU(5)
                                          -  WRK-RECE41G-TENSU(6)
                                          -  WRK-RECE41G-TENSU(7)
                                          -  WRK-RECE41G-TENSU(8)
                                          -  WRK-RECE41G-TENSU(9)
                                          -  WRK-RECE41G-TENSU(12)
                                          -  WRK-RECE41G-TENSU(14)
      *
               IF   TBL-SYUTEN-JI(2)  >=  WRK-RECE41G-TENSU(2)
                 COMPUTE  TBL-SYUTEN-JI(2) =
                          TBL-SYUTEN-JI(2) -  WRK-RECE41G-TENSU(2)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(2)
               END-IF
               IF   TBL-SYUTEN-JI(7)  >=  WRK-RECE41G-TENSU(7)
                 COMPUTE  TBL-SYUTEN-JI(7) =
                          TBL-SYUTEN-JI(7) -  WRK-RECE41G-TENSU(7)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(7)
               END-IF
               IF   TBL-SYUTEN-JI(8)  >=  WRK-RECE41G-TENSU(8)
                 COMPUTE  TBL-SYUTEN-JI(8) =
                          TBL-SYUTEN-JI(8) -  WRK-RECE41G-TENSU(8)
               ELSE
                 MOVE     ZERO    TO   TBL-SYUTEN-JI(8)
               END-IF
      *
               EVALUATE   RECE41-JIBAIKBN
      *
                 WHEN       "1"
      *
                   COMPUTE   WRK-KSNRATE   =  RECE41-KSNRATE / 100
      *
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-SYUTEN-JI  *  RECE41-TANKA  *  WRK-KSNRATE)
                                                       +  0.5
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-YKZTEN-JI  *  RECE41-TANKA) +  0.5
      *
                   COMPUTE  RECE41-SONOTAKIN-JYU  =
                            RECE41-SONOTAKIN-JYU  +
                      (WRK-KINGAKU-JI *  WRK-KSNRATE)  +  0.5
      ***
                   PERFORM    VARYING    ID1    FROM    1   BY   1
                               UNTIL     ID1     >     38
                     IF   ID1  =   9  OR  18  OR  22  OR  25  OR
                                  28  OR  31  OR  34  OR  38
                        CONTINUE
                     ELSE
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-SYUTEN-JI(ID1) * RECE41-TANKA
                                            * WRK-KSNRATE )
                                                           + 0.5
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-YKZTEN-JI(ID1) * RECE41-TANKA)
                                                           + 0.5
                     END-IF
                   END-PERFORM
      *
                 WHEN       "2"
      *
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-SYUTEN-JI  *  RECE41-SYUTENTANKA)
                   COMPUTE  RECE41-TOTALKIN-JYU   =
                            RECE41-TOTALKIN-JYU   +
                      (WRK-YKZTEN-JI  *  RECE41-YKZTENTANKA)
      ***
                   PERFORM    VARYING    ID1    FROM    1   BY   1
                               UNTIL     ID1     >     38
                     IF   ID1  =   9  OR  18  OR  22  OR  25  OR
                                  28  OR  31  OR  34  OR  38
                        CONTINUE
                     ELSE
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-SYUTEN-JI(ID1) * RECE41-SYUTENTANKA)
                        COMPUTE  RECE41-KINGAKU-JYU(ID1) =
                                 RECE41-KINGAKU-JYU(ID1) +
                        (TBL-YKZTEN-JI(ID1) * RECE41-YKZTENTANKA)
                     END-IF
                   END-PERFORM
      *
               END-EVALUATE
      *
               COMPUTE   RECE41-KINGAKU-JYU(9)  =
                         RECE41-KINGAKU-JYU(1)  +
                         RECE41-KINGAKU-JYU(2)  +
                         RECE41-KINGAKU-JYU(3)  +
                         RECE41-KINGAKU-JYU(4)  +
                         RECE41-KINGAKU-JYU(5)  +
                         RECE41-KINGAKU-JYU(6)  +
                         RECE41-KINGAKU-JYU(7)  +
                         RECE41-KINGAKU-JYU(8)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(18)  =
                         RECE41-KINGAKU-JYU(10)  +
                         RECE41-KINGAKU-JYU(11)  +
                         RECE41-KINGAKU-JYU(12)  +
                         RECE41-KINGAKU-JYU(13)  +
                         RECE41-KINGAKU-JYU(14)  +
                         RECE41-KINGAKU-JYU(15)  +
                         RECE41-KINGAKU-JYU(16)  +
                         RECE41-KINGAKU-JYU(17)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(22)  =
                         RECE41-KINGAKU-JYU(19)  +
                         RECE41-KINGAKU-JYU(20)  +
                         RECE41-KINGAKU-JYU(21)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(25)  =
                         RECE41-KINGAKU-JYU(23)  +
                         RECE41-KINGAKU-JYU(24)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(28)  =
                         RECE41-KINGAKU-JYU(26)  +
                         RECE41-KINGAKU-JYU(27)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(31)  =
                         RECE41-KINGAKU-JYU(29)  +
                         RECE41-KINGAKU-JYU(30)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(34)  =
                         RECE41-KINGAKU-JYU(32)  +
                         RECE41-KINGAKU-JYU(33)   
      *
               COMPUTE   RECE41-KINGAKU-JYU(38)  =
                         RECE41-KINGAKU-JYU(35)  +
                         RECE41-KINGAKU-JYU(36)  +
                         RECE41-KINGAKU-JYU(37)   
      *
             END-EVALUATE
      *
           END-IF                                                       
      *
           REWRITE   RECE41-REC
      *
           ADD     1                   TO  CNT-OUT3
      *
           .
       2105-RECE41-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの基本点数部分のクリア処理（公務災害）
      *****************************************************************
       2106-KHNTEN-KO-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN7-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(7)
           END-IF
           IF        FLG-KHNTEN8-KO  =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(8)
           END-IF
           IF        FLG-KHNTEN17-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(17)
           END-IF
           IF        FLG-KHNTEN19-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(19)
           END-IF
           IF        FLG-KHNTEN21-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(21)
           END-IF
           IF        FLG-KHNTEN22-KO =    1
             MOVE    ZERO             TO   RECE41-KHNTEN(22)
           END-IF
      *
           .
       2106-KHNTEN-KO-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE41-FILE
                   RECE44-FILE
                   RECE45-FILE
                   RECE4A-FILE
                   RECE4B-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41-IN  :" CNT-IN   
           DISPLAY "RECE41-MOD :" CNT-OUT3  
           DISPLAY "RECE44-OUT :" CNT-OUT1 
           DISPLAY "RECE45-OUT :" CNT-OUT2  
           DISPLAY "RECE4A-OUT :" CNT-OUT4 
           DISPLAY "RECE4B-OUT :" CNT-OUT5  
           DISPLAY "*** ORCR0420 END ***"
      * 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       800-SRYACCT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       800-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       800-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計付加情報読込
      *****************************************************************
       800-SRYACCTPLUS-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       800-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       800-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
grpsys     MOVE    SRY-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    SRY-SRYCD(IDX2)      TO      TNS-SRYCD
           MOVE    RECE45-SRYYM         TO      WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
           MOVE    WK-TAIHI-SRYYMD      TO      TNS-YUKOSTYMD
                                                TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE   MCPDATA-REC   TO  TNS-TENSU-REC
                   MOVE   ZERO          TO  FLG-TENSU
               ELSE
                   INITIALIZE               TNS-TENSU-REC
                   MOVE    1            TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                   TNS-TENSU-REC
               MOVE    1                TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       800-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTCOM-REC
                   MOVE    ZERO            TO  FLG-PTCOM
               ELSE
                   MOVE    1               TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                      PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE41-READ-SEC            SECTION.
      *
           READ    RECE41-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   PERFORM 910-SYSKANRI-HENSYU-SEC
      *
           END-READ
           .
       900-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       910-SYSKANRI-HENSYU-SEC         SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
             CONTINUE
           ELSE
      *      医療機関ＩＤ編集
             MOVE  ZERO           TO  FLG-SET
             MOVE  RECE41-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
             MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
             PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                   UNTIL  ( SYSKAN-IDX              >  100  )
                    OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                    OR    ( FLG-SET                 =   1   )
      *
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                   MOVE     1       TO    FLG-SET
                   MOVE    SYS-1001-TAX-HASU-KBN
                                    TO    WRK-TAX1001-KBN
                END-IF
      *
             END-PERFORM
             IF     FLG-SET  =  ZERO
      *
               STRING "医療機関情報が取得できませんでした。診療年月＝"
                                          DELIMITED  BY  SIZE 
                       RECE41-SRYYM       DELIMITED  BY  SIZE
                                      INTO    WRK-RECEERR
               END-STRING
      *
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-END
               GO  TO  910-SYSKANRI-HENSYU-EXT
             END-IF
      *
      *      施設基準情報
             MOVE    ZERO                TO  FLG-SET
      *
             PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                       UNTIL ( SYSKAN-IDX2               >  100  )
                        OR   ( TBL-SYS10062(SYSKAN-IDX2) = SPACE )
                        OR   ( FLG-SET                   =   1   )
                 MOVE    TBL-SYS10062(SYSKAN-IDX2)  TO  SYS-1006-REC
                 IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                     MOVE     1                  TO  FLG-SET
                 END-IF
             END-PERFORM
             IF      FLG-SET    =   ZERO
                 MOVE  ZERO                    TO  WRK-SISETU613
                 MOVE  ZERO                    TO  WRK-SISETU695
             ELSE
                 MOVE  SYS-1006-SSTKIJUN(113)  TO  WRK-SISETU613
                 MOVE  SYS-1006-SSTKIJUN(195)  TO  WRK-SISETU695
             END-IF
      *
             MOVE    ZERO                TO  FLG-SET
      *
             PERFORM  VARYING  SYSKAN-IDX2  FROM  1   BY  1
                       UNTIL ( SYSKAN-IDX2               >  100  )
                        OR   ( TBL-SYS10067(SYSKAN-IDX2) = SPACE )
                        OR   ( FLG-SET                   =   1   )
                 MOVE    TBL-SYS10067(SYSKAN-IDX2)  TO  SYS-1006-REC
                 IF   ( SYS-1006-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                 AND  ( WRK-SYS-SRYYMD     <=  SYS-1006-EDYUKYMD )
                     MOVE     1                  TO  FLG-SET
                 END-IF
             END-PERFORM
             IF      FLG-SET    =   ZERO
                 MOVE  ZERO                    TO  WRK-SISETU3055
                 MOVE  ZERO                    TO  WRK-SISETU3056
                 MOVE  ZERO                    TO  WRK-SISETU3057
                 MOVE  ZERO                    TO  WRK-SISETU3058
                 MOVE  ZERO                    TO  WRK-SISETU3168
                 MOVE  ZERO                    TO  WRK-SISETU3169
             ELSE
                 MOVE  SYS-1006-SSTKIJUN(55)   TO  WRK-SISETU3055
                 MOVE  SYS-1006-SSTKIJUN(56)   TO  WRK-SISETU3056
                 MOVE  SYS-1006-SSTKIJUN(57)   TO  WRK-SISETU3057
                 MOVE  SYS-1006-SSTKIJUN(58)   TO  WRK-SISETU3058
                 MOVE  SYS-1006-SSTKIJUN(168)  TO  WRK-SISETU3168
                 MOVE  SYS-1006-SSTKIJUN(169)  TO  WRK-SISETU3169
             END-IF
      *
           END-IF
      *
           .
      *
       910-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
      *
