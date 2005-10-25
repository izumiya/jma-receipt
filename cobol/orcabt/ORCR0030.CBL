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
       PROGRAM-ID.             ORCR0030.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 診療区分別点数集計
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    MCC-藤原     01.07.05  ＯＣＲの編集を修正
      *                                     記号・番号、公費実日数・点数
      *                                     ３行目が全てゼロのとき   
      *  01.00.03    MCC-藤原     01.07.10  ＯＣＲ・二行目　記号番号以降
      *                                     がないときは出力しない     
      *  01.00.04    MCC-藤原     01.07.11  記号・番号の途中に空白が
      *                                     あるときの編集修正     
      *  01.00.05   MCC-藤原      01.07.11  フリーコメントのとき
      *                                     患者コメントから名称編集
      *  01.00.06   MCC-藤原      01.10.03  診療行為コ一ド１桁目が８のと
      *                                     き患者コメントがあれば患者コ
      *                                     メントから名称編集
      *  01.00.07   MCC-小豆沢    01.10.25  検査のフィルム等の器材点数
      *                                     をレセプト点数欄の検査薬剤
      *                                     にセットする
      *  01.00.08   MCC-小豆沢    01.10.29  特定疾患処方管理加算(処方せん料)
      *                                     をレセプト点数欄の処方せんに加算
      *  01.00.09   MCC-小豆沢    01.10.31  処置から画像診断までの点数欄の
      *                                     回数カウントを修正(薬剤のみの剤で
      *                                     あればカウントしない)
      *  01.00.10   MCC-小豆沢    01.11.27  特定疾患処方管理加算の算定回数を
      *                                     処方料の欄から減算する
      *  01.00.11   MCC-小豆沢    02.01.09  剤の点数、回数に係わらずフリー
      *                                     コメントをレセプトの適用欄に印字
      *                                     するよう判定文の修正
      *                                     (診療区分＝９９)
      *  01.00.12   MCC-小豆沢    02.01.09  投薬の剤に「粉砕」のコメントが
      *                                     セットされた場合の点数欄対応
      *  01.00.13   MCC-小豆沢    02.02.01  処方(乳幼児)加算の算定回数を
      *                                     処方料の欄から減算する
      *  01.00.14   MCC-小豆沢    02.02.04  特定疾患処方管理加算(処方せん料)
      *                                     の算定回数を処方せんの欄から
      *                                     減算する
      *  01.00.15   MCC-小豆沢    02.02.13  手術麻酔(診療区分＝５４)の
      *                                     レセプト編集漏れの修正
      *                                     (手術点数欄)
      *  01.00.16   MCC-藤原      02.02.13  保険組合せを３から１０に変更
      *  01.00.17   MCC-小豆沢    02.02.19  検査の回数から採血料の回数を
      *                                     減算
      *  01.00.18   MCC-藤原      02.02.21  診療行為コードの読み飛ばしの追加
      *  01.00.19   MCC-藤原      02.03.28  国保のときシス管の区分によりＯＣＲ
      *                                     の印刷をする
      *  01.00.20   MCC-小豆沢    02.04.09  再診の点数欄集計時に注加算通番が
      *                                     セットされている場合、回数の加算
      *                                     をしない
      *  01.00.21   MCC-小豆沢    02.04.09  初診の時間外／深夜／休日をレセプト
      *                                     表示するための区分を追加
      *  01.00.22   MCC-小豆沢    02.04.09  処方せん(乳幼児)加算の点数集計欄の
      *                                     修正と回数をカウントしないよう修正
      *
      *  ２００２年４月改正対応
      *  01.02.01   MCC-小豆沢    02.04.19  処方せんのコード修正
      *                                     外来管理加算の点数表示修正
      *  01.02.02   MCC-小豆沢    02.04.20  その他の点数集計欄を修正
      *  01.02.03   MCC-小豆沢    02.04.22  レセプト摘要欄の印刷区分編集を追加
      *                                     再診料、外来管理加算について
      *  01.02.04   MCC-小豆沢    02.04.23  レセプト特記事項の編集を追加
      *  01.02.05   MCC-小豆沢    02.04.28  在宅の診療時間加算を入力時の点数
      *                                     欄の集計を修正
      *  01.02.06   MCC-小豆沢    02.05.30  採血料と判断料の回数減算を修正
      *  01.02.07   NACL-藤原     02.09.06  検査の編集のために回数を
      *                                     全て編集する
      *  01.02.08   NACL-門脇     02.10.07  在宅総合・末期フラグのセットを行う
      *                                     一部負担金再掲のセットを行う
      *  01.02.09   MCC-小豆沢    02.10.16  特定器材点数の集計先を修正
      *  01.02.10   MCC-小豆沢    02.11.06  約束処方時の点数欄表示修正
      *  01.02.11   NACL-門脇     02.11.26  検診コードを編集できるよう修正
      *  01.02.12   NACL-藤原     02.12.02  酸素の時単価を表示するように追加
      *  01.02.13   NACL-門脇     02.12.05  生保、長期に関わる１０月改正に
      *                                     おける一部負担金額欄の対応
      *  01.02.14    NACL-藤原    03.01.17  データ区分の追加
      *                                     診療行為による特記事項の編集の修正
      *  01.02.15   NACL-門脇     03.02.06  コメントカウントフラグの編集追加
      *  01.02.14   NACL-藤原     03.02.12  補助区分の追加
      *  01.02.15   NACL-門脇     03.03.07  在総診フラグセット取りやめ
      *  01.02.16   NACL-藤原     03.04.09  特定器材（画像診断のフィルム以外）
      *                                     の単価を表示するように修正
      *  01.02.17   NACL-門脇     03.04.16  船員保険（被保険者）＋通勤災害
      *                                     ＋初診時　対応
      *  01.02.18   NACL-門脇     03.04.17  結核、精神の一部負担額編集修正
      *  01.02.19   NACL-藤原     03/04/21  地方公費データのとき不明分は
      *                                     処理の対象としない
      *  01.02.20   NACL-門脇     03/04/24  一部負担金（）の編集修正
      *  01.02.21   NACL-門脇     03/05/08  名称有効桁のセット
      *  01.02.22   NACL-門脇     03/05/13  処方せん回数のセット修正
      *  01.02.23   NACL-門脇     03/05/22  残量廃棄の数量セット
      *  01.02.24   NACL-門脇     03/05/23  一部負担金記載修正
      *
      *  ２００３年６月改正対応
      *  01.02.25   NACL-門脇     03/05/30  平成１５年６月改正に関わる
      *                                     再診料関係の摘要欄編集修正
      *  01.02.26   NACL-門脇     03/06/03  外来診療料対応
      *                                     （平成１５年６月以降）
      *  01.02.27   NACL-藤原     03/06/30  三種公費のときのＯＣＲの修正
      *  01.02.28   NACL-門脇     03/07/15  乳幼児・幼児のコメントフラグ
      *                                     セット
      *  01.02.29   NACL-門脇     03/08/20  公費実日数テーブル情報のセット
      *  01.02.30   NACL-門脇     03/09/18  一部負担金額編集修正
      *  01.02.31   NACL-門脇     03/09/24  コメント編集フラグのセット
      *  01.02.32   NACL-門脇     03/09/29  包括対象検査情報のセット
      *                                     検査等実施判断情報のセット
      *                                     各種番号等情報のセット
      *  01.02.33   NACL-門脇     03/10/06  実日数等の編集追加
      *  01.02.34   NACL-門脇     03/10/15  同日再診を実日数カウントの対象
      *                                     に追加
      *  01.02.35   NACL-門脇     03/11/26  自費レセプトの自費分
      *                                     摘要欄記載対応
      *  01.02.36   NACL-門脇     04/01/06  診療行為コードの１桁目の判定
      *                                     修正
      *  01.02.37   NACL-門脇     04/01/14  診療行為の包括分は除く
      *                                     SRYACCT2→32
      *  01.02.38   NACL-門脇     04/01/19  病床数編集フラグの編集
      *  01.02.39   NACL-門脇     04/01/20  自費レセ摘要欄編集レコード
      *                                     フラグの編集
      *  01.02.40   NACL-門脇     04/02/05  診療コード＊数量の累計編集
      *  01.02.41   NACL-門脇     04/02/10  生保の一部負担金額記載修正
      *  01.02.42   NACL-藤原     04/02/18  社保のＯＣＲ印刷をシス管より決定
      *  01.02.43   NACL-門脇     04/03/17  愛媛県地方公費：福祉レセ対応
      *  ２００４年４月改正対応
      *  01.03.01   NACL-門脇     04/04/01  上限あり公費併用時の一部負担額
      *                                     編集修正
      *  01.03.02   NACL-門脇     04/04/01  長期投薬加算（処方料、処方せん
      *                                     料）追加対応
      *  01.03.03   NACL-門脇     04/04/01  リハ言語聴覚療法（３）追加対応
      *  01.03.04   NACL-門脇     04/04/05  在宅血液透析頻回指導管理料の
      *                                     コメント対応
      *  01.03.05   NACL-門脇     04/04/10  在宅患者訪問点滴注射管理指導料
      *                                     算定時の点滴注射のコメント対応
      *  01.03.06   NACL-門脇     04/04/26  ユーザーコメント追加対応
      *  01.03.07   NACL-太田     04/05/06  点数マスタ検索ロジック修正
      *  01.03.08   NACL-門脇     04/05/13  初診時に時間外・深夜・休日が
      *                                     ２つ以上あった場合の記載対応
      *
      *  01.04.01   NACL-藤原     04/05/26  RECE05レイアウト変更
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.04.02   NACL-門脇     04/06/08  福岡県４１老人の一部負担額記載
      *                                     対応
      *  01.04.03   NACL-小豆沢   04/06/08  ＣＴ・ＭＲＩ（２回目以降）の
      *                                     摘要欄記載順修正
      *  01.04.04   NACL-小豆沢   04/06/11  皮下筋肉注射で、薬剤点・器材点
      *                                     が無いの場合の摘要欄記載対応
      *  01.04.05   NACL-門脇     04/06/21  生活保護の負担金額記載修正
      *  01.04.06   NACL-門脇     04/07/28  投薬料の器材点の集計修正
      *  01.04.07   NACL-門脇     04/08/04  同一保険番号対応
      *  01.04.08   NACL-門脇     04/08/11  複数保険・複数科入力対応
      *  01.04.09   NACL-門脇     04/09/17  ＯＣＲ公費２の編集修正
      *  01.04.10   NACL-藤原     04/09/17  ＯＣＲ区分の設定に追加
      *  中止処理追加
      *  01.05.01   NACL-藤原     04/09/28  中止処理の追加
      *  01.05.02   NACL-門脇     04/10/14  外来管理加算の剤番号置き換え
      *  01.05.03   NACL-門脇     04/11/15  公費月上限額取得修正
      *  01.05.04   NACL-門脇     04/11/22  フィルム点数計算
      *                                     （診区４０、５０、５４、６０）
      *  01.05.05    NACL-小豆沢  04/12/08  点数フラグの初期化追加
      *  01.05.06    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.05.07    NACL-門脇    04/12/15  公費一部負担金レセ記載フラグ
      *                                     の編集
      *                                     公費一部負担金０円記載フラグ
      *                                     の編集
      *  01.05.08    NACL-門脇    05/01/18  酸素計算式コメント対応
      *  01.05.09    NACL-門脇    05/02/14  高齢者一部負担額記載修正
      *  01.05.10    NACL-門脇    05/02/28  （サ）記載対応
      *  01.05.11    NACL-門脇    05/03/03  特記事項の編集修正
      *  01.05.12    NACL-門脇    05/03/16  負担金額（一部負担額）・
      *                                     負担金額（控除額）・
      *                                     一部負担金・食事療養費標準
      *                                     負担額区分の編集
      *  01.06.01    NACL-門脇    05/03/24  小児特定上限対応
      *  01.06.02    NACL-小豆沢  05/03/24  老人理学療法摘要欄対応
      *  01.06.03    NACL-門脇    05/04/08  負担金額（一部負担額）編集
      *                                     修正
      *  01.06.04    NACL-門脇    05/04/20  内服薬剤逓減編集対応
      *  01.06.05    NACL-門脇    05/05/19  点数マスタの単位文字数がゼロ
      *                                     の場合の編集
      *  01.06.06    NACL-門脇    05/05/31  レセ記載フラグ、０円記載フラグ
      *                                     の編集修正
      *  01.06.07    NACL-門脇    05/06/10  負担金額（一部負担額）編集
      *                                     修正
      *  01.06.08    NACL-門脇    05/10/05  不要条件削除
      *  01.06.09    NACL-門脇    05/10/25  島根県地方公費１０月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE04-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
           SELECT  RECE044-FILE    ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE044-KEY
                                   FILE    STATUS  IS  STS-RECE04.
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
           SELECT  RECE055-FILE    ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE055-KEY
                                   FILE    STATUS  IS  STS-RECE05.
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
       FD  RECE04-FILE.
       01  RECE04-REC. 
           COPY    "CPRCF004.INC".
      *
       FD  RECE044-FILE.
       01  RECE044-REC. 
           COPY    "CPRCF004.INC"  REPLACING  //RECE04//
                                   BY         //RECE044//.
      *
       FD  RECE05-FILE.
       01  RECE05-REC. 
           COPY    "CPRCF005.INC".
      *
       FD  RECE055-FILE.
       01  RECE055-REC. 
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //RECE055//.
      *
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC".
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE04//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE05//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE09//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02                              PIC X(02).
           03  STS-RECE04                              PIC X(02).
           03  STS-RECE05                              PIC X(02).
           03  STS-RECE09                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-SYUNOU                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-ZAITAKU                             PIC 9(01).
           03  FLG-OSINKBN                             PIC 9(01).
           03  FLG-RECE02                              PIC 9(01).
           03  FLG-KANJI                               PIC 9(01).
           03  FLG-SP                                  PIC 9(01). 
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-ZANHAIKI                            PIC 9(01).
           03  FLG-RECE05                              PIC 9(01).
           03  FLG-REWRITE                             PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).       
           03  FLG-SET                                 PIC 9(01).
           03  FLG-HOUSIJI-COM                         PIC 9(01).
           03  FLG-HOUTEN-COM                          PIC 9(01).
           03  FLG-FUKUSHI                             PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-SYUTOTAL                            PIC 9(01).
           03  FLG-SYUTOTAL-SET                        PIC 9(01).
           03  FLG-RECESKYKBN                          PIC 9(01).
           03  FLG-ZENKOH-ARI                          PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-RECE09                              PIC 9(01).
           03  FLG-RINYAK                              PIC 9(01).
           03  FLG-TOKKI                               PIC 9(01).
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
      *    再診料基本点数加算フラグ
       01  FLG-SAISIN-AREA.
           03  FLG-NYUYOJI                             PIC 9(01).
           03  FLG-YOJI                                PIC 9(01).
           03  FLG-YOJI-ARI                            PIC 9(01).
           03  FLG-SAISIN-ARI                          PIC 9(01).
      *    乳幼児加算カウント
       01  CNT-SAISIN-AREA.
           03  CNT-NYUYOJI                             PIC 9(02).
           03  CNT-YOJI                                PIC 9(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-IN-COMMIT                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-IN                              PIC 9(06).
               05  CNT-OUT3                            PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDX4                                    PIC 9(03).
           03  IDX8                                    PIC 9(02).
           03  IDX9                                    PIC 9(03).
           03  IDXX                                    PIC 9(02).
           03  IDYX                                    PIC 9(02).
           03  IDZX                                    PIC 9(02).
           03  IDY                                     PIC 9(03).
           03  IDY2                                    PIC 9(03).
           03  IDZ                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDW2                                    PIC 9(03).
           03  IDV                                     PIC 9(03).
           03  NUM-IDX                                 PIC 9(01).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  BUN-IDX                                 PIC 9(03).
           03  BUN-IDX2                                PIC 9(03).
           03  BUN-IDX3                                PIC 9(03).
           03  BUN-IDX4                                PIC 9(03).
           03  BUN-IDX5                                PIC 9(03).       
           03  ACCT-IDX                                PIC 9(02).       
           03  HKN-IDX                                 PIC 9(02).       
           03  KOHFTN-IDX                              PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *     
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDX5                PIC 9(03).
           03  IDX6                PIC 9(03).
           03  IDX7                PIC 9(03).
           03  SEIHIDX             PIC 9(02).
           03  KAKKIDX             PIC 9(02).
           03  SESNIDX             PIC 9(02).
      *
      *    ＯＣＲ編集領域
       01  OCR1-AREA.
           03  OCR1-HKNJANUM                           PIC 9(08).
           03  OCR1-HOSPCD                             PIC 9(07).
           03  OCR1-SKYTENSU                           PIC 9(07).
           03  OCR1-CHKDGT1                            PIC 9(01).
           03  OCR1-YKZFTN                             PIC 9(05).
           03  OCR1-CHKDGT2                            PIC 9(01).
           03  OCR1-FTNMONEY                           PIC 9(05).
           03  OCR1-CHKDGT3                            PIC 9(01).
           03  OCR1-CHKDGT4                            PIC 9(01).
           03  OCR1-JNISSU                             PIC 9(02).
           03  OCR1-SRYYM                              PIC 9(04).
           03  OCR1-CHKDGT5                            PIC 9(01).
           03  OCR1-FTNJANUM                           PIC 9(08).
           03  OCR1-JKYSNUM                            PIC 9(07).
           03  OCR1-CHKDGT6                            PIC 9(01).
           03  OCR1-PREFNUM                            PIC 9(02).
           03  OCR1-TENSUHYO                           PIC 9(01).
           03  OCR1-HKNSYUBETU1                        PIC 9(01).
           03  OCR1-HKNSYUBETU2                        PIC 9(01).
           03  OCR1-HONKZKKBN                          PIC 9(01).
           03  OCR1-SEIRINO                            PIC 9(01).
           03  OCR1-CHKDGT7                            PIC 9(01).
      *
       01  OCR2-AREA.
           03  OCR2-SEX                                PIC 9(01).
           03  OCR2-GENGO                              PIC 9(01).
           03  OCR2-BIRTHYY                            PIC 9(02).
           03  OCR2-CHKDGT1                            PIC 9(01).
           03  OCR2-KIGO                               PIC 9(10).
           03  OCR2-NUM                                PIC 9(10).
           03  OCR2-CHKDGT2                            PIC 9(01).
           03  OCR2-AREA-X.
               05  OCR2-FTNJANUM                       PIC 9(08).
               05  OCR2-JKYSNUM                        PIC 9(07).
               05  OCR2-CHKDGT3                        PIC 9(01).
               05  OCR2-JNISSU                         PIC 9(02).
               05  OCR2-TENSU                          PIC 9(07).
               05  OCR2-CHKDGT4                        PIC 9(01).
               05  OCR2-YKZFTN                         PIC 9(05).
               05  OCR2-CHKDGT5                        PIC 9(01).
               05  OCR2-FTNMONEY                       PIC 9(05).
               05  OCR2-CHKDGT6                        PIC 9(01).
               05  OCR2-CHKDGT7                        PIC 9(01).
           03  OCR2-YOBI                               PIC X(02).
      *
       01  OCR3-AREA.
           03  OCR3-AREA-X.
               05  OCR3-FTNJANUM                       PIC 9(08).
               05  OCR3-JKYSNUM                        PIC 9(07).
               05  OCR3-CHKDGT1                        PIC 9(01).
               05  OCR3-JNISSU                         PIC 9(02).
               05  OCR3-TENSU                          PIC 9(07).
               05  OCR3-CHKDGT2                        PIC 9(01).
               05  OCR3-YKZFTN                         PIC 9(05).
               05  OCR3-CHKDGT3                        PIC 9(01).
               05  OCR3-FTNMONEY                       PIC 9(05).
               05  OCR3-CHKDGT4                        PIC 9(01).
               05  OCR3-CHKDGT5                        PIC 9(01).
           03  OCR3-YOBI                               PIC X(28).
      *
      *    パス名領域
       01  WK-PATH                                     PIC X(20).
      *
      *    剤情報退避領域
       01  WK-ZAIKAISU                                 PIC S9(07).
       01  WK-ZAITEN                                   PIC S9(08).
       01  WK-SYUTEN                                   PIC S9(08).
       01  WK-KIZAITEN                                 PIC S9(08).
       01  WK-YKZTEN                                   PIC S9(08).
      *
      *    点数テーブル検索用対象日領域
       01  WK-TAIHI-AREA.
           03  WK-TAIHI-DAY                            PIC 9(02).
           03  WK-TAIHI-FLG                            PIC 9(01).
           03  WK-TAIHI-SRYYMD.
               05  WK-TAIHI-SRYYM                      PIC 9(06).
               05  WK-TAIHI-SRYD                       PIC 9(02).
      *
       01  WRK-AREA-SAIKEI.
           03  WRK-KOH-FTN                             PIC 9(09).
           03  WRK-KOH-GAI                             PIC 9(09).
           03  FTN-KOH-XXX                             PIC 9(09).       
           03  FTN-KOH-YYY                             PIC 9(09).       
           03  FLG-KOH                                 PIC 9(01).
           03  WRK-FTNKEI                              PIC 9(09).
           03  WRK-FTNKEI-FTN                          PIC 9(09).
           03  WRK-GENDO                               PIC 9(09).
           03  WRK-KINGAKU                             PIC 9(09).
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
      *
      *    実日数カウント用領域                                         
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN                          OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  10.
               05  TBL-SYS1001                         PIC X(610).
      *    診療行為機能情報退避用領域
       01  TBL-SYS1038-T.
           03  TBL-SYS1038-OCC                         OCCURS  10.
               05  TBL-SYS1038                         PIC X(610).
      *
       01  TBL-DENPNUM.
           03  TBL-DENPNUM-OCC                         OCCURS  100.
               05  TBL-FUKU-DENPNUM                    PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
      *********05  WRK-PARA-PREFNUM                    PIC X(02).
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
      *
           03  WRK-SYS-SRYYMD                          PIC X(08).
      *
           03  WRK-CHK-HKNJANUM                        PIC X(08).
           03  WRK-CHK-IDOU                            PIC 9(01).
           03  WRK-CHK-TAISYO                          PIC 9(01).
           03  WRK-CHK-HKNID                           PIC 9(10).
           03  WRK-CHK-HKNNUM                          PIC X(03).
           03  WRK-CHK-ICHIBUGAKU                      PIC 9(07).
      *
           03  WRK-ZENKAKU                             PIC X(80).
           03  WRK-NUM                                 PIC X(80).
           03  WRK-NUM-X                               PIC X(10). 
           03  WRK-STIDX                               PIC 9(02).
           03  WRK-EDIDX                               PIC 9(02).
      *
           03  WRK-RECEERR                             PIC X(200).
      *
           03  WRK-GAIJGNGAK                           PIC 9(07).
      *
           03  WRK-TOKKI                               PIC X(02).
           03  TOKKI-IDX                               PIC 9(02). 
      *
           03  WRK-TANICD                              PIC 9(03).
           03  WRK-TANIMOJISU                          PIC 9(01).
           03  WRK-TANINAME                            PIC X(24).
           03  WRK-SRYSURYO                            PIC 9(05)V9(03).
           03  WRK-SRYSURYO2                           PIC 9(05).
           03  WRK-SRYSURYO3                           PIC 9(05).
           03  WRK-RENNUM                              PIC 9(02).
      *
           03  WRK-SRYCD                               PIC 9(09).
      *
           03  WRK-JYOGEN41                            PIC 9(09).
      *
           03  WRK-FILMTEN                             PIC 9(07)V9(02).
           03  WRK-FILMTEN2                            PIC 9(07)V9(02).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU        PIC X(03).
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
           03  WRK-TANKA                       PIC 9(09)V9(02).
           03  WRK-TANKA-R  REDEFINES  WRK-TANKA.
               05  WRK-TANKA-S1                PIC 9(09).
               05  WRK-TANKA-S2                PIC 9(02).
           03  WRK-TANKA-X.
               05  WRK-TANKA-Z1                PIC ZZZZZZZZ9.
               05  WRK-TANKA-Z2                PIC X(01).
               05  WRK-TANKA-Z3                PIC ZZ.
      *
      *
           03  WRK-SRYCDTOTAL9                 PIC 9(14).
           03  WRK-SRYCDTOTALX   REDEFINES     WRK-SRYCDTOTAL9
                                               PIC X(14).
           03  WRK-ZAINUM                      PIC 9(08).
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
      *---------------------------------------------------------------
      *
      *    固定値
       01  CONST-AREA.
      *    フリーコメント
           03  CONS-COMMENT    PIC X(09)   VALUE   "810000001".
      *    継続管理加算
           03  CONS-KEIZOKU1   PIC X(09)   VALUE   "112007170".
           03  CONS-KEIZOKU2   PIC X(09)   VALUE   "112702270".
      *    処方せん料
           03  CONS-SHOHOSEN1 PIC X(09)   VALUE    "120002710".
           03  CONS-SHOHOSEN2 PIC X(09)   VALUE    "120002810".
           03  CONS-SHOHOSEN3 PIC X(09)   VALUE    "120002910".
           03  CONS-SHOHOSEN4 PIC X(09)   VALUE    "120003010".
      *    処方せん（乳幼児）加算
           03  CONS-SHOHOKSN1  PIC X(09)   VALUE   "120002470".
           03  CONS-SHOHOKSN2  PIC X(09)   VALUE   "120002570".
      *    長期投薬加算（処方せん料）
           03  CONS-SHOHOKSN3  PIC X(09)   VALUE   "120003270".
      *    薬剤料逓減（９０／１００）（内服薬）
           03  CONS-YAKTEIGEN  PIC X(09)   VALUE   "630010002".
      *    粉砕（コメント）
           03  CONS-FUNSAI-COMMENT PIC X(09) VALUE "099209901".
      *    採血（Ｂ−Ａ）
           03  CONS-SAIKETSU1      PIC X(09) VALUE "160101210".
      *    採血（Ｂ−Ｖ）
           03  CONS-SAIKETSU2      PIC X(09) VALUE "160095710".
      *    採血（Ｂ−Ｃ）
           03  CONS-SAIKETSU3      PIC X(09) VALUE "160095810".
      *    再診（２回目以降のコード）
           03  CONS-SAISHIN1       PIC X(09) VALUE "112009310".
           03  CONS-SAISHIN2       PIC X(09) VALUE "112010250".
           03  CONS-SAISHIN3       PIC X(09) VALUE "112009850".
           03  CONS-SAISHIN4       PIC X(09) VALUE "112010750".
           03  CONS-SAISHIN5       PIC X(09) VALUE "112703810".
           03  CONS-SAISHIN6       PIC X(09) VALUE "112704550".
           03  CONS-SAISHIN7       PIC X(09) VALUE "112704250".
           03  CONS-SAISHIN8       PIC X(09) VALUE "112704950".
           03  CONS-SAISHIN9       PIC X(09) VALUE "112007510".
           03  CONS-SAISHIN10      PIC X(09) VALUE "112008050".
           03  CONS-SAISHIN11      PIC X(09) VALUE "112008450".
           03  CONS-SAISHIN12      PIC X(09) VALUE "112008950".
           03  CONS-SAISHIN13      PIC X(09) VALUE "112702410".
           03  CONS-SAISHIN14      PIC X(09) VALUE "112702850".
           03  CONS-SAISHIN15      PIC X(09) VALUE "112703150".
           03  CONS-SAISHIN16      PIC X(09) VALUE "112703550".
      *    外来管理加算（４回目以降のコード）
           03  CONS-GAIRAIKSN1     PIC X(09) VALUE "112011110".
           03  CONS-GAIRAIKSN2     PIC X(09) VALUE "112011210".
           03  CONS-GAIRAIKSN3     PIC X(09) VALUE "112012910".
           03  CONS-GAIRAIKSN4     PIC X(09) VALUE "112705410".
           03  CONS-GAIRAIKSN5     PIC X(09) VALUE "112707010".
           03  CONS-GAIRAIKSN6     PIC X(09) VALUE "112705210".
           03  CONS-GAIRAIKSN7     PIC X(09) VALUE "112706910".
      *    在宅総合・在宅末期
           03  CONS-ZAITAKU1       PIC X(09) VALUE "114701210".
           03  CONS-ZAITAKU2       PIC X(09) VALUE "114701710".
           03  CONS-ZAITAKU3       PIC X(09) VALUE "114007610".
           03  CONS-ZAITAKU4       PIC X(09) VALUE "114007710".
      *    特記事項
           03  CONS-TOKKI1         PIC X(14) VALUE "00000099990101".
           03  CONS-TOKKI2         PIC X(14) VALUE "00000099990102".
           03  CONS-TOKKI3         PIC X(14) VALUE "00000099990103".
           03  CONS-TOKKI4         PIC X(14) VALUE "00000099990104".
           03  CONS-TOKKI5         PIC X(14) VALUE "00000099990105".
           03  CONS-TOKKI7         PIC X(14) VALUE "00000099990107".
           03  CONS-TOKKI8         PIC X(14) VALUE "00000099990108".
           03  CONS-TOKKI9         PIC X(14) VALUE "00000099990109".
           03  CONS-TOKKI10        PIC X(14) VALUE "00000099990110".
           03  CONS-TOKKI11        PIC X(14) VALUE "00000099990111".
           03  CONS-TOKKI12        PIC X(14) VALUE "00000099990112".
      *
      *    病床数編集フラグ編集の為のコード（特定疾患療養指導料等）
           03  CONS-SIDORYO1       PIC X(09) VALUE "113001810".
           03  CONS-SIDORYO2       PIC X(09) VALUE "113001910".
           03  CONS-SIDORYO3       PIC X(09) VALUE "113002010".
           03  CONS-SIDORYO4       PIC X(09) VALUE "113700610".
           03  CONS-SIDORYO5       PIC X(09) VALUE "113700710".
           03  CONS-SIDORYO6       PIC X(09) VALUE "113700810".
      *
      *    再診（乳幼児）加算
           03  CONS-NYUYOJI        PIC X(09) VALUE "112000970".
      *    再診（乳幼児）（時間外等）加算
           03  CONS-NYUYOJI-JIKAN  PIC X(09) VALUE "112006970".
      *    再診（幼児）加算
           03  CONS-YOJI           PIC X(09) VALUE "112001070".
      *    再診（幼児）（時間外等）加算
           03  CONS-YOJI-JIKAN     PIC X(09) VALUE "112007070".
      *
      *    外来診療料（乳幼児）加算
           03  CONS-GNYUYOJI       PIC X(09) VALUE "112006270".
      *    外来診療料（乳幼児）（時間外等）加算
           03  CONS-GNYUYOJI-JIKAN PIC X(09) VALUE "112007270".
      *    外来診療料（幼児）加算
           03  CONS-GYOJI          PIC X(09) VALUE "112006370".
      *    外来診療料（幼児）（時間外等）加算
           03  CONS-GYOJI-JIKAN    PIC X(09) VALUE "112007370".
      *
      *    フィルム料（乳幼児）加算
           03  CONS-FILM-YOUJI-KASAN  
                                   PIC X(09) VALUE "799990070".
      *    酸素補正率
           03  CONS-SANSO          PIC X(09) VALUE "770020070".
      *    高気圧酸素加算
           03  CONS-SANSO2         PIC X(09) VALUE "770030070".
      *    残量廃棄
           03  CONS-ZANHAIKI       PIC X(09) VALUE "099309901".
      *
      *
      *    慢性とう痛疾患
           03  CONS-MANSEI           PIC X(09)  VALUE  "113006510".
      *    傷病手当金意見書交付料
           03  CONS-SYOBYO-COMMENT   PIC X(09)  VALUE  "180000710".
      *    療養費同意書交付料
           03  CONS-RYOYO-COMMENT    PIC X(09)  VALUE  "113004310".
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
      *    在宅血液透析頻回指導管理料
           03  CONS-HINKAI-COMMENT   PIC X(09)  VALUE  "114009410".
      *
      *    皮下筋肉注射
           03  CONS-HIKAKIN          PIC X(09)  VALUE  "130000510".
      *    生物学的製剤注射
           03  CONS-SEIBUTU          PIC X(09)  VALUE  "130000110".
      *    麻薬注射
           03  CONS-MAYAKU           PIC X(09)  VALUE  "130000310".
      *
      *    外来管理加算
           03  CONS-GAIKAN1          PIC X(09)  VALUE  "112011010".
           03  CONS-GAIKAN2          PIC X(09)  VALUE  "112705110".
           03  CONS-GAIKAN3          PIC X(09)  VALUE  "112705310".
      *    （サ）
           03  CONS-RINYAK           PIC X(09)  VALUE  "820000046".
      *
      *    老人理学療法の摘要欄記載判定用（名称に老人を追加）
           03  CONS-RJNRIGAKU1     PIC X(09) VALUE "180706810".
           03  CONS-RJNRIGAKU2     PIC X(09) VALUE "180706910".
           03  CONS-RJNRIGAKU3     PIC X(09) VALUE "180707010".
           03  CONS-RJNRIGAKU4     PIC X(09) VALUE "180707110".
           03  CONS-RJNRIGAKU5     PIC X(09) VALUE "180707210".
           03  CONS-RJNRIGAKU6     PIC X(09) VALUE "180707310".
           03  CONS-RJNRIGAKU7     PIC X(09) VALUE "180707410".
           03  CONS-RJNRIGAKU8     PIC X(09) VALUE "180707510".
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
       01  TBL-SUJI-AREA-X.
           03  FILLER          PIC X(10)   VALUE
              "1234567890".
       01  TBL-SUJI-X-R        REDEFINES   TBL-SUJI-AREA-X.
           03  TBL-SUJI-X      PIC X(01)   OCCURS   10.
      *    
      *
       01  WRK-CONS-AREA.    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
       01  WRK-RECE02-REC. 
           COPY    "CPRCF002.INC"  REPLACING  //RECE02//
                                   BY         //WRK-RECE02//.
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
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納合計マスタ
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    
           COPY    "CPSK1001.INC".
      *    
           COPY    "CPSK1038.INC".
      *    
           COPY    "CPSK2005.INC".     
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
      *    公費月上限額取得サブ
           COPY    "CPORCSJGNGET.INC".
      *    高齢者の一部負担金額記載に関するチェックサブ
           COPY    "CPORCSKOREIFTNCHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCMCP.INC".  
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       WRK-AREA
                                       TTNS-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCR0030"          TO  JOB-PGID
           MOVE    "診療区分別点数集計"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1001"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  ORC-DBYMD (1:6)
              MOVE    "01"             TO  ORC-DBYMD (7:2)
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF    
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM  970-KANRIMST-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM 970-KANRIMST-READ-SEC
      *
                  END-PERFORM
              ELSE
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
              END-IF
      *
              MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       診療行為機能情報
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1038"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  ORC-DBYMD (1:6)
              MOVE    "01"             TO  ORC-DBYMD (7:2)
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1038-REC
              ELSE
                  INITIALIZE               SYS-1038-REC
                  MOVE    "0"          TO  SYS-1038-KAGRPFLG
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1038-T
              INITIALIZE                      TBL-SYS1038-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1038"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM  970-KANRIMST-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1038-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1038-REC  TO  TBL-SYS1038(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM 970-KANRIMST-READ-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  ORC-DBYMD (1:6)
           MOVE    "01"             TO  ORC-DBYMD (7:2) 
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               INITIALIZE                   SYS-2005-REC
               MOVE    ZERO             TO  SYS-2005-KKHOCRKBN 
           END-IF
           IF      SYS-2005-JIHITEKKSIKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-JIHITEKKSIKBN
           END-IF
           IF      SYS-2005-SYAOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYAOCRKBN
           END-IF
           IF      SYS-2005-KKHOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-KKHOCRKBN
           END-IF
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
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       2
                   OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
      *    レセプト処理
                   MOVE    "RECE020"           TO  RECE01PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
                   MOVE    "RECE040"           TO  RECE04PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
                   MOVE    "RECE050"           TO  RECE05PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE05PARA-TERMID
                   MOVE    "RECE090"           TO  RECE09PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE09PARA-TERMID
               ELSE
      *    請求書処理
                   MOVE    "RECE02X"           TO  RECE01PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE01PARA-TERMID
                   MOVE    "RECE090"           TO  RECE09PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE09PARA-TERMID
               END-IF
      *
               PERFORM 200-HENSYU-SEC
           END-PERFORM 
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE02
      *
           IF      IDZ             =   1               
      *    レセプト処理
               OPEN    OUTPUT              RECE044-FILE
               CLOSE                       RECE044-FILE
      *
               OPEN    OUTPUT              RECE055-FILE
               CLOSE                       RECE055-FILE
      *
               OPEN    OUTPUT              RECE04-FILE
      ****                                 RECE05-FILE
               OPEN    I-O                 RECE05-FILE
           END-IF
           OPEN    I-O                 RECE02-FILE
           OPEN    INPUT               RECE09-FILE
      *
           PERFORM    RECE02-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE02  =   1
                                       OR      FLG-END     =   1
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE02-READ-SEC
      *
               IF  CNT-IN-COMMIT       =   300
                   MOVE  ZERO              TO  CNT-IN-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE02-FILE
           CLOSE   RECE09-FILE
      *
           IF      IDZ             =   1               
      *    レセプト処理
               CLOSE   RECE04-FILE
                       RECE05-FILE
           END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
      *    地方公費データのとき不明分は処理の対象としない
           IF    ( IDZ                  =   2    )
           AND   ( RECE02-RECESYUBETU   =   ZERO )
               GO  TO  210-HENSYU-EXT
           END-IF
      *
           INITIALIZE                   WRK-AREA-SAIKEI
           INITIALIZE                   TBL-JNISSU                      
           INITIALIZE                   TBL-DENPNUM
      *
           INITIALIZE                   FLG-KHNTEN-AREA
           INITIALIZE                   FLG-SAISIN-AREA
           INITIALIZE                   CNT-SAISIN-AREA
      *
           MOVE      ZERO       TO      FLG-FUKUSHI
           MOVE    88888880     TO      WRK-ZAINUM
      *
           IF    RECE02-ZAITAKU      NOT =    SPACE
               PERFORM     2101-KOREISYA-KOHI-SEC
               IF   RECE02-RECEKA  =  SPACE
                  PERFORM     2102-KOREISYA-CHECK-SEC
      *********ELSE
      *********   PERFORM     2103-KOREISYA-CHECK2-SEC
               END-IF
           END-IF
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL     (IDX      >   20)  OR
                                (RECE02-SRYKA(IDX)   =   SPACE)
               PERFORM    2104-RECE04-HENSYU-SEC
               PERFORM    2105-YKZFTN-SET-SEC
           END-PERFORM
           PERFORM     2106-JNISSU-SET-SEC                                   
           PERFORM     2107-KHNTEN-CLEAR-SEC
           PERFORM     2108-BUNTENSU-CLEAR-SEC                               
           PERFORM     2109-SAIKEI-KEISAN-SEC
           PERFORM     2110-NYUYOJI-COMENTFLG-SET-SEC
           PERFORM     2111-ICHIBUFUTAN-OKIKAE-SEC
      *
      *    レセプト請求区分による公費情報の振り分け処理
           MOVE    ZERO            TO  FLG-RECESKYKBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
                   OR      FLG-RECESKYKBN  =   1 
               IF    ( RECE02-KOHNUM     (IDX) NOT =   SPACE )
               AND   ( RECE02-RECESKYKBN (IDX) NOT =   SPACE )
                   MOVE    1                TO  FLG-RECESKYKBN
               END-IF
           END-PERFORM
           IF     FLG-RECESKYKBN        =   1
               PERFORM 2112-RECESKYKBN-HENSYU-SEC
           END-IF
      *
           IF      IDZ                  =   1
               IF ( ( RECE02-TEISYUTUSAKI   =   1               )  AND
                    ( SYS-2005-SYAOCRKBN    =   ZERO    OR  "2" )      )
               OR ( ( RECE02-TEISYUTUSAKI   =   2               )  AND
                    ( SYS-2005-KKHOCRKBN    =   "1"     OR  "3" )      )
      *
      *            医療保険・公費のフラグ
                   MOVE    SPACE              TO  WRK-3SYU-KBN
                   MOVE    ZERO               TO  SEIHIDX
                   MOVE    ZERO               TO  KAKKIDX
                   MOVE    ZERO               TO  SESNIDX
                   PERFORM   VARYING   IDX  FROM    1   BY  1
                         UNTIL   ( IDX                >    4    )
                         OR      ( RECE02-KOHID(IDX)  =   SPACE )
      *                生保
                       IF  RECE02-KOHNUM (IDX)  =   "012"
                           MOVE    "1"          TO  WRK-3SYU (1:1)
                           MOVE     IDX         TO  SEIHIDX
                       END-IF
      *                結核
                       IF  RECE02-KOHNUM (IDX)  =   "010"  OR  "011"
                           MOVE    "1"          TO  WRK-3SYU (2:1)
                           MOVE     IDX         TO  KAKKIDX
                       END-IF
      *                精神
                       IF  RECE02-KOHNUM (IDX)  =   "020"  OR  "021"
                           MOVE    "1"          TO  WRK-3SYU (3:1)
                           MOVE     IDX         TO  SESNIDX
                       END-IF
                   END-PERFORM
      *    
                   IF      WRK-3SYU-KBN           =   ALL "1" AND 
                           RECE02-HKNNUM          =   SPACE            
      *                ３種の場合
                       IF  KAKKIDX         >   SESNIDX
                           MOVE    SESNIDX     TO  IDA
                           MOVE    KAKKIDX     TO  IDB
                       ELSE
                           MOVE    KAKKIDX     TO  IDA
                           MOVE    SESNIDX     TO  IDB
                       END-IF
                       COMPUTE IDX5     =   SEIHIDX    +   1
                       COMPUTE IDX6     =   IDA        +   1
                       COMPUTE IDX7     =   IDB        +   1
                   ELSE    
      *                ３種以外の場合
                       MOVE    1               TO  IDA
                       MOVE    2               TO  IDB
                       MOVE    1               TO  IDX5
                       MOVE    2               TO  IDX6
                       MOVE    3               TO  IDX7
                   END-IF
      *
                   PERFORM     2113-OCR1-HENSYU-SEC
                   PERFORM     2114-OCR2-HENSYU-SEC
                   PERFORM     2115-OCR3-HENSYU-SEC
               END-IF
           END-IF
      *
           PERFORM     RECE02-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せに全国公費を含むかチェック
      *****************************************************************
       2101-KOREISYA-KOHI-SEC      SECTION.
      *
           MOVE      ZERO        TO     FLG-ZENKOH-ARI
           PERFORM   VARYING  IDY2    FROM    1   BY  1
                      UNTIL   IDY2    >       10
                       OR     RECE02-HKNCOMBI (IDY2)  =   ZERO
               INITIALIZE                  HKNCOMBI-REC
               MOVE    RECE02-HOSPID   TO  COMB-HOSPID
               MOVE    RECE02-PTID     TO  COMB-PTID
               MOVE    RECE02-HKNCOMBI(IDY2)
                                       TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
               PERFORM 900-HKNCOMBI-INV-SEC
               IF      FLG-HKNCOMBI    =   ZERO
                  IF   COMB-KOH1HKNNUM =  "027"
                    IF  (COMB-KOH2HKNNUM(1:1)  =  "0" OR "9")
                    OR  (COMB-KOH3HKNNUM(1:1)  =  "0" OR "9")
                    OR  (COMB-KOH4HKNNUM(1:1)  =  "0" OR "9")
                       MOVE     1        TO     FLG-ZENKOH-ARI
                       MOVE    10        TO     IDY2
                    END-IF
                  ELSE
                    IF  (COMB-KOH1HKNNUM(1:1)  =  "0" OR "9")
                    OR  (COMB-KOH2HKNNUM(1:1)  =  "0" OR "9")
                    OR  (COMB-KOH3HKNNUM(1:1)  =  "0" OR "9")
                    OR  (COMB-KOH4HKNNUM(1:1)  =  "0" OR "9")
                       MOVE     1        TO     FLG-ZENKOH-ARI
                       MOVE    10        TO     IDY2
                    END-IF
                  END-IF
               END-IF
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-PERFORM
      *
           .
       2101-KOREISYA-KOHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢者の一部負担金額記載に関するチェック
      *****************************************************************
       2102-KOREISYA-CHECK-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-CHK-HKNJANUM
                                           WRK-CHK-HKNNUM
           MOVE    ZERO                TO  WRK-CHK-IDOU
                                           WRK-CHK-TAISYO
                                           WRK-CHK-HKNID
           MOVE    ZERO                TO  WRK-CHK-ICHIBUGAKU
      *
           INITIALIZE                      RECE09-REC
           MOVE    RECE02-HOSPID       TO  RECE09-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE09-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE09-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE09-PTID
           MOVE    RECE02-RECEKA       TO  RECE09-RECEKA
      *
           START   RECE09-FILE
               KEY IS    >=   RECE09-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE09
               NOT INVALID
                   PERFORM RECE09-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE09           =   1                )
                   OR    ( RECE02-HOSPID    NOT =   RECE09-HOSPID    )
                   OR    ( RECE02-SRYYM     NOT =   RECE09-SRYYM     )
                   OR    ( RECE02-NYUGAIKBN NOT =   RECE09-NYUGAIKBN )
                   OR    ( RECE02-PTID      NOT =   RECE09-PTID      )
                   OR    ( RECE02-RECEKA    NOT =   RECE09-RECEKA    )
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "1"
                     EVALUATE    WRK-CHK-IDOU
                       WHEN    ZERO
                         IF   RECE09-HKNJANUM  NOT =  SPACE
                            MOVE    1             TO  WRK-CHK-IDOU
                            MOVE  RECE09-HKNJANUM TO  WRK-CHK-HKNJANUM
                            MOVE  RECE09-HKNID    TO  WRK-CHK-HKNID
                            MOVE  RECE09-HKNNUM   TO  WRK-CHK-HKNNUM
                         END-IF
                       WHEN     1
                         IF   RECE09-HKNJANUM  NOT =  WRK-CHK-HKNJANUM
                         AND  RECE09-HKNJANUM  NOT =  SPACE
                             MOVE    1            TO  WRK-CHK-TAISYO
                         END-IF
                       WHEN     2
                         MOVE    1                TO  WRK-CHK-TAISYO
                     END-EVALUATE
                   WHEN    "2"
                     EVALUATE    WRK-CHK-IDOU
                       WHEN    ZERO
                         IF   RECE09-HKNJANUM  NOT =  SPACE
                            MOVE    2             TO  WRK-CHK-IDOU
                            MOVE  RECE09-HKNJANUM TO  WRK-CHK-HKNJANUM
                            MOVE  RECE09-HKNID    TO  WRK-CHK-HKNID
                            MOVE  RECE09-HKNNUM   TO  WRK-CHK-HKNNUM
                         END-IF
                       WHEN     1
                         MOVE    1                TO  WRK-CHK-TAISYO
                       WHEN     2
                         IF   RECE09-HKNJANUM  NOT =  WRK-CHK-HKNJANUM
                         AND  RECE09-HKNJANUM  NOT =  SPACE
                             MOVE    1            TO  WRK-CHK-TAISYO
                         END-IF
                     END-EVALUATE
               END-EVALUATE
      *
               PERFORM RECE09-NEXT-SEC
           END-PERFORM
      *
           IF   WRK-CHK-TAISYO       =  1
           AND  RECE02-HKNJANUM  NOT =  SPACE
           AND  RECE02-HKNJANUM  NOT =  WRK-CHK-HKNJANUM
      *
                INITIALIZE                 ORCSKOREIFTNCHKAREA
                MOVE    RECE02-HOSPID  TO  LNK-KOREIFTNCHK-HOSPID
                MOVE    RECE02-PTID    TO  LNK-KOREIFTNCHK-PTID
                MOVE    "2"            TO  LNK-KOREIFTNCHK-NYUGAIKBN
                MOVE    RECE02-SRYYM   TO  LNK-KOREIFTNCHK-SRYYM
                MOVE    WRK-CHK-HKNNUM TO  LNK-KOREIFTNCHK-HKNNUM
                MOVE    WRK-CHK-HKNID  TO  LNK-KOREIFTNCHK-HKNID
                MOVE    RECE02-TEISYOTOKU
                                       TO  LNK-KOREIFTNCHK-TEISYOTOKU
                MOVE    RECE02-RECEKA  TO  LNK-KOREIFTNCHK-RECEKA
      *
                CALL    "ORCSKOREIFTNCHK"   USING
                                            ORCSKOREIFTNCHKAREA
                IF      LNK-KOREIFTNCHK-RC   =   ZERO
                    MOVE   LNK-KOREIFTNCHK-ICHIBUGAKU
                                       TO  FTN-KOH-XXX
                                           WRK-CHK-ICHIBUGAKU
                END-IF
      *
           END-IF
      *
           .
       2102-KOREISYA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００４）
      *****************************************************************
       2104-RECE04-HENSYU-SEC                SECTION.
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE02-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE02-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE02-PTID          TO      ACCT-PTID
           MOVE    RECE02-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE02-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY32"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY32"  TO      ORC-DBPATH
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE02-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE02-HKNCOMBI (IDX1)
      *
                           MOVE   ZERO           TO  FLG-RINYAK
      *
                           PERFORM    21041-KUMIAWASE-HANBETU-SEC
      *
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               MOVE  ZERO  TO   FLG-SHOSAISIN           
                               MOVE  ZERO  TO   WRK-SRYSURYO
                               MOVE  ZERO  TO   WRK-SRYSURYO2
                               MOVE  ZERO  TO   WRK-SRYSURYO3
                               MOVE  ZERO  TO   WRK-TANICD
                               MOVE  ZERO  TO   WRK-TANIMOJISU
                               MOVE  SPACE TO   WRK-TANINAME
                               MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                FLG-HOUTEN-COM
      *
                               MOVE  ZERO  TO   WRK-FILMTEN
      *
                               PERFORM 21042-RECE02-TOTALTEN-HENSYU-SEC     
                               PERFORM 21043-RECE04-TENSOU-SEC
                               PERFORM 21044-RECE05-HENSYU-SEC
                               PERFORM RECE04-WRITE-SEC
                           ELSE
                             IF   ACCT-ZAIKAISU  NOT =    ZERO
                               IF   ACCT-SRYKBN  =  "11"  OR  "12" 
                                                          OR  "99" 
                                 IF  ACCT-SRYCDTOTAL = CONS-TOKKI1 OR
                                                       CONS-TOKKI2 OR
                                                       CONS-TOKKI3 OR
                                                       CONS-TOKKI4 OR
                                                       CONS-TOKKI5 OR
                                                       CONS-TOKKI7 OR
                                                       CONS-TOKKI8 OR
                                                       CONS-TOKKI9 OR
                                                       CONS-TOKKI10 OR
                                                       CONS-TOKKI11 OR
                                                       CONS-TOKKI12
                                      MOVE    1    TO      FLG-TOKKI
                                      PERFORM   21045-TOKKI-HENSYU-SEC
                                 ELSE
                                      MOVE  ZERO  TO   FLG-SHOSAISIN    
                                      MOVE  ZERO  TO   WRK-SRYSURYO
                                      MOVE  ZERO  TO   WRK-SRYSURYO2
                                      MOVE  ZERO  TO   WRK-SRYSURYO3
                                      MOVE  ZERO  TO   WRK-TANICD
                                      MOVE  ZERO  TO   WRK-TANIMOJISU
                                      MOVE  SPACE TO   WRK-TANINAME
                                      MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                       FLG-HOUTEN-COM
      *
                                      MOVE  ZERO  TO   WRK-FILMTEN
      *
                                      PERFORM   21043-RECE04-TENSOU-SEC
                                      PERFORM   21044-RECE05-HENSYU-SEC
                                      PERFORM   RECE04-WRITE-SEC
                                 END-IF
                               ELSE
                                 INITIALIZE                SRYACT-REC
                                 MOVE RECE02-HOSPID    TO  SRY-HOSPID
                                 MOVE RECE02-NYUGAIKBN TO  SRY-NYUGAIKBN
                                 MOVE RECE02-PTID      TO  SRY-PTID
                                 MOVE RECE02-SRYYM     TO  SRY-SRYYM
                                 MOVE ACCT-ZAINUM      TO  SRY-ZAINUM
                                 MOVE CONS-RINYAK      TO  SRY-SRYCD(1)
                                 MOVE CONS-RINYAK      TO  SRY-SRYCD(2)
                                 MOVE CONS-RINYAK      TO  SRY-SRYCD(3)
                                 MOVE CONS-RINYAK      TO  SRY-SRYCD(4)
                                 MOVE CONS-RINYAK      TO  SRY-SRYCD(5)
                                 MOVE SRYACT-REC       TO  MCPDATA-REC
                                 MOVE "DBSELECT"       TO  MCP-FUNC
                                 MOVE "SRYACT-KEY8"    TO  ORC-DBPATH
                                 CALL "ORCMCPSUB"   USING  MCPAREA
                                                           ORCMCPAREA
                                                           MCPDATA-REC
                                 IF      MCP-RC        =  ZERO
                                   MOVE  "SRYACT-KEY8" TO  ORC-DBPATH 
                                   PERFORM   900-SRYACT-READ-SEC
                                   IF     FLG-SRYACT   =  ZERO
                                       MOVE    1       TO  FLG-RINYAK
                                   END-IF
                                 END-IF
                                 MOVE    "SRYACT-KEY8" TO  WK-PATH
                                 PERFORM   900-CSR-CLOSE-SEC
                                 IF   FLG-RINYAK  =  1
                                    MOVE  ZERO  TO   FLG-SHOSAISIN
                                    MOVE  ZERO  TO   WRK-SRYSURYO
                                    MOVE  ZERO  TO   WRK-SRYSURYO2
                                    MOVE  ZERO  TO   WRK-SRYSURYO3
                                    MOVE  ZERO  TO   WRK-TANICD
                                    MOVE  ZERO  TO   WRK-TANIMOJISU
                                    MOVE  SPACE TO   WRK-TANINAME
                                    MOVE  ZERO  TO   FLG-HOUSIJI-COM
                                                     FLG-HOUTEN-COM
      *
                                    MOVE  ZERO  TO   WRK-FILMTEN
      *
                                    PERFORM   21043-RECE04-TENSOU-SEC
                                    PERFORM   21044-RECE05-HENSYU-SEC
                                    PERFORM   RECE04-WRITE-SEC
                                 END-IF
                               END-IF
                             END-IF
                           END-IF
                           MOVE    10                TO  IDX1
                       END-IF
                   END-PERFORM          
                   MOVE     "SRYACCT-KEY32"  TO   ORC-DBPATH
                   PERFORM   900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACCT SELECT ERR" 
           END-IF
      *
           MOVE    "SRYACCT-KEY32"     TO  WK-PATH
           PERFORM 900-CSR-CLOSE-SEC
      *
           .
       2104-RECE04-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    組み合わせ判別（福祉用）主保険＋全国公費＋地方公費
      *****************************************************************
       21041-KUMIAWASE-HANBETU-SEC               SECTION.
      *
           MOVE     ZERO      TO      FLG-FUKUSHI
      *
           IF      IDZ             =   1 
             EVALUATE   SYS-1001-PREFNUM
      *
               WHEN    "38"
      *
                 IF ( RECE02-KOHNUMHC(IDX1 1)(1:1)  =   "0"     )  AND
                    ( RECE02-KOHNUMHC(IDX1 2)(1:1)
                                             >=  "1" AND <=  "8")
      *
                      MOVE     1       TO      FLG-FUKUSHI
      *
                      MOVE    "1"      TO      RECE02-FUKUSHIFLG
      *
                 END-IF
      *
             END-EVALUATE
           END-IF
      *
           .
       21041-KUMIAWASE-HANBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）:トータル点編集
      *****************************************************************
       21042-RECE02-TOTALTEN-HENSYU-SEC                SECTION.               
      *
           IF    (RECE02-HKNNUM          NOT =   SPACE)   OR
                 (RECE02-RJNHKNNUM       NOT =   SPACE)
               IF        ACCT-SRYKBN        =    "95"   OR  "96"
                   CONTINUE
               ELSE
                 IF        ACCT-SRYCDTOTAL    =    630010002
                   COMPUTE  RECE02-TOTALTEN(1) =  RECE02-TOTALTEN(1) -
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                   COMPUTE  RECE02-HENTOTALTEN(1) 
                                            =  RECE02-HENTOTALTEN(1) -
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                 ELSE
                   COMPUTE  RECE02-TOTALTEN(1) =  RECE02-TOTALTEN(1) +
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                   COMPUTE  RECE02-HENTOTALTEN(1) 
                                            =  RECE02-HENTOTALTEN(1) +
                                                 (ACCT-ZAITEN  *  
                                                  ACCT-ZAIKAISU)
                 END-IF
               END-IF
           END-IF
      *
           MOVE    IDX1    TO    HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL   (IDX2      >       4)
               MOVE   ZERO               TO        KH-IDX
               IF     RECE02-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(1)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(2)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(3)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(4)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                  IF        ACCT-SRYKBN        =    "95"   OR  "96"
                     CONTINUE
                  ELSE
                     IF        ACCT-SRYCDTOTAL    =    630010002
                         COMPUTE  RECE02-HENTOTALTEN(KH-IDX) =
                                        RECE02-HENTOTALTEN(KH-IDX) -
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                     ELSE
                         COMPUTE  RECE02-HENTOTALTEN(KH-IDX) =
                                        RECE02-HENTOTALTEN(KH-IDX) +
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                     END-IF
                     IF     FLG-FUKUSHI  =   1
                     AND    RECE02-KOHNUM(KH-IDX - 1)(1:1)
                                             >=  "1" AND <=  "8"
                         CONTINUE
                     ELSE
                       IF        ACCT-SRYCDTOTAL    =    630010002
                         COMPUTE  RECE02-TOTALTEN(KH-IDX) =
                                              RECE02-TOTALTEN(KH-IDX) -
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                       ELSE
                         COMPUTE  RECE02-TOTALTEN(KH-IDX) =
                                              RECE02-TOTALTEN(KH-IDX) +
                                             (ACCT-ZAITEN * 
                                              ACCT-ZAIKAISU)
                       END-IF
                     END-IF
                  END-IF
                 END-IF
               END-IF  
           END-PERFORM
      *
           .
       21042-RECE02-TOTALTEN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００４）
      *****************************************************************
       21043-RECE04-TENSOU-SEC                SECTION.
      *
           MOVE    SPACE                TO      RECE04-REC
           INITIALIZE                           RECE04-REC
           MOVE    RECE02-HOSPID        TO      RECE04-HOSPID
           MOVE    RECE02-SRYYM         TO      RECE04-SRYYM
           MOVE    RECE02-NYUGAIKBN     TO      RECE04-NYUGAIKBN
           MOVE    RECE02-PTID          TO      RECE04-PTID
           MOVE    RECE02-RECEKA        TO      RECE04-RECEKA
           MOVE    RECE02-TEISYUTUSAKI  TO      RECE04-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU   TO      RECE04-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY  TO      RECE04-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY   TO      RECE04-HOJOKBN
           MOVE    RECE02-TEKSTYMD      TO      RECE04-TEKSTYMD
           MOVE    ZERO                 TO      RECE04-KOHBUNRI
           MOVE    ACCT-SRYKBN          TO      RECE04-SRYKBN
           MOVE    ACCT-ZAINUM          TO      RECE04-ZAINUM
           MOVE    ACCT-SRYCDTOTAL      TO      WRK-SRYCDTOTAL9
           IF      WRK-SRYCDTOTALX(6:9) =       CONS-GAIKAN1   OR
                                                CONS-GAIKAN2   OR
                                                CONS-GAIKAN3
               ADD     1                TO      WRK-ZAINUM
               MOVE    WRK-ZAINUM       TO      RECE04-ZAINUM
           END-IF
           MOVE    RECE02-PTNUM         TO      RECE04-PTNUM
      *
           MOVE    ACCT-SRYKA           TO      RECE04-SRYKA
           MOVE    ACCT-HKNCOMBI        TO      RECE04-HKNCOMBI
           MOVE    ACCT-JIHOKBN         TO      RECE04-JIHOKBN
           MOVE    ACCT-ZAITEN          TO      RECE04-ZAITEN
           MOVE    ACCT-SRYCDTOTAL      TO      RECE04-SRYCDTOTAL
           MOVE    ACCT-SURYOUTOTAL     TO      RECE04-SURYOUTOTAL
           MOVE    ACCT-MEISAISU        TO      RECE04-MEISAISU
           MOVE    ACCT-ZAIKAISU        TO      RECE04-ZAIKAISU
           MOVE    ACCT-SYUTEN1         TO      RECE04-SYUTEN1
           MOVE    ACCT-SYUTEN2         TO      RECE04-SYUTEN2
           MOVE    ACCT-YKZTEN          TO      RECE04-YKZTEN
           MOVE    ACCT-KIZAITEN        TO      RECE04-KIZAITEN
           MOVE    ACCT-DRCD            TO      RECE04-DRCD
           MOVE    ACCT-ZAIREQFLG       TO      RECE04-ZAIREQFLG
           MOVE    ACCT-TEIGENRATE      TO      RECE04-TEIGENRATE
           MOVE        ZERO             TO      WK-TAIHI-DAY
           MOVE        ZERO             TO      WK-TAIHI-FLG
           PERFORM     VARYING   IDX3   FROM    1    BY   1
                       UNTIL     IDX3   >       4
               PERFORM     VARYING   IDX2   FROM    1    BY   1
                           UNTIL     IDX2   >       31
                   MOVE    ACCT-DAY(IDX3 IDX2)  TO
                                            RECE04-KAISU (IDX3 IDX2)
                   IF     (WK-TAIHI-FLG            =   ZERO)   AND
                          (ACCT-DAY(1 IDX2)    NOT =   ZERO)
                       MOVE     IDX2    TO      WK-TAIHI-DAY
                       MOVE     1       TO      WK-TAIHI-FLG
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
           IF    FLG-RINYAK    =    1
                MOVE    "1"     TO     RECE04-SAFLG
           END-IF
      *
           .
       21043-RECE04-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの作成（Ｆ００５）
      *****************************************************************
       21044-RECE05-HENSYU-SEC               SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
           MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY2"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY2"    TO      ORC-DBPATH
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   MOVE   ZERO   TO  FLG-ZANHAIKI
                   MOVE   ZERO   TO  WRK-RENNUM
                   PERFORM   210441-RECE05-TENSOU-SEC
                   PERFORM   210442-RECE02-HENSYU-SEC
                   PERFORM   RECE05-WRITE-SEC
                   IF  FLG-ZANHAIKI  =  1
                      PERFORM  RECE05-REWRITE-SEC
                   END-IF
                   MOVE     "SRYACT-KEY2"    TO   ORC-DBPATH
                   PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SRYACT SELECT ERR" 
      *         ACCEPT     WRK-IN FROM CONSOLE
           END-IF
      *
           MOVE   "SRYACT-KEY2"      TO  WK-PATH
           PERFORM   900-CSR-CLOSE-SEC
      *
           IF     WRK-FILMTEN  NOT =  ZERO
                COMPUTE    RECE04-SYUTEN2  =  WRK-FILMTEN + 0.5
           END-IF
      *
           .
       21044-RECE05-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００５）
      *****************************************************************
       210441-RECE05-TENSOU-SEC                SECTION.
      *
           INITIALIZE                   WRK-TNS-TAIHI-AREA
      *
           MOVE    SPACE                TO      RECE05-REC
           INITIALIZE                           RECE05-REC
           MOVE    RECE02-HOSPID        TO      RECE05-HOSPID
           MOVE    RECE02-SRYYM         TO      RECE05-SRYYM
           MOVE    RECE02-NYUGAIKBN     TO      RECE05-NYUGAIKBN
           MOVE    RECE02-PTID          TO      RECE05-PTID
           MOVE    RECE02-RECEKA        TO      RECE05-RECEKA
           MOVE    RECE02-TEISYUTUSAKI  TO      RECE05-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU   TO      RECE05-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY  TO      RECE05-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY   TO      RECE05-HOJOKBN
           MOVE    RECE02-TEKSTYMD      TO      RECE05-TEKSTYMD
           MOVE    ZERO                 TO      RECE05-KOHBUNRI
           MOVE    SRY-ZAINUM           TO      RECE05-ZAINUM
           IF      RECE04-ZAINUM        >       88888880
               MOVE   RECE04-ZAINUM     TO      RECE05-ZAINUM
           END-IF
           MOVE    SRY-RENNUM           TO      RECE05-RENNUM
           MOVE    RECE02-PTNUM         TO      RECE05-PTNUM
      *
           MOVE    SRY-SRYKA            TO      RECE05-SRYKA
           MOVE    SRY-SRYKBN           TO      RECE05-SRYKBN
           MOVE    SRY-SRYSYUKBN        TO      RECE05-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL   TO      RECE05-JIHIMONEY
           MOVE    ZERO                 TO      FLG-OSINKBN
                                                IDX4
           PERFORM     VARYING   IDX2   FROM    1    BY   1
                       UNTIL    (IDX2      >    5)    OR
                                (SRY-SRYCD(IDX2)  =   SPACE)
      *
      *
             PERFORM 900-TENSU-TBL-SEC
      **     点数マスタ情報退避
             MOVE    TNS-CHUKSNTUBAN    TO  WRK-TNS-CHUKSNTUBAN(IDX2)
             MOVE    TNS-DATAKBN        TO  WRK-TNS-DATAKBN(IDX2)
             MOVE    TNS-TIMEKSNKBN     TO  WRK-TNS-TIMEKSNKBN(IDX2)
             MOVE    TNS-TEN            TO  WRK-TNS-TEN(IDX2)
             MOVE    TNS-GAITENTTLKBN   TO  WRK-TNS-GAITENTTLKBN(IDX2)
             MOVE    TNS-OSINKBN        TO  WRK-TNS-OSINKBN(IDX2)
             MOVE    TNS-KNSJISKBN      TO  WRK-TNS-KNSJISKBN(IDX2)
             MOVE    TNS-SRYCD          TO  WRK-TNS-SRYCD(IDX2)
      *
             IF     ( SRY-SRYCD(IDX2)(1:1)  =  "S"   )
                  COMPUTE   RECE04-MEISAISU  =  RECE04-MEISAISU
                                             -  1
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
             OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                     (TNS-TEN               =  ZERO )       )
                  COMPUTE   RECE04-MEISAISU    =  RECE04-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE04-SRYCDTOTAL  =  RECE04-SRYCDTOTAL
                                               -  WRK-SRYCD
                  COMPUTE   RECE04-SURYOUTOTAL =  RECE04-SURYOUTOTAL
                                               -  1
             END-IF
      *
             IF     ( SRY-SRYCD(IDX2)  =  CONS-TOKKI1(6:9)
                                      OR  CONS-TOKKI2(6:9)
                                      OR  CONS-TOKKI3(6:9)
                                      OR  CONS-TOKKI4(6:9)
                                      OR  CONS-TOKKI5(6:9)
                                      OR  CONS-TOKKI7(6:9)
                                      OR  CONS-TOKKI8(6:9)
                                      OR  CONS-TOKKI9(6:9)
                                      OR  CONS-TOKKI10(6:9)
                                      OR  CONS-TOKKI11(6:9)
                                      OR  CONS-TOKKI12(6:9) )
                  COMPUTE   RECE04-MEISAISU    =  RECE04-MEISAISU
                                               -  1
                  MOVE      SRY-SRYCD(IDX2)   TO  WRK-SRYCD
                  COMPUTE   RECE04-SRYCDTOTAL  =  RECE04-SRYCDTOTAL
                                               -  WRK-SRYCD
                  COMPUTE   RECE04-SURYOUTOTAL =  RECE04-SURYOUTOTAL
                                               -  1
                  MOVE    2    TO      FLG-TOKKI
                  PERFORM   21045-TOKKI-HENSYU-SEC
             END-IF
      *
             IF      SRY-SRYCD(IDX2)(1:1)     =   "0" OR  "1" OR
                                                  "6" OR  "7" OR
                                                  "8"
      *
              IF     ( SRY-SRYCD(IDX2)  =  CONS-TOKKI1(6:9)
                                       OR  CONS-TOKKI2(6:9)
                                       OR  CONS-TOKKI3(6:9)
                                       OR  CONS-TOKKI4(6:9)
                                       OR  CONS-TOKKI5(6:9)
                                       OR  CONS-TOKKI7(6:9)
                                       OR  CONS-TOKKI8(6:9)
                                       OR  CONS-TOKKI9(6:9)
                                       OR  CONS-TOKKI10(6:9)
                                       OR  CONS-TOKKI11(6:9)
                                       OR  CONS-TOKKI12(6:9) )
              OR     ( SRY-SRYCD(IDX2)(1:3)  =  "001"        )
              OR     ((SRY-SRYCD(IDX2)(1:3)  =  "059") AND
                      (TNS-TEN               =  ZERO )       )
      *
               CONTINUE
      *
              ELSE
      *
               ADD     1                  TO    IDX4
      *
               MOVE    SRY-SRYCD(IDX2)    TO    RECE05-SRYCD(IDX4)
      *
               IF      SRY-SRYKBN         =     "12"
                   IF   SRY-SRYCD(IDX2)   =     CONS-SAISHIN1    OR
                                                CONS-SAISHIN2    OR
                                                CONS-SAISHIN3    OR
                                                CONS-SAISHIN4    OR
                                                CONS-SAISHIN5    OR
                                                CONS-SAISHIN6    OR
                                                CONS-SAISHIN7    OR
                                                CONS-SAISHIN8    OR
                                                CONS-SAISHIN9    OR
                                                CONS-SAISHIN10   OR
                                                CONS-SAISHIN11   OR
                                                CONS-SAISHIN12   OR
                                                CONS-SAISHIN13   OR
                                                CONS-SAISHIN14   OR
                                                CONS-SAISHIN15   OR
                                                CONS-SAISHIN16
                        MOVE   "1"        TO    RECE02-SAISHIN-PRTKBN
                   END-IF
                   IF   SRY-SRYCD(IDX2)   =     CONS-GAIRAIKSN1    OR
                                                CONS-GAIRAIKSN2    OR
                                                CONS-GAIRAIKSN3    OR
                                                CONS-GAIRAIKSN4    OR
                                                CONS-GAIRAIKSN5    OR
                                                CONS-GAIRAIKSN6    OR
                                                CONS-GAIRAIKSN7
                        MOVE   "1"        TO    RECE02-GAIRAIKSN-PRTKBN
                   END-IF
               END-IF
      *
               IF      SRY-SRYKBN         =     "13"
                   IF   SRY-SRYCD(IDX2)   =     CONS-SIDORYO1    OR
                                                CONS-SIDORYO2    OR
                                                CONS-SIDORYO3    OR
                                                CONS-SIDORYO4    OR
                                                CONS-SIDORYO5    OR
                                                CONS-SIDORYO6
                        MOVE   "1"        TO    RECE02-BEDSUFLG
                   END-IF
               END-IF
      *
               IF     SRY-SRYCD(IDX2)  IS  NUMERIC
                 IF  SRY-SRYCD(IDX2)(1:3) NOT = "001"
                   MOVE    SRY-SRYCD(IDX2)  TO  WRK-SRYCD
                   COMPUTE  RECE04-RUIKEITOTAL  =
                            RECE04-RUIKEITOTAL  +
                           (WRK-SRYCD  *  SRY-SRYSURYO(IDX2))
                 END-IF
               END-IF
      *
               IF      SRY-SRYKBN    =   "40" OR "50" OR "54" OR "60"
                  IF   SRY-SRYCD(IDX2)(1:1)  =  "7"
                  AND  TNS-DATAKBN           =  "3"
                         COMPUTE WRK-FILMTEN2   =
                                (TNS-TEN  * SRY-SRYSURYO(IDX2) / 10)
                         IF   TNS-JGNTEN  NOT =  ZERO
                         AND  TNS-JGNTEN   <  WRK-FILMTEN2
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      TNS-JGNTEN
                         ELSE
                             COMPUTE  WRK-FILMTEN  =
                                      WRK-FILMTEN  +
                                      WRK-FILMTEN2
                         END-IF
                  END-IF
               END-IF
      *
               MOVE    SRY-SRYSURYO(IDX2) TO    RECE05-SRYSURYO(IDX4)
               MOVE    SRY-SRYKAISU(IDX2) TO    RECE05-SRYKAISU(IDX4)
               MOVE    SRY-MEISKYFLG(IDX2) TO   RECE05-MEISKYFLG(IDX4)
               MOVE    SRY-AUTOKBN(IDX2)  TO    RECE05-AUTOKBN(IDX4)
               MOVE    SRY-INPUTNUM(IDX2) TO    RECE05-INPUTNUM(IDX4)
      *
               IF      RECE02-RECESYUBETU  =  8888
               AND     SYS-2005-JIHITEKKSIKBN  =  "1"
                 IF      SRY-SRYKBN    =     "95" OR "96"
                   IF      SRY-SRYCD(IDX2)(1:3)  =  "095" OR "096"
                     IF      SRY-SRYCD(IDX2)(1:5)  =  "09500" OR "09600"
                                                  OR  "09591" OR "09691"
                                                  OR  "09592" OR "09692"
                                                  OR  "09593" OR "09693"
                                                  OR  "09594" OR "09694"
                          CONTINUE
                     ELSE
                       IF     TNS-TEN     NOT =  ZERO
                          ADD     TNS-TEN    TO   RECE04-SYUTEN1
                          MOVE    "1"        TO   RECE02-JIHTEKFLG
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
      *
      **診療実日数のカウントを行える剤であるかの判定
               IF  ((ACCT-ZAITEN    NOT =    ZERO)    AND               
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND               
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR           
                                           "13"  OR  "14"     ))        
               OR  ((ACCT-ZAITEN        =    ZERO)    AND               
                    (ACCT-ZAIKAISU  NOT =    ZERO)    AND               
                    (ACCT-SRYKBN        =  "11"  OR  "12"  OR           
                                           "99"               ))        
      *
                       IF ( (TNS-JITUDAY    =     2          ) AND      
                            (TNS-DAYCNT     =     1   OR   2 )    )     
                       OR (  SRY-SRYCD(IDX2)  =  "112700850"            
                                              OR "112003350"            
                                              OR "099409901"            
                                              OR "099999902"            
                                              OR "099110001"            
                                              OR "099120001"            
      *                              同日再診
                                              OR "112010150"
                                              OR "112010250"
                                              OR "112010350"
                                              OR "112010450"
                                              OR "112010550"
                                              OR "112704450"
                                              OR "112704550"
                                              OR "112704650"
                                              OR "112704750"
                                              OR "112010650"
                                              OR "112010750"
                                              OR "112010850"
                                              OR "112010950"
                                              OR "112012850"
                                              OR "112704850"
                                              OR "112704950"
                                              OR "112705050"
                                              OR "112706850"
                                              OR "112008850"
                                              OR "112008950"
                                              OR "112009050"
                                              OR "112009150"
                                              OR "112012550"
                                              OR "112703450"
                                              OR "112703550"
                                              OR "112703650"
                                              OR "112706550"
                                              OR "112008350"
                                              OR "112008450"
                                              OR "112008550"
                                              OR "112008650"
                                              OR "112008750"
                                              OR "112703050"
                                              OR "112703150"
                                              OR "112703250"
                                              OR "112703350"
      *
                                              OR "112011710"
                                              OR "112011810"
                                              OR "112011910"
                                              OR "112012010"
                                              OR "112012210"
                                              OR "112705850"
                                              OR "112705950"
                                              OR "112706050"      )
      *
                             MOVE    1      TO    FLG-SHOSAISIN         
      *
                       END-IF                                           
      *
               END-IF                                                   
      *
               IF      TNS-SRYKBN         =     "80"
                   IF  TNS-GAITENTTLKBN   =     130
                       MOVE   "13"        TO    RECE04-SRYKBN
                                                RECE05-SRYKBN
                   END-IF
               END-IF
      *
               PERFORM  2104411-RECE05-TENSOU-COMCNT-HEN-SEC
      *
               IF  SRY-SRYCD(IDX2)        =     CONS-ZANHAIKI           
                  COMPUTE WRK-SRYSURYO2   =     WRK-SRYSURYO            
                                          +     0.999                   
                  COMPUTE RECE05-SRYSURYO(IDX4)                         
                                          =     WRK-SRYSURYO2           
                                          -     WRK-SRYSURYO            
                  IF  RECE05-SRYSURYO(IDX4)  NOT = ZERO                 
                    MOVE  WRK-TANICD      TO    RECE05-TANICD(IDX4)     
                    MOVE  WRK-TANIMOJISU  TO    RECE05-TANIMOJISU(IDX4) 
                    MOVE  WRK-TANINAME    TO    RECE05-TANINAME(IDX4)   
                    IF    RECE05-RENNUM  >  1                           
                    AND   IDX4           =  1                           
                      MOVE    1           TO    FLG-ZANHAIKI            
                      COMPUTE  WRK-RENNUM  =  RECE05-RENNUM  -  1       
                      MOVE WRK-SRYSURYO2  TO  WRK-SRYSURYO3             
                    ELSE                                                
                      MOVE WRK-SRYSURYO2  TO  RECE05-SRYSURYO(IDX4 - 1) 
                    END-IF
                  END-IF                                                
               ELSE                                                     
                  MOVE    SRY-SRYSURYO(IDX2)                            
                                          TO    WRK-SRYSURYO            
                  MOVE    TNS-TANICD      TO    RECE05-TANICD(IDX4)
                                                WRK-TANICD              
                  MOVE    TNS-TANIMOJISU  TO    RECE05-TANIMOJISU(IDX4)
                                                WRK-TANIMOJISU          
                  MOVE    TNS-TANINAME    TO    RECE05-TANINAME(IDX4)
                                                WRK-TANINAME            
               END-IF                                                   
      *********IF      TNS-TENSIKIBETU    =     1   OR   3
      *********    MOVE  TNS-TENSIKIBETU  TO   RECE05-TENSIKIBETU(IDX4)
      *********    MOVE  TNS-TEN          TO    RECE05-TEN(IDX4)
      *********END-IF
               MOVE  TNS-TENSIKIBETU  TO   RECE05-TENSIKIBETU(IDX4)
               MOVE  TNS-TEN          TO   RECE05-TEN(IDX4)
               IF      RECE02-NYUGAIKBN   =     "1"
                   MOVE  TNS-NYUTENTTLKBN TO    RECE05-TENTTLKBN(IDX4)
               ELSE
                   MOVE  TNS-GAITENTTLKBN TO    RECE05-TENTTLKBN(IDX4)
               END-IF
               IF     (SRY-SRYCD    (IDX2)(1:1)     =   "8"  )
               OR     (SRY-SRYCD    (IDX2)(1:3)     =   "008")
                  MOVE ZERO               TO    RECE05-YUKOKETA(IDX4)
               ELSE
                  MOVE TNS-YUKOKETA       TO    RECE05-YUKOKETA(IDX4)
               END-IF
               MOVE    TNS-DATAKBN        TO    RECE05-DATAKBN (IDX4)
               MOVE    TNS-HOUKNSKBN      TO    RECE05-HOUKNSKBN(IDX4)
               MOVE    TNS-KNSJISKBN      TO    RECE05-KNSJISKBN(IDX4)
               MOVE    TNS-CDKBN-KBN      TO    RECE05-CDKBN-KBN(IDX4)
               MOVE    TNS-CDKBN-SYO      TO    RECE05-CDKBN-SYO(IDX4)
               MOVE    TNS-CDKBN-BU       TO    RECE05-CDKBN-BU(IDX4)
               MOVE    TNS-CDKBN-KBNNUM   
                                      TO RECE05-CDKBN-KBNNUM(IDX4)
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                      TO RECE05-CDKBN-KBNNUM-EDA(IDX4)
               MOVE    TNS-CDKBN-KOUBAN
                                      TO RECE05-CDKBN-KOUBAN(IDX4)
               MOVE    TNS-KOUHYOJYUNNUM  
                                      TO RECE05-KOUHYOJYUNNUM(IDX4)
               MOVE    TNS-SRYSYUKBN  TO RECE05-SRYSYUKBN-X  (IDX4)
               MOVE    TNS-KOKUJISKBKBN1  
                                      TO RECE05-KOKUJISKBKBN1(IDX4)
               MOVE    TNS-KZMCOMPSIKIBETU
                                      TO RECE05-KZMCOMPSIKIBETU(IDX4)
               MOVE    TNS-CHUKSNCD   TO RECE05-CHUKSNCD       (IDX4)
               MOVE    TNS-CHUKSNTUBAN
                                      TO RECE05-CHUKSNTUBAN    (IDX4)
      *
               IF     TNS-SRYCD       =  "170022290"  OR
                                         "170022390"  OR
                                         "170022490"
                      MOVE   202      TO  RECE05-CDKBN-KBNNUM(IDX4)
                      MOVE   99       TO  RECE05-CDKBN-KOUBAN(IDX4)
               END-IF
      *
               IF    ((SRY-SRYCD    (IDX2)(1:1)     =   "8"  )  OR
                      (SRY-SRYCD    (IDX2)(1:3)     =   "008"))
               AND   ( SRY-INPUTNUM (IDX2)      NOT =   ZERO  )
                   MOVE    SRY-HOSPID           TO  PTCOM-HOSPID
                   MOVE    SRY-PTID             TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM           TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD    (IDX2)  TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM (IDX2)  TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT    TO  RECE05-NAME (IDX4)
                                                    RECE05-INPUTCOMENT
                                                                (IDX4)
      *            
                   IF      SRY-SRYCD (IDX2)(1:1)
                                                =   "8"
                       IF      PTCOM-INPUTCHI1     =   SPACE
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           MOVE    TNS-NAME        TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-MAX     >   ZERO )
                               COMPUTE IDXX    =   KANACHK-MAX +   1
                               MOVE    PTCOM-INPUTCOMENT (IDXX:)
                                                       TO
                                           RECE05-INPUTCOMENT   (IDX4)
                           ELSE
                               MOVE    PTCOM-INPUTCOMENT
                                                       TO
                                           RECE05-INPUTCOMENT   (IDX4)
                           END-IF
                       ELSE
                           INITIALIZE              ORCSKANACHKAREA
                           MOVE    "2"             TO  KANACHK-SYORI 
                           STRING  PTCOM-INPUTCHI1 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI2 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI3 DELIMITED  BY  SPACE
                                   PTCOM-INPUTCHI4 DELIMITED  BY  SPACE
                                   INTO   KANACHK-MAE-INPUT
                           END-STRING
                           CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC      =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO
                                           RECE05-INPUTCOMENT   (IDX4)
                           END-IF
                       END-IF
                   END-IF
      *
                   IF    RECE04-COMCDFLG   NOT =   "2"
                      MOVE   "1"           TO    RECE04-COMCDFLG
                   END-IF
               ELSE
                   MOVE    TNS-NAME        TO    RECE05-NAME    (IDX4)
                                                 RECE05-INPUTCOMENT
                                                                (IDX4)
                   MOVE    TNS-YUKOKETA    TO    RECE05-YUKOKETA(IDX4)
                   IF      TNS-SRYCD       =     CONS-RJNRIGAKU1  OR
                                                 CONS-RJNRIGAKU2  OR
                                                 CONS-RJNRIGAKU3  OR
                                                 CONS-RJNRIGAKU4  OR
                                                 CONS-RJNRIGAKU5  OR
                                                 CONS-RJNRIGAKU6  OR
                                                 CONS-RJNRIGAKU7  OR
                                                 CONS-RJNRIGAKU8
                         MOVE  "老人"    TO   RECE05-NAME(IDX4)(1:4)
                         MOVE  TNS-NAME  TO   RECE05-NAME(IDX4)(5:) 
                         COMPUTE  RECE05-YUKOKETA(IDX4) =
                                  RECE05-YUKOKETA(IDX4) + 2
                   END-IF
               END-IF
      *
               IF          SRY-SRYKBN       =  "21"
                    IF   SRY-SRYCD(IDX2)  =  "820000047" OR 
                                             "630010002"
                        MOVE     "2"       TO    RECE04-COMCDFLG
                    END-IF
               END-IF
      *
               IF          SRY-SRYCD(IDX2)  =    CONS-HIKAKIN
                                           OR    CONS-SEIBUTU
                                           OR    CONS-MAYAKU
                   MOVE    SPACE           TO    RECE05-NAME(IDX4)
               END-IF
               IF  ( SRY-SRYSYUKBN         =  "334"       )
               AND ((SRY-SRYCD(IDX2)       =  "810000001") OR
                    (SRY-SRYCD(IDX2)(1:3)  =  "008"      ))
               AND ( FLG-HOUTEN-COM        =   ZERO       )
                    PERFORM  VARYING  IDV  FROM  1  BY  2
                            UNTIL  (IDV            >   200 )
                             OR    (FLG-HOUTEN-COM =     1 )
                      IF   RECE05-NAME(IDX4)(IDV:2)   =   "訪"
                         MOVE   SPACE  TO    RECE04-FLG-HOUTEN-COMMENT
                         MOVE    1     TO    FLG-HOUTEN-COM
                      END-IF
                    END-PERFORM
               END-IF
      *
               IF  RECE05-YUKOKETA(IDX4)  =  ZERO
                   PERFORM   VARYING   IDW   FROM   200   BY  -1
                           UNTIL   IDW   <   1
                       IF   RECE05-NAME(IDX4)(IDW:1)  NOT =   SPACE
                           COMPUTE RECE05-YUKOKETA(IDX4)  =
                                   IDW     /   2
                           MOVE    1          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF
      *
      *        特定器材（画像診断のフィルム以外）のとき単価を編集
               IF      SRY-SRYCD (IDX2) (1:1)  =   "7"
                   IF   (( SRY-SRYKBN               =   "70" )  AND
                         ( TNS-DATAKBN              =   "3"  ))
                   OR   (  SRY-SRYCD (IDX2)         =
                                    CONS-FILM-YOUJI-KASAN  OR  
                                    CONS-SANSO             OR
                                    CONS-SANSO2               )
                       CONTINUE
                   ELSE
      *********        SET     TBL-SANSO           TO  1
      *                SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
      *                     AT  END
      *                         MOVE    ZERO            TO  FLG-SANSO-OK
      *                     WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
      *                                                          (TBL-SANSO)
      *                         MOVE    1               TO  FLG-SANSO-OK
      *                END-SEARCH
      *********        IF      FLG-SANSO-OK        =   "1"
                     IF      TNS-TEN         =   ZERO
                       CONTINUE
                     ELSE
                       PERFORM 2104412-RECE05-TENSOU-TANKA-HEN-SEC
                       COMPUTE IDW =   RECE05-YUKOKETA(IDX4)  *  2 
                                                   +   3                    
                       MOVE    WRK-CHAR-N (1:WRK-CHAR-N-CNT)
                                                   TO
                                               RECE05-NAME(IDX4)(IDW:)   
                       COMPUTE RECE05-YUKOKETA(IDX4)
                                               =   RECE05-YUKOKETA(IDX4)
                                                   +   1   
                                                   +
                                                  (WRK-CHAR-N-CNT  /  2)
                     END-IF
                   END-IF
               END-IF        
      *----------------------------------------------------------------
      *        在宅部分のレセプト編集用に往診（特別往診）・夜間・
      *        深夜・緊急の剤判定を追加
      *----------------------------------------------------------------
               IF    SRY-SRYKBN     =  "14"
                 EVALUATE    SRY-SRYCD(IDX2)
      *往診（特別往診）
                   WHEN    "114000110"
                   WHEN    "114001610"
                   WHEN    "114000970"
                   IF  FLG-OSINKBN        NOT = 2  AND 3
                       MOVE     1         TO    FLG-OSINKBN
                   END-IF
      *夜間
                   WHEN    "114000470"
                   WHEN    "114001970"
                       MOVE     2         TO    FLG-OSINKBN
      *深夜・緊急
                   WHEN    "114000370"
                   WHEN    "114000570"
                   WHEN    "114001870"
                   WHEN    "114002070"
                       MOVE     3         TO    FLG-OSINKBN
                 END-EVALUATE
               END-IF
              END-IF
             END-IF  
           END-PERFORM
      *
           .
       210441-RECE05-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントフラグ編集処理
      *****************************************************************
       2104411-RECE05-TENSOU-COMCNT-HEN-SEC               SECTION.
      *
           IF      RECE04-SRYKBN      =     "11"
      *       初診料算定科：
              IF   SRY-SRYCD(IDX2)    =     "830000020"
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-SYOSANTEIKA-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "12"
      *       再診料算定科：
              IF   SRY-SRYCD(IDX2)    =     "830000021"
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-SAISANTEIKA-COMMENT
              END-IF
      *       電話再診
              IF   SRY-SRYCD(IDX2)    =     "112009750"   OR
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
                                            "112706550"     
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-DENWASAI-COMMENT
              END-IF
      *       同日再診
              IF   SRY-SRYCD(IDX2)    =     "112010150"   OR
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
                                            "112705850"   
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-DOUJITUSAI-COMMENT
              END-IF
      *       再診（４回目以降／人工腎臓）
              IF   SRY-SRYCD(IDX2)    =     "112009510"   OR
                                            "112009610"   OR
                                            "112012610"   OR
                                            "112704010"   OR
                                            "112706610"   OR
                                            "112010450"   OR
                                            "112010550"   OR
                                            "112704750"   OR
                                            "112007710"   OR
                                            "112007810"   OR
                                            "112012310"   OR
                                            "112702610"   OR
                                            "112706310"   OR
                                            "112008650"   OR
                                            "112008750"   OR
                                            "112703350"     
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-SAIJINKO-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "40"
      *       人工腎臓
              IF   SRY-SRYCD(IDX2)    =     "140036710"   OR
                                            "140007710"
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-JINKOJIN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "80"
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
                        MOVE   "1"    TO    RECE02-COMCNTFLG
                        MOVE   "1"    TO    RECE04-RIHABIRI-COMMENT
              END-IF
           END-IF
      *
      *
           IF      RECE04-SRYKBN      =     "13"
              IF   SRY-SRYCD(IDX2)    =     CONS-MANSEI
                   MOVE   "1"   TO    RECE04-FLG-SHIDOU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SYOBYO-COMMENT
                   MOVE   "1"   TO    RECE04-FLG-SYOBYO-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-RYOYO-COMMENT
                   MOVE   "1"   TO    RECE04-FLG-RYOYO-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI-COMMENT
              AND  FLG-HOUSIJI-COM    =     ZERO
                   MOVE   "1"   TO    RECE04-FLG-HOUSIJI-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-HOUSIJI2-COMMENT
                   MOVE  SPACE  TO    RECE04-FLG-HOUSIJI-COMMENT
                   MOVE    1    TO    FLG-HOUSIJI-COM
              END-IF
           END-IF
           IF      RECE04-SRYKBN      =     "14"
              IF   SRY-SRYCD(IDX2)    =     CONS-HINKAI-COMMENT
                   MOVE   "1"   TO    RECE04-FLG-HINKAI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "33"
              IF   SRY-SRYSYUKBN      =     "334"
              AND  FLG-HOUTEN-COM     =     ZERO
                   MOVE   "1"   TO    RECE04-FLG-HOUTEN-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "50"
              IF   RECE04-SYUTEN1     =     ZERO
                   MOVE   "1"   TO    RECE04-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYSYUKBN      =     "510"
                   MOVE   "1"   TO    RECE04-FLG-SHUJUTU-COMMENT
              END-IF
              IF   SRY-SRYCD(IDX2)    =     CONS-SHUJUTU-COMMENT
                   MOVE   "1"   TO    RECE04-FLG-SHUJUTU-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN      =     "70"
              IF   SRY-SRYCD(IDX2)    =     CONS-CTMRI-COMMENT1  OR
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
                                            CONS-CTMRI-COMMENT17
                   MOVE   "1"   TO    RECE04-FLG-CTMRI-COMMENT
              END-IF
           END-IF
      *
           IF      RECE04-SRYKBN   =  "14" OR "40" OR "50" OR "54"
                                           OR "60" OR "70" OR "80"
              SET     TBL-SANSO    TO      1
              SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    SRY-SRYCD (IDX2)    =   TBL-SANSO-SRYCD
                                                        (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
              END-SEARCH
              IF      FLG-SANSO-OK        =    1
                   MOVE   "1"   TO    RECE04-FLG-SANSO-COMMENT
              END-IF
           END-IF
      *
           .
       2104411-RECE05-TENSOU-COMCNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の単価編集処理
      *****************************************************************
       2104412-RECE05-TENSOU-TANKA-HEN-SEC     SECTION.
      *
      ***  IF      TNS-TEN         =   ZERO
      ***      GO  TO  2104412-RECE05-TENSOU-TANKA-HEN-EXT  
      ***  END-IF
      *
           MOVE    SPACE           TO  WRK-TANKA-X
      * 
      *    数字表示用編集（半角）
           MOVE    TNS-TEN         TO  WRK-TANKA
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
       2104412-RECE05-TENSOU-TANKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）
      *****************************************************************
       210442-RECE02-HENSYU-SEC               SECTION.
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
      *
      *
      *    公費の分離表示用インデックスの確定
           MOVE          ZERO       TO     BUN-IDX
           IF    (RECE02-HKNNUM          NOT =   SPACE)   OR
                 (RECE02-RJNHKNNUM       NOT =   SPACE)
               MOVE      1          TO     BUN-IDX
               IF   FLG-SHOSAISIN        =       1                      
                    PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1  
                               UNTIL    ACCT-IDX   >      31            
                      IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO          
                          MOVE    "1"       TO                          
                                        TBL-JNISSU-DAY(1 ACCT-IDX)      
                      END-IF                                            
                    END-PERFORM                                         
               END-IF                                                   
           END-IF
      *    インデックス２
           MOVE          ZERO       TO     BUN-IDX2
           MOVE          IDX1       TO     HC-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
              UNTIL   (IDX2      >       4)
                     IF    RECE02-KOHNUMHC(HC-IDX IDX2) NOT =  SPACE
                      IF   RECE02-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE02-KOHNUM(1)
                      AND  RECE02-KOHIDHC(HC-IDX IDX2)  =
                                                   RECE02-KOHID(1)
                           MOVE      2       TO     BUN-IDX2
                           IF   FLG-SHOSAISIN      =    1               
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1     
                                    UNTIL    ACCT-IDX   >   31          
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO   
                                    IF     FLG-FUKUSHI  =   1
                                    AND    RECE02-KOHNUM(1)(1:1)
                                             >=  "1" AND <=  "8"
                                       CONTINUE
                                    ELSE
                                       MOVE  "1"  TO
                                            TBL-JNISSU-DAY(2 ACCT-IDX)
                                    END-IF
                                 END-IF                                 
                               END-PERFORM                 
                           END-IF                                       
                      END-IF
                     END-IF
           END-PERFORM
      *    インデックス３
           MOVE          ZERO       TO     BUN-IDX3
           MOVE          IDX1       TO     HC-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
              UNTIL   (IDX2      >       4)
                     IF    RECE02-KOHNUMHC(HC-IDX IDX2) NOT =  SPACE
                      IF   RECE02-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE02-KOHNUM(2)
                      AND  RECE02-KOHIDHC(HC-IDX IDX2)  =
                                                   RECE02-KOHID(2)
                           MOVE      3       TO     BUN-IDX3
                           IF   FLG-SHOSAISIN      =    1               
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1     
                                    UNTIL    ACCT-IDX   >   31          
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO   
                                    IF     FLG-FUKUSHI  =   1
                                    AND    RECE02-KOHNUM(2)(1:1)
                                             >=  "1" AND <=  "8"
                                       CONTINUE
                                    ELSE
                                       MOVE  "1"  TO
                                            TBL-JNISSU-DAY(3 ACCT-IDX)
                                    END-IF
                                 END-IF                                 
                               END-PERFORM                              
                           END-IF                                       
                      END-IF
                     END-IF
           END-PERFORM
      *    インデックス４
           MOVE          ZERO       TO     BUN-IDX4
           MOVE          IDX1       TO     HC-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
              UNTIL   (IDX2      >       4)
                     IF    RECE02-KOHNUMHC(HC-IDX IDX2) NOT =  SPACE
                      IF   RECE02-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE02-KOHNUM(3)
                      AND  RECE02-KOHIDHC(HC-IDX IDX2)  =
                                                   RECE02-KOHID(3)
                            MOVE      4       TO     BUN-IDX4
                           IF   FLG-SHOSAISIN      =    1               
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1     
                                    UNTIL    ACCT-IDX   >   31          
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO   
                                    IF     FLG-FUKUSHI  =   1
                                    AND    RECE02-KOHNUM(3)(1:1)
                                             >=  "1" AND <=  "8"
                                       CONTINUE
                                    ELSE
                                       MOVE  "1"  TO
                                            TBL-JNISSU-DAY(4 ACCT-IDX)
                                    END-IF
                                 END-IF                                 
                               END-PERFORM                              
                           END-IF                                       
                      END-IF
                     END-IF
           END-PERFORM
      *    インデックス５
           MOVE          ZERO       TO     BUN-IDX5
           MOVE          IDX1       TO     HC-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
              UNTIL   (IDX2      >       4)
                     IF    RECE02-KOHNUMHC(HC-IDX IDX2) NOT =  SPACE
                      IF   RECE02-KOHNUMHC(HC-IDX IDX2) =
                                                   RECE02-KOHNUM(4)
                      AND  RECE02-KOHIDHC(HC-IDX IDX2)  =
                                                   RECE02-KOHID(4)
                            MOVE      5       TO     BUN-IDX5
                           IF   FLG-SHOSAISIN      =    1               
                               PERFORM VARYING ACCT-IDX FROM 1 BY 1     
                                    UNTIL    ACCT-IDX   >   31          
                                 IF  ACCT-DAY(1 ACCT-IDX) NOT =  ZERO   
                                    IF     FLG-FUKUSHI  =   1
                                    AND    RECE02-KOHNUM(4)(1:1)
                                             >=  "1" AND <=  "8"
                                       CONTINUE
                                    ELSE
                                       MOVE  "1"  TO
                                            TBL-JNISSU-DAY(5 ACCT-IDX)
                                    END-IF
                                 END-IF                                 
                               END-PERFORM                              
                           END-IF                                       
                      END-IF
                     END-IF
           END-PERFORM
      *
           PERFORM   VARYING  IDX3   FROM   1   BY   1
                     UNTIL    (IDX3         >     5)        OR
                              (SRY-SRYCD(IDX3)   =   SPACE)
               IF    SRY-RENNUM     =     1
                   IF  IDX3         =     1
                       MOVE  ACCT-ZAIKAISU     TO   WK-ZAIKAISU
                       MOVE  ACCT-ZAITEN       TO   WK-ZAITEN
                       IF    SRY-SRYKBN     =  "40"  OR  "50"  OR
                                               "60"  OR  "70"  OR
                                               "80"  OR  "54"
                                MOVE   ACCT-SYUTEN1   TO   WK-SYUTEN
                                COMPUTE   WK-YKZTEN   =
                                                      ACCT-KIZAITEN  +
                                                      ACCT-YKZTEN
                       END-IF
                       IF    SRY-SRYKBN     =  "14"
                           MOVE    ACCT-SYUTEN1   TO   WK-SYUTEN
                           MOVE    ACCT-KIZAITEN  TO   WK-KIZAITEN
                           MOVE    ACCT-YKZTEN    TO   WK-YKZTEN
                       END-IF
                   ELSE
                       IF   SRY-SRYKBN   =  "21"  OR  "22"  OR  "23" OR
                                            "31"  OR  "32"  OR  "33" OR
                                            "40"  OR  "50"  OR  "54" OR
                                            "60"  OR  "70"  OR  "80" OR
                                            "13"  OR  "14"
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
               IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                    (ACCT-ZAIKAISU  NOT =    ZERO)
                     PERFORM   2104421-RECE-LEFT-SET-SEC
               END-IF
               IF    SRY-SRYKBN   =   "21"   OR   "22"   OR   "23"   OR
                                      "31"   OR   "32"   OR   "33"   OR
                                      "40"   OR   "50"   OR   "54"   OR
                                      "60"   OR   "70"   OR
                                      "13"   OR   "14"
                     MOVE    6      TO    IDX3
               END-IF
           END-PERFORM
      *
      *    処方の算定回数から特定疾患処方管理加算の減算
           COMPUTE    RECE02-KAISU(19)   =    RECE02-KAISU(19)  -
                                              WRK-TOKUTEI-SKAN
      *
      *    処方の算定回数から処方(乳幼児)加算の減算
           COMPUTE    RECE02-KAISU(19)   =    RECE02-KAISU(19)  -
                                              WRK-NYUYOUJI-SKAN           
      *
      *    処方の算定回数から長期投薬加算（処方料)の減算
           COMPUTE    RECE02-KAISU(19)   =    RECE02-KAISU(19)  -
                                              WRK-CYOKI-SKAN     
      *
      *    処方の算定回数から特定疾患処方管理加算(処方料)の減算
           COMPUTE    RECE02-KAISU(33)   =    RECE02-KAISU(33)  -
                                              WRK-TOKUTEI-SKAN2           
      *
      *    処方せんの算定回数から処方せん(乳幼児)加算の減算
           COMPUTE    RECE02-KAISU(33)   =    RECE02-KAISU(33)  -
                                              WRK-NYUYOUJI-SHOKAN           
      *
      *    処方せんの算定回数から長期投薬加算(処方せん料)の減算
           COMPUTE    RECE02-KAISU(33)   =    RECE02-KAISU(33)  -
                                              WRK-CYOKI-SKAN2    
      *
      *    麻毒の算定回数から処方料（麻・向・毒・覚）の減算
           COMPUTE    RECE02-KAISU(20)   =    RECE02-KAISU(20)  -
                                              WRK-MADOKU-SHOHO           
      *
           .                                           
      *
       210442-RECE02-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）
      *****************************************************************
       2104421-RECE-LEFT-SET-SEC               SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *    初診のレセ点数欄の編集
               WHEN    "11"
                   IF   WRK-TNS-CHUKSNTUBAN(IDX3)   =   "0"
                        ADD   WK-ZAIKAISU  TO   RECE02-KAISU(1)
                   END-IF
      *
      ***          IF   WRK-TNS-DATAKBN(IDX3)  =   2
      ***            EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
      ***                WHEN    1
      ***                WHEN    5
      ***                    MOVE  "1"     TO   RECE02-SHOSHINTIME
      ***                WHEN    4
      ***                    MOVE  "2"     TO   RECE02-SHOSHINTIME
      ***                WHEN    3
      ***                    MOVE  "3"     TO   RECE02-SHOSHINTIME
      ***            END-EVALUATE
      ***          END-IF
      *
      *************１：時間外　２：深夜　３：休日　４：時間外・深夜
      *************５：時間外・休日　６：休日・深夜
      *************７：時間外・休日・深夜
                   IF   WRK-TNS-DATAKBN(IDX3)  =   2
                     EVALUATE    WRK-TNS-TIMEKSNKBN(IDX3)
                         WHEN    1
                         WHEN    5
                             EVALUATE   RECE02-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "1"     TO   RECE02-SHOSHINTIME
                               WHEN   "1"
                                  CONTINUE
                               WHEN   "2"
                                  MOVE  "4"     TO   RECE02-SHOSHINTIME
                               WHEN   "3"
                                  MOVE  "5"     TO   RECE02-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  MOVE  "7"     TO   RECE02-SHOSHINTIME
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    4
                             EVALUATE   RECE02-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "2"     TO   RECE02-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "4"     TO   RECE02-SHOSHINTIME
                               WHEN   "2"
                                  CONTINUE
                               WHEN   "3"
                                  MOVE  "6"     TO   RECE02-SHOSHINTIME
                               WHEN   "4"
                                  CONTINUE
                               WHEN   "5"
                                  MOVE  "7"     TO   RECE02-SHOSHINTIME
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                         WHEN    3
                             EVALUATE   RECE02-SHOSHINTIME
                               WHEN   SPACE
                                  MOVE  "3"     TO   RECE02-SHOSHINTIME
                               WHEN   "1"
                                  MOVE  "5"     TO   RECE02-SHOSHINTIME
                               WHEN   "2"
                                  MOVE  "6"     TO   RECE02-SHOSHINTIME
                               WHEN   "3"
                                  CONTINUE
                               WHEN   "4"
                                  MOVE  "7"     TO   RECE02-SHOSHINTIME
                               WHEN   "5"
                                  CONTINUE
                               WHEN   "6"
                                  CONTINUE
                               WHEN   "7"
                                  CONTINUE
                             END-EVALUATE
                     END-EVALUATE
                   END-IF
      *
                   IF    BUN-IDX        =   1
                       COMPUTE    RECE02-TENSU(1 BUN-IDX)  =
                                  RECE02-TENSU(1 BUN-IDX)  +
                       (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE    RECE02-TENSU(1 BUN-IDX2)  =
                                  RECE02-TENSU(1 BUN-IDX2)  +
                       (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE    RECE02-TENSU(1 BUN-IDX3)  =
                                  RECE02-TENSU(1 BUN-IDX3)  +
                       (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE    RECE02-TENSU(1 BUN-IDX4)  =
                                  RECE02-TENSU(1 BUN-IDX4)  +
                       (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
      *    再診のレセ点数欄の編集（集計方法変更後）
               WHEN    "12"
                 EVALUATE  WRK-TNS-GAITENTTLKBN(IDX3)
      *      （基本手技）
                   WHEN  120
                     IF   RECE02-SRYYM  < "200306"
                       IF ( SRY-SRYCD(IDX3)      =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                       OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT =  "0"  )
                           CONTINUE
                       ELSE
                           ADD   WK-ZAIKAISU    TO   RECE02-KAISU(2)
                       END-IF
                       IF    RECE02-KHNTEN(2)   =   ZERO
                         MOVE  WRK-TNS-TEN(IDX3)  TO   RECE02-KHNTEN(2)
                       ELSE
                         IF   RECE02-KHNTEN(2)  NOT =  WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO   RECE02-KHNTEN(2)
                               MOVE   1       TO   FLG-KHNTEN2
                         END-IF
                       END-IF
                       IF    BUN-IDX        =   1
                            COMPUTE    RECE02-TENSU(2 BUN-IDX)  =
                                       RECE02-TENSU(2 BUN-IDX)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                            COMPUTE    RECE02-TENSU(2 BUN-IDX2)  =
                                       RECE02-TENSU(2 BUN-IDX2)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                            COMPUTE    RECE02-TENSU(2 BUN-IDX3)  =
                                       RECE02-TENSU(2 BUN-IDX3)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                            COMPUTE    RECE02-TENSU(2 BUN-IDX4)  =
                                       RECE02-TENSU(2 BUN-IDX4)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                     ELSE
                       IF ( SRY-SRYCD(IDX3)      =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                       OR ( WRK-TNS-CHUKSNTUBAN(IDX3)  NOT = "0"   )
                           CONTINUE
                       ELSE
                           ADD   WK-ZAIKAISU    TO   RECE02-KAISU(2)
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI-JIKAN
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-NYUYOJI-JIKAN
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI-JIKAN
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-YOJI-JIKAN
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI
                            MOVE     1       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI-JIKAN
                            MOVE     2       TO    FLG-NYUYOJI
                       END-IF
                       IF   FLG-NYUYOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-GNYUYOJI-JIKAN
                            MOVE     9       TO    FLG-NYUYOJI
                       END-IF
      *
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI
                            MOVE     1       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   2
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI
                            MOVE     9       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   ZERO
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI-JIKAN
                            MOVE     2       TO    FLG-YOJI
                       END-IF
                       IF   FLG-YOJI          =   1
                       AND  SRY-SRYCD(IDX3)      =   CONS-GYOJI-JIKAN
                            MOVE     9       TO    FLG-YOJI
                       END-IF
      **
                       IF  SRY-SRYCD(IDX3)  =    CONS-NYUYOJI        OR
                                                 CONS-NYUYOJI-JIKAN  OR
                                                 CONS-GNYUYOJI       OR
                                                 CONS-GNYUYOJI-JIKAN 
                            COMPUTE  CNT-NYUYOJI =   CNT-NYUYOJI
                                                 +   WK-ZAIKAISU
                       END-IF
                       IF  SRY-SRYCD(IDX3)  =    CONS-YOJI           OR
                                                 CONS-YOJI-JIKAN     OR
                                                 CONS-GYOJI          OR
                                                 CONS-GYOJI-JIKAN    
                            COMPUTE  CNT-YOJI    =   CNT-YOJI
                                                 +   WK-ZAIKAISU
                       END-IF
      **
                       IF ( SRY-SRYCD(IDX3)      =   CONS-KEIZOKU1  OR
                                                     CONS-KEIZOKU2 )
                       OR ( FLG-NYUYOJI          =   9             )
                       OR ( FLG-YOJI             =   9             )
                            MOVE   ZERO       TO    RECE02-KHNTEN(2)
                            MOVE   1          TO    FLG-KHNTEN2
                       ELSE
                         IF  SRY-SRYCD(IDX3)  =  CONS-NYUYOJI        OR
                                                 CONS-NYUYOJI-JIKAN  OR
                                                 CONS-YOJI           OR
                                                 CONS-YOJI-JIKAN     OR
                                                 CONS-GNYUYOJI       OR
                                                 CONS-GNYUYOJI-JIKAN OR
                                                 CONS-GYOJI          OR
                                                 CONS-GYOJI-JIKAN    
                            IF   FLG-YOJI-ARI  =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO RECE02-KHNTEN(2)
                              MOVE     1       TO    FLG-YOJI-ARI
                            END-IF
                         ELSE
                            IF   FLG-SAISIN-ARI   =  ZERO
                              ADD  WRK-TNS-TEN(IDX3) TO RECE02-KHNTEN(2)
                              MOVE     1       TO    FLG-SAISIN-ARI
                            END-IF
                         END-IF
                       END-IF
      **
                       IF    BUN-IDX        =   1
                            COMPUTE    RECE02-TENSU(2 BUN-IDX)  =
                                       RECE02-TENSU(2 BUN-IDX)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                            COMPUTE    RECE02-TENSU(2 BUN-IDX2)  =
                                       RECE02-TENSU(2 BUN-IDX2)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                            COMPUTE    RECE02-TENSU(2 BUN-IDX3)  =
                                       RECE02-TENSU(2 BUN-IDX3)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                            COMPUTE    RECE02-TENSU(2 BUN-IDX4)  =
                                       RECE02-TENSU(2 BUN-IDX4)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                     END-IF
      *      （外来管理加算）
                   WHEN  122
                        IF    RECE02-KHNTEN(3)   =   ZERO
                           MOVE  WRK-TNS-TEN(IDX3) TO  RECE02-KHNTEN(3)
                        ELSE
                           IF RECE02-KHNTEN(3) NOT = WRK-TNS-TEN(IDX3)
                              MOVE   ZERO    TO  RECE02-KHNTEN(3)
                              MOVE   1    TO  FLG-KHNTEN3
                           END-IF
                        END-IF
                        ADD    WK-ZAIKAISU   TO   RECE02-KAISU(3)
                        IF    BUN-IDX        =   1
                            COMPUTE    RECE02-TENSU(3 BUN-IDX)  =
                                       RECE02-TENSU(3 BUN-IDX)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX2       =   2
                            COMPUTE    RECE02-TENSU(3 BUN-IDX2)  =
                                       RECE02-TENSU(3 BUN-IDX2)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX3       =   3
                            COMPUTE    RECE02-TENSU(3 BUN-IDX3)  =
                                       RECE02-TENSU(3 BUN-IDX3)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                        END-IF
                        IF    BUN-IDX4       =   4
                            COMPUTE    RECE02-TENSU(3 BUN-IDX4)  =
                                       RECE02-TENSU(3 BUN-IDX4)  +
                            (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                        END-IF
      *      （時間外）
                   WHEN  123
                                  IF    RECE02-KHNTEN(4)   =   ZERO
                                   MOVE  WRK-TNS-TEN(IDX3)    TO
                                                     RECE02-KHNTEN(4)
                                  ELSE
                                   IF RECE02-KHNTEN(4) NOT = 
                                                     WRK-TNS-TEN(IDX3)
                                        MOVE   ZERO    TO
                                                      RECE02-KHNTEN(4)
                                        MOVE   1    TO  FLG-KHNTEN4
                                   END-IF
                                  END-IF
                                  ADD   WK-ZAIKAISU  TO
                                                      RECE02-KAISU(4)
                                  IF    BUN-IDX        =   1
                                     COMPUTE RECE02-TENSU(4 BUN-IDX) =
                                             RECE02-TENSU(4 BUN-IDX) +
                                     (WRK-TNS-TEN(IDX3) 
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX2       =   2
                                     COMPUTE RECE02-TENSU(4 BUN-IDX2) =
                                             RECE02-TENSU(4 BUN-IDX2) +
                                     (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX3       =   3
                                     COMPUTE RECE02-TENSU(4 BUN-IDX3) =
                                             RECE02-TENSU(4 BUN-IDX3) +
                                     (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX4       =   4
                                     COMPUTE RECE02-TENSU(4 BUN-IDX4) =
                                             RECE02-TENSU(4 BUN-IDX4) +
                                     (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
      *      （休日）
                   WHEN  124
                                  IF    RECE02-KHNTEN(5)   =   ZERO
                                   MOVE  WRK-TNS-TEN(IDX3)    TO
                                                     RECE02-KHNTEN(5)
                                  ELSE
                                   IF RECE02-KHNTEN(5) NOT = 
                                                    WRK-TNS-TEN(IDX3)
                                        MOVE   ZERO    TO
                                                      RECE02-KHNTEN(5)
                                        MOVE   1     TO   FLG-KHNTEN5
                                   END-IF
                                  END-IF
                                  ADD   WK-ZAIKAISU   TO
                                                        RECE02-KAISU(5)
                                  IF    BUN-IDX        =   1
                                    COMPUTE  RECE02-TENSU(5 BUN-IDX) =
                                             RECE02-TENSU(5 BUN-IDX) +
                                    (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX2       =   2
                                    COMPUTE  RECE02-TENSU(5 BUN-IDX2) =
                                             RECE02-TENSU(5 BUN-IDX2) +
                                    (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX3       =   3
                                    COMPUTE  RECE02-TENSU(5 BUN-IDX3) =
                                             RECE02-TENSU(5 BUN-IDX3) +
                                    (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX4       =   4
                                    COMPUTE  RECE02-TENSU(5 BUN-IDX4) =
                                             RECE02-TENSU(5 BUN-IDX4) +
                                    (WRK-TNS-TEN(IDX3)   
                                                     *   ACCT-ZAIKAISU)
                                  END-IF
      *      （深夜）
                   WHEN  125
                                  IF    RECE02-KHNTEN(6)   =   ZERO
                                   MOVE  WRK-TNS-TEN(IDX3)    TO
                                                     RECE02-KHNTEN(6)
                                  ELSE
                                   IF RECE02-KHNTEN(6) NOT = 
                                                     WRK-TNS-TEN(IDX3)
                                        MOVE   ZERO    TO
                                                      RECE02-KHNTEN(6)
                                        MOVE   1       TO   FLG-KHNTEN6
                                   END-IF
                                  END-IF
                                  ADD   WK-ZAIKAISU  TO
                                                      RECE02-KAISU(6)
                                  IF    BUN-IDX        =   1
                                    COMPUTE  RECE02-TENSU(6 BUN-IDX) =
                                             RECE02-TENSU(6 BUN-IDX) +
                                    (WRK-TNS-TEN(IDX3)   
                                                    *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX2       =   2
                                    COMPUTE  RECE02-TENSU(6 BUN-IDX2) =
                                             RECE02-TENSU(6 BUN-IDX2) +
                                    (WRK-TNS-TEN(IDX3)   
                                                    *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX3       =   3
                                    COMPUTE  RECE02-TENSU(6 BUN-IDX3) =
                                             RECE02-TENSU(6 BUN-IDX3) +
                                    (WRK-TNS-TEN(IDX3)   
                                                    *   ACCT-ZAIKAISU)
                                  END-IF
                                  IF    BUN-IDX4       =   4
                                    COMPUTE  RECE02-TENSU(6 BUN-IDX4) =
                                             RECE02-TENSU(6 BUN-IDX4) +
                                    (WRK-TNS-TEN(IDX3)   
                                                    *   ACCT-ZAIKAISU)
                                  END-IF
                 END-EVALUATE
      *    指導のレセ点数欄の編集
               WHEN    "13"
                   IF    BUN-IDX        =   1
                       COMPUTE    RECE02-TENSU(7 BUN-IDX)  =
                                  RECE02-TENSU(7 BUN-IDX)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE    RECE02-TENSU(7 BUN-IDX2)  =
                                  RECE02-TENSU(7 BUN-IDX2)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE    RECE02-TENSU(7 BUN-IDX3)  =
                                  RECE02-TENSU(7 BUN-IDX3)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE    RECE02-TENSU(7 BUN-IDX4)  =
                                  RECE02-TENSU(7 BUN-IDX4)  +
                                 (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    在宅のレセ点数欄の編集
               WHEN    "14"
                   MOVE      ZERO            TO   FLG-ZAITAKU
      *            往診・特別往診（時間加算）
                   IF    FLG-OSINKBN         =   1
                       MOVE         1        TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(8)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(8 BUN-IDX)  =
                                     RECE02-TENSU(8 BUN-IDX)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(8 BUN-IDX2)  =
                                     RECE02-TENSU(8 BUN-IDX2)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(8 BUN-IDX3)  =
                                     RECE02-TENSU(8 BUN-IDX3)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(8 BUN-IDX4)  =
                                     RECE02-TENSU(8 BUN-IDX4)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                   END-IF
      *            夜間
                   IF    FLG-OSINKBN         =    2
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(9)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(9 BUN-IDX)  =
                                     RECE02-TENSU(9 BUN-IDX)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(9 BUN-IDX2)  =
                                     RECE02-TENSU(9 BUN-IDX2)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(9 BUN-IDX3)  =
                                     RECE02-TENSU(9 BUN-IDX3)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(9 BUN-IDX4)  =
                                     RECE02-TENSU(9 BUN-IDX4)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                   END-IF
      *            深夜・緊急
                   IF    FLG-OSINKBN         =    3
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(10)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(10 BUN-IDX)  =
                                     RECE02-TENSU(10 BUN-IDX)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(10 BUN-IDX2)  =
                                     RECE02-TENSU(10 BUN-IDX2)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(10 BUN-IDX3)  =
                                     RECE02-TENSU(10 BUN-IDX3)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(10 BUN-IDX4)  =
                                     RECE02-TENSU(10 BUN-IDX4)  +
                                    (WK-SYUTEN   *   WK-ZAIKAISU)
                       END-IF
                   END-IF
      *            在宅患者訪問診療
                   IF  ( WRK-TNS-OSINKBN(IDX3)   =     1          )  AND
                       ((SRY-SRYCD(IDX3)     NOT =    "114000110")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001610")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000470")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001970")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114000370")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001570")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114001870")   AND
                        (SRY-SRYCD(IDX3)     NOT =    "114002070"))
                       MOVE   1              TO   FLG-ZAITAKU
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(11)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(11 BUN-IDX)  =
                                     RECE02-TENSU(11 BUN-IDX)  +
                           (WK-SYUTEN            *  ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(11 BUN-IDX2)  =
                                     RECE02-TENSU(11 BUN-IDX2)  +
                           (WK-SYUTEN            *  ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(11 BUN-IDX3)  =
                                     RECE02-TENSU(11 BUN-IDX3)  +
                           (WK-SYUTEN            *  ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(11 BUN-IDX4)  =
                                     RECE02-TENSU(11 BUN-IDX4)  +
                           (WK-SYUTEN            *  ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *            その他
                   IF    FLG-ZAITAKU    =   ZERO
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(12 BUN-IDX)  =
                                     RECE02-TENSU(12 BUN-IDX)  +
                                    (WK-SYUTEN     *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(12 BUN-IDX2)  =
                                     RECE02-TENSU(12 BUN-IDX2)  +
                                    (WK-SYUTEN     *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(12 BUN-IDX3)  =
                                     RECE02-TENSU(12 BUN-IDX3)  +
                                    (WK-SYUTEN     *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(12 BUN-IDX4)  =
                                     RECE02-TENSU(12 BUN-IDX4)  +
                                    (WK-SYUTEN     *   WK-ZAIKAISU)
                       END-IF
                   END-IF
      *            薬剤
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(13 BUN-IDX)  =
                                 RECE02-TENSU(13 BUN-IDX)  +
                                (WK-KIZAITEN *   WK-ZAIKAISU)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(13 BUN-IDX2)  =
                                 RECE02-TENSU(13 BUN-IDX2)  +
                                (WK-KIZAITEN *   WK-ZAIKAISU)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(13 BUN-IDX3)  =
                                 RECE02-TENSU(13 BUN-IDX3)  +
                                (WK-KIZAITEN *   WK-ZAIKAISU)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(13 BUN-IDX4)  =
                                 RECE02-TENSU(13 BUN-IDX4)  +
                                (WK-KIZAITEN *   WK-ZAIKAISU)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    投薬のレセ点数欄の編集
               WHEN    "21"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                     IF   SRY-SRYCD(IDX3)         =    CONS-YAKTEIGEN
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(14 BUN-IDX)  =
                                     RECE02-TENSU(14 BUN-IDX)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(14 BUN-IDX2)  =
                                     RECE02-TENSU(14 BUN-IDX2)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(14 BUN-IDX3)  =
                                     RECE02-TENSU(14 BUN-IDX3)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(14 BUN-IDX4)  =
                                     RECE02-TENSU(14 BUN-IDX4)  -
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                     ELSE
                       ADD   WK-ZAIKAISU       TO   RECE02-KAISU(14)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(14 BUN-IDX)  =
                                     RECE02-TENSU(14 BUN-IDX)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(14 BUN-IDX2)  =
                                     RECE02-TENSU(14 BUN-IDX2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(14 BUN-IDX3)  =
                                     RECE02-TENSU(14 BUN-IDX3)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(14 BUN-IDX4)  =
                                     RECE02-TENSU(14 BUN-IDX4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                     END-IF
                   ELSE
                     IF   SRY-SRYCD(IDX3)     =    CONS-FUNSAI-COMMENT
                       ADD   WK-ZAIKAISU       TO   RECE02-KAISU(14)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(14 BUN-IDX)  =
                                     RECE02-TENSU(14 BUN-IDX)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(14 BUN-IDX2)  =
                                     RECE02-TENSU(14 BUN-IDX2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(14 BUN-IDX3)  =
                                     RECE02-TENSU(14 BUN-IDX3)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(14 BUN-IDX4)  =
                                     RECE02-TENSU(14 BUN-IDX4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                     ELSE   
                       IF    RECE02-KHNTEN(15)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO RECE02-KHNTEN(15)
                       ELSE
                          IF   RECE02-KHNTEN(15) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE02-KHNTEN(15)
                               MOVE   1       TO    FLG-KHNTEN15
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(15)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(15 BUN-IDX)  =
                                     RECE02-TENSU(15 BUN-IDX)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(15 BUN-IDX2)  =
                                     RECE02-TENSU(15 BUN-IDX2)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(15 BUN-IDX3)  =
                                     RECE02-TENSU(15 BUN-IDX3)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(15 BUN-IDX4)  =
                                     RECE02-TENSU(15 BUN-IDX4)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                     END-IF   
                   END-IF
               WHEN    "22"
                   ADD    WK-ZAIKAISU    TO   RECE02-KAISU(16)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(16 BUN-IDX)  =
                                 RECE02-TENSU(16 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(16 BUN-IDX2)  =
                                 RECE02-TENSU(16 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(16 BUN-IDX3)  =
                                 RECE02-TENSU(16 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(16 BUN-IDX4)  =
                                 RECE02-TENSU(16 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
               WHEN    "23"
                   IF    (SRY-SRYCD(IDX3) (1:1)  = "6" OR "S" OR "8"
                                                       OR "7"       )
                   OR    (SRY-SRYCD(IDX3) (1:3)  = "001" OR "008"   
                                                         OR "059"   )
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(17)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(17 BUN-IDX)  =
                                     RECE02-TENSU(17 BUN-IDX)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(17 BUN-IDX2)  =
                                     RECE02-TENSU(17 BUN-IDX2)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(17 BUN-IDX3)  =
                                     RECE02-TENSU(17 BUN-IDX3)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(17 BUN-IDX4)  =
                                     RECE02-TENSU(17 BUN-IDX4)  +
                                    (WK-ZAITEN   *   WK-ZAIKAISU)
                       END-IF
                   ELSE
                       IF    RECE02-KHNTEN(18)   =   ZERO
                          MOVE  WRK-TNS-TEN(IDX3) TO  RECE02-KHNTEN(18)
                       ELSE
                          IF   RECE02-KHNTEN(18) NOT = WRK-TNS-TEN(IDX3)
                               MOVE   ZERO    TO    RECE02-KHNTEN(18)
                               MOVE   1       TO    FLG-KHNTEN18
                          END-IF
                       END-IF
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(18)
                       IF    BUN-IDX        =   1
                           COMPUTE   RECE02-TENSU(18 BUN-IDX)  =
                                     RECE02-TENSU(18 BUN-IDX)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX2       =   2
                           COMPUTE   RECE02-TENSU(18 BUN-IDX2)  =
                                     RECE02-TENSU(18 BUN-IDX2)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX3       =   3
                           COMPUTE   RECE02-TENSU(18 BUN-IDX3)  =
                                     RECE02-TENSU(18 BUN-IDX3)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                       IF    BUN-IDX4       =   4
                           COMPUTE   RECE02-TENSU(18 BUN-IDX4)  =
                                     RECE02-TENSU(18 BUN-IDX4)  +
                          (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       END-IF
                   END-IF
      *    処方のレセ点数欄の編集
               WHEN    "25"
                   IF SRY-SRYCD(IDX3)      =    "120002270"
                      COMPUTE  WRK-TOKUTEI-SKAN  =  WRK-TOKUTEI-SKAN +
                                                    WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120002170"
                      COMPUTE  WRK-NYUYOUJI-SKAN  =  WRK-NYUYOUJI-SKAN +
                                                    WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    "120003170"             
                      COMPUTE  WRK-CYOKI-SKAN  =  WRK-CYOKI-SKAN +      
                                                  WK-ZAIKAISU           
                   END-IF                                               
                   IF    RECE02-KHNTEN(19)   =   ZERO
                      MOVE  WRK-TNS-TEN(IDX3) TO  RECE02-KHNTEN(19)
                   ELSE
                      IF   RECE02-KHNTEN(19) NOT = WRK-TNS-TEN(IDX3)
                           MOVE   ZERO    TO    RECE02-KHNTEN(19)
                           MOVE   1       TO    FLG-KHNTEN19
                      END-IF
                   END-IF
                   ADD    WK-ZAIKAISU     TO    RECE02-KAISU(19)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(19 BUN-IDX)  =
                                 RECE02-TENSU(19 BUN-IDX)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(19 BUN-IDX2)  =
                                 RECE02-TENSU(19 BUN-IDX2)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(19 BUN-IDX3)  =
                                 RECE02-TENSU(19 BUN-IDX3)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(19 BUN-IDX4)  =
                                 RECE02-TENSU(19 BUN-IDX4)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
      *    麻毒のレセ点数欄の編集
               WHEN    "26"
                   ADD    WK-ZAIKAISU     TO   RECE02-KAISU(20)
                   IF SRY-SRYCD(IDX3)      =    "120001310"
                    COMPUTE WRK-MADOKU-SHOHO = WRK-MADOKU-SHOHO +
                                               WK-ZAIKAISU 
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(20 BUN-IDX)  =
                                 RECE02-TENSU(20 BUN-IDX)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(20 BUN-IDX2)  =
                                 RECE02-TENSU(20 BUN-IDX2)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(20 BUN-IDX3)  =
                                 RECE02-TENSU(20 BUN-IDX3)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(20 BUN-IDX4)  =
                                 RECE02-TENSU(20 BUN-IDX4)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
      *    調基のレセ点数欄の編集
               WHEN    "27"
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(21 BUN-IDX)  =
                                 RECE02-TENSU(21 BUN-IDX)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(21 BUN-IDX2)  =
                                 RECE02-TENSU(21 BUN-IDX2)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(21 BUN-IDX3)  =
                                 RECE02-TENSU(21 BUN-IDX3)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(21 BUN-IDX4)  =
                                 RECE02-TENSU(21 BUN-IDX4)  +
                      (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                   END-IF
      *    皮下筋肉内注射のレセ点数欄の編集
               WHEN    "31"
                   ADD    WK-ZAIKAISU     TO   RECE02-KAISU(22)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(22 BUN-IDX)  =
                                 RECE02-TENSU(22 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(22 BUN-IDX2)  =
                                 RECE02-TENSU(22 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(22 BUN-IDX3)  =
                                 RECE02-TENSU(22 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(22 BUN-IDX4)  =
                                 RECE02-TENSU(22 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    静脈内注射のレセ点数欄の編集
               WHEN    "32"
                   ADD    WK-ZAIKAISU     TO   RECE02-KAISU(23)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(23 BUN-IDX)  =
                                 RECE02-TENSU(23 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(23 BUN-IDX2)  =
                                 RECE02-TENSU(23 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(23 BUN-IDX3)  =
                                 RECE02-TENSU(23 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(23 BUN-IDX4)  =
                                 RECE02-TENSU(23 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    その他注射のレセ点数欄の編集
               WHEN    "33"
                   ADD    WK-ZAIKAISU     TO   RECE02-KAISU(24)
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(24 BUN-IDX)  =
                                 RECE02-TENSU(24 BUN-IDX)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(24 BUN-IDX2)  =
                                 RECE02-TENSU(24 BUN-IDX2)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(24 BUN-IDX3)  =
                                 RECE02-TENSU(24 BUN-IDX3)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(24 BUN-IDX4)  =
                                 RECE02-TENSU(24 BUN-IDX4)  +
                                (WK-ZAITEN   *   WK-ZAIKAISU)
                   END-IF
      *    処置のレセ点数欄の編集
               WHEN    "40"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(25)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(25 BUN-IDX)  =
                                 RECE02-TENSU(25 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(25 BUN-IDX2)  =
                                 RECE02-TENSU(25 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(25 BUN-IDX3)  =
                                 RECE02-TENSU(25 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(25 BUN-IDX4)  =
                                 RECE02-TENSU(25 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(26 BUN-IDX)  =
                                 RECE02-TENSU(26 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(26 BUN-IDX2)  =
                                 RECE02-TENSU(26 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(26 BUN-IDX3)  =
                                 RECE02-TENSU(26 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(26 BUN-IDX4)  =
                                 RECE02-TENSU(26 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    手術のレセ点数欄の編集
               WHEN    "50"
               WHEN    "54"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(27)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(27 BUN-IDX)  =
                                 RECE02-TENSU(27 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(27 BUN-IDX2)  =
                                 RECE02-TENSU(27 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(27 BUN-IDX3)  =
                                 RECE02-TENSU(27 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(27 BUN-IDX4)  =
                                 RECE02-TENSU(27 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(28 BUN-IDX)  =
                                 RECE02-TENSU(28 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(28 BUN-IDX2)  =
                                 RECE02-TENSU(28 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(28 BUN-IDX3)  =
                                 RECE02-TENSU(28 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(28 BUN-IDX4)  =
                                 RECE02-TENSU(28 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    検査のレセ点数欄の編集
               WHEN    "60"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(29)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(29 BUN-IDX)  =
                                 RECE02-TENSU(29 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(29 BUN-IDX2)  =
                                 RECE02-TENSU(29 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(29 BUN-IDX3)  =
                                 RECE02-TENSU(29 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(29 BUN-IDX4)  =
                                 RECE02-TENSU(29 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    WRK-TNS-KNSJISKBN(IDX3)    =   2
                         COMPUTE   RECE02-KAISU(29)  =
                                   RECE02-KAISU(29)  -  
                                   WK-ZAIKAISU
                   END-IF
                   IF    WRK-TNS-SRYCD(IDX3)  =   CONS-SAIKETSU1  OR
                                                  CONS-SAIKETSU2  OR
                                                  CONS-SAIKETSU3
                         IF   RECE02-KAISU(29)  NOT =  ZERO
                              COMPUTE   RECE02-KAISU(29)  =
                                        RECE02-KAISU(29)  -   
                                        WK-ZAIKAISU
                         END-IF
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(30 BUN-IDX)  =
                                 RECE02-TENSU(30 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(30 BUN-IDX2)  =
                                 RECE02-TENSU(30 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(30 BUN-IDX3)  =
                                 RECE02-TENSU(30 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(30 BUN-IDX4)  =
                                 RECE02-TENSU(30 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    画像診断のレセ点数欄の編集
               WHEN    "70"
                   IF    WK-SYUTEN       NOT =   ZERO
                       ADD    WK-ZAIKAISU    TO   RECE02-KAISU(31)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(31 BUN-IDX)  =
                                 RECE02-TENSU(31 BUN-IDX)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(31 BUN-IDX2)  =
                                 RECE02-TENSU(31 BUN-IDX2)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(31 BUN-IDX3)  =
                                 RECE02-TENSU(31 BUN-IDX3)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(31 BUN-IDX4)  =
                                 RECE02-TENSU(31 BUN-IDX4)  +
                                (WK-SYUTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(32 BUN-IDX)  =
                                 RECE02-TENSU(32 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(32 BUN-IDX2)  =
                                 RECE02-TENSU(32 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(32 BUN-IDX3)  =
                                 RECE02-TENSU(32 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(32 BUN-IDX4)  =
                                 RECE02-TENSU(32 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
      *    その他のレセ点数欄の編集
               WHEN    "80"
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN2
                      COMPUTE  WRK-TOKUTEI-SKAN2 =  WRK-TOKUTEI-SKAN2 +
                                                    WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN1
                    COMPUTE WRK-NYUYOUJI-SHOKAN = WRK-NYUYOUJI-SHOKAN +
                                                  WK-ZAIKAISU 
                   END-IF
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOKSN3          
                      COMPUTE  WRK-CYOKI-SKAN2  =   WRK-CYOKI-SKAN2   + 
                                                    WK-ZAIKAISU         
                   END-IF                                               
                   IF SRY-SRYCD(IDX3)      =    CONS-SHOHOSEN1 OR
                                                CONS-SHOHOSEN2 OR
                                                CONS-SHOHOSEN3 OR
                                                CONS-SHOHOSEN4 OR
                                                CONS-SHOHOKSN1 OR
                                                CONS-SHOHOKSN2 OR
                                                CONS-SHOHOKSN3          
                           ADD    WK-ZAIKAISU  TO  RECE02-KAISU(33)
                   END-IF
                   IF    BUN-IDX        =   1
                       IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                                 CONS-SHOHOSEN2 OR
                                                 CONS-SHOHOSEN3 OR
                                                 CONS-SHOHOSEN4 OR
                                                 CONS-SHOHOKSN1 OR
                                                 CONS-SHOHOKSN2 OR
                                                 CONS-SHOHOKSN3         
                           COMPUTE   RECE02-TENSU(33 BUN-IDX)  =
                                     RECE02-TENSU(33 BUN-IDX)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE02-TENSU(7 BUN-IDX)  =
                                     RECE02-TENSU(7 BUN-IDX)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE02-TENSU(34 BUN-IDX)  =
                                     RECE02-TENSU(34 BUN-IDX)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                       END-IF
                   END-IF
                   IF    BUN-IDX2       =   2
                       IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                                 CONS-SHOHOSEN2 OR
                                                 CONS-SHOHOSEN3 OR
                                                 CONS-SHOHOSEN4 OR
                                                 CONS-SHOHOKSN1 OR
                                                 CONS-SHOHOKSN2 OR
                                                 CONS-SHOHOKSN3         
                           COMPUTE   RECE02-TENSU(33 BUN-IDX2)  =
                                     RECE02-TENSU(33 BUN-IDX2)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE02-TENSU(7 BUN-IDX2)  =
                                     RECE02-TENSU(7 BUN-IDX2)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE02-TENSU(34 BUN-IDX2)  =
                                     RECE02-TENSU(34 BUN-IDX2)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                       END-IF
                   END-IF
                   IF    BUN-IDX3       =   3
                       IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                                 CONS-SHOHOSEN2 OR
                                                 CONS-SHOHOSEN3 OR
                                                 CONS-SHOHOSEN4 OR
                                                 CONS-SHOHOKSN1 OR
                                                 CONS-SHOHOKSN2 OR
                                                 CONS-SHOHOKSN3         
                           COMPUTE   RECE02-TENSU(33 BUN-IDX3)  =
                                     RECE02-TENSU(33 BUN-IDX3)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE02-TENSU(7 BUN-IDX3)  =
                                     RECE02-TENSU(7 BUN-IDX3)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                         ELSE  
                           COMPUTE   RECE02-TENSU(34 BUN-IDX3)  =
                                     RECE02-TENSU(34 BUN-IDX3)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                       END-IF
                   END-IF
                   IF    BUN-IDX4       =   4
                       IF   SRY-SRYCD(IDX3)   =  CONS-SHOHOSEN1 OR
                                                 CONS-SHOHOSEN2 OR
                                                 CONS-SHOHOSEN3 OR
                                                 CONS-SHOHOSEN4 OR
                                                 CONS-SHOHOKSN1 OR
                                                 CONS-SHOHOKSN2 OR
                                                 CONS-SHOHOKSN3         
                           COMPUTE   RECE02-TENSU(33 BUN-IDX4)  =
                                     RECE02-TENSU(33 BUN-IDX4)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                       ELSE
                         IF    WRK-TNS-GAITENTTLKBN(IDX3)   =    130
                           COMPUTE   RECE02-TENSU(7 BUN-IDX4)  =
                                     RECE02-TENSU(7 BUN-IDX4)  +
                           (WRK-TNS-TEN(IDX3)   *   ACCT-ZAIKAISU)
                         ELSE
                           COMPUTE   RECE02-TENSU(34 BUN-IDX4)  =
                                     RECE02-TENSU(34 BUN-IDX4)  +
                                    (WK-SYUTEN   *   ACCT-ZAIKAISU)
                         END-IF
                       END-IF
                   END-IF
                   IF    BUN-IDX        =   1
                       COMPUTE   RECE02-TENSU(35 BUN-IDX)  =
                                 RECE02-TENSU(35 BUN-IDX)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX2       =   2
                       COMPUTE   RECE02-TENSU(35 BUN-IDX2)  =
                                 RECE02-TENSU(35 BUN-IDX2)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX3       =   3
                       COMPUTE   RECE02-TENSU(35 BUN-IDX3)  =
                                 RECE02-TENSU(35 BUN-IDX3)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
                   IF    BUN-IDX4       =   4
                       COMPUTE   RECE02-TENSU(35 BUN-IDX4)  =
                                 RECE02-TENSU(35 BUN-IDX4)  +
                                (WK-YKZTEN   *   WK-ZAIKAISU)
                   END-IF
           END-EVALUATE
           .
      *
       2104421-RECE-LEFT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項の編集
      *****************************************************************
       21045-TOKKI-HENSYU-SEC                 SECTION.
      *
           MOVE     SPACE            TO      WRK-TOKKI
      *
           EVALUATE    FLG-TOKKI
             WHEN        1
               EVALUATE    ACCT-SRYCDTOTAL
                   WHEN    CONS-TOKKI1
                       MOVE    "01"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI2
                       MOVE    "02"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI3
                       MOVE    "03"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI4
                       MOVE    "04"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI5
                       MOVE    "05"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI7
                       MOVE    "07"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI8
                       MOVE    "08"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI9
                       MOVE    "09"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI10
                       MOVE    "10"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI11
                       MOVE    "11"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI12
                       MOVE    "12"      TO      WRK-TOKKI
               END-EVALUATE
             WHEN        2
               EVALUATE    SRY-SRYCD(IDX2)
                   WHEN    CONS-TOKKI1(6:9)
                       MOVE    "01"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI2(6:9)
                       MOVE    "02"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI3(6:9)
                       MOVE    "03"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI4(6:9)
                       MOVE    "04"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI5(6:9)
                       MOVE    "05"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI7(6:9)
                       MOVE    "07"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI8(6:9)
                       MOVE    "08"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI9(6:9)
                       MOVE    "09"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI10(6:9)
                       MOVE    "10"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI11(6:9)
                       MOVE    "11"      TO      WRK-TOKKI
                   WHEN    CONS-TOKKI12(6:9)
                       MOVE    "12"      TO      WRK-TOKKI
               END-EVALUATE
           END-EVALUATE
      *
      *    既に同じ特記事項コードが編集済みのときはセットしない
           PERFORM VARYING TOKKI-IDX  FROM    1   BY  1
                   UNTIL ( TOKKI-IDX       >   5     )
               IF      RECE02-TOKKI (TOKKI-IDX)   =   SPACE
                   MOVE    WRK-TOKKI       TO  RECE02-TOKKI (TOKKI-IDX)
                   MOVE    5               TO  TOKKI-IDX
               ELSE
                   IF      WRK-TOKKI       =   RECE02-TOKKI (TOKKI-IDX)
                       MOVE    5               TO  TOKKI-IDX
                   END-IF 
               END-IF
           END-PERFORM
           .
       21045-TOKKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルへの薬剤一部負担金セット（Ｆ００２）
      *****************************************************************
       2105-YKZFTN-SET-SEC                SECTION.
      *
           INITIALIZE                           SYUNOU-REC
           MOVE    RECE02-HOSPID        TO      SYU-HOSPID
           MOVE    RECE02-NYUGAIKBN     TO      SYU-NYUGAIKBN
           MOVE    RECE02-PTID          TO      SYU-PTID
           MOVE    RECE02-SRYKA(IDX)    TO      SYU-SRYKA
           MOVE    RECE02-SRYYM         TO      SYU-SRYYMD(1:6)
           MOVE    "__"                 TO      SYU-SRYYMD(7:2)
           MOVE    SYUNOU-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SYUNOU-KEY16"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SYUNOU-KEY16"   TO      ORC-DBPATH
               PERFORM  900-SYUNOU-READ-SEC
               PERFORM  UNTIL
                        (FLG-SYUNOU             =    1)
                   PERFORM  VARYING  IDX1   FROM  1   BY  1
                             UNTIL   IDX1    >    10
                              OR   RECE02-HKNCOMBI (IDX1)   =   ZERO
                       IF    SYU-HKNCOMBINUM   =
                                            RECE02-HKNCOMBI (IDX1)
                          PERFORM       21051-SYUTOTAL-FIND-SEC
                          EVALUATE  FLG-SYUTOTAL-SET
                             WHEN     ZERO
                                PERFORM 21052-RECE02-HENSYU3-SEC
                                MOVE    10              TO  IDX1
                             WHEN      1
                                PERFORM 21053-SYUNOU-HENKAN-SEC
                                PERFORM 21052-RECE02-HENSYU3-SEC
                                MOVE    10              TO  IDX1
                             WHEN      2
                                MOVE    10              TO  IDX1
                          END-EVALUATE
                       END-IF
                   END-PERFORM
                   MOVE     "SYUNOU-KEY16"  TO   ORC-DBPATH
                   PERFORM   900-SYUNOU-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY   "SYUNOU SELECT ERR"
           END-IF
      *
           MOVE    "SYUNOU-KEY16"     TO  WK-PATH
           PERFORM 900-CSR-CLOSE-SEC
      *
           .
       2105-YKZFTN-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    収納合計マスタの読み込みの判別
      ******************************************************************
       21051-SYUTOTAL-FIND-SEC           SECTION.
      *
           MOVE     ZERO       TO     FLG-SYUTOTAL-SET
      *
      ***     IF    (RECE02-RECEKA      NOT =  SPACE)
      ***     OR    (SYS-1038-KAGRPFLG      =   "1" )
      ***        CONTINUE
      ***     ELSE
      *
      ***       IF   SYU-FUKU-DENPNUM    NOT =   ZERO
      ***       AND  SYU-GRP-DENPNUM     NOT =   ZERO
      *
      ***         MOVE     ZERO     TO    FLG-SET
      ***         PERFORM  VARYING  IDX9    FROM    1   BY  1
      ***                   UNTIL   IDX9                  > 100
      ***                    OR    TBL-FUKU-DENPNUM(IDX9) = ZERO
      ***                    OR    FLG-SET                =  1
      ***            IF  TBL-FUKU-DENPNUM(IDX9)  =  SYU-FUKU-DENPNUM
      ***                MOVE      1      TO      FLG-SET
      ***                MOVE      2      TO      FLG-SYUTOTAL-SET
      ***            END-IF
      ***         END-PERFORM
      *
      ***         IF  FLG-SET  =  ZERO
      *
      ***            INITIALIZE                         SYUTTL-REC
      ***            MOVE    SYU-HOSPID         TO      SYUTTL-HOSPID
      ***            MOVE    SYU-NYUGAIKBN      TO      SYUTTL-NYUGAIKBN
      ***            MOVE    SYU-PTID           TO      SYUTTL-PTID
      ***            MOVE    SYU-FUKU-DENPNUM   TO      SYUTTL-DENPNUM
      ***            MOVE    SYUTTL-REC         TO      MCPDATA-REC
      ***            MOVE    "DBSELECT"         TO      MCP-FUNC
      ***            MOVE    "SYUTOTAL-KEY"     TO      ORC-DBPATH
      ***            CALL    "ORCMCPSUB"         USING   MCPAREA
      ***                                                ORCMCPAREA
      ***                                                MCPDATA-REC
      ***            IF      MCP-RC             =       ZERO
      ***                 MOVE      "SYUTOTAL-KEY"   TO   ORC-DBPATH
      ***                 PERFORM    900-SYUTOTAL-READ-SEC
      ***                 IF   FLG-SYUTOTAL   =    ZERO
      ***                      MOVE   SYU-FUKU-DENPNUM
      ***                                   TO    TBL-FUKU-DENPNUM(IDX9)
      ***                      MOVE    1    TO    FLG-SYUTOTAL-SET
      ***                 ELSE
      ***                      DISPLAY   "SYUTOTAL FETCH ERR"
      ***                 END-IF
      ***            ELSE
      ***                 DISPLAY   "SYUTOTAL SELECT ERR"
      ***            END-IF
      *
      ***            MOVE    "SYUTOTAL-KEY"     TO  WK-PATH
      ***            PERFORM 900-CSR-CLOSE-SEC
      *
      ***         END-IF
      *
      ***       END-IF
      *
      ***     END-IF
      *
           .
      *
       21051-SYUTOTAL-FIND-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送−薬剤一部負担金（Ｆ００２）
      *****************************************************************
       21052-RECE02-HENSYU3-SEC               SECTION.
      *
           COMPUTE   RECE02-YKZFTN(1)   =    RECE02-YKZFTN(1)   +
                                             SYU-SYUCOMPYKZFTN
           IF      RECE02-SRYYM      >=  "200210"
             IF    RECE02-ZAITAKU      NOT =    SPACE
                PERFORM     210521-HOKEN-FUTAN-HENSYU-SEC
             ELSE                                                       
      ***>      IF    RECE02-RJNHKNNUM    NOT =    SPACE                
      ***>        ADD    SYU-KOH1COMPFTN  TO   RECE02-RJNGAIFTN         
      ***>      END-IF                                                  
      *         船員保険（被保険者）＋通勤災害
                IF    RECE02-SENIN-TUUSAI     =    "1"
                  ADD    SYU-SYUCOMPFTN   TO   RECE02-FTNMONEY(1)
                END-IF
             END-IF
           ELSE
             IF    RECE02-RJNHKNNUM    NOT =    SPACE
                ADD    SYU-KOH1COMPFTN  TO   RECE02-FTNMONEY(1)
      ***>      ADD    SYU-KOH1COMPFTN  TO   RECE02-RJNGAIFTN
             END-IF
      *      船員保険（被保険者）＋通勤災害
             IF    RECE02-SENIN-TUUSAI     =    "1"
                ADD    SYU-SYUCOMPFTN   TO   RECE02-FTNMONEY(1)
             END-IF
           END-IF
      *
           MOVE    IDX1            TO  HC-IDX
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL    IDX2      >       4
              IF      RECE02-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(1)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(1)
                      MOVE    2          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE02-KOHNUM(1)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE02-KOHNUM(1)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE02-KOHNUM(1)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE02-KOHNUM(1)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   210522-KOHJNISSUTBL-SET-SEC
      *
                      COMPUTE  RECE02-YKZFTN(KH-IDX) =
                                          RECE02-YKZFTN(KH-IDX) +
                                          SYU-KOH-COMPYKZFTN(IDX3)
                      ADD      1          TO   RECE02-KOHKENSU(1)
                      IF    (RECE02-SRYYM        >=  "200404")
                      AND   (RECE02-ZAITAKU   NOT =    SPACE )
                      AND   (RECE02-KOHNUM(1)     =  "015" OR "016"
                                                           OR "051")
                         ADD   SYU-KOH-HKNFTNMONEY(IDX3)  TO
                                            RECE02-FTNMONEY(KH-IDX)
                                            RECE02-FTNMONEY-KOJ(KH-IDX)
                      ELSE
                         IF    (RECE02-FTNMONEY(1)  =  ZERO)
                         AND   (RECE02-KOHNUM(1)    =  "010" OR "021")
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         ELSE
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY(KH-IDX)
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         END-IF
                      END-IF
                      IF   RECE02-KOHNUM(1) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE02-KOHNUM(1) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                      PERFORM   210523-FTNMONEY-FTN-HENSYU-SEC
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(2)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(2)
                      MOVE    3          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE02-KOHNUM(2)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE02-KOHNUM(2)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE02-KOHNUM(2)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE02-KOHNUM(2)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   210522-KOHJNISSUTBL-SET-SEC
      *
                      COMPUTE  RECE02-YKZFTN(KH-IDX) =
                                          RECE02-YKZFTN(KH-IDX) +
                                          SYU-KOH-COMPYKZFTN(IDX3)
                      ADD      1          TO   RECE02-KOHKENSU(2)
                      IF    (RECE02-SRYYM        >=  "200404")
                      AND   (RECE02-ZAITAKU   NOT =    SPACE )
                      AND   (RECE02-KOHNUM(2)     =  "015" OR "016"
                                                           OR "051")
                         ADD   SYU-KOH-HKNFTNMONEY(IDX3)  TO
                                            RECE02-FTNMONEY(KH-IDX)
                                            RECE02-FTNMONEY-KOJ(KH-IDX)
                      ELSE
                         IF    (RECE02-FTNMONEY(1)  =  ZERO)
                         AND   (RECE02-KOHNUM(2)    =  "010" OR "021")
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         ELSE
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY(KH-IDX)
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         END-IF
                      END-IF
                      IF   RECE02-KOHNUM(2) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE02-KOHNUM(2) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                      PERFORM   210523-FTNMONEY-FTN-HENSYU-SEC
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(3)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(3)
                      MOVE    4          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE02-KOHNUM(3)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE02-KOHNUM(3)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE02-KOHNUM(3)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE02-KOHNUM(3)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   210522-KOHJNISSUTBL-SET-SEC
      *
                      COMPUTE  RECE02-YKZFTN(KH-IDX) =
                                          RECE02-YKZFTN(KH-IDX) +
                                          SYU-KOH-COMPYKZFTN(IDX3)
                      ADD      1          TO   RECE02-KOHKENSU(3)
                      IF    (RECE02-SRYYM        >=  "200404")
                      AND   (RECE02-ZAITAKU   NOT =    SPACE )
                      AND   (RECE02-KOHNUM(3)     =  "015" OR "016"
                                                           OR "051")
                         ADD   SYU-KOH-HKNFTNMONEY(IDX3)  TO
                                            RECE02-FTNMONEY(KH-IDX)
                                            RECE02-FTNMONEY-KOJ(KH-IDX)
                      ELSE
                         IF    (RECE02-FTNMONEY(1)  =  ZERO)
                         AND   (RECE02-KOHNUM(3)    =  "010" OR "021")
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         ELSE
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY(KH-IDX)
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         END-IF
                      END-IF
                      IF   RECE02-KOHNUM(3) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE02-KOHNUM(3) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                      PERFORM   210523-FTNMONEY-FTN-HENSYU-SEC
                 END-IF
                 IF   RECE02-KOHNUMHC(HC-IDX IDX2) =  RECE02-KOHNUM(4)
                 AND  RECE02-KOHIDHC(HC-IDX IDX2)  =  RECE02-KOHID(4)
                      MOVE    5          TO        KH-IDX
                      EVALUATE  TRUE
                          WHEN  RECE02-KOHNUM(4)  =  SYU-KOH-HKNNUM(1)
                                MOVE     1    TO   IDX3
                          WHEN  RECE02-KOHNUM(4)  =  SYU-KOH-HKNNUM(2)
                                MOVE     2    TO   IDX3
                          WHEN  RECE02-KOHNUM(4)  =  SYU-KOH-HKNNUM(3)
                                MOVE     3    TO   IDX3
                          WHEN  RECE02-KOHNUM(4)  =  SYU-KOH-HKNNUM(4)
                                MOVE     4    TO   IDX3
                      END-EVALUATE
      *
                      PERFORM   210522-KOHJNISSUTBL-SET-SEC
      *
                      COMPUTE  RECE02-YKZFTN(KH-IDX) =
                                          RECE02-YKZFTN(KH-IDX) +
                                          SYU-KOH-COMPYKZFTN(IDX3)
                      ADD      1          TO   RECE02-KOHKENSU(4)
                      IF    (RECE02-SRYYM        >=  "200404")
                      AND   (RECE02-ZAITAKU   NOT =    SPACE )
                      AND   (RECE02-KOHNUM(4)     =  "015" OR "016"
                                                           OR "051")
                         ADD   SYU-KOH-HKNFTNMONEY(IDX3)  TO
                                            RECE02-FTNMONEY(KH-IDX)
                                            RECE02-FTNMONEY-KOJ(KH-IDX)
                      ELSE
                         IF    (RECE02-FTNMONEY(1)  =  ZERO)
                         AND   (RECE02-KOHNUM(4)    =  "010" OR "021")
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         ELSE
                               ADD   SYU-KOH-COMPFTN(IDX3)  TO
                                        RECE02-FTNMONEY(KH-IDX)
                                        RECE02-FTNMONEY-KOJ(KH-IDX)
                         END-IF
                      END-IF
                      IF   RECE02-KOHNUM(4) =  "051"
                      AND  FLG-KOH      NOT =   2
                            MOVE     1   TO   FLG-KOH
                      END-IF
                      IF   RECE02-KOHNUM(4) =  "010"  OR  "021" 
                            MOVE     2   TO   FLG-KOH
                      END-IF 
                      PERFORM   210523-FTNMONEY-FTN-HENSYU-SEC
                 END-IF
              END-IF
           END-PERFORM
           .
       21052-RECE02-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険分負担金額編集
      *****************************************************************
       210521-HOKEN-FUTAN-HENSYU-SEC               SECTION.
      *
      ***  IF      RECE02-SRYYM      <    "200310"
      *
                COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                    * 10
                                    * RECE02-KYURATE
                                    / 100
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
                COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
                IF   ( SYU-KOH-FTN         =   ZERO   )                 
                AND  ( SYU-KOH-HKNNUM(1)   =   SPACE OR "027"  )        
                AND  ( SYU-KOH-HKNNUM(2)   =   SPACE  )                 
                AND  ( SYU-KOH-HKNNUM(3)   =   SPACE  )                 
                AND  ( SYU-KOH-HKNNUM(4)   =   SPACE  )                 
                    IF    RECE02-RJNHKNNUM    NOT =    SPACE            
                      ADD    SYU-KOH1COMPFTN  TO   RECE02-FTNMONEY(1)   
      ***>                                         RECE02-RJNGAIFTN     
                                                   WRK-KOH-GAI          
                    ELSE                                                
                      ADD    SYU-SYUCOMPFTN   TO   RECE02-FTNMONEY(1)   
                                                   WRK-KOH-GAI          
                    END-IF                                              
                ELSE
                  IF   ( SYU-KOH-HKNNUM(1)   =   "012" )                
                  OR   ( SYU-KOH-HKNNUM(2)   =   "012" )                
                  OR   ( SYU-KOH-HKNNUM(3)   =   "012" )                
                  OR   ( SYU-KOH-HKNNUM(4)   =   "012" )                
                      MOVE    8000     TO   WRK-KINGAKU
                      PERFORM   2105211-ICHIBUFTN-KOH-SET-SEC
                  ELSE
                    IF   ( SYU-KOH-HKNNUM(1) =   "972" )                
                    OR   ( SYU-KOH-HKNNUM(2) =   "972" )                
                    OR   ( SYU-KOH-HKNNUM(3) =   "972" )                
                    OR   ( SYU-KOH-HKNNUM(4) =   "972" )                
      ***           IF   ( RECE02-TOKKI(1)     =   "02"  )
      ***           OR   ( RECE02-TOKKI(2)     =   "02"  )
      ***           OR   ( RECE02-TOKKI(3)     =   "02"  )
      ***           OR   ( RECE02-TOKKI(4)     =   "02"  )
      ***           OR   ( RECE02-TOKKI(5)     =   "02"  )
                      MOVE    10000    TO   WRK-KINGAKU
                      PERFORM   2105211-ICHIBUFTN-KOH-SET-SEC
                    ELSE
                      IF   (  RECE02-TEISYOTOKU  NOT =  SPACE  )
                      AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                        MOVE  8000     TO   WRK-KINGAKU
                        PERFORM   2105211-ICHIBUFTN-KOH-SET-SEC
                      ELSE
                        IF   (  RECE02-KYURATE         =  20     )
                        AND  (  FLG-ZENKOH-ARI         =  ZERO   )
                           MOVE 40200     TO   WRK-KINGAKU
                           PERFORM   2105211-ICHIBUFTN-KOH-SET-SEC
                        ELSE
                           MOVE 12000     TO   WRK-KINGAKU
                           PERFORM   2105211-ICHIBUFTN-KOH-SET-SEC
                        END-IF
                      END-IF
                    END-IF
                  END-IF                                              
                END-IF                                                  
      *
                ADD   SYU-KOH-FTN   TO   WRK-KOH-FTN
      *
      ***  ELSE
      *         収納項目から編集するように変更
      ***          IF   ( SYU-KOH-FTN         =   ZERO   )                 
      ***          AND  ( SYU-KOH-HKNNUM(1)   =   SPACE OR "027"  )        
      ***          AND  ( SYU-KOH-HKNNUM(2)   =   SPACE  )                 
      ***          AND  ( SYU-KOH-HKNNUM(3)   =   SPACE  )                 
      ***          AND  ( SYU-KOH-HKNNUM(4)   =   SPACE  )                 
      ***              IF    RECE02-RJNHKNNUM    NOT =    SPACE            
      ***                ADD    SYU-KOH1COMPFTN  TO   RECE02-FTNMONEY(1)   
      ******>                                         RECE02-RJNGAIFTN     
      ***                                             WRK-KOH-GAI          
      ***              ELSE                                                
      ***                ADD    SYU-SYUCOMPFTN   TO   RECE02-FTNMONEY(1)   
      ***                                             WRK-KOH-GAI          
      ***              END-IF                                              
      ***          ELSE
      ***              IF    RECE02-RJNHKNNUM    NOT =    SPACE
      ***                ADD    SYU-KOH1COMPFTN  TO   RECE02-FTNMONEY(1)
      ******>                                         RECE02-RJNGAIFTN
      ***                                             FTN-KOH-XXX
      ***              ELSE
      ***                ADD    SYU-SYUCOMPFTN   TO   RECE02-FTNMONEY(1)
      ***                                             FTN-KOH-XXX
      ***              END-IF
      ***          END-IF
      *
      ***          ADD   SYU-KOH-FTN   TO   WRK-KOH-FTN
      *
      ***  END-IF
      *
           .
       210521-HOKEN-FUTAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額セット（公費含む場合）
      *****************************************************************
       2105211-ICHIBUFTN-KOH-SET-SEC               SECTION.
      *
           IF  FTN-KOH-XXX  >   WRK-KINGAKU
                MOVE   WRK-KINGAKU    TO  FTN-KOH-XXX
           END-IF
           COMPUTE  FTN-KOH-YYY  =   WRK-KINGAKU -  FTN-KOH-XXX
           IF  WRK-FTNKEI   >   FTN-KOH-YYY
                ADD    FTN-KOH-YYY    TO  RECE02-FTNMONEY(1)
                                          FTN-KOH-XXX
      ***>      IF    RECE02-RJNHKNNUM    NOT =    SPACE
      ***>          ADD  FTN-KOH-YYY  TO  RECE02-RJNGAIFTN
      ***>      END-IF
           ELSE
                ADD    WRK-FTNKEI     TO  RECE02-FTNMONEY(1)
                                          FTN-KOH-XXX
      ***>      IF    RECE02-RJNHKNNUM    NOT =    SPACE
      ***>          ADD  WRK-FTNKEI   TO  RECE02-RJNGAIFTN
      ***>      END-IF
           END-IF
      *
           .
       2105211-ICHIBUFTN-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費実日数テーブル情報のセット
      *****************************************************************
       210522-KOHJNISSUTBL-SET-SEC               SECTION.
      *
           EVALUATE   SYS-1001-PREFNUM
      *
             WHEN    "38"
      *
               IF   SYU-KOH-HKNNUM(IDX3)(1:1) = "1" OR "2" OR "3" OR
                                                "4" OR "5" OR "6" OR
                                                "7" OR "8"
                    MOVE   SYU-SRYYMD(7:2)  TO   IDX8
                    MOVE     "1"     TO   RECE02-KOHJNISSUTBL(IDX8)
               END-IF
      *
           END-EVALUATE
      *
           .
       210522-KOHJNISSUTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担金額（一部負担額）項目の編集
      *****************************************************************
       210523-FTNMONEY-FTN-HENSYU-SEC               SECTION.
      *
      *    老人保険＋原爆
           IF      RECE02-SRYYM      >=  "200404"
              IF   ( RECE02-ZAITAKU            =    SPACE )  AND
                   ( RECE02-RJNHKNNUM      NOT =    SPACE )  AND
                   ( SYU-KOH-HKNNUM(IDX3)      =    "019" )
      *
                    EVALUATE   IDX3
                      WHEN      2
                        COMPUTE  WRK-FTNKEI-FTN  =  SYU-TOTAL-HKNTEN
                                                 *  10
                                                 *  RECE02-KYURATE
                                                 /  100
                        ADD      WRK-FTNKEI-FTN  TO
                                         RECE02-FTNMONEY-FTN(KH-IDX)
      *
                      WHEN      3
                      WHEN      4
                        MOVE    SYU-KOH-COMPFTN(IDX3 - 1)
                                                 TO   WRK-FTNKEI-FTN
                        ADD      WRK-FTNKEI-FTN  TO
                                         RECE02-FTNMONEY-FTN(KH-IDX)
                    END-EVALUATE
      *
                    IF   ( SYU-KOH-HKNNUM(1) =   "972" )  OR
                         ( SYU-KOH-HKNNUM(2) =   "972" )  OR
                         ( SYU-KOH-HKNNUM(3) =   "972" )  OR
                         ( SYU-KOH-HKNNUM(4) =   "972" )
                       IF    RECE02-FTNMONEY-FTN(KH-IDX)   >   10000
                          MOVE   10000  TO  RECE02-FTNMONEY-FTN(KH-IDX)
                       END-IF
                    ELSE
                       IF    RECE02-FTNMONEY-FTN(KH-IDX)   >   12000
                          MOVE   12000  TO  RECE02-FTNMONEY-FTN(KH-IDX)
                       END-IF
                    END-IF
      *
              END-IF
           END-IF
      *
           .
       210523-FTNMONEY-FTN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    収納合計マスタの内容を収納マスタへ
      ******************************************************************
       21053-SYUNOU-HENKAN-SEC           SECTION.
      *
           INITIALIZE                             SYUNOU-REC
      *ＫＥＹ
           MOVE     SYUTTL-KEY                TO  SYU-KEY
      *診療日
           MOVE     SYUTTL-SRYYMD             TO  SYU-SRYYMD
      *保険組合せ番号
           MOVE     SYUTTL-HKNCOMBINUM        TO  SYU-HKNCOMBINUM
      *主保険−負担情報
           MOVE     SYUTTL-SYUHKNNUM          TO  SYU-SYUHKNNUM
           MOVE     SYUTTL-SYUHKNFTNMONEY     TO  SYU-SYUHKNFTNMONEY
           MOVE     SYUTTL-SYUCOMPFTN         TO  SYU-SYUCOMPFTN
           MOVE     SYUTTL-SYUCOMPFTN-ENTANI  TO  SYU-SYUCOMPFTN-ENTANI
           MOVE     SYUTTL-SYUCOMPYKZFTN      TO  SYU-SYUCOMPYKZFTN
           MOVE     SYUTTL-SYUCOMPTOTAL       TO  SYU-SYUCOMPTOTAL
           MOVE     SYUTTL-SYUYKZFTNKBN       TO  SYU-SYUYKZFTNKBN
      *公費−負担情報
           MOVE     SYUTTL-KOH-AREA           TO  SYU-KOH-AREA
      *請求内容
           MOVE     SYUTTL-SKY-NAIYOU         TO  SYU-SKY-NAIYOU
      *公費一部負担金
           MOVE     SYUTTL-KOH-FTN            TO  SYU-KOH-FTN
      *公費一部負担金（円単位）
           MOVE     SYUTTL-KOH-FTN-ENTANI     TO  SYU-KOH-FTN-ENTANI
      *
           .
      *
       21053-SYUNOU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送２（Ｆ００２）:実日数編集
      *****************************************************************
       2106-JNISSU-SET-SEC                    SECTION.                       
      *実日数編集
           PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                      UNTIL      HKN-IDX    >      5
               PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                          UNTIL      ACCT-IDX    >     31
                    IF    TBL-JNISSU-DAY(HKN-IDX ACCT-IDX)
                                        NOT =  SPACE
                          ADD    1      TO    RECE02-JNISSU(HKN-IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *実日数テーブル編集
           PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                      UNTIL      HKN-IDX    >      2
               PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                          UNTIL      ACCT-IDX    >     31
                    IF    TBL-JNISSU-DAY(HKN-IDX ACCT-IDX)
                                        NOT =  SPACE
                          MOVE   "1"    TO    RECE02-JNISSUTBL(ACCT-IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *公費実日数テーブル再編集
           PERFORM    VARYING    ACCT-IDX    FROM    1   BY   1
                      UNTIL      ACCT-IDX    >     31
              IF    RECE02-KOHJNISSUTBL(ACCT-IDX)  NOT =  SPACE
                 IF  RECE02-JNISSUTBL(ACCT-IDX)  = SPACE
                     MOVE   SPACE   TO   RECE02-KOHJNISSUTBL(ACCT-IDX)
                 END-IF
              END-IF
           END-PERFORM
      *
           .
       2106-JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの基本点数部分のクリア処理（Ｆ００２）
      *****************************************************************
       2107-KHNTEN-CLEAR-SEC               SECTION.
      *
           IF        FLG-KHNTEN2        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(2)
           END-IF
           IF        FLG-KHNTEN3        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(3)
           END-IF
           IF        FLG-KHNTEN4        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(4)
           END-IF
           IF        FLG-KHNTEN5        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(5)
           END-IF
           IF        FLG-KHNTEN6        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(6)
           END-IF
           IF        FLG-KHNTEN15        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(15)
           END-IF
           IF        FLG-KHNTEN18        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(18)
           END-IF
           IF        FLG-KHNTEN19        =    1
               MOVE    ZERO             TO   RECE02-KHNTEN(19)
           END-IF
      *
           .
       2107-KHNTEN-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルのぶん点数部分のクリア処理（Ｆ００２）
      *****************************************************************
       2108-BUNTENSU-CLEAR-SEC               SECTION.                        
      *
      *    インデックス２
           IF (RECE02-HENTOTALTEN(2)   NOT =  ZERO)           AND
              (RECE02-HENTOTALTEN(1)   NOT =  RECE02-HENTOTALTEN(2))
               CONTINUE
           ELSE
               PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                          UNTIL      HKN-IDX    >      50
                       MOVE   ZERO   TO   RECE02-TENSU(HKN-IDX 2)
               END-PERFORM
           END-IF
      *    インデックス３
           IF (RECE02-HENTOTALTEN(3)   NOT =  ZERO)           AND
              (RECE02-HENTOTALTEN(1)   NOT =  RECE02-HENTOTALTEN(3))
               CONTINUE
           ELSE
               PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                          UNTIL      HKN-IDX    >      50
                       MOVE   ZERO   TO   RECE02-TENSU(HKN-IDX 3)
               END-PERFORM
           END-IF
      *    インデックス４
           IF (RECE02-HENTOTALTEN(4)   NOT =  ZERO)           AND
              (RECE02-HENTOTALTEN(1)   NOT =  RECE02-HENTOTALTEN(4))
               CONTINUE
           ELSE
               PERFORM    VARYING    HKN-IDX    FROM   1   BY   1
                          UNTIL      HKN-IDX    >      50
                       MOVE   ZERO   TO   RECE02-TENSU(HKN-IDX 4)
               END-PERFORM
           END-IF
      *
           .
       2108-BUNTENSU-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅総合・在宅末期一部負担金再掲の計算処理
      *****************************************************************
       2109-SAIKEI-KEISAN-SEC               SECTION.
      *
           IF   FTN-KOH-XXX    >    WRK-CHK-ICHIBUGAKU
                COMPUTE  FTN-KOH-XXX = FTN-KOH-XXX - WRK-CHK-ICHIBUGAKU
           ELSE
                MOVE     ZERO    TO    FTN-KOH-XXX
           END-IF
      *
           IF   RECE02-ZAITAKU  =  SPACE
                  MOVE     ZERO      TO     RECE02-ZAITAKU-SAIKEI
           ELSE
              IF   FTN-KOH-XXX   =   ZERO        
      *       OR   WRK-KOH-FTN   =   ZERO
                  MOVE     ZERO      TO     RECE02-ZAITAKU-SAIKEI
              ELSE 
                  IF  FLG-KOH     =   ZERO
                     COMPUTE   RECE02-ZAITAKU-SAIKEI  =  
                               FTN-KOH-XXX
                     IF   FTN-KOH-XXX   =   WRK-KOH-FTN 
                           MOVE  ZERO   TO   RECE02-ZAITAKU-SAIKEI
                     END-IF
                     IF   FTN-KOH-XXX   =   RECE02-FTNMONEY(1)
                           MOVE  ZERO   TO   RECE02-ZAITAKU-SAIKEI
                     END-IF
                  ELSE
                     IF  FLG-KOH  =   1
                        COMPUTE   RECE02-ZAITAKU-SAIKEI  =  
                                  FTN-KOH-XXX
                     END-IF
                     IF  FLG-KOH  =   2
                        ADD   WRK-KOH-GAI   TO  RECE02-FTNMONEY(2)
                                                RECE02-FTNMONEY-KOJ(2)
                        COMPUTE   RECE02-ZAITAKU-SAIKEI  =  
                                  FTN-KOH-XXX
                     END-IF
                     IF   FTN-KOH-XXX   =   WRK-KOH-FTN 
                           MOVE  ZERO   TO   RECE02-ZAITAKU-SAIKEI
                     END-IF
                     IF   FTN-KOH-XXX   =   RECE02-FTNMONEY(1)
                           MOVE  ZERO   TO   RECE02-ZAITAKU-SAIKEI
                     END-IF
                  END-IF
              END-IF
           END-IF
      *
           .
       2109-SAIKEI-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳幼児・幼児のコメントフラグセット
      *****************************************************************
       2110-NYUYOJI-COMENTFLG-SET-SEC               SECTION.
      *
           IF     RECE02-NYUYOJIFLG      =  SPACE
             IF     CNT-NYUYOJI      NOT =  ZERO
               IF   RECE02-KAISU(2)  NOT =  CNT-NYUYOJI
                   MOVE     "1"        TO   RECE02-NYUYOJIFLG
                   MOVE   ZERO         TO   RECE02-KHNTEN(2)
               END-IF
             END-IF
           END-IF
      *
           IF     RECE02-NYUYOJIFLG      =  SPACE
             IF     CNT-YOJI         NOT =  ZERO
               IF   RECE02-KAISU(2)  NOT =  CNT-YOJI
                   MOVE     "2"        TO   RECE02-NYUYOJIFLG
                   MOVE   ZERO         TO   RECE02-KHNTEN(2)
               END-IF
             END-IF
           END-IF
      *
           .
       2110-NYUYOJI-COMENTFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担金額の置き換え等
      *****************************************************************
       2111-ICHIBUFUTAN-OKIKAE-SEC               SECTION.
      *
      *    生保
      *
           PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                  UNTIL    KOHFTN-IDX    >    4
      *
             IF   (RECE02-KOHNUM(KOHFTN-IDX) =  "012"   )   AND
                  (RECE02-SRYYM             >=  "200304")
      *
                PERFORM  21111-KOHFTN-KINGAKU-SEC
      *
                IF    RECE02-HKNNUM   =  SPACE
                   MOVE     WRK-GAIJGNGAK
                             TO  RECE02-FTNMONEY(KOHFTN-IDX + 1)
                                 RECE02-FTNMONEY-KOJ(KOHFTN-IDX + 1)
                END-IF
      *
             END-IF
      *
           END-PERFORM
      *
      *    福岡４１老人
      *
           EVALUATE   SYS-1001-PREFNUM
      *
             WHEN    "40"
      *
                PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                            UNTIL    KOHFTN-IDX     >    4
      *
                   IF   RECE02-KOHHBTNUM(KOHFTN-IDX) =  "41"
      *
      *               保険番号テーブルの検索
                      INITIALIZE                      HKNNUM-REC
                      MOVE   RECE02-HOSPID      TO    HKN-HOSPID
                      MOVE   RECE02-KOHNUM(KOHFTN-IDX)
                                                TO    HKN-HKNNUM
                      MOVE   HKNNUM-REC         TO    MCPDATA-REC
                      MOVE   "DBSELECT"         TO    MCP-FUNC
                      MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
                      CALL   "ORCMCPSUB"        USING MCPAREA
                                                      ORCMCPAREA
                                                      MCPDATA-REC
                      IF      MCP-RC            =     ZERO
                         MOVE    "HKNNUM-KEY2"  TO    ORC-DBPATH
                         PERFORM  900-HKNNUM-READ-SEC
                         PERFORM  UNTIL
                          ( FLG-HKNNUM         =   1               ) OR
                          ((HKN-TEKSTYMD(1:6) <= RECE02-SRYYM     ) AND
                           (RECE02-SRYYM      <= HKN-TEKEDYMD(1:6)))
                             MOVE     "HKNNUM-KEY2" TO    ORC-DBPATH
                             PERFORM   900-HKNNUM-READ-SEC
                         END-PERFORM
                      ELSE
                         MOVE    1              TO    FLG-HKNNUM
                      END-IF
      *
                      IF   FLG-HKNNUM  =  ZERO
      *
                          MOVE    ZERO    TO   WRK-JYOGEN41
      *
                          COMPUTE  WRK-FTNKEI
                                =  RECE02-TOTALTEN(KOHFTN-IDX + 1)
                                *  10
                                *  HKN-HON-GAIKFTNRATE
                                /  100
                          COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
                          COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
                          COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
                          IF     HKN-HON-GAIMNAIJGNGAK  NOT = ZERO
                                MOVE   HKN-HON-GAIMNAIJGNGAK
                                              TO   WRK-JYOGEN41
                          ELSE
                             IF  HKN-HON-GAIMGAIJGNGAK  NOT = ZERO
                                MOVE   HKN-HON-GAIMGAIJGNGAK
                                              TO   WRK-JYOGEN41
                             END-IF
                          END-IF
                          IF   WRK-JYOGEN41  =  ZERO
                              MOVE   WRK-FTNKEI   TO
                                   RECE02-FTNMONEY(KOHFTN-IDX + 1)
                                   RECE02-FTNMONEY-KOJ(KOHFTN-IDX + 1)
                          ELSE
                            IF   WRK-JYOGEN41  <  WRK-FTNKEI
                              MOVE   WRK-JYOGEN41 TO
                                   RECE02-FTNMONEY(KOHFTN-IDX + 1) 
                                   RECE02-FTNMONEY-KOJ(KOHFTN-IDX + 1)
                            ELSE
                              MOVE   WRK-FTNKEI   TO
                                   RECE02-FTNMONEY(KOHFTN-IDX + 1) 
                                   RECE02-FTNMONEY-KOJ(KOHFTN-IDX + 1)
                            END-IF
                          END-IF
      *
                      END-IF
      *
                      MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
                      MOVE    "HKNNUM-KEY2"         TO  ORC-DBPATH
                      CALL    "ORCMCPSUB"           USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
      *
                   END-IF
      *
                END-PERFORM
      *
           END-EVALUATE
      *
      *    公費一部負担金レセ記載フラグの編集
      *    公費一部負担金０円記載フラグの編集
      *
           IF    RECE02-SRYYM        >=  "200404"
      *
             PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                         UNTIL    KOHFTN-IDX     >    4
      *
      ****     IF   RECE02-ZAITAKU   NOT =    SPACE
                   IF ( RECE02-KOHNUM(KOHFTN-IDX) =  "012" OR "015"
                                                  OR "016" OR "051" )
                   OR ((SYS-1001-PREFNUM =   13           ) AND
                       (RECE02-KOHNUM(KOHFTN-IDX) =  "151" OR "451"))
                   OR ((SYS-1001-PREFNUM =   28           ) AND
                       (RECE02-KOHNUM(KOHFTN-IDX) =  "186"         ))
                         MOVE  "1"  TO
                                 RECE02-KISAIFLG(KOHFTN-IDX + 1)
                   END-IF
      ****     END-IF
      *
               IF ( RECE02-KOHNUM(KOHFTN-IDX) =  "015" OR "016"
                                                       OR "051" )
               OR ((SYS-1001-PREFNUM =   13           ) AND
                   (RECE02-KOHNUM(KOHFTN-IDX) =  "151" OR "451"))
                     MOVE  "1"  TO
                             RECE02-ZEROKISAIFLG(KOHFTN-IDX + 1)
               ELSE
                 IF ( RECE02-KOHNUM(KOHFTN-IDX) =  "012"   )
                 OR ((SYS-1001-PREFNUM          =   28    ) AND
                     (RECE02-KOHNUM(KOHFTN-IDX) =  "186"  ))
                   CONTINUE
                 ELSE
      *            保険番号テーブルの検索
                   INITIALIZE                      HKNNUM-REC
                   MOVE   RECE02-HOSPID      TO    HKN-HOSPID
                   MOVE   RECE02-KOHNUM(KOHFTN-IDX)
                                             TO    HKN-HKNNUM
                   MOVE   HKNNUM-REC         TO    MCPDATA-REC
                   MOVE   "DBSELECT"         TO    MCP-FUNC
                   MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
                   CALL   "ORCMCPSUB"        USING MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                   IF      MCP-RC            =     ZERO
                     MOVE    "HKNNUM-KEY2"   TO    ORC-DBPATH
                     PERFORM  900-HKNNUM-READ-SEC
                     PERFORM  UNTIL
                      ( FLG-HKNNUM         =   1               ) OR
                      ((HKN-TEKSTYMD(1:6) <= RECE02-SRYYM     ) AND
                       (RECE02-SRYYM      <= HKN-TEKEDYMD(1:6)))
                         MOVE     "HKNNUM-KEY2" TO    ORC-DBPATH
                         PERFORM   900-HKNNUM-READ-SEC
                     END-PERFORM
                   ELSE
                     MOVE     1              TO    FLG-HKNNUM
                   END-IF
      *
                   IF   FLG-HKNNUM  =  ZERO
                     IF   HKN-HON-GAIDJGNGAK    NOT = ZERO
                     OR   HKN-HON-GAIDJGNCNT    NOT = ZERO
                     OR   HKN-HON-GAIMNAIJGNGAK NOT = ZERO
                     OR   HKN-HON-GAIMGAIJGNGAK NOT = ZERO
                     OR   HKN-HON-GAIMJGNCNT    NOT = ZERO
                        MOVE  "1"  TO
                                RECE02-ZEROKISAIFLG(KOHFTN-IDX + 1)
                     END-IF
                   END-IF
      *
                 END-IF
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
           IF    RECE02-SRYYM        >=  "200504"
      *
             PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                         UNTIL    KOHFTN-IDX     >    4
      *
               IF   RECE02-KOHNUM(KOHFTN-IDX) =  "052"
                     MOVE  "1"  TO
                                 RECE02-KISAIFLG(KOHFTN-IDX + 1)
                     MOVE  "1"  TO
                             RECE02-ZEROKISAIFLG(KOHFTN-IDX + 1)
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
      *    一部負担金・食事療養費標準負担額区分の編集
      *
           EVALUATE    RECE02-TEISYOTOKU
                   WHEN    "1"
                       MOVE    "3"   TO   RECE02-SHOKUJIFTNKBN
                   WHEN    "2"
                       MOVE    "1"   TO   RECE02-SHOKUJIFTNKBN
           END-EVALUATE
      *
      *    在総診＋老人保険＋全国公費
           IF      RECE02-SRYYM      >=  "200404"
      *
             PERFORM    VARYING   KOHFTN-IDX   FROM   1   BY   1
                         UNTIL    KOHFTN-IDX     >    4
      *
               IF  ( RECE02-ZAITAKU              NOT =    SPACE )  AND
                   ( RECE02-RJNHKNNUM            NOT =    SPACE )  AND
                   ( RECE02-KOHNUM(KOHFTN-IDX)(1:1)  =    "0"   )
                  MOVE   FTN-KOH-XXX  TO
                                  RECE02-FTNMONEY-FTN(KOHFTN-IDX + 1)
               END-IF
      *
             END-PERFORM
      *
           END-IF
      *
      *    島根県乳幼児・福祉
      *
           EVALUATE   SYS-1001-PREFNUM
      *
             WHEN    "32"
      *
               IF   RECE02-SRYYM    >=   "200510"
      *
                   IF  ( RECE02-HKNNUM       NOT = SPACE    )
                   AND ( RECE02-KOHNUM(1)(1:1)   =  "0"     )
                   AND ( RECE02-KOHNUM(2)(1:1)
                                           >= "1" AND <= "8")
                   AND ( RECE02-TOTALTEN(1)  NOT =
                                          RECE02-TOTALTEN(2))
                      COMPUTE  RECE02-TOTALTEN(3) =
                               RECE02-TOTALTEN(1) -
                               RECE02-TOTALTEN(2)
                   END-IF
      *
               END-IF
      *
           END-EVALUATE
      *
           .
       2111-ICHIBUFUTAN-OKIKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担（金額）取込み処理
      *****************************************************************
       21111-KOHFTN-KINGAKU-SEC               SECTION.
      *
           MOVE      ZERO             TO  WRK-GAIJGNGAK
      *
      *    公費月上限額取得
           INITIALIZE                     LNK-ORCSJGNGET-REC
           MOVE   RECE02-HOSPID       TO  LNK-ORCSJGNGET-HOSPID
           MOVE   RECE02-NYUGAIKBN    TO  LNK-ORCSJGNGET-NYUGAIKBN
           MOVE   RECE02-PTID         TO  LNK-ORCSJGNGET-PTID
           MOVE   RECE02-KOHNUM(KOHFTN-IDX)
                                      TO  LNK-ORCSJGNGET-KOHHKNNUM
           MOVE   RECE02-KOHID(KOHFTN-IDX)
                                      TO  LNK-ORCSJGNGET-KOHID
           MOVE   RECE02-SRYYM        TO  LNK-ORCSJGNGET-SRYYM
      *
           CALL    "ORCSJGNGET"       USING
                                      LNK-ORCSJGNGET-REC
      *
           IF     LNK-ORCSJGNGET-RC   =   ZERO
             MOVE    LNK-ORCSJGNGET-GAIJGNGAK
                                      TO  WRK-GAIJGNGAK
      *
      ***    IF  ( RECE02-AGE            >=   70      )
      ***    OR  ( RECE02-AGE            >=   65  AND  
      ***          RECE02-RJNHKNNUM   NOT =   SPACE   )
      ***         IF    WRK-GAIJGNGAK    >   8000
      ***              MOVE    8000    TO   WRK-GAIJGNGAK
      ***         END-IF
      ***    END-IF
      *
           END-IF
      *
           .
       21111-KOHFTN-KINGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト請求区分による公費情報の振り分け処理
      *****************************************************************
       2112-RECESKYKBN-HENSYU-SEC      SECTION.
      *
      *    初期化
           MOVE    RECE02-REC           TO  WRK-RECE02-REC
           INITIALIZE                       RECE02-KOHINF-AREA
                                            RECE02-HKNCOMBIINF-AREA
                                            RECE02-JNISSUINF-AREA
                                            RECE02-TOTALTENINF-AREA
                                            RECE02-FTNMONEYINF-AREA
                                            RECE02-YKZFTNINF-AREA
                                            RECE02-ETC-KOH-G
                                            RECE02-HENTOTALTENINF-AREA
                                            RECE02-KISAIFLGINF-AREA
                                            RECE02-ZEROKISAIFLGINF-AREA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
               INITIALIZE              RECE02-TENSUINF-AREA (IDX)
           END-PERFORM
      *
      *    主保険情報のみ編集
           MOVE    WRK-RECE02-JNISSU   (1)
                                       TO  RECE02-JNISSU  (1)
           MOVE    WRK-RECE02-TOTALTEN (1)
                                       TO  RECE02-TOTALTEN(1)
           MOVE    WRK-RECE02-FTNMONEY (1)
                                       TO  RECE02-FTNMONEY(1)
           MOVE    WRK-RECE02-FTNMONEY-FTN (1)
                                       TO  RECE02-FTNMONEY-FTN(1)
           MOVE    WRK-RECE02-FTNMONEY-KOJ (1)
                                       TO  RECE02-FTNMONEY-KOJ(1)
           MOVE    WRK-RECE02-YKZFTN   (1)
                                       TO  RECE02-YKZFTN  (1)
           MOVE    WRK-RECE02-HENTOTALTEN (1)
                                       TO  RECE02-HENTOTALTEN (1)
           MOVE    WRK-RECE02-KISAIFLG (1)
                                       TO  RECE02-KISAIFLG (1)
           MOVE    WRK-RECE02-ZEROKISAIFLG (1)
                                       TO  RECE02-ZEROKISAIFLG (1)
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
               MOVE    WRK-RECE02-TENSU (IDX 1)
                                           TO  RECE02-TENSU (IDX 1)
           END-PERFORM
      *
      *    レセプト請求区分による公費情報の振り分け
      *    公費情報・実日数・請求点数・負担金額
      *    明細点数・合計点数（ぶん点・公費分離区分編集用）
           MOVE    ZERO                TO  IDY
                                           IDXX
                                           IDZX
           MOVE    1                   TO  IDX2  
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF      WRK-RECE02-KOHNUM (IDX) NOT =   SPACE
                   COMPUTE IDXX    =   IDX +   1
                   EVALUATE    WRK-RECE02-RECESKYKBN (IDX)
                       WHEN    SPACE
                           ADD     1       TO  IDY
                                               IDX2
                           MOVE    WRK-RECE02-KOHINF (IDX)
                                           TO  RECE02-KOHINF (IDY)
                           COMPUTE IDYX    =   IDY +   1
                           MOVE    WRK-RECE02-JNISSU   (IDXX)
                                           TO  RECE02-JNISSU  (IDYX)
                           MOVE    WRK-RECE02-TOTALTEN (IDXX)
                                           TO  RECE02-TOTALTEN(IDYX)
                           MOVE    WRK-RECE02-FTNMONEY (IDXX)
                                           TO  RECE02-FTNMONEY(IDYX)
                           MOVE    WRK-RECE02-FTNMONEY-FTN (IDXX)
                                           TO  RECE02-FTNMONEY-FTN(IDYX)
                           MOVE    WRK-RECE02-FTNMONEY-KOJ (IDXX)
                                           TO  RECE02-FTNMONEY-KOJ(IDYX)
                           MOVE    WRK-RECE02-YKZFTN   (IDXX)
                                           TO  RECE02-YKZFTN  (IDYX)
                           PERFORM VARYING IDX1    FROM    1   BY  1
                                   UNTIL   IDX1    >       50
                               MOVE    WRK-RECE02-TENSU (IDX1 IDXX)
                                           TO  RECE02-TENSU (IDX1 IDX2)
                           END-PERFORM
                           MOVE    WRK-RECE02-HENTOTALTEN (IDXX)
                                           TO  RECE02-HENTOTALTEN (IDX2)
                           MOVE    WRK-RECE02-KISAIFLG (IDXX)
                                           TO  RECE02-KISAIFLG (IDX2)
                           MOVE    WRK-RECE02-ZEROKISAIFLG (IDXX)
                                           TO  RECE02-ZEROKISAIFLG(IDX2)
                       WHEN    "4"
                           ADD     1       TO  IDZX
                           MOVE    WRK-RECE02-KOHNUM (IDX)
                                           TO  RECE02-ETC-KOHNUM (IDZX)
                           MOVE    WRK-RECE02-JNISSU   (IDXX)
                                           TO  RECE02-ETC-JNISSU (IDZX)
                           MOVE    WRK-RECE02-TOTALTEN (IDXX)
                                           TO RECE02-ETC-TOTALTEN(IDZX)
                           MOVE    WRK-RECE02-FTNMONEY (IDXX)
                                           TO RECE02-ETC-FTNMONEY(IDZX)
                           MOVE    WRK-RECE02-FTNMONEY-FTN (IDXX)
                                       TO RECE02-ETC-FTNMONEY-FTN(IDZX)
                           MOVE    WRK-RECE02-FTNMONEY-KOJ (IDXX)
                                       TO RECE02-ETC-FTNMONEY-KOJ(IDZX)
                           MOVE    WRK-RECE02-YKZFTN   (IDXX)
                                           TO  RECE02-ETC-YKZFTN (IDZX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    保険組合せ番号情報
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               IF     WRK-RECE02-HKNCOMBI (IDX)    NOT =   ZERO
                   MOVE    WRK-RECE02-HKNCOMBI (IDX)
                                           TO  RECE02-HKNCOMBI (IDX)
                   MOVE    WRK-RECE02-HKNNUMHC (IDX)
                                           TO  RECE02-HKNNUMHC (IDX)
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       4
                       IF     WRK-RECE02-KOHNUMHC (IDX IDY)
                                           NOT =   SPACE
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       4
                               IF      WRK-RECE02-KOHNUMHC (IDX IDY)   =
                                                   RECE02-KOHNUM (IDXX)
                                   MOVE    WRK-RECE02-KOHNUMHCINF
                                                         (IDX IDY)
                                                   TO
                                           RECE02-KOHNUMHCINF(IDX IDY)
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM                 
               END-IF
           END-PERFORM
      *
           .
       2112-RECESKYKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット１
      *****************************************************************
       2113-OCR1-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           OCR1-AREA
      *
      *保険者番号
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE02-HKNNUM          =   SPACE            
      *        ３種の場合
               INITIALIZE                           ORCSNUMAREA
               MOVE    RECE02-KOHFTNJANUM(SEIHIDX)
                                            TO      SNUM-INX
               CALL    "ORCSNUM"            USING   ORCSNUMAREA
               IF      SNUM-RC              =       ZERO
                       MOVE   SNUM-OUTNUM   TO      OCR1-HKNJANUM
               ELSE
                       MOVE   ZERO          TO      OCR1-HKNJANUM
               END-IF
           ELSE
               INITIALIZE                           ORCSNUMAREA
               MOVE    RECE02-HKNJANUM      TO      SNUM-INX
               CALL    "ORCSNUM"            USING   ORCSNUMAREA
               IF      SNUM-RC              =       ZERO
                       MOVE   SNUM-OUTNUM   TO      OCR1-HKNJANUM
               ELSE
                       MOVE   ZERO          TO      OCR1-HKNJANUM
               END-IF
           END-IF
      *医療機関コード
           MOVE   SYS-1001-HOSPCD       TO      OCR1-HOSPCD
      *請求点数
           MOVE    RECE02-TOTALTEN(IDX5)
                                        TO      OCR1-SKYTENSU
      *チェックディジット１
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-SKYTENSU        TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT1
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT1
           END-IF
      *薬剤一部負担金
           MOVE    RECE02-YKZFTN(IDX5)  TO      OCR1-YKZFTN
      *チェックディジット２
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-YKZFTN          TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT2
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT2
           END-IF
      *一部負担金
           MOVE    RECE02-FTNMONEY(IDX5)
                                        TO      OCR1-FTNMONEY
      *チェックディジット３
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-FTNMONEY        TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT3
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT3
           END-IF
      *チェックディジット４
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(1:35)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT4
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT4
           END-IF
      *診療実日数
           MOVE    RECE02-JNISSU(IDX5)  TO      OCR1-JNISSU
      *診療年月（和暦）
           INITIALIZE                           LNK-DAY1-AREA
           INITIALIZE                           STS-AREA-DAY
           MOVE    "11"                 TO      LNK-DAY1-IRAI
           MOVE    RECE02-SRYYM         TO      LNK-DAY1-YMD(1:6)
           MOVE    "01"                 TO      LNK-DAY1-YMD(7:2)
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY1-AREA
           IF      STS-DAY-RC1          =       ZERO
                   MOVE  LNK-DAY1-YMD-X(3:4)
                                        TO      OCR1-SRYYM
           ELSE
                   MOVE  ZERO           TO      OCR1-SRYYM
           END-IF
      *チェックディジット５
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(37:6)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT5
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT5
           END-IF
      *市町村番号
           IF      RECE02-RJNFTNJANUM   =       SPACE
               MOVE  ZERO               TO      OCR1-FTNJANUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-RJNFTNJANUM
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR1-FTNJANUM
               ELSE
                       MOVE   ZERO      TO      OCR1-FTNJANUM
               END-IF
           END-IF
      *受給者番号
           IF      RECE02-RJNJKYSNUM    =       SPACE
               MOVE  ZERO               TO      OCR1-JKYSNUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-RJNJKYSNUM
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR1-JKYSNUM
               ELSE
                       MOVE   ZERO      TO      OCR1-JKYSNUM
               END-IF
           END-IF
      *チェックディジット６
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(44:15)     TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT6
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT6
           END-IF
      *都道府県番号
           MOVE    SYS-1001-PREFNUM     TO      OCR1-PREFNUM
      *点数表
           MOVE    RECE02-RECESYUBETU(1:1)
                                        TO      OCR1-TENSUHYO
      *保険種別１
           MOVE    RECE02-RECESYUBETU(2:1)
                                        TO      OCR1-HKNSYUBETU1
      *保険種別２
           MOVE    RECE02-RECESYUBETU(3:1)
                                        TO      OCR1-HKNSYUBETU2
      *本人・家族・入外
           MOVE    RECE02-RECESYUBETU(4:1)
                                        TO      OCR1-HONKZKKBN
      *整理番号
           MOVE    1                    TO      OCR1-SEIRINO
      *チェックディジット７
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR1-AREA(60:7)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR1-CHKDGT7
           ELSE
                   MOVE  ZERO           TO      OCR1-CHKDGT7
           END-IF
      *
           MOVE    OCR1-AREA            TO      RECE02-OCR1           
      *
           .
       2113-OCR1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット２（Ｆ００２）
      *****************************************************************
       2114-OCR2-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           OCR2-AREA
      *
      *性別
           MOVE    RECE02-SEX           TO      OCR2-SEX
      *元号・生年
           INITIALIZE                           LNK-DAY1-AREA
           INITIALIZE                           STS-AREA-DAY
           MOVE    "11"                 TO      LNK-DAY1-IRAI
           MOVE    RECE02-BIRTHDAY      TO      LNK-DAY1-YMD
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                LNK-DAY1-AREA
           IF      STS-DAY-RC1          =       ZERO
                   MOVE  LNK-DAY1-YMD-X(2:1)
                                        TO      OCR2-GENGO
                   MOVE  LNK-DAY1-YMD-X(3:2)
                                        TO      OCR2-BIRTHYY
           ELSE
                   MOVE  ZERO           TO      OCR2-GENGO
                   MOVE  ZERO           TO      OCR2-BIRTHYY
           END-IF
      *チェックディジット１
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(1:4)       TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT1
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT1
           END-IF
      *記号・番号
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE02-HKNNUM          =   SPACE
      *        ３種の場合（８桁）
               IF      RECE02-KOHJKYSNUM(SEIHIDX) =       SPACE
                   MOVE  ZERO               TO      OCR2-KIGO
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE02-KOHJKYSNUM(SEIHIDX)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-KIGO
                   ELSE
                       MOVE   ZERO      TO      OCR2-KIGO
                   END-IF
               END-IF
               MOVE    ZERO                TO  OCR2-NUM
           ELSE
               MOVE    RECE02-KIGO         TO  WRK-ZENKAKU
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-NUM (1:IDY)     TO  OCR2-KIGO
      *
               MOVE    RECE02-NUM          TO  WRK-ZENKAKU
               PERFORM 800-NUM-ZENKAKU-HENKAN-SEC
               MOVE    WRK-NUM (1:IDY)     TO  OCR2-NUM
           END-IF
      *チェックディジット２
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(6:20)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT2
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT2
           END-IF
      *公費１負担者番号
           IF      RECE02-KOHFTNJANUM(IDA) =      SPACE
               MOVE  ZERO               TO      OCR2-FTNJANUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-KOHFTNJANUM(IDA)
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-FTNJANUM
               ELSE
                       MOVE   ZERO      TO      OCR2-FTNJANUM
               END-IF
           END-IF
      *公費１受給者番号
           IF      RECE02-KOHJKYSNUM(IDA) =       SPACE
               MOVE  ZERO               TO      OCR2-JKYSNUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-KOHJKYSNUM(IDA)
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-JKYSNUM
               ELSE
                       MOVE   ZERO      TO      OCR2-JKYSNUM
               END-IF
           END-IF
      *チェックディジット３
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(27:15)     TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT3
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT3
           END-IF
      *公費１診療実日数・請求点数
           IF      RECE02-JNISSU   (IDX5)  =   RECE02-JNISSU   (IDX6)
           AND     RECE02-TOTALTEN (IDX5)  =   RECE02-TOTALTEN (IDX6)
               MOVE    ZERO                 TO  OCR2-JNISSU
                                                OCR2-TENSU   
           ELSE    
               MOVE    RECE02-JNISSU  (IDX6)   TO  OCR2-JNISSU
               MOVE    RECE02-TOTALTEN(IDX6)   TO  OCR2-TENSU
           END-IF
      *チェックディジット４
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(45:7)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT4
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT4
           END-IF
      *公費１薬剤一部負担金
           MOVE    RECE02-YKZFTN(IDX6)  TO      OCR2-YKZFTN
      *チェックディジット５
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(53:5)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT5
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT5
           END-IF
      *公費１患者負担額
           MOVE    RECE02-FTNMONEY(IDX6)
                                        TO      OCR2-FTNMONEY
      *チェックディジット６
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(59:5)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT6
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT6
           END-IF
      *チェックディジット７
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR2-AREA(43:22)     TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT7
           ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT7
           END-IF
      *予備１
           MOVE    SPACE                TO      OCR2-YOBI
      *
           IF      OCR2-AREA-X          =   ALL "0"
               MOVE    SPACE                    TO  OCR2-AREA-X
           END-IF  
           MOVE    OCR2-AREA            TO      RECE02-OCR2
      *
           .
       2114-OCR2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット３（Ｆ００２）
      *****************************************************************
       2115-OCR3-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           OCR3-AREA
      *
      *公費２負担者番号
           IF      RECE02-KOHFTNJANUM(IDB) =      SPACE
               MOVE  ZERO               TO      OCR3-FTNJANUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-KOHFTNJANUM(IDB)
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-FTNJANUM
               ELSE
                       MOVE   ZERO      TO      OCR3-FTNJANUM
               END-IF
           END-IF
      *公費２受給者番号
           IF      RECE02-KOHJKYSNUM(IDB) =       SPACE
               MOVE  ZERO               TO      OCR3-JKYSNUM
           ELSE
               INITIALIZE                       ORCSNUMAREA
               MOVE    RECE02-KOHJKYSNUM(IDB)
                                        TO      SNUM-INX
               CALL    "ORCSNUM"        USING   ORCSNUMAREA
               IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-JKYSNUM
               ELSE
                       MOVE   ZERO      TO      OCR3-JKYSNUM
               END-IF
           END-IF
      *チェックディジット１
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(1:15)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT1
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT1
           END-IF
      *公費２診療実日数・請求点数
           IF      RECE02-JNISSU   (IDX6)  =   RECE02-JNISSU   (IDX7)
           AND     RECE02-TOTALTEN (IDX6)  =   RECE02-TOTALTEN (IDX7)
               MOVE    ZERO                 TO  OCR3-JNISSU
                                                OCR3-TENSU   
           ELSE    
               MOVE    RECE02-JNISSU  (IDX7)   TO  OCR3-JNISSU
               MOVE    RECE02-TOTALTEN(IDX7)   TO  OCR3-TENSU
           END-IF
      *チェックディジット２
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(19:7)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT2
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT2
           END-IF
      *公費２薬剤一部負担金
           MOVE    RECE02-YKZFTN(IDX7)  TO      OCR3-YKZFTN
      *チェックディジット３
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(27:5)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT3
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT3
           END-IF
      *公費２患者負担額
           MOVE    RECE02-FTNMONEY(IDX7)
                                        TO      OCR3-FTNMONEY
      *チェックディジット４
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(33:5)      TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT4
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT4
           END-IF
      *チェックディジット５
           INITIALIZE                           LNK-CHKDGT-AREA
           MOVE    1                    TO      LNK-I-SYORIKBN
           MOVE    OCR3-AREA(17:22)     TO      LNK-I-CODE
           CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
           IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR3-CHKDGT5
           ELSE
                   MOVE  ZERO           TO      OCR3-CHKDGT5
           END-IF
      *予備２
           MOVE    SPACE                TO      OCR3-YOBI
      *
           IF      OCR3-AREA-X          =   ALL "0"
               MOVE    SPACE                TO  RECE02-OCR3
           ELSE 
               MOVE    OCR3-AREA            TO  RECE02-OCR3
           END-IF
      *
           .
       2115-OCR3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角→数値半角変換処理
      *****************************************************************
       800-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE    ZERO                TO  WRK-STIDX
                                           WRK-EDIDX 
           MOVE    SPACE               TO  WRK-NUM
      *
      *    空白のときはゼロ 
           IF      WRK-ZENKAKU          =       SPACE
               MOVE    ZERO                TO  WRK-NUM
               MOVE    10                  TO  IDY   
           ELSE
      *
      *        文字の開始桁
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   80
                       OR      WRK-STIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-STIDX
                   END-IF     
               END-PERFORM
      *        文字の終了桁
               PERFORM VARYING IDX FROM    80  BY  -1
                       UNTIL   IDX         <   1
                       OR      WRK-EDIDX   >   ZERO
                   IF      WRK-ZENKAKU (IDX:1)  NOT =   SPACE
                       MOVE    IDX                 TO  WRK-EDIDX
                   END-IF     
               END-PERFORM
      *
      *        全角数字のとき半角数字に変換（数字以外のときはゼロ）
               IF      WRK-ZENKAKU (WRK-STIDX:1)   >=  X"A1"
                   MOVE    ZERO                TO  IDX3
                   PERFORM VARYING     IDX     FROM    WRK-STIDX  BY  2
                           UNTIL       IDX     >       WRK-EDIDX
                       IF      WRK-ZENKAKU (IDX:2)  >= "０"  AND
                                                   <=  "９"
                           ADD     1               TO  IDX3
                           EVALUATE    WRK-ZENKAKU (IDX:2)
                               WHEN    "０"
                                   MOVE    "0"     TO  WRK-NUM (IDX3:1)
                               WHEN    "１"
                                   MOVE    "1"     TO  WRK-NUM (IDX3:1)
                               WHEN    "２"
                                   MOVE    "2"     TO  WRK-NUM (IDX3:1)
                               WHEN    "３"
                                   MOVE    "3"     TO  WRK-NUM (IDX3:1)
                               WHEN    "４"
                                   MOVE    "4"     TO  WRK-NUM (IDX3:1)
                               WHEN    "５"
                                   MOVE    "5"     TO  WRK-NUM (IDX3:1)
                               WHEN    "６"
                                   MOVE    "6"     TO  WRK-NUM (IDX3:1)
                               WHEN    "７"
                                   MOVE    "7"     TO  WRK-NUM (IDX3:1)
                               WHEN    "８"
                                   MOVE    "8"     TO  WRK-NUM (IDX3:1)
                               WHEN    "９"
                                   MOVE    "9"     TO  WRK-NUM (IDX3:1)
                           END-EVALUATE
                       ELSE
                           MOVE    ZERO            TO  WRK-NUM
                           MOVE    WRK-EDIDX       TO  IDX
                           MOVE    10              TO  IDX3
                       END-IF
                   END-PERFORM
                   MOVE    IDX3                TO  IDY     
               ELSE
      *        半角数字に変換（数字以外のときはゼロ）
                   INSPECT WRK-ZENKAKU(WRK-STIDX:)  REPLACING   ALL " "
                                                                BY  "0"
                   IF      WRK-ZENKAKU(WRK-STIDX:)      NOT NUMERIC
                       MOVE    ZERO                TO  WRK-NUM
                       MOVE    10                  TO  IDY   
                   ELSE
                       COMPUTE IDY     =   WRK-EDIDX   -
                                           WRK-STIDX   +   1
                       MOVE    WRK-ZENKAKU (WRK-STIDX:IDY) TO  WRK-NUM
                   END-IF     
               END-IF
           END-IF
      *
           IF      IDY                 >   10
               MOVE    1                   TO  IDX
           ELSE
               COMPUTE IDX     =   11      -   IDY
           END-IF
           MOVE    ZERO                TO  WRK-NUM-X
           MOVE    WRK-NUM (1:IDY)     TO  WRK-NUM-X (IDX:)
           MOVE    SPACE               TO  WRK-NUM
           MOVE    WRK-NUM-X           TO  WRK-NUM (1:10)
           MOVE    10                  TO  IDY
       
      *
           .
       800-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           MOVE    ZERO                 TO      FLG-TENSU
           MOVE    SRY-SRYCD(IDX2)      TO      TTNS-CHK-SRYCD
           MOVE    RECE05-SRYYM         TO      WK-TAIHI-SRYYM
           MOVE    WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
           MOVE    WK-TAIHI-SRYYMD      TO      TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                           TNS-TENSU-REC
               MOVE    SRY-SRYCD(IDX2)      TO      TNS-SRYCD
               MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
               MOVE    RECE05-SRYYM         TO      WK-TAIHI-SRYYM
               MOVE    WK-TAIHI-DAY         TO      WK-TAIHI-SRYD
               MOVE    WK-TAIHI-SRYYMD      TO      ORC-DBYMD
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "TENSU-KEY"          TO      ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "TENSU-KEY"      TO      ORC-DBPATH
                   PERFORM  910-TENSU-READ-SEC
                   IF       FLG-TENSU       =       1
                       DISPLAY   "TENSU FETCH ERR"  
      *                ACCEPT     WRK-IN FROM CONSOLE
                   ELSE
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   END-IF
               ELSE
                   DISPLAY   "TENSU SELECT ERR" 
      *            ACCEPT     WRK-IN FROM CONSOLE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE   "TENSU-KEY"      TO  WK-PATH
               PERFORM   900-CSR-CLOSE-SEC
           END-IF
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF      TNS-TANIMOJISU    =   ZERO
           AND     TNS-TANINAME  NOT =   SPACE
               PERFORM   VARYING   IDW2   FROM   24   BY  -1
                          UNTIL    IDW2    <      1
                  IF   TNS-TANINAME(IDW2:1)  NOT =   SPACE
                      COMPUTE  TNS-TANIMOJISU   =   IDW2   /   2
                      MOVE    1     TO    IDW2
                  END-IF
               END-PERFORM
           END-IF
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       910-TENSU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       900-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       900-SYUNOU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納合計マスタ読込
      *****************************************************************
       900-SYUTOTAL-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUTTL-REC
               MOVE    ZERO            TO  FLG-SYUTOTAL
           ELSE
               INITIALIZE                  SYUTTL-REC
               MOVE    1               TO  FLG-SYUTOTAL
           END-IF
      *
           .
      *
       900-SYUTOTAL-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号読込
      *****************************************************************
       900-HKNNUM-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       900-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1               TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                       TO  FLG-HKNCOMBI
               INITIALIZE                          HKNCOMBI-REC
           END-IF
      *
           .
       900-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      * 
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               MOVE    "DBFETCH"       TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       970-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       970-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "PTCOM-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE02-READ-SEC            SECTION.
      *
           READ    RECE02-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE02
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
                   ADD     1     TO    CNT-IN-COMMIT
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE02 " IDZ " PTNUM=" RECE02-PTNUM     
      *
                   IF      WRK-PARA-SRYYM     =  ZERO
      *                 医療機関編集
                       MOVE  RECE02-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF
           END-READ
           .
       RECE02-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  10   )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1              TO  FLG-RECE02
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    診療行為機能情報
           MOVE    ZERO                TO  FLG-SET
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  10   )
                   OR    ( TBL-SYS1038(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE  TBL-SYS1038(SYSKAN-IDX)   TO  SYS-1038-REC
               IF   ( SYS-1038-STYUKYMD        <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD           <=  SYS-1038-EDYUKYMD )
                   MOVE    1                   TO  FLG-SET
               END-IF
           END-PERFORM
      *
           IF     FLG-SET          =   ZERO
              INITIALIZE                   SYS-1038-REC
              MOVE    "0"              TO  SYS-1038-KAGRPFLG
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       RECE02-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE02-REC
      *
           ADD     1                   TO  CNT-OUT3 (IDZ)
      *
           .
       RECE02-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００４）
      *****************************************************************
       RECE04-WRITE-SEC                SECTION.
      *
           IF      IDZ             =   1               
      *    レセプト処理
               WRITE   RECE04-REC
      *
               ADD     1                   TO  CNT-OUT1
           END-IF
      *
           .
       RECE04-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE05-WRITE-SEC                SECTION.
      *
           IF      IDZ             =   1               
      *    レセプト処理
               WRITE   RECE05-REC
      *
               ADD     1                   TO  CNT-OUT2
           END-IF 
      *
           .
       RECE05-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理（Ｆ００５）
      *****************************************************************
       RECE05-REWRITE-SEC              SECTION.                        
      *
           IF      IDZ             =   1               
      *    レセプト処理
      *
              MOVE    ZERO                TO  FLG-REWRITE
      *
              INITIALIZE                      RECE05-REC
              MOVE    RECE04-HOSPID       TO  RECE05-HOSPID
              MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
              MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
              MOVE    RECE04-PTID         TO  RECE05-PTID
              MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
              MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
              MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
              MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
              MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
              MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
              MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
              START   RECE05-FILE
                    KEY IS    >=   RECE05-KEY
                    INVALID
                        MOVE    1         TO  FLG-RECE05
                    NOT INVALID
                        MOVE    ZERO      TO  FLG-RECE05
                        READ    RECE05-FILE   NEXT
                          AT  END
                            MOVE    1     TO  FLG-RECE05
                        END-READ
              END-START
              PERFORM   UNTIL
                  (RECE04-HOSPID       NOT =  RECE05-HOSPID)      OR
                  (RECE04-SRYYM        NOT =  RECE05-SRYYM)       OR
                  (RECE04-NYUGAIKBN    NOT =  RECE05-NYUGAIKBN)   OR
                  (RECE04-PTID         NOT =  RECE05-PTID)        OR
                  (RECE04-RECEKA       NOT =  RECE05-RECEKA)      OR
                  (RECE04-TEISYUTUSAKI NOT =  RECE05-TEISYUTUSAKI) OR
                  (RECE04-RECESYUBETU  NOT =  RECE05-RECESYUBETU)  OR
                  (RECE04-HKNJANUM     NOT =  RECE05-HKNJANUM)    OR
                  (RECE04-HOJOKBN      NOT =  RECE05-HOJOKBN )    OR
                  (RECE04-TEKSTYMD     NOT =  RECE05-TEKSTYMD)    OR
                  (RECE04-ZAINUM       NOT =  RECE05-ZAINUM)      OR
                  (FLG-RECE05                =      1        )    OR
                  (FLG-REWRITE               =      1        )
                   IF    RECE05-RENNUM   =   WRK-RENNUM
                      MOVE    1              TO  FLG-REWRITE
                      MOVE    WRK-SRYSURYO3  TO  RECE05-SRYSURYO(5)
                      REWRITE   RECE05-REC
                   END-IF
                   READ    RECE05-FILE   NEXT
                     AT  END
                       MOVE    1     TO  FLG-RECE05
                   END-READ
              END-PERFORM
      *
           END-IF 
      *
           .
       RECE05-REWRITE-EXT.
           EXIT.                                                        
      *
      ****************************************************************
      *    レセプト０９データ読み
      ****************************************************************
       RECE09-NEXT-SEC           SECTION.
      *
           READ    RECE09-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE09
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE09
           END-READ
      *
           .
       RECE09-NEXT-EXT.
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
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
      *            ジョブ管理終了処理
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "RECE02  CNT-IN  :" CNT-IN  (1) 
           DISPLAY "RECE04  CNT-OUT :" CNT-OUT1  
           DISPLAY "RECE05  CNT-OUT :" CNT-OUT2  
           DISPLAY "RECE02X CNT-IN  :" CNT-IN  (2) 
           DISPLAY "*** ORCR0030 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       900-CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-CSR-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "MCPSUB"          USING
                                        MCPAREA
                                        MCPDATA-REC.
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
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC.
           .
       900-DBSTART-EXT.
           EXIT.
      *
