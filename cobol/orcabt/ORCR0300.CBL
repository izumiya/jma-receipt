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
       PROGRAM-ID.             ORCR0300.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト電算ファイル作成（ＣＳＶ形式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/18    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03/07/18  主病名編集フラグによって
      *                                     主傷病名コードを変更する
      *  01.00.02    NACL-藤原    03/08/01  コメントの編集の訂正
      *  01.00.03    NACL-藤原    03/08/06  用法は出力しないようにする
      *                                     証明書番号の省略値の変更
      *                                     画像診断の点数の変更
      *                                     薬剤・器材の剤点数が０のとき
      *                                     摘要欄に印刷しないように修正
      *  01.00.04    NACL-伊藤    03/08/13  指導料・在宅料摘要欄編集
      *                                     加算点数でも告示区分１により
      *                                     基本点数剤とする
      *  01.00.05    NACL-藤原    03/08/17  廃止年月日の取扱
      *  01.00.06    NACL-藤原    03/08/20  レセプト番号の編集を止める
      *  01.00.07    NACL-藤原    03/08/22  画像診断の乳幼児加算の点数の修正
      *                                     薬剤料逓減を出力する
      *                                     未コード化傷病名コードのとき
      *                                     修飾語コードの編集をしない
      *  01.00.08    NACL-多々納  03/08/25  コメントの値を全角編集へ修正
      *  01.00.09    NACL-藤原    03/09/03  公費の負担者番号・受給者番号
      *                                     の全角チェック追加
      *  01.00.10    NACL-藤原    03/09/22  再診のときの時間外の編集
      *  01.00.11    NACL-藤原    03/09/30  酸素の単価の編集
      *  01.00.12    NACL-藤原    03/10/10  コメントの名称編集の修正
      *  01.00.13    NACL-藤原    03/10/15  診療年月の編集の修正
      *  01.00.14    NACL-藤原    03/10/22  摘要情報の編集の修正
      *  01.00.15    NACL-藤原    03/10/27  テスト患者を対象としない
      *  01.00.16    NACL-藤原    03/11/12  時間加算の追加
      *  01.00.17    NACL-藤原    03/11/17  傷病名データファイル名の変更
      *  01.00.18    NACL-藤原    03/11/19  老人外来金額の項目変更
      *  01.00.19    NACL-藤原    03/11/20  給付割合の編集の変更
      *                                     その他材料の単価の編集
      *                                     老人減免区分コードの編集
      *                                     医薬品・特定器材の出力順
      *  01.00.20    NACL-藤原    03/11/26  生体検査の加算について
      *  01.00.21    NACL-藤原    03/12/11  診療識別が指導（１３）
      *                                     在宅（１４）のときコメントは
      *                                     最後に記載する
      *  01.00.22    NACL-藤原    03/12/17  特定器材の単位コード編集
      *  01.00.23    NACL-藤原    03/12/19  病床数を一般病床数から編集
      *                                     山形の地方公費対応（国保）
      *  01.00.24    NACL-藤原    04/01/19  病床数の記載の修正
      *  01.00.25    NACL-藤原    04/01/20  老人減免区分コードの修正
      *  01.00.26    NACL-藤原    04/02/01  病名コード２１に変更
      *  01.00.27    NACL-藤原    04/02/05  特記事項のカスタマイズ項目
      *                                     の編集
      *  01.00.28    NACL-藤原    04/03/02  出力対象のコメントを追加
      *  01.00.29    NACL-藤原    04/03/18  画像診断の出力順の修正
      *  01.00.30    NACL-藤原    04/03/19  ５１公費のとき実施機関番号「６０２」
      *                                     のときのみ公費負担額を記録する
      *  01.00.31    NACL-藤原    04/04/05  病名コード９９９９９９９読み飛ばす
      *  01.00.32    NACL-藤原    04/04/06  保険履歴に保険者番号を追加
      *
      *  ２００４年４月仕様変更
      *  01.01.01    NACL-藤原    04/04/06  手術歴の記載
      *                                     診療科コード（レセプト共通）・
      *                                     商品名及び規格名及びサイズ
      *                                     （特定器材）の追加
      *  01.01.02    NACL-藤原    04/04/22  画像診断の加算が複数ある場合の
      *                                     出力の修正
      *  01.01.03    NACL-藤原    04/04/26  乳幼児の時間加算の追加
      *  01.01.04    NACL-藤原    04/04/27  ユーザーコメントの追加
      *  01.01.05    NACL-藤原    04/05/11  診療識別が画像診断（７０）の
      *                                     ときコメントは先頭に記載する
      *  01.01.06    NACL-藤原    03/08/13  指導料・在宅料摘要欄編集
      *                                     加算点数でも告示区分１により
      *                                     剤回数を編集する
      *  01.01.07    NACL-小豆沢  04/05/19  ＤＢ書き込み時のコミット追加
      *  01.01.08    NACL-藤原    04/05/21  指導料・在宅料のとき
      *                                     加算点数で告示区分１が７の
      *                                     ときの点数・回数の編集
      *  01.01.09    NACL-太田    04/05/21  処理高速化対応（保険組合せ）
      *  01.01.10    NACL-太田    04/05/25  処理高速化対応（点数マスタ）
      *
      *  01.02.01    NACL-藤原    04/05/26  RECE05レイアウト変更
      *                                     告示等識別区分１
      *                                     きざみ値計算識別
      *                                     注加算コードの追加
      *  01.02.02    NACL-藤原    04/06/17  症状詳記をデータにも編集する
      *                                     01:保険履歴、手術歴
      *                                     99:レセコメント
      *  01.02.03    NACL-藤原    04/08/05  神奈川対応
      *                                     地方公費のとき、負担者番号・
      *                                     受給者番号が空白のとき公費
      *                                     レコードを出力しない
      *  01.02.04    NACL-藤原    04/08/27  一部負担金・食事療養費標準負
      *                                     担額区分の編集を追加
      *  01.02.05    NACL-藤原    04/09/29  福岡対応
      *                                     地方公費のとき、４１老人以外
      *                                     はレセ電データを作成しない
      *  01.02.06    NACL-藤原    04/10/18  氏名欄編集方法の変更
      *                                     職務上の事由の編集条件に
      *                                     共済組合の下船３ヶ月を追加
      *                                     医療保険一部負担金額の編集追加 
      *                                     パラメタの追加
      *  01.02.07    NACL-藤原    04/11/16  特定器材レコードのフィルムと
      *                                     器材の点数・回数を別々に記載
      *  01.02.08    NACL-太田    04/12/08  点数フラグの初期化追加
      *  01.02.09    NACL-藤原    04/12/20  職務上の事由の編集方法の修正
      *  01.02.10    NACL-藤原    05/01/17  愛知県対応
      *                                     地方公費のとき、法別番号を判
      *                                     定して受給者番号にゼロを編集
      *                                     する
      *  01.02.11    NACL-藤原    05/01/18  特定器材レコードの点数の編集
      *                                     方法を修正
      *  01.02.12    NACL-藤原    05/01/25  酸素の計算式を対象としないよ
      *                                     うにする
      *  01.02.13    NACL-小豆沢  05/02/10  負担区分の判定に公費ＩＤを追加
      *  01.02.14    NACL-藤原    05/02/23  注加算による加算項目の記載
      *                                     （手術・麻酔・検査・処置・その他）
      *                                     加算項目であるが基本項目となって
      *                                     いる項目がある場合の記載順
      *                                     超音波内視鏡加算を逓減の後に記載
      *  01.02.15    NACL-藤原    05/02/05  公費の負担者番号・受給者番号
      *                                     のカスタマイズ項目の編集
      *  01.02.16    NACL-藤原    05/03/18  レセプト特記事項編集情報によ
      *                                     るレセプト編集
      *                                     公費の負担者番号・受給者番号
      *                                     のカスタマイズ項目を中止
      *  01.02.17    NACL-藤原    05/04/04  コメント編集行を変更
      *  01.02.18    NACL-藤原    05/04/04   01.02.03の神奈川対応
      *                                     レセプト特記事項編集情報対応
      *                                     により中止する
      *  01.02.19    NACL-小豆沢  05/04/20  手術の５０％併施加算を告示識別
      *                                     区分を手技の扱いに置き換え
      *  01.02.20    NACL-藤原    05/05/09  改定通信医１６０３９号
      *                                     「主」の記録についてより
      *                                    「0000998」を「0000999」に変更
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *
      *    レセプト明細書（病名）  
           SELECT  RECE03-FILE     ASSIGN  RECE03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *    レセプト０４データ
           SELECT  RECE041-FILE     ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE041-KEY
                                   FILE    STATUS  IS  STS-RECE04.
      *    レセプト０５データ
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
      *
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    レセプト（県単用項目）データ
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
      *    レセプト明細書（病名）  
       FD  RECE03-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
           03  RECE031-SYUBYOFLG-X PIC X(01).
      *
      *    レセプト０４データ
       FD  RECE041-FILE.
       01  RECE041-REC.
           COPY    "CPRCF0041.INC".
      *
      *    レセプト０５データ
       FD  RECE05-FILE.
       01  RECE05-REC.
           COPY    "CPRCF005.INC".
      *
      *    レセプト０９データ
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
      *    レセプト（県単用項目）データ
       FD  RECE11-FILE.
       01  RECE11-REC. 
           COPY    "CPRCF011.INC".
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE02//.
      *
      *     レセプト明細ファイル（病名） 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE03//.
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
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE11//.
      *
      *     レセプト電算ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE30//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03                      PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE30              PIC X(02).
           03  STS-RECE02              PIC X(02).
           03  STS-RECE03              PIC X(02).
           03  STS-RECE04              PIC X(02).
           03  STS-RECE05              PIC X(02).
           03  STS-RECE09              PIC X(02).
           03  STS-RECE11              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTCOM               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01). 
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
           03  FLG-RECEDEN             PIC 9(01).
           03  FLG-RECE30              PIC 9(01).
           03  FLG-RECE02              PIC 9(01).
           03  FLG-RECE03              PIC 9(01).
           03  FLG-RECE04              PIC 9(01).
           03  FLG-RECE05              PIC 9(01).
           03  FLG-RECE09              PIC 9(01).
           03  FLG-RECE11              PIC 9(01).
           03  FLG-OPECOM              PIC 9(01).
      *
           03  FLG-READ                PIC 9(01).
      *
           03  FLG-TEIGEN              PIC 9(01).
           03  FLG-YKZ20               PIC 9(01).
           03  FLG-TOTALTEN            PIC 9(01).
           03  FLG-KOKUJISKBKBN1       PIC 9(03).
           03  FLG-GAITENTTLKBN        PIC 9(03).
           03  FLG-TBLMAX              PIC 9(01).
           03  FLG-SRYSYUKBN721        PIC 9(01).
           03  FLG-SRYKBN70-TEIGEN     PIC 9(01).
           03  FLG-KENSA-KASAN         PIC 9(01).
           03  FLG-KENSA-TEIGEN        PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-OPECOM-CHK          PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-KOUI                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECE04              PIC 9(06).
           03  CNT-RECE09              PIC 9(06).
           03  CNT-COMMIT              PIC 9(03). 
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(03). 
           03  IDY                     PIC 9(03). 
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(03).
           03  IDY2                    PIC 9(03).
           03  IDY3                    PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDXY                    PIC 9(03).
           03  IDXZ                    PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(03).
      *
           03  SYSKAN-IDX              PIC 9(03).
           03  IDX0                    PIC 9(03).
      *
       01  KEY-AREA                                VALUE   LOW-VALUE.
           03  KEY-NEW.
      *****    05  KEY-N-TEISYUTUSAKI  PIC 9(01).
               05  KEY-N-SRYKA         PIC X(02).
           03  KEY-OLD.
      *****    05  KEY-O-TEISYUTUSAKI  PIC 9(01).
               05  KEY-O-SRYKA         PIC X(02).      
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKENSU         PIC 9(06).
           03  SUM-TOTALTEN         PIC 9(10).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  10.
               05  TBL-SYS1001         PIC  X(610).
     *
      *    一時領域
       01  WRK-AREA.
      *    パラメタエリア
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-OPECOM-PATH         PIC X(64).
      *
           03  WRK-KIZAI-SRYCD         PIC X(09).
      *     右ずめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(200).
           03  WRK-REC-LENGTH          PIC 9(03).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(80).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(03).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    傷病名レコード編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEICD        PIC X(07).
               05  WRK-RECECD-G.
                   07  WRK-RECECD      PIC X(04)   OCCURS  20.                            
      *    マルチボリューム識別情報     
           03  WRK-VOLNUM              PIC 9(06).
      *    給付割合 
           03  WRK-KYURATE             PIC 9(03).
      *    レセプト番号
           03  WRK-RECENUM             PIC 9(06).        
      *       
           03  WRK-RENNUM              PIC 9(04).
           03  WRK-COM-RENNUM          PIC 9(04).
      *
           03  WRK-RECE041-SRYKBN      PIC X(02).
           03  WRK-COM-FTNKBN          PIC X(01).
      *
           03  WRK-CHUKSNCD            PIC X(04).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU            PIC X(03).
      *    生保 
           03  SEHIDX                  PIC 9(01).
      *    精神 
           03  SESIDX                  PIC 9(01).
      *    結核 
           03  KEKIDX                  PIC 9(01).
      *
           03  IDX1                    PIC 9(01).     
           03  IDX2                    PIC 9(01).     
           03  IDX3                    PIC 9(01).
      *
      *    摘要レコード編集用
           03  WRK-ZAIWMAX             PIC 9(03).
           03  WRK-TBLMAX1             PIC 9(03).
           03  WRK-TBLMAX6             PIC 9(03).
           03  WRK-TBLMAX7             PIC 9(03).
           03  WRK-TBLMAX8             PIC 9(03).
           03  WRK-SURYO               PIC 9(08).
           03  WRK-TENSU               PIC 9(07).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-TANKA               PIC 9(07)V9(02).
           03  WRK-SRYSURYO            PIC 9(05)V9(03).
           03  WRK-FTNKBN-AREA.                            
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  3.
               05  WRK-SEHFLG          PIC 9(01).
           03  WRK-SRYCD1-MAX          PIC 9(03).           
           03  WRK-SRYCD71-MAX         PIC 9(03).           
           03  WRK-SRYCD72-MAX         PIC 9(03).           
           03  WRK-KOKUJISKBKBN1-KOUI-TEN
                                       PIC 9(09)V9(02).     
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  FILLER              PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *                 
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  50.  
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(12).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(02).                
           03  WRK-HKN-RIREKI-X                PIC X(60).
           03  WRK-HKN-HKNJANUM                PIC X(16).
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).    
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).   
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  8.
           03  WRK-COMM                        PIC X(400).
           03  WRK-COMM1                       PIC X(400).
           03  WRK-COL                         PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
      *
      *    手術歴編集
           03  WRK-OPECOM-HEN      PIC X(50).
      *
      *    医薬品・特定器材用テーブル
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-TABLE          OCCURS  2.
               05  TBL-ZAIW-REC        OCCURS 100.
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
      *                診療種別区分
                       09  TBL-ZAIW-SRYSYUKBN       PIC  X(03).
      *                数量
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(03).
      *                回数
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード
                       09  TBL-ZAIW-TANICD          PIC  9(03).
      *                単位名称有効桁数
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
      *                単位名称
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
      *                点数
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
      *                明細請求フラグ
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
      *                名称入力番号
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
      *                点数欄集計先識別
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *                データ区分
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
      *                包括対象検査
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *                検査等実施判断
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
      *                各種番号等
                       09  TBL-ZAIW-CDKBNINF.
                           11  TBL-ZAIW-CDKBN-KBN   PIC  X(01).
                           11  TBL-ZAIW-CDKBN-SYO   PIC  9(01).
                           11  TBL-ZAIW-CDKBN-BU    PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                           11  TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                           11  TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
      *                告示等識別区分１
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
      *                きざみ値計算識別
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
      *                注加算コード
                       09  TBL-ZAIW-CHUKSNINF.
      *                    コード
                           11  TBL-ZAIW-CHUKSNCD    PIC  X(04).
      *                    通番
                           11  TBL-ZAIW-CHUKSNTUBAN PIC  X(01).
      *                入力コメント
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(80).
      *            名称情報
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数
                       09  TBL-ZAIW-YUKOKETA        PIC  9(02).
      *                漢字名称
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *            データ識別(１：フィルム ２：特定機材)
                   07  TBL-ZAIW-DATSKB              PIC 9(01).
      *
      *    診療行為・コメント用テーブル
       01  TBL-KOUI-AREA.
           03  TBL-KOUI-REC                     OCCURS  100.
               05  TBL-KOUI-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KOUI-SRYSURYO        PIC  9(05)V9(03).
      *            回数
                   07  TBL-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KOUI-KNSJISKBN       PIC  9(01).
      *            各種番号等
                   07  TBL-KOUI-CDKBNINF.
                       09  TBL-KOUI-CDKBN-KBN   PIC  X(01).
                       09  TBL-KOUI-CDKBN-SYO   PIC  9(01).
                       09  TBL-KOUI-CDKBN-BU    PIC  9(02).
                       09  TBL-KOUI-CDKBN-KBNNUM     PIC  9(03).
                       09  TBL-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  TBL-KOUI-CDKBN-KOUBAN     PIC  9(02).
                       09  TBL-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                   07  TBL-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KOUI-CHUKSNINF.
      *                コード
                       09  TBL-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KOUI-CHUKSNTUBAN PIC  X(01).
      *            入力コメント
                   07  TBL-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
               05  TBL-KOUI-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KOUI-YUKOKETA        PIC  9(02).
      *            漢字名称
                   07  TBL-KOUI-NAME            PIC  X(200).
      *
      *    コメントデータ用テーブル
       01  TBL-COMMENT-AREA.
           03  TBL-COMMENT-REC                  OCCURS  100.
               05  TBL-COMMENT-SRYCD            PIC  X(09).
               05  TBL-COMMENT-NAME             PIC  X(200).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-TAIHI-AREA.
           03  WRK-KOUI-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-TAIHI-REC           PIC X(500).
      *        診療コード
               05  WRK-KOUI-TAIHI-SRYCD         PIC  X(09).
      *        告示等識別区分１
               05  WRK-KOUI-TAIHI-KOKUJISKBKBN1 PIC  9(01).
      *        注加算コード
               05  WRK-KOUI-TAIHI-CHUKSNCD      PIC  X(04).
      *        点数識別区分
               05  WRK-KOUI-TAIHI-TENSIKIBETU   PIC  9(01).
      *
       01  WRK-KOUI-AREA.
           03  WRK-KOUI-TABLE                   OCCURS  4.
               05  WRK-KOUI-TBLMAX              PIC 9(03).
               05  WRK-KOUI-REC                     OCCURS  100.
                   07  WRK-KOUI-SRYACTINF.
      *            診療コード
                       09  WRK-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                       09  WRK-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                       09  WRK-KOUI-SRYSURYO        PIC  9(05)V9(03).
      *            回数
                       09  WRK-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                       09  WRK-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                       09  WRK-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                       09  WRK-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                       09  WRK-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                       09  WRK-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                       09  WRK-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                       09  WRK-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                       09  WRK-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                       09  WRK-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                       09  WRK-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                       09  WRK-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                       09  WRK-KOUI-KNSJISKBN       PIC  9(01).
      *            各種番号等
                       09  WRK-KOUI-CDKBNINF.
                           11  WRK-KOUI-CDKBN-KBN   PIC  X(01).
                           11  WRK-KOUI-CDKBN-SYO   PIC  9(01).
                           11  WRK-KOUI-CDKBN-BU    PIC  9(02).
                           11  WRK-KOUI-CDKBN-KBNNUM     PIC  9(03).
                           11  WRK-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  WRK-KOUI-CDKBN-KOUBAN     PIC  9(02).
                           11  WRK-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                       09  WRK-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            きざみ値計算識別
                       09  WRK-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                       09  WRK-KOUI-CHUKSNINF.
      *                コード
                           11  WRK-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                           11  WRK-KOUI-CHUKSNTUBAN PIC  X(01).
      *            入力コメント
                       09  WRK-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
                   07  WRK-KOUI-NAMEINF.
      *            漢字有効桁数
                       09  WRK-KOUI-YUKOKETA        PIC  9(02).
      *            漢字名称
                       09  WRK-KOUI-NAME            PIC  X(200).
      *
      *    並び替え用ワーク
       01  TBL-KOUI-SORT                        PIC X(500).
      *
      *    診療行為データ用テーブル（再診 時間外）
       01  TBL-KOUI-SRYKBN12-AREA.
           03  TBL-KOUI-SRYKBN12-REC                     OCCURS  3.
               05  TBL-KOUI-SRYKBN12-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-SRYCD  PIC  X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-KAISU  PIC  9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-TEN    PIC  9(09)V9(02).
      *
       01  WRK-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-RECE05//.
      *    画像診断用退避エリア
       01  WRK-721-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-721-RECE05//.
       01  WRK-700-RECE05-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE05-REC                        OCCURS  5.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-700-RECE05//.
       01  WRK-SRYKBN70-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-SRYKBN70-RECE05//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-KASAN-RECE05//.
      *    生体検査・逓減用退避エリア
       01  WRK-TEIGEN-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-TEIGEN-RECE05//.
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *    退職者保険番号
      *        組合
           03  WRK-CONS-HKNNUM1       PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-CONS-HKNNUM2       PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-CONS-HKNNUM3       PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-CONS-HKNNUM4       PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-CONS-HKNNUM5       PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-CONS-HKNNUM6       PIC X(03)   VALUE  "067".
      *    薬剤料逓減
           03  WRK-CONS-TEIGEN        PIC X(09)   VALUE   "630010002".
      *
      *    再診の時間加算
           03  WRK-CONS-JIKANGAI-TABLE. 
      *    一般
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112001110".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112001210".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112001310".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112001410".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006670".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006770".
      *    老人
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112701010".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112701110".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112701210".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112701310".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701710".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701810".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701910".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112702010".
      *    乳幼児
      *        夜間（再診）
               05  FILLER             PIC X(09)    VALUE   "112014170".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112014270".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112014370".
      *        夜間（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014670".
      *
           03  WRK-CONS-JIKANGAI-TABLE-R  REDEFINES   
                                      WRK-CONS-JIKANGAI-TABLE.
               05  WRK-CONS-JIKANGAI-SRYCD-G       OCCURS  22.
                   07  WRK-CONS-JIKANGAI-SRYCD
                                      PIC X(09).
           03  WRK-CONS-JIKANGAI-TEN-TABLE.
               05  WRK-CONS-JIKANGAI-TEN-G         OCCURS  22.
                   07  WRK-CONS-JIKANGAI-TEN
                                      PIC 9(09).
           03  WRK-CONS-JIKANGAI-MAX  PIC 9(02)    VALUE   22.    
      *
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）
           03  WRK-CONS-SANSO1        PIC X(09)   VALUE   "739210000".
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）（離島）
           03  WRK-CONS-SANSO2        PIC X(09)   VALUE   "739250000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）
           03  WRK-CONS-SANSO3        PIC X(09)   VALUE   "739200000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）（離島）
           03  WRK-CONS-SANSO4        PIC X(09)   VALUE   "739240000".
      *    酸素ボンベ・大型
           03  WRK-CONS-SANSO5        PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ・大型（離島）
           03  WRK-CONS-SANSO6        PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ・小型
           03  WRK-CONS-SANSO7        PIC X(09)   VALUE   "739230000".
      *    酸素ボンベ・小型（離島）
           03  WRK-CONS-SANSO8        PIC X(09)   VALUE   "739270000".
      *
      *    新生児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN1  PIC X(09)   VALUE   "160155290".
      *    乳幼児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN2  PIC X(09)   VALUE   "160155390".
      *
      *    検査逓減
           03  WRK-CONS-KENSA-TEIGEN  PIC X(09)   VALUE   "160000190".
      *    間接撮影の５０％逓減
           03  WRK-CONS-SRYKBN70-TEIGEN
                                      PIC X(09)   VALUE   "170020570".
      *
      *    超音波内視鏡加算
           03  WRK-CONS-CHOONPA-KASAN
                                      PIC X(09)   VALUE   "160148670".
      *
      *    出力対象のコメント
           03  WRK-CONS-COMMENT-G.
      *        複数診療科受診
               05  WRK-CONS-COMMENT1  PIC X(09)   VALUE   "099999901".
      *        他保険にて診察料算定済
               05  WRK-CONS-COMMENT2  PIC X(09)   VALUE   "099999902".
      *********（粉砕）
      *********05  WRK-CONS-COMMENT3  PIC X(09)   VALUE   "099209901".
      *********（残量廃棄）
      *********05  WRK-CONS-COMMENT4  PIC X(09)   VALUE   "099309901".
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
      *    保険組み合わせ退避ワーク
       01  THKNCOMBI-AREA.
           02      TCOMB-MAX        PIC 9(05).
           02      TCOMB-OCC        OCCURS     30.
           COPY    "CPHKNCOMBI.INC" REPLACING   //COMB-//
                                    BY          //TCOMB-//.
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報
           COPY   "CPSK1001.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
                                       WRK-AREA
                                       FLG-AREA
                                       TTNS-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
           END-UNSTRING                                  
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "レセ電データ作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCR0300"      TO  JOB-PGID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *     
           MOVE    "RECE300"           TO  RECE30PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE30PARA-TERMID
      *
           MOVE    "RECE020"           TO  RECE02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE02PARA-TERMID
      *
           MOVE    "RECE039"           TO  RECE03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE03PARA-TERMID
      *
           MOVE    "RECE049"           TO  RECE04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE04PARA-TERMID
      *
           MOVE    "RECE051"           TO  RECE05PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE05PARA-TERMID
      *
           MOVE    "RECE090"           TO  RECE09PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE09PARA-TERMID
      *
           MOVE    "RECE110"           TO  RECE11PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE11PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    I-O                RECE02-FILE  
      *
           OPEN    I-O                RECE03-FILE  
      *
           OPEN    I-O                RECE041-FILE  
      *
           OPEN    I-O                RECE05-FILE  
      *
           OPEN    INPUT               RECE09-FILE
      *
           OPEN    INPUT               RECE11-FILE
      * 
           IF      FLG-END             =   ZERO
      *
      *        ファイル読み込み
               PERFORM 900-RECE02-READ-SEC
      *
      *        時間外加算点数取得
               INITIALIZE                  WRK-CONS-JIKANGAI-TEN-TABLE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-JIKANGAI-MAX
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    WRK-CONS-JIKANGAI-SRYCD (IDX)
                                           TO  TNS-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   MOVE    TNS-TEN         TO
                                           WRK-CONS-JIKANGAI-TEN (IDX)
               END-PERFORM
           END-IF    
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
              MOVE    "01"                TO  ORC-DBYMD (7:2)
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              PERFORM 910-SYSKANRI-INV-SEC
              IF      FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC         TO  SYS-1001-REC
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
              INITIALIZE                      SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    SYS-1001-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"             USING
                                              MCPAREA
                                              ORCMCPAREA
                                              MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
              MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
              CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
               IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           IF      SYS-2005-RECECOMKBN      =   SPACE
               MOVE    ZERO             TO  SYS-2005-RECECOMKBN
           END-IF
      *    
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-VOLNUM
                                       WRK-RECENUM
      *
      *    審査支払機関・診療科別処理              
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
      *
      *        県単用項目ファイル読込処理
               PERFORM 301-RECE11-HENSYU-SEC
      *
               MOVE    ZERO               TO  WRK-RENNUM
      *
               MOVE    ZERO               TO  WRK-COM-RENNUM
                                              FLG-COMMENT
               MOVE    SPACE              TO  WRK-COM-SRYKBN
                                              WRK-COM-FTNKBN
      *
               INITIALIZE                     WRK-RECE041-SRYKBN
                                              TBL-KOUI-SRYKBN12-AREA
      *
      *        患者保険組み合わせ取得処理
               PERFORM     2001-HKNCOMBI-GET-SEC
      *
      *        保険履歴情報編集処理
               PERFORM     2001-RIREKI-HEN-SEC 
      *
      *        実日数／合計点数等のＩＮＤＥＸ
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
      *
      *        総件数／総合計点数の集計
               MOVE    ZERO                TO  SUM-TOTALTEN
                                               FLG-TOTALTEN       
               IF      RECE02-RJNHKNNUM    =   SPACE           
               AND     RECE02-HKNNUM       =   SPACE
                   IF      RECE02-KOHNUM (3)   NOT =   SPACE
                       MOVE    RECE02-TOTALTEN (4) TO  SUM-TOTALTEN
                   ELSE
                       MOVE    RECE02-TOTALTEN (2) TO  SUM-TOTALTEN
                   END-IF
               ELSE
                   MOVE     RECE02-TOTALTEN (1) TO  SUM-TOTALTEN
               END-IF
      *         
      *        レセプト共通レコード
               PERFORM     2002-HENSYU-SEC
      *        保険者レコード
               PERFORM     2003-HENSYU-SEC
      *        老人レコード
               PERFORM     2004-HENSYU-SEC
      *        公費レコード
               PERFORM     2005-HENSYU-SEC
      *        傷病名レコード
               PERFORM     2006-HENSYU-SEC
      *        適用レコード
               PERFORM     2007-HENSYU-SEC
      *        症状詳記（レセコメント）
               PERFORM     2009-COMMENT-HEN-99-SEC
      *
               ADD     1           TO  CNT-COMMIT
               IF  CNT-COMMIT      =    300
                   MOVE  ZERO      TO      CNT-COMMIT
                   PERFORM 900-DBCOMMIT-SEC
                   PERFORM 900-DBSTART-SEC
               END-IF
      *
               PERFORM     900-RECE02-READ-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理
      *****************************************************************
       2002-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RE,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    レセプト番号
      *****ADD     1                   TO  WRK-RECENUM
      *****MOVE    WRK-RECENUM         TO  WRK-NUM
      *****MOVE    6                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    レセプト種別
           MOVE    RECE02-RECESYUBETU  TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    RECE02-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD (1:5)    TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    氏名
           IF      RECE02-NAME (1:2)   =   "　"
               IF      RECE02-KANANAME (1:2)   =   "　"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       49
                       IF  RECE02-KANANAME (IDX:2)   =   "　"
                           CONTINUE
                       ELSE     
                           MOVE    RECE02-KANANAME (IDX:) TO  WRK-DATA
                           MOVE    49                     TO  IDX
                       END-IF
                   END-PERFORM 
               ELSE
                   MOVE    RECE02-KANANAME     TO  WRK-DATA
               END-IF
           ELSE
               MOVE    RECE02-NAME         TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    RECE02-SEX          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    RECE02-BIRTHDAY     TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    給付割合（国保OR退職者のみ）
           IF    ( RECE02-TEISYUTUSAKI =   "2"     AND
                   RECE02-RJNHKNNUM    =   SPACE                )
      ***********  RECE02-RJNHKNNUM    =   SPACE                ) OR
      *           ( RECE02-HKNNUM       =   WRK-CONS-HKNNUM1  OR
      *                                     WRK-CONS-HKNNUM2  OR
      *                                     WRK-CONS-HKNNUM3  OR
      *                                     WRK-CONS-HKNNUM4  OR
      *                                     WRK-CONS-HKNNUM5  OR
      ***********                           WRK-CONS-HKNNUM6     )
               COMPUTE WRK-KYURATE =  (100 -   RECE02-KYURATE) 
               MOVE   WRK-KYURATE          TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF    
      *    レセプト特記事項編集情報
           IF      RECE02-2006-RECEDEN-KYURATE
                                       >   ZERO
               MOVE   RECE02-2006-RECEDEN-KYURATE
                                           TO  WRK-NUM
               MOVE    3                   TO  WRK-NAGASA
           END-IF
      *
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院年月日
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病棟区分
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    一部負担金・食事療養費標準負担額区分
           IF    ( RECE02-TEISYOTOKU   =   SPACE )
           OR    ( RECE02-ZAITAKU      =   SPACE )
               CONTINUE
           ELSE
               EVALUATE    RECE02-TEISYOTOKU
                   WHEN    "1"
                       MOVE    "3"             TO  WRK-DATA
                   WHEN    "2"
                       MOVE    "1"             TO  WRK-DATA
               END-EVALUATE
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *
      *    特記事項
           IF      RECE02-2006-RECEDEN-TOKKIINF
                                   NOT =   SPACE
      *        レセプト特記事項編集情報
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   IF      RECE02-2006-RECEDEN-TOKKI (IDX)
                                           NOT =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       5
                           IF      RECE02-TOKKI (IDZ)  =   SPACE
                               MOVE    RECE02-2006-RECEDEN-TOKKI (IDX)
                                               TO  RECE02-TOKKI (IDZ)
                           END-IF
                           IF      RECE02-2006-RECEDEN-TOKKI (IDX)
                                               =   RECE02-TOKKI (IDZ)      
                               MOVE    5       TO  IDZ
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-RECE11          =   ZERO
      *    レセプトカスタマイズ情報
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   IF      RECE11-TOKKI (IDX)  NOT =   SPACE
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       5
                           IF      RECE02-TOKKI (IDZ)  =   SPACE
                               MOVE    RECE11-TOKKI (IDX)
                                               TO  RECE02-TOKKI (IDZ)
                           END-IF
                           IF      RECE11-TOKKI (IDX)
                                               =   RECE02-TOKKI (IDZ)      
                               MOVE    5       TO  IDZ
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF 
           MOVE    1               TO  IDZ                  
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02-TOKKI (IDX)  TO  WRK-DATA (IDZ:2)
                   ADD     2               TO  IDZ
           END-PERFORM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE02-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-NUM
                   MOVE    4                   TO  WRK-NAGASA
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-NUM
                       MOVE    4                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC         
      *    カルテ番号等
           MOVE    RECE02-PTNUM        TO  WRK-DATA     
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    割引点数単価
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科コード
           MOVE    RECE02-RECEKA       TO  WRK-DATA
           MOVE    1                   TO  WRK-END 
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *
           MOVE    1                   TO  FLG-TOTALTEN
           PERFORM 5001-FILE-WRITE-SEC
           .
       2002-HENSYU-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    保険者レコード編集処理
      *****************************************************************
       2003-HENSYU-SEC              SECTION.
      *
           IF      RECE02-HKNNUM           =   SPACE
           OR      RECE02-RJNHKNNUM    NOT =   SPACE 
               GO  TO  2003-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "HO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    保険者番号
           IF  RECE02-TEISYUTUSAKI    =   "2"
               IF      RECE02-HKNJANUM (1:2)  =   "67"
                   MOVE    RECE02-HKNJANUM  TO  WRK-HENKAN
               ELSE
      *        国保の場合（６桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:6)
                                            TO  WRK-HENKAN
               END-IF
           ELSE
               IF  RECE02-HKNNUM      =   "001"
      *            政府管掌の場合（４桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:4)
                                         TO  WRK-HENKAN
               ELSE
      *            他の保険の場合（８桁）
                   MOVE    RECE02-HKNJANUM
                                         TO  WRK-HENKAN
               END-IF
           END-IF
           MOVE    8                     TO  WRK-LENGTH 
           PERFORM 5003-DATA-RIGHT-HENSYU-SEC
           MOVE    WRK-HENKAN            TO  WRK-DATA
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           MOVE    RECE02-KIGO           TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           MOVE    RECE02-NUM            TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX1)   TO  WRK-NUM
           MOVE    2                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX1) TO  WRK-NUM
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    薬剤一部負担金
           MOVE    ZERO                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養費（日数）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJINISSU(IDX1)
                                             TO  WRK-NUM
               MOVE    2                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC   
      *    食事療養費（合計金額）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJIRYOYOHI(IDX1)
                                             TO  WRK-NUM
               MOVE    8                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    職務上の事由
           IF      RECE02-HKNNUM       =   "002"
               EVALUATE    RECE02-HOJOKBN
                   WHEN    "1"
                   WHEN    "A"
                   WHEN    "D"
                       MOVE    "1"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "E"
                       MOVE    "2"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
                   WHEN    "3" 
                   WHEN    "C"
                   WHEN    "F"
                       MOVE    "3"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
               END-EVALUATE
           END-IF    
           IF      RECE02-HKNNUM (1:2) =   "03"
               EVALUATE    RECE02-HOJOKBN
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "E"
                       MOVE    "2"                   TO  WRK-DATA
                       MOVE    1                     TO  WRK-NAGASA
               END-EVALUATE
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    証明書番号（国保OR退職者のみ）
           IF    ( RECE02-TEISYUTUSAKI =   "2"     AND
                   RECE02-RJNHKNNUM    =   SPACE                ) OR
                 ( RECE02-HKNNUM       =   WRK-CONS-HKNNUM1  OR
                                           WRK-CONS-HKNNUM2  OR
                                           WRK-CONS-HKNNUM3  OR
                                           WRK-CONS-HKNNUM4  OR
                                           WRK-CONS-HKNNUM5  OR
                                           WRK-CONS-HKNNUM6     )
               MOVE    RECE02-CERTNUM      TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    医療保険一部負担金額
           IF      RECE02-FTNMONEY(IDX1)
                                       >   ZERO
               IF    ( RECE02-HKNNUM       =   "002" )
               AND   ( RECE02-HOJOKBN      =   "3"   )
                   MOVE    RECE02-FTNMONEY(IDX1) TO  WRK-NUM
                   MOVE    9                     TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    国保減免区分                                 
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    国保減額割合 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    国保減額金額 
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2003-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    老人レコード編集処理
      *****************************************************************
       2004-HENSYU-SEC              SECTION.
      *
           IF      RECE02-RJNHKNNUM    =   SPACE
               GO  TO  2004-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    保険者番号
           IF  RECE02-TEISYUTUSAKI    =   "2"
               IF      RECE02-HKNJANUM (1:2)  =   "67"
                   MOVE    RECE02-HKNJANUM  TO  WRK-HENKAN
               ELSE
      *        国保の場合（６桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:6)
                                            TO  WRK-HENKAN
               END-IF
           ELSE
               IF  RECE02-HKNNUM      =   "001"
      *            政府管掌の場合（４桁右詰に編集）
                   MOVE    RECE02-HKNJANUM(1:4)
                                         TO  WRK-HENKAN
               ELSE
      *            他の保険の場合（８桁）
                   MOVE    RECE02-HKNJANUM
                                         TO  WRK-HENKAN
               END-IF
           END-IF
           MOVE    8                   TO  WRK-LENGTH
           PERFORM 5003-DATA-RIGHT-HENSYU-SEC
           MOVE    WRK-HENKAN          TO  WRK-DATA
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           MOVE    RECE02-KIGO           TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           MOVE    RECE02-NUM            TO  WRK-DATA
           MOVE    40                    TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX1)   TO  WRK-NUM
           MOVE    2                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX1) TO  WRK-NUM
           MOVE    8                     TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC
      *    食事療養費（日数）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJINISSU(IDX1)
                                             TO  WRK-NUM
               MOVE    2                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC   
      *    食事療養費（合計金額）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJIRYOYOHI(IDX1)
                                             TO  WRK-NUM
               MOVE    8                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（市町村番号）
           MOVE    RECE02-RJNFTNJANUM  TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    老人医療（受給者番号）
           MOVE    RECE02-RJNJKYSNUM   TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担金額（老人減免区分）
      *****IF    ( RECE02-TEISYUTUSAKI     =   "2"        )
           IF    ( RECE02-RJNHKNNUM    NOT =   SPACE )
           AND   (RECE02-ETC-KOHNUM  (1)   =   "019" ) 
               MOVE    4                   TO  WRK-NUM
               MOVE    1                   TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（老人外来金額）
           IF    ( RECE02-NYUGAIKBN    =   "2"  )
           AND   ( RECE02-FTNMONEY (IDX1)
                                       >   ZERO )
               MOVE    RECE02-FTNMONEY (IDX1)
                                           TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC     
      *    老人医療（老人入院金額）
           IF    ( RECE02-NYUGAIKBN    =   "1"  )
           AND   ( RECE02-FTNMONEY (IDX1)
                                       >   ZERO )
               MOVE    RECE02-FTNMONEY (IDX1)
                                           TO  WRK-NUM
               MOVE    6                   TO  WRK-NAGASA
           END-IF    
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC     
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2004-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    公費レコード編集処理
      *****************************************************************
       2005-HENSYU-SEC              SECTION.
      *
           IF      RECE02-KOHNUM (1)   =   SPACE
               GO  TO  2005-HENSYU-EXT
           END-IF
      *
      *    三種公費併用のとき
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       3
                   OR      RECE02-KOHNUM (IDY) =   SPACE
               COMPUTE IDX     =   IDY     +   1
      *********IF    ( RECE02-PREFNUM              =   "14"    )
      *********AND   ( RECE02-KOHNUM (IDY) (1:1)   >=  "1" AND 
      *********                                    <=  "8"     )
      *********AND   ( RECE02-KOHFTNJANUM (IDY)    =   SPACE   )
      *********AND   ( RECE02-KOHJKYSNUM  (IDY)    =   SPACE   )
      *********    CONTINUE
      *********ELSE 
      *            レセプト特記事項編集情報
                   IF      RECE02-2006-RECEDEN-KORECKBN (IDY)
                                                   =   "1"
                       CONTINUE
                   ELSE 
                       PERFORM 20051-KOH-HENSYU-SEC
                   END-IF
      *********END-IF
           END-PERFORM                      
           .
       2005-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    公費レコード編集処理
      *****************************************************************
       20051-KOH-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "KO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    負担者番号
           IF      RECE02-KOHFTNJANUM (IDY)
                                   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE02-KOHFTNJANUM (IDY)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-SYUBETU =   "1"
                   MOVE    RECE02-KOHFTNJANUM (IDY)
                                           TO  WRK-DATA
               END-IF
           END-IF
           EVALUATE    RECE02-2006-RECEDEN-FTNJANUMKBN  (IDY)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  WRK-DATA
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-RECEDEN-FTNJANUM (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-RECEDEN-FTNJANUM (IDY)
                                               TO  WRK-DATA
                   END-IF
           END-EVALUATE
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC     
      *    受給者番号
           IF    ( RECE02-PREFNUM      =   "23"                    )
           AND   ( RECE02-TEISYUTUSAKI =   "2"                     )
           AND   ( RECE02-KOHNUM (IDY) (1:1)
                                       >=  "1" AND <= "8"          )
           AND   ( RECE02-KOHHBTNUM (IDY)
                                       =   "81" OR  "82"  OR 
                                           "83" OR  "84"  OR  "85" )
               MOVE    "0000000"           TO  WRK-DATA
           ELSE                          
               IF      RECE02-KOHJKYSNUM  (IDY)
                                           NOT =   SPACE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHJKYSNUM  (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-KOHJKYSNUM  (IDY)
                                               TO  WRK-DATA
                   END-IF
               END-IF
           END-IF
           EVALUATE    RECE02-2006-RECEDEN-JKYSNUMKBN  (IDY)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  WRK-DATA
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE02-2006-RECEDEN-JKYSNUM (IDY)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE02-2006-RECEDEN-JKYSNUM (IDY)
                                               TO  WRK-DATA
                   END-IF
           END-EVALUATE
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    任意給付区分                             
           IF    ( RECE02-TEISYUTUSAKI =   "2"     AND
                   RECE02-RJNHKNNUM    =   SPACE                ) 
           OR    ( RECE02-HKNNUM       =   WRK-CONS-HKNNUM1  OR
                                           WRK-CONS-HKNNUM2  OR
                                           WRK-CONS-HKNNUM3  OR
                                           WRK-CONS-HKNNUM4  OR
                                           WRK-CONS-HKNNUM5  OR
                                           WRK-CONS-HKNNUM6     )
               IF      RECE02-KOHNINIKYUKBN (IDY) NOT =   SPACE
                   MOVE    "1"             TO  WRK-DATA
                   MOVE    1               TO  WRK-NAGASA 
               END-IF         
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数     
           MOVE    RECE02-JNISSU(IDX)     TO  WRK-NUM
           MOVE    2                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計点数     
           MOVE    RECE02-TOTALTEN(IDX)   TO  WRK-NUM
           MOVE    8                      TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（公費）
           IF     RECE02-KOHFTNJANUM (IDY) (1:2)
                                           =   "51"
               IF      RECE02-KOHFTNJANUM (IDY) (5:3)
                                               =   "602"
                   MOVE    RECE02-FTNMONEY(IDX)   TO  WRK-NUM
                   MOVE    8                      TO  WRK-NAGASA
               END-IF
           ELSE
               IF      RECE02-FTNMONEY(IDX)   >   ZERO     
                   MOVE    RECE02-FTNMONEY(IDX)   TO  WRK-NUM
                   MOVE    8                      TO  WRK-NAGASA
               END-IF
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（老人公費給付対象老人外来一部負担金）
           IF      RECE02-NYUGAIKBN      =   "2"
           AND     RECE02-RJNHKNNUM  NOT =   SPACE
      ****     MOVE                          TO  WRK-NUM
      ****     MOVE    6                     TO  WRK-NAGASA
               CONTINUE      
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    負担金額（老人公費給付対象老人入院一部負担金）
           IF      RECE02-NYUGAIKBN      =   "1"
           AND     RECE02-RJNHKNNUM  NOT =   SPACE
      ****     MOVE                          TO  WRK-NUM
      ****     MOVE    6                     TO  WRK-NAGASA
               CONTINUE      
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC
      *    薬剤一部負担金
           MOVE    ZERO                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養費（日数）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJINISSU(IDX)
                                             TO  WRK-NUM
               MOVE    2                     TO  WRK-NAGASA
           END-IF    
           PERFORM 5005-NUM-HENSYU-SEC    
      *    食事療養費（合計金額）
           IF      RECE02-NYUGAIKBN      =   "1"
               MOVE    RECE02-SHOKUJIRYOYOHI(IDX)
                                             TO  WRK-NUM
               MOVE    5                     TO  WRK-NAGASA
           END-IF    
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20051-KOH-HENSYU-EXT.
           EXIT. 
      *            
      * 
      *****************************************************************
      *    傷病名レコード編集処理
      *****************************************************************
       2006-HENSYU-SEC              SECTION.
      *     
      *    傷病名情報
           INITIALIZE                      RECE031-REC
           MOVE    RECE02-HOSPID       TO  RECE031-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE031-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE031-PTID
           MOVE    RECE02-RECEKA       TO  RECE031-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    RECE02-HKNJANUM     TO  RECE031-HKNJANUM
           MOVE    RECE02-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
           START   RECE03-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE03
               NOT INVALID
                   PERFORM 990-RECE03-NEXT-SEC
           END-START
      *     
           PERFORM UNTIL  ( FLG-RECE03   =   1                    )
                   OR     ( RECE02-HOSPID
                                     NOT =   RECE031-HOSPID       )
                   OR     ( RECE02-SRYYM
                                     NOT =   RECE031-SRYYM        )
                   OR     ( RECE02-NYUGAIKBN
                                     NOT =   RECE031-NYUGAIKBN    )
                   OR     ( RECE02-PTID
                                     NOT =   RECE031-PTID         )
                   OR     ( RECE02-RECEKA
                                     NOT =   RECE031-RECEKA       )
                   OR     ( RECE02-TEISYUTUSAKI
                                     NOT =   RECE031-TEISYUTUSAKI )
                   OR     ( RECE02-RECESYUBETU
                                     NOT =   RECE031-RECESYUBETU  )
                   OR     ( RECE02-HKNJANUM
                                     NOT =   RECE031-HKNJANUM     )
                   OR     ( RECE02-TEKSTYMD
                                     NOT =   RECE031-TEKSTYMD     )
      *        転帰区分:8(移行)以外 処理対象
               IF      RECE031-TENKIKBN     =   "8"
                   CONTINUE
               ELSE    
                   PERFORM 20061-BYOMEI-HENSYU-SEC
               END-IF
      *             
               PERFORM 990-RECE03-NEXT-SEC
           END-PERFORM       
           .
       2006-HENSYU-EXT.
           EXIT. 
      *            
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20061-BYOMEI-HENSYU-SEC         SECTION.
      *
      *    傷病名コード／修飾語コードの取得
           MOVE    SPACE               TO  WRK-BYOMEI-AREA
      *     
           IF      RECE031-BYOMEIHENFLG =   "1"
      *        編集病名のとき未傷病名コードにする
      *********IF      RECE031-SYUBYOFLG-X          =   "1" 
      *********AND     SYS-2005-SYUBYOHENFLG    NOT =   ZERO
      *********    MOVE    "0000998"            TO  WRK-BYOMEICD
      *********ELSE
      *********    MOVE    "0000999"            TO  WRK-BYOMEICD
      *********END-IF
               MOVE    "0000999"           TO  WRK-BYOMEICD
           ELSE    
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE031-BYOMEICD(IDX)   =   SPACE
                 IF      RECE031-BYOMEICD(IDX) =   ALL "9"
                     CONTINUE
                 ELSE  
                   IF      RECE031-BYOMEICD(IDX)(1:3)  NOT =   "ZZZ"
                       IF      WRK-BYOMEICD                =   SPACE
      *                    廃止年月日が設定されているとき診療開始日以前
      *                    なら未傷病名コードにする
                           MOVE    RECE031-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HAISIYMD    NOT =   SPACE
                               IF      BYO-HAISIYMD    <
                                                       RECE031-SRYYMD
      *****************************IF      RECE031-SYUBYOFLG-X  =   "1" 
      *****************************AND     SYS-2005-SYUBYOHENFLG
      *****************************                         NOT =   ZERO
      *****************************    MOVE    "0000998"
      *****************************                    TO  WRK-BYOMEICD
      *****************************ELSE
      *****************************    MOVE    "0000999"
      *****************************                    TO  WRK-BYOMEICD
      *****************************END-IF
                                   MOVE    "0000999"   TO  WRK-BYOMEICD
                               ELSE
                                   MOVE    RECE031-BYOMEICD(IDX)
                                                       TO  WRK-BYOMEICD
                               END-IF
                           ELSE
                               MOVE    RECE031-BYOMEICD(IDX)
                                                       TO  WRK-BYOMEICD
                           END-IF
                       END-IF    
                   ELSE            
                       ADD     1           TO  IDY
                       MOVE    RECE031-BYOMEICD(IDX)(4:)
                                           TO  WRK-RECECD(IDY)
                   END-IF    
                 END-IF       
               END-PERFORM
           END-IF    
      *
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "SY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    傷病名コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    診療開始日
           MOVE    RECE031-SRYYMD      TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD          TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    転記区分
           EVALUATE    RECE031-TENKIKBN
               WHEN    "1"
                   MOVE    "2"         TO  WRK-DATA
               WHEN    "2"
                   MOVE    "3"         TO  WRK-DATA
               WHEN    "3"
                   MOVE    "4"         TO  WRK-DATA
               WHEN    SPACE
                   MOVE    "1"         TO  WRK-DATA
           END-EVALUATE        
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    修飾語コード
           EVALUATE    WRK-BYOMEICD 
               WHEN    "0000998"
               WHEN    "0000999"
                   CONTINUE
               WHEN    OTHER
                   MOVE    WRK-RECECD-G        TO  WRK-DATA
                   MOVE    80                  TO  WRK-NAGASA
           END-EVALUATE
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    傷病名称
           IF      WRK-BYOMEICD        =   "0000999"   OR
                                           "0000998"
               MOVE    RECE031-BYOMEI      TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    主傷病
           IF      RECE031-SYUBYOFLG-X         =   "1" 
           AND     SYS-2005-SYUBYOHENFLG   NOT =   ZERO
               MOVE    "01"                TO  WRK-DATA
           END-IF 
           MOVE    2                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20061-BYOMEI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    摘要レコード編集処理
      *****************************************************************
       2007-HENSYU-SEC              SECTION.
      *                                        
      *    摘要情報
           INITIALIZE                      RECE041-REC
           MOVE    RECE02-HOSPID       TO  RECE041-HOSPID
           MOVE    RECE02-SRYYM        TO  RECE041-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE041-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE041-PTID
           MOVE    RECE02-RECEKA       TO  RECE041-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE041-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE041-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE041-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE041-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE041-TEKSTYMD
      *
           START   RECE041-FILE
               KEY IS    >=   RECE041-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE041-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                   )
               OR     ( FLG-RECE04           =   1                   )
               OR     ( RECE02-HOSPID    NOT =   RECE041-HOSPID       )
               OR     ( RECE02-SRYYM     NOT =   RECE041-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE041-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE041-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE041-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE041-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE041-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE041-HKNJANUM     )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE041-HOJOKBN      )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE041-TEKSTYMD     )
      *
      *        負担区分編集処理
               PERFORM 20070-FTNKBN-HENSYU-SEC
      *
      *        症状詳記（手術歴・保険履歴）
               IF      FLG-COMMENT         =   ZERO
                   MOVE    1                   TO  FLG-COMMENT
                   MOVE    WRK-FTNKBN          TO  WRK-COM-FTNKBN
                   PERFORM 2008-COMMENT-HEN-01-SEC
               END-IF
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *
      *       摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   PERFORM 20072-ZAI-HENSYU-SEC
      *
      *            再診の時間外を別剤とする
                   IF      FLG-GAITENTTLKBN        >   ZERO
                       PERFORM 20079-SRYKBN12-JIKANGAI-HENSYU-SEC
                   END-IF
      * 
                   PERFORM 990-RECE041-NEXT-SEC
               END-IF     
           END-PERFORM
      *
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    負担区分編集処理
      *****************************************************************
       20070-FTNKBN-HENSYU-SEC    SECTION.
      *      
      *    主保険＋３種公費のとき該当の負担区分がないので
      *    ＋２種公費のときと同じ負担区分を編集する      
      *   
           INITIALIZE                 WRK-FTNKBN-AREA
      *
           MOVE    ZERO               TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1  BY 1
                   UNTIL ( IDX0    >  TCOMB-MAX )
                    OR   ( FLG-HIT =  1 )
               IF    ( RECE041-HOSPID      =   TCOMB-HOSPID (IDX0))
                AND  ( RECE041-PTID        =   TCOMB-PTID   (IDX0))
                AND  ( RECE041-HKNCOMBI    =   TCOMB-HKNCOMBINUM
                                                            (IDX0))
                    MOVE   TCOMB-OCC (IDX0)
                                       TO  HKNCOMBI-REC
                    MOVE   1           TO  FLG-HIT
               END-IF
           END-PERFORM
      * 
           MOVE    ZERO            TO  FLG-HKNCOMBI
           IF    ( FLG-HIT         =  ZERO )
      *        保険組合せ読込み
               INITIALIZE                 HKNCOMBI-REC
               MOVE    RECE041-HOSPID  TO  COMB-HOSPID
               MOVE    RECE041-PTID    TO  COMB-PTID
               MOVE    RECE041-HKNCOMBI
                                      TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC   TO  MCPDATA-REC
               PERFORM 910-HKNCOMBI-INV-SEC
               IF      FLG-HKNCOMBI   =   1
                   DISPLAY "HKNCONBI INV"
                   GO  TO  20070-FTNKBN-HENSYU-EXT
               END-IF
           END-IF
      *                                                      
      *    主保険のみ      
           IF      COMB-HKNNUM    NOT =   SPACE
           AND     COMB-KOH1HKNNUM    =   SPACE
               MOVE    "1"            TO  WRK-FTNKBN
           END-IF
      *         
      *    老人保険のみ     
           IF      COMB-HKNNUM    NOT =   SPACE
           AND     COMB-KOH1HKNNUM    =   "027"
           AND     COMB-KOH2HKNNUM    =   SPACE
               MOVE    "1"            TO  WRK-FTNKBN
           END-IF
      *     
      *    生保のみ（三種公費のとき）      
           IF      WRK-3SYU-KBN       =   ALL "1"  
           AND     RECE02-HKNNUM      =   SPACE            
               IF      COMB-KOH1HKNNUM    =   "012"
               AND     COMB-KOH2HKNNUM    =   SPACE
                   MOVE    "1"            TO  WRK-FTNKBN
               END-IF
           END-IF    
      *???
      **     DISPLAY "HKNNUM=" COMB-HKNNUM  "KOH1=" COMB-KOH1HKNNUM "##"
      **             "KOH2="   COMB-KOH2HKNNUM "##" WRK-FTNKBN        
      *???              
      *
           IF      WRK-FTNKBN     NOT =   SPACE
               GO  TO  20070-FTNKBN-HENSYU-EXT
           END-IF
      *
      *    公費の編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
                   OR      COMB-KOHHKNNUM  (IDX)   =   SPACE
               IF      COMB-KOHHKNNUM  (IDX)   =   "027"
                   CONTINUE
               ELSE        
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       3
                           OR      RECE02-KOHNUM(IDY)  =   SPACE
                       IF     (COMB-KOHHKNNUM(IDX) =
                                              RECE02-KOHNUM(IDY)) AND
                              (COMB-KOHID(IDX) =
                                              RECE02-KOHID(IDY))
                           MOVE    1           TO  WRK-KOHNUM (IDY)
                           MOVE    3           TO  IDY                
                       END-IF
                   END-PERFORM
               END-IF    
           END-PERFORM
      *???
      **     DISPLAY "RECE02(1)=" RECE02-KOHNUM (1) "(2)="
      **                          RECE02-KOHNUM (2)
      **             "(3)=" RECE02-KOHNUM (3) "##"
      **     DISPLAY "(1)=" WRK-KOHNUM (1) "(2)=" WRK-KOHNUM (2)
      **             "(3)=" WRK-KOHNUM (3) "##"
      *???                  
      *                                                      
      *    主保険があるとき      
           IF      COMB-HKNNUM        NOT =   SPACE
               IF      COMB-KOH1HKNNUM    NOT =   "027"
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "2"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "4"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    ZERO
                           MOVE    "1"            TO  WRK-FTNKBN
                   END-EVALUATE
               ELSE
      *    老人保険があるとき     
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "8"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "A"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    ZERO
                           MOVE    "1"            TO  WRK-FTNKBN
                   END-EVALUATE
               END-IF    
           ELSE
      *     
      *        公費単独（三種公費）のとき      
               IF      WRK-3SYU-KBN       =   ALL "1"  
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "2"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "3"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "4"            TO  WRK-FTNKBN
                   END-EVALUATE
               ELSE    
      *                                                      
      *        公費単独（三種公費以外）のとき      
                   EVALUATE    WRK-KOHNUM (1) ALSO    WRK-KOHNUM (2)
                       WHEN    1              ALSO    ZERO
                           MOVE    "5"            TO  WRK-FTNKBN
                       WHEN    ZERO           ALSO    1
                           MOVE    "6"            TO  WRK-FTNKBN
                       WHEN    1              ALSO    1
                           MOVE    "7"            TO  WRK-FTNKBN
                   END-EVALUATE
               END-IF
           END-IF
      *???  
           IF      WRK-FTNKBN    =   SPACE
               DISPLAY "FTNKBN ERR " RECE02-PTNUM "##"
           END-IF
      *???              
           .
       20070-FTNKBN-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       20071-ZAI-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
                                           TBL-KOUI-AREA
                                           TBL-KOUI-SRYKBN12-AREA
                                           TBL-COMMENT-AREA
           INITIALIZE                      WRK-RECE05-REC
                                           WRK-KASAN-RECE05-REC
                                           WRK-TEIGEN-RECE05-REC
                                           WRK-700-RECE05-AREA
                                           WRK-721-RECE05-REC
                                           WRK-SRYKBN70-RECE05-REC
      *
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX1
                                           WRK-TBLMAX6
                                           WRK-TBLMAX7
                                           WRK-TBLMAX8
                                           FLG-TEIGEN
                                           FLG-YKZ20
                                           FLG-KOKUJISKBKBN1
                                           FLG-GAITENTTLKBN
                                           FLG-SRYSYUKBN721
      *************************************FLG-SRYSYUKBN700
                                           FLG-SRYKBN70-TEIGEN
                                           FLG-KENSA-KASAN
                                           FLG-KENSA-TEIGEN
                                           WRK-SRYCD1-MAX
                                           WRK-SRYCD71-MAX
                                           WRK-SRYCD72-MAX
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
      *                                     
           MOVE    RECE041-SRYKBN      TO  WRK-SRYKBN
      *
           INITIALIZE                      RECE05-REC
           MOVE    RECE041-HOSPID       TO  RECE05-HOSPID
           MOVE    RECE041-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE041-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE041-PTID         TO  RECE05-PTID
           MOVE    RECE041-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE041-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE041-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE041-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE041-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE041-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE041-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END                =   1                   )
               OR     ( FLG-RECE05             =   1                   )
               OR     ( RECE041-HOSPID     NOT =   RECE05-HOSPID       )
               OR     ( RECE041-SRYYM      NOT =   RECE05-SRYYM        )
               OR     ( RECE041-NYUGAIKBN  NOT =   RECE05-NYUGAIKBN    )
               OR     ( RECE041-PTID       NOT =   RECE05-PTID         )
               OR     ( RECE041-RECEKA     NOT =   RECE05-RECEKA       )
               OR     ( RECE041-TEISYUTUSAKI
                                           NOT =   RECE05-TEISYUTUSAKI )
               OR     ( RECE041-RECESYUBETU
                                           NOT =   RECE05-RECESYUBETU  )
               OR     ( RECE041-HKNJANUM   NOT =   RECE05-HKNJANUM     )
               OR     ( RECE041-HOJOKBN    NOT =   RECE05-HOJOKBN      )
               OR     ( RECE041-TEKSTYMD   NOT =   RECE05-TEKSTYMD     )
               OR     ( RECE041-ZAINUM     NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                         OR      ( FLG-END           =   1     )
                 IF      RECE05-RECEDENFLG(IDX) =   SPACE
                   MOVE    SPACE                TO  WRK-KIZAI-SRYCD
                   IF      RECE05-SRYCD (IDX) (1:3) = "059"
                       MOVE    RECE05-SRYCD (IDX)
                                                TO  WRK-KIZAI-SRYCD
                       MOVE    "777770000"      TO  RECE05-SRYCD (IDX)
                   END-IF
      *
                   EVALUATE    RECE05-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
                           IF      RECE041-SRYKBN  =   "13"    OR  "14"
                                                               OR  "70"
                               PERFORM 200711-SRYKBN13-COMMENT-TAIHI-SEC
                           ELSE
                               PERFORM 200711-COMMENT-TAIHI-SEC
                           END-IF
                       WHEN    "1"
                           PERFORM 200711-KOUI-TAIHI-SEC
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                   END-EVALUATE
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE05-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX 
               MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX 
                       MOVE    WRK-700-RECE05-REC (IDXY)  TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
      *
      *    生体検査とき順番を変える
           IF      FLG-KENSA-TEIGEN    >   ZERO
               MOVE    FLG-KENSA-TEIGEN        TO  IDX 
               MOVE    WRK-TEIGEN-RECE05-REC   TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
           IF      FLG-KENSA-KASAN     >   ZERO
               MOVE     FLG-KENSA-KASAN        TO  IDX 
               MOVE    WRK-KASAN-RECE05-REC    TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
      *
           .
       20071-TBL-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-COMMENT-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE05-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE05-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE05-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-COMMENT-TAIHI-EXT
               END-IF
           END-IF    
      *     
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0300 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    RECE05-SRYCD (IDX)  TO
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
               MOVE    RECE05-NAME  (IDX)  TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
      *
               IF      RECE05-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE05-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
                   END-IF
      *            コメント
                   IF    ( RECE05-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE05-SRYCD (IDX) (1:3)     =   "008" )
                       MOVE    RECE05-INPUTCOMENT (IDX)     TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE05-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
               END-IF
           END-IF
           .
       200711-COMMENT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-SRYKBN13-COMMENT-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE05-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE05-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE05-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE05-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-SRYKBN13-COMMENT-TAIHI-EXT
               END-IF
           END-IF    
      *               
           IF      WRK-TBLMAX8         >=  100
               MOVE    "*** ORCR0300 COMMENT TABLE  OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX8
      *
               MOVE    RECE05-SRYCD (IDX)  TO
                                   TBL-COMMENT-SRYCD (WRK-TBLMAX8)
               MOVE    RECE05-NAME  (IDX)  TO
                                   TBL-COMMENT-NAME  (WRK-TBLMAX8)
      *
               IF      RECE05-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE05-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-COMMENT-SRYCD (WRK-TBLMAX8)
                   END-IF
      *            コメント
                   IF    ( RECE05-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE05-SRYCD (IDX) (1:3)     =   "008" )
                       MOVE    RECE05-INPUTCOMENT (IDX)     TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE05-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
               END-IF
           END-IF
          .
       200711-SRYKBN13-COMMENT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ退避処理
      *****************************************************************
       200711-KOUI-TAIHI-SEC          SECTION.
      *      
           IF      RECE041-SRYKBN      =   "12"
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-JIKANGAI-MAX
                   IF      RECE05-SRYCD (IDX)
                                   =   WRK-CONS-JIKANGAI-SRYCD (IDY)
                       MOVE    IDY           TO  FLG-GAITENTTLKBN
                       GO  TO  200711-KOUI-TAIHI-EXT
                   END-IF
               END-PERFORM
           END-IF
      *
      *    画像診断・生体検査のとき順番を変える
      *****INITIALIZE                      TNS-TENSU-REC
      *****MOVE    RECE05-SRYCD (IDX)  TO  TNS-SRYCD
      *****PERFORM 900-TENSU-READ-SEC
      *
      *****IF      TNS-SRYSYUKBN       =   "721"
           IF      RECE05-SRYSYUKBN-X (IDX)
                                       =   "721"
               MOVE    RECE05-REC          TO  WRK-RECE05-REC
               MOVE    IDX                 TO  IDW1 
      *
               IF      FLG-SRYSYUKBN721    >   ZERO
                   MOVE     FLG-SRYSYUKBN721   TO  IDX 
                   MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE05-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE05-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX 
                           MOVE    WRK-700-RECE05-REC (IDXY)
                                                           TO RECE05-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE05-AREA
                   END-IF
               END-IF
      *
               MOVE    WRK-RECE05-REC      TO  RECE05-REC
               MOVE    IDW1                TO  IDX
      *
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE05-REC          TO WRK-721-RECE05-REC
           ELSE
      *********IF    ( TNS-SRYCD           =  WRK-CONS-SRYKBN70-TEIGEN )
               IF    ( RECE05-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE05-REC  TO  WRK-SRYKBN70-RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
      *********    IF    ( TNS-SRYSYUKBN       =   "700" )
      *********    AND   ( TNS-DATAKBN         =   "2"   )
                   IF    ( RECE05-SRYSYUKBN-X (IDX)
                                               =   "700" )
                   AND   ( RECE05-DATAKBN   (IDX)
                                               =   "2"   )
                   AND   ( FLG-SRYSYUKBN721    >   ZERO  )
                       ADD     1               TO  WRK-SRYSYUKBN700-MAX
                       MOVE    IDX             TO  FLG-SRYSYUKBN700
                                               (WRK-SRYSYUKBN700-MAX) 
                       MOVE    RECE05-REC      TO WRK-700-RECE05-REC
                                               (WRK-SRYSYUKBN700-MAX) 
                   ELSE
      *****************IF    ( RECE05-SRYCD (IDX)
      *****************                    =   WRK-CONS-KENSA-KASAN1
      *****************                    OR  WRK-CONS-KENSA-KASAN2 )
      *****************    MOVE    IDX         TO  FLG-KENSA-KASAN
      *****************    MOVE    RECE05-REC  TO  WRK-KASAN-RECE05-REC
      *****************ELSE
      *****************    IF      RECE05-SRYCD (IDX)
      *****************                        =   WRK-CONS-KENSA-TEIGEN
      *****************        MOVE    IDX         TO  FLG-KENSA-TEIGEN
      *****************        MOVE    RECE05-REC  TO
      *****************                            WRK-TEIGEN-RECE05-REC
      *****************    ELSE
                               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *****************    END-IF
      *****************END-IF
                   END-IF    
               END-IF     
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）退避編集処理
      *****************************************************************
          2007111-KOUI-TAIHI-HENSYU-SEC SECTION.
      *
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0300 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE05-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE05-NAMEINF (IDX)    TO
                                     TBL-KOUI-NAMEINF   (WRK-TBLMAX1)
      *********MOVE    RECE05-SRYCD    (IDX)   TO
      *********                        TBL-KOUI-SRYCD   (WRK-TBLMAX1)
      *********MOVE    RECE05-SRYSURYO (IDX)   TO
      *********                        TBL-KOUI-SRYSURYO(WRK-TBLMAX1)
      *********MOVE    RECE05-DATAKBN  (IDX)   TO
      *********                        TBL-KOUI-DATAKBN (WRK-TBLMAX1)
      *********INITIALIZE                  TNS-TENSU-REC
      *********MOVE    RECE05-SRYCD (IDX)      TO  TNS-SRYCD
      *********PERFORM 900-TENSU-READ-SEC
      *********IF      FLG-TENSU           =   ZERO
      ************ MOVE    TNS-KOKUJISKBKBN1   TO
      ************                 TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
      *----(01.00.04) LINE ADD START ----------------------------------
      ************ MOVE    TNS-TENSIKIBETU     TO
      ************                 TBL-KOUI-TENSIKIBETU  (WRK-TBLMAX1)
      ************ MOVE    TNS-TEN             TO
      ************                 TBL-KOUI-TEN          (WRK-TBLMAX1)
      *----(01.00.04) LINE ADD END ------------------------------------
      ************ IF      TNS-SRYCD (1:1)     =   "1"
      ************     MOVE    TNS-KZMCOMPSIKIBETU TO
      ************                 TBL-KOUI-KZMCOMPSIKIBETU(WRK-TBLMAX1)
      ************ END-IF
      *********END-IF 
      *
      *        
               IF    ( RECE041-SRYKBN      =   "13"  OR  "14" )
                   IF      TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                           NOT =   "1"
                       MOVE    IDX             TO  FLG-KOKUJISKBKBN1
                   END-IF
                   IF    ( TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                                   =   "1" )
                   AND   ( TBL-KOUI-DATAKBN (WRK-TBLMAX1)
                                                   =   "2" )
                       ADD     TBL-KOUI-TEN (WRK-TBLMAX1)
                                       TO  WRK-KOKUJISKBKBN1-KOUI-TEN
                   END-IF
               END-IF
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）退避処理
      *****************************************************************
       200711-YKZ-TAIHI-SEC          SECTION.
      *
           IF      RECE05-SRYCD (IDX)  =   WRK-CONS-TEIGEN
               CONTINUE
           ELSE
               IF      RECE041-YKZTEN    =   ZERO
                   GO  TO  200711-YKZ-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE05-SRYCD (IDX)
                                       =   WRK-CONS-TEIGEN
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *     
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE05-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE05-YUKOKETA   (IDX)  TO
                                   TBL-ZAIW-YUKOKETA   (1 WRK-TBLMAX6)
           END-IF
           .
       200711-YKZ-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（その他）退避処理
      *****************************************************************
       200711-KIZAI-TAIHI-SEC          SECTION.
      *
           IF      RECE041-SRYKBN    NOT =   "70"
               IF      RECE041-KIZAITEN  =   ZERO
                   GO  TO  200711-KIZAI-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7
      *     
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCR0300 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE05-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
      *
               IF    ( RECE05-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE041-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
               ELSE
                   IF       RECE05-SRYCD (IDX)
                                              =   "799990070"
                       MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
                   ELSE
                       MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX 
                   END-IF
               END-IF
      *
      *         INITIALIZE                  TNS-TENSU-REC
      *         IF      RECE05-SRYCD (IDX)  =   "777770000"
      *             MOVE    WRK-KIZAI-SRYCD     TO  TNS-SRYCD
      *             PERFORM 900-TENSU-READ-SEC
      *             IF      FLG-TENSU           =   ZERO
      *                 MOVE    TNS-TANICD          TO
      *                             TBL-ZAIW-TANICD (2 WRK-TBLMAX7)
      *             END-IF
      *         END-IF
           END-IF
           .
       200711-KIZAI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）並び替え処理
      *****************************************************************
       20074-ZAI-KOUI-SORT-SEC          SECTION.
      *
           INITIALIZE                   WRK-KOUI-AREA
                                        WRK-KOUI-TAIHI-AREA
      *
           IF      RECE041-SRYKBN       =   "50"    OR  "54"    OR
                                            "60"    OR  "40"    OR
                                            "80"
               CONTINUE
           ELSE
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           IF      WRK-TBLMAX1          =   1
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDY
      *
      *    コメントを最後に編集（手術のとき）
           IF      RECE041-SRYKBN      =   "50"
             PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "0"
                   WHEN    "8" 
                       ADD     1                           TO  IDY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC          (IDY)
                       MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD        (IDY)
                       MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU  (IDY)
                       MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD     (IDY)
                       MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDY)
                       INITIALIZE      TBL-KOUI-REC (IDX)
                 END-EVALUATE
              END-PERFORM
            END-IF             
      *
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >   WRK-TBLMAX1
               MOVE    SPACE               TO  WRK-CHUKSNCD
               MOVE    ZERO                TO  FLG-KOUI
      *
               PERFORM UNTIL   IDX >   WRK-TBLMAX1
                 IF      TBL-KOUI-SRYCD (IDX) = "150268970"
                      MOVE    1      TO  
                                  TBL-KOUI-KOKUJISKBKBN1 (IDX)
                 END-IF
                 EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                           ALSO    TBL-KOUI-KOKUJISKBKBN1 (IDX)
                   WHEN    SPACE   ALSO    ANY
                   WHEN    "0"     ALSO    ANY
                   WHEN    "8"     ALSO    ANY
                       CONTINUE
                   WHEN    ANY    ALSO    "1"
                   WHEN    ANY    ALSO    "3"
                   WHEN    ANY    ALSO    "5"
                       IF      WRK-CHUKSNCD        =   SPACE
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-CHUKSNCD
                           ADD     1                           TO  IDY
                           MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC          (IDY)
                           MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD        (IDY)
                           MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU  (IDY)
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD     (IDY)
                           MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDY)
                           INITIALIZE      TBL-KOUI-REC (IDX)
                           MOVE    IDY             TO  WRK-SRYCD1-MAX
                       ELSE
                           MOVE    1               TO  FLG-KOUI
                       END-IF    
                   WHEN    OTHER
                       ADD     1                           TO  IDY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC          (IDY)
                       MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD        (IDY)
                       MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU  (IDY)
                       MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD     (IDY)
                       MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDY)
                       INITIALIZE      TBL-KOUI-REC (IDX)
                       MOVE    IDY             TO  WRK-SRYCD1-MAX
                 END-EVALUATE    
                 COMPUTE IDX     =   IDX     +   1
               END-PERFORM
      *
      *        出力対象となっていないデータがある 
               IF      FLG-KOUI            =   1
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
      *
      *    コメントを最後に編集
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "0"
                   WHEN    "8"
                       ADD     1                           TO  IDY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC         (IDY)
                       MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD       (IDY)
                       MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU (IDY)
                       MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD    (IDY)
                       MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                      WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDY)
                 END-EVALUATE
            END-PERFORM           
      *
           INITIALIZE                      TBL-KOUI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX1
      * 
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >=      100
                   OR      WRK-KOUI-TAIHI-SRYCD (IDX)  =   SPACE
               EVALUATE    WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                           PERFORM 200741-KASAN-HENSYU-SEC
                       END-IF
                       ADD     1           TO  WRK-TBLMAX1   
                       MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
                       COMPUTE IDX     =   IDX     +   1
                   WHEN    OTHER
      *                基本項目・加算項目・通則加算項目に分ける
                       PERFORM UNTIL   IDX >=      100
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                       =   SPACE
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                                                   NOT =   "1"
                           EVALUATE    WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDX)
                               WHEN    "1"
                               WHEN    "3" 
                               WHEN    "5"
                                   IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                                       PERFORM 200741-KASAN-HENSYU-SEC
                                   END-IF
                                   MOVE    1               TO  IDZ  
                               WHEN    "7"
                                   IF      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                           =
                                                 WRK-CONS-CHOONPA-KASAN
                                     MOVE    4               TO  IDZ  
                                   ELSE
                                     IF    ( WRK-KOUI-TAIHI-TENSIKIBETU 
                                                               (IDX)
                                                         =   5   OR  6 )
                                     AND   ( WRK-KOUI-TAIHI-CHUKSNCD
                                                                 (IDX)
                                                         =   "0000"    )
                                     AND   ( RECE041-SRYKBN
                                                     NOT =   "54"      ) 
                                       MOVE    3               TO  IDZ
                                     ELSE
                                       MOVE    2               TO  IDZ
                                     END-IF
                                   END-IF   
                               WHEN    "9" 
                                   MOVE    4               TO  IDZ  
                           END-EVALUATE
                           ADD     1           TO  WRK-KOUI-TBLMAX (IDZ)
                           MOVE    WRK-KOUI-TBLMAX (IDZ)
                                               TO  IDY
                           MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                               TO
                                               WRK-KOUI-REC (IDZ IDY)
                           COMPUTE IDX     =   IDX     +   1
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
      *               
           IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
               PERFORM 200741-KASAN-HENSYU-SEC
           END-IF
           .
       20074-ZAI-KOUI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       200741-KASAN-HENSYU-SEC          SECTION.
      *
      *    加算項目の記録順
           IF      WRK-KOUI-TBLMAX (2)         >   ZERO
               COMPUTE IDY     =   WRK-KOUI-TBLMAX (2) -   1
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       IDY     
                   COMPUTE IDZ     =   IDY1    +   1
                   PERFORM VARYING IDY2    FROM   IDZ  BY  1
                           UNTIL   IDY2    >   WRK-KOUI-TBLMAX (2)
                       IF      WRK-KOUI-CHUKSNINF (2 IDY1)
                                       > WRK-KOUI-CHUKSNINF (2 IDY2)
                           MOVE    WRK-KOUI-REC (2 IDY1)  TO
                                                 TBL-KOUI-SORT
                           MOVE    WRK-KOUI-REC (2 IDY2)  TO
                                                 WRK-KOUI-REC (2 IDY1)
                           MOVE    TBL-KOUI-SORT         TO
                                                 WRK-KOUI-REC (2 IDY2)
                       END-IF
                   END-PERFORM                              
               END-PERFORM
           END-IF    
      *
      *    基本項目・加算項目・通則加算項目の順に編集
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       4
               MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDY
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       IDY
                   ADD     1               TO  WRK-TBLMAX1
                   MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
               END-PERFORM
           END-PERFORM
      *
           INITIALIZE                   WRK-KOUI-AREA
           .
       200741-KASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       20072-ZAI-HENSYU-SEC          SECTION.
      *
      *    コメントデータの編集（診療区分７０）
           IF      RECE041-SRYKBN      =   "70"                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *     
      *    診療行為データ・コメントデータの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX1
      *
      *        診療行為最終データ又は次がコメントデータの場合
               MOVE    ZERO            TO  FLG-TBLMAX
               IF      IDX             =   WRK-SRYCD1-MAX
                   CONTINUE
               ELSE
                   COMPUTE IDX1    =   IDX     +   1
                   IF      TBL-KOUI-SRYCD (IDX1) (1:1)
                                           =   "0" OR  "8"
                       MOVE    1               TO  FLG-TBLMAX
                   END-IF
               END-IF
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       MOVE    "CO,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF    
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                負担区分
                       MOVE    WRK-FTNKBN          TO  WRK-DATA
                       MOVE    1                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ
                       MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-DATA
                       MOVE    76                  TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-END 
                       PERFORM 5004-MOJI-HENSYU-SEC  
                   WHEN    OTHER     
                       MOVE    "SI,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
      *----(01.00.04) LINE ADD START ----------------------------------
                       IF    ( RECE041-SRYKBN      =   "13"  OR  "14" )
                       AND   ( IDX                 >   1              )
                       AND   ( FLG-KOKUJISKBKBN1   =   ZERO           )
                           MOVE    RECE041-SRYKBN  TO  WRK-DATA
                            MOVE    2               TO  WRK-NAGASA
                            MOVE    SPACE           TO  WRK-SRYKBN
                       END-IF
      *----(01.00.04) LINE ADD END ------------------------------------
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                負担区分
                       MOVE    WRK-FTNKBN          TO  WRK-DATA
                       MOVE    1                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                診療行為コード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                数量
                       IF      TBL-KOUI-SRYSURYO (IDX) >   ZERO
                           IF    ( RECE041-SRYKBN(1:1) =
                                                   "1" OR  "2" OR "3" )
                           AND   ( TBL-KOUI-SRYSURYO (IDX)
                                                   <=  1              )
                               CONTINUE
                           ELSE
                               IF    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                                       =   "1"        )
                               AND   ( TBL-KOUI-KZMCOMPSIKIBETU (IDX)
                                                   NOT =   "1"        )
                                   CONTINUE
                               ELSE
                                   MOVE    TBL-KOUI-SRYSURYO (IDX)
                                                       TO  WRK-SURYO
                                   MOVE    WRK-SURYO   TO  WRK-NUM
                                   MOVE    8           TO  WRK-NAGASA
                               END-IF
                           END-IF
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                点数
      *----(01.00.04) LINE ADD START ----------------------------------
      **************** IF      RECE041-SRYKBN       =   "13"  OR  "14"
      ****************      IF      IDX                 <   WRK-TBLMAX1
      ****************          IF      FLG-KOKUJISKBKBN1   =   ZERO
                       IF    ( RECE041-SRYKBN      =   "13"  OR  "14" )
                       AND   ( IDX                 <   WRK-SRYCD1-MAX )
                           IF      FLG-KOKUJISKBKBN1   =   ZERO
                               MOVE    TBL-KOUI-TEN (IDX)
                                                       TO  WRK-TENSU
                               MOVE    WRK-TENSU       TO  WRK-NUM
                               MOVE    7               TO  WRK-NAGASA
                               COMPUTE RECE041-SYUTEN1 =
                                                       RECE041-SYUTEN1
                                                   -   TBL-KOUI-TEN(IDX)
                           ELSE
                               IF    ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                                           =   "1" )
                               AND   ( TBL-KOUI-DATAKBN (IDX)
                                                           =   "2" )
                                   MOVE    TBL-KOUI-TEN (IDX)
                                                       TO  WRK-TENSU
                                   MOVE    WRK-TENSU   TO  WRK-NUM
                                   MOVE    7           TO  WRK-NAGASA
                                   COMPUTE RECE041-SYUTEN1 =
                                                       RECE041-SYUTEN1
                                                   -   TBL-KOUI-TEN(IDX)
                               ELSE
                                   IF      IDX         =
                                                   FLG-KOKUJISKBKBN1
                                       COMPUTE WRK-TENSU     =
                                           RECE041-SYUTEN1   -
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
                                       MOVE    WRK-TENSU
                                                       TO  WRK-NUM
                                       MOVE    7       TO  WRK-NAGASA
                                       COMPUTE RECE041-SYUTEN1 =
                                               RECE041-SYUTEN1     -
                                               WRK-TENSU
                                   END-IF
                               END-IF 
                           END-IF
                       END-IF
      *----(01.00.04) LINE ADD END ------------------------------------
      *****************IF      IDX                 =   WRK-TBLMAX1
                       IF    ( IDX                 =   WRK-SRYCD1-MAX )
      *****            OR    ( FLG-TBLMAX          =   1              ) 
                           IF      RECE041-SYUTEN1     >   ZERO
                               IF      RECE041-SRYKBN      =   "70"
                                 COMPUTE WRK-TENSU =   RECE041-SYUTEN1
                                                   -   RECE041-SYUTEN2
                               ELSE
                                   IF      FLG-GAITENTTLKBN    =   ZERO
                                       MOVE    RECE041-SYUTEN1
                                                           TO  WRK-TENSU
                                   ELSE
                                       MOVE    FLG-GAITENTTLKBN
                                                           TO  IDY
                                       COMPUTE WRK-TENSU =   
                                           RECE041-SYUTEN1     -
                                           WRK-CONS-JIKANGAI-TEN (IDY)
                                       ADD RECE041-ZAIKAISU
                                                           TO
                                           TBL-KOUI-SRYKBN12-KAISU (1)
                                       MOVE
                                           WRK-CONS-JIKANGAI-SRYCD (IDY)
                                                           TO
                                           TBL-KOUI-SRYKBN12-SRYCD (1)    
                                       MOVE WRK-CONS-JIKANGAI-TEN (IDY)
                                                           TO
                                           TBL-KOUI-SRYKBN12-TEN   (1)    
                                   END-IF
                               END-IF
                               MOVE    WRK-TENSU         TO  WRK-NUM
                               MOVE    7                 TO  WRK-NAGASA
                           END-IF
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                回数
      *----(01.00.04) LINE ADD START ----------------------------------
      **************** IF      RECE041-SRYKBN      =   "13"  OR  "14"
      ****************     IF      IDX                 <   WRK-TBLMAX1
      ****************         IF      FLG-KOKUJISKBKBN1   =   ZERO
      ****************             IF      RECE041-ZAIKAISU    >   ZERO
                       IF    ( RECE041-SRYKBN      =   "13"  OR  "14" )
                       AND   ( RECE041-ZAIKAISU    >   ZERO           )
                       AND   ( IDX                 <   WRK-SRYCD1-MAX )
                           IF      FLG-KOKUJISKBKBN1   =   ZERO
                               MOVE    RECE041-ZAIKAISU
                                                       TO  WRK-KAISU
                               MOVE    WRK-KAISU       TO  WRK-NUM
                               MOVE    3               TO  WRK-NAGASA
                           ELSE
                               IF    ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                                       =   "1"      )
                               AND   ( TBL-KOUI-DATAKBN (IDX)
                                                       =   "2"      )
                                   MOVE    RECE041-ZAIKAISU
                                                       TO  WRK-KAISU
                                   MOVE    WRK-KAISU   TO  WRK-NUM
                                   MOVE    3           TO  WRK-NAGASA
                               ELSE
                                   IF      IDX     =   FLG-KOKUJISKBKBN1
                                       MOVE    RECE041-ZAIKAISU
                                                       TO  WRK-KAISU
                                       MOVE    WRK-KAISU
                                                       TO  WRK-NUM
                                       MOVE    3       TO  WRK-NAGASA
                                   END-IF
                               END-IF
                           END-IF        
                       END-IF
      *----(01.00.04) LINE ADD END ------------------------------------
      **************** IF      IDX             =   WRK-TBLMAX1
      ****************     IF      RECE041-ZAIKAISU    >   ZERO
                       IF    ( IDX             =   WRK-SRYCD1-MAX     )
                       AND   ( RECE041-ZAIKAISU
                                               >   ZERO               )
                           MOVE    RECE041-ZAIKAISU    TO  WRK-KAISU
                           MOVE    WRK-KAISU           TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       MOVE    1                   TO  WRK-END 
                       PERFORM 5005-NUM-HENSYU-SEC
               END-EVALUATE  
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-PERFORM
      *
      *    コメントデータの編集（診療区分１３・１４）
           IF      RECE041-SRYKBN      =   "13"    OR  "14"                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *
      *    医薬品データの編集                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX6
               PERFORM 200722-YKZ-HENSYU-SEC
           END-PERFORM            
      *
      *    特定器材データの編集                 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX7
                   IF      IDXX        =   TBL-ZAIW-DATSKB (2 IDX)
                       PERFORM 200723-KIZAI-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       20072-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）編集処理（診療区分１３・１４）
      *****************************************************************
       200724-COMMENT-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    TBL-COMMENT-SRYCD (IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    TBL-COMMENT-NAME(IDX)
                                       TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       200724-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）編集処理
      *****************************************************************
       200722-YKZ-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "IY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    医薬品コード
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
               CONTINUE
           ELSE
               IF      TBL-ZAIW-SRYSURYO (1 IDX) >   ZERO
                   MOVE    TBL-ZAIW-SRYSURYO (1 IDX) TO  WRK-SRYSURYO
                   MOVE    WRK-SRYSURYO              TO  WRK-NUM
                   MOVE    9                         TO  WRK-NAGASA
                   MOVE    3                         TO  WRK-SYOSUU     
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF      IDX                 =   WRK-TBLMAX6
               IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                   MOVE    RECE041-ZAITEN      TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               ELSE
                   IF      RECE041-YKZTEN      >   ZERO
                       MOVE    RECE041-YKZTEN      TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           IF      IDX                 =   WRK-TBLMAX6
               IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
               END-IF
           END-IF        
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       200722-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（特定機材）編集処理
      *****************************************************************
       200723-KIZAI-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "TO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
      *****IF      IDX                 =   1
      *****AND     WRK-SRYKBN      NOT =   SPACE     
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-FTNKBN          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    特定器材コード
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF      TBL-ZAIW-SRYSURYO (2 IDX)   >   ZERO
               MOVE    TBL-ZAIW-SRYSURYO (2 IDX)   TO  WRK-SRYSURYO
               MOVE    WRK-SRYSURYO                TO  WRK-NUM
               MOVE    9                           TO  WRK-NAGASA
               MOVE    3                           TO  WRK-SYOSUU
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF    ( IDXX                =   1               )
           AND   ( IDX                 =   WRK-SRYCD71-MAX )
           AND   ( RECE041-SRYKBN      =   "70"            )
               MOVE    RECE041-SYUTEN2     TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           IF    ( IDXX                =   2               )
           AND   ( IDX                 =   WRK-SRYCD72-MAX )
      *********IF      RECE041-SRYKBN      =   "70"
      *********    MOVE    RECE041-KIZAITEN    TO  WRK-TENSU
      *********ELSE
      *********    COMPUTE WRK-TENSU   =   RECE041-KIZAITEN    -
      *********                            RECE041-SYUTEN2
      *********END-IF
               MOVE    RECE041-KIZAITEN    TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           IF    ( IDXX                =   1               )
           AND   ( IDX                 =   WRK-SRYCD71-MAX )
           AND   ( RECE041-SRYKBN      =   "70"            )
               IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
               END-IF
           END-IF        
           IF    ( IDXX                =   2               )
           AND   ( IDX                 =   WRK-SRYCD72-MAX )
               IF      RECE041-ZAIKAISU    >   ZERO
                   MOVE    RECE041-ZAIKAISU    TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC
      *    単位コード
           IF      TBL-ZAIW-TANICD (2 IDX)     >   ZERO
               MOVE    TBL-ZAIW-TANICD (2 IDX) TO  WRK-NUM
               MOVE    3                       TO  WRK-NAGASA
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    単価
           IF    ( TBL-ZAIW-TENSIKIBETU (2 IDX)    =   2               )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   "777770000"     )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   WRK-CONS-SANSO1
                                                   OR  WRK-CONS-SANSO2
                                                   OR  WRK-CONS-SANSO3
                                                   OR  WRK-CONS-SANSO4
                                                   OR  WRK-CONS-SANSO5
                                                   OR  WRK-CONS-SANSO6
                                                   OR  WRK-CONS-SANSO7
                                                   OR  WRK-CONS-SANSO8 )
          
               IF      TBL-ZAIW-TEN (2 IDX)  >   ZERO
                   MOVE    TBL-ZAIW-TEN (2 IDX)  TO  WRK-TANKA
                   MOVE    WRK-TANKA             TO  WRK-NUM
                   MOVE    10                    TO  WRK-NAGASA
                   MOVE    2                     TO  WRK-SYOSUU
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    特定器材名称
           IF      TBL-ZAIW-SRYCD (2 IDX)        =   "777770000"
               MOVE    TBL-ZAIW-NAME (2 IDX)     TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    商品名及び規格名及びサイズ
           MOVE    300                 TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       200723-KIZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診のときの編集処理
      *****************************************************************
       20079-SRYKBN12-JIKANGAI-HENSYU-SEC   SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               IF      TBL-KOUI-SRYKBN12-KAISU (IDX)   >   ZERO
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
                   MOVE    "SI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   MOVE    "12"                TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            負担区分
                   MOVE    WRK-FTNKBN          TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    TBL-KOUI-SRYKBN12-SRYCD (IDX)
                                                   TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   MOVE    TBL-KOUI-SRYKBN12-TEN (IDX)
                                               TO  WRK-TENSU
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    TBL-KOUI-SRYKBN12-KAISU (IDX)
                                               TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5005-NUM-HENSYU-SEC  
      *
                   PERFORM 5001-FILE-WRITE-SEC
               END-IF
           END-PERFORM
           .
       20079-SRYKBN12-JIKANGAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    県単用項目ファイル読込処理
      *****************************************************************
       301-RECE11-HENSYU-SEC           SECTION.
      *       
           INITIALIZE                      RECE11-REC
           MOVE    RECE02-KEY          TO  RECE11-KEY
           PERFORM 990-RECE11-READ-SEC
           .
       301-RECE11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       2001-HKNCOMBI-GET-SEC    SECTION.
      *
           INITIALIZE                 THKNCOMBI-AREA
      *
           MOVE    ZERO            TO FLG-HKNCOMBI
           INITIALIZE                 HKNCOMBI-REC
           MOVE    RECE02-HOSPID   TO  COMB-HOSPID
           MOVE    RECE02-PTID     TO  COMB-PTID
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-KEY2-SEL-SEC
           IF    ( FLG-HKNCOMBI    =   ZERO )
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-HKNCOMBI    =   1 )
                    OR   ( TCOMB-MAX      >=   30 )
      *
               COMPUTE TCOMB-MAX   =   TCOMB-MAX   +   1
               MOVE    HKNCOMBI-REC    TO  TCOMB-OCC (TCOMB-MAX)
      *
               PERFORM 910-HKNCOMBI-KEY2-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "HKNCOMBI-KEY2"     TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *    
           .
       2001-HKNCOMBI-GET-EXT.
           EXIT. 
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE02-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE09
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
                   MOVE    1            TO  FLG-RECE09
               NOT INVALID
                   PERFORM 990-RECE09-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE09           =   1                )
                   OR    ( RECE02-HOSPID    NOT =   RECE09-HOSPID    )
                   OR    ( RECE02-SRYYM     NOT =   RECE09-SRYYM     )
                   OR    ( RECE02-NYUGAIKBN NOT =   RECE09-NYUGAIKBN )
                   OR    ( RECE02-PTID      NOT =   RECE09-PTID      )
                   OR    ( RECE02-RECEKA    NOT =   RECE09-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE09
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   50
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
               END-EVALUATE          
               IF    ( RECE09-NUM         =   SPACE )
               AND   ( RECE09-KIGO        =   SPACE )        
               AND   ( RECE09-HKNJANUM    =   SPACE )        
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  WRK-HKN-HKNJANUM
                   IF      RECE09-HKNJANUM     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-HKNJANUM TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-HKNJANUM
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-KIGO
                   IF      RECE09-KIGO     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-NUM
                   IF      RECE09-NUM      NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF
                   MOVE    SPACE                  TO  WRK-HKN-RIREKI-X
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-HKNJANUM       DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           "−"                   DELIMITED  BY  SIZE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI-X
                   END-STRING
                   IF      WRK-HKN-RIREKI-X (53:) NOT =   SPACE           
                       STRING  WRK-HKN-TEISYUTUSAKI
                                                  DELIMITED  BY  SPACE
                               RECE09-TANSEIDONAME
                                                  DELIMITED  BY  SPACE
                               "　"               DELIMITED  BY  SIZE
                               WRK-HKN-HKNJANUM   DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                       END-STRING
                       ADD     1                   TO  WRK-RIREKI-MAX
                       IF      WRK-RIREKI-MAX     >   50
                           MOVE
                               "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           GO  TO  2001-RIREKI-HEN-EXT
                       END-IF    
                       STRING  WRK-HKN-KIGO       DELIMITED  BY  SPACE
                               "−"               DELIMITED  BY  SIZE
                               WRK-HKN-NUM        DELIMITED  BY  SPACE
                               INTO
                                   WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                       END-STRING
                   ELSE
                       MOVE    WRK-HKN-RIREKI-X   TO
                                       WRK-HKN-RIREKI (WRK-RIREKI-MAX)  
                   END-IF   
               END-IF                   
               IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                                          NOT =   SPACE
                   ADD     1                   TO  WRK-RIREKI-MAX
                   IF      WRK-RIREKI-MAX     >   50
                       MOVE    "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF    
               END-IF                             
      *
               MOVE    RECE09-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      RECE09-TEKSTYMDX(1:6)  =   RECE02-SRYYM                      
                   MOVE    LNK-DAY2-EDTYMD3(11:12)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(9:)
               ELSE
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
               END-IF                                    
      *  
               IF      RECE09-TEKEDYMDX(1:6)  =   RECE02-SRYYM                      
                   MOVE    RECE09-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(11:12)    TO
                                      WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)                                     
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX) 
               END-IF                                    
      *
               PERFORM 990-RECE09-NEXT-SEC
           END-PERFORM
      *
           IF      CNT-RECE09             =   1
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF          
      *
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴・保険履歴）
      *****************************************************************
       2008-COMMENT-HEN-01-SEC      SECTION.
      *
           MOVE    "01"                TO  WRK-COM-SRYKBN
      *
      *    手術歴編集
           PERFORM 20081-OPECOM-HEN-SEC
      *
      *    保険履歴情報
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-RIREKI-MAX  
      *
               ADD     1                   TO  WRK-COM-RENNUM
      *          
               INITIALIZE                  RECEDEN-REC
               MOVE    RECE02-HOSPID       TO  RECEDEN-HOSPID
               MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
               MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
               MOVE    RECE02-PTID         TO  RECEDEN-PTID
               IF      RECE02-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECEDEN-RECEKA
               ELSE
                   MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
               END-IF
               MOVE    RECE02-TEISYUTUSAKI TO
                                               RECEDEN-TEISYUTUSAKI
               MOVE    RECE02-RECESYUBETU  TO
                                               RECEDEN-RECESYUBETU
               IF      RECE02-HKNJANUM-KEY =   SPACE
                   MOVE    ZERO                TO  RECEDEN-HKNJANUM
               ELSE
                   MOVE    RECE02-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
               END-IF
               IF      RECE02-HOJOKBN-KEY  =   SPACE
                   MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
               ELSE
                   MOVE    RECE02-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
               END-IF   
               IF      RECE02-TEKSTYMD     =   SPACE
                   MOVE    ZERO            TO  RECEDEN-TEKSTYMD
               ELSE
                   MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
               END-IF
               MOVE    "2"                 TO  RECEDEN-RECKBN
               MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
               MOVE    WRK-PARA-SYSYMD     TO  RECEDEN-CREYMD
               INSPECT WRK-HKN-RIREKI-TBL  (IDXX)  REPLACING   ALL "  "
                                                               BY  "　"
               MOVE    WRK-HKN-RIREKI-TBL  (IDXX)
                                           TO  RECEDEN-RECEDATA
      *
               MOVE    RECEDEN-REC         TO  MCPDATA-REC
               MOVE    "RECEDEN-KEY"       TO  ORC-DBPATH
               MOVE    "DBINSERT"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"             USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC      
      *
               ADD     1                   TO  CNT-OUT
      *
      *        症状詳記（コメント）編集処理
               PERFORM 20099-COMMENT-HENSYU-SEC
           END-PERFORM
           .
       2008-COMMENT-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）編集処理
      *****************************************************************
       20081-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE02-HOSPID       TO  OPECOM-HOSPID
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "OPECOM-KEY4"       TO  WRK-OPECOM-PATH
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE02-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE02-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE02-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                                   MOVE    1       TO  FLG-OPECOM-CHK
                                   MOVE    4       TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-PATH TO  ORC-DBPATH
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-OPECOM-PATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE02-HOSPID       TO  OPECOM-HOSPID
           MOVE    RECE02-PTID         TO  OPECOM-PTID
           MOVE    RECE02-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE02-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "OPECOM-KEY5"       TO  WRK-OPECOM-PATH
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE02-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE02-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE02-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE02-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE02-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE02-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE02-KOHID  (IDY) )
                                   MOVE    1       TO  FLG-OPECOM-CHK
                                   MOVE    4       TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-PATH TO  ORC-DBPATH
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-OPECOM-PATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       20081-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）コメントの改行コード削除処理
      *****************************************************************
       200811-OPECOM-HENSYU-SEC    SECTION.
      *
           ADD     1                   TO  WRK-COM-RENNUM
      *          
           INITIALIZE                  RECEDEN-REC
           MOVE    RECE02-HOSPID       TO  RECEDEN-HOSPID
           MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECEDEN-PTID
           IF      RECE02-RECEKA       =   SPACE
               MOVE    ZERO            TO  RECEDEN-RECEKA
           ELSE
              MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO
                                               RECEDEN-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO
                                               RECEDEN-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECEDEN-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY  =   SPACE
               MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
           END-IF   
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECEDEN-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
           END-IF
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
           MOVE    WRK-PARA-SYSYMD     TO  RECEDEN-CREYMD
           INSPECT WRK-OPECOM-HEN      REPLACING   ALL "  "
                                                   BY  "　"
           MOVE    WRK-OPECOM-HEN      TO  RECEDEN-RECEDATA
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "RECEDEN-KEY"       TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC      
      *
           ADD     1                   TO  CNT-OUT
      *
      *    症状詳記（コメント）編集処理
           PERFORM 20099-COMMENT-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       200811-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（レセコメント）
      *****************************************************************
       2009-COMMENT-HEN-99-SEC      SECTION.
      *
           MOVE    "99"                TO  WRK-COM-SRYKBN
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE02-HOSPID       TO  RECECOM-HOSPID
           MOVE    RECE02-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE02-PTID         TO  RECECOM-PTID
           MOVE    RECE02-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "RECECOM-KEY1"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "RECECOM-KEY1"      TO  ORC-DBPATH
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE02-HKNCOMBI (IDY) =   ZERO
                   IF      RECECOM-HKNCOMBI    =   RECE02-HKNCOMBI(IDY)
                       PERFORM 20091-RECECOM-HENSYU-SEC                 
                       MOVE    10                  TO  IDY
                   END-IF
               END-PERFORM
      *                 
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           .
       2009-COMMENT-HEN-99-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（レセコメント）コメントの改行コード削除処理
      *****************************************************************
       20091-RECECOM-HENSYU-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       400
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1     =   IDX     -   1
                               COMPUTE IDY2     =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                                TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                                TO  WRK-COMM1
                               MOVE    400      TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G   >   SPACE
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       8
                           OR      WRK-COMMENT (IDW)   =   SPACE
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
                       INITIALIZE                  RECEDEN-REC
                       MOVE    RECE02-HOSPID       TO  RECEDEN-HOSPID
                       MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
                       MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
                       MOVE    RECE02-PTID         TO  RECEDEN-PTID
                       IF      RECE02-RECEKA       =   SPACE
                           MOVE    ZERO            TO  RECEDEN-RECEKA
                       ELSE
                           MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
                       END-IF
                       MOVE    RECE02-TEISYUTUSAKI TO
                                                   RECEDEN-TEISYUTUSAKI
                       MOVE    RECE02-RECESYUBETU  TO
                                                   RECEDEN-RECESYUBETU
                       IF      RECE02-HKNJANUM-KEY =   SPACE
                           MOVE    ZERO                TO
                                                   RECEDEN-HKNJANUM
                       ELSE
                           MOVE    RECE02-HKNJANUM-KEY TO
                                                   RECEDEN-HKNJANUM
                       END-IF
                       IF      RECE02-HOJOKBN-KEY  =   SPACE
                           MOVE    ZERO                TO
                                                   RECEDEN-HOJOKBN-KEY
                       ELSE
                           MOVE    RECE02-HOJOKBN-KEY  TO
                                                   RECEDEN-HOJOKBN-KEY
                       END-IF   
                       IF      RECE02-TEKSTYMD     =   SPACE
                           MOVE    ZERO            TO  RECEDEN-TEKSTYMD
                       ELSE
                           MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
                       END-IF
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       MOVE    WRK-PARA-SYSYMD     TO  RECEDEN-CREYMD
                       MOVE    WRK-COMMENT (IDW)   TO  RECEDEN-RECEDATA
      *
                       MOVE    RECEDEN-REC         TO  MCPDATA-REC
                       MOVE    "RECEDEN-KEY"       TO  ORC-DBPATH
                       MOVE    "DBINSERT"          TO  MCP-FUNC
                       CALL    "ORCMCPSUB"             USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC      
      *
                       ADD     1                   TO  CNT-OUT
      *
      *                症状詳記（コメント）編集処理
                       PERFORM 20099-COMMENT-HENSYU-SEC
                     END-PERFORM
                 END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       20091-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（コメント）編集処理
      *****************************************************************
       20099-COMMENT-HENSYU-SEC       SECTION.
      *
           IF      SYS-2005-RECECOMKBN =   "1"
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-COM-SRYKBN      NOT =   SPACE     
               MOVE    WRK-COM-SRYKBN      TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-COM-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           MOVE    WRK-COM-FTNKBN      TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    "810000001"         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    RECEDEN-RECEDATA    TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       20099-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE02-FILE
                   RECE03-FILE
                   RECE041-FILE
                   RECE05-FILE
                   RECE09-FILE
                   RECE11-FILE
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
           DISPLAY "RECE02  CNT-IN  :" CNT-IN  
           DISPLAY "RECE04  CNT-IN  :" CNT-RECE04  
           DISPLAY "DB      CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR0300 END ***"
      *     ACCEPT  WRK-IN FROM CONSOLE 
      *
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
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
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING        
      *
           ADD     1                   TO  WRK-RENNUM
      *          
           INITIALIZE                  RECEDEN-REC
           MOVE    RECE02-HOSPID       TO  RECEDEN-HOSPID
           MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECEDEN-PTID
           IF      RECE02-RECEKA       =   SPACE
               MOVE    ZERO            TO  RECEDEN-RECEKA
           ELSE
               MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECEDEN-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
           END-IF   
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECEDEN-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
           END-IF
           MOVE    "1"                 TO  RECEDEN-RECKBN
           MOVE    WRK-RENNUM          TO  RECEDEN-RENNUM
           MOVE    WRK-PARA-SYSYMD     TO  RECEDEN-CREYMD
           IF      FLG-TOTALTEN        =   1
               MOVE    SUM-TOTALTEN        TO  RECEDEN-TOTALTEN
           END-IF
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "RECEDEN-KEY"       TO  ORC-DBPATH
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC      
      *
           ADD     1                   TO  CNT-OUT
           MOVE    ZERO                TO  FLG-TOTALTEN
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    ZERO                TO  WRK-SYMD
           END-IF    
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    右ずめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW2
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
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
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *????
      *     IF      FLG-HKNCOMBI        =   1
      *         DISPLAY "HKNCOMBI INV " COMB-HOSPID " " COMB-PTID " "
      *                                 COMB-HKNCOMBINUM " "    
      *     END-IF
      *???? 
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ検索処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    "HKNCOMBI-KEY2" TO  ORC-DBPATH
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO         TO FLG-HKNCOMBI
           ELSE
               MOVE    1            TO FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスタ読込処理(KEY2)
      *****************************************************************
       910-HKNCOMBI-KEY2-FETCH-SEC     SECTION.
      *
           MOVE    "HKNCOMBI-KEY2" TO  ORC-DBPATH
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO        TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC TO  HKNCOMBI-REC
           ELSE
               MOVE    1           TO  FLG-HKNCOMBI
               INITIALIZE              HKNCOMBI-REC
           END-IF
      *
           .
       910-HKNCOMBI-KEY2-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "BYOMEI-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "BYOMEI-KEY"           TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "BYOMEI-KEY"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    RECE02-SRYYM        TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    ORC-DBYMD           TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  ORC-DBYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "DBFETCH"           TO  MCP-FUNC
                   MOVE    "TENSU-KEY"         TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"         USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           END-IF
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
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
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
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
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE02-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1            
      *
               READ    RECE02-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                       MOVE    HIGH-VALUE  TO  KEY-NEW
                   NOT AT  END
                       IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
                                                   OR  "2" )   AND
                             ( RECE02-TEISYUTUSAKI
                                                   =   "1"  )      )
                       OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
                                                   OR  "2"  )  AND
                             ( RECE02-TEISYUTUSAKI
                                                   =   "2"  )      )
                       OR  (   RECE02-PTNUM        =   SPACE       )
                       OR  (   RECE02-TSTPTNUMKBN  =   "1"         )
                       OR  (   RECE02-RECESYUBETU  =   ZERO   OR 
                                                       8888        )
                           CONTINUE
                       ELSE
      *
      *                    医療機関情報取得
                           IF      WRK-PARA-SRYYM  =   ZERO
                               MOVE  RECE02-SRYYM      TO
                                                   WRK-SYS-SRYYMD(1:6)
                               MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD(7:2)
                               PERFORM 900-SYSKANRI-HENSYU-SEC
                           END-IF
      *
      *                    山形地方公費対応（国保）
                           IF    ( SYS-1001-PREFNUM    =   "06" )
                           AND   ( RECE02-TEISYUTUSAKI =   "2"  ) 
                               IF    ( RECE02-KOHNUM (1) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (2) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (3) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (4) (1:1)
                                                       =   "1" OR  "2" )
                                   CONTINUE
                               ELSE
                                   MOVE    1               TO  FLG-READ
                               END-IF
                           ELSE
      *
      *                      福岡地方公費対応（国保）
                             IF    ( SYS-1001-PREFNUM    =   "40" )
                             AND   ( RECE02-TEISYUTUSAKI =   "2"  ) 
                               IF    ( RECE02-KOHNUM (1) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (2) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (3) (1:1)
                                                       =   "1" OR  "2" )
                               OR    ( RECE02-KOHNUM (4) (1:1)
                                                       =   "1" OR  "2" )
                                   IF    ( RECE02-KOHNUM (1)   =
                                                       "141" OR "142" )
                                   OR    ( RECE02-KOHNUM (2)    =
                                                       "141" OR "142" )
                                   OR    ( RECE02-KOHNUM (3)    =
                                                       "141" OR "142" )
                                   OR    ( RECE02-KOHNUM (4)    =
                                                       "141" OR "142" )
                                       MOVE    1           TO  FLG-READ
                                   END-IF
                               ELSE
                                   MOVE    1           TO  FLG-READ
                               END-IF
                             ELSE
                               MOVE    1               TO  FLG-READ
                             END-IF
                           END-IF
                       END-IF    
      *
                       IF      FLG-READ            =   1
                           ADD     1           TO  CNT-IN
                           MOVE    RECE02-RECEKA 
                                               TO  KEY-N-SRYKA
      *
                           DISPLAY "RECE02  PTNUM=" RECE02-PTNUM     
                       END-IF
               END-READ
           END-PERFORM
           .
       910-RECE02-READ-EXT.
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
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-OPECOM-PATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-PATH          TO  ORC-DBPATH
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE03-NEXT-SEC           SECTION.
      *
           READ    RECE03-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE03
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE03
           END-READ
      *
           .
       990-RECE03-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE041-NEXT-SEC           SECTION.
      *
           READ    RECE041-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE04
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE04
                   ADD     1         TO  CNT-RECE04
           END-READ
      *
           .
       990-RECE041-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE05-NEXT-SEC           SECTION.
      *
           READ    RECE05-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE05
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE05
           END-READ
      *
           .
       990-RECE05-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０９データ読み
      ****************************************************************
       990-RECE09-NEXT-SEC           SECTION.
      *
           READ    RECE09-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE09
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE09
           END-READ
      *
           .
       990-RECE09-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト１１データ読み
      ****************************************************************
       990-RECE11-READ-SEC           SECTION.
      *
           READ    RECE11-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE11
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE11
           END-READ
      *
           .
       990-RECE11-READ-EXT.
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
