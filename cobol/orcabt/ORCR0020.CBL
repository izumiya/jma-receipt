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
       PROGRAM-ID.             ORCR0020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 保険情報編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-小豆沢   01.12.10  自費保険のレセプト種別対応
      *                                     追加
      *  01.00.02    MCC-藤原     02.02.08  氏名が変更されたときの
      *                                     編集の追加
      *  01.01.03    MCC-藤原     02.02.13  同一保険者で記号番号が変わっ
      *                                     たとき任継・継の変更の時一枚に
      *                                     印刷するように修正
      *                                     保険組合せを３から１０に変更
      *  01.00.04   MCC-藤原      02.02.15  特記事項（老保）の編集の追加
      *  01.00.05   MCC-藤原      02.03.13  同一保険者で記号番号が変わった
      *                                     とき保険の履歴を表示するように修正
      *  01.00.06   MCC-小豆沢    02.03.25  長期特定疾患の場合、レセプトに
      *                                     印字しないように修正
      *  01.00.07   MCC-藤原      02.03.26  老人保険情報の編集の修正
      *  01.00.08   MCC-藤原      02.04.02  県単用項目ファイルのクリア処理追加
      *  01.00.09   MCC-藤原      02.05.28  訂正区分＝１のときレセプト種別は
      *                                     不明分とする
      *  01.00.10   MCC-藤原      02.07.18  生保の受給者番号を月代わり受給者
      *                                     番号マスタから編集する
      *  01.00.11   NACL-藤原     02.08.23  自費を不明分と分ける
      *  01.00.12   NACL-藤原     02.09.05  請求書用ファイルのみ端末ＩＤを固定
      *                                     にする
      *  01.00.13   NACL-小豆沢   02.09.24  （平成１４年１０月改正対応）
      *                                     レセプト種別コード追加
      *  01.00.14   NACL-小豆沢   02.09.27  （平成１４年１０月改正対応）
      *                                     老人保険の場合も給付割合のセットを
      *                                     を行う
      *  01.00.15   NACL-門脇     02.10.07  （平成１４年１０月改正対応）
      *                                     低所得フラグのセットを行う
      *  01.00.16   NACL-小豆沢   02.10.16  （平成１４年１０月改正対応）
      *                                     国保退職者の給付割合セット
      *  01.00.17   NACL-藤原     02.11.06  主保険が自費のとき保険履歴を作成
      *                                     しないようにする 
      *  01.00.18   NACL-藤原     02.11.25  前期高齢者の船員保険のときの
      *                                     レセプト種別の編集を修正 
      *                                     前期高齢者の共済組合の下船３ヶ月の
      *                                     ときのレセプト種別の編集を修正 
      *  01.00.19   NACL-小豆沢   02.12.04  退職者国保の３才未満の給付割合表示
      *                                     を修正
      *  01.00.20   NACL-藤原     03.01.17  特記事項０４老保の判定に年齢判定
      *                                     を追加する
      *  01.00.21   NACL-藤原     03.01.20  月代り受給者番号に入外区分を追加
      *  01.00.22   NACL-藤原     03.01.30  特記事項０４老保の判定にを７０歳
      *                                     を追加
      *  01.00.23    NACL-藤原    03/02/12  テスト患者番号区分・補助区分の追加
      *  01.00.24    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *                                     診療科数１０から２０へ変更
      *  01.00.25    NACL-門脇    03/03/07  在総診フラグセット
      *  01.00.26    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.27    NACL-門脇    03/03/18  在総診フラグセットの修正
      *  01.00.28    NACL-門脇    03/04/16  船員保険（被保険者）＋通勤災害
      *                                     フラグセット
      *  01.00.29    NACL-藤原    03/04/21  地方公費データのとき主保険が
      *                                     無い場合は不明分とする
      *  01.00.30    NACL-小豆沢  03/05/30  山形県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *  01.00.31    NACL-藤原    03/10/07  福岡県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *                                     （法別番号４１以外のとき）
      *  01.00.32    NACL-藤原    03/10/27  患者情報の編集の修正
      *                                     県番号の追加
      *  01.00.33    NACL-門脇    03/11/27  ”老保”編集条件修正
      *  01.00.34    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT5→31
      *  01.00.35    NACL-藤原    04/02/04  月途中に老保の変更があった場合
      *                                     のＫＥＹの編集の修正
      *  01.00.36    NACL-門脇    04/02/12  年齢判定文修正
      *  01.00.37    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.38    NACL-門脇    04/04/06  公費単独７０歳未満で老人保険入力
      *                                     のある方の在総診フラグ編集修正
      *  01.00.39    NACL-藤原    04/05/27  同一開始日の複数自費レセプトが
      *                                     別々に作成されるように修正
      *  01.00.40    NACL-藤原    04/06/08  適用終了日の追加
      *  01.00.41    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *  01.00.42    NACL-藤原    04/07/12  生保の受給者番号を月代わり受給者DBに
      *                                     無い場合は公費DBより編集する
      *                                     保険組合せの公費ＩＤを追加
      *  01.00.43    NACL-藤原    04/07/20  特例退職者＋老人のとき不明分
      *                                     とする
      *  01.00.44    NACL-藤原    04/07/23  補助区分の追加に伴うレセプト
      *                                     種別・給付割合の編集を変更
      *  01.00.45    NACL-藤原    04/10/04  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *  01.00.46    NACL-門脇    04/10/05  公費単独の在総診フラグセットの修正
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/09/28  中止処理の追加
      *  01.03.02    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.03.03    NACL-門脇    05/02/25  特記事項”老保”編集修正
      *  01.03.04    NACL-藤原    05/05/11  沖縄県対応
      *                                     精神障害者特別措置(121)の場合
      *                                     主保険なしでも不明分としない
      *  01.03.05    NACL-藤原    05/05/17  神奈川県の障害(180､280)は
      *                                     国保で負担者番号が空白の場合
      *                                     公費カウントしない
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE01-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE01-KEY
                                   FILE    STATUS  IS  STS-RECE01.
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE022-FILE    ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE022-KEY
                                   FILE    STATUS  IS  STS-RECE02.
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
           SELECT  RECE091-FILE    ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE091-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    レセプト（県単用項目）データ
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE01-FILE.
       01  RECE01-REC. 
           COPY    "CPRCF001.INC".
      *
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
       FD  RECE022-FILE.
       01  RECE022-REC. 
           COPY    "CPRCF002.INC"  REPLACING  //RECE02//
                                   BY         //RECE022//.
      *
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
       FD  RECE091-FILE.
       01  RECE091-REC. 
           COPY    "CPRCF009.INC"  REPLACING  //RECE09//
                                   BY         //RECE091//.
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
           COPY    "CPCOMMONDAT2.INC".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE02//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE09//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE11//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE01                              PIC X(02).
           03  STS-RECE02                              PIC X(02).
           03  STS-RECE09                              PIC X(02).
           03  STS-RECE11                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE01                              PIC 9(01). 
           03  FLG-RECE09                              PIC 9(01). 
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-CHK                                 PIC 9(01).
           03  FLG-MONTHLYNUM                          PIC 9(01).
           03  FLG-TSYRRK                              PIC 9(01).
           03  FLG-TNKRRK                              PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-ROUJINHOKEN                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE09                              PIC 9(06). 
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-TBL-OCC                             OCCURS  2. 
               05  CNT-IN                              PIC 9(06).
               05  CNT-OUT                             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(03).
           03  IDZ                                     PIC 9(02).
           03  TOKKI-IDX                               PIC 9(02).
           03  SYSKAN-IDX                              PIC 9(03).
      *
      *    パス名領域
       01  WK-PATH                                     PIC X(20).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(01).
           03  WK-HONKZKKBN                            PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                                PIC X(64).
           03  WRK-PARA.
      *********05  WRK-PARA-PREFNUM                    PIC  X(02). 
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1              PIC 9(04).
               05  WRK-MMS1              PIC 9(02).
               05  WRK-DDS1              PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2              PIC 9(04).
               05  WRK-MMS2              PIC 9(02).
               05  WRK-DDS2              PIC 9(02).
           03  WRK-YMDS2R    REDEFINES   WRK-YMDS2.
               05  WRK-YYMMS2R           PIC 9(06).
               05  WRK-DDS2R             PIC 9(02).
      *
           03  WRK-RECEERR               PIC X(200).
      *
           03  WRK-HKNNUM                PIC X(03).
      *
           03  WRK-JIHI-FLG              PIC X(01).
      *
           03  WRK-CHOKI                 PIC X(03).
      *
           03  WRK-ZAITAKU               PIC X(01).
      *
           03  RJN-RATE-KBN              PIC X(01).
      *
           03  WRK-RECESKYKBN            PIC  X(01).
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM
                                   PIC X(03)   VALUE   "012". 
      *    
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM
                                   PIC X(03)   VALUE   "980". 
      *    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
      *    在総診テーブル
       01  TBL-ZAITAKU-AREA.
      *    寝たきり老人在宅総合診療料（院内処方せんを交付する）
           03  FILLER          PIC X(09)   VALUE   "114701210".
      *    寝たきり老人在宅総合診療料（院内処方せんを交付しない）
           03  FILLER          PIC X(09)   VALUE   "114701710".
      *    在宅末期医療総合診療料（院外）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在宅末期医療総合診療料（院内）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *
       01  TBL-ZAITAKU-R       REDEFINES   TBL-ZAITAKU-AREA.
            03  TBL-ZAITAKU    PIC X(09)   OCCURS   4.       
      *
       01  TBL-ZAITAKU-MAX     PIC 9(02)   VALUE    4.  
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
                                       SPA-AREA
                                       WRK-AREA 
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
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0020"      TO  JOB-PGID
           MOVE    "保険情報編集"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
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
                   MOVE    "RECE010"           TO  RECE01PARA-FILE-ID
                   MOVE    "RECE020"           TO  RECE02PARA-FILE-ID
                   MOVE    "RECE090"           TO  RECE09PARA-FILE-ID
                   MOVE    "RECE110"           TO  RECE11PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE02PARA-TERMID
               ELSE
      *    請求書処理
                   MOVE    "RECE01X"           TO  RECE01PARA-FILE-ID
                   MOVE    "RECE02X"           TO  RECE02PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE02PARA-TERMID
               END-IF
               MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
               MOVE    WRK-PARA-TERMID     TO  RECE09PARA-TERMID
               MOVE    WRK-PARA-TERMID     TO  RECE11PARA-TERMID
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
           MOVE    ZERO                TO  FLG-RECE01
      *
           OPEN    OUTPUT              RECE022-FILE
           CLOSE                       RECE022-FILE
      *
           IF      IDZ             =   1               
               OPEN    OUTPUT              RECE091-FILE
               CLOSE                       RECE091-FILE
      *
               OPEN    I-O                 RECE09-FILE
      *
               OPEN    OUTPUT              RECE11-FILE
               CLOSE                       RECE11-FILE
           END-IF    
      *
           OPEN    INPUT               RECE01-FILE
           IF      STS-RECE01      NOT =   ZERO
               DISPLAY "*** ORCR0020 RECE01 OPEN ERR *** " STS-RECE01 
               MOVE    1                   TO  FLG-END
               GO  TO  200-HENSYU-EXT
           END-IF
      *
           OPEN    I-O                 RECE02-FILE
      *
           PERFORM     RECE01-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE01  =   1
                                       OR      FLG-END     =   1
      *
      *        保険履歴情報編集処理
               IF      IDZ             =   1               
                   PERFORM 230-RECE09-HENSYU-SEC
               END-IF
      *            
      *        編集処理
      *        処理区分（社保・国保）により読み飛ばす
               EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "0"
                       PERFORM 210-HENSYU-SEC
                   WHEN    "1"
                       IF      RECE01-HKNNUM   NOT =   "060" AND "067"
                           PERFORM 210-HENSYU-SEC
                       END-IF
                   WHEN    "2"
                       IF      RECE01-HKNNUM       =   "060" OR  "067"
                           PERFORM 210-HENSYU-SEC
                       END-IF
               END-EVALUATE
      *
               PERFORM     RECE01-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE01-FILE
                   RECE02-FILE
      *
           IF      IDZ             =   1               
               CLOSE   RECE09-FILE
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
           MOVE    SPACE                TO  WK-RECESYUBETU-AREA
           INITIALIZE                       WK-RECESYUBETU-AREA
      *     
           MOVE    ZERO                 TO  TOKKI-IDX
      *
           MOVE    SPACE                TO  RECE02-REC
           INITIALIZE                       RECE02-REC
      *
           MOVE    SPACE                TO  RJN-RATE-KBN
      *
           MOVE    RECE01-HOSPID        TO  RECE02-HOSPID
           MOVE    RECE01-SRYYM         TO  RECE02-SRYYM
           MOVE    RECE01-NYUGAIKBN     TO  RECE02-NYUGAIKBN
           MOVE    RECE01-PTID          TO  RECE02-PTID
           MOVE    RECE01-RECEKA        TO  RECE02-RECEKA
           IF      RECE01-HKNNUM (1:2)  =   "98"
               MOVE    RECE01-HKNNUM        TO  RECE02-HKNJANUM-KEY
           ELSE  
               MOVE    RECE01-HKNJANUM      TO  RECE02-HKNJANUM-KEY
           END-IF
           MOVE    RECE01-HOJOKBN       TO  RECE02-HOJOKBN-KEY
      *****MOVE    RECE01-TEKSTYMD      TO  RECE02-TEKSTYMD
      *****MOVE    RECE01-TEKSTYMDX     TO  RECE02-TEKSTYMD
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE01-TEKSTYMDX (IDX)  =   SPACE
               IF      RECE02-TEKSTYMD     =   SPACE 
                   MOVE    RECE01-TEKSTYMDX (IDX)  TO  RECE02-TEKSTYMD
               ELSE
                   IF      RECE02-TEKSTYMD <   RECE01-TEKSTYMDX (IDX)
                       MOVE    RECE01-TEKSTYMDX (IDX)  
                                               TO  RECE02-TEKSTYMD
                   END-IF
               END-IF
               IF      RECE02-TEKSTYMD-X   =   SPACE 
                   MOVE    RECE01-TEKSTYMDX (IDX)  TO  RECE02-TEKSTYMD-X
                   MOVE    RECE01-TEKEDYMDX (IDX)  TO  RECE02-TEKEDYMD-X
               ELSE
                   IF      RECE02-TEKSTYMD-X >   RECE01-TEKSTYMDX (IDX)
                       MOVE    RECE01-TEKSTYMDX (IDX)  
                                               TO  RECE02-TEKSTYMD-X
                   END-IF
                   IF      RECE02-TEKEDYMD-X <   RECE01-TEKEDYMDX (IDX)
                       MOVE    RECE01-TEKEDYMDX (IDX)  
                                               TO  RECE02-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
      *    患者番号
           MOVE    RECE01-PTNUM        TO  RECE02-PTNUM
      *    診療科情報のセット
           PERFORM   VARYING   IDX   FROM   1   BY   1
                     UNTIL    (IDX      >   20)    OR
                              (RECE01-SRYKA(IDX)  =   SPACE)
               MOVE  RECE01-SRYKA(IDX)    TO   RECE02-SRYKA(IDX)
           END-PERFORM
      *    審査支払機関
           IF      RECE01-HKNNUM        =   "060"   OR    "067"
               MOVE    2                TO  RECE02-TEISYUTUSAKI
           ELSE
               MOVE    1                TO  RECE02-TEISYUTUSAKI
           END-IF
      *    患者情報の取得
           MOVE    RECE01-KANANAME     TO  RECE02-KANANAME
           MOVE    RECE01-NAME         TO  RECE02-NAME
           MOVE    RECE01-SEX          TO  RECE02-SEX
           MOVE    RECE01-BIRTHDAY     TO  RECE02-BIRTHDAY
           MOVE    RECE01-DEATHKBN     TO  RECE02-DEATHKBN
           MOVE    RECE01-TSTPTNUMKBN  TO  RECE02-TSTPTNUMKBN
           MOVE    RECE01-AGE          TO  WRK-NENREI
                                           RECE02-AGE
      *    県番号
           MOVE    RECE01-PREFNUM      TO  RECE02-PREFNUM
      *    患者保険情報テーブルから主保険情報の取得
           IF      RECE01-HKNID            NOT =   ZERO
               MOVE    RECE01-HKNNUM        TO  RECE02-HKNNUM
               MOVE    RECE01-HKNID         TO  RECE02-HKNID
      *        法別番号のセット
               INITIALIZE                  HKNNUM-REC
               MOVE   RECE01-HOSPID    TO  HKN-HOSPID
      *********MOVE   PTHKN-HKNNUM     TO  HKN-HKNNUM
               MOVE   RECE02-HKNNUM    TO  HKN-HKNNUM
               MOVE   HKNNUM-REC       TO  MCPDATA-REC
               MOVE   "DBSELECT"       TO  MCP-FUNC
               MOVE   "HKNNUM-KEY2"    TO  ORC-DBPATH
               CALL   "ORCMCPSUB"       USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               IF     MCP-RC           =   ZERO
                   MOVE    "HKNNUM-KEY2"   TO  ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   PERFORM  UNTIL
                            (FLG-HKNNUM    =    1)      OR
                            ((HKN-TEKSTYMD (1:6)
                                           <=  RECE01-SRYYM      ) AND
                            (RECE01-SRYYM  <=  HKN-TEKEDYMD (1:6))     )
                       MOVE   "HKNNUM-KEY2" TO  ORC-DBPATH
                       PERFORM   HKNNUM-READ-SEC
                   END-PERFORM
                   IF       FLG-HKNNUM     =   ZERO
                       MOVE   HKN-HBTNUM TO RECE02-HKNHBTNUM
                       MOVE   SPACE        TO    WRK-JIHI-FLG
                       IF     HKN-HKNKOHSBTKBN    =    "8"
                            MOVE   "1"     TO    WRK-JIHI-FLG
                       END-IF
                   END-IF
               END-IF
               MOVE   "HKNNUM-KEY2"       TO  WK-PATH
               PERFORM   CSR-CLOSE-SEC
               MOVE    RECE01-HKN-HKNJANUM  TO  RECE02-HKNJANUM
               MOVE    RECE01-HKN-HONKZKKBN TO  RECE02-HONKZKKBN
                                                WK-HONKZKKBN
               MOVE    RECE01-HKN-HOJOKBN   TO  RECE02-HOJOKBN
               MOVE    RECE01-CONTKBN       TO  RECE02-CONTKBN
               MOVE    RECE01-KIGO          TO  RECE02-KIGO
               MOVE    RECE01-NUM           TO  RECE02-NUM
               MOVE    RECE01-CERTNUM       TO  RECE02-CERTNUM
           END-IF
      *    患者公費情報テーブルから老人保険情報の取得
           IF    ( RECE01-KOHHKNNUM-O (1)  =   "027" )
           OR    ( RECE01-KOHID            >   ZERO  )
               INITIALIZE                      PTKOHINF-REC
               MOVE    RECE01-HOSPID       TO  PTKOH-HOSPID
               MOVE    RECE01-PTID         TO  PTKOH-PTID
               MOVE    RECE01-KOHID        TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
                   PERFORM PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF        =   ZERO
                       MOVE    PTKOH-KOHID         TO  RECE02-RJNHKNID
                       MOVE    PTKOH-KOHNUM        TO  RECE02-RJNHKNNUM
                       IF    ( PTKOH-TEKSTYMD (1:6)
                                                   =   RECE01-SRYYM    )
                       AND   ( PTKOH-TEKSTYMD      >   RECE02-TEKSTYMD )
                           MOVE    PTKOH-TEKSTYMD  TO  RECE02-TEKSTYMD
                       END-IF
      *                法別番号のセット
                       INITIALIZE                  HKNNUM-REC
                       MOVE   RECE01-HOSPID    TO  HKN-HOSPID
                       MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                       MOVE   HKNNUM-REC       TO  MCPDATA-REC
                       MOVE   "DBSELECT"       TO  MCP-FUNC
                       MOVE   "HKNNUM-KEY2"    TO  ORC-DBPATH
                       CALL   "ORCMCPSUB"      USING   MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                       IF      MCP-RC          =   ZERO
                           MOVE    "HKNNUM-KEY2"   TO  ORC-DBPATH
                           PERFORM  HKNNUM-READ-SEC
                           PERFORM  UNTIL
                                 (FLG-HKNNUM     =    1)      OR
                                ((HKN-TEKSTYMD(1:6) <=
                                                   RECE01-SRYYM) AND
                                 (RECE01-SRYYM      <=
                                                   HKN-TEKEDYMD(1:6)))
                               MOVE   "HKNNUM-KEY2"  TO  ORC-DBPATH
                               PERFORM   HKNNUM-READ-SEC
                           END-PERFORM
                           IF       FLG-HKNNUM     =   ZERO
                               MOVE   HKN-HBTNUM TO RECE02-RJNHBTNUM
                           END-IF
                       END-IF
                       MOVE   PTKOH-FTNJANUM   TO  RECE02-RJNFTNJANUM
                       MOVE   PTKOH-JKYSNUM    TO  RECE02-RJNJKYSNUM
      *----(01.00.14) LINE ADD START --------------------------
      *                平成１４年１０月対応
      *                老人保険の患者について負担割合を退避
                       IF   RECE02-SRYYM   >=   "200210"
                            EVALUATE    PTKOH-PAYKBN
                                WHEN    "01"
                                    MOVE      "1"   TO  RJN-RATE-KBN
                                WHEN    "02"
                                    MOVE      "2"   TO  RJN-RATE-KBN
                            END-EVALUATE
                       END-IF
      *----(01.00.14) LINE ADD END   --------------------------
                   ELSE
                        DISPLAY  "PTKOHINF FETCH ERR" 
                   END-IF
               ELSE
                   DISPLAY   "PTKOHINF SELECT ERR" 
               END-IF
           END-IF
           MOVE   "PTKOHINF-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
           MOVE   "HKNNUM-KEY2"       TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    長期高額療養費の編集
           MOVE    SPACE              TO   WRK-CHOKI
           PERFORM   VARYING   IDX   FROM    1   BY   1
                     UNTIL    (IDX     >   4)    
                     OR       (RECE01-KOHHKNNUM-O(IDX)  =   SPACE)
               IF    RECE01-KOHHKNNUM-O(IDX)     =    "027"
                   CONTINUE
               ELSE
                 INITIALIZE                        HKNNUM-REC
                 MOVE    RECE01-HOSPID         TO  HKN-HOSPID
                 MOVE    RECE01-KOHHKNNUM-O(IDX) TO  HKN-HKNNUM
                 MOVE    "00"                  TO  HKN-PAYKBN
                 MOVE    RECE01-SRYYM          TO  HKN-TEKSTYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKSTYMD(7:2)
                 MOVE    RECE01-SRYYM          TO  HKN-TEKEDYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKEDYMD(7:2)
                 MOVE    HKNNUM-REC            TO  MCPDATA-REC
                 MOVE    "DBSELECT"            TO  MCP-FUNC
                 MOVE    "HKNNUM-KEY"          TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"            USING   MCPAREA
                                                        ORCMCPAREA
                                                        MCPDATA-REC
                 IF      MCP-RC                =   ZERO
                   MOVE    "HKNNUM-KEY"        TO  ORC-DBPATH
                   PERFORM  HKNNUM-READ-SEC
                   IF       FLG-HKNNUM    =   0
                       IF   HKN-HKNKOHSBTKBN   =   "2"
                           ADD      1           TO  TOKKI-IDX
                           MOVE    "02"         TO  RECE02-TOKKI
                                                       (TOKKI-IDX)
                           MOVE    RECE01-KOHHKNNUM-O(IDX)  TO
                                                    WRK-CHOKI 
                           MOVE    1            TO  FLG-HKNNUM
                           MOVE    4            TO  IDX  
                       END-IF     
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
      *    公費情報のセット
           MOVE    ZERO        TO  IDX2
           PERFORM   VARYING   IDX    FROM   1   BY   1
                     UNTIL    (IDX          >     4)
                     OR       (RECE01-KOHID-O(IDX)  =   ZERO)  
      ******** IF  (RECE01-KOHHKNNUM-O (IDX)  =       "027")  OR
      ********     (RECE01-KOHHKNNUM-O (IDX)  =   WRK-CHOKI)
               IF  (RECE01-KOHHKNNUM-O (IDX)  =       "027")
                     CONTINUE
               ELSE
                   IF      RECE01-KOHHKNNUM-O (IDX)  =   WRK-CHOKI
                       MOVE   "4"             TO  WRK-RECESKYKBN
                   ELSE
                       MOVE   RECE01-RECESKYKBN-O (IDX)
                                              TO  WRK-RECESKYKBN
                   END-IF
                   PERFORM   KOHI-SET-SEC
               END-IF
           END-PERFORM
      *
      *    レセプト種別
           PERFORM   RECESYUBETU-SET-SEC
      *
      *    主保険給付割合のセット
           PERFORM   KYURATE-SET-SEC
      *
      *    保険組合せ番号情報
           PERFORM VARYING IDX    FROM   1   BY   1
                   UNTIL   IDX          >     10
                   OR          RECE01-HKNCOMBI-O(IDX)  =   ZERO
               MOVE    RECE01-HKNCOMBI-O(IDX)
                                       TO  RECE02-HKNCOMBI(IDX)
           END-PERFORM    
      *
      *    組合せ公費保険番号情報のセット
           PERFORM   KOHNUMHC-SET-SEC
      *
      *****補足区分・レセプト記載外公費のセット
      *****IF      RECE01-ETC-HKNNUM   NOT =   SPACE
      *****    MOVE    "1"                 TO  RECE02-HOSOKUKBN
      *****    MOVE    RECE01-ETC-HKNNUM   TO  RECE02-ETC-KOHNUM (1)
      *****END-IF  
      *****IF      WRK-CHOKI           NOT =   SPACE
      *****    PERFORM VARYING IDX FROM    1   BY  1
      *****            UNTIL   IDX >       4
      *****        IF         RECE02-ETC-KOHNUM (IDX)  =   SPACE
      *****            MOVE    WRK-CHOKI   TO  RECE02-ETC-KOHNUM (IDX)
      *****            MOVE    4           TO  IDX
      *****        END-IF
      *****    END-PERFORM  
      *****END-IF  
      *
      *    特記事項のセット
      *    老保の編集
           MOVE    ZERO       TO     FLG-ROUJINHOKEN
           IF      RECE02-HKNNUM        =   SPACE
      ***  AND     RECE02-KOHHBTNUM (1)   NOT =  SPACE
      ***  AND     RECE02-KOHHBTNUM (2) =   SPACE
      ***  AND     RECE02-KOHHBTNUM (3) =   SPACE
      ***  AND     RECE02-KOHHBTNUM (4) =   SPACE
               IF  (   RECE01-SRYYM         >=  "200210"            )
               AND ((  RECE02-AGE           >=  75             ) OR 
                    (( RECE02-BIRTHDAY      <=  19320930 ) AND
                     ( RECE02-AGE           >=  70       )     )    )
                   ADD      1           TO  TOKKI-IDX
                   MOVE    "04"         TO  RECE02-TOKKI (TOKKI-IDX)
               ELSE
                   INITIALIZE                       PTKOHINF-REC
                   MOVE    RECE01-HOSPID        TO  PTKOH-HOSPID
                   MOVE    RECE01-PTID          TO  PTKOH-PTID
                   MOVE    "027"                TO  PTKOH-KOHNUM
                   MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                   MOVE    "DBSELECT"           TO  MCP-FUNC
                   MOVE    "PTKOHINF-KEY4"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"           USING   MCPAREA
                                                         ORCMCPAREA
                                                         MCPDATA-REC
                   IF      MCP-RC               =       ZERO
                       MOVE    "PTKOHINF-KEY4"  TO      ORC-DBPATH
                       PERFORM  PTKOHINF-READ-SEC
                       PERFORM  UNTIL   FLG-PTKOHINF    =   1
                           IF      PTKOH-TEKSTYMD(1:6)  <=
                                                    RECE01-SRYYM
                           AND     RECE01-SRYYM         <=
                                                    PTKOH-TEKEDYMD(1:6)
                           AND     PTKOH-SAKJOKBN   =  SPACE
                               ADD      1           TO  TOKKI-IDX
                               MOVE    "04"         TO  RECE02-TOKKI
                                                            (TOKKI-IDX)
                               MOVE    1            TO  FLG-ROUJINHOKEN
                               MOVE    1            TO  FLG-PTKOHINF     
                           ELSE
                              MOVE    "PTKOHINF-KEY4"  TO  ORC-DBPATH
                              PERFORM  PTKOHINF-READ-SEC
                           END-IF   
                       END-PERFORM
                   END-IF
               END-IF
           END-IF              
      *
           IF   RECE01-SRYYM    >=  "200210"
      *
      *      年金履歴より
             INITIALIZE                      TNKRRK-REC
             MOVE    RECE01-HOSPID       TO  TNKRRK-HOSPID
             MOVE    RECE01-PTID         TO  TNKRRK-PTID
             MOVE    TNKRRK-REC          TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "TNKRRK-KEY3"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"          USING     MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "TNKRRK-KEY3"      TO  ORC-DBPATH
                 PERFORM TNKRRK-READ-SEC
                 PERFORM  UNTIL  
                       ( FLG-TNKRRK          =   1                ) OR
                      (( TNKRRK-STYMD(1:6)  <=  RECE01-SRYYM   AND
                         RECE01-SRYYM       <=  TNKRRK-EDYMD(1:6) ))  
      *
                        MOVE    "TNKRRK-KEY3"      TO  ORC-DBPATH
                        PERFORM TNKRRK-READ-SEC
      *
                 END-PERFORM
             END-IF
      *
             MOVE   "TNKRRK-KEY3"       TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
      *
      *      低所得より
             INITIALIZE                      TSYRRK-REC
             MOVE    RECE01-HOSPID       TO  TSYRRK-HOSPID
             MOVE    RECE01-PTID         TO  TSYRRK-PTID
             MOVE    TSYRRK-REC          TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "TSYRRK-KEY3"       TO  ORC-DBPATH
             CALL    "ORCMCPSUB"          USING     MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
             IF      MCP-RC              =   ZERO
                 MOVE    "TSYRRK-KEY3"      TO  ORC-DBPATH
                 PERFORM TSYRRK-READ-SEC
                 PERFORM  UNTIL  
                     ( FLG-TSYRRK          =   1                ) OR
                    (( TSYRRK-NINSTYMD(1:6) <= RECE01-SRYYM   AND
                       RECE01-SRYYM         <= TSYRRK-NINEDYMD(1:6) ))  
      *
                        MOVE    "TSYRRK-KEY3"      TO  ORC-DBPATH
                        PERFORM TSYRRK-READ-SEC
      *
                 END-PERFORM
             END-IF
      *
             MOVE   "TSYRRK-KEY3"       TO  WK-PATH
             PERFORM   CSR-CLOSE-SEC
      *
             EVALUATE    FLG-TSYRRK    ALSO     FLG-TNKRRK
                 WHEN    1             ALSO     1
                       MOVE     SPACE   TO       RECE02-TEISYOTOKU
                 WHEN    0             ALSO     1
                       MOVE     "2"     TO       RECE02-TEISYOTOKU
                 WHEN    0             ALSO     0
                 WHEN    1             ALSO     0
                       MOVE     "1"     TO       RECE02-TEISYOTOKU
             END-EVALUATE
      *
           END-IF
      *
      *    在総診フラグセット
           PERFORM    ZAITAKU-SET-SEC
      *
      *    レセプト種別不明エラー情報
           IF      RECE01-TEISEIKBN      =   "1"
               MOVE    RECE01-ERR-ERRKBN     TO  RECE02-ERR-ERRKBN
               MOVE    RECE01-ERR-DAY        TO  RECE02-ERR-DAY
               MOVE    RECE01-ERR-HKNCOMBINUM
                                             TO  RECE02-ERR-HKNCOMBINUM
               MOVE    RECE01-ERR-TEKSTYMD   TO  RECE02-ERR-TEKSTYMD
               MOVE    RECE01-ERR-TEKEDYMD   TO  RECE02-ERR-TEKEDYMD
           END-IF 
      *
      *    印刷順
           IF      IDZ                 =   1
               MOVE    RECE01-PRTSEQ       TO  RECE02-PRTSEQ
           END-IF
      * 
           PERFORM     220-DATA-WRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報のセット
      *****************************************************************
       KOHI-SET-SEC                SECTION.
      *
           INITIALIZE                           PTKOHINF-REC
           MOVE    RECE01-HOSPID        TO      PTKOH-HOSPID
           MOVE    RECE01-PTID          TO      PTKOH-PTID
           MOVE    RECE01-KOHID-O(IDX)  TO      PTKOH-KOHID
           MOVE    PTKOHINF-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "PTKOHINF-KEY"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "PTKOHINF-KEY"   TO      ORC-DBPATH
               PERFORM  PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   ADD     1               TO  IDX2
      *
                   MOVE   PTKOH-KOHID      TO  RECE02-KOHID(IDX2)
                   MOVE   PTKOH-KOHNUM     TO  RECE02-KOHNUM(IDX2)
                   MOVE   WRK-RECESKYKBN   TO  RECE02-RECESKYKBN(IDX2)
      *            法別番号のセット
                   INITIALIZE                  HKNNUM-REC
                   MOVE   RECE01-HOSPID    TO  HKN-HOSPID
                   MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
                   MOVE   HKNNUM-REC       TO  MCPDATA-REC
                   MOVE   "DBSELECT"       TO  MCP-FUNC
                   MOVE   "HKNNUM-KEY2"    TO  ORC-DBPATH
                   CALL   "ORCMCPSUB"       USING   MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   IF     MCP-RC           =   ZERO
                       MOVE    "HKNNUM-KEY2"   TO  ORC-DBPATH
                       PERFORM  HKNNUM-READ-SEC
                       PERFORM  UNTIL
                           (FLG-HKNNUM         =    1)          OR
                          ((HKN-TEKSTYMD(1:6) <=  RECE01-SRYYM) AND
                           (RECE01-SRYYM      <=  HKN-TEKEDYMD(1:6)))
                          MOVE   "HKNNUM-KEY2"  TO  ORC-DBPATH
                          PERFORM   HKNNUM-READ-SEC
                       END-PERFORM
                       IF       FLG-HKNNUM     =   ZERO
                           MOVE   HKN-HBTNUM  TO RECE02-KOHHBTNUM(IDX2)
                       END-IF
                   END-IF
                   MOVE   PTKOH-FTNJANUM  TO RECE02-KOHFTNJANUM(IDX2)
                   MOVE   SPACE           TO RECE02-KOHNINIKYUKBN(IDX2)
      *             
      *            生保のとき月代わり受給者番号マスタより受給者番号をセット 
                   IF      PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM
                       MOVE    RECE02-HOSPID   TO  MONTHLYNUM-HOSPID
                       MOVE    RECE02-PTID     TO  MONTHLYNUM-PTID
                       MOVE    WRK-CONS-SEIHO-HKNNUM
                                               TO  MONTHLYNUM-KOHNUM
                       MOVE    RECE02-SRYYM    TO  MONTHLYNUM-SRYYM
                       MOVE    RECE02-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF    ( FLG-MONTHLYNUM          =   ZERO  )
      *****************AND   ( MONTHLYNUM-JKYSNUM  NOT =   SPACE )
                           MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                       ELSE                       
                           MOVE   PTKOH-JKYSNUM
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                       END-IF
                   ELSE                       
                       MOVE   PTKOH-JKYSNUM
                                           TO  RECE02-KOHJKYSNUM (IDX2)
                   END-IF    
               ELSE
                   DISPLAY  "PTKOHINF FETCH ERR"  
               END-IF
           ELSE
               DISPLAY   "PTKOHINF SELECT ERR" 
           END-IF
           MOVE   "PTKOHINF-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
           MOVE   "HKNNUM-KEY2"       TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    神奈川地方公費対応
           IF    ( RECE02-PREFNUM  =   "14"           )
           AND   ( RECE02-TEISYUTUSAKI
                                   =   "2"            )
           AND   ( RECE02-KOHNUM      (IDX2)
                                   =   "180" OR "280" )
           AND   ( RECE02-KOHFTNJANUM (IDX2)
                                   =   SPACE          )
               MOVE   "4"          TO  RECE02-RECESKYKBN(IDX2)
           END-IF
      *
           .
       KOHI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       RECESYUBETU-SET-SEC              SECTION.
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      RECE01-TEISEIKBN     =   "1"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    地方公費データのとき主保険が無かったら不明分とする
           IF      IDZ                  =   2
           AND     RECE02-HKNNUM        =   SPACE
      *
      *        沖縄県で精神障害者特別措置(121)の場合は不明分としない
               IF    ( RECE02-PREFNUM       =   "47"  )
               AND   ( RECE02-KOHNUM (1)    =   "121" )
                   CONTINUE
               ELSE
                   MOVE    6                    TO  RECE02-ERR-ERRKBN
                   GO  TO  RECESYUBETU-SET-EXT
               END-IF
           END-IF
      *     
      *    自費のときのレセプト種別を編集
           IF     (RECE02-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   "1"      ) 
               MOVE    8888             TO  RECE02-RECESYUBETU    
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *         
           IF     (RECE02-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   SPACE)
               IF      RECE02-HKNNUM    =   "060"   OR   "067"
                   IF   RECE02-HKNNUM   =   "060"
                        MOVE   "1"      TO  WK-KOKUHO
                   ELSE
                        MOVE   "2"      TO  WK-KOKUHO
                   END-IF
               ELSE
                   MOVE    "1"          TO  WK-IHO
               END-IF
           END-IF
      *
           IF      RECE02-SRYYM     <      "200210" 
             IF      RECE02-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
           MOVE    ZERO            TO    WK-KOHI-CNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF    ( RECE02-KOHNUM     (IDX) NOT =   SPACE )
               AND   ( RECE02-RECESKYKBN (IDX)     =   SPACE )
                   ADD     1               TO    WK-KOHI-CNT
               END-IF
           END-PERFORM    
      *
      *    山形地方公費対応
           IF      RECE02-PREFNUM      =   "06"
               IF  WK-KOKUHO               =     "1"  OR  "2"
                   IF   (RECE02-KOHNUM(1) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(2) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(3) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(4) (1:1)  =   "1")   OR
                        (RECE02-KOHNUM(1) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(2) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(3) (1:1)  =   "2")   OR
                        (RECE02-KOHNUM(4) (1:1)  =   "2")
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
      *    福岡地方公費対応
           IF      RECE02-PREFNUM      =   "40"
               IF  WK-KOKUHO               =   "1" OR  "2"
                   IF   (RECE02-KOHNUM   (1)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(1)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (2)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(2)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (3)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(3)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE02-KOHNUM   (4)(1:1)  =   "1" OR  "2")
                   AND  (RECE02-KOHHBTNUM(4)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *----(01.00.13) LINE ADD START --------------------------
      *   平成１４年１０月対応
      *    ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
           IF  RECE01-HKNID             NOT =   ZERO
               IF      RECE02-SRYYM             >=      "200210"
      **             MOVE   PTINF-BIRTHDAY   TO  WRK-YMDS1
      **             MOVE   RECE02-SRYYM     TO  WRK-YYMMS2R
      **             MOVE   01               TO  WRK-DDS2R
      **             MOVE   ZERO             TO  WRK-NENREI
      **             COMPUTE  WRK-NENREI     =   WRK-YYS2
      **                                     -   WRK-YYS1
      **             IF   WRK-YMDS1(5:4)     >   WRK-YMDS2(5:4)
      **                  ADD   -1           TO  WRK-NENREI
      **             END-IF
      **             IF   WRK-NENREI         =   3
      **                 IF    WRK-YMDS1(5:2)  = WRK-YMDS2(5:2)
      **                       IF   WRK-YMDS1(7:2)  NOT = "01"
      **                            ADD   -1     TO  WRK-NENREI
      **                       END-IF
      **                 END-IF
      **             END-IF
      *
                   IF      WRK-NENREI         <     3
                               MOVE   "3"     TO   WK-HONKZKKBN
                   END-IF
               END-IF
           END-IF
      *
      *    １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *    "９"または"８"をセット
      *    老人保険の場合はＷＫ−ＲＪＮにセットする
           IF  RECE01-HKNID             NOT =   ZERO
               IF      RECE02-SRYYM     >=      "200210"
      *            保険組合せテーブルの検索
                   INITIALIZE                        HKNCOMBI-REC
                   MOVE    RECE01-HOSPID        TO   COMB-HOSPID
                   MOVE    RECE01-PTID          TO   COMB-PTID
                   MOVE    RECE01-HKNCOMBI-O(1) TO   COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC         TO   MCPDATA-REC
                   MOVE    "DBSELECT"           TO   MCP-FUNC
                   MOVE    "HKNCOMBI-KEY"       TO   ORC-DBPATH
                   CALL    "ORCMCPSUB"           USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
                   IF      MCP-RC               =    ZERO
                       MOVE    "HKNCOMBI-KEY"   TO   ORC-DBPATH
                       PERFORM  HKNCOMBI-READ-SEC
                       IF       FLG-HKNCOMBI    =    ZERO
                           EVALUATE    COMB-HKNNUM
                             WHEN    "002" 
                                 EVALUATE   COMB-HOJOKBN
                                     WHEN   "A"
                                     WHEN   "B"
                                     WHEN   "C"
                                     WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                     WHEN   "D"
                                     WHEN   "E"
                                     WHEN   "F"
                                     WHEN   "8"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                 END-EVALUATE
                             WHEN    "031"  THRU    "034"  
                                 EVALUATE   COMB-HOJOKBN
                                     WHEN   "B"
                                     WHEN   "9"   
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                     WHEN   "E"
                                     WHEN   "8"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                 END-EVALUATE
                             WHEN    "060" 
                               IF  (WRK-NENREI  >=   70)  AND
                                   (WRK-NENREI  <=   74)
                                 EVALUATE   COMB-HOJOKBN
                                     WHEN   "1"
                                     WHEN   "4"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                     WHEN   "2"
                                     WHEN   "5"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                                 END-EVALUATE
                               END-IF     
                             WHEN    OTHER 
                               EVALUATE   COMB-HOJOKBN
                                   WHEN   "9"
                                       MOVE   "9"   TO   WK-HONKZKKBN
                                   WHEN   "8"
                                       MOVE   "8"   TO   WK-HONKZKKBN
                               END-EVALUATE
                           END-EVALUATE
                           IF  COMB-KOH1HKNNUM  =    "027"
                               EVALUATE   COMB-KOH1PAYKBN
                                   WHEN   "01"
                                       MOVE   "9"   TO   WK-RJN
                                   WHEN   "02"
                                       MOVE   "8"   TO   WK-RJN
                               END-EVALUATE
                           END-IF
                       ELSE
                           DISPLAY  "HKNCOMBI FETCH ERR" 
                       END-IF
                   ELSE
                       DISPLAY   "HKNCOMBI SELECT ERR" 
                   END-IF
                   MOVE   "HKNCOMBI-KEY"        TO   WK-PATH
                   PERFORM   CSR-CLOSE-SEC
               END-IF
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( RECE02-HONKZKKBN    =   "1"   )     
               IF      RECE02-HKNNUM   =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *          
      *----(01.00.13) LINE ADD END   --------------------------
      *
      *      DISPLAY "WK-HONKZKKBN: " WK-HONKZKKBN 
      *      DISPLAY "RECE01-NYUGAIKBN: " RECE01-NYUGAIKBN 
      *      DISPLAY "WK-IHO: " WK-IHO 
      *      DISPLAY "WK-KOKUHO: " WK-KOKUHO 
      *      DISPLAY "WK-RJN: " WK-RJN 
      *      DISPLAY "WK-KOHI-CNT: " WK-KOHI-CNT 
      *      ACCEPT     WRK-IN FROM CONSOLE
      *
      *----(01.00.13) LINE MOD START --------------------------
      *    この評価文は平成１４年９月まで有効
           IF      RECE02-SRYYM     <=      "200209"
             EVALUATE  WK-HONKZKKBN ALSO RECE01-NYUGAIKBN  ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   1
                   MOVE    1122            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1125            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1126            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1131            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1132            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO   2
                   MOVE    1132            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1135            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1136            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1311            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   ZERO
                   MOVE    1312            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1321            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   1
                   MOVE    1322            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1331            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       ANY   ALSO   ANY    ALSO  "1"    ALSO   2
                   MOVE    1332            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1411            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1412            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1415            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   ZERO
                   MOVE    1416            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1421            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1422            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1425            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   1
                   MOVE    1426            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1431            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1432            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1435            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO   2
                   MOVE    1436            TO    RECE02-RECESYUBETU
             END-EVALUATE
           END-IF
      *----(01.00.13) LINE MOD END   --------------------------
      *
      *----(01.00.13) LINE ADD START --------------------------
      *    平成１４年１０月改正対応
           IF      RECE02-SRYYM     >=      "200210"
             EVALUATE  WK-HONKZKKBN ALSO RECE01-NYUGAIKBN  ALSO
                    WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
      *保険単独
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    RECE02-RECESYUBETU
      *保険と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    RECE02-RECESYUBETU
      *保険と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    RECE02-RECESYUBETU
      *公費単独
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    RECE02-RECESYUBETU
      *２種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    RECE02-RECESYUBETU
      *３種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    RECE02-RECESYUBETU
      *老人単独
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    RECE02-RECESYUBETU
      *老人と公費１種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    RECE02-RECESYUBETU
      *老人と公費２種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    RECE02-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    RECE02-RECESYUBETU
      *退職者単独
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1409            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410            TO    RECE02-RECESYUBETU
      *退職者と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420            TO    RECE02-RECESYUBETU
      *退職者と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431            TO    RECE02-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433            TO    RECE02-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435            TO    RECE02-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437            TO    RECE02-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439            TO    RECE02-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430            TO    RECE02-RECESYUBETU
             END-EVALUATE
           END-IF
      *----(01.00.13) LINE ADD END   --------------------------
      *     DISPLAY   "RECE02-RECESYUBETU: " RECE02-RECESYUBETU
      *     ACCEPT     WRK-IN FROM CONSOLE
           .
       RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険−給付割合のセット
      *****************************************************************
       KYURATE-SET-SEC                SECTION.
      *
      *    保険組合せテーブルの検索
           INITIALIZE                           HKNCOMBI-REC
           MOVE    RECE01-HOSPID        TO      COMB-HOSPID
           MOVE    RECE01-PTID          TO      COMB-PTID
           MOVE    RECE01-HKNCOMBI-O(1) TO      COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
               PERFORM  HKNCOMBI-READ-SEC
               IF       FLG-HKNCOMBI       =   ZERO
                   CONTINUE
               ELSE
                   DISPLAY  "HKNCOMBI FETCH ERR" 
      *             ACCEPT    WRK-IN FROM CONSOLE 
               END-IF
           ELSE
               DISPLAY   "HKNCOMBI SELECT ERR" 
      *         ACCEPT     WRK-IN FROM CONSOLE
           END-IF
           MOVE   "HKNCOMBI-KEY"     TO    WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    保険番号テーブルの検索
           INITIALIZE                      HKNNUM-REC
           MOVE   RECE01-HOSPID      TO    HKN-HOSPID
           MOVE   RECE01-HKNNUM      TO    HKN-HKNNUM
           MOVE   HKNNUM-REC         TO    MCPDATA-REC
           MOVE   "DBSELECT"         TO    MCP-FUNC
           MOVE   "HKNNUM-KEY2"      TO    ORC-DBPATH
           CALL   "ORCMCPSUB"         USING MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
           IF      MCP-RC            =     ZERO
               MOVE    "HKNNUM-KEY2" TO    ORC-DBPATH
               PERFORM  HKNNUM-READ-SEC
               PERFORM  UNTIL
                 (FLG-HKNNUM     =    1)                       OR
                ((HKN-TEKSTYMD(1:6)   <=  RECE01-SRYYM)        AND
                 (RECE01-SRYYM        <=  HKN-TEKEDYMD(1:6)))
                   PERFORM   HKNNUM-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "HKNNUM-KEY2"        TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *----(01.00.14) LINE MOD START --------------------------
      *平成１４年１０月改正対応
      *平成１４年９月まで有効
      *    保険組合せテーブルの補助区分から患者の負担割合を再セット
      *   （国保のみ）
           IF      RECE02-SRYYM     <=      "200209"
             IF    RECE01-HKNNUM           =    "060"
               EVALUATE   COMB-HOJOKBN
                   WHEN   "0"
                          MOVE   ZERO      TO   RECE02-KYURATE
                   WHEN   "1"
                          MOVE   10        TO   RECE02-KYURATE
                   WHEN   "2"
                          MOVE   20        TO   RECE02-KYURATE
                   WHEN   "3"
                          MOVE   30        TO   RECE02-KYURATE
                   WHEN   "4"
                          MOVE   10        TO   RECE02-KYURATE
                   WHEN   "5"
                          MOVE   20        TO   RECE02-KYURATE
                   WHEN   "6"
                          MOVE   30        TO   RECE02-KYURATE
               END-EVALUATE
             ELSE
               IF    COMB-HONKZKKBN        =    "1"
                     MOVE   HKN-HON-GAIKFTNRATE  TO  RECE02-KYURATE
               ELSE
                     MOVE   HKN-KZK-GAIKFTNRATE  TO  RECE02-KYURATE
               END-IF
             END-IF
           END-IF
      *----(01.00.14) LINE MOD START --------------------------
      *
      *----(01.00.14) LINE ADD START --------------------------
      *平成１４年１０月改正対応
      *平成１４年１０月以降有効
      *    保険組合せテーブルの補助区分から患者の負担割合を再セット
           IF      RECE02-SRYYM     >=      "200210"
              EVALUATE    RECE01-HKNNUM       
                  WHEN    "060"
                      IF       WRK-NENREI           <     3
                          IF    COMB-HOJOKBN    =    "0"  OR   "1" OR
                                                     "4"
                              EVALUATE   COMB-HOJOKBN
                                  WHEN   "0"
                                      MOVE   ZERO    TO   RECE02-KYURATE
                                  WHEN   "1"
                                      MOVE   10      TO   RECE02-KYURATE
                                  WHEN   "4"
                                      MOVE   10      TO   RECE02-KYURATE
                              END-EVALUATE
                          ELSE
                              MOVE  20      TO    RECE02-KYURATE
                          END-IF
                      ELSE
                          EVALUATE   COMB-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO   RECE02-KYURATE
                              WHEN   "1"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "2"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "3"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "4"
                                  MOVE   10        TO   RECE02-KYURATE
                              WHEN   "5"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "6"
                                  MOVE   30        TO   RECE02-KYURATE
                              WHEN   "8"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE02-KYURATE
                          END-EVALUATE
                      END-IF
                  WHEN    "067"
                      IF       WRK-NENREI           <     3
                          MOVE  20      TO    RECE02-KYURATE
                      ELSE
                        IF      COMB-HOJOKBN          NOT =  SPACE
                          EVALUATE   COMB-HOJOKBN
                              WHEN   "8"
                                  MOVE   20        TO   RECE02-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE02-KYURATE
                          END-EVALUATE
                        ELSE
                          IF      COMB-HONKZKKBN        =    "1"
                              MOVE   HKN-HON-GAIKFTNRATE
                                                   TO   RECE02-KYURATE
                          ELSE
                              MOVE   HKN-KZK-GAIKFTNRATE
                                                   TO   RECE02-KYURATE
                          END-IF
                        END-IF
                      END-IF
                  WHEN    OTHER
                      IF   WRK-NENREI           <     3
                          MOVE  20      TO    RECE02-KYURATE
                      ELSE
                          IF  COMB-HOJOKBN        =     SPACE
                              IF  COMB-HONKZKKBN      =     "1"
                                  MOVE HKN-HON-GAIKFTNRATE
                                                  TO  RECE02-KYURATE
                              ELSE
                                  MOVE HKN-KZK-GAIKFTNRATE
                                                  TO  RECE02-KYURATE
                              END-IF
                          ELSE
                              EVALUATE   COMB-HOJOKBN
                                  WHEN   "8"
                                      MOVE   20   TO  RECE02-KYURATE
                                  WHEN   "9"
                                      MOVE   10   TO  RECE02-KYURATE
                              END-EVALUATE
                         END-IF
                     END-IF
              END-EVALUATE
      *
              IF  RJN-RATE-KBN      NOT =    SPACE
                  EVALUATE   RJN-RATE-KBN
                      WHEN   "1"
                             MOVE   10        TO  RECE02-KYURATE
                      WHEN   "2"
                             MOVE   20        TO  RECE02-KYURATE
                  END-EVALUATE
              END-IF
           END-IF
      *----(01.00.14) LINE ADD START --------------------------
      *
      *    船員保険（被保険者）＋通勤災害フラグセット
           IF    RECE01-HKNNUM           =    "002"
               EVALUATE   COMB-HOJOKBN
                    WHEN   "3"
                    WHEN   "C"
                    WHEN   "F"
                        MOVE     "1"     TO     RECE02-SENIN-TUUSAI
               END-EVALUATE
           END-IF
      *
           .
       KYURATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ公費保険番号情報のセット
      *****************************************************************
       KOHNUMHC-SET-SEC                SECTION.
      *
           PERFORM   VARYING   IDX   FROM   1   BY   1
             UNTIL  (IDX        >       10)
             OR     (RECE01-HKNCOMBI-O(IDX)   =   ZERO)   
      *        保険組合せテーブルの検索
               INITIALIZE                           HKNCOMBI-REC
               MOVE    RECE01-HOSPID        TO      COMB-HOSPID
               MOVE    RECE01-PTID          TO      COMB-PTID
               MOVE    RECE01-HKNCOMBI-O(IDX)
                                            TO      COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC         TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "HKNCOMBI-KEY"       TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "HKNCOMBI-KEY"   TO      ORC-DBPATH
                   PERFORM  HKNCOMBI-READ-SEC
                   IF       FLG-HKNCOMBI     =   ZERO
                       MOVE    COMB-HKNNUM   TO  RECE02-HKNNUMHC (IDX)
                       IF   COMB-KOH1HKNNUM  =   "027"
                         MOVE COMB-KOH2HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH2HKNNUM
                                             TO RECE02-KOHNUMHC(IDX 1)
                             MOVE COMB-KOH2ID
                                             TO RECE02-KOHIDHC (IDX 1)
                         END-IF
                         MOVE COMB-KOH3HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH3HKNNUM
                                             TO RECE02-KOHNUMHC(IDX 2)
                             MOVE COMB-KOH3ID
                                             TO RECE02-KOHIDHC (IDX 2)
                         END-IF
                         MOVE COMB-KOH4HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH4HKNNUM
                                             TO RECE02-KOHNUMHC(IDX 3)
                             MOVE COMB-KOH4ID
                                             TO RECE02-KOHIDHC (IDX 3)
                         END-IF
                       ELSE
                         MOVE COMB-KOH1HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH1HKNNUM
                                              TO RECE02-KOHNUMHC(IDX 1)
                             MOVE COMB-KOH1ID
                                              TO RECE02-KOHIDHC (IDX 1)
                         END-IF
                         MOVE COMB-KOH2HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH2HKNNUM
                                              TO RECE02-KOHNUMHC(IDX 2)
                             MOVE COMB-KOH2ID
                                              TO RECE02-KOHIDHC (IDX 2)
                         END-IF
                         MOVE COMB-KOH3HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH3HKNNUM
                                              TO RECE02-KOHNUMHC(IDX 3)
                             MOVE COMB-KOH3ID
                                              TO RECE02-KOHIDHC (IDX 3)
                         END-IF
                         MOVE COMB-KOH4HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH4HKNNUM
                                              TO RECE02-KOHNUMHC(IDX 4)
                             MOVE COMB-KOH4ID
                                              TO RECE02-KOHIDHC (IDX 4)
                         END-IF 
                       END-IF
                   ELSE
                       DISPLAY  "HKNCOMBI FETCH ERR" 
                   END-IF
               ELSE
                   DISPLAY   "HKNCOMBI SELECT ERR" 
               END-IF
               MOVE   "HKNCOMBI-KEY"     TO    WK-PATH
               PERFORM   CSR-CLOSE-SEC
           END-PERFORM
      *
           .
       KOHNUMHC-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       KOHNUM-CHECK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           IF      WRK-HKNNUM          NOT =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                       OR      RECE01-KOHHKNNUM-O (IDY)  =   SPACE
                       OR      FLG-CHK     =  1
                   IF      WRK-HKNNUM      =   RECE01-KOHHKNNUM-O (IDY)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
           END-IF             
      *
           .
       KOHNUM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診フラグセット
      *****************************************************************
       ZAITAKU-SET-SEC                SECTION.
      *
           IF   RECE01-SRYYM    >=  "200210"
      *
             MOVE      SPACE     TO    WRK-ZAITAKU
      *
             PERFORM   VARYING   IDX3   FROM    1   BY  1
                         UNTIL   ( IDX3   >   TBL-ZAITAKU-MAX )
                          OR     ( WRK-ZAITAKU    =    "1"    )
      *
               INITIALIZE                           SRYACT-REC
               MOVE    RECE01-HOSPID        TO      SRY-HOSPID
               MOVE    RECE01-NYUGAIKBN     TO      SRY-NYUGAIKBN
               MOVE    RECE01-PTID          TO      SRY-PTID
               MOVE    RECE01-SRYYM         TO      SRY-SRYYM
               MOVE    TBL-ZAITAKU(IDX3)    TO      SRY-SRYCD(1)
               MOVE    TBL-ZAITAKU(IDX3)    TO      SRY-SRYCD(2)
               MOVE    TBL-ZAITAKU(IDX3)    TO      SRY-SRYCD(3)
               MOVE    TBL-ZAITAKU(IDX3)    TO      SRY-SRYCD(4)
               MOVE    TBL-ZAITAKU(IDX3)    TO      SRY-SRYCD(5)
               MOVE    SRYACT-REC           TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "SRYACT-KEY7"        TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "SRYACT-KEY7"    TO      ORC-DBPATH 
                   PERFORM   SRYACT-READ-SEC
                   IF      FLG-SRYACT       =       ZERO
                       INITIALIZE                   SRYACCT-REC
                       MOVE    SRY-HOSPID       TO   ACCT-HOSPID
                       MOVE    SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                       MOVE    SRY-SRYKA        TO   ACCT-SRYKA
                       MOVE    SRY-PTID         TO   ACCT-PTID
                       MOVE    SRY-SRYYM        TO   ACCT-SRYYM
                       MOVE    SRY-ZAINUM       TO   ACCT-ZAINUM
                       MOVE    SRYACCT-REC      TO   MCPDATA-REC
                       MOVE    "DBSELECT"       TO   MCP-FUNC
                       MOVE    "SRYACCT-KEY31"  TO   ORC-DBPATH
                       CALL    "ORCMCPSUB"           USING
                                                     MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
                       IF      MCP-RC           =       ZERO
                           MOVE    "SRYACCT-KEY31"  TO  ORC-DBPATH 
                           PERFORM   SRYACCT-READ-SEC
                           IF      FLG-SRYACCT  =       ZERO
                               IF  (ACCT-ZAITEN   NOT =  ZERO)  AND
                                   (ACCT-ZAIKAISU NOT =  ZERO)
                                   MOVE    "1" TO   WRK-ZAITAKU
                               END-IF
                           END-IF
                       END-IF
      *
                       MOVE    "SRYACCT-KEY31" TO    WK-PATH
                       PERFORM   CSR-CLOSE-SEC
      *
                   END-IF
               END-IF
      *
               MOVE    "SRYACT-KEY7"    TO    WK-PATH
               PERFORM   CSR-CLOSE-SEC
      *
             END-PERFORM
      *
             IF    WRK-ZAITAKU   =   "1"
               IF  ( RECE02-AGE          >=  70      )   OR
                   ( RECE02-AGE          >=  65  AND                    
                     RECE01-KOHID    NOT  =   ZERO   )   OR
                   ( RECE02-AGE          >=  65  AND                    
                     FLG-ROUJINHOKEN      =   1      )
                 MOVE    "1"           TO  RECE02-ZAITAKU
      *
                 IF  RECE02-RECESYUBETU   =   "8888"
                     MOVE  SPACE       TO  RECE02-ZAITAKU
                 END-IF
                 IF  RECE02-HKNNUM        =   SPACE 
                     MOVE  SPACE       TO  RECE02-ZAITAKU
                 END-IF
      *
               END-IF
             END-IF
      *
           END-IF
           .
       ZAITAKU-SET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       220-DATA-WRITE-SEC                SECTION.
      *
           WRITE   RECE02-REC
      *
           ADD     1                   TO  CNT-OUT (IDZ)
      *
           .
       220-DATA-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    保険履歴情報編集処理
      *****************************************************************
       230-RECE09-HENSYU-SEC                   SECTION.
      *
           IF      RECE01-HKNID             NOT =   ZERO
      ***  AND     RECE01-HKNNUM            NOT =   WRK-CONS-JIHI-HKNNUM
           AND     RECE01-HKNNUM(1:2)       NOT =   "98"
           AND     RECE01-TEISEIKBN             =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      RECE01-TEKSTYMDX (IDX)  =   SPACE
                 MOVE    SPACE                TO  RECE09-REC
                 INITIALIZE                       RECE09-REC
                 MOVE    RECE01-HOSPID        TO  RECE09-HOSPID
                 MOVE    RECE01-SRYYM         TO  RECE09-SRYYM
                 MOVE    RECE01-NYUGAIKBN     TO  RECE09-NYUGAIKBN
                 MOVE    RECE01-PTID          TO  RECE09-PTID
                 MOVE    RECE01-RECEKA        TO  RECE09-RECEKA
                 MOVE    RECE01-TEKSTYMDX (IDX)
                                              TO  RECE09-TEKSTYMD
                 PERFORM 900-RECE09-READ-SEC
                 IF      FLG-RECE09           =   1
                   MOVE    SPACE                TO  RECE09-REC
                   INITIALIZE                       RECE09-REC
                   MOVE    RECE01-HOSPID        TO  RECE09-HOSPID
                   MOVE    RECE01-SRYYM         TO  RECE09-SRYYM
                   MOVE    RECE01-NYUGAIKBN     TO  RECE09-NYUGAIKBN
                   MOVE    RECE01-PTID          TO  RECE09-PTID
                   MOVE    RECE01-RECEKA        TO  RECE09-RECEKA
                   MOVE    RECE01-TEKSTYMDX (IDX)
                                              TO  RECE09-TEKSTYMD
                   MOVE    RECE01-TEKSTYMDX (IDX)
                                                TO  RECE09-TEKSTYMDX
                   MOVE    RECE01-TEKEDYMDX (IDX)
                                                TO  RECE09-TEKEDYMDX
      * 
                   IF      RECE01-HKNNUM       =   "060"   OR  "067"
                       MOVE    2               TO  RECE09-TEISYUTUSAKI
                   ELSE
                       MOVE    1               TO  RECE09-TEISYUTUSAKI
                   END-IF
      *                                          
                   INITIALIZE                       PTHKNINF-REC
                   MOVE    RECE01-HOSPID        TO  PTHKN-HOSPID
                   MOVE    RECE01-PTID          TO  PTHKN-PTID
                   MOVE    RECE01-HKNNUM        TO  PTHKN-HKNNUM
                   MOVE    RECE01-TEKSTYMDX(IDX)
                                                TO  PTHKN-TEKSTYMD
                   MOVE    RECE01-TEKEDYMDX(IDX)
                                                TO  PTHKN-TEKEDYMD
                   MOVE    PTHKNINF-REC         TO  MCPDATA-REC
                   MOVE    "DBSELECT"           TO  MCP-FUNC
                   MOVE    "PTHKNINF-KEY4"      TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING   MCPAREA
                                                        ORCMCPAREA
                                                        MCPDATA-REC
                   IF      MCP-RC               =   ZERO
                       MOVE    "PTHKNINF-KEY4"  TO  ORC-DBPATH
                       PERFORM PTHKNINF-READ-SEC
                   ELSE
                       MOVE    1                TO  FLG-PTHKNINF  
                   END-IF
                   PERFORM              UNTIL   FLG-PTHKNINF     =   1
                       IF      PTHKN-SAKJOKBN   =   SPACE
                           MOVE   PTHKN-HKNID      TO  RECE09-HKNID
                           MOVE   PTHKN-HKNNUM     TO  RECE09-HKNNUM
                           MOVE   PTHKN-HKNJANUM   TO  RECE09-HKNJANUM
                           MOVE   PTHKN-HONKZKKBN  TO  RECE09-HONKZKKBN
                           MOVE   PTHKN-HOJOKBN    TO  RECE09-HOJOKBN
                           MOVE   PTHKN-CONTKBN    TO  RECE09-CONTKBN
                           MOVE   PTHKN-KIGO       TO  RECE09-KIGO
                           MOVE   PTHKN-NUM        TO  RECE09-NUM
                           MOVE   SPACE            TO  RECE09-CERTNUM
      *         
                           INITIALIZE                  HKNNUM-REC
                           MOVE   RECE01-HOSPID    TO  HKN-HOSPID
                           MOVE   PTHKN-HKNNUM     TO  HKN-HKNNUM
                           MOVE   RECE01-TEKSTYMDX (IDX)
                                                   TO  HKN-TEKSTYMD
                           MOVE   RECE01-TEKEDYMDX (IDX)
                                                   TO  HKN-TEKEDYMD
                           MOVE   HKNNUM-REC       TO  MCPDATA-REC
                           MOVE   "DBSELECT"       TO  MCP-FUNC
                           MOVE   "HKNNUM-KEY5"    TO  ORC-DBPATH
                           CALL   "ORCMCPSUB"      USING
                                                   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
                           IF     MCP-RC           =   ZERO
                               MOVE    "HKNNUM-KEY5"   TO  ORC-DBPATH
                               PERFORM HKNNUM-READ-SEC
                               IF      FLG-HKNNUM      =   ZERO
                                   MOVE   hKN-TANSEIDONAME
                                               TO  RECE09-TANSEIDONAME
                                   MOVE   HKN-HBTNUM
                                               TO RECE09-HKNHBTNUM
                               END-IF
                           END-IF
                           MOVE   "HKNNUM-KEY5"       TO  WK-PATH
                           PERFORM   CSR-CLOSE-SEC
                           MOVE    1                TO  FLG-PTHKNINF
                       ELSE
                           MOVE    "PTHKNINF-KEY4"  TO  ORC-DBPATH
                           PERFORM PTHKNINF-READ-SEC
                       END-IF
                   END-PERFORM
                   MOVE   "PTHKNINF-KEY4"     TO  WK-PATH
                   PERFORM   CSR-CLOSE-SEC
      *
                   WRITE   RECE09-REC
                   ADD     1                  TO  CNT-RECE09
                 END-IF  
               END-PERFORM
           END-IF           
      *
           .
       230-RECE09-HENSYU-EXT.
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
           IF      CNT-OUT (1)          =   ZERO
           AND     CNT-OUT (2)          =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           DISPLAY "RECE01  CNT-IN  :" CNT-IN  (1) 
           DISPLAY "RECE02  CNT-OUT :" CNT-OUT (1) 
           DISPLAY "RECE09  CNT-OUT :" CNT-RECE09  
           DISPLAY "RECE01X CNT-IN  :" CNT-IN  (2) 
           DISPLAY "RECE02X CNT-OUT :" CNT-OUT (2) 
           DISPLAY "*** ORCR0020 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       PTHKNINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険番号読込
      *****************************************************************
       HKNNUM-READ-SEC           SECTION.
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
       HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       HKNCOMBI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       TSYRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       TNKRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       SRYACCT-READ-SEC         SECTION.
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
       SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       SRYACT-READ-SEC         SECTION.
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
       SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH            TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       800-KYUSEIRRK-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "KYUSEIRRK-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "KYUSEIRRK-KEY3"     TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
                   MOVE    ZERO            TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1               TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "KYUSEIRRK-KEY3"    TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       800-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    "MONTHLYNUM-KEY"       TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "MONTHLYNUM-KEY"    TO      ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       CSR-CLOSE-SEC            SECTION.
      *
           INITIALIZE                  MCPAREA
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           .
       CSR-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE01-READ-SEC            SECTION.
      *
           READ    RECE01-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE01
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE01 " IDZ " PTNUM=" RECE01-PTNUM     
           END-READ
           .
       RECE01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険履歴情報読込
      *****************************************************************
       900-RECE09-READ-SEC             SECTION.
      *
           READ    RECE09-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE09
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE09
           END-READ
           .
       900-RECE09-READ-EXT.
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
