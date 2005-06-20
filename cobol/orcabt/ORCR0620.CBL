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
       PROGRAM-ID.             ORCR0620.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプト基本情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/10    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/03/07  レセプト記載外公費情報追加
      *  01.00.03    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.04    NACL-藤原    03/03/15  入院履歴から診療科の編集を行う
      *  01.00.05    NACL-藤原    03/04/14  不明分の保険履歴情報は作成しない
      *  01.00.06    NACL-門脇    03/04/16  船員保険（被保険者）＋通勤災害
      *                                     フラグセット
      *  01.00.07    NACL-藤原    03/04/21  地方公費データのとき主保険が
      *                                     無い場合は不明分とする
      *  01.00.08    NACL-藤原    03/05/09  入退院履歴情報に入院中区分の追加
      *  01.00.09    NACL-門脇    03/05/28  病棟番号・種別編集
      *  01.00.10    NACL-藤原    03/06/16  レセプト種別の修正
      *  01.00.11    NACL-藤原    03/07/25  入院歴に労災・自賠責を対象
      *                                     としないようにする
      *                                     入院会計の医保の組合せがないとき
      *                                     入院歴を全て表示する
      *  01.00.12    NACL-門脇    03/11/27  ”老保”編集条件修正
      *  01.00.13    NACL-門脇    04/02/12  年齢判定文修正
      *  01.00.14    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.15    NACL-門脇    04/04/06  公費単独７０歳未満で老人保険入力
      *                                     のある方の高齢者フラグ編集修正
      *  01.00.16    NACL-門脇    04/06/02  月途中に老保の変更があった場合
      *                                     のＫＥＹの編集の修正
      *  01.00.17    NACL-藤原    04/06/08  同一開始日の複数自費レセプトが
      *                                     別々に作成されるように修正
      *                                     診療科情報が未編集のとき
      *                                     最終入院科を編集する
      *  01.00.18    NACL-藤原    04/06/08  適用終了日の追加
      *  01.00.19    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *  01.00.20    NACL-藤原    04/07/12  生保の受給者番号を月代わり受給者DBに
      *                                     無い場合は公費DBより編集する
      *                                     保険組合せの公費ＩＤを追加
      *  01.00.21    NACL-藤原    04/07/20  特例退職者＋老人のとき不明分
      *                                     とする
      *  01.00.22    NACL-藤原    04/07/23  補助区分の追加に伴うレセプト
      *                                     種別・給付割合の編集を変更
      *  01.00.23    NACL-藤原    04/10/04  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.01.02    NACL-藤原    04/10/30  福岡県の地方公費は主保険が
      *                                     国保の場合は公費カウントしない
      *                                     （法別番号４１以外のとき）
      *                                     病棟区分の編集方法を変更
      *  01.01.03    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.01.04    NACL-藤原    05/01/08  同月に継続なしで２回入院した
      *                                     場合の入院年月日の記載方法
      *  01.01.05    NACL-門脇    05/02/14  高齢者一部負担額記載修正
      *  01.01.06    NACL-門脇    05/02/25  特記事項”老保”編集修正
      *  01.01.07    NACL-門脇    05/03/16  高齢者フラグ編集修正
      *  01.01.08    NACL-藤原    05/05/17  神奈川県の障害(180､280)は
      *                                     国保で負担者番号が空白の場合
      *                                     公費カウントしない
      *  01.01.09    NACL-小豆沢  05/06/20  転室時の入院レセ病棟区分の表示
      *                                     修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE61-FILE     ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE61-KEY
                                   FILE    STATUS  IS  STS-RECE61.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
           SELECT  RECE62X-FILE    ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE62X-KEY
                                   FILE    STATUS  IS  STS-RECE62.
           SELECT  RECE68-FILE     ASSIGN  RECE68PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE68-KEY
                                   FILE    STATUS  IS  STS-RECE68.
           SELECT  RECE68X-FILE    ASSIGN  RECE68PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE68X-KEY
                                   FILE    STATUS  IS  STS-RECE68.
           SELECT  RECE69-FILE     ASSIGN  RECE69PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE69-KEY
                                   FILE    STATUS  IS  STS-RECE69.
           SELECT  RECE69X-FILE    ASSIGN  RECE69PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE69X-KEY
                                   FILE    STATUS  IS  STS-RECE69.
      *    レセプト（県単用項目）データ
           SELECT  RECE67-FILE     ASSIGN  RECE67PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE67-KEY
                                   FILE    STATUS  IS  STS-RECE67.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE61-FILE.
       01  RECE61-REC. 
           COPY    "CPRCF061.INC".
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
      *
       FD  RECE62X-FILE.
       01  RECE62X-REC. 
           COPY    "CPRCF062.INC"  REPLACING  //RECE62//
                                   BY         //RECE62X//.
      *
       FD  RECE68-FILE.
       01  RECE68-REC. 
           COPY    "CPRCF068.INC".
      *
       FD  RECE68X-FILE.
       01  RECE68X-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE68//
                                   BY         //RECE68X//.
      *
       FD  RECE69-FILE.
       01  RECE69-REC. 
           COPY    "CPRCF069.INC".
      *
       FD  RECE69X-FILE.
       01  RECE69X-REC. 
           COPY    "CPRCF069.INC"  REPLACING  //RECE69//
                                   BY         //RECE69X//.
      *
      *    レセプト（県単用項目）データ
       FD  RECE67-FILE.
       01  RECE67-REC. 
           COPY    "CPRCF067.INC".
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
                                   BY         //RECE61//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE62//.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE69//.
      *
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE68//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE67//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE61                              PIC X(02).
           03  STS-RECE62                              PIC X(02).
           03  STS-RECE68                              PIC X(02).
           03  STS-RECE69                              PIC X(02).
           03  STS-RECE67                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-RECE61                              PIC 9(01). 
           03  FLG-RECE68                              PIC 9(01). 
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-CHK                                 PIC 9(01).
           03  FLG-MONTHLYNUM                          PIC 9(01).
           03  FLG-TSYRRK                              PIC 9(01).
           03  FLG-TNKRRK                              PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-PTNYUINRRK12                        PIC 9(01).
           03  FLG-PTNYUINRRK22                        PIC 9(01).
           03  FLG-PTNYUINRRK23                        PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-ROUJINHOKEN                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE68                              PIC 9(06). 
           03  CNT-RECE69                              PIC 9(06). 
           03  CNT-TBL-OCC                             OCCURS  2. 
               05  CNT-IN                              PIC 9(06).
               05  CNT-OUT                             PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDW                                     PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  IDX3                                    PIC 9(02).
           03  IDX4                                    PIC 9(02).
           03  IDX5                                    PIC 9(02).
           03  IDXX                                    PIC 9(02).
           03  IDZ                                     PIC 9(02).
           03  TOKKI-IDX                               PIC 9(02).
           03  SYSKAN-IDX                              PIC 9(03).
           03  SYSKAN-1017-IDX                         PIC 9(03).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  10.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    患者登録機能制御情報退避用領域
       01  TBL-SYS1017-T.
           03  TBL-SYS1017-OCC                         OCCURS  10.
               05  TBL-SYS1017                         PIC X(610).
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
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
      *
           03  WRK-SYS-SRYYMD                          PIC  X(08).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM                         PIC  9(06).
               05  WRK-SRYDD                           PIC  9(02).
           03  WRK-PTNYUINRRK-TENSTUYMD                PIC  X(08).
      *
      *    患者年齢算出用エリア
       01  WRK-NENREI-AREA.
           03  WRK-NENREI                PIC 9(03).
      *
           03  WRK-RECEERR               PIC X(200).
      *
           03  WRK-HKNNUM                PIC X(03).
      *
           03  WRK-JIHI-FLG              PIC X(01).
      *
           03  WRK-CHOKI                 PIC X(03).
      *
           03  RJN-RATE-KBN              PIC X(01).
      *
           03  WRK-RECESKYKBN            PIC X(01).
      *
      *    病棟区分テーブル                                             
       01  WRK-BYOTOKBN-TABLE.                                          
           03  WRK-BYOTOKBN-T            OCCURS  4.                     
               05  WRK-BYOTOKBN          PIC X(02).                     
      *                                                                 
       01  WRK-BYOTOKBN-DATA             PIC X(02).                     
      *    初回番号(最新の入院歴分)                                                                 
       01  WRK-SHONUM                    PIC 9(03).                     
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    ワーク入退院履歴
       01  WRK-RECE68-REC. 
           COPY    "CPRCF068.INC"  REPLACING  //RECE68//
                                   BY         //WRK-RECE68//.
      *
      *    ワーク入院履歴
       01  WRK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNYUINRRK-//.
      *
      *    固定値
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
           COPY    "CPSK1001.INC".
      *
      *    病棟情報                                                     
           COPY    "CPSK5001.INC".                                      
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
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
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
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
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
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
                                       FLG-AREA 
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCR0620"          TO  JOB-PGID
           MOVE    "保険情報編集"      TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
           IF      FLG-END             =   ZERO
               IF      WRK-PARA-SRYYM  NOT =  ZERO
      *            医療機関情報
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "1001"              TO  SYS-KANRICD
                   MOVE    "*"                 TO  SYS-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)              
                   MOVE    "01"                TO  ORC-DBYMD (7:2)              
                   PERFORM 960-SYSKANRI-READ-SEC
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
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL   FLG-SYSKANRI  =    1
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "SYSKANRI-KEY8" TO  ORC-DBPATH
                           PERFORM 900-SYSKANRI-READ-SEC
      *
                       END-PERFORM
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1                 TO  FLG-END
                   END-IF
      *
                   MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
                   MOVE    "SYSKANRI-KEY8"     TO  ORC-DBPATH
                   CALL    "ORCMCPSUB"          USING
                                               MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               END-IF
           END-IF  
      *
           IF      FLG-END             =   ZERO
      *        患者番号構成管理情報編集
               MOVE    SPACE               TO  SYSKANRI-REC
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1009"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)              
               MOVE    "01"                TO  ORC-DBYMD (7:2)              
               PERFORM 960-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               END-IF
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
                   MOVE    "RECE610"           TO  RECE61PARA-FILE-ID
                   MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
                   MOVE    "RECE680"           TO  RECE68PARA-FILE-ID
                   MOVE    "RECE690"           TO  RECE69PARA-FILE-ID
                   MOVE    "RECE670"           TO  RECE67PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
               ELSE
      *    請求書処理
                   MOVE    "RECE61X"           TO  RECE61PARA-FILE-ID
                   MOVE    "RECE62X"           TO  RECE62PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE62PARA-TERMID
               END-IF
               MOVE    WRK-PARA-TERMID     TO  RECE61PARA-TERMID
               MOVE    WRK-PARA-TERMID     TO  RECE68PARA-TERMID
               MOVE    WRK-PARA-TERMID     TO  RECE69PARA-TERMID
               MOVE    WRK-PARA-TERMID     TO  RECE67PARA-TERMID
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
           MOVE    ZERO                TO  FLG-RECE61
      *
           OPEN    OUTPUT              RECE62X-FILE
           CLOSE                       RECE62X-FILE
      *
           IF      IDZ             =   1               
               OPEN    OUTPUT              RECE68X-FILE
               CLOSE                       RECE68X-FILE
               OPEN    I-O                 RECE68-FILE
      *
               OPEN    OUTPUT              RECE69X-FILE
               CLOSE                       RECE69X-FILE
               OPEN    OUTPUT              RECE69-FILE
      *
               OPEN    OUTPUT              RECE67-FILE
               CLOSE                       RECE67-FILE
           END-IF    
      *
           OPEN    INPUT               RECE61-FILE
           IF      STS-RECE61      NOT =   ZERO
               DISPLAY "*** ORCR0620 RECE61 OPEN ERR *** " STS-RECE61 
               MOVE    1                   TO  FLG-END
               GO  TO  200-HENSYU-EXT
           END-IF
      *
           OPEN    I-O                 RECE62-FILE
      *
           PERFORM     RECE61-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE61  =   1
                                       OR      FLG-END     =   1
      *
               IF      IDZ             =   1               
                   PERFORM 230-RECE69-HENSYU-SEC
               END-IF
      *
      *        編集処理
      *        処理区分（社保・国保）により読み飛ばす
               EVALUATE    WRK-PARA-RECEKBN
                   WHEN    "0"
                       PERFORM 210-HENSYU-SEC
                   WHEN    "1"
                       IF      RECE61-HKNNUM   NOT =   "060" AND "067"
                           PERFORM 210-HENSYU-SEC
                       END-IF
                   WHEN    "2"
                       IF      RECE61-HKNNUM       =   "060" OR  "067"
                           PERFORM 210-HENSYU-SEC
                       END-IF
               END-EVALUATE
      *
               PERFORM     RECE61-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           CLOSE   RECE61-FILE
                   RECE62-FILE
      *
           IF      IDZ             =   1               
               CLOSE   RECE69-FILE
                       RECE68-FILE
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
           MOVE    SPACE                TO  RECE62-REC
           INITIALIZE                       RECE62-REC
      *
           MOVE    SPACE                TO  RJN-RATE-KBN
      *
           MOVE    RECE61-HOSPID        TO  RECE62-HOSPID
           MOVE    RECE61-SRYYM         TO  RECE62-SRYYM
           MOVE    RECE61-NYUGAIKBN     TO  RECE62-NYUGAIKBN
           MOVE    RECE61-PTID          TO  RECE62-PTID
           MOVE    RECE61-RECEKA        TO  RECE62-RECEKA
           IF      RECE61-HKNNUM (1:2)  =   "98"
               MOVE    RECE61-HKNNUM        TO  RECE62-HKNJANUM-KEY
           ELSE  
               MOVE    RECE61-HKNJANUM      TO  RECE62-HKNJANUM-KEY
           END-IF
           MOVE    RECE61-HOJOKBN       TO  RECE62-HOJOKBN-KEY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      RECE61-TEKSTYMDX (IDX)  =   SPACE
               IF      RECE62-TEKSTYMD     =   SPACE 
                   MOVE    RECE61-TEKSTYMDX (IDX)  TO  RECE62-TEKSTYMD
               ELSE
                   IF      RECE62-TEKSTYMD <   RECE61-TEKSTYMDX (IDX)
                       MOVE    RECE61-TEKSTYMDX (IDX)  
                                               TO  RECE62-TEKSTYMD
                   END-IF
               END-IF
               IF      RECE62-TEKSTYMD-X   =   SPACE 
                   MOVE    RECE61-TEKSTYMDX (IDX)  TO  RECE62-TEKSTYMD-X
                   MOVE    RECE61-TEKEDYMDX (IDX)  TO  RECE62-TEKEDYMD-X
               ELSE
                   IF      RECE62-TEKSTYMD-X >   RECE61-TEKSTYMDX (IDX)
                       MOVE    RECE61-TEKSTYMDX (IDX)  
                                               TO  RECE62-TEKSTYMD-X
                   END-IF
                   IF      RECE62-TEKEDYMD-X <   RECE61-TEKEDYMDX (IDX)
                       MOVE    RECE61-TEKEDYMDX (IDX)  
                                               TO  RECE62-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
      *    患者番号変換
           MOVE    RECE61-PTNUM        TO  RECE62-PTNUM
      *
      *    保険組合せ番号情報
           PERFORM VARYING IDX    FROM   1   BY   1
                   UNTIL   IDX          >     10
                   OR          RECE61-HKNCOMBI-O(IDX)  =   ZERO
               MOVE    RECE61-HKNCOMBI-O(IDX)
                                       TO  RECE62-HKNCOMBI(IDX)
           END-PERFORM    
      *    診療科情報のセット
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       20
                   OR      RECE61-SRYKA (IDX)  =   SPACE
               MOVE    RECE61-SRYKA (IDX)  TO  RECE62-SRYKA (IDX)
           END-PERFORM
      *    審査支払機関
           IF      RECE61-HKNNUM        =   "060"   OR    "067"
               MOVE    2                TO  RECE62-TEISYUTUSAKI
           ELSE
               MOVE    1                TO  RECE62-TEISYUTUSAKI
           END-IF
      *    患者情報の取得
           MOVE    RECE61-KANANAME     TO  RECE62-KANANAME
           MOVE    RECE61-NAME         TO  RECE62-NAME
           MOVE    RECE61-SEX          TO  RECE62-SEX
           MOVE    RECE61-BIRTHDAY     TO  RECE62-BIRTHDAY
           MOVE    RECE61-DEATHKBN     TO  RECE62-DEATHKBN
           MOVE    RECE61-TSTPTNUMKBN  TO  RECE62-TSTPTNUMKBN
           MOVE    RECE61-AGE          TO  WRK-NENREI
                                           RECE62-AGE
      *    県番号
           MOVE    RECE61-PREFNUM      TO  RECE62-PREFNUM
      *    患者保険情報テーブルから主保険情報の取得
           IF  RECE61-HKNID             NOT =   ZERO
               INITIALIZE                           PTHKNINF-REC
               MOVE    RECE61-HOSPID        TO      PTHKN-HOSPID
               MOVE    RECE61-PTID          TO      PTHKN-PTID
               MOVE    RECE61-HKNID         TO      PTHKN-HKNID
               MOVE    PTHKNINF-REC         TO      MCPDATA-REC
      *********MOVE    RECE62-TEKSTYMD      TO      ORC-DBYMD
               MOVE    "DBSELECT"           TO      MCP-FUNC
      *********MOVE    "PTHKNINF-KEY3"      TO      ORC-DBPATH
               MOVE    "PTHKNINF-KEY"       TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
      *********    MOVE    "PTHKNINF-KEY3"      TO      ORC-DBPATH
                   MOVE    "PTHKNINF-KEY"       TO      ORC-DBPATH
                   PERFORM  PTHKNINF-READ-SEC
                   IF       FLG-PTHKNINF        =   ZERO
                        MOVE   PTHKN-HKNID      TO  RECE62-HKNID
                        MOVE   PTHKN-HKNNUM     TO  RECE62-HKNNUM
      *                 法別番号のセット
                        INITIALIZE                  HKNNUM-REC
                        MOVE   RECE61-HOSPID    TO  HKN-HOSPID
                        MOVE   PTHKN-HKNNUM     TO  HKN-HKNNUM
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
                                 (FLG-HKNNUM     =    1)      OR
                                 ((HKN-TEKSTYMD(1:6) <=
                                                   RECE61-SRYYM) AND
                                 (RECE61-SRYYM      <=
                                                   HKN-TEKEDYMD(1:6)))
                                MOVE   "HKNNUM-KEY2" TO  ORC-DBPATH
                                PERFORM   HKNNUM-READ-SEC
                            END-PERFORM
                            IF       FLG-HKNNUM     =   ZERO
                                MOVE   HKN-HBTNUM TO RECE62-HKNHBTNUM
                                MOVE   SPACE        TO    WRK-JIHI-FLG
                                IF     HKN-HKNKOHSBTKBN    =    "8"
                                     MOVE   "1"     TO    WRK-JIHI-FLG
                                END-IF
                            END-IF
                        END-IF
                        MOVE   "HKNNUM-KEY2"       TO  WK-PATH
                        PERFORM   CSR-CLOSE-SEC
      *                 
                        MOVE   PTHKN-HKNJANUM   TO  RECE62-HKNJANUM
                        MOVE   PTHKN-HONKZKKBN  TO  RECE62-HONKZKKBN
                                                    WK-HONKZKKBN
                        MOVE   PTHKN-HOJOKBN    TO  RECE62-HOJOKBN
                        MOVE   PTHKN-CONTKBN    TO  RECE62-CONTKBN
                        MOVE   PTHKN-KIGO       TO  RECE62-KIGO
                        MOVE   PTHKN-NUM        TO  RECE62-NUM
                        MOVE   SPACE            TO  RECE62-CERTNUM
                   END-IF
               END-IF
      *         
      *********MOVE    "PTHKNINF-KEY3"      TO      WK-PATH
               MOVE    "PTHKNINF-KEY"       TO      WK-PATH
               PERFORM   CSR-CLOSE-SEC
           END-IF
      *    患者公費情報テーブルから老人保険情報の取得
           IF  RECE61-KOHHKNNUM-O(1)        =       "027"
           OR  RECE61-KOHID                 >       ZERO
               INITIALIZE                           PTKOHINF-REC
               MOVE    RECE61-HOSPID        TO      PTKOH-HOSPID
               MOVE    RECE61-PTID          TO      PTKOH-PTID
               MOVE    RECE61-KOHID         TO      PTKOH-KOHID
               MOVE    PTKOHINF-REC         TO      MCPDATA-REC
               MOVE    "DBSELECT"           TO      MCP-FUNC
               MOVE    "PTKOHINF-KEY"       TO      ORC-DBPATH
               CALL    "ORCMCPSUB"           USING   MCPAREA
                                                     ORCMCPAREA
                                                     MCPDATA-REC
               IF      MCP-RC               =       ZERO
                   MOVE    "PTKOHINF-KEY"   TO      ORC-DBPATH
                   PERFORM  PTKOHINF-READ-SEC
                   IF       FLG-PTKOHINF        =   ZERO
                        MOVE   PTKOH-KOHID      TO  RECE62-RJNHKNID
                        MOVE   PTKOH-KOHNUM     TO  RECE62-RJNHKNNUM
                        IF  ( PTKOH-TEKSTYMD (1:6)
                                                  =   RECE61-SRYYM    )
                        AND ( PTKOH-TEKSTYMD      >   RECE62-TEKSTYMD )
                           MOVE    PTKOH-TEKSTYMD  TO  RECE62-TEKSTYMD
                        END-IF
      *                 法別番号のセット
                        INITIALIZE                  HKNNUM-REC
                        MOVE   RECE61-HOSPID    TO  HKN-HOSPID
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
                                 (FLG-HKNNUM     =    1)      OR
                                ((HKN-TEKSTYMD(1:6) <=
                                                   RECE61-SRYYM) AND
                                 (RECE61-SRYYM      <=
                                                   HKN-TEKEDYMD(1:6)))
                                MOVE   "HKNNUM-KEY2"  TO  ORC-DBPATH
                                PERFORM   HKNNUM-READ-SEC
                            END-PERFORM
                            IF       FLG-HKNNUM     =   ZERO
                                MOVE   HKN-HBTNUM TO RECE62-RJNHBTNUM
                            END-IF
                        END-IF
                        MOVE   "HKNNUM-KEY2"       TO  WK-PATH
                        PERFORM   CSR-CLOSE-SEC
      *                  
                        MOVE   PTKOH-FTNJANUM   TO  RECE62-RJNFTNJANUM
                        MOVE   PTKOH-JKYSNUM    TO  RECE62-RJNJKYSNUM
      *                 老人保険の患者について負担割合を退避
                        IF   RECE62-SRYYM   >=   "200210"
                            EVALUATE    PTKOH-PAYKBN
                                WHEN    "01"
                                    MOVE      "1"   TO  RJN-RATE-KBN
                                WHEN    "02"
                                    MOVE      "2"   TO  RJN-RATE-KBN
                            END-EVALUATE
                        END-IF
                   END-IF
               END-IF
               MOVE   "PTKOHINF-KEY"      TO  WK-PATH
               PERFORM   CSR-CLOSE-SEC
           END-IF
      *
      *    長期高額療養費の編集
           MOVE    SPACE              TO   WRK-CHOKI
           PERFORM   VARYING   IDX   FROM    1   BY   1
                     UNTIL    (IDX     >   4)    
                     OR       (RECE61-KOHHKNNUM-O(IDX)  =   SPACE)
               IF    RECE61-KOHHKNNUM-O(IDX)     =    "027"
                   CONTINUE
               ELSE
                 INITIALIZE                        HKNNUM-REC
                 MOVE    RECE61-HOSPID         TO  HKN-HOSPID
                 MOVE    RECE61-KOHHKNNUM-O(IDX) TO  HKN-HKNNUM
                 MOVE    "00"                  TO  HKN-PAYKBN
                 MOVE    RECE61-SRYYM          TO  HKN-TEKSTYMD(1:6)
                 MOVE    "01"                  TO  HKN-TEKSTYMD(7:2)
                 MOVE    RECE61-SRYYM          TO  HKN-TEKEDYMD(1:6)
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
                           MOVE    "02"         TO  RECE62-TOKKI
                                                       (TOKKI-IDX)
                           MOVE    RECE61-KOHHKNNUM-O(IDX)  TO
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
                     OR       (RECE61-KOHID-O(IDX)  =   ZERO)  
      ******** IF  (RECE61-KOHHKNNUM-O (IDX)  =       "027")  OR
      ********     (RECE61-KOHHKNNUM-O (IDX)  =   WRK-CHOKI)
               IF  (RECE61-KOHHKNNUM-O (IDX)  =       "027")
                     CONTINUE
               ELSE
                   IF      RECE61-KOHHKNNUM-O (IDX)  =   WRK-CHOKI
                       MOVE   "4"             TO  WRK-RECESKYKBN
                   ELSE
                       MOVE   RECE61-RECESKYKBN-O (IDX)
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
      *    組合せ公費保険番号情報のセット
           PERFORM   KOHNUMHC-SET-SEC
      *
      *****補足区分・レセプト記載外公費のセット
      *****IF      RECE61-ETC-HKNNUM   NOT =   SPACE
      *****    MOVE    "1"                 TO  RECE62-HOSOKUKBN
      *****    MOVE    RECE61-ETC-HKNNUM   TO  RECE62-ETC-KOHNUM (1)
      *****END-IF  
      *****IF      WRK-CHOKI           NOT =   SPACE
      *****    PERFORM VARYING IDX FROM    1   BY  1
      *****            UNTIL   IDX >       4
      *****        IF         RECE62-ETC-KOHNUM (IDX)  =   SPACE
      *****            MOVE    WRK-CHOKI   TO  RECE62-ETC-KOHNUM (IDX)
      *****            MOVE    4           TO  IDX
      *****        END-IF
      *****    END-PERFORM  
      *****END-IF  
      *
      *    特記事項のセット
      *    老保の編集
           MOVE    ZERO       TO     FLG-ROUJINHOKEN
           IF      RECE62-HKNNUM        =   SPACE
      ***  AND     RECE62-KOHHBTNUM (1)   NOT =  SPACE
      ***  AND     RECE62-KOHHBTNUM (2) =   SPACE
      ***  AND     RECE62-KOHHBTNUM (3) =   SPACE
      ***  AND     RECE62-KOHHBTNUM (4) =   SPACE
               IF  (   RECE61-SRYYM         >=  "200210"            )
               AND ((  RECE62-AGE           >=  75             ) OR 
                    (( RECE62-BIRTHDAY      <=  19320930 ) AND
                     ( RECE62-AGE           >=  70       )     )    )
                   ADD      1           TO  TOKKI-IDX
                   MOVE    "04"         TO  RECE62-TOKKI (TOKKI-IDX)
               ELSE
                   INITIALIZE                       PTKOHINF-REC
                   MOVE    RECE61-HOSPID        TO  PTKOH-HOSPID
                   MOVE    RECE61-PTID          TO  PTKOH-PTID
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
                           IF      PTKOH-TEKSTYMD(1:6)  <=  RECE61-SRYYM
                           AND     RECE61-SRYYM         <=
                                                    PTKOH-TEKEDYMD(1:6)
                           AND     PTKOH-SAKJOKBN   =  SPACE
                               ADD      1           TO  TOKKI-IDX
                               MOVE    "04"         TO  RECE62-TOKKI
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
      *    年金履歴より
           INITIALIZE                      TNKRRK-REC
           MOVE    RECE61-HOSPID       TO  TNKRRK-HOSPID
           MOVE    RECE61-PTID         TO  TNKRRK-PTID
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
                    (( TNKRRK-STYMD(1:6)  <=  RECE61-SRYYM   AND
                       RECE61-SRYYM       <=  TNKRRK-EDYMD(1:6) ))  
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
      *    低所得より
           INITIALIZE                      TSYRRK-REC
           MOVE    RECE61-HOSPID       TO  TSYRRK-HOSPID
           MOVE    RECE61-PTID         TO  TSYRRK-PTID
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
                  (( TSYRRK-NINSTYMD(1:6) <= RECE61-SRYYM   AND
                     RECE61-SRYYM         <= TSYRRK-NINEDYMD(1:6) ))  
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
                   MOVE     SPACE   TO       RECE62-TEISYOTOKU
               WHEN    0             ALSO     1
                   MOVE     "2"     TO       RECE62-TEISYOTOKU
                   IF  (TSYRRK-SKJGNSTYMD     NOT =  SPACE       )
                   AND (TSYRRK-SKJGNSTYMD(1:6)   <=  RECE61-SRYYM)
                   AND (TSYRRK-NINEDYMD  (1:6)   >=  RECE61-SRYYM)
                      MOVE  "3"     TO       RECE62-TEISYOTOKU
                   END-IF                                     
               WHEN    0             ALSO     0
               WHEN    1             ALSO     0
                   MOVE     "1"     TO       RECE62-TEISYOTOKU
           END-EVALUATE
      *     
      *    高齢者フラグ
           IF      RECE62-SRYYM      >=  "200210"
              IF    ( RECE62-AGE          >=  70      )   OR
                    ( RECE62-AGE          >=  65  AND                    
                      RECE61-KOHID    NOT  =   ZERO   )   OR             
                    ( RECE62-AGE          >=  65  AND                    
                      FLG-ROUJINHOKEN      =   1      )
                  MOVE    "1"                 TO  RECE62-KOUREI
      *
                  IF  RECE62-RECESYUBETU   =   "8888"
                      MOVE  SPACE       TO  RECE62-KOUREI
                  END-IF
                  IF  RECE62-HKNNUM        =   SPACE 
                      MOVE  SPACE       TO  RECE62-KOUREI
                  END-IF
      *
              END-IF
           END-IF
      *
      *    病院・診療所区分
           MOVE    SYS-1001-HOSPSBT TO  RECE62-HOSPSBT               
      *
      *    レセプト種別不明エラー情報
           IF      RECE61-TEISEIKBN      =   "1"
               MOVE    RECE61-ERR-ERRKBN     TO  RECE62-ERR-ERRKBN
               MOVE    RECE61-ERR-DAY        TO  RECE62-ERR-DAY
               MOVE    RECE61-ERR-HKNCOMBINUM
                                             TO  RECE62-ERR-HKNCOMBINUM
               MOVE    RECE61-ERR-TEKSTYMD   TO  RECE62-ERR-TEKSTYMD
               MOVE    RECE61-ERR-TEKEDYMD   TO  RECE62-ERR-TEKEDYMD
           END-IF 
      *
      *    入退院履歴より情報取得
           IF      IDZ             =   1               
               PERFORM 240-PTNYUINRRK-SYORI-SEC
           END-IF
      *
      *    印刷順
           IF      IDZ                 =   1
               MOVE    RECE61-PRTSEQ       TO  RECE62-PRTSEQ
           END-IF
      *
           PERFORM     220-DATA-WRITE-SEC
      *
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
           MOVE    RECE61-HOSPID        TO      PTKOH-HOSPID
           MOVE    RECE61-PTID          TO      PTKOH-PTID
           MOVE    RECE61-KOHID-O(IDX)  TO      PTKOH-KOHID
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
                   MOVE   PTKOH-KOHID      TO  RECE62-KOHID(IDX2)
                   MOVE   PTKOH-KOHNUM     TO  RECE62-KOHNUM(IDX2)
                   MOVE   WRK-RECESKYKBN   TO  RECE62-RECESKYKBN(IDX2)
      *            法別番号のセット
                   INITIALIZE                  HKNNUM-REC
                   MOVE   RECE61-HOSPID    TO  HKN-HOSPID
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
                          ((HKN-TEKSTYMD(1:6) <=  RECE61-SRYYM) AND
                           (RECE61-SRYYM      <=  HKN-TEKEDYMD(1:6)))
                          MOVE   "HKNNUM-KEY2"  TO  ORC-DBPATH
                          PERFORM   HKNNUM-READ-SEC
                       END-PERFORM
                       IF       FLG-HKNNUM     =   ZERO
                           MOVE   HKN-HBTNUM  TO RECE62-KOHHBTNUM(IDX2)
                       END-IF
                   END-IF
                   MOVE   "HKNNUM-KEY2"       TO  WK-PATH
                   PERFORM   CSR-CLOSE-SEC
      *            
                   MOVE   PTKOH-FTNJANUM  TO RECE62-KOHFTNJANUM(IDX2)
                   MOVE   SPACE           TO RECE62-KOHNINIKYUKBN(IDX2)
      *             
      *            生保のとき月代わり受給者番号マスタより受給者番号をセット 
                   IF      PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM
                       MOVE    RECE62-HOSPID   TO  MONTHLYNUM-HOSPID
                       MOVE    RECE62-PTID     TO  MONTHLYNUM-PTID
                       MOVE    WRK-CONS-SEIHO-HKNNUM
                                               TO  MONTHLYNUM-KOHNUM
                       MOVE    RECE62-SRYYM    TO  MONTHLYNUM-SRYYM
                       MOVE    RECE62-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF    ( FLG-MONTHLYNUM          =   ZERO  )
      *****************AND   ( MONTHLYNUM-JKYSNUM  NOT =   SPACE )
                           MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                       ELSE                       
                           MOVE   PTKOH-JKYSNUM
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                       END-IF
                   ELSE                       
                       MOVE   PTKOH-JKYSNUM
                                           TO  RECE62-KOHJKYSNUM (IDX2)
                   END-IF    
               END-IF
           END-IF
           MOVE   "PTKOHINF-KEY"      TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    神奈川地方公費対応
           IF    ( RECE62-PREFNUM  =   "14"           )
           AND   ( RECE62-TEISYUTUSAKI
                                   =   "2"            )
           AND   ( RECE62-KOHNUM      (IDX2)
                                   =   "180" OR "280" )
           AND   ( RECE62-KOHFTNJANUM (IDX2)
                                   =   SPACE          )
               MOVE   "4"          TO  RECE62-RECESKYKBN(IDX2)
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
           IF      RECE61-TEISEIKBN     =   "1"
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    地方公費データのとき主保険が無かったら不明分とする
           IF      IDZ                  =   2
           AND     RECE62-HKNNUM        =   SPACE
               MOVE    7                    TO  RECE62-ERR-ERRKBN
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *
      *    自費のときのレセプト種別を編集
           IF     (RECE62-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   "1"      ) 
               MOVE    8888                 TO  RECE62-RECESYUBETU    
               GO  TO  RECESYUBETU-SET-EXT
           END-IF
      *         
           IF     (RECE62-HKNNUM        NOT =   SPACE)   AND
                  (WRK-JIHI-FLG         =   SPACE)
               IF      RECE62-HKNNUM    =   "060"   OR   "067"
                   IF   RECE62-HKNNUM   =   "060"
                        MOVE   "1"      TO  WK-KOKUHO
                   ELSE
                        MOVE   "2"      TO  WK-KOKUHO
                   END-IF
               ELSE
                   MOVE    "1"          TO  WK-IHO
               END-IF
           END-IF
      *
           IF      RECE62-SRYYM     <      "200210" 
             IF      RECE62-RJNHKNNUM     =   "027"
                 MOVE        "1"          TO  WK-RJN
             END-IF
           END-IF
      *
           MOVE    ZERO            TO    WK-KOHI-CNT
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF    ( RECE62-KOHNUM     (IDX) NOT =   SPACE )
               AND   ( RECE62-RECESKYKBN (IDX)     =   SPACE )
                   ADD     1               TO    WK-KOHI-CNT
               END-IF
           END-PERFORM    
      *
      *    福岡地方公費対応
           IF      RECE62-PREFNUM      =   "40"
               IF  WK-KOKUHO               =   "1" OR  "2"
                   IF   (RECE62-KOHNUM   (1)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(1)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (2)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(2)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (3)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(3)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
                   IF   (RECE62-KOHNUM   (4)(1:1)  =   "1" OR  "2")
                   AND  (RECE62-KOHHBTNUM(4)   NOT =   "41"       )     
                       COMPUTE  WK-KOHI-CNT  =  WK-KOHI-CNT  -  1
                   END-IF
               END-IF
           END-IF
      *
      *    ３歳未満の場合、ＷＫ−ＨＯＮＫＺＫＫＢＮに"３"をセット
           IF      RECE61-HKNID            NOT =   ZERO
               IF      WRK-NENREI              <     3
                           MOVE   "3"          TO   WK-HONKZKKBN
               END-IF
           END-IF
      *
      *    １割、２割負担の高齢者の場合ＷＫ−ＨＯＮＫＺＫＫＢＮに
      *    "９"または"８"をセット
      *    老人保険の場合はＷＫ−ＲＪＮにセットする	
           IF  RECE61-HKNID             NOT =   ZERO
      *        保険組合せテーブルの検索
                   INITIALIZE                        HKNCOMBI-REC
                   MOVE    RECE61-HOSPID        TO   COMB-HOSPID
                   MOVE    RECE61-PTID          TO   COMB-PTID
                   MOVE    RECE61-HKNCOMBI-O(1) TO   COMB-HKNCOMBINUM
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
                       END-IF
                   END-IF
                   MOVE   "HKNCOMBI-KEY"        TO   WK-PATH
                   PERFORM   CSR-CLOSE-SEC
           END-IF
      *
           IF    ( WK-RJN          NOT =   SPACE )
           AND   ( RECE62-HONKZKKBN    =   "1"   )     
               IF      RECE62-HKNNUM   =   "063"   OR  "072"   OR
                                           "073"   OR  "074"   OR
                                           "075"
                   MOVE    "2"          TO  WK-IHO
               END-IF
           END-IF    
      *          
      *
      *    DISPLAY "WK-HONKZKKBN: " WK-HONKZKKBN 
      *    DISPLAY "RECE61-NYUGAIKBN: " RECE61-NYUGAIKBN 
      *    DISPLAY "WK-IHO: " WK-IHO 
      *    DISPLAY "WK-KOKUHO: " WK-KOKUHO 
      *    DISPLAY "WK-RJN: " WK-RJN 
      *    DISPLAY "WK-KOHI-CNT: " WK-KOHI-CNT 
           EVALUATE  WK-HONKZKKBN ALSO RECE61-NYUGAIKBN  ALSO
                     WK-IHO ALSO WK-KOKUHO ALSO WK-RJN ALSO WK-KOHI-CNT
      *保険単独
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1111            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1112            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1113            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1114            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1115            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1116            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1117            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1118            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1119            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1110            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1121            TO    RECE62-RECESYUBETU
      *保険と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1121            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1122            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1123            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1124            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1125            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1126            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1127            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1128            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1129            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  1
                   MOVE    1120            TO    RECE62-RECESYUBETU
      *保険と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1131            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1132            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1133            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1134            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1135            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1136            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1137            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1138            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1139            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       "1"   ALSO   SPACE  ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  SPACE  ALSO  2
                   MOVE    1130            TO    RECE62-RECESYUBETU
      *公費単独
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1211            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   1
                   MOVE    1212            TO    RECE62-RECESYUBETU
      *２種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1221            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   2
                   MOVE    1222            TO    RECE62-RECESYUBETU
      *３種の公費併用
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1231            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   SPACE  ALSO  SPACE  ALSO   3
                   MOVE    1232            TO    RECE62-RECESYUBETU
      *老人単独
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1317            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   ZERO
                   MOVE    1318            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1319            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   ZERO
                   MOVE    1310            TO    RECE62-RECESYUBETU
      *老人と公費１種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1327            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   1
                   MOVE    1328            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1329            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   1
                   MOVE    1320            TO    RECE62-RECESYUBETU
      *老人と公費２種の併用
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1337            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "9"    ALSO   2
                   MOVE    1338            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "1"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1339            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       "1"   ALSO   ANY    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    RECE62-RECESYUBETU
               WHEN    ANY   ALSO   "2"    ALSO
                       SPACE ALSO   "1"    ALSO  "8"    ALSO   2
                   MOVE    1330            TO    RECE62-RECESYUBETU
      *退職者単独
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1411            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1412            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1413            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1414            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1415            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1416            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1417            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1418            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1419            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  ZERO
                   MOVE    1410            TO    RECE62-RECESYUBETU
      *退職者と公費１種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1421            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1422            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1423            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1424            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1425            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1426            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1427            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1428            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1429            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  1
                   MOVE    1420            TO    RECE62-RECESYUBETU
      *退職者と公費２種の併用
               WHEN    "1"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1431            TO    RECE62-RECESYUBETU
               WHEN    "1"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1432            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1433            TO    RECE62-RECESYUBETU
               WHEN    "3"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1434            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1435            TO    RECE62-RECESYUBETU
               WHEN    "2"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1436            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1437            TO    RECE62-RECESYUBETU
               WHEN    "9"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1438            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "1"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1439            TO    RECE62-RECESYUBETU
               WHEN    "8"   ALSO   "2"    ALSO
                       SPACE ALSO   "2"    ALSO  SPACE  ALSO  2
                   MOVE    1430            TO    RECE62-RECESYUBETU
           END-EVALUATE
      *    DISPLAY   "RECE62-RECESYUBETU: " RECE62-RECESYUBETU
      *    ACCEPT     WRK-IN FROM CONSOLE
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
           MOVE    RECE61-HOSPID        TO      COMB-HOSPID
           MOVE    RECE61-PTID          TO      COMB-PTID
           MOVE    RECE61-HKNCOMBI-O(1) TO      COMB-HKNCOMBINUM
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
               END-IF
           END-IF
           MOVE   "HKNCOMBI-KEY"     TO    WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    保険番号テーブルの検索
           INITIALIZE                      HKNNUM-REC
           MOVE   RECE61-HOSPID      TO    HKN-HOSPID
           MOVE   RECE61-HKNNUM      TO    HKN-HKNNUM
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
                ((HKN-TEKSTYMD(1:6)   <=  RECE61-SRYYM)        AND
                 (RECE61-SRYYM        <=  HKN-TEKEDYMD(1:6)))
                   PERFORM   HKNNUM-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "HKNNUM-KEY2"        TO  WK-PATH
           PERFORM   CSR-CLOSE-SEC
      *
      *    保険組合せテーブルの補助区分から患者の負担割合を再セット
      *   （国保のみ）
              EVALUATE    RECE61-HKNNUM       
                  WHEN    "060"
                      IF       WRK-NENREI           <     3
                          IF    COMB-HOJOKBN    =    "0"  OR   "1" OR
                                                     "4"  OR   "5"
                              EVALUATE   COMB-HOJOKBN
                                  WHEN   "0"
                                      MOVE   ZERO    TO   RECE62-KYURATE
                                  WHEN   "1"
                                      MOVE   10      TO   RECE62-KYURATE
                                  WHEN   "4"
                                      MOVE   ZERO    TO   RECE62-KYURATE
                                  WHEN   "5"
                                      MOVE   10      TO   RECE62-KYURATE
                              END-EVALUATE
                          ELSE
                              MOVE  20      TO    RECE62-KYURATE
                          END-IF
                      ELSE
                          EVALUATE   COMB-HOJOKBN
                              WHEN   "0"
                                  MOVE   ZERO      TO   RECE62-KYURATE
                              WHEN   "1"
                                  MOVE   10        TO   RECE62-KYURATE
                              WHEN   "2"
                                  MOVE   20        TO   RECE62-KYURATE
                              WHEN   "3"
                                  MOVE   30        TO   RECE62-KYURATE
                              WHEN   "4"
                                  MOVE   ZERO      TO   RECE62-KYURATE
                              WHEN   "5"
                                  MOVE   10        TO   RECE62-KYURATE
                              WHEN   "6"
                                  MOVE   20        TO   RECE62-KYURATE
                              WHEN   "8"
                                  MOVE   20        TO   RECE62-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE62-KYURATE
                          END-EVALUATE
                      END-IF
                  WHEN    "067"
                      IF       WRK-NENREI           <     3
                          MOVE  20      TO    RECE62-KYURATE
                      ELSE
                        IF      COMB-HOJOKBN          NOT =  SPACE
                          EVALUATE   COMB-HOJOKBN
                              WHEN   "8"
                                  MOVE   20        TO   RECE62-KYURATE
                              WHEN   "9"
                                  MOVE   10        TO   RECE62-KYURATE
                          END-EVALUATE
                        ELSE
                          IF      COMB-HONKZKKBN        =    "1"
                              MOVE   HKN-HON-GAIKFTNRATE
                                                   TO   RECE62-KYURATE
                          ELSE
                              MOVE   HKN-KZK-GAIKFTNRATE
                                                   TO   RECE62-KYURATE
                          END-IF
                        END-IF
                      END-IF
                  WHEN    OTHER
                      IF   WRK-NENREI           <     3
                          MOVE  20      TO    RECE62-KYURATE
                      ELSE
                          IF  COMB-HOJOKBN        =     SPACE
                              IF  COMB-HONKZKKBN      =     "1"
                                  MOVE HKN-HON-GAIKFTNRATE
                                                  TO  RECE62-KYURATE
                              ELSE
                                  MOVE HKN-KZK-GAIKFTNRATE
                                                  TO  RECE62-KYURATE
                              END-IF
                          ELSE
                              EVALUATE   COMB-HOJOKBN
                                  WHEN   "8"
                                      MOVE   20   TO  RECE62-KYURATE
                                  WHEN   "9"
                                      MOVE   10   TO  RECE62-KYURATE
                              END-EVALUATE
                         END-IF
                     END-IF
              END-EVALUATE
      *
              IF  RJN-RATE-KBN      NOT =    SPACE
                  EVALUATE   RJN-RATE-KBN
                      WHEN   "1"
                             MOVE   10        TO  RECE62-KYURATE
                      WHEN   "2"
                             MOVE   20        TO  RECE62-KYURATE
                  END-EVALUATE
              END-IF
      *
      *    船員保険（被保険者）＋通勤災害フラグセット
           IF    RECE61-HKNNUM           =    "002"
               EVALUATE   COMB-HOJOKBN
                    WHEN   "3"
                    WHEN   "C"
                    WHEN   "F"
                        MOVE     "1"     TO     RECE62-SENIN-TUUSAI
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
             OR     (RECE61-HKNCOMBI-O(IDX)   =   ZERO)   
      *        保険組合せテーブルの検索
               INITIALIZE                           HKNCOMBI-REC
               MOVE    RECE61-HOSPID        TO      COMB-HOSPID
               MOVE    RECE61-PTID          TO      COMB-PTID
               MOVE    RECE61-HKNCOMBI-O(IDX)
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
                       MOVE    COMB-HKNNUM   TO  RECE62-HKNNUMHC (IDX)
                       IF   COMB-KOH1HKNNUM  =   "027"
                         MOVE COMB-KOH2HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH2HKNNUM
                                             TO RECE62-KOHNUMHC(IDX 1)
                             MOVE COMB-KOH2ID
                                             TO RECE62-KOHIDHC (IDX 1)
                         END-IF
                         MOVE COMB-KOH3HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH3HKNNUM
                                             TO RECE62-KOHNUMHC(IDX 2)
                             MOVE COMB-KOH3ID
                                             TO RECE62-KOHIDHC (IDX 2)
                         END-IF
                         MOVE COMB-KOH4HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH4HKNNUM
                                             TO RECE62-KOHNUMHC(IDX 3)
                             MOVE COMB-KOH4ID
                                             TO RECE62-KOHIDHC (IDX 3)
                         END-IF
                       ELSE
                         MOVE COMB-KOH1HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH1HKNNUM
                                              TO RECE62-KOHNUMHC(IDX 1)
                             MOVE COMB-KOH1ID
                                              TO RECE62-KOHIDHC (IDX 1)
                         END-IF
                         MOVE COMB-KOH2HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH2HKNNUM
                                              TO RECE62-KOHNUMHC(IDX 2)
                             MOVE COMB-KOH2ID
                                              TO RECE62-KOHIDHC (IDX 2)
                         END-IF
                         MOVE COMB-KOH3HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH3HKNNUM
                                              TO RECE62-KOHNUMHC(IDX 3)
                             MOVE COMB-KOH3ID
                                              TO RECE62-KOHIDHC (IDX 3)
                         END-IF
                         MOVE COMB-KOH4HKNNUM TO WRK-HKNNUM
                         PERFORM KOHNUM-CHECK-SEC
                         IF      FLG-CHK         =   1
                             MOVE COMB-KOH4HKNNUM
                                              TO RECE62-KOHNUMHC(IDX 4)
                             MOVE COMB-KOH4ID
                                              TO RECE62-KOHIDHC (IDX 4)
                         END-IF 
                       END-IF
                   END-IF
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
                       OR      RECE61-KOHHKNNUM-O (IDY)  =   SPACE
                       OR      FLG-CHK     =  1
                   IF      WRK-HKNNUM      =   RECE61-KOHHKNNUM-O (IDY)
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
      *    データ出力処理
      *****************************************************************
       220-DATA-WRITE-SEC                SECTION.
      *
           WRITE   RECE62-REC
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
       230-RECE69-HENSYU-SEC                   SECTION.
      *
           IF      RECE61-HKNID             NOT =   ZERO
      ***  AND     RECE61-HKNNUM            NOT =   WRK-CONS-JIHI-HKNNUM
           AND     RECE61-HKNNUM(1:2)       NOT =   "98"
           AND     RECE61-TEISEIKBN             =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       10
                       OR      RECE61-TEKSTYMDX (IDX)  =   SPACE
                   MOVE    SPACE                TO  RECE69-REC
                   INITIALIZE                       RECE69-REC
                   MOVE    RECE61-HOSPID        TO  RECE69-HOSPID
                   MOVE    RECE61-SRYYM         TO  RECE69-SRYYM
                   MOVE    RECE61-NYUGAIKBN     TO  RECE69-NYUGAIKBN
                   MOVE    RECE61-PTID          TO  RECE69-PTID
                   MOVE    RECE61-RECEKA        TO  RECE69-RECEKA
                   MOVE    RECE61-TEKSTYMDX (IDX)
                                                TO  RECE69-TEKSTYMD
      *
                   MOVE    RECE61-TEKSTYMDX (IDX)
                                                TO  RECE69-TEKSTYMDX
                   MOVE    RECE61-TEKEDYMDX (IDX)
                                                TO  RECE69-TEKEDYMDX
      * 
                   IF      RECE61-HKNNUM       =   "060"   OR  "067"
                       MOVE    2               TO  RECE69-TEISYUTUSAKI
                   ELSE
                       MOVE    1               TO  RECE69-TEISYUTUSAKI
                   END-IF
      *                                          
                   INITIALIZE                       PTHKNINF-REC
                   MOVE    RECE61-HOSPID        TO  PTHKN-HOSPID
                   MOVE    RECE61-PTID          TO  PTHKN-PTID
                   MOVE    RECE61-HKNNUM        TO  PTHKN-HKNNUM
                   MOVE    RECE61-TEKSTYMDX(IDX)
                                                TO  PTHKN-TEKSTYMD
                   MOVE    RECE61-TEKEDYMDX(IDX)
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
                           MOVE   PTHKN-HKNID      TO  RECE69-HKNID
                           MOVE   PTHKN-HKNNUM     TO  RECE69-HKNNUM
                           MOVE   PTHKN-HKNJANUM   TO  RECE69-HKNJANUM
                           MOVE   PTHKN-HONKZKKBN  TO  RECE69-HONKZKKBN
                           MOVE   PTHKN-HOJOKBN    TO  RECE69-HOJOKBN
                           MOVE   PTHKN-CONTKBN    TO  RECE69-CONTKBN
                           MOVE   PTHKN-KIGO       TO  RECE69-KIGO
                           MOVE   PTHKN-NUM        TO  RECE69-NUM
                           MOVE   SPACE            TO  RECE69-CERTNUM
      *         
                           INITIALIZE                  HKNNUM-REC
                           MOVE   RECE61-HOSPID    TO  HKN-HOSPID
                           MOVE   PTHKN-HKNNUM     TO  HKN-HKNNUM
                           MOVE   RECE61-TEKSTYMDX (IDX)
                                                   TO  HKN-TEKSTYMD
                           MOVE   RECE61-TEKEDYMDX (IDX)
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
                                   MOVE   HKN-TANSEIDONAME
                                               TO  RECE69-TANSEIDONAME
                                   MOVE   HKN-HBTNUM
                                               TO RECE69-HKNHBTNUM
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
                   WRITE   RECE69-REC
                   ADD     1                  TO  CNT-RECE69  
               END-PERFORM
           END-IF           
      *
           .
       230-RECE69-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴より情報取得処理
      *****************************************************************
       240-PTNYUINRRK-SYORI-SEC                   SECTION.
      *
           INITIALIZE                      WRK-BYOTOKBN-TABLE
                                           WRK-SHONUM
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    RECE62-HOSPID       TO  PTNYUINRRK-HOSPID
           MOVE    RECE62-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE62-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE62-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
             IF    ( PTNYUINRRK-TENNYUYMD  >   RECE62-TEKEDYMD-X )
             AND   ( RECE62-TEKEDYMD-X NOT =   SPACE             )
                 CONTINUE
             ELSE
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   RECE62-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   RECE62-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック（会計に対象の組合せがないときは全て）
               IF      RECE61-DAY-AREA     =   ALL "0"
                   MOVE    "1"   TO   RECE62-NYUINRYOFLG
                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY >       10
                               OR  RECE62-HKNCOMBI (IDY)   =   ZERO
                           IF      RECE61-DAY (IDX)
                                           =   RECE62-HKNCOMBI (IDY)
                               PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                               MOVE    10                  TO  IDY
                               MOVE   IDX5                 TO  IDX
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF
             END-IF          
      *                 
             PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM        
      *                 
           MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
      *
      *    診療科情報が未編集のとき
           IF      RECE62-SRYKA (1)    =   SPACE
               MOVE    RECE62-LASTSRYKA    TO  RECE62-SRYKA (1)
           END-IF
      *                                                                
      *    病棟区分並替え
      *                                                                
           PERFORM    VARYING   IDX3     FROM    1   BY  1             
                  UNTIL  ( IDX3                   >   4    )           
                    OR   ( WRK-BYOTOKBN(IDX3)     =  SPACE )           
             COMPUTE    IDX4  =  IDX3  +  1                            
             PERFORM  VARYING  IDX5  FROM  IDX4  BY  1                 
                        UNTIL  ( IDX5                >   4    )        
                          OR   ( WRK-BYOTOKBN(IDX5)  =  SPACE )        
                IF       WRK-BYOTOKBN(IDX3)  >                         
                                           WRK-BYOTOKBN(IDX5)          
                   MOVE  WRK-BYOTOKBN(IDX3) TO  WRK-BYOTOKBN-DATA      
                   MOVE  WRK-BYOTOKBN(IDX5) TO  WRK-BYOTOKBN(IDX3)     
                   MOVE  WRK-BYOTOKBN-DATA  TO  WRK-BYOTOKBN(IDX5)     
                END-IF                                                 
             END-PERFORM                                               
           END-PERFORM                                                 
      *                                                                
           PERFORM    VARYING   IDX3     FROM    1   BY  1             
                  UNTIL   IDX3    >     4                              
                MOVE    WRK-BYOTOKBN(IDX3)  TO  RECE62-BYOTOKBN(IDX3)  
           END-PERFORM                                                 
      *                                                                
           .
       240-PTNYUINRRK-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2401-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    病棟区分セット
           PERFORM    24011-BYOTOKBN-SET-SEC
      *
      *    診療科情報のセット
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       20
                   OR      PTNYUINRRK-NYUINKA
                                           =   RECE62-SRYKA (IDW)
               IF      RECE62-SRYKA (IDW)  =   SPACE  
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       10
                           OR      RECE62-HKNCOMBI (IDXX)
                                                   =   ZERO
                       IF      RECE62-HKNCOMBI (IDXX)   =
                                               PTNYUINRRK-HKNCOMBINUM
                           MOVE    PTNYUINRRK-NYUINKA  
                                               TO  RECE62-SRYKA (IDW)
                           MOVE    10  TO  IDXX
                       END-IF
                   END-PERFORM
                   MOVE    20          TO  IDW
               END-IF
           END-PERFORM
      *
      *    入院年月日・最終科
           IF      RECE62-LASTSRYKA    =   SPACE
               MOVE    PTNYUINRRK-NYUINYMD TO  RECE62-NYUINYMD
               MOVE    PTNYUINRRK-NYUINKA  TO  RECE62-LASTSRYKA
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM  
           END-IF
      *
      *    入退院履歴情報
           PERFORM 24012-RECE68-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE68          =   1                            
               MOVE    PTNYUINRRK-REC  TO  WRK-PTNYUINRRK-REC
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    RECE61-HOSPID   TO  PTNYUINRRK-HOSPID
               MOVE    RECE61-PTID     TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNYUINRRK-SHONUM
                                       TO  PTNYUINRRK-SHONUM
               MOVE    RECE61-SRYYM    TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"            TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               MOVE    "PTNYUINRRK-KEY22"  TO  ORC-DBPATH
               MOVE    "DBSELECT"      TO  MCP-FUNC
               CALL    "ORCMCPSUB"     USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK22 
               END-IF
      *
      *        入院年月日
               IF    ( PTNYUINRRK-NYUINYMD <    RECE62-NYUINYMD )
               AND   ( PTNYUINRRK-SHONUM   =    WRK-SHONUM      )
                   MOVE    PTNYUINRRK-NYUINYMD  TO  RECE62-NYUINYMD
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 24012-RECE68-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "PTNYUINRRK-KEY22"  TO  ORC-DBPATH
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCMCPSUB"         USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
           END-IF
           .
       2401-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟区分セット                                              
      *****************************************************************
       24011-BYOTOKBN-SET-SEC                SECTION.
      *
      *    異動による転室等の場合は転出日を前日に置き換える
           MOVE    SPACE               TO  WRK-PTNYUINRRK-TENSTUYMD
           IF      PTNYUINRRK-TENSTUYMD  =  PTNYUINRRK-TAIINYMD
              MOVE  PTNYUINRRK-TENSTUYMD  TO  WRK-PTNYUINRRK-TENSTUYMD
           ELSE
              INITIALIZE                STS-AREA-DAY
              INITIALIZE                LNK-DAY6-AREA
              MOVE    "61"              TO  LNK-DAY6-IRAI
              MOVE   PTNYUINRRK-TENSTUYMD
                                        TO  LNK-DAY6-KIJUN
              MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
              MOVE    -1                TO  LNK-DAY6-ZOGEN
              CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                LNK-DAY6-AREA
              MOVE    LNK-DAY6-KEISAN TO WRK-PTNYUINRRK-TENSTUYMD
           END-IF
      *    病棟情報                                           
           MOVE    SPACE               TO  SYSKANRI-REC     
           INITIALIZE                      SYSKANRI-REC     
           MOVE    "5001"              TO  SYS-KANRICD      
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD        
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  ORC-DBYMD        
           PERFORM 960-SYSKANRI-READ-SEC                      
           IF      FLG-SYSKANRI        =   ZERO               
               MOVE  MCPDATA-REC           TO  SYS-5001-REC
      *
               IF      SYS-5001-RECEDSPKBN =  SPACE
                   CONTINUE
               ELSE
                   IF    ( WRK-PTNYUINRRK-TENSTUYMD (1:6)
                                               <   RECE62-SRYYM )
                   OR    ( PTNYUINRRK-TENNYUYMD (1:6)
                                               >   RECE62-SRYYM )
                       CONTINUE
                   ELSE 
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       4
                               OR      WRK-BYOTOKBN (IDX3)
                                               =   SYS-5001-RECEDSPKBN
                           IF      WRK-BYOTOKBN (IDX3) =   SPACE
                               MOVE    SYS-5001-RECEDSPKBN
                                               TO   WRK-BYOTOKBN (IDX3)
                               MOVE    4           TO  IDX3
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF                                           
      *        病棟番号・種別
               MOVE    RECE62-SRYYM        TO  WRK-SRYYYMM
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   IF      RECE62-BYOTONUM(IDX3)   =  SPACE
                       MOVE    IDX3            TO  WRK-SRYDD
                       IF       PTNYUINRRK-TENNYUYMD  <=  WRK-SRYYMD
                       AND      PTNYUINRRK-TENSTUYMD  >=  WRK-SRYYMD
                           MOVE  SYS-5001-BTU-NUM
                                           TO  RECE62-BYOTONUM    (IDX3)
                           MOVE  SYS-5001-BTU-SBT
                                           TO  RECE62-BYOTOSYUBETU(IDX3)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .                                                           
       24011-BYOTOKBN-SET-EXT.                   
           EXIT.                                                       
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       24012-RECE68-WRITE-SEC         SECTION.
      *
           MOVE    RECE62-HOSPID       TO  RECE68-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE68-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE68-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE68-PTID
           MOVE    RECE62-RECEKA       TO  RECE68-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE68-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE68-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE68-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE68-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE68-TEKSTYMD
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE68-NYUINYMD
           MOVE    RECE68-REC          TO  WRK-RECE68-REC          
           PERFORM 900-RECE68-READ-SEC
           IF      FLG-RECE68          =   1
               MOVE    WRK-RECE68-REC      TO  RECE68-REC 
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE68-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE68-NYUINCHUKBN
               WRITE   RECE68-REC
               ADD     1                   TO  CNT-RECE68
           END-IF    
           .
       24012-RECE68-WRITE-EXT.
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
           DISPLAY "RECE61  CNT-IN  :" CNT-IN  (1) 
           DISPLAY "RECE62  CNT-OUT :" CNT-OUT (1) 
           DISPLAY "RECE68  CNT-OUT :" CNT-RECE68  
           DISPLAY "RECE69  CNT-OUT :" CNT-RECE69  
           DISPLAY "RECE61X CNT-IN  :" CNT-IN  (2) 
           DISPLAY "RECE62X CNT-OUT :" CNT-OUT (2) 
           DISPLAY "*** ORCR0620 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       PTINF-READ-EXT.
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
      *    システム管理マスタ読込
      *****************************************************************
       960-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
      *****MOVE    WRK-PARA-SRYYM  TO  ORC-DBYMD (1:6)
      *****MOVE    "01"            TO  ORC-DBYMD (7:2) 
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
       960-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
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
      *    入院履歴読込
      *****************************************************************
       900-PTNYUINRRK-INV-SEC         SECTION.
      *    
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    WRK-PATH             TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"            TO  MCP-FUNC
               MOVE    WRK-PATH             TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-PTNYUINRRK
                   MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
               ELSE
                   MOVE    1                    TO  FLG-PTNYUINRRK
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO      ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTNYUINRRK-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ１２）
      *****************************************************************
       900-PTNYUINRRK-KEY12-READ-SEC    SECTION.
      *    
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY12"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK12
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK12
           END-IF
           .
       900-PTNYUINRRK-KEY12-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *    
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY22"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK22
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK22
           END-IF
           .
       900-PTNYUINRRK-KEY22-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ１３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *    
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "PTNYUINRRK-KEY23"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK23
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK23
           END-IF
           .
       900-PTNYUINRRK-KEY23-READ-EXT.
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
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       910-HKNCOMBI-INV-EXT.
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
       RECE61-READ-SEC            SECTION.
      *
           READ    RECE61-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE61
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE61 " IDZ " PTID=" RECE61-PTID
      *
                   IF      WRK-PARA-SRYYM  =  ZERO
      *                医療機関ＩＤ編集
                       MOVE  RECE61-SRYYM     TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"             TO  WRK-SYS-SRYYMD(7:2)
                        PERFORM 900-SYSKANRI-HENSYU-SEC
                   END-IF
           END-READ
           .
       RECE61-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
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
      *    入退院履歴読込
      *****************************************************************
       900-RECE68-READ-SEC            SECTION.
      *
           READ    RECE68-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE68
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE68
           END-READ
           .
       900-RECE68-READ-EXT.
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
           .
       900-DBDISCONNECT-EXT.
           EXIT.
