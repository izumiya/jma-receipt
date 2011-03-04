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
       PROGRAM-ID.             ORCR0610.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 月次業務　明細書・請求書用ファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/01    NACL−藤原　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.03.03  診療科数１０から２０へ変更
      *  01.00.02    NACL-藤原    03/03/11  種別不明の情報を編集
      *  01.00.03    NACL-藤原    03/03/15  入院会計からの診療科の編集の修正
      *  01.00.04    NACL-藤原    03/04/21  レセのとき地方公費のみの場合は
      *                                     出力しない
      *  01.00.05    NACL-藤原    03/07/28  診療回数テーブルを編集
      *  01.00.06    NACL-藤原    03/11/18  社保・国保別で作成のとき
      *                                     読み飛ばしをしない
      *  01.00.07    NACL-藤原    03/11/19  老人＋原爆のレセ記載をシス管
      *                                     より取得
      *  01.00.08    NACL-門脇    04/01/14  診療行為の包括分は除く
      *                                     SRYACCT8→29
      *                                     SRYACCT9→30
      *  01.00.09    NACL-藤原    04/04/15  ＤＢ更新エラー時の処理
      *  01.00.10    NACL-藤原    04/06/15  印刷順の追加（個別作成時のみ）
      *
      *  レセ電機能追加
      *  01.02.01    NACL-藤原    04/07/30  入院レセ電処理の追加
      *
      *  中止処理追加
      *  01.03.01    NACL-藤原    04/10/07  中止処理の追加
      *  01.03.02    NACL-藤原    04/10/30  福岡県の地方公費対応（国保）
      *  01.03.03    NACL-藤原    04/11/01  記載なしの公費の処理
      *  01.03.04    NACL-藤原    05/03/03  老人又は前期高齢者のとき、
      *                                     月途中に保険者は変わらずに
      *                                     保険が変更になった場合は１枚
      *                                     にする
      *  01.03.05    NACL-藤原    05/07/05  前期高齢者のとき、月途中に
      *                                     給付割合が変更になった場合は
      *                                     それぞれにレセプトを作成する
      *
      *  02.06.00    NACL-竹田    05/05/30  realtime preview 処理追加
      *  02.06.01    NACL-門脇    05/06/01  長期高額療養レセ記載判定追加
      *  02.06.02    NACL-藤原    05/07/12  国保で本人・家族の変更があった
      *                                     場合、給付割合の変更がないとき
      *                                     は１枚にまとめる
      *  02.06.03    NACL-藤原    05/07/12  診療会計テーブルの読み込みを
      *                                     グループ化に変更
      *                                     SRYACCT-KEY29 -> SRYACCT-KEY36
      *                                     SRYACCT-KEY30 -> SRYACCT-KEY37
      *  02.06.04    NACL-藤原    05/07/28  レセプト特記事項編集情報のため
      *                                     のレセプト請求しない地方公費の
      *                                     を追加
      *  02.06.05    NACL-門脇    05/10/05  省庁対応
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *
      *  03.02.01    NACL-門脇    06/09/05  ２００６年１０月改正対応
      *                                     長期９７４対応
      *                                     福岡地方公費対応
      *
      *  03.03.01    NACL-藤原    06/10/15  ２００６年１０月改正対応
      *                                     公費の追加
      *  03.03.02    NACL-藤原    06/10/10  主保険がない場合、精神入院は
      *                                     公費単独のレセプトとする
      *  03.03.03    NACL-藤原    06/10/25  ２００６年１０月改正対応
      *                                     福岡地方公費の障害
      *  03.03.04    NACL-藤原    06/10/26  福岡地方公費のレセ記載判定の修正
      *  03.03.05    NACL-藤原    06/10/27  レセプトで生活保護の請求がな
      *                                     い場合記載の対象としない
      *  03.03.06    NACL-藤原    06/10/31  療養介護医療２４の追加
      *  03.03.07    NACL-藤原    06/11/02  福岡地方公費の障害のついて全て
      *                                     公費付加情報より記載判定を行う
      *  03.03.08    NACL-藤原    06/12/14  不明分のとき生保単独の上限
      *                                     チェックはしない
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *                                     MONFUNC対応
      *  03.04.03    NACL-藤原    07/08/20  北海道の地方公費の記載順対応
      *
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *  04.01.02    NACL-藤原    07/12/08  長崎県対応
      *                                     生保と被爆８６の記載順 
      *  04.01.03    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/03/10  心神喪失者等医療
      *                                    （法第８１条関係）の追加
      *  04.02.03    NACL-藤原    08/03/25  広域連合対応
      *                                     肝炎治療特別促進事業
      *                                     中国残留法人等の追加
      *  04.02.04    NACL-藤原    08/04/14  月途中の負担者番号変更対応
      *                                     （中国残留法人等の公費単独） 
      *  03.02.05    NACL-藤原    08/04/18  福岡地方公費の国保のとき
      *                                     平成２０年４月以降は公費付加
      *                                     情報より記載判定はしない
      *  04.02.06    NACL-藤原    08/04/23  後期高齢者の船員保険を本人とする
      *  04.02.07    NACL-門脇    08/08/07  精神通院の請求点判定追加
      *
      *  04.03.01    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.02    NACL-門脇    08/12/17  平成２１年１月改正対応
      *
      *  04.04.01    NACL-藤原    09/01/14  月途中の生保変更時の保険履歴
      *                                     記載対応
      *  04.04.02    NACL-藤原    09/05/19  東京都地方公費対応
      *                                     マル都（８２）は再優先とする
      *  04.04.03    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独）
      * 
      *  04.05.01    NACL-藤原    10/03/12  平成２２年４月改正対応
      *                                     平成２２年４月分以降の政府管
      *                                     掌をエラーとする
      *  04.05.02    NACL-藤原    10/04/13  愛知県対応
      *                                     生保と特定疾患９１の記載順 
      *  04.05.03    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *  04.05.04    NACL-藤原    11/03/01  山梨県対応
      *                                     老人医療費４１と特定疾患の
      *                                     記載順 
      *  04.05.05    NACL-藤原    11/03/02  徳島県対応
      *                                     乳児４５、４８が月途中で変更
      *                                     になった場合の記載順
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書  
           SELECT  RECE61-FILE     ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61-KEY
                                   FILE    STATUS  IS  STS-RECE61.
      *
           SELECT  RECE611-FILE    ASSIGN  RECE61PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE611-KEY
                                   FILE    STATUS  IS  STS-RECE611.
      *
      *    医療費請求書  
           SELECT  RECE61X-FILE    ASSIGN  RECE61XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE61X-KEY
                                   FILE    STATUS  IS  STS-RECE61X.
      *
           SELECT  RECE611X-FILE   ASSIGN  RECE61XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE611X-KEY
                                   FILE    STATUS  IS  STS-RECE611X.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書  
       FD  RECE61-FILE.
       01  RECE61-REC. 
           COPY    "CPRCF061.INC".
      *
       FD  RECE611-FILE.
       01  RECE611-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE611//.
      *
      *    医療費請求書  
       FD  RECE61X-FILE.
       01  RECE61X-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE61X//.
      *
       FD  RECE611X-FILE.
       01  RECE611X-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //RECE611X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE61//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE61X//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"
                                   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE61                              PIC X(02).
           03  STS-RECE611                             PIC X(02).
           03  STS-RECE61X                             PIC X(02).
           03  STS-RECE611X                            PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01). 
           03  FLG-HKNNUM                              PIC 9(01). 
           03  FLG-RECE61                              PIC 9(01).
           03  FLG-RECE61X                             PIC 9(01).
           03  FLG-ADDKBN                              PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-SEIKYU                              PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-KYUSEIRRK                           PIC 9(01).
           03  FLG-BTPARA                              PIC 9(01).
           03  FLG-SYUKA                               PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
      * 
           03  FLG-KOH                                 PIC 9(01).
           03  FLG-CONTKBN                             PIC 9(01).
           03  FLG-SET                                 PIC 9(01).
           03  FLG-LASTHKNID                           PIC 9(01). 
           03  FLG-KOHRECEDSP                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT1                                PIC 9(06).
           03  CNT-OUT2                                PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
           03  IDX1                                    PIC 9(03). 
           03  IDX2                                    PIC 9(03). 
           03  IDX3                                    PIC 9(03). 
           03  IDY                                     PIC 9(03). 
           03  IDZ                                     PIC 9(03).
           03  IDXX                                    PIC 9(03).
           03  TBL-MAX                                 PIC 9(03).
           03  SYSKAN-IDX                              PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                          PIC 9(10).
               05  KEY-N-HKNCOMBI                      PIC 9(04).
               05  KEY-N-SRYKA                         PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID                          PIC 9(10).
               05  KEY-O-HKNCOMBI                      PIC 9(04).
               05  KEY-O-SRYKA                         PIC X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC                         OCCURS  100.
               05  TBL-SYS1001                         PIC X(610).
      *
      *    パラメタ領域
       01  WRK-PARA.
           03  WRK-PARA-SRYYM                      PIC  X(06).
           03  WRK-PARA-TERMID                     PIC  X(16).
           03  WRK-PARA-SYSYMD                     PIC  X(08).
           03  WRK-PARA-SYOKBN                     PIC  X(01).
           03  WRK-PARA-RECEKBN                    PIC  X(01).
           03  WRK-PARA-JOBID                      PIC  9(07).
           03  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-PTID                       PIC  X(10).
           03  WRK-PARA-SRYYM-O                    PIC  X(06).
           03  WRK-PARA-HOSPNUM                    PIC  9(02).
      *
      *    一時領域
      *
       01  WRK-AREA.
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-CONTKBN             PIC X(01).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-PRTSEQ              PIC 9(03).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
           03  WRK-SYOKBN              PIC X(01).
           03  WRK-DBKBN               PIC X(01).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(01).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      * 
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      * 
           03  WRK-NENREI              PIC 9(03).
      *
           03  WRK-SYUKA-SRYYM         PIC X(06).
           03  WRK-SYUKA-PTID          PIC 9(10).
      *
           03  WRK-SEIHO-IDX           PIC 9(01).
           03  WRK-HIBAKU-IDX          PIC 9(01).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *              
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
      *    公費並替え用ワーク
           03  WRK-KOHI-AREA.
               05  WRK-KOHI-TBL        OCCURS  4.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHPAYKBN-O PIC  X(02).
                   07  WRK-RECESKYKBN-O
                                       PIC  X(01).
                   07  WRK-KOHTEKSTYMD-O
                                       PIC  X(08).
           03  WRK-KOHI.
               05  WRK-KOHID           PIC  9(10).
               05  WRK-KOHHKNNUM       PIC  X(03).
               05  WRK-KOHPAYKBN       PIC  X(02).
               05  WRK-RECESKYKBN      PIC  X(01).
               05  WRK-KOHTEKSTYMD     PIC  X(08).
      *
      *    請求区分別公費情報（１：レセプト　２：請求書）
           03  WRK-SEIKYUKBN-KOHI-AREA.
               05  WRK-SEIKYU-TBL   OCCURS  2.
                   07  WRK-SEIKYU-OCC    OCCURS  4.
                       09  WRK-SEIKYU-KOHID      PIC  9(10).
                       09  WRK-SEIKYU-KOHHKNNUM  PIC  X(03).
                       09  WRK-SEIKYU-KOHPAYKBN  PIC  X(02).
                       09  WRK-SEIKYU-RECESKYKBN PIC  X(01).
                       09  WRK-SEIKYU-TEKSTYMD   PIC  X(08).
      *
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
      *********05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
      *
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-OCC         OCCURS  3.
                   07  WRK-SRT-KOHHKNNUM
                                       PIC X(03)   OCCURS  28.
      *
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   28.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           02  WRK-HKNCOMBI-ERR        OCCURS  10. 
               03  WRK-HKNCOMBI-SYOKBN PIC X(01).   
               03  WRK-HKNCOMBI-ERR-DAY
                                       PIC 9(02).   
      *
       01  WRK-CONS-AREA.
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975". 
      *
           03  WRK-CONS-SRYYM          PIC 9(06)   VALUE   200604.      
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
      *
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.      
      *     
      * 
       01  WRK-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-RECE61//.
      *     
       01  WRK-SEIHO-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-SEIHO-RECE61//.
      *     
      * 
       01  WRK-KOHSKY-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-KOHSKY-RECE61//.
      *
      *    社保（継続）のとき 
       01  WRK-CONTKBN-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WRK-CONTKBN//.
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *     
           COPY    "CPSK2005.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           MOVE    "RECE610"           TO  RECE61PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE61PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE61PARA-HOSPNUM
      *
           MOVE    "RECE61X"           TO  RECE61XPARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE61XPARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE61XPARA-HOSPNUM
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0610"          TO  JOB-PGID
               MOVE    "対象患者抽出"      TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               IF   WRK-PARA-SRYYM    NOT =  ZERO
      *            医療機関ＩＤ編集
                   INITIALIZE                  SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO 
                                               SYS-1001-STYUKYMD (1:6)
                   MOVE    "01"                TO
                                               SYS-1001-STYUKYMD (7:2)
                   MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
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
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                                 OR    (SYSKAN-IDX   >=  100)
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
                       END-PERFORM
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
      *
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
               MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
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
               END-IF
      * 
      *        県内扱い保険者情報
               MOVE    SPACE           TO  WRK-1033-TABLE
      *     
               MOVE    SPACE           TO  SYS-1033-REC
               INITIALIZE                  SYS-1033-REC
               MOVE    "1033"          TO  SYS-1033-KANRICD
               MOVE    "*"             TO  SYS-1033-KBNCD
               MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
               MOVE    SYS-1033-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    ZERO                TO  IDX
               PERFORM             UNTIL   FLG-SYSKANRI   =   1
                                   OR      IDX            >=  5
                   MOVE    MCPDATA-REC         TO  SYS-1033-REC
                   ADD     1                   TO  IDX
                   MOVE    SYS-1033-STYUKYMD   TO  
                                               WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD   TO
                                               WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL        TO
                                               WRK-1033-HKNJANUM-G (IDX)
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *     
      *        レセプト編集区分情報
               INITIALIZE                  SYS-2005-REC
               MOVE    "2005"              TO  SYS-2005-KANRICD
               MOVE    "*"                 TO  SYS-2005-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-2005-STYUKYMD  (7:2)
               MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
               MOVE    SYS-2005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2005-REC
               ELSE    
                   MOVE    SPACE               TO  SYS-2005-REC
               END-IF
               IF      SYS-2005-RJNGNBKKBN =   SPACE
                   MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
               END-IF
      *
             IF        WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        パラメタＤＢ読み込み
               INITIALIZE                  BTPARA-REC
               MOVE    WRK-PARA-SHELLID
                                       TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID  TO  BTPARA-JOBID
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA      NOT =   ZERO
                   MOVE    "パラメタ情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
           OPEN    OUTPUT              RECE611-FILE
           CLOSE                       RECE611-FILE
      *
           OPEN    I-O                 RECE61-FILE  
      *
           OPEN    OUTPUT              RECE611X-FILE
           CLOSE                       RECE611X-FILE
      *
           OPEN    I-O                 RECE61X-FILE  
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
           IF      FLG-END             =   ZERO 
      *        診療会計読み込み（患者ＩＤ、保険組合せ、診療科順）
      *
               MOVE    "1"                 TO  WRK-DBKBN 
      *      
               INITIALIZE                  SRYACCT-REC
               MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
               MOVE    "1"                 TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM UNTIL       FLG-SRYACCT     =   1
                       OR          FLG-END         =   1
                   PERFORM 20011-IKKATU-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF 
      *
           IF      FLG-END             =   ZERO 
      *        入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
      *
               MOVE    "2"                 TO  WRK-DBKBN 
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *     
               INITIALIZE                  NYUINACCT-REC
               MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT   =   1
                       OR          FLG-END         =   1
                   PERFORM 20012-IKKATU-HENSYU-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計一括処理（患者ＩＤ、保険組合せ、診療科順）
      *****************************************************************
       20011-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者又は未請求設定時は読み飛ばす
           MOVE    SYS-1001-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SYS-2001-SRYYM          TO  WRK-SYUKA-SRYYM
           MOVE    KEY-N-PTID              TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   ( PTINF-TSTPTNUMKBN   =   "1"  )
           OR   ( SYUKA-MISEIKYU-FLG  =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *
      *            患者年齢計算
                   PERFORM 500-AGECHK-SEC
      *
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM 
                           MOVE    "1"             TO  WRK-ROUSAIKBN
                       ELSE     
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                           ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       END-IF    
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN    
                       MOVE    1               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO  WRK-ERR-HKNCOMBINUM
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       PERFORM 20011-FILE-SYORI-SEC 
      *
                       IF      WRK-UPDKBN          =   SPACE                   
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      FLG-END     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    "key36"         TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key36"             TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
           END-IF    
           .
       20011-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    診療会計読込処理
      *****************************************************************
       20011-FILE-SYORI-SEC              SECTION.
      *
      *    労災保険・自賠責のとき処理しない
           IF      WRK-ROUSAIKBN       =   "1"
               GO  TO  20011-FILE-SYORI-EXT
           END-IF
      *                  
           IF      WRK-UPDKBN          =   "1"
      *    保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    2               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    3               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    4               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    5               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD    
                   END-IF    
               END-IF    
           END-IF
      *
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    8                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
           AND   ( WRK-NENREI          >=  65                    )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    9                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE     9                  TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    最終診療日
           MOVE    ACCT-SRYYM          TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      ACCT-DAY (1 IDX)    >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPNUM    TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
                                       FLG-KOHRECEDSP
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *    地方公費のときレセプト請求区分を取得して判別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      COMB-KOHHKNNUM (IDX)    
                                           =   SPACE
      *
      *      平成２０年４月以降の老人保険は対象外とする
             IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
             AND   ( COMB-KOHHKNNUM (IDX)  =  "027"                )
                 CONTINUE
             ELSE        
               IF     (COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8")
               OR     (COMB-KOHHKNNUM (IDX)(1:2)   =  "99"    )
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SYS-1001-HOSPNUM        TO  HKN-HOSPNUM
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                   MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                       HKN-TEKEDYMD
                   MOVE    "00"                    TO  HKN-PAYKBN   
                   MOVE    HKNNUM-REC              TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( SYS-1001-PREFNUM    =   "40"              )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )    
                   AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                              >=  "1" AND <=  "8"   )
                   AND   ( WRK-SRYYMD(1:6)    <
                                              WRK-CONS-SRYYM-200804  )
                       IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
                       AND   ( COMB-KOHHKNNUM (IDX)
                                               NOT =   "180"   )
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                                                           OR  "5"
                               MOVE    2               TO  IDX1
                           ELSE
                               MOVE    1               TO  IDX1
                           END-IF
                       ELSE
                           PERFORM 20011-PREFKBN-40-CHECK-SEC
                       END-IF
                   ELSE
      *                福岡県の社保のとき（４１老人）
                       IF    ( COMB-HKNNUM     NOT =   "060" AND "067"
                                                             AND "039" )
                       AND   ( SYS-1001-PREFNUM    =   "40"            )
                       AND   ( HKN-HBTNUM          =   "41"            )    
                           IF      COMB-HONKZKKBN      =   "1" 
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE
                           EVALUATE    COMB-HKNNUM
                               WHEN    "060"
                               WHEN    "067" 
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    "039"                
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "4"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    OTHER                
                                   IF      HKN-RECESKYKBN
                                                       =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
                   IF     IDX1          =        1
      *                地方公費レセ記載判定
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF     LNK-FTNJGNCHK-RC   =   1
                           MOVE       1        TO    FLG-KOHRECEDSP
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                      TO  IDX1
      **>          IF   COMB-KOHHKNNUM (IDX)  =  "972" OR "974" OR "012"
      **>                                              OR "021"
                     IF    ( WRK-ERR-ERRKBN          >   ZERO  )
      **>            AND   ( COMB-KOHHKNNUM (IDX)    =   "012"
      **>                                            OR  "021" )
                     AND   ( COMB-KOHHKNNUM (IDX)(1:1)  =  "0" )
                     AND   ( COMB-HKNNUM             =   SPACE )
                         CONTINUE
                     ELSE 
      *                長期高額療養レセ記載判定（生保単独の不明分はしない）
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF     LNK-FTNJGNCHK-RC   =   1
                           MOVE       1        TO    FLG-KOHRECEDSP
                       END-IF
                     END-IF
      **>          END-IF
               END-IF       
      *
               IF     IDX1                =   1
                   IF      FLG-KOHRECEDSP  NOT =  1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                       MOVE    COMB-TEKSTYMD          TO
                                          WRK-SEIKYU-TEKSTYMD (IDX1 IDY)
      *                福岡県の国保のとき（４１老人以外）
                       IF    ( COMB-HKNNUM     =   "060"   OR  "067" )
                       AND   ( SYS-1001-PREFNUM
                                               =   "40"              )
                       AND   ( HKN-HBTNUM  NOT =   "41"              )    
                       AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                               >=  "1" AND <=  "8"   )
                           ADD     1                      TO  IDZ    
                           MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(2 IDZ)
                           MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (2 IDZ)
                           MOVE    1                      TO  FLG-KOH 
                       END-IF
                   END-IF    
               ELSE
                   ADD     1                      TO  IDZ    
                   MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                      WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                   MOVE    COMB-KOHID     (IDX)   TO
                                      WRK-SEIKYU-KOHID    (IDX1 IDZ)
                   MOVE    1                      TO  FLG-KOH 
               END-IF  
             END-IF  
          END-PERFORM
      *
           INITIALIZE                      WRK-KOHSKY-RECE61-REC
      * 
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
               MOVE    WRK-RECE61-REC      TO  WRK-KOHSKY-RECE61-REC
           END-IF   
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
           PERFORM 20011-FILE-HENSYU-SEC
           .
       20011-FILE-SYORI-EXT.
           EXIT. 
      *            
      *            
      *****************************************************************
      *    入院会計一括編集処理
      *****************************************************************
       20012-IKKATU-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SYS-1001-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SYS-2001-SRYYM          TO  WRK-SYUKA-SRYYM
           MOVE    KEY-N-PTID              TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "NYUINACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   ( PTINF-TSTPTNUMKBN   =   "1"  )
           OR   ( SYUKA-MISEIKYU-FLG  =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *        患者年齢計算
               PERFORM 500-AGECHK-SEC
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-NEW     NOT =   KEY-OLD
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
               IF      FLG-END                 =   ZERO 
      *            入院会計より保険組合せ読込     
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF      WRK-DAY (1 IDX)     >   ZERO    AND
                               WRK-DAY (2 IDX)     >   ZERO
                           PERFORM 200121-NYUINACCT-CHK-SEC
                       END-IF
                   END-PERFORM        
      *
      *            入院会計の保険組合せを追加
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       10
                           OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                           CONTINUE
                       ELSE    
                           MOVE    SPACE               TO  WRK-ERR-AREA
                           INITIALIZE                      WRK-ERR-AREA
      *
                           MOVE    WRK-HKNCOMBI-REC (IDX2)
                                                   TO  HKNCOMBI-REC    
                           IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                               MOVE    "1"         TO  WRK-TEISEIKBN
                               IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                       =   ZERO
                                   MOVE    1       TO  WRK-ERR-ERRKBN
                               ELSE
                                   MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                                   MOVE    6       TO  WRK-ERR-ERRKBN
                               END-IF
                               MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                                   WRK-ERR-HKNCOMBINUM
                           END-IF        
      *
                           PERFORM 200122-NYUINACCT-HENSYU-SEC
                       END-IF 
                   END-PERFORM
               END-IF
           END-IF    
           .
       20012-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       200121-NYUINACCT-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPNUM TO  COMB-HOSPNUM
                   MOVE    KEY-O-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
      *                医保以外読み飛ばし
                       IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM OR
                                           WRK-CONS-JIBAI-HKNNUM  OR
                                           WRK-CONS-KOUGAI-HKNNUM 
                           MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                       END-IF
                   ELSE
                       IF      COMB-DELKBN         =   "1"
                           MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
      *                    医保以外読み飛ばし
                           IF      COMB-HKNNUM     =
                                               WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                           ELSE
                               MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       ELSE
                           MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                           MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE                    
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE   
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           MOVE    IDX         TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF            
           END-PERFORM
           .
       200121-NYUINACCT-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計ファイル処理
      *****************************************************************
       200122-NYUINACCT-HENSYU-SEC           SECTION.
      *
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    8                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
           AND   ( WRK-NENREI          >=  65                    )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE    9                   TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-ERR-ERRKBN      =   ZERO                  )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
               MOVE     9                  TO  WRK-ERR-ERRKBN
           END-IF
      *
      *    最終診療日
           MOVE    NACCT-SRYYM         TO  WRK-SRYYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       31
               IF      NACCT-DAY (IDX)     >   ZERO
                   MOVE    IDX                 TO  WRK-SRYDD
               END-IF
           END-PERFORM
      *
      *   主保険情報読み出し
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYS-1001-HOSPNUM    TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               PERFORM 910-PTHKNINF-INV-SEC
           ELSE
               INITIALIZE                      PTHKNINF-REC
           END-IF
      *
      *    公費のレセプト・請求書への出力
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOH
                                       FLG-KOHRECEDSP
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *    地方公費のときレセプト請求区分を取得して判別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      COMB-KOHHKNNUM (IDX)    
                                           =   SPACE
      *
      *      平成２０年４月以降の老人保険は対象外とする
             IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
             AND   ( COMB-KOHHKNNUM (IDX)  =  "027"                )
                 CONTINUE
             ELSE        
               IF     (COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8")
               OR     (COMB-KOHHKNNUM (IDX)(1:2)   =  "99"    )
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SYS-1001-HOSPNUM        TO  HKN-HOSPNUM
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                   MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                       HKN-TEKEDYMD
                   MOVE    "00"                    TO  HKN-PAYKBN   
                   MOVE    HKNNUM-REC              TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( SYS-1001-PREFNUM    =   "40"              )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )    
                   AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                              >=  "1" AND <=  "8"    )
                       IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
                       AND   ( COMB-KOHHKNNUM (IDX)
                                               NOT =   "180"   )
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                                                           OR  "5"
                               MOVE    2               TO  IDX1
                           ELSE
                               MOVE    1               TO  IDX1
                           END-IF
                       ELSE
                           PERFORM 20011-PREFKBN-40-CHECK-SEC
                       END-IF
                   ELSE
      *                福岡県の社保のとき（４１老人）
                       IF    ( COMB-HKNNUM     NOT =   "060" AND "067"
                                                             AND "039" )
                       AND   ( SYS-1001-PREFNUM    =   "40"            )
                       AND   ( HKN-HBTNUM          =   "41"            )    
                           IF      COMB-HONKZKKBN      =   "1" 
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE
                           EVALUATE    COMB-HKNNUM
                               WHEN    "060"
                               WHEN    "067" 
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    "039"                
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "4"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    OTHER                
                                   IF      HKN-RECESKYKBN
                                                       =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
                   IF     IDX1          =        1
      *                地方公費レセ記載判定
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF     LNK-FTNJGNCHK-RC   =   1
                           MOVE       1        TO    FLG-KOHRECEDSP
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                      TO  IDX1
      **>          IF   COMB-KOHHKNNUM (IDX)  =  "972" OR "974" OR "012"
      **>                                              OR "021"
                     IF    ( WRK-ERR-ERRKBN          >   ZERO  )
      **>            AND   ( COMB-KOHHKNNUM (IDX)    =   "012"
      **>                                            OR  "021" )
                     AND   ( COMB-KOHHKNNUM (IDX)(1:1)  =  "0" )
                     AND   ( COMB-HKNNUM             =   SPACE )
                         CONTINUE
                     ELSE 
      *                長期高額療養レセ記載判定（生保単独の不明分はしない）
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    "1"         TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYMD(1:6) TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                               TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                               TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                               TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID      TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
                                                   SPA-AREA
                       IF     LNK-FTNJGNCHK-RC   =   1
                           MOVE       1        TO    FLG-KOHRECEDSP
                       END-IF
                     END-IF
      **>          END-IF
               END-IF       
      *
               IF     IDX1                =   1
                   IF      FLG-KOHRECEDSP  NOT =  1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                       MOVE    COMB-TEKSTYMD          TO
                                          WRK-SEIKYU-TEKSTYMD (IDX1 IDY)
      *                福岡県の国保のとき（４１老人以外）
                       IF    ( COMB-HKNNUM     =   "060"   OR  "067" )
                       AND   ( SYS-1001-PREFNUM
                                               =   "40"              )
                       AND   ( HKN-HBTNUM  NOT =   "41"              )    
                       AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                               >=  "1" AND <=  "8"   )
                           ADD     1                      TO  IDZ    
                           MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(2 IDZ)
                           MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (2 IDZ)
                           MOVE    1                      TO  FLG-KOH 
                       END-IF
                   END-IF    
               ELSE
                   ADD     1                      TO  IDZ    
                   MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDZ)
                   MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDZ)
                   MOVE    1                      TO  FLG-KOH 
               END-IF  
             END-IF  
           END-PERFORM
      *
           INITIALIZE                      WRK-KOHSKY-RECE61-REC
      * 
      *    医療費請求書
           IF      FLG-KOH             =   1  
               MOVE    "2"             TO  WRK-SYOKBN
               PERFORM 20011-FILE-HENSYU-SEC
               MOVE    WRK-RECE61-REC      TO  WRK-KOHSKY-RECE61-REC
           END-IF   
      *
      *    レセプト明細書
           MOVE    "1"                 TO  WRK-SYOKBN
           PERFORM 20011-FILE-HENSYU-SEC
           .
       200122-NYUINACCT-HENSYU-EXT.
           EXIT. 
           .
      *            
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    ２００６年１０月以降は公費付加情報より設定
           IF    ( WRK-SRYYMD(1:6)    >=  "200610" )
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
      *      県外扱いの保険者はレセプトへ記載しない（２００６年９月まで）
             IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
             ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
             END-IF
             IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
             ELSE
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM      TO  WRK-SYMD (1:6)
               ELSE
                   MOVE    SYS-2002-SRYYM      TO  WRK-SYMD (1:6)
               END-IF
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD      
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"     
                           IF      PTHKN-HKNJANUM = 
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF            
               END-PERFORM
             END-IF
           END-IF
      *        
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN      =   SPACE
               MOVE    2                   TO  IDX1
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               IF      WRK-PARA-SYOKBN     =   "1"
                   MOVE    SYS-2001-SRYYM  TO  LNK-SKOHPLUS-SRYYMD (1:6)
               ELSE
                   MOVE    SYS-2002-SRYYM  TO  LNK-SKOHPLUS-SRYYMD (1:6)
               END-IF
               MOVE    "01"             TO  LNK-SKOHPLUS-SRYYMD (7:2)  
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
               IF      LNK-SKOHPLUS-RC =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    2                   TO  IDX1
                   ELSE
                       MOVE    1                   TO  IDX1
                   END-IF
               ELSE
                   MOVE    1                   TO  IDX1
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    ファイル編集処理
      *****************************************************************
       20011-FILE-HENSYU-SEC              SECTION.
      *
      *    公費で不明分は出力しない
           IF    ( WRK-SYOKBN         =   "2" )
           AND   ( WRK-TEISEIKBN      =   1   )
               INITIALIZE                      WRK-RECE61-REC
               GO  TO  20011-FILE-HENSYU-EXT
           END-IF
      *     
           INITIALIZE                      WRK-RECE61-REC
           MOVE    SYS-1001-HOSPNUM    TO  WRK-RECE61-HOSPNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    SYS-2001-SRYYM      TO  WRK-RECE61-SRYYM
           ELSE
               MOVE    SYS-2002-SRYYM      TO  WRK-RECE61-SRYYM
           END-IF
           MOVE    "1"                 TO  WRK-RECE61-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE61-PTID
           MOVE    COMB-HKNNUM         TO  WRK-RECE61-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  WRK-RECE61-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOHID
                   MOVE    "0"                 TO  WRK-RECE61-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  WRK-RECE61-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
      *****                IF     (WRK-NENREI  >=   70)  AND
      *****                       (WRK-NENREI  <=   74)
                               EVALUATE   PTHKN-HOJOKBN
                                   WHEN   "1"
                                   WHEN   "5"
                                       MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "2"
                                   WHEN   "6"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "0"
                                   WHEN   "4"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                   WHEN   "3"
                                       MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                              END-EVALUATE
      *****               END-IF     
                      WHEN    "040" 
                      WHEN    "039" 
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                          END-EVALUATE             
                      WHEN    OTHER 
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF 
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKNJANUM
      *
           MOVE    ZERO                TO  WRK-RECE61-TEKSTYMD 
           IF      COMB-HKNNUM         =   SPACE
               IF      COMB-KOH1HKNNUM     =   "020"   OR  "030"
                   MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
               END-IF
           END-IF
      *****MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      WRK-STYMD (1:6)     <=   "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
      *****        IF     (WRK-NENREI  >=   70)  AND
      *****               (WRK-NENREI  <=   74)
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "0"
                           WHEN   "4"
                               MOVE    "0"             TO
                                               WRK-RECE61-HOJOKBN
                           WHEN   "1"
                           WHEN   "5"
                               MOVE    "1"             TO
                                               WRK-RECE61-HOJOKBN
                           WHEN   "2"
                           WHEN   "6"
                               MOVE    "2"             TO
                                               WRK-RECE61-HOJOKBN
                       END-EVALUATE
      *****        END-IF     
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( WRK-STYMD (1:6)
                                           <   WRK-CONS-SRYYM-200804 )
                       OR    ( WRK-NENREI  <=  74                    )  
                           MOVE    PTHKN-HOJOKBN   TO
                                                  WRK-RECE61-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "0"
                       WHEN   "4"
                           MOVE    "0"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "1"
                       WHEN   "5"
                           MOVE    "1"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "2"
                       WHEN   "6"
                           MOVE    "2"             TO
                                               WRK-RECE61-HOJOKBN
                       WHEN   "3"
                           MOVE    "3"             TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE
               WHEN    "040" 
               WHEN    "039" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           END-IF               
           MOVE    SPACE               TO WRK-RECE61-RECEKA
      *
      *    レセのとき地方公費のみの場合は出力しない
           IF      WRK-SYOKBN             =   "1"        
               IF    ( WRK-RECE61-HKNNUM      =   SPACE )
               AND   ( WRK-RECE61-KOHID       =   ZERO  )
               AND   ( WRK-SEIKYU-KOHID (1 1) =   ZERO  )
                   GO  TO  20011-FILE-HENSYU-EXT
               END-IF
           END-IF 
      *   
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
                   PERFORM 900-RECE61-READ-SEC
                   IF      FLG-RECE61          =   ZERO
                       MOVE    RECE61-REC          TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
               WHEN    "2"
                   MOVE    WRK-RECE61-KEY      TO  RECE61X-KEY 
                   PERFORM 900-RECE61X-READ-SEC
                   IF      FLG-RECE61X         =   ZERO
                       MOVE    RECE61X-REC     TO  WRK-RECE61-REC 
                       PERFORM 20011-RECE61-REWRITE-SEC
                   ELSE
                       PERFORM 20011-RECE61-WRITE-SEC
                   END-IF
           END-EVALUATE
      *
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF    ( WRK-SYOKBN          =   "1"   )
           AND   ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOHHKNNUM (1)  =   "012"
                                       OR  "015"
                                       OR  "016"
                                       OR  "021"
                                       OR  "025" )
           AND   ( COMB-KOHHKNNUM (2)  =   SPACE )
               PERFORM 20013-SEIHO-RECE61-WRITE-SEC
           END-IF
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE61-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           MOVE    COMB-HKNCOMBINUM   TO  WRK-RECE61-HKNCOMBI-O (1)
           MOVE    COMB-HKNNUM        TO  WRK-RECE61-HKNNUMHC   (1)
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
               MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                           WRK-RECE61-KOHNUMHC (1 IDX1)
               MOVE    COMB-KOHID     (IDX1)   TO
                                           WRK-RECE61-KOHIDHC  (1 IDX1)
           END-PERFORM
      *
           IF      WRK-DBKBN          =   "2"
               CONTINUE
           ELSE              
               MOVE    ACCT-SRYKA     TO  WRK-RECE61-SRYKA      (1)
           END-IF
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF      WRK-DAY (1 IDX3)     >   ZERO    AND
                         WRK-DAY (2 IDX3)     >   ZERO
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
           MOVE    PTHKN-TEKSTYMD     TO  WRK-RECE61-TEKSTYMDX  (1)
           MOVE    PTHKN-TEKEDYMD     TO  WRK-RECE61-TEKEDYMDX  (1)
           IF    ( COMB-HKNNUM        =   SPACE )
           AND   ( COMB-KOH1HKNNUM    =   "020"
                                      OR  "030" )
               MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMDX  (1)
               MOVE    COMB-TEKEDYMD       TO  WRK-RECE61-TEKEDYMDX  (1)
           END-IF
      *    
      *    主保険情報
           MOVE    PTHKN-HKNID        TO  WRK-RECE61-HKNID
           MOVE    PTHKN-CONTKBN      TO  WRK-RECE61-CONTKBN
           MOVE    PTHKN-HKNJANUM     TO  WRK-RECE61-HKN-HKNJANUM
           MOVE    PTHKN-HONKZKKBN    TO  WRK-RECE61-HKN-HONKZKKBN
           MOVE    PTHKN-HOJOKBN      TO  WRK-RECE61-HKN-HOJOKBN
           IF      PTHKN-HKNNUM       =   "002"
               IF    ( WRK-STYMD (1:6)    <   WRK-CONS-SRYYM-200804 )
               OR    ( WRK-NENREI         <=  74                    )  
                   CONTINUE
               ELSE
                   EVALUATE    PTHKN-HOJOKBN
                       WHEN    "A"
                       WHEN    "G"
                           MOVE    "1" TO  WRK-RECE61-HKN-HOJOKBN
                       WHEN    "B"
                       WHEN    "H"
                           MOVE    "2" TO  WRK-RECE61-HKN-HOJOKBN
                       WHEN    "C"
                       WHEN    "I"
                           MOVE    "3" TO  WRK-RECE61-HKN-HOJOKBN
                   END-EVALUATE
               END-IF
           END-IF
           MOVE    PTHKN-KIGO         TO  WRK-RECE61-KIGO
           MOVE    PTHKN-NUM          TO  WRK-RECE61-NUM
           MOVE    SPACE              TO  WRK-RECE61-CERTNUM
      *
           MOVE    WRK-TEISEIKBN      TO  WRK-RECE61-TEISEIKBN
           IF      WRK-TEISEIKBN      =   "1"
               MOVE    WRK-ERR-ERRKBN     TO  WRK-RECE61-ERR-ERRKBN
               MOVE    WRK-ERR-DAY        TO  WRK-RECE61-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                          TO  WRK-RECE61-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD   TO  WRK-RECE61-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD   TO  WRK-RECE61-ERR-TEKEDYMD
           END-IF
      *
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    WRK-RECE61-HOSPNUM  TO  SPTID-HOSPNUM
           MOVE    WRK-RECE61-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-RECE61-PTNUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME       TO  WRK-RECE61-KANANAME
           MOVE   PTINF-NAME           TO  WRK-RECE61-NAME
           MOVE   PTINF-SEX            TO  WRK-RECE61-SEX
           MOVE   PTINF-BIRTHDAY       TO  WRK-RECE61-BIRTHDAY
           MOVE   PTINF-DEATHKBN       TO  WRK-RECE61-DEATHKBN
           MOVE   PTINF-TSTPTNUMKBN    TO  WRK-RECE61-TSTPTNUMKBN
      *    患者年齢
           MOVE   WRK-NENREI           TO  WRK-RECE61-AGE
           MOVE    AGECHK-SYUGAKUKBN   TO  WRK-RECE61-SYUGAKUKBN
      *    旧姓履歴の取得
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    WRK-RECE61-HOSPNUM      TO  KYUSEI-HOSPNUM
           MOVE    WRK-RECE61-PTID         TO  KYUSEI-PTID
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-RECE61-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK      =   ZERO
               MOVE    KYUSEI-NAME         TO  WRK-RECE61-NAME
               MOVE    KYUSEI-KANANAME     TO  WRK-RECE61-KANANAME
           END-IF
      *    県番号
           MOVE    SYS-1001-PREFNUM    TO  WRK-RECE61-PREFNUM
      *    印刷順
           IF    ( WRK-PARA-SYOKBN     =   "2" )
           AND   ( WRK-SYOKBN          =   "1" )
           AND   ( BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                   OR  "05" )
               ADD     1                       TO  WRK-PRTSEQ
               MOVE    WRK-PRTSEQ              TO  WRK-RECE61-PRTSEQ
           END-IF 
      *
      *レセプト請求しない地方公費情報
           IF    ( WRK-SYOKBN          =   "1"  )
           AND   ( FLG-KOH             =   1    )
           AND   ( WRK-KOHSKY-RECE61-KOHID-O (1)
                                       >   ZERO )
               INITIALIZE              WRK-RECE61-KOHSKY-KOHI-AREA
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       4
                   MOVE    WRK-KOHSKY-RECE61-KOHID-O     (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHID-O
                                                               (IDX1)
                   MOVE    WRK-KOHSKY-RECE61-KOHHKNNUM-O (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHHKNNUM-O
                                                               (IDX1)
               END-PERFORM
           END-IF    
      *
      *未請求フラグ
           IF      WRK-SYOKBN          =   "1"
               MOVE    SYUKA-MISEIKYU-FLG  TO  WRK-RECE61-MISEIKYU-FLG
           END-IF
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   WRITE   RECE61-REC
                   ADD     1                   TO  CNT-OUT1
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   WRITE   RECE61X-REC
                   ADD     1                   TO  CNT-OUT2
           END-EVALUATE
      *
           .
       20011-RECE61-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       20011-RECE61-REWRITE-SEC            SECTION.
      *
           PERFORM 20012-KOHID-HENSYU-SEC
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      WRK-RECE61-HKNCOMBI-O  (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                   MOVE    COMB-HKNNUM         TO
                                       WRK-RECE61-HKNNUMHC   (IDX)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       4
                       MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                       WRK-RECE61-KOHNUMHC (IDX IDX1)
                       MOVE    COMB-KOHID     (IDX1)   TO
                                       WRK-RECE61-KOHIDHC  (IDX IDX1)
                   END-PERFORM
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-DBKBN               =   "2"
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                   IF      WRK-RECE61-SRYKA    (IDX)       =   SPACE
                       MOVE    ACCT-SRYKA  TO  WRK-RECE61-SRYKA (IDX)
                       MOVE    20          TO  IDX
                   ELSE
                       IF      WRK-RECE61-SRYKA  (IDX) =   ACCT-SRYKA
                           MOVE    20          TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           MOVE    1                       TO  FLG-LASTHKNID
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF    ( WRK-RECE61-TEKSTYMDX (IDX)
                                       =   PTHKN-TEKSTYMD )
               AND   ( WRK-RECE61-TEKEDYMDX (IDX)
                                       =   PTHKN-TEKEDYMD )
                   MOVE    10              TO  IDX
               ELSE
                   IF      WRK-RECE61-TEKSTYMDX (IDX)  =   SPACE
                       MOVE    PTHKN-TEKSTYMD  TO
                                           WRK-RECE61-TEKSTYMDX (IDX)
                       MOVE    PTHKN-TEKEDYMD  TO
                                           WRK-RECE61-TEKEDYMDX (IDX)
                       MOVE    10              TO  IDX
                   ELSE
                       IF      WRK-RECE61-TEKSTYMDX(IDX)
                                                   >=  PTHKN-TEKSTYMD
                           MOVE    ZERO                TO  FLG-LASTHKNID
                       END-IF
                   END-IF
               END-IF        
           END-PERFORM
           IF      FLG-LASTHKNID       =   1
               MOVE    PTHKN-HKNID         TO  WRK-RECE61-HKNID
               MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKN-HKNJANUM
               MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HKN-HONKZKKBN
               MOVE    PTHKN-HOJOKBN       TO  WRK-RECE61-HKN-HOJOKBN
               IF      PTHKN-HKNNUM        =   "002"
                   IF    ( WRK-STYMD (1:6) <   WRK-CONS-SRYYM-200804 )
                   OR    ( WRK-NENREI      <=  74                    )  
                       CONTINUE
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                               MOVE    "1" TO  WRK-RECE61-HKN-HOJOKBN
                           WHEN    "B"
                           WHEN    "H"
                               MOVE    "2" TO  WRK-RECE61-HKN-HOJOKBN
                           WHEN    "C"
                           WHEN    "I"
                               MOVE    "3" TO  WRK-RECE61-HKN-HOJOKBN
                       END-EVALUATE
                   END-IF
               END-IF
               MOVE    PTHKN-CONTKBN       TO  WRK-RECE61-CONTKBN
               MOVE    PTHKN-KIGO          TO  WRK-RECE61-KIGO
               MOVE    PTHKN-NUM           TO  WRK-RECE61-NUM
               MOVE    SPACE               TO  WRK-RECE61-CERTNUM
           END-IF
      *
           IF      WRK-DBKBN          =   "2"
      ***      MOVE    WRK-DAY-TBL (2)    TO  WRK-RECE61-DAY-AREA
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >      31
                 IF      WRK-DAY (1 IDX3)     >   ZERO    AND
                         WRK-DAY (2 IDX3)     >   ZERO
                    MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE61-DAY(IDX3)
                 END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
               IF      WRK-RECE61-TEISEIKBN    =   SPACE
                   MOVE    WRK-ERR-ERRKBN  TO  WRK-RECE61-ERR-ERRKBN
                   MOVE    WRK-ERR-DAY     TO  WRK-RECE61-ERR-DAY
                   MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE61-ERR-HKNCOMBINUM
                   MOVE    WRK-ERR-TEKSTYMD
                                           TO  WRK-RECE61-ERR-TEKSTYMD
                   MOVE    WRK-ERR-TEKEDYMD
                                           TO  WRK-RECE61-ERR-TEKEDYMD
               END-IF            
               MOVE    "1"                 TO  WRK-RECE61-TEISEIKBN     
           END-IF    
      *
      *レセプト請求しない地方公費情報
           IF    ( WRK-SYOKBN          =   "1"  )
           AND   ( FLG-KOH             =   1    )
           AND   ( WRK-KOHSKY-RECE61-KOHID-O (1)
                                       >   ZERO )
               INITIALIZE              WRK-RECE61-KOHSKY-KOHI-AREA
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       4
                   MOVE    WRK-KOHSKY-RECE61-KOHID-O     (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHID-O
                                                               (IDX1)
                   MOVE    WRK-KOHSKY-RECE61-KOHHKNNUM-O (IDX1)
                                       TO  WRK-RECE61-KOHSKY-KOHHKNNUM-O
                                                               (IDX1)
               END-PERFORM
           END-IF    
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-RECE61-REC      TO  RECE61-REC
                   REWRITE RECE61-REC
               WHEN    "2"
                   MOVE    WRK-RECE61-REC      TO  RECE61X-REC
                   REWRITE RECE61X-REC
           END-EVALUATE
           .
       20011-RECE61-REWRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理（生保単独のときのみ）
      *****************************************************************
       20013-SEIHO-RECE61-WRITE-SEC              SECTION.
      *
           MOVE    WRK-RECE61-REC      TO  WRK-SEIHO-RECE61-REC
      *
           INITIALIZE                      WRK-RECE61-REC
           MOVE    WRK-SEIHO-RECE61-KEY
                                       TO  WRK-RECE61-KEY
      *
      *****MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
           INITIALIZE                      PTKOHINF-REC
           MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
           MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
           MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMD
      *
           MOVE    WRK-RECE61-KEY      TO  RECE61-KEY 
           PERFORM 900-RECE61-READ-SEC
           IF      FLG-RECE61          =   ZERO
               MOVE    RECE61-REC          TO  WRK-RECE61-REC 
               PERFORM 20011-RECE61-REWRITE-SEC
           ELSE
      *********MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD 
      *********MOVE    COMB-TEKEDYMD       TO  PTHKN-TEKEDYMD
               MOVE    PTKOH-TEKSTYMD      TO  PTHKN-TEKSTYMD
               MOVE    PTKOH-TEKEDYMD      TO  PTHKN-TEKEDYMD
      *  
               PERFORM 20011-RECE61-WRITE-SEC
           END-IF
      *
           .
       20013-SEIHO-RECE61-WRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       20012-KOHID-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                    TO  TBL-MAX
                                               FLG-ADDKBN 
      *
           MOVE    WRK-RECE61-KOHI-AREA    TO  WRK-KOHI-AREA
      *
           MOVE    WRK-SYOKBN              TO  IDX1   
      *
      *    公費の追加 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    SPACE               TO  WRK-RECESKYKBN
      *
               IF      WRK-SEIKYU-KOHID (IDX1 IDX)    =   ZERO
                   MOVE    4                   TO  IDX
               ELSE    
      *            老人＋原爆のとき  
                   IF    ( WRK-RECE61-KOHID    NOT =   ZERO  )
                   AND   ( WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)  
                                                   =   "019" )
                       IF      WRK-RECE61-HKNNUM   =   "060"   OR  "067" 
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       END-IF
                   ELSE
                       MOVE    SPACE       TO  WRK-RECESKYKBN
                   END-IF
                   PERFORM 200121-KOHID-TBL-HENSYU-SEC
               END-IF      
           END-PERFORM
      *
      *    公費を追加しなかったとき
           IF      FLG-ADDKBN              =   ZERO
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    優先順位に並べ替え
           INITIALIZE                      WRK-RECE61-KOHI-AREA
           MOVE    ZERO                    TO  TBL-MAX
      *
      *
           IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM
               MOVE    1                   TO  IDXX
           ELSE
               IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM-200804
                   MOVE    2                   TO  IDXX
               ELSE
                   MOVE    3                   TO  IDXX
               END-IF
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-SRT-KOHHKNNUM-MAX
                   OR      WRK-SRT-KOHHKNNUM (IDXX IDX)
                                       =   SPACE
                   OR      TBL-MAX     =   4 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       4
                   IF      WRK-SRT-KOHHKNNUM   (IDXX IDX)
                                       =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-RECE61-KOHI-TBL (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL    (IDY)
                       MOVE    4               TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時 
           IF      WRK-KOHHKNNUM-O (1)     =   SPACE
           AND     WRK-KOHHKNNUM-O (2)     =   SPACE
           AND     WRK-KOHHKNNUM-O (3)     =   SPACE
           AND     WRK-KOHHKNNUM-O (4)     =   SPACE
               GO  TO  20012-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       3
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       4
                   IF     WRK-KOHHKNNUM-O (IDX) =   SPACE
                       MOVE    WRK-KOHI-TBL(IDX) TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL(IDY) TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI          TO  WRK-KOHI-TBL(IDY)
                   ELSE
      *                北海道の地方公費（１９１＋２９０）
                       IF    ( SYS-1001-PREFNUM    =   "01"  ) 
                       AND   ( WRK-KOHHKNNUM-O (IDX)
                                                   =   "290" )
                       AND   ( WRK-KOHHKNNUM-O (IDY)
                                                   =   "191" )
                           CONTINUE
                       ELSE
      *                    東京都の地方公費（法別８２）
                           IF    ( SYS-1001-PREFNUM    =   "13" ) 
                               IF   ( WRK-KOHHKNNUM-O (IDX) (2:2)
                                                           =   "82" )
                               AND  ( IDX                  =   1    )
                                   CONTINUE
                               ELSE   
                                   IF    ( WRK-KOHHKNNUM-O (IDY) (2:2)
                                                   =   "82"           )
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                   ELSE
                                     IF    ( WRK-KOHHKNNUM-O (IDX)
                                                   >   WRK-KOHHKNNUM-O
                                                                (IDY) )
                                       MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                       MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                       MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                     END-IF
                                   END-IF
                               END-IF    
                           ELSE
      *                        徳島県の地方公費（法別４５、４８）
                               IF    ( SYS-1001-PREFNUM    =   "36"    ) 
                               AND   ( WRK-KOHHKNNUM-O (IDX) (2:2)
                                                   =   "45"    OR "48" )
                               AND   ( WRK-KOHHKNNUM-O (IDY) (2:2)
                                                   =   "45"    OR "48" )
                                   IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   <   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                       CONTINUE
                                   ELSE
                                       IF      WRK-KOHTEKSTYMD-O (IDX)
                                                   >   WRK-KOHTEKSTYMD-O
                                                                (IDY)
                                           MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                           MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                           MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                       ELSE
                                           IF    ( WRK-KOHHKNNUM-O (IDX)
                                                   >   WRK-KOHHKNNUM-O
                                                                (IDY) )
                                               MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                               MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                               MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                           END-IF
                                       END-IF
                                   END-IF
                               ELSE
      *
                                 IF      WRK-KOHHKNNUM-O (IDX)
                                               >   WRK-KOHHKNNUM-O (IDY)
                                   MOVE    WRK-KOHI-TBL(IDX)
                                                   TO  WRK-KOHI
                                   MOVE    WRK-KOHI-TBL(IDY)
                                                   TO  WRK-KOHI-TBL(IDX)
                                   MOVE    WRK-KOHI
                                                   TO  WRK-KOHI-TBL(IDY)
                                 END-IF
                               END-IF
                           END-IF
                       END-IF    
                   END-IF    
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      TBL-MAX     =   4 
               IF      WRK-KOHID-O (IDX)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDX)  TO 
                                           WRK-RECE61-KOHI-TBL(TBL-MAX)
               END-IF    
           END-PERFORM
      *
      *    長崎の生保と被爆（０１２＋１８６）
           IF      SYS-1001-PREFNUM    =   "42"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "012"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    4                   TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "186"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    4                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    愛知の生保と特定疾患（０１２＋１９１）
           IF      SYS-1001-PREFNUM    =   "23"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "012"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    4                   TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "191"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    4                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    山梨の特定疾患と老人医療費（０５１、０９１＋１４１、２４１）
           IF      SYS-1001-PREFNUM    =   "19"
               MOVE    ZERO                TO  WRK-SEIHO-IDX
                                               WRK-HIBAKU-IDX                                                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      WRK-RECE61-KOHHKNNUM-O (IDX)
                                   =       SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "051"   OR  "091"
                       IF      WRK-HIBAKU-IDX      =   0 
                           MOVE    IDX                 TO  WRK-SEIHO-IDX
                       ELSE
                           MOVE    4                   TO  IDX
                       END-IF
                   END-IF
                   IF      WRK-RECE61-KOHHKNNUM-O (IDX)
                                               =   "141"   OR  "241"
                       IF      WRK-SEIHO-IDX       >   0
                           MOVE    WRK-RECE61-KOHI-TBL(IDX)
                                               TO  WRK-KOHI
                           MOVE    WRK-RECE61-KOHI-TBL (WRK-SEIHO-IDX)
                                               TO  WRK-RECE61-KOHI-TBL
                                                   (IDX)
                           MOVE    WRK-KOHI    TO  WRK-RECE61-KOHI-TBL
                                                   (WRK-SEIHO-IDX)
                       END-IF
                       MOVE    4                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       20012-KOHID-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費情報退避処理
      *****************************************************************
       200121-KOHID-TBL-HENSYU-SEC     SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               IF      WRK-KOHID-O (IDY)   =   ZERO
                   MOVE    WRK-SEIKYU-KOHID    (IDX1 IDX)
                                           TO  WRK-KOHID-O    (IDY)
                   MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                           TO  WRK-KOHHKNNUM-O(IDY)
                   MOVE    WRK-RECESKYKBN  TO  WRK-RECESKYKBN-O(IDY)
      *            徳島県の地方公費（法別４５、４８）のとき開始日セット
                   IF    ( SYS-1001-PREFNUM    =   "36"             )
                   AND   ( WRK-SEIKYU-KOHHKNNUM(IDX1 IDX) (2:2)
                                               =   "45"    OR  "48" )
                       MOVE    WRK-SEIKYU-TEKSTYMD (IDX1 IDX)
                                           TO  WRK-KOHTEKSTYMD-O (IDY)
                   END-IF 
                   MOVE    1               TO  FLG-ADDKBN
                   MOVE    4               TO  IDY
               ELSE 
                   IF      WRK-SEIKYU-KOHID (IDX1 IDX)
                                       =   WRK-KOHID-O  (IDY)
                       MOVE    4           TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           .
       200121-KOHID-TBL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
               PERFORM 20021-KOBETU-REAL-SEC
               MOVE    "1"   TO  PTINF-TSTPTNUMKBN
               GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYSKANRI    =   1
                              OR          FLG-END         =   1
      *   
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "1"              TO  SYS-2002-NYUGAIKBN
      *
           MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
           MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    該当診療年月の医療機関情報取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
           MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
      *    患者年齢計算
           PERFORM 500-AGECHK-SEC
      *
      *    診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *   
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                   OR          FLG-END         =   1 
               PERFORM 20011-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院会計読み込み
           MOVE    "2"                 TO  WRK-DBKBN 
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *     
           INITIALIZE                  NYUINACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  NACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACCT     =   1
                   OR          FLG-END           =   1
               PERFORM 20012-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *     
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
           MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
           MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    未請求設定時は読み飛ばす
           IF    SYUKA-MISEIKYU-FLG  =   "01"
               CONTINUE
           ELSE
      *
      *        医療機関ＩＤ編集
               MOVE  ZERO           TO  FLG-SET
               MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
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
                    END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                   STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                            SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO       TO      20021-KOBETU-HENSYU-EXT
               END-IF
      *
      *        患者情報取得
               MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
               MOVE    SYS-2002-PTID       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
      *        診療年月編集
               MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
               MOVE    "01"                TO  WRK-STYMD (7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *        患者年齢計算
               PERFORM 500-AGECHK-SEC
      *
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
               IF      FLG-END             =   ZERO
      *            診療会計読み込み
                   MOVE    "1"                 TO  WRK-DBKBN 
      *      
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  ACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key37"             TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key37"             TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
      *
                   PERFORM UNTIL       FLG-SRYACCT     =   1
                           OR          FLG-END         =   1 
                       PERFORM 20011-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key37"             TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF 
      *
               IF      FLG-END             =   ZERO
      *            入院会計読み込み
                   MOVE    "2"                 TO  WRK-DBKBN 
      *
                   MOVE    LOW-VALUE           TO  KEY-AREA
      *     
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  NACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                       MOVE    "key14"             TO  MCP-PATHNAME
                       PERFORM 900-NYUINACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-NYUINACCT
                   END-IF
      *
                   PERFORM UNTIL       FLG-NYUINACCT     =   1
                           OR          FLG-END           =   1
                       PERFORM 20012-HENSYU-SEC
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計個別編集処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM OR
                                               WRK-CONS-JIBAI-HKNNUM  OR
                                               WRK-CONS-KOUGAI-HKNNUM 
                       MOVE    "1"             TO  WRK-ROUSAIKBN
                   ELSE                                         
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   END-IF    
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN        
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM        
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1      
                               OR      KEY-NEW     NOT =   KEY-OLD
                   PERFORM 20011-FILE-SYORI-SEC 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1      
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key37"             TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key37"             TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       20011-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計個別編集処理
      *****************************************************************
       20012-HENSYU-SEC              SECTION.
      *     
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBI-TABLE
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      FLG-END         =   1      
                           OR      KEY-NEW     NOT =   KEY-OLD
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                               WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
      *        入院会計より保険組合せ読込     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      WRK-DAY (1 IDX)     >   ZERO    AND
                           WRK-DAY (2 IDX)     >   ZERO
                       PERFORM 200121-NYUINACCT-CHK-SEC
                   END-IF
               END-PERFORM        
      *
      *        入院会計の保険組合せを追加
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                       CONTINUE
                   ELSE    
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    WRK-HKNCOMBI-REC (IDX2)
                                                   TO  HKNCOMBI-REC    
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                           MOVE    "1"         TO  WRK-TEISEIKBN
                           IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                       =   ZERO
                               MOVE    1           TO  WRK-ERR-ERRKBN
                           ELSE
                               MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                               MOVE    6           TO  WRK-ERR-ERRKBN
                           END-IF
                           MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                               WRK-ERR-HKNCOMBINUM
                       END-IF        
                       PERFORM 200122-NYUINACCT-HENSYU-SEC
                   END-IF 
               END-PERFORM
           END-IF    
           .
       20012-HENSYU-EXT.
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
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM   900-CALL-ORCSJOB-SEC
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
                       PERFORM   900-CALL-ORCSJOB-SEC
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
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE61-FILE
           CLOSE   RECE61X-FILE
      *
           IF      CNT-OUT1            =   ZERO
           AND     CNT-OUT2            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           DISPLAY "RECE61  CNT " CNT-OUT1 
           DISPLAY "RECE61X CNT " CNT-OUT2  
           DISPLAY "*** ORCR0610 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者年齢計算処理
      *****************************************************************
       500-AGECHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    SYS-1001-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID          TO  AGECHK-PTID
           MOVE    WRK-STYMD (1:6)     TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-NENREI
           END-IF
      *
           .
       500-AGECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求フラグチェック処理
      *****************************************************************
       800-MISEIKYU-CHECK-SEC          SECTION.
      *
           INITIALIZE                      SYUKA-REC
           MOVE    SYS-1001-HOSPNUM    TO  SYUKA-HOSPNUM
           MOVE    WRK-SYUKA-PTID      TO  SYUKA-PTID
           MOVE    WRK-SYUKA-SRYYM     TO  SYUKA-SRYYM
           MOVE    "1"                 TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "tbl_syuka"         TO  WRK-MCP-TABLE
           MOVE    "key15"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           .
       800-MISEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
               MOVE    SPACE               TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-SYUKA
                   MOVE    MCPDATA-REC          TO  SYUKA-REC
               ELSE
                   MOVE    1                    TO  FLG-SYUKA
                   INITIALIZE                       SYUKA-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   INITIALIZE                      PTKOHINF-REC
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE                      PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF  
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE61-READ-SEC             SECTION.
      *
           READ    RECE61-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61
           END-READ
           .
       900-RECE61-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込（順読み）
      *****************************************************************
       910-RECE61-READ-N-SEC             SECTION.
      *
           READ    RECE61-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-RECE61
               NOT AT  END
                   MOVE    ZERO        TO  FLG-RECE61
           END-READ
           .
       910-RECE61-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療費請求書読込
      *****************************************************************
       900-RECE61X-READ-SEC            SECTION.
      *
           READ    RECE61X-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE61X
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE61X
           END-READ
           .
       900-RECE61X-READ-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
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
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
