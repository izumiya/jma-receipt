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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCR0488.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      自賠責保険・印刷データ編集（自賠責）
      *                      平成１９年４月改正様式（新様式）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/06/26    NACL-門脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *  04.02.02    NACL-藤原    08/04/18  「レセ電通信医２０００６号」
      *                                     症状詳記区分コード５２の追加
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.02    NACL-藤原    09/01/16  症状詳記区分の表題の変更
      *  04.03.03    NACL-門脇    09/02/04  請求点が０の場合の対応
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    09/06/29  療養期間記載方法選択機能
      *                                     （休業証明請求時）
      *  04.05.03    NACL-藤原    10/03/03  平成２２年４月改正対応
      *  04.05.04    NACL-藤原    10/03/29  「改正未対応」の記載をやめる
      *  04.05.05    NACL-藤原    11/08/02  点検レセ印字位置変更対応
      *  04.05.06    NACL-藤原    12/03/14  平成２４年４月改正対応
      *                                     「改正未対応」の記載を行う
      *  04.05.07    NACL-藤原    12/04/11  平成２４年４月改正対応
      *                                     「改正未対応」の記載をやめる
      *
      *  04.06.01    NACL-藤原    10/10/04  レセプトコメント文字数増対応
      *  04.06.02    NACL-藤原    11/10/07  傷病名を３行まで記載可能とする
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *                             
           SELECT  RECE43-FILE     ASSIGN  RECE43PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE43-KEY
                                   FILE    STATUS  IS  STS-RECE43.
      *   
           SELECT  RECE47-FILE     ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *   
           SELECT  RECE471-FILE    ASSIGN  RECE471PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE471-KEY
                                   FILE    STATUS  IS  STS-RECE471.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    保険別患者情報ファイル
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
      *    病名ファイル    
       FD  RECE43-FILE.
       01  RECE43-REC.
           COPY    "CPRCF043.INC".
      *
      *    点数摘要欄ファイル
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
      *
      *    金額摘要欄ファイル
       FD  RECE471-FILE.
       01  RECE471-REC. 
           COPY    "CPRCF0471.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE43//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE471//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    自賠責     
      *****COPY    "HCM53.INC"     REPLACING  //HCM53//
      *****COPY    "HCM53V01.INC"  REPLACING  //HCM53V01//
           COPY    "HCM53V02.INC"  REPLACING  //HCM53V02//
                                   BY         //HC23//.
      *
      *    自賠責（続紙）     
      *****COPY    "HCM29V02.INC"  REPLACING  //HCM29V02//
      *****COPY    "HCM29V03.INC"  REPLACING  //HCM29V03//
           COPY    "HCM29V04.INC"  REPLACING  //HCM29V04//
                                   BY         //HC29//.
      *
      *    自賠責（枠あり）
      ***  COPY    "HCM54.INC"     REPLACING  //HCM54//
      ***                          BY         //HC23//.
      *
      *    自賠責（枠あり）（続紙）
      ***  COPY    "HCM49.INC"     REPLACING  //HCM49//
      ***                          BY         //HC29//.
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE43          PIC X(02).
           03  STS-RECE47          PIC X(02).
           03  STS-RECE471          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE43          PIC 9(01).
           03  FLG-RECE47          PIC 9(01).
           03  FLG-RECE471          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
      *
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-STS             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE47          PIC 9(06).
           03  CNT-RECE471         PIC 9(06).
           03  CNT-RECE09          PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDX6                PIC 9(02).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(03).
           03  IDXY                PIC 9(03).
      *
           03  SYSKAN-IDX          PIC 9(03).
      *
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE.          
           03  KEY-NEW.
               05  KEY-N-HOSPNUM   PIC  9(02).
               05  KEY-N-SRYYM     PIC  9(06).
               05  KEY-N-NYUGAIKBN PIC  X(01).
               05  KEY-N-PTID      PIC  9(10).
               05  KEY-N-RECEKA    PIC  X(02).
           03  KEY-OLD.
               05  KEY-O-HOSPNUM   PIC  9(02).
               05  KEY-O-SRYYM     PIC  9(06).
               05  KEY-O-NYUGAIKBN PIC  X(01).
               05  KEY-O-PTID      PIC  9(10).
               05  KEY-O-RECEKA    PIC  X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001     PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC     OCCURS  100.
               05  TBL-SYS1002     PIC  X(610).
      *    帳票別医療機関編集情報指定退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC         OCCURS  100.
               05  TBL-SYS1900         PIC  X(610).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-CREYMD  PIC  X(08).
               05  WRK-PARA-CREHMS  PIC  X(08).
               05  WRK-PARA-SRYYM   PIC  X(06).
               05  WRK-PARA-TERMID  PIC  X(16).
               05  WRK-PARA-SYSYMD  PIC  X(08).
               05  WRK-PARA-SYOKBN  PIC  X(01).
               05  WRK-PARA-RECEKBN PIC  X(01).
               05  WRK-PARA-RECEYM  PIC  X(06). 
               05  WRK-PARA-JOBID   PIC  9(07).
               05  WRK-PARA-SHELLID PIC  X(08).
           03  WRK-PARA-FRAME       PIC  X(01).
           03  WRK-PARA-PRTKBN      PIC  X(01).
           03  WRK-PARA-HOSPNUM     PIC  9(02).
      *
           03  WRK-SYS-SRYYMD       PIC  X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RECENUM          PIC 9(06). 
      *
           03  WRK-NENREI          PIC 9(05).
           03  WRK-NENREI-R        REDEFINES   WRK-NENREI.
               05  WRK-NENREI-R1   PIC 9(03).
               05  WRK-NENREI-R2   PIC 9(02).
           03  WRK-BIRTHDAY.
               05  WRK-BIRTHYM     PIC 9(06).
               05  WRK-BIRTHDD     PIC 9(02). 
      *
           03  WRK-LINE-MAX        PIC 9(03).          
      *
           03  WRK-GOKEI           PIC 9(07).            
      *
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-STYMDW          PIC X(06).
           03  WRK-EDYMDW          PIC X(06).
      *
           03  WRK-SINRYO-MM.
               05  WRK-SINRYO-MM2  PIC 9(02).
           03  WRK-KYUGYO-MM.
               05  WRK-KYUGYO-MM2  PIC 9(02).
      *
           03  WRK-OPECOM-MCP-TABLE
                                   PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME
                                   PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-EDIT-AREA.
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(03).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(03).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(03).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(03).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行（点数）               
               05  WRK-LINETEK                 PIC 9(03).
      *        適用欄編集行（金額）               
               05  WRK-LINETEK1                PIC 9(03).
      *         
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1.
      **       05  WRK-BYOMEI1-NAME            PIC X(44).
               05  WRK-BYOMEI1-NAME            PIC X(50).
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2.  
               05  WRK-BYOMEI2-NAME            PIC X(52).
      *
      *    傷病名が３行になるときの編集
           03  WRK-BYOMEI3                     PIC X(52).
           03  WRK-MOJISU3                     PIC 9(03).
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT     PIC X(50)
                                       OCCURS  48.
           03  WRK-COMM            PIC X(2400).
           03  WRK-COMM1           PIC X(2400).
           03  WRK-COL             PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-RECECOM-NAME    PIC X(52).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
           03  WRK-RECECOM-SJKBN   PIC X(02).
      *
      *    手術歴編集
           03  WRK-OPECOM          PIC 9(01).
           03  WRK-OPECOM-HEN      PIC X(50).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE-HEN    PIC X(01).
               05  WRK-PAGE2       PIC Z9.
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG.
               05  WRK-HENGENGO    PIC X(01).
               05  WRK-HENYMD.
                   07  WRK-HENYY   PIC X(02).
                   07  WRK-HENMM   PIC X(02).
                   07  WRK-HENDD   PIC X(02).
      *
      *    レセプト格納用
       01  TABLE-AREA                          VALUE   SPACE.
           03  TBL-RECE-MAX        PIC 9(02).
           03  TBL-RECE-OCC            OCCURS  99.
               05  TBL-RECE-PRTDATA
                                   PIC X(10000).
               05  TBL-RECE-FORMID PIC X(20). 
               05  TBL-RECE-RENNUM PIC 9(02).
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "−".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(16)   VALUE
                                   "＝＝＝＝＝＝＝＝".
               05  FILLER          PIC X(20)   VALUE
                                   "　金額行為（続き）　".
               05  FILLER          PIC X(16)   VALUE
                                   "＝＝＝＝＝＝＝＝".
      **   03  WRK-KUGIRI3         PIC X(44)   VALUE
      **       ALL "−".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTENZ         PIC ZZZZZZZ.
           03  WRK-KAISUZ          PIC ZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZ.
           03  WRK-JNISSU          PIC ZZ9.
           03  WRK-TOTALTEN        PIC ZZZ,ZZZ.
           03  WRK-NENREI-X        PIC ZZ9.
           03  WRK-KINGAKUZ        PIC ZZZ,ZZZ.
           03  WRK-ZZZ9            PIC ZZZ9.
           03  WRK-SEIKYUZ         PIC Z,ZZZ,ZZ9.
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
      *
       01  WRK-CONS-AREA.    
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   50.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   52.
      *     
      *    適用欄最大行数（点数）
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   28.
      *    適用欄最大行数（金額）
           03  WRK-TEKIYOU1-LINE   PIC 9(02)   VALUE   6.
      *     
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   3.
      *     
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HC29-LINE
                                   PIC 9(03)   VALUE   110.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(52)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  FILLER              PIC X(52)   VALUE
               "【廃用症候群に係る評価表】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(52)   OCCURS   2.
      *     
           COPY    "ORCA-DBMETA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
           COPY    "CPSK1010.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
           COPY    "CPSK200501.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
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
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-STS     =   2
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TABLE-AREA
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-FRAME
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ファイルＯＰＥＮ処理
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE430"           TO  RECE43PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE43PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE43PARA-HOSPNUM
      *
           MOVE    "RECE470"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           MOVE    "RECE471"           TO  RECE471PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE471PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE471PARA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0488"      TO  JOB-PGID
               MOVE    "自賠責保険・印刷データ編集（自賠責）"
                                   TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               IF      FLG-STS             =   ZERO
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
      *         
      *        ジョブ管理更新（中止フラグ非活性）処理
               IF      FLG-STS             =   ZERO
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    3               TO  JOB-STOPFLG
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
      *    システム管理マスタ（医療機関情報）
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( FLG-STS             =   ZERO      )
               PERFORM 5001-DB-DELETE-SEC
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           OPEN    INPUT               RECE41-FILE
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE43-FILE
           IF      STS-RECE43          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE43PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE43          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE47-FILE
           IF      STS-RECE47          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE47PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE47          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE471-FILE
           IF      STS-RECE471         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE471PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE471         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    ZERO                TO  WRK-RECENUM
           ELSE
               MOVE    100                 TO  WRK-RECENUM
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *          
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1001"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報（所在地ほか）
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1002"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1002-REC
              ELSE
             MOVE   "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1002-T
              INITIALIZE                      TBL-SYS1002-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1002"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1002-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1002-REC  TO  TBL-SYS1002(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYS-1900
                                           FLG-SYS-1901
                                           FLG-SYS-1902
      *
      *    帳票別指定情報
           IF   WRK-PARA-SRYYM     NOT =  ZERO
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1900"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-1900-REC
              ELSE    
                  INITIALIZE                   SYS-1900-REC
              END-IF
              IF      SYS-1900-PRTKBN (3)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1900-T
              INITIALIZE                      TBL-SYS1900-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1900"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-PRTKBN (3)  =   SPACE
                        CONTINUE
                    ELSE
                        ADD     1             TO  SYSKAN-IDX
                        MOVE    SYS-1900-REC  TO
                                              TBL-SYS1900 (SYSKAN-IDX)
                        MOVE    1            TO  FLG-SYS-1900
                    END-IF 
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF 
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA          =   1
                   MOVE    "パラメタ情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "01"            TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-200501-REC
           ELSE    
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-INSPECT-CHARACTER-KBN
                                   =   SPACE
               MOVE    ZERO            TO
                                       SYS-200501-INSPECT-CHARACTER-KBN
           END-IF
           DISPLAY "SYS-200501-INSPECT-CHARACTER-KBN="
                                    SYS-200501-INSPECT-CHARACTER-KBN
                    " " WRK-PARA-PRTKBN
      *
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE41     =   1
                           OR      FLG-STS        =   2 
      *
      *        患者情報読込処理      
               PERFORM 2001-PTINF-HEN-SEC 
      *            レセプト種別別件数カウント
               MOVE    ZERO            TO  CNT-KENSU
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE41     =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW    NOT =   KEY-OLD
      *
                   ADD     1                      TO  CNT-KENSU
      *                                      
                   PERFORM 2001-SYORI-SEC
      *
                   PERFORM 990-RECE41-READ-SEC
               END-PERFORM
           END-PERFORM
      *
           MOVE    2               TO  FLG-STS
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者情報読込処理      
      *****************************************************************
       2001-PTINF-HEN-SEC       SECTION.
      *     
      *    診療年月編集
           MOVE    RECE41-SRYYM        TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           MOVE    WRK-STYMD           TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-STYMDW
      *     
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE41-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
           MOVE    WRK-EDYMD           TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-EDYMDW
           . 
       2001-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1         TO  CNT-PAGE
           INITIALIZE                  WRK-EDIT-AREA
           INITIALIZE                  TABLE-AREA
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *    帳票編集<手術歴>処理
           PERFORM 380-HC101-OPECOM-HEN-SEC 
      *
      *    帳票編集<適用（点数）>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<傷病の経過>処理
           PERFORM 360-HC101-COMMENT-HEN-SEC 
      *
      *    帳票編集<適用（金額）>処理
           PERFORM 350-HC101-ZAI-HEN1-SEC
      *
      *    帳票印刷処理（続紙）
           IF      CNT-PAGE           >   1
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      FLG-STS            =   ZERO
               MOVE    1                  TO  CNT-PAGE
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HC101-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-LINE    TO  WRK-LINE-MAX
      *     
           INITIALIZE                      HC23
      *
      *    対象診療年月
           MOVE     WRK-STYMDW(1:2)    TO  HC23-HAKKOUYY
           MOVE     WRK-STYMDW(3:2)    TO  HC23-HAKKOUMM 
      *    患者番号
           STRING   RECE41-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE41-RECEKA  DELIMITED   BY  SPACE
                                   INTO    HC23-PTNUM         
      *    カナ氏名
           MOVE    RECE41-KANANAME     TO  HC23-KANANAME
      *    氏名
           MOVE    RECE41-NAME         TO  HC23-NAME
      *    生年月日
           MOVE    RECE41-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           EVALUATE    WRK-HENGENGO
               WHEN    "M"
                   MOVE    "○"        TO  HC23-TANJYOG(1)
               WHEN    "T"
                   MOVE    "○"        TO  HC23-TANJYOG(2)
               WHEN    "S"
                   MOVE    "○"        TO  HC23-TANJYOG(3)
               WHEN    "H"
                   MOVE    "○"        TO  HC23-TANJYOG(4)
           END-EVALUATE     
           MOVE    WRK-HENYY           TO  HC23-TANJYOYY
      *    性別
           EVALUATE    RECE41-SEX
               WHEN    "1"
                   MOVE    "○"        TO  HC23-SEX(1)
               WHEN    "2"
                   MOVE    "○"        TO  HC23-SEX(2)
           END-EVALUATE     
      *    年齢
           MOVE    RECE41-BIRTHDAY     TO  WRK-BIRTHDAY    
           COMPUTE WRK-NENREI          =   RECE41-SRYYM
                                       -   WRK-BIRTHYM
           MOVE    WRK-NENREI-R1       TO  WRK-NENREI-X
           MOVE    WRK-NENREI-X        TO  HC23-NENREI 
      *    傷病年月日
           MOVE    RECE41-SHOBYOYMD    TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYY           TO  HC23-JYUSYOUBIYY
           MOVE    WRK-HENMM           TO  HC23-JYUSYOUBIMM
           MOVE    WRK-HENDD           TO  HC23-JYUSYOUBIDD
      *    初診年月日
           MOVE    RECE41-SHOSHINYMD   TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYY           TO  HC23-SYOSHINBIYY
           MOVE    WRK-HENMM           TO  HC23-SYOSHINBIMM
           MOVE    WRK-HENDD           TO  HC23-SYOSHINBIDD
      *
      *    療養開始年月日
           IF      RECE41-RYOSTYMD     <   WRK-STYMD
               MOVE    WRK-STYMDW(1:2)     TO  HC23-STRYY
               MOVE    WRK-STYMDW(3:2)     TO  HC23-STRMM
               MOVE    WRK-STYMDW(5:2)     TO  HC23-STRDD
               MOVE    WRK-STYMD(7:2)      TO  IDX5
           ELSE      
               MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENYY           TO  HC23-STRYY
               MOVE    WRK-HENMM           TO  HC23-STRMM
               MOVE    WRK-HENDD           TO  HC23-STRDD
               MOVE  RECE41-RYOSTYMD(7:2)  TO  IDX5
           END-IF    
      *    療養終了年月日
           IF      RECE41-RYOEDYMD     >   WRK-EDYMD
               MOVE    WRK-EDYMDW(1:2)     TO  HC23-ENDYY 
               MOVE    WRK-EDYMDW(3:2)     TO  HC23-ENDMM
               MOVE    WRK-EDYMDW(5:2)     TO  HC23-ENDDD
               MOVE    WRK-EDYMD(7:2)      TO  IDX6
           ELSE      
               MOVE    RECE41-RYOEDYMD     TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENYY           TO  HC23-ENDYY
               MOVE    WRK-HENMM           TO  HC23-ENDMM
               MOVE    WRK-HENDD           TO  HC23-ENDDD
               MOVE  RECE41-RYOEDYMD(7:2)  TO  IDX6
           END-IF    
      *    療養開始年月日再セット
           PERFORM   VARYING   IDX4   FROM   IDX5   BY  1
                     UNTIL     IDX4    >     IDX6
                 IF  (RECE41-NYUINBITBL(IDX4)  =  SPACE)  OR
                     (RECE41-JNISSUTBL(IDX4)   =  "1"  )
                     MOVE    IDX4                TO  WRK-ZZ
                     MOVE    WRK-ZZ              TO  HC23-STRDD
                     MOVE    IDX6                TO  IDX4
                 END-IF
           END-PERFORM
      *    療養終了年月日再セット
           PERFORM   VARYING   IDX4   FROM   IDX6   BY  -1
                     UNTIL     IDX4    <     IDX5
                 IF  (RECE41-NYUINBITBL(IDX4)   =  SPACE) OR
                     (RECE41-JNISSUTBL(IDX4)    =  "1"  )
                     MOVE    IDX4                TO  WRK-ZZ
                     MOVE    WRK-ZZ              TO  HC23-ENDDD
                     MOVE    IDX5                TO  IDX4
                 END-IF
           END-PERFORM
      *
           IF      RECE41-JNISSU  =  999
              IF  (RECE41-KYUGYOFLG      =   SPACE)
              OR  (RECE41-KYUGYO-PRTKBN  =   "2"  )
                MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
                PERFORM 31012-SE-WA-YMDEDIT-SEC
                MOVE    WRK-HENYY           TO  HC23-STRYY
                                                HC23-ENDYY
                MOVE    WRK-HENMM           TO  HC23-STRMM
                                                HC23-ENDMM
                MOVE    WRK-HENDD           TO  HC23-STRDD
                                                HC23-ENDDD
              ELSE
                MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
                PERFORM 31012-SE-WA-YMDEDIT-SEC
                MOVE    WRK-HENYY           TO  HC23-STRYY
                MOVE    WRK-HENMM           TO  HC23-STRMM
                MOVE    WRK-HENDD           TO  HC23-STRDD
      *
                IF      RECE41-KYUGYO-END   NOT   NUMERIC
                OR      RECE41-KYUGYO-END   =  "0000"
                  MOVE    SPACE             TO  HC23-ENDYY
                  MOVE    SPACE             TO  HC23-ENDMM
                  MOVE    SPACE             TO  HC23-ENDDD
                ELSE
                  MOVE  RECE41-SRYYM(5:2)   TO  WRK-SINRYO-MM
                  MOVE  RECE41-KYUGYO-END(1:2)
                                            TO  WRK-KYUGYO-MM
                  IF    WRK-KYUGYO-MM2    <=    WRK-SINRYO-MM2
                      MOVE   RECE41-SRYYM(1:4)  TO  WRK-SYMD(1:4)
                      MOVE   RECE41-KYUGYO-END  TO  WRK-SYMD(5:4)
                      PERFORM 31012-SE-WA-YMDEDIT-SEC
                      MOVE    WRK-HENYY         TO  HC23-ENDYY
                      MOVE    WRK-HENMM         TO  HC23-ENDMM
                      MOVE    WRK-HENDD         TO  HC23-ENDDD
                  ELSE
                      INITIALIZE                    STS-AREA-DAY
                      INITIALIZE                    LNK-DAY6-AREA
                      MOVE    "61"              TO  LNK-DAY6-IRAI
                      MOVE    RECE41-SRYYM      TO  LNK-DAY6-KIJUN(1:6)
                      MOVE    "01"              TO  LNK-DAY6-KIJUN(7:2)
                      MOVE    "2"               TO  LNK-DAY6-ZOGENPTN
                      MOVE    -12               TO  LNK-DAY6-ZOGEN
                      CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                        LNK-DAY6-AREA
                      MOVE    LNK-DAY6-KEISAN(1:4)
                                                TO  WRK-SYMD(1:4)
                      MOVE    RECE41-KYUGYO-END TO  WRK-SYMD(5:4)
                      PERFORM 31012-SE-WA-YMDEDIT-SEC
                      MOVE    WRK-HENYY         TO  HC23-ENDYY
                      MOVE    WRK-HENMM         TO  HC23-ENDMM
                      MOVE    WRK-HENDD         TO  HC23-ENDDD
                  END-IF
                END-IF
              END-IF
           END-IF
      *
      *    実日数
           MOVE    RECE41-JNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC23-JITSUNISSU
      *    転帰事由
           EVALUATE    RECE41-TENKI
               WHEN    "1"
                   MOVE    "○"        TO  HC23-TENKI(1)
               WHEN    "3"
                   MOVE    "○"        TO  HC23-TENKI(2)
               WHEN    "5"
                   MOVE    "○"        TO  HC23-TENKI(3)
               WHEN    "7"
                   MOVE    "○"        TO  HC23-TENKI(4)
               WHEN    "9"
                   MOVE    "○"        TO  HC23-TENKI(5)
           END-EVALUATE    
      *    加算率
           IF      RECE41-KSNRATE(1:1)  =  "1"
               CONTINUE
           ELSE
             MOVE  RECE41-KSNRATE(1:1) TO  HC23-SEIKYU-SEISU(1)
                                           HC23-SEIKYU-SEISU(2)
           END-IF
           MOVE    RECE41-KSNRATE(2:2) TO  HC23-SEIKYU(1)
                                           HC23-SEIKYU(2)
      *    請求額Ａ
           MOVE    RECE41-TOTALKIN       TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKU(1)
      *    請求額Ｂ
           MOVE    RECE41-TOTALKIN-YAK   TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKU(2) 
      *    請求額Ｃ
           MOVE    RECE41-TOTALKIN2      TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKU(3) 
      *    請求額Ｄ
           MOVE    RECE41-TOTALKIN2-ETC  TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKU(4)
      *    請求額計
           COMPUTE WRK-GOKEI   =    RECE41-TOTALKIN
                               +    RECE41-TOTALKIN-YAK
                               +    RECE41-TOTALKIN2
                               +    RECE41-TOTALKIN2-ETC

           MOVE    WRK-GOKEI             TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKU(5)
      *    通院月
           MOVE    WRK-STYMDW(3:2)     TO  HC23-TSUINMM(1) 
      *    通院日
           PERFORM    VARYING    IDZ    FROM    1   BY   1              
                          UNTIL      IDZ    >     31                    
                 IF    RECE41-TUUIN-DAY(IDZ)                         
                                        NOT =  SPACE                    
                        MOVE    "○"    TO  HC23-TSUIN(1 IDZ)
                 END-IF                                              
           END-PERFORM
      *    通院日数                                         
           MOVE    RECE41-TUUIN-KEI     TO  WRK-Z9
           MOVE    WRK-Z9               TO  HC23-TSUINSUU(1)
      *    請求額
           COMPUTE WRK-GOKEI   =    RECE41-TOTALKIN
                               +    RECE41-TOTALKIN-YAK
                               +    RECE41-TOTALKIN2
                               +    RECE41-TOTALKIN2-ETC
           MOVE    WRK-GOKEI             TO  WRK-SEIKYUZ
           MOVE    WRK-SEIKYUZ           TO  HC23-SEIKYUGAKUTOTAL
      *    所在地
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-ADRS1      TO  HC23-HOSPJYUSYO (1:)
               MOVE    SYS-1902-ADRS2      TO  HC23-HOSPJYUSYO (51:)
           ELSE
               MOVE    SYS-1002-ADRS       TO  HC23-HOSPJYUSYO
           END-IF
      *    名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC23-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC23-HOSPNAME (41:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC23-HOSPNAME
           END-IF
      *    管理者
           IF   RECE41-DRCD  =  SPACE
               MOVE    SYS-1001-KANRINAME  TO  HC23-ISYA
           ELSE
      *        職員情報
               INITIALIZE                   SYSKANRI-REC
               MOVE    "1010"           TO  SYS-KANRICD
               MOVE    RECE41-DRCD      TO  SYS-KBNCD
               MOVE    RECE41-SRYYM     TO  SYS-STYUKYMD (1:6)
               MOVE    "01"             TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
               MOVE    "key10"          TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI     =   ZERO
                   MOVE  MCPDATA-REC    TO  SYS-1010-REC
                   MOVE  SYS-1010-NAME  TO  HC23-ISYA
               END-IF
           END-IF
      *    電話番号
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-TEL        TO  HC23-TEL
           ELSE
               MOVE    SYS-1002-TEL        TO  HC23-TEL
           END-IF
      *    病床数
           MOVE    SYS-1001-BEDSU      TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  HC23-BYOSYOSU
      *    保険会社名
           IF   RECE41-HOKENNAME(31:) = SPACE
               MOVE    RECE41-HOKENNAME(1:30)
                                       TO  HC23-HOKENNAME(31:30)
           ELSE
               MOVE    RECE41-HOKENNAME
                                       TO  HC23-HOKENNAME
           END-IF
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN     =   "2"
               IF      SYS-200501-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HC23-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HC23-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HC23-LOPTION
           END-IF
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HC29-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-HC29-LINE    TO  WRK-LINE-MAX
      *
           INITIALIZE                 HC29
      *      
      *    カナ氏名
           MOVE    RECE41-KANANAME     TO  HC29-KANANAME
      *    氏名
           MOVE    RECE41-NAME         TO  HC29-NAME
      *    患者番号
           STRING   RECE41-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE41-RECEKA  DELIMITED   BY  SPACE
                    "-"            DELIMITED   BY  SIZE
                    CNT-PAGE (3:1) DELIMITED   BY  SIZE
                                   INTO    HC29-PTNUM         
           END-STRING         
      *
      *    点検用レセプト作成時
           MOVE    HC23-LABEL          TO  HC29-LABEL
           MOVE    HC23-LOPTION        TO  HC29-LOPTION
      *
           .
       310-HC29-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *     
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
                                           WRK-BYOMEI3
           MOVE    ZERO                TO  WRK-MOJISU3
      *
      *    傷病の部位編集
      **     IF      RECE41-COMMENT      =   SPACE
      **         CONTINUE
      **     ELSE
      **         ADD     1               TO  WRK-LINEBYO
      **         MOVE    RECE41-COMMENT  TO  HC23-SYOBYO (WRK-LINEBYO)
      **         IF      RECE41-COMMENT (51:)    NOT =   SPACE
      **             ADD     1           TO  WRK-LINEBYO
      **             MOVE    RECE41-COMMENT (51:50)
      **                                 TO  HC23-SYOBYO (WRK-LINEBYO)
      **         END-IF                        
      **         IF      RECE41-COMMENT (100:)    NOT =   SPACE
      **             ADD     1           TO  WRK-LINEBYO
      **             MOVE    RECE41-COMMENT (100:)
      **                                 TO  HC23-SYOBYO (WRK-LINEBYO)
      **         END-IF                        
      **     END-IF
      *
      *    傷病名の編集                    
           INITIALIZE                      RECE43-REC
           MOVE    RECE41-HOSPNUM      TO  RECE43-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE43-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE43-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE43-PTID
           MOVE    RECE41-RECEKA       TO  RECE43-RECEKA
           MOVE    RECE41-HKNID        TO  RECE43-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE43-TEKSTYMD
      *
           START   RECE43-FILE
               KEY IS    >=   RECE43-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE43
               NOT INVALID
                   PERFORM 990-RECE43-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE43          =   1                 )
               OR     ( RECE41-HOSPNUM   NOT =   RECE43-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE43-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE43-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE43-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE43-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE43-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE43-TEKSTYMD  )
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE43     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   PERFORM 3222-BYOMEI2-OUT-SEC
               END-IF
           END-IF
      *
           .
       320-HC101-BYOMEI-HEN-EXT.
           EXIT.
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       3210-BYOMEI-SET-SEC           SECTION.
      *
      *    病名をつなげると何桁になるかを計算（，＋病名）
           IF  WRK-CLM           =   ZERO
               MOVE    WRK-CLM   TO  WRK-WCLM
           ELSE
               COMPUTE WRK-WCLM  =   WRK-CLM   +   2
           END-IF
           COMPUTE     WRK-WCLM  =
                      ( RECE43-BYOMEIMOJI  *   2 )  +   WRK-WCLM
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                       IF  WRK-BYOMEI1  NOT =  SPACE
                           PERFORM 3221-BYOMEI1-OUT-SEC
                       END-IF
                   ELSE
                       IF  WRK-CLM       >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM =   WRK-CLM   +    1
                           MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +   2
                       END-IF
                   END-IF
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                       PERFORM 3222-BYOMEI2-OUT-SEC
                   ELSE
                       IF  WRK-CLM       >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM =   WRK-CLM   +   1
                           MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +   2
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM       =   WRK-CLM   +   1
      *     
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE43-BYOMEIMOJI   *   2
           MOVE    SPACE               TO  WRK-BYOMEI3
           MOVE    ZERO                TO  WRK-MOJISU3
      *     
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    RECE43-BYOMEI   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    RECE43-BYOMEI(51:)
                                                   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
      *
                           MOVE    RECE43-BYOMEI(101:) TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       100
                       ELSE
                           MOVE    RECE43-BYOMEI(51:)
                                                   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
      *
                           MOVE    RECE43-BYOMEI(103:) TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       102
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    RECE43-BYOMEI   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    RECE43-BYOMEI(53:)
                                               TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
      *
                       MOVE    RECE43-BYOMEI(105:) TO  WRK-BYOMEI3
                       COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                   104
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *
           IF      WRK-BYOMEI3       NOT =   SPACE
               EVALUATE    TRUE
      *            傷病名欄に編集
                   WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                       ELSE
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                       END-IF                                   
      *            適用欄に編集
                   WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                       PERFORM 3222-BYOMEI2-OUT-SEC
                       MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
               END-EVALUATE
      *
               COMPUTE     WRK-CLM           =   WRK-MOJISU3
                                             +   WRK-CLM
           END-IF
      *
           PERFORM 990-RECE43-NEXT-SEC
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *     傷病名欄出力処理
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
           ADD     1               TO  WRK-LINEBYO
      *
           INSPECT WRK-BYOMEI1
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI1     TO  HC23-SYOBYO (WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名 適用欄出力処理
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF    
      *     
           ADD     1               TO  WRK-LINETEK
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
      *
           IF      CNT-PAGE        =   1
               MOVE    WRK-BYOMEI2     TO  HC23-TEKIYO2 (WRK-LINETEK)
           ELSE
               MOVE    WRK-BYOMEI2     TO  HC29-TEKIYO  (WRK-LINETEK)
           END-IF    
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<適用（金額）>処理
      *****************************************************************
       350-HC101-ZAI-HEN1-SEC      SECTION.
      *
           MOVE    ZERO                TO  CNT-RECE471
                                           WRK-LINETEK1
      *
      * 
           INITIALIZE                      RECE471-REC
           MOVE    RECE41-HOSPNUM      TO  RECE471-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE471-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE471-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE471-PTID
           MOVE    RECE41-RECEKA       TO  RECE471-RECEKA
           MOVE    RECE41-HKNID        TO  RECE471-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE471-TEKSTYMD
      *
           START   RECE471-FILE
               KEY IS    >=   RECE471-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE471
               NOT INVALID
                   PERFORM 990-RECE471-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE471          =   1                )
                   OR    ( RECE41-HOSPNUM  NOT =   RECE471-HOSPNUM   )
                   OR    ( RECE41-SRYYM    NOT =   RECE471-SRYYM     )
                   OR    ( RECE41-NYUGAIKBN
                                           NOT =   RECE471-NYUGAIKBN )
                   OR    ( RECE41-PTID     NOT =   RECE471-PTID      )
                   OR    ( RECE41-RECEKA   NOT =   RECE471-RECEKA    )
                   OR    ( RECE41-HKNID    NOT =   RECE471-HKNID     )
                   OR    ( RECE41-TEKSTYMD NOT =   RECE471-TEKSTYMD  )
      *
      *        明細
               INSPECT RECE471-TEKIYO-INF
                               REPLACING    ALL   "  "   BY   "　"
      *                         
               EVALUATE    TRUE        
                   WHEN    WRK-LINETEK1        <   WRK-TEKIYOU1-LINE
                       ADD     1                   TO  WRK-LINETEK1
                       MOVE    RECE471-TEKIYO-INF  TO  HC23-TEKIYO1
                                                       (WRK-LINETEK1)
                   WHEN    WRK-LINETEK1        =   WRK-TEKIYOU1-LINE
                       IF      WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK         >=   WRK-LINE-MAX
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           ADD     1           TO  WRK-LINETEK
                                                   WRK-LINETEK1
                           IF      CNT-PAGE        =   1 
                               MOVE    WRK-KUGIRI2     TO 
                                               HC23-TEKIYO2(WRK-LINETEK)
                           ELSE
                               MOVE    WRK-KUGIRI2     TO 
                                               HC29-TEKIYO (WRK-LINETEK)
                           END-IF
                       END-IF
      ***              ADD     1                   TO  WRK-LINETEK
                       IF      WRK-LINETEK          >=   WRK-LINE-MAX
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       ADD     1                   TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1 
                           MOVE    RECE471-TEKIYO-INF  TO
                                               HC23-TEKIYO2(WRK-LINETEK)
                       ELSE
                           MOVE    RECE471-TEKIYO-INF  TO
                                               HC29-TEKIYO (WRK-LINETEK)
                       END-IF
                   WHEN    WRK-LINETEK1        >   WRK-TEKIYOU1-LINE
                       IF      WRK-LINETEK     >=  WRK-LINE-MAX
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1                   TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1 
                           MOVE    RECE471-TEKIYO-INF  TO
                                               HC23-TEKIYO2(WRK-LINETEK)
                       ELSE
                           MOVE    RECE471-TEKIYO-INF  TO
                                               HC29-TEKIYO (WRK-LINETEK)
                       END-IF
               END-EVALUATE        
      *
               PERFORM 990-RECE471-NEXT-SEC
           END-PERFORM
      *
           .
       350-HC101-ZAI-HEN1-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
      *    診療内容・点数（点）
      *
      *    初診                           
           EVALUATE    RECE41-SHOSHINTIME
               WHEN    "1"
                   MOVE  "○"       TO  HC23-SYOSHIN(1)
               WHEN    "2"
                   MOVE  "○"       TO  HC23-SYOSHIN(2)
               WHEN    "3"
                   MOVE  "○"       TO  HC23-SYOSHIN(3)
           END-EVALUATE
      *    乳幼児
           IF          RECE41-SHOSHIN-NYU  =   "1"
                   MOVE  "○"       TO  HC23-SYOSHIN(4)
           END-IF
      *
           PERFORM   VARYING   IDX   FROM    2   BY  1
                     UNTIL     IDX       >   35
      *
               COMPUTE     IDZ  =    IDX   -   1
      * 
      *        <単価>
               EVALUATE    IDX
      *            投薬
                   WHEN    10
                   WHEN    13
                   WHEN    14
                       MOVE    RECE41-KHNTEN(IDX) TO  WRK-KHNTENZ
                       MOVE    WRK-KHNTENZ        TO  HC23-KHNTEN  (IDZ)
               END-EVALUATE
      *        <回数>
               MOVE    RECE41-KAISU(IDX)          TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ                 TO  HC23-KAISU   (IDZ)
           END-PERFORM
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   35
      *        <手技点数>
               MOVE    RECE41-TENSU(IDX)          TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ                 TO  HC23-KOUTEN  (IDX)
      *        <薬剤等点数>
               MOVE    RECE41-TENSU-YAK(IDX)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ                 TO  HC23-YKZTENSU(IDX)
           END-PERFORM
      *
      *    手技点数計
           MOVE    RECE41-TOTALTEN        TO  WRK-TOTALTEN
           MOVE    WRK-TOTALTEN           TO  HC23-KOUTENG
      *    薬剤等点数計
           MOVE    RECE41-TOTALTEN-YAK    TO  WRK-TOTALTEN
           MOVE    WRK-TOTALTEN           TO  HC23-YKZTENSUG
      *
      *    診療内容・金額                     
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   6
      *        <金額>
               MOVE    RECE41-KINGAKU2(IDX)      TO  WRK-KINGAKUZ
               MOVE    WRK-KINGAKUZ              TO  HC23-KINGAKU (IDX)
           END-PERFORM
      *     
      *    <再診回数>
           MOVE    RECE41-KAISU2 (2)         TO  WRK-ZZ
           MOVE    WRK-ZZ                    TO  HC23-SAISINKAISU
      *     
      *    <指導回数>
           MOVE    RECE41-KAISU2 (3)         TO  WRK-ZZ
           MOVE    WRK-ZZ                    TO  HC23-SHIDOKAISU
      *
      *    診断書料
           MOVE    RECE41-SINDAN-TUU         TO  WRK-ZZZ
           MOVE    WRK-ZZZ                   TO  HC23-SHINDANSYOSUU
      *    診断書金額
           MOVE    RECE41-SINDAN-KIN         TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ              TO  HC23-SHINDANSYOMONEY
      *    明細書料
           MOVE    RECE41-MEISAI-TUU         TO  WRK-ZZZ
           MOVE    WRK-ZZZ                   TO  HC23-MEISAISYOSUU
      *    明細書金額
           MOVE    RECE41-MEISAI-KIN         TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ              TO  HC23-MEISAISYOMONEY
      *
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<剤>処理
      *****************************************************************
       340-HC101-ZAI-HEN-SEC      SECTION.
      *
      *    摘要欄に既に印刷されている場合、１行あける
           IF  WRK-LINETEK             >   ZERO
      *
               IF  WRK-LINETEK          >=   WRK-LINE-MAX
                   PERFORM 34120-PAGE-SEC
               END-IF    
               PERFORM 34110-KUGIRI-LINE-SEC
           END-IF
      *
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  CNT-RECE47
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE47-REC
           MOVE    RECE41-HOSPNUM      TO  RECE47-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE47-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE47-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE47-PTID
           MOVE    RECE41-RECEKA       TO  RECE47-RECEKA
           MOVE    RECE41-HKNID        TO  RECE47-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE47-TEKSTYMD
      *
           START   RECE47-FILE
               KEY IS    >=   RECE47-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE47
               NOT INVALID
                   PERFORM 990-RECE47-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE47           =   1                  )
               OR     ( RECE41-HOSPNUM   NOT =   RECE47-HOSPNUM     )
               OR     ( RECE41-SRYYM     NOT =   RECE47-SRYYM       )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE47-NYUGAIKBN   )
               OR     ( RECE41-PTID      NOT =   RECE47-PTID        )
               OR     ( RECE41-RECEKA    NOT =   RECE47-RECEKA      )
               OR     ( RECE41-HKNID     NOT =   RECE47-HKNID       )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE47-TEKSTYMD    )
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE47-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE47-HENSYU-SEC
      *
               MOVE    RECE47-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE47-SRYKBN        TO  WRK-R061-SRYKBN-O
      *
               PERFORM 990-RECE47-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE47-HENSYU-SEC           SECTION.
      *
           IF  CNT-ZAIMEISAISU           =   1
      *        次の行為明細数を追加すると何行になるか
               COMPUTE   WRK-WLINETEK    =   WRK-LINETEK
                                         +   RECE47-ZAIMEISAISU
               IF  CNT-RECE47           >   1
      *            診療区分が変更している場合、診療区切り線分を追加
                   IF  RECE47-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       ADD     1             TO  WRK-WLINETEK
                       MOVE    1             TO  FLG-KUGIRI
                   END-IF
               END-IF
      *
               IF  WRK-WLINETEK          >         WRK-LINE-MAX

                   PERFORM 34120-PAGE-SEC
      *
      *            区切り線
                   IF  FLG-KUGIRI        =   2
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1               TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1
                       MOVE    RECE47-SRYKBN  TO  HC23-SRYKBN
                                                       (WRK-LINETEK)
                   ELSE
                       MOVE    RECE47-SRYKBN  TO  HC29-SRYKBN
                                                       (WRK-LINETEK)
                   END-IF                                    
               ELSE
      *            区切り線
                   IF  FLG-KUGIRI    NOT =   ZERO
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1             TO  WRK-LINETEK
                   IF  RECE47-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       IF      CNT-PAGE        =   1
                           MOVE    RECE47-SRYKBN  TO  HC23-SRYKBN
                                                       (WRK-LINETEK)
                       ELSE
                           MOVE    RECE47-SRYKBN  TO  HC29-SRYKBN
                                                       (WRK-LINETEK)
                       END-IF                                    
                   END-IF
               END-IF
           ELSE
               ADD     1                 TO  WRK-LINETEK
           END-IF
      *
      *    明細
           INSPECT RECE47-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    RECE47-TEKIYO-INF  TO  HC23-TEKIYO2(WRK-LINETEK)
           ELSE    
               MOVE    RECE47-TEKIYO-INF  TO  HC29-TEKIYO (WRK-LINETEK)
           END-IF    
      *
           .
       3410-RECE47-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           IF      CNT-PAGE        =   1
               MOVE    WRK-KUGIRI1 TO  HC23-TEKIYO2 (WRK-LINETEK)
           ELSE    
               MOVE    WRK-KUGIRI1 TO  HC29-TEKIYO  (WRK-LINETEK)
           END-IF    
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    改ページ処理
      *****************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE      >   1
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
      * 
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HC29-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       360-HC101-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
           .
       360-HC101-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       3601-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE41-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE41-PTID         TO  RECECOM-PTID
           MOVE    RECE41-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               IF      RECECOM-HKNCOMBI    =   RECE41-HKNCOMBI
                   PERFORM 3601-COMMENT-HENSYU-SEC                 
               END-IF
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       3601-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3601-COMMENT-HENSYU-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
           MOVE    ZERO            TO  FLG-RECECOM-SJKBN
      *
      *    症状詳記区分の見出しを記載
           IF      RECECOM-SJKBN   =   "51"   OR  "52"
               MOVE    RECECOM-SJKBN   TO  RECECOM-SJKBN
           ELSE
               MOVE    SPACE           TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
      *
               MOVE    SPACE           TO  WRK-RECECOM-NAME
               EVALUATE    WRK-RECECOM-SJKBN
                   WHEN    "51"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (1)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "52"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (2)
                                               TO  WRK-RECECOM-NAME
               END-EVALUATE
           END-IF
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
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1    =   IDX     -   1
                               COMPUTE IDY2    =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                               TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                               TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                               TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-CONS-RECECOM-MAX
                           OR      WRK-COMMENT (IDX)   =   SPACE
      *                摘要欄に１行あける
                       IF      WRK-RECECOM         =   ZERO
                           MOVE    1               TO  WRK-RECECOM
                           IF  WRK-LINETEK         >   ZERO
                               IF      WRK-LINETEK >=  WRK-LINE-MAX
                                   PERFORM 34120-PAGE-SEC
                               ELSE
                                   PERFORM 34110-KUGIRI-LINE-SEC
                               END-IF
                           END-IF    
                       END-IF
      *
      *                症状詳記区分の見出しを記載
                       IF      FLG-RECECOM-SJKBN   =   1
                           IF      WRK-LINETEK      >= WRK-LINE-MAX
                               PERFORM 34120-PAGE-SEC
                           END-IF
                           ADD     1               TO  WRK-LINETEK
                           IF      CNT-PAGE        =   1   
                               MOVE    WRK-RECECOM-NAME
                                           TO  HC23-TEKIYO2(WRK-LINETEK)
                           ELSE                
                               MOVE    WRK-RECECOM-NAME
                                           TO  HC29-TEKIYO (WRK-LINETEK)
                           END-IF
                           MOVE    ZERO            TO  FLG-RECECOM-SJKBN
                       END-IF            
      *                 
                       IF      WRK-LINETEK      >=
                                                WRK-LINE-MAX
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE    =   1 
                           MOVE    WRK-COMMENT (IDX)
                                           TO  HC23-TEKIYO2(WRK-LINETEK)
                       ELSE
                           MOVE    WRK-COMMENT (IDX)
                                           TO  HC29-TEKIYO (WRK-LINETEK)
                       END-IF                
                   END-PERFORM
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       3601-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<手術歴>処理
      *****************************************************************
       380-HC101-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-OPECOM
                                           WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE41-HKNID  )
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE41-HKNID  )
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       380-HC101-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3801-OPECOM-HENSYU-SEC    SECTION.
      *
           IF      WRK-OPECOM          =   ZERO
      *        傷病名との間に区切り線を出力
               IF      WRK-LINETEK         >   ZERO
                   IF      WRK-LINETEK >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
               END-IF
           ELSE
      *        手術歴とコメントの間に改行
               IF    ( WRK-OPECOM-MCP-PATHNAME
                                           =   "key5" )
               AND   ( WRK-OPECOM          =   1      )
                   IF      WRK-LINETEK     >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1                   TO  WRK-LINETEK
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-OPECOM-MCP-PATHNAME =   "key4"
               MOVE    1                   TO  WRK-OPECOM
           ELSE
               MOVE    2                   TO  WRK-OPECOM
           END-IF                 
      *
           IF      WRK-LINETEK         >=  WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
           IF      CNT-PAGE            =   1   
               MOVE    WRK-OPECOM-HEN      TO  HC23-TEKIYO2(WRK-LINETEK)
           ELSE                
               MOVE    WRK-OPECOM-HEN      TO  HC29-TEKIYO (WRK-LINETEK)
           END-IF                
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       3801-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       390-PRINT-OUT-SEC               SECTION.
      *
      *    レセプト種別不明のとき続紙は出力しない
           IF    ( RECE41-RECESYUBETU   =   ZERO )
           AND   ( CNT-PAGE             >   1    )
               GO  TO  390-PRINT-OUT-EXT
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               EVALUATE  WRK-PARA-FRAME
                 WHEN      "0"
                   MOVE  "HCM53V02.red"
                                       TO  TBL-RECE-FORMID(TBL-RECE-MAX)
                 WHEN      "1"
                   MOVE  "HCM54V02.red"
                                       TO  TBL-RECE-FORMID(TBL-RECE-MAX)
               END-EVALUATE
               MOVE       HC23       TO  TBL-RECE-PRTDATA(TBL-RECE-MAX)
           ELSE
               EVALUATE  WRK-PARA-FRAME
                 WHEN      "0"
                   MOVE  "HCM29V04.red"
                                     TO  TBL-RECE-FORMID(TBL-RECE-MAX)
                 WHEN      "1"
                   MOVE  "HCM49V02.red"
                                     TO  TBL-RECE-FORMID(TBL-RECE-MAX)
               END-EVALUATE
               MOVE       HC29       TO  TBL-RECE-PRTDATA(TBL-RECE-MAX)
           END-IF        
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票更新処理
      *****************************************************************
       391-DB-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               ADD     10                  TO  WRK-RECENUM
           END-IF
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL   IDXY    >       TBL-RECE-MAX
                   OR      FLG-ERR >       ZERO
      *
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                   ADD     1                   TO  WRK-RECENUM
               END-IF
      * 
      *        レセプト明細管理ＤＢ
               PERFORM 3901-RECE08-INSERT-SEC
      *
      *        レセプト帳票管理ファイル出力
               IF    ( FLG-ERR              =   ZERO     )
               AND   ( WRK-PARA-SHELLID NOT =  "RECEPTX" )
                   PERFORM 3903-RECE07-WRITE-SEC
               END-IF
           END-PERFORM
      *
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  FLG-STS
           END-IF
      *
           .
       391-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細管理ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC23"              TO  RECE08-PRTID
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    ALL "9"         TO  RECE08-RECEYM
           ELSE
                   MOVE    WRK-PARA-SRYYM  TO  RECE08-RECEYM
           END-IF
           MOVE    WRK-PARA-CREYMD     TO  RECE08-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECE08-CREHMS
      ***  MOVE    RECEYMD-CREYMD      TO  RECE08-CREYMD
      ***  MOVE    RECEYMD-CREHMS      TO  RECE08-CREHMS
           MOVE    "0"                 TO  RECE08-PREFKBN
           MOVE    "00"                TO  RECE08-PREFNUM
           MOVE    RECE41-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    "4"                 TO  RECE08-TEISYUTUSAKI
      *****IF      RECE41-PTNUM        =   SPACE
      *         MOVE    ZERO                TO  RECE08-RECESYUBETU
      *     ELSE      
               IF      TBL-RECE-RENNUM (IDXY)  =   1                                 
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
               ELSE
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
                   MOVE    "2"                 TO  RECE08-RECESYUBETU
                                                                (4:1)
               END-IF                                                         
      *****END-IF    
           MOVE    RECE41-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE41-SRYYM        TO  RECE08-SRYYM
           MOVE    TBL-RECE-RENNUM (IDXY)
                                       TO  RECE08-RENNUM
      *****IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *****    MOVE    WRK-RECENUM         TO  RECE08-RECENUM
      *****ELSE
      *****    COMPUTE RECE08-RECENUM      =   WRK-RECENUM
      *****                                +   TBL-RECE-RENNUM (IDXY)
      *****END-IF
           MOVE    WRK-RECENUM         TO  RECE08-RECENUM
      *
           MOVE    "00"                TO  RECE08-SRYKA
           MOVE    "99999999"          TO  RECE08-HKNJANUM-S
           MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR0488 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE41-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           ELSE    
               MOVE    RECE41-KANANAME     TO  RECE08-KANANAME
      *********MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    WRK-PARA-TERMID     TO  RECE08-TERMID
               MOVE    ZERO                TO  RECE08-PRTJYUN
                                               RECE08-PRTRENNUM
               MOVE    TBL-RECE-FORMID  (IDXY)
                                           TO  RECE08-FORMID
               MOVE    TBL-RECE-PRTDATA (IDXY)
                                           TO  RECE08-PRTDATA
      *        複数枚レセの場合連番を記載
               IF      TBL-RECE-MAX        >   1
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    TBL-RECE-RENNUM (IDXY) 
                                               TO  WRK-PAGE1
                   MOVE    "/"                 TO  WRK-PAGE-HEN
                   MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
                   EVALUATE    RECE08-FORMID
                       WHEN    "HCM53V02.red"
                       WHEN    "HCM54V02.red"
                           MOVE    RECE08-PRTDATA  TO HC23
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC23-PAGE
                           MOVE    HC23            TO RECE08-PRTDATA
                       WHEN    "HCM29V04.red"                  
                       WHEN    "HCM49V02.red"
                           MOVE    RECE08-PRTDATA  TO HC29
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC29-PAGE
                           MOVE    HC29            TO RECE08-PRTDATA
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE     RECE08-REC         TO  MCPDATA-REC
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT 
               ELSE 
                   DISPLAY "ORCR0488 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE41-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
                   END-STRING
      *********    PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 900-DB-ERR-ABORT-SEC
               END-IF
           END-IF    
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
grpsys     MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
           MOVE    RECE08-PRTID        TO  RECE07-PRTID
           IF      WRK-PARA-SRYYM      =   ZERO
               MOVE    "2"                 TO  RECE07-SYOKBN
           ELSE
               MOVE    "1"                 TO  RECE07-SYOKBN
           END-IF         
           MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
           MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
           MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
           MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
           MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
           MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN
           MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
           MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
           MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
           MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
           MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
           MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
           MOVE    RECE08-RECESYUBETU-SRT
                                       TO  RECE07-RECESYUBETU-SRT
           MOVE    RECE07-REC          TO  MCPDATA-REC 
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   1
               INITIALIZE                      RECE07-REC
grpsys         MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
               MOVE    RECE08-PRTID        TO  RECE07-PRTID
               IF      WRK-PARA-SRYYM      =   ZERO
                   MOVE    "2"                 TO  RECE07-SYOKBN
               ELSE
                   MOVE    "1"                 TO  RECE07-SYOKBN
               END-IF         
               MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
               MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
               MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
               MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
               MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN 
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
               MOVE    RECE08-RECESYUBETU-SRT
                                           TO  RECE07-RECESYUBETU-SRT
               MOVE    RECE08-TERMID       TO  RECE07-TERMID
               MOVE    RECE08-OPID         TO  RECE07-OPID
               IF      WRK-PARA-SYOKBN     =   "2"
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "01"
                           MOVE    ZERO            TO  RECE07-SYORISBT
                       WHEN    "02"
                           MOVE    "1"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECE07-STYMD
                       WHEN    "03"
                           MOVE    "2"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  RECE07-STYMD
                           MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  RECE07-EDYMD
                       WHEN    "04"
                           MOVE    "3"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE07-STYMD
                   END-EVALUATE
               END-IF 
               IF      WRK-PARA-PRTKBN     =   "2"
                   MOVE    "1"                 TO  RECE07-PRTKBN
               END-IF
               MOVE    1                   TO  RECE07-PAGE
               MOVE    1                   TO  RECE07-KENSU
           ELSE
               ADD     1                   TO  RECE07-PAGE
               IF    ( CNT-KENSU           =   1 )
               AND   ( TBL-RECE-RENNUM (IDXY)
                                           <   3 )
                   ADD     1                   TO  RECE07-KENSU
               END-IF
           END-IF
      *
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE07          =   1
               MOVE    "DBINSERT"      TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           END-IF 
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0488 MCP-RC= "  MCP-RC   
               IF      FLG-RECE07         =   1    
               MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
      *********PERFORM 500-ERR-HENSYU-SEC
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
           .
       3903-RECE07-WRITE-EXT.
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
      *     MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦日付編集
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-HENYY
               MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-HENMM
               MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-HENDD
               MOVE    LNK-DAY2-EDTYMD1-G  TO  WRK-HENGENGO 
           ELSE
               MOVE    SPACE               TO  WRK-HENYMDG
           END-IF
           .
       31012-SE-WA-YMDEDIT-EXT.
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
           MOVE    2                   TO  FLG-STS
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM-FILE-ID DELIMITED   BY  SIZE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0488 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM         DELIMITED   BY  SIZE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
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
                   MOVE    2                   TO  FLG-STS
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    中止処理時データ削除処理
      *****************************************************************
       5001-DB-DELETE-SEC               SECTION.
      *
           IF      WRK-PARA-SYOKBN     =   "1"
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC23"              TO  RECE07-PRTID
               MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               PERFORM 900-RECE07-READ-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-REC          TO  WRK-RECE07-REC
                   MOVE    RECE07-REC          TO  MCPDATA-REC 
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                   "レセプト管理削除処理（一括分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月レセプト削除処理（一括分）
                   INITIALIZE                  RECE08-REC
grpsys             MOVE    WRK-RECE07-HOSPNUM  TO  RECE08-HOSPNUM
                   MOVE    WRK-RECE07-PRTID    TO  RECE08-PRTID
                   MOVE    WRK-RECE07-SRYYM    TO  RECE08-RECEYM
                   MOVE    WRK-RECE07-CREYMD   TO  RECE08-CREYMD
                   MOVE    WRK-RECE07-CREHMS   TO  RECE08-CREHMS
                   MOVE    WRK-RECE07-NYUGAIKBN
                                               TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receprt"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセプト削除処理（一括分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF  
           ELSE
               MOVE    TBL-SYS1001 (1)     TO  SYS-1001-REC
      *
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC23"              TO  RECE07-PRTID
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                   "レセプト管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
      *        レセプト削除処理（個別分）
               INITIALIZE                  RECE08-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
               MOVE    "HC23"              TO  RECE08-PRTID
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    RECE08-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                       "レセプト削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
           END-IF
           .
       5001-DB-DELETE-EXT.
           EXIT.    
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *          
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE41-FILE
           CLOSE                       RECE43-FILE
           CLOSE                       RECE47-FILE
           CLOSE                       RECE471-FILE
      *
           DISPLAY "*** ORCR0488 IN       "  CNT-RECE41
      *    DISPLAY "*** ORCR0488 RECE471= "  CNT-RECE471
           DISPLAY "*** ORCR0488 OUT      "  CNT-OUT
           DISPLAY "*** ORCR0488 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE41
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE41PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE41
                   IF    ( RECE41-PTNUM        =   SPACE  )
                   OR    ( RECE41-HKNKBN       =   "4"    )
                   OR    ( RECE41-RECESYUBETU  =   "0000" )
      *
                       IF    ( RECE41-TOTALKIN      =   ZERO )
                       AND   ( RECE41-TOTALKIN-YAK  =   ZERO )
                       AND   ( RECE41-TOTALKIN2     =   ZERO )
                       AND   ( RECE41-TOTALKIN2-ETC =   ZERO )
                               GO  TO  990-RECE41-READ-SEC
                       END-IF
      *
                       DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                       ADD     1                   TO  CNT-RECE41
                       MOVE    RECE41-HOSPNUM      TO  KEY-N-HOSPNUM
                       MOVE    RECE41-SRYYM        TO  KEY-N-SRYYM
                       MOVE    RECE41-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                       MOVE    RECE41-PTID         TO  KEY-N-PTID
                       MOVE    RECE41-RECEKA       TO  KEY-N-RECEKA
                       IF      WRK-PARA-SRYYM      =  ZERO
      *                    医療機関ＩＤ編集
                           MOVE  RECE41-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
                           MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
                   ELSE
                       GO  TO  990-RECE41-READ-SEC 
                   END-IF        
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYS-1901
                                           FLG-SYS-1902
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
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
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    医療機関情報（所在地ほか）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1002(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1002(SYSKAN-IDX) TO  SYS-1002-REC
               IF    ( SYS-1002-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1002-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING
              "医療機関情報（所在地）が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    帳票別指定医療機関編集情報
           IF      FLG-SYS-1900        =   1
               MOVE    ZERO                TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                       UNTIL ( SYSKAN-IDX              >  100  )
                       OR    ( TBL-SYS1900(SYSKAN-IDX) = SPACE )
                       OR    ( FLG-SET                 =   1   )
                   MOVE    TBL-SYS1900(SYSKAN-IDX) TO  SYS-1900-REC
                   IF    ( SYS-1900-STYUKYMD   <= WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <= SYS-1900-EDYUKYMD )
                           MOVE     1              TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET             =   1
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE43-NEXT-SEC           SECTION.
      *
           READ    RECE43-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE43
               NOT AT  END
                   IF      STS-RECE43          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE43PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE43      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE43
                   IF      RECE43-TENKIKBN     =   "8"
                       GO  TO  990-RECE43-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE43-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE47-NEXT-SEC           SECTION.
      *
           READ    RECE47-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE47
               NOT AT  END
                   IF      STS-RECE47          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE47PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE47      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE47
                   ADD     1         TO  CNT-RECE47
                   IF      RECE47-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE47-SRYKBN
                   END-IF    
                   IF      RECE47-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE47-SRYKBN
                   END-IF
                   IF      RECE47-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE47-SRYKBN
                   END-IF    
           END-READ
      *
           .
       990-RECE47-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE471-NEXT-SEC           SECTION.
      *
           READ    RECE471-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE471
               NOT AT  END
                   IF      STS-RECE471         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE471PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE471     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE471
      *             ADD     1         TO  CNT-RECE471
                   IF      RECE471-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE471-SRYKBN
                   END-IF    
                   IF      RECE471-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE471-SRYKBN
                   END-IF    
                   IF      RECE471-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE471-SRYKBN
                   END-IF
           END-READ
      *
           .
       990-RECE471-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-READ-SEC           SECTION.
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE08-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-NEXT-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    ＤＢ更新エラー処理
      *****************************************************************
       900-DB-ERR-ABORT-SEC                SECTION.
      *
           PERFORM 500-ERR-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
           STRING  "ORCR0488 db err PTNUM="
                                       DELIMITED   BY  SIZE
                   RECE41-PTNUM        DELIMITED   BY  SPACE
                   " TABLE="           DELIMITED   BY  SIZE
                   MCP-TABLE           DELIMITED   BY  SPACE
                   " MCP-FUNC="        DELIMITED   BY  SIZE
                   MCP-FUNC            DELIMITED   BY  SPACE
                   " PATHNAME="        DELIMITED   BY  SIZE
                   MCP-PATHNAME        DELIMITED   BY  SPACE
                   " RC="              DELIMITED   BY  SIZE
                   WRK-MCP-RC          DELIMITED   BY  SIZE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       900-DBINSERT-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0488 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
           .
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
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
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
      *    ＤＢ　オープン処理
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
      *    ＤＢ　クローズ処理
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
