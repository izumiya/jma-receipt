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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCR0720.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : レセプト印刷用ファイル作成
      *                      ２００６年１０月改正
      *  管理者            : 
      *  作成日付    作業者        記述 
      *  06/10/10    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.03.01     NACL-藤原    06/10/31  療養介護医療２４の追加
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.03    NACL-門脇    07/02/15  自立支援医療と生活保護の併用
      *                                     の点数欄記載
      *  03.04.04    NACL-門脇    07/02/16  帳票レイアウト変更による修正
      *  03.04.05    NACL-門脇    07/02/19  北海道５１＋４１対応
      *  03.04.06    NACL-藤原    07/02/27  請求管理へ保険ＩＤ、
      *                                     老人保険ＩＤ、公費ＩＤの追加
      *  03.04.07    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *  ２００７年４月改正
      *  03.04.08    NACL-門脇    07/04/01  現物給付対応
      *  03.04.09    NACL-藤原    07/04/13  症状詳記区分コード追加に伴う対応
      *  03.04.10    NACL-藤原    07/04/23  「区分」欄の記載の変更
      *  03.04.11    NACL-門脇    07/05/07  第２公費一部負担金記載条件修正
      *  03.04.12    NACL-藤原    07/09/04  宮崎県「公費９０」対応
      *                                     ２００７年９月以降のみ対象 
      *  03.04.13    NACL-藤原    07/09/11  新潟県中越沖地震対応
      *                                     ２００７年９月〜１２月対象 
      *  03.04.14    NACL-門脇    07/10/25  北海道５１（道単独事業）に関わる
      *                                     請求点記載対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    レセプト０２データ
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *
      *    レセプト０２データ（公費請求書）
           SELECT  RECE62X-FILE    ASSIGN  RECE62XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE62X-KEY
                                   FILE    STATUS  IS  STS-RECE62X.
      *    レセプト０３１データ
           SELECT  RECE631-FILE     ASSIGN  RECE63PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE631-KEY
                                   FILE    STATUS  IS  STS-RECE63.
      *    レセプト０６１データ
           SELECT  RECE661-FILE     ASSIGN  RECE66PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE661-KEY
                                   FILE    STATUS  IS  STS-RECE66.
      *    入退院履歴情報
           SELECT  RECE68-FILE     ASSIGN  RECE68PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE68-KEY
                                   FILE    STATUS  IS  STS-RECE68.
      *                             
           SELECT  RECE69-FILE     ASSIGN  RECE69PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE69-KEY
                                   FILE    STATUS  IS  STS-RECE69.
      *    レセプト（県単用項目）データ
           SELECT  RECE67-FILE     ASSIGN  RECE67PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE67-KEY
                                   FILE    STATUS  IS  STS-RECE67.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  WRK-PARA-ERRFILENM
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    レセプト０２データ
       FD  RECE62-FILE.
       01  RECE62-REC.
           COPY    "CPRCF062.INC".
      *
      *    レセプト０２データ（公費請求書）
       FD  RECE62X-FILE.
       01  RECE62X-REC. 
           COPY    "CPRCF062.INC"  REPLACING  //RECE62//
                                   BY         //RECE62X//.
      *
      *    レセプト０３１データ
       FD  RECE631-FILE.
       01  RECE631-REC.
           COPY    "CPRCF0631.INC".
      *
      *    レセプト０６１データ
       FD  RECE661-FILE.
       01  RECE661-REC. 
           COPY    "CPRCF0661.INC".
      *
       FD  RECE68-FILE.
       01  RECE68-REC. 
           COPY    "CPRCF068.INC".
      *
      *    レセプト０９データ
       FD  RECE69-FILE.
       01  RECE69-REC. 
           COPY    "CPRCF069.INC".
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
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE62//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE62X//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE63//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE66//.
      *
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE68//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE69//.
      *
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE67//.
      *
      *       
      ***  COPY    "HCN08.INC"     REPLACING  //HCN08//
           COPY    "HCN08V01.INC"  REPLACING  //HCN08V01//
                                   BY         //HC06//.
           COPY    "HCN061V03.INC" REPLACING  //HCN061V03//
                                   BY         //HC061//.
           COPY    "HCN062V03.INC" REPLACING  //HCN062V03//
                                   BY         //HC062//.
           COPY    "HCN063V02.INC" REPLACING  //HCN063V02//
                                   BY         //HC063//.
      *
      *    スパ領域
       01  SPA-AREA.
           03  SPA-STS                 PIC 9(04).
           03  SPA-ERRCD               PIC X(04).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE62          PIC X(02).
           03  STS-RECE62X         PIC X(02).
           03  STS-RECE63          PIC X(02).
           03  STS-RECE66          PIC X(02).
           03  STS-RECE68          PIC X(02).
           03  STS-RECE69          PIC X(02).
           03  STS-RECE67          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE62          PIC 9(01).
           03  FLG-RECE631         PIC 9(01).
           03  FLG-RECE661         PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECE68          PIC 9(01).
           03  FLG-RECE69          PIC 9(01).
           03  FLG-RECE67          PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-RECE-PUTH       PIC 9(01).
           03  FLG-RECE-PUTB       PIC 9(01).
      *
           03  FLG-KOHSKY          PIC 9(01).
           03  FLG-RECE62X         PIC 9(01).
           03  FLG-TENKI           PIC 9(01).
           03  FLG-SRYKBNPRT       PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-OPECOM-CHK      PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-SYS-2008        PIC 9(01).
           03  FLG-SHOKUJI-ETC     PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-LINE            PIC 9(01).
           03  FLG-JIRITU-HENSYU   PIC 9(01).
           03  FLG-RECE-PUT-UM     PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE62          PIC 9(06).
           03  CNT-RECE62X         PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE661         PIC 9(06).
           03  CNT-RECE69          PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDY1                PIC 9(03).
           03  IDY2                PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDY4                PIC 9(03).                           
           03  IDY5                PIC 9(03).
           03  IDXX                PIC 9(03).                           
           03  IDXY                PIC 9(03).
      *     
           03  SEIHIDX             PIC 9(02).
           03  KAKKIDX             PIC 9(02).
           03  SESNIDX             PIC 9(02).
           03  SYSKAN-IDX          PIC 9(03).
      *
      *    
       01  SUM-AREA                          VALUE   ZERO.          
           03  SUM-NYUKHNNISSU         PIC 9(03).
           03  SUM-NYUKHNKEI           PIC 9(07)     OCCURS  2.
      *
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE.          
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-HOSPID    PIC  X(24).
                   07  KEY-N-SRYYM     PIC  9(06).
                   07  KEY-N-NYUGAIKBN PIC  X(01).
                   07  KEY-N-PTID      PIC  9(10).
                   07  KEY-N-RECEKA    PIC  X(02).
               05  KEY-N-TEISYUTUSAKI  PIC  9(01).
               05  KEY-N-RECESYUBETU   PIC  9(04).
               05  KEY-N-HKNJANUM-KEY  PIC  X(08).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-HOSPID    PIC  X(24).
                   07  KEY-O-SRYYM     PIC  9(06).
                   07  KEY-O-NYUGAIKBN PIC  X(01).
                   07  KEY-O-PTID      PIC  9(10).
                   07  KEY-O-RECEKA    PIC  X(02).
               05  KEY-O-TEISYUTUSAKI  PIC  9(01).
               05  KEY-O-RECESYUBETU   PIC  9(04).
               05  KEY-O-HKNJANUM-KEY  PIC  X(08).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  10.
               05  TBL-SYS1001     PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC     OCCURS  10.
               05  TBL-SYS1002     PIC  X(610).
      *    帳票別指定医療機関編集情報退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC         OCCURS  10.
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
           03  WRK-PARA-ERRFILENM   PIC X(200).
      *
           03  WRK-SYS-SRYYMD       PIC  X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-RECENUM          PIC 9(06). 
      *
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-KYURATE-X.
               05  WRK-KYURATE-9   PIC 9(03).
           03  WRK-KYURATE         PIC Z9.
           03  WRK-BEDSU           PIC ZZZZ.
           03  WRK-HBTNUM-X.
               05  WRK-HBTNUM-9    PIC 9(03).
           03  WRK-RECE62-HBTNUM-X. 
               05  WRK-RECE62-HBTNUM-9
                                   PIC 9(02).
      *
           03  WRK-SYUBETU-SRT1    PIC X(04).
           03  WRK-RECE-PUTB-SYUBETU-SRT1 PIC 9(9).
           03  WRK-RECE-PUTB-SYUBETU-SRT1-R
                               REDEFINES   WRK-RECE-PUTB-SYUBETU-SRT1.
               05  FILLER          PIC X(01).
               05  WRK-RECE-PUTB-SYUBETU-SRT1-X
                                   PIC X(08).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-OPECOM-PATH     PIC X(64).
      *
           03  WRK-HSEQNUM         PIC 9(08).
      *
           03  WRK-HC06-HEAD4-G    PIC X(128).
      *
           03  WRK-SUM-TENSU       PIC S9(08).
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
      *    県単用項目ファイル用ワーク     
           COPY    "HCN06.INC"     REPLACING  //HCN06//
                                   BY         //RECE67-HC06//.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU        PIC X(03).
      *
           03  WRK-EDIT-AREA.
      *        傷病名連番     
               05  WRK-LINE                    PIC 9(02).
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
      *        適用欄編集行         
               05  WRK-LINETEK                 PIC 9(03).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE                PIC 9(03).
      *    適用欄行数
           03  WRK-TEKIYOU-LINE-GYO            PIC 9(03).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1                     PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2                     PIC X(01). 
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
               
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2                     PIC X(52).
           03  WRK-BYOMEI2-W   REDEFINES       WRK-BYOMEI2.
               05  WRK-BYOMEI2-NO              PIC X(08).
               05  WRK-BYOMEI2-NAME            PIC X(44).
               05  WRK-BYOMEI2-NAMEW   REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(20).
                   07  WRK-BYOMEI2-ELSE.
                       09  WRK-BYOMEI2-YMD     PIC X(20).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(14).
                   07  WRK-BYOMEI2R-ELSE.
                       09  WRK-BYOMEI2R-YMD    PIC X(20).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(06).
      *                 
      *    入退院履歴編集（適用欄）
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-RIREKI           PIC X(52).
               05  WRK-NYUTAI-RIREKI-W         REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  WRK-NYUTAI-MSG          PIC X(12).
                   07  WRK-NYUTAI-NYUINYMD     PIC X(18).
                   07  WRK-NYUTAI-FL1          PIC X(02).
                   07  WRK-NYUTAI-TAIINYMD     PIC X(18).
                   07  WRK-NYUTAI-FL2          PIC X(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *                 
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  50.  
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(04).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD.
                               13  WRK-HKN-TEKEDYY
                                               PIC X(08).
                               13  WRK-HKN-TEKEDMD
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
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-TNSIDX                      PIC 9(02).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
           03  WRK-R061-KOHBUNRI-O             PIC 9(01).
      *
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-KGKZ                        PIC ZZZ,ZZZ,ZZZ.
           03  WRK-KGK9                        PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KGK7                        PIC ZZZZ,ZZ9.
           03  WRK-KGK8                        PIC ZZZZZ,ZZ9.
           03  WRK-KGK6                        PIC ZZZ,ZZ9.
           03  WRK-KGK4                        PIC ZZZ9.
           03  WRK-KGK2                        PIC Z9.
           03  WRK-KGK10                       PIC ZZ,ZZZ,ZZ9.
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  16.
           03  WRK-COMM                        PIC X(800).
           03  WRK-COMM1                       PIC X(800).
           03  WRK-COL                         PIC 9(03). 
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
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SRYKACD         PIC X(04).
           03  WRK-SRYKANAME       PIC X(40).
      *                 
      *    食事基本料情報（その他）（摘要欄）
           03  WRK-SHOKUJIKHNINF-ETC-AREA.
               05  WRK-SHOKUJIKHNRYO-ETC-NM    PIC X(06).
               05  WRK-SHOKUJIKHNRYO-ETC       PIC X(08).
               05  WRK-SHOKUJIKHNRYO-ETC-FL    PIC X(04).
               05  WRK-SHOKUJIKHNNISSU-ETC     PIC X(04).
               05  WRK-SHOKUJIKHNNISSU-ETC-FL  PIC X(02).
      *
           03  WRK-HENSYU-KIN.
               05  WRK-HENSYU-KIN-9            PIC 9(04).
           03  WRK-HENSYU-DAY.
               05  WRK-HENSYU-DAY-9            PIC 9(02).
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
      *    編綴詳細設定用テーブル
       01  TABLE-RECE-PUTB-AREA.
      *    種別用 
           03  TABLE-SYUBETU-AREA  OCCURS  2.
               05  TBL-SYUBETU-RECE-PUTB-TBL    OCCURS  50.
                   07  TBL-SYUBETU-RECE-PUTB-SYUBETU
                                   PIC X(04).
                   07  TBL-SYUBETU-RECE-PUTB-REC
                                   PIC X(1000).
      *    公費用
           03  TABLE-KOH-AREA      OCCURS  2.
               05  TBL-KOH-RECE-PUTB-TBL       OCCURS  2000.
                   07  TBL-KOH-RECE-PUTB-HKNNUM
                                   PIC X(03).
                   07  TBL-KOH-RECE-PUTB-REC
                                   PIC X(1000).
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *    適用欄最大行数
           03  WRK-TEKIYOU1-LINE   PIC 9(02)   VALUE   35.
      *    適用欄（続紙）最大行数（横タイプ）
           03  WRK-TEKIYOU2-LINE   PIC 9(03)   VALUE   140.
      *    適用欄（続紙）最大行数（縦タイプ）
           03  WRK-TEKIYOU3-LINE   PIC 9(03)   VALUE   110.
      *    適用欄（続紙）最大行数（縦タイプ・長）
           03  WRK-TEKIYOU4-LINE   PIC 9(03)   VALUE   138.
      *    適用欄（続紙）の行別行数（横タイプ）
           03  WRK-TEKIYOU2-LINE-GYO
                                   PIC 9(03)   VALUE   35.
      *    適用欄（続紙）行別行数（縦タイプ）
           03  WRK-TEKIYOU3-LINE-GYO
                                   PIC 9(03)   VALUE   55.
      *
      *   特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER          PIC X(02)   VALUE   "01".
           03  FILLER          PIC X(10)   VALUE   "０１公".
           03  FILLER          PIC X(02)   VALUE   "02".
           03  FILLER          PIC X(10)   VALUE   "０２長".
           03  FILLER          PIC X(02)   VALUE   "03".
           03  FILLER          PIC X(10)   VALUE   "０３長処".
           03  FILLER          PIC X(02)   VALUE   "04".
           03  FILLER          PIC X(10)   VALUE   "０４老保".
           03  FILLER          PIC X(02)   VALUE   "07".
           03  FILLER          PIC X(10)   VALUE   "０７老併".
           03  FILLER          PIC X(02)   VALUE   "08".
           03  FILLER          PIC X(10)   VALUE   "０８老健".
           03  FILLER          PIC X(02)   VALUE   "09".
           03  FILLER          PIC X(10)   VALUE   "０９施".
           03  FILLER          PIC X(02)   VALUE   "10".
           03  FILLER          PIC X(10)   VALUE   "１０第三".
           03  FILLER          PIC X(02)   VALUE   "11".
           03  FILLER          PIC X(10)   VALUE   "１１薬治".
           03  FILLER          PIC X(02)   VALUE   "12".
           03  FILLER          PIC X(10)   VALUE   "１２器治".
           03  FILLER          PIC X(02)   VALUE   "13".
           03  FILLER          PIC X(10)   VALUE   "１３先進".
           03  FILLER          PIC X(02)   VALUE   "14".
           03  FILLER          PIC X(10)   VALUE   "１４制超".
           03  FILLER          PIC X(02)   VALUE   "15".
           03  FILLER          PIC X(10)   VALUE   "１５経過".
           03  FILLER          PIC X(02)   VALUE   "16".
           03  FILLER          PIC X(10)   VALUE   "１６長２".
           03  FILLER          PIC X(02)   VALUE   "17".
           03  FILLER          PIC X(10)   VALUE   "１７上位".
           03  FILLER          PIC X(02)   VALUE   "18".
           03  FILLER          PIC X(10)   VALUE   "１８一般".
           03  FILLER          PIC X(02)   VALUE   "19".
           03  FILLER          PIC X(10)   VALUE   "１９低所".
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   17.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
       01  TBL-TOKKI-MAX           PIC 9(02)   VALUE   17.
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状】".
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状の診察検査所見】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の必要性】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の経過】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の薬剤に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の処置に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【その他】".
           03  FILLER              PIC X(52)   VALUE
               "【薬事法に規定する治験に係る治験概要】".
           03  FILLER              PIC X(52)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(52)   OCCURS   9.
      *
      *   退職者保険番号
       01  WRK-RETIRE-AREA.
      *        組合
           03  WRK-RETIRE1         PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-RETIRE2         PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-RETIRE3         PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-RETIRE4         PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-RETIRE5         PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-RETIRE6         PIC X(03)   VALUE  "067".
      *
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "−".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(22)   VALUE
                                   "＝＝＝＝＝＝＝　以下　".
               05  WRK-KUGIRI2-KOHNAME         PIC X(10).
               05  FILLER          PIC X(20)   VALUE
                                   "　適用分　＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(52)   VALUE
               ALL "−".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTEN.
               05  WRK-KHNTENZ     PIC  ZZZZZZZ.
           03  WRK-KAISU.
               05  WRK-KAISUZ      PIC  ZZZ.
           03  WRK-KOUTEN.
               05  WRK-KOUTENZ     PIC  ZZZZZZZ.
           03  WRK-TENSU.
               05  WRK-TENSUZ      PIC  ZZZZZZZ.
           03  WRK-JINISSU.
               05  WRK-JINISSUZ    PIC  ZZ.
           03  WRK-JINISSU2.
               05  WRK-JINISSU2Z   PIC  Z9.
      *
       01  WRK-CNT                 PIC  9(5).
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
       01  WRK-ERR-MSG-AREA.
           03  WRK-ERR-MSG-1.
               05  WRK-ERR-HKNCOMBINUM-1
                                   PIC X(08).
               05  FILLER          PIC X(28)   VALUE
                   "の保険組合せが存在しません。". 
      *
           03  WRK-ERR-MSG-2.
               05  WRK-ERR-DAY-2   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-21.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-2
                                   PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用開始日が". 
                   07  WRK-ERR-TEKSTYMD-2
                                   PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-3.
               05  WRK-ERR-DAY-3   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-31.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-3
                                   PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用終了日が". 
                   07  WRK-ERR-TEKEDYMD-3
                                   PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-4.
               05  WRK-ERR-DAY-4   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-41.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-4
                                   PIC X(08).
                   07  FILLER          PIC X(28)   VALUE
                   "は適用年月日外となっています". 
      *
           03  WRK-ERR-MSG-5           PIC X(48)  VALUE
               "地方公費のみ選択されています。主保険がありません".
      *
           03  WRK-ERR-MSG-6           PIC X(14)  VALUE
               "【主科未設定】".
      *
           03  WRK-ERR-MSG-7           PIC X(30)  VALUE
              "【未請求設定の患者です】".
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID         PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *    改行コード
           03  WRK-CONS-RETURN         PIC X(01)   VALUE   X"0A".
      *
           03  WRK-CONS-SRYYM-200604  PIC 9(06)   VALUE   200604. 
      *
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.      
      *
           03  WRK-CONS-SRYYM-200703   PIC 9(06)   VALUE   200703.
           03  WRK-CONS-SRYYM-200709   PIC 9(06)   VALUE   200709.      
      *
           03  WRK-CONS-BODYLIST-MAX
                                   PIC 9(02)   VALUE   50.
           03  WRK-CONS-KOHLIST-MAX
                                   PIC 9(04)   VALUE   2000.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *    レセ電診療科コードテーブル
           COPY    "CMRECEDENSRYKACD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2005.INC".
      *
           COPY    "CPSK2008.INC".
      *
           COPY    "CPSK2009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
      *    
           COPY    "CPSK1005.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
           COPY    "CPRCF008.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    編綴管理レコード（管理部）
       01  RECE-PUTH-REC.
           COPY    "CPRECE-PUTH.INC".
      *
      *    編綴管理レコード（明細部）
       01  RECE-PUTB-REC.
           COPY    "CPRECE-PUTB.INC".
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
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    チェックディジット（モジュラス１１用）算出サブ
           COPY    "CPORCCHKDGT11.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
                   UNTIL       SPA-STS     =   2
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
                                       TABLE-AREA
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-ERRFILENM
      *
      *    ファイルＯＰＥＮ処理
           PERFORM 110-FILE-OPEN-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    "ORCR0720"      TO  JOB-PGID
               MOVE    "印刷データ編集"
                                       TO  JOB-SHELLMSG
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *
      *        処理中止設定処理
               IF      SPA-STS             =   ZERO
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
      *         
      *        ジョブ管理更新（中止フラグ非活性）処理
               IF      SPA-STS             =   ZERO
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
                   MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    3               TO  JOB-STOPFLG
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
               END-IF
           END-IF
      *
      *    システム管理マスタ（医療機関情報）
           IF      SPA-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( SPA-STS             =   ZERO      )
               PERFORM 5001-DB-DELETE-SEC
           END-IF
      *
           IF      SPA-STS             =   ZERO
              PERFORM 900-RECE62-READ-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
      *
           MOVE    "RECE62X"           TO  RECE62XPARA-FILE-ID
      *->  MOVE    WRK-CONS-TERMID     TO  RECE62XPARA-TERMID
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               MOVE    WRK-PARA-TERMID TO  RECE62XPARA-TERMID
           ELSE
               MOVE    WRK-CONS-TERMID TO  RECE62XPARA-TERMID
           END-IF
      *
           MOVE    "RECE631"           TO  RECE63PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE63PARA-TERMID
      *
           MOVE    "RECE661"           TO  RECE66PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE66PARA-TERMID
      *
           MOVE    "RECE680"           TO  RECE68PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE68PARA-TERMID
      *
           MOVE    "RECE690"           TO  RECE69PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE69PARA-TERMID
      *
           MOVE    "RECE670"           TO  RECE67PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE67PARA-TERMID
      *
           OPEN    INPUT               RECE62-FILE
           IF  STS-RECE62          NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE62 OPEN ERR  ***"
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE62X-FILE
           IF  STS-RECE62X         NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE62X OPEN ERR  ***"
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE631-FILE
           IF  STS-RECE63          NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE631 OPEN ERR  ***"
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE661-FILE
           IF  STS-RECE66          NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE661 OPEN ERR  ***"
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE69-FILE
           IF  STS-RECE69            NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE69 OPEN ERR  ***" STS-RECE69
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE68-FILE
           IF  STS-RECE68            NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE68 OPEN ERR  ***" STS-RECE68
               MOVE    2               TO  SPA-STS
           END-IF
      *
           OPEN    INPUT               RECE67-FILE
           IF  STS-RECE67            NOT =   ZERO
               DISPLAY "*** ORCR0720 RECE67 OPEN ERR  ***" STS-RECE67
               MOVE    2               TO  SPA-STS
           END-IF
           .
       110-FILE-OPEN-EXT.
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC   TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                        TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2               TO  SPA-STS
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
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              CALL    "MONFUNC"       USING
                                          MCPAREA
                                          MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL   FLG-KANRIMST  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  SPA-STS
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1002-REC
              ELSE    
             MOVE    "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  SPA-STS
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
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              CALL    "MONFUNC"       USING
                                          MCPAREA
                                          MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL   FLG-KANRIMST  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1002-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1002-REC  TO  TBL-SYS1002(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  SPA-STS
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-1900-REC
              ELSE    
                  INITIALIZE                   SYS-1900-REC
              END-IF
              IF      SYS-1900-PRTKBN (4)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (4)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (4)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
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
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "DBSELECT"          TO  MCP-FUNC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              CALL    "MONFUNC"       USING
                                          MCPAREA
                                          MCPDATA-REC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL   FLG-KANRIMST  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-PRTKBN (4)  =   SPACE
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
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF 
      *         
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-KANRIMST     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               INITIALIZE                   SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN
           END-IF
           IF      SYS-2005-TENKIHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
           IF      SYS-2005-NEXTPRTKBN      =   SPACE
               MOVE    ZERO             TO  SYS-2005-NEXTPRTKBN
           END-IF
           IF      SYS-2005-SYUBYOHENFLG    =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYUBYOHENFLG
           END-IF
           IF      SYS-2005-SRYKAHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-SRYKAHENKBN
           END-IF
           IF      SYS-2005-SYAOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-SYAOCRKBN
           END-IF
           IF      SYS-2005-KKHOCRKBN   =   SPACE
               MOVE    ZERO             TO  SYS-2005-KKHOCRKBN
           END-IF
           IF      SYS-2005-MOJI-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO  SYS-2005-MOJI-KISAIKBN
           END-IF
           IF      SYS-2005-KYURATE-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO  SYS-2005-KYURATE-KISAIKBN
           END-IF
           IF      SYS-2005-RECESYUBETU-KISAIKBN
                                        =   SPACE
               MOVE    ZERO             TO
                                        SYS-2005-RECESYUBETU-KISAIKBN
           END-IF
      *         
      *    レセプト編綴順情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2008"           TO  SYS-KANRICD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key13"             TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-KANRIMST     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2008-REC
           ELSE    
               INITIALIZE                   SYS-2008-REC
           END-IF
           IF      SYS-2008-PREFKBN-PRTJYUN
                                NOT =   SPACE
               MOVE    1                TO  FLG-SYS-2008
           END-IF
      *
      *    レセプト編綴順詳細設定情報（一括作成時のみ）
           IF   WRK-PARA-SRYYM    NOT =  ZERO
               PERFORM 1201-HENTETU-HENSYU-SEC
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2009"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-KANRIMST     =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE    
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
      * 
      *    県内扱い保険者情報
           MOVE    SPACE            TO  WRK-1033-TABLE
      *     
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "1033"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    SYSKANRI-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"   TO  MCP-TABLE
           MOVE    "key5"           TO  MCP-PATHNAME
           CALL    "MONFUNC"        USING
                                        MCPAREA
                                        MCPDATA-REC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-KANRIMST   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               IF      SYS-1033-EDYUKYMD   <   "20021001"
                   CONTINUE
               ELSE    
                   ADD     1              TO  IDX
                   MOVE    SYS-1033-STYUKYMD
                                          TO  WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD
                                          TO  WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL   TO  WRK-1033-HKNJANUM-G (IDX)
               END-IF    
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA          =   1
                   MOVE
                   "パラメタ情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  SPA-STS
               END-IF
           END-IF
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト編綴順詳細設定情報（一括作成時のみ）
      *****************************************************************
       1201-HENTETU-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  TABLE-RECE-PUTB-AREA
      *
      *    レセプト編綴順詳細設定情報（ＨＥＡＤ部）
           INITIALIZE                      RECE-PUTH-REC
           MOVE    SYS-1001-HOSPID     TO  RECE-PUTH-HOSPID
           MOVE    WRK-PARA-SRYYM      TO  RECE-PUTH-STYUKYMD (1:6)
           MOVE    "01"                TO  RECE-PUTH-STYUKYMD (7:2)
           MOVE    RECE-PUTH-STYUKYMD  TO  RECE-PUTH-EDYUKYMD
           MOVE    RECE-PUTH-REC       TO  MCPDATA-REC
           PERFORM 900-RECE-PUTH-READ-SEC
      *
      *    「使用しない」以外のときＨＥＡＤ部の情報を２００８に設定
           IF      RECE-PUTH-PREFKBN-PRTJYUN =   "3"
            OR     FLG-RECE-PUTH             =    1
               CONTINUE
           ELSE
               MOVE    RECE-PUTH-PREFKBN-PRTJYUN
                                           TO  SYS-2008-PREFKBN-PRTJYUN
               MOVE    RECE-PUTH-HKNJANUM-PRTJYUN
                                           TO  SYS-2008-HKNJANUM-PRTJYUN
               MOVE    RECE-PUTH-HKNJANUM-PRTKBN
                                           TO  SYS-2008-HKNJANUM-PRTKBN
               MOVE    RECE-PUTH-HKNJANUM
                                           TO  SYS-2008-HKNJANUM
      *
               MOVE    1                   TO  FLG-RECE-PUT-UM
      *
      *        レセプト編綴順詳細設定情報（ＢＯＤＹ部）
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2
                   INITIALIZE                      RECE-PUTB-REC
                   MOVE    SYS-1001-HOSPID     TO  RECE-PUTB-HOSPID
                   EVALUATE    IDXX
                       WHEN    1
                           MOVE    "1"         TO  RECE-PUTB-PREFKBN
                       WHEN    2
                           MOVE    "2"         TO  RECE-PUTB-PREFKBN
                   END-EVALUATE
                   MOVE    WRK-PARA-SRYYM  TO  RECE-PUTB-STYUKYMD (1:6)
                   MOVE    "01"            TO  RECE-PUTB-STYUKYMD (7:2)
                   MOVE    RECE-PUTB-STYUKYMD
                                           TO  RECE-PUTB-EDYUKYMD
                   MOVE    RECE-PUTB-REC   TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   CALL    "MONFUNC"       USING
                                               MCPAREA
                                               MCPDATA-REC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_rece_putb" TO  MCP-TABLE
                       MOVE    "key4"          TO  MCP-PATHNAME
                       PERFORM 900-RECE-PUTB-READ-N-SEC
                   ELSE
                       INITIALIZE                  RECE-PUTB-REC
                       MOVE    1               TO  FLG-RECE-PUTB
                   END-IF
      *
                   MOVE    ZERO            TO  IDX
                                               IDY
                   PERFORM     UNTIL   FLG-RECE-PUTB
                                           =   1
      ***                         OR      IDX >=  WRK-CONS-BODYLIST-MAX
      ***                         OR      IDY >=  WRK-CONS-KOHLIST-MAX
      *
                       EVALUATE    RECE-PUTB-KOUHI-NO
                           WHEN    ZERO
                               ADD     1               TO  IDX
                               MOVE    RECE-PUTB-REC   TO
                                       TBL-SYUBETU-RECE-PUTB-REC
                                                             (IDXX IDX)
                               MOVE    RECE-PUTB-SYUBETU-NYU
                                                       TO
                                       TBL-SYUBETU-RECE-PUTB-SYUBETU
                                                             (IDXX IDX)
                           WHEN    OTHER
                               ADD     1               TO  IDY
                               MOVE    RECE-PUTB-REC   TO
                                       TBL-KOH-RECE-PUTB-REC (IDXX IDY)
                               MOVE    RECE-PUTB-KOUHI-NUM
                                                       TO
                                       TBL-KOH-RECE-PUTB-HKNNUM
                                                             (IDXX IDY)
                       END-EVALUATE
      *
                       MOVE    "tbl_rece_putb" TO  MCP-TABLE
                       MOVE    "key4"          TO  MCP-PATHNAME
                       PERFORM 900-RECE-PUTB-READ-N-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_rece_putb" TO  MCP-TABLE
                   MOVE    "key4"          TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
           END-IF    
      *
           .
       1201-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE62     =   1
                           OR      SPA-STS        =   2 
      *        患者番号別保険履歴情報編集処理      
               PERFORM 2001-RIREKI-HEN-SEC 
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE62     =   1
                               OR      SPA-STS        =   2
                               OR      KEY-NEW1   NOT =   KEY-OLD1
      *            レセプト種別別件数カウント
                   MOVE    ZERO            TO  CNT-KENSU
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-RECE62     =   1
                                   OR      SPA-STS        =   2
                                   OR      KEY-NEW    NOT =   KEY-OLD
      *
                       ADD     1                      TO  CNT-KENSU
      *                                      
                       PERFORM 2001-SYORI-SEC
      *
                       PERFORM 900-RECE62-READ-SEC
                   END-PERFORM
               END-PERFORM    
           END-PERFORM
      *
      *    公費請求書出力
           IF      SPA-STS         =   2
               CONTINUE
           ELSE
               PERFORM 2002-KOHSKY-WRITE-SEC
           END-IF 
      *
           MOVE    2               TO  SPA-STS
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE62-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE62-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE69
      *     
           INITIALIZE                      RECE69-REC
           MOVE    RECE62-HOSPID       TO  RECE69-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE69-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE69-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE69-PTID
           MOVE    RECE62-RECEKA       TO  RECE69-RECEKA
      *
           START   RECE69-FILE
               KEY IS    >=   RECE69-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE69
               NOT INVALID
                   PERFORM 900-RECE69-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE69           =   1                )
                   OR    ( RECE62-HOSPID    NOT =   RECE69-HOSPID    )
                   OR    ( RECE62-SRYYM     NOT =   RECE69-SRYYM     )
                   OR    ( RECE62-NYUGAIKBN NOT =   RECE69-NYUGAIKBN )
                   OR    ( RECE62-PTID      NOT =   RECE69-PTID      )
                   OR    ( RECE62-RECEKA    NOT =   RECE69-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE69
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   50
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  SPA-STS
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE69-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
               END-EVALUATE          
               IF    ( RECE69-NUM         =   SPACE )
               AND   ( RECE69-KIGO        =   SPACE )        
               AND   ( RECE69-HKNJANUM    =   SPACE )        
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE69-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  WRK-HKN-HKNJANUM
                   IF      RECE69-HKNJANUM     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-HKNJANUM TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-HKNJANUM
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-KIGO
                   IF      RECE69-KIGO     NOT =   SPACE           
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  WRK-HKN-NUM 
                   IF      RECE69-NUM      NOT =   SPACE    
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE69-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF         
                   MOVE    SPACE                  TO  WRK-HKN-RIREKI-X
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE69-TANSEIDONAME    DELIMITED  BY  SPACE
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
                               RECE69-TANSEIDONAME
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
                           MOVE    2               TO  SPA-STS
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
                       MOVE    2               TO  SPA-STS
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF    
               END-IF                             
      *
               MOVE    RECE69-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *********IF      RECE69-TEKSTYMDX(1:6)  =   RECE62-SRYYM                      
      *            MOVE    LNK-DAY2-EDTYMD3(11:12)    TO  
      *                           WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(9:)
      *********ELSE
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
      *********END-IF                                    
      *  
               IF      RECE69-TEKEDYMDX(1:6)  =   RECE62-SRYYM                      
                   MOVE    RECE69-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
      *********    MOVE    LNK-DAY2-EDTYMD3(11:12)    TO
      *********                       WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)(3:)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)                                     
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX) 
               END-IF                                    
      *
               PERFORM 900-RECE69-NEXT-SEC
           END-PERFORM
      *
           IF      CNT-RECE69             =   1
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF          
      *
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1                  TO  CNT-PAGE
           MOVE    WRK-TEKIYOU1-LINE  TO  WRK-TEKIYOU-LINE
           INITIALIZE                     TABLE-AREA
      *
      *    医療保険・公費のフラグ
           MOVE    SPACE              TO  WRK-3SYU-KBN
           MOVE    ZERO               TO  SEIHIDX
           MOVE    ZERO               TO  KAKKIDX
           MOVE    ZERO               TO  SESNIDX
           PERFORM   VARYING   IDX  FROM    1   BY  1
                     UNTIL   ( IDX                >    4    )
                     OR      ( RECE62-KOHID(IDX)  =   SPACE )
      *        生保
               IF  RECE62-KOHNUM (IDX)  =   "012"
                   MOVE    "1"          TO  WRK-3SYU (1:1)
                   MOVE     IDX         TO  SEIHIDX
               END-IF
      *        結核
               IF  RECE62-KOHNUM (IDX)  =   "010"  OR  "011"
                   MOVE    "1"          TO  WRK-3SYU (2:1)
                   MOVE     IDX         TO  KAKKIDX
               END-IF
      *
               IF      RECE62-SRYYM     <   WRK-CONS-SRYYM-200604
      *            精神
                   IF  RECE62-KOHNUM (IDX)  =   "020"  OR  "021"
                       MOVE    "1"          TO  WRK-3SYU (3:1)
                       MOVE     IDX         TO  SESNIDX
                   END-IF
               ELSE
      *            自立支援法
                   IF  RECE62-KOHNUM (IDX)  =   "021"  OR
                                                "015"  OR  "016"  OR
                                                "024"
                       MOVE    "1"          TO  WRK-3SYU (3:1)
                       MOVE     IDX         TO  SESNIDX
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE     ZERO       TO    FLG-JIRITU-HENSYU
           IF (( RECE62-SRYYM      >=  WRK-CONS-SRYYM-200604 ) AND
               ( RECE62-HKNNUM     =   SPACE                 ) AND
               ( RECE62-KOHNUM (1) =   "021"                 ) AND
               ( RECE62-KOHNUM (2) =   "012"                 ))
           OR (( RECE62-SRYYM      >=  WRK-CONS-SRYYM-200703 ) AND
               ( RECE62-HKNNUM     =   SPACE                 ) AND
               ( RECE62-KOHNUM (1) =   "015" OR "016"        ) AND
               ( RECE62-KOHNUM (2) =   "012"                 ))
                    MOVE       1       TO    FLG-JIRITU-HENSYU
           END-IF
      *
      *    県単用項目ファイル読込処理
           PERFORM 301-RECE67-HENSYU-SEC
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HCN06-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<補足>処理
           PERFORM 350-HC101-HOSOKU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *    帳票編集<入退院履歴・入院基本料種別の続き>処理
           PERFORM 380-HC101-NYUTAI-RIREKI-HEN-SEC
      *
      *    帳票編集<手術歴>処理
           PERFORM 381-HC101-OPECOM-HEN-SEC 
      *
      *    帳票編集<剤>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<コメント>処理
           PERFORM 360-HC101-COMMENT-HEN-SEC 
      *
      *    帳票編集<保険履歴情報>処理
           PERFORM 370-HC101-HKN-RIREKI-HEN-SEC 
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書出力処理
      *****************************************************************
       2002-KOHSKY-WRITE-SEC           SECTION.
      *
           PERFORM 990-RECE62X-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE62X =   1
                               OR      FLG-ERR     =   1
                               OR      SPA-STS     =   2
      * 
               INITIALIZE                     TABLE-AREA
      *
               IF      RECE62X-RECESYUBETU     =   ZERO
                   IF      RECE62X-HKNNUM      NOT =   SPACE 
                       CONTINUE
                   ELSE
                       PERFORM 20021-KOHSKY-FUMEI-HENSYU-SEC
                   END-IF
               ELSE
                   PERFORM 20022-KOHSKY-WRITE-SEC
               END-IF 
      *
               PERFORM 990-RECE62X-READ-SEC
           END-PERFORM
      *
           .
       2002-KOHSKY-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書不明分レセプト出力処理
      *****************************************************************
       20021-KOHSKY-FUMEI-HENSYU-SEC           SECTION.
      *
           IF   WRK-PARA-SRYYM         =   ZERO
      *        医療機関編集
               MOVE  RECE62X-SRYYM         TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
               PERFORM 900-SYSKANRI-HENSYU-SEC
           END-IF
      *
           MOVE    RECE62X-REC         TO  RECE62-REC 
      *
           MOVE    1                   TO  CNT-PAGE
           MOVE    1                   TO  FLG-RECE67
      *
           MOVE    SPACE               TO  WRK-3SYU-KBN
           MOVE    ZERO                TO  SEIHIDX
           MOVE    ZERO                TO  KAKKIDX
           MOVE    ZERO                TO  SESNIDX
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HCN06-HEAD-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       20021-KOHSKY-FUMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書編集処理
      *****************************************************************
       20022-KOHSKY-WRITE-SEC          SECTION.
      *
           MOVE    SPACE               TO  KOHSKY-REC
           INITIALIZE                      KOHSKY-REC
           MOVE    RECE62X-HOSPID      TO  KOHSKY-HOSPID
           MOVE    RECE62X-SRYYM       TO  KOHSKY-SRYYM
           MOVE    RECE62X-NYUGAIKBN   TO  KOHSKY-NYUGAIKBN
           MOVE    RECE62X-PTID        TO  KOHSKY-PTID
           IF      RECE62X-RECEKA      =   SPACE
               MOVE    ZERO                TO  KOHSKY-RECEKA
           ELSE
               MOVE    RECE62X-RECEKA      TO  KOHSKY-RECEKA
           END-IF
           MOVE    RECE62X-TEISYUTUSAKI    TO  KOHSKY-TEISYUTUSAKI
           IF      RECE62X-HKNJANUM-KEY    =   SPACE
               MOVE    ZERO            TO  KOHSKY-HKNJANUM-KEY
           ELSE
               MOVE    RECE62X-HKNJANUM-KEY
                                       TO  KOHSKY-HKNJANUM-KEY
           END-IF
           IF      RECE62X-HOJOKBN-KEY    =   SPACE
               MOVE    ZERO            TO  KOHSKY-HOJOKBN-KEY
           ELSE
               MOVE    RECE62X-HOJOKBN-KEY
                                       TO  KOHSKY-HOJOKBN-KEY
           END-IF
           IF      RECE62X-TEKSTYMD    =   SPACE
               MOVE    ZERO                TO  KOHSKY-TEKSTYMD
           ELSE
               MOVE    RECE62X-TEKSTYMD    TO  KOHSKY-TEKSTYMD
           END-IF
      *
      *    レセプト種別を請求管理より編集
           INITIALIZE                      RECE10-REC
           MOVE    KOHSKY-KEY          TO  RECE10-KEY
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key43"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    RECE10-RECESYUBETU  TO  KOHSKY-RECESYUBETU
               MOVE    RECE10-RECEKA       TO  KOHSKY-RECEKA
           ELSE
               GO  TO  2002-KOHSKY-WRITE-EXT
      *        STRING  "レセプト種別が設定不能 PTNUM="
      *                                 DELIMITED  BY  SIZE
      *                 RECE62X-PTNUM       DELIMITED  BY  SIZE 
      *                                    INTO  WRK-RECEERR
      *        END-STRING
      *        PERFORM 500-ERR-HENSYU-SEC
      *        MOVE    1                  TO  FLG-ERR
      *********GO  TO  2002-KOHSKY-WRITE-EXT
           END-IF
      *
           PERFORM 900-KOHSKY-INV-SEC
           IF      FLG-KOHSKY          =   ZERO
               MOVE    MCPDATA-REC         TO  KOHSKY-REC
               MOVE    WRK-PARA-SYSYMD     TO  KOHSKY-UPDYMD
               IF      KOHSKY-SKYKBN       =   "2"
                   MOVE    "1"                 TO  KOHSKY-SKYKBN
               END-IF 
               MOVE    "2"                 TO  KOHSKY-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  KOHSKY-SKYKBN
               MOVE    WRK-PARA-SYSYMD     TO  KOHSKY-CREYMD
               MOVE    ZERO                TO  KOHSKY-UPDYMD
               MOVE    ZERO                TO  KOHSKY-SKYYM
                                               KOHSKY-HENREIYM
                                               KOHSKY-TEISEIKBN
           END-IF
           MOVE    RECE62X-PTNUM       TO  KOHSKY-PTNUM
           MOVE    RECE62X-KANANAME    TO  KOHSKY-KANANAME
           MOVE    RECE62X-NAME        TO  KOHSKY-NAME
           MOVE    RECE62X-SEX         TO  KOHSKY-SEX
           MOVE    RECE62X-BIRTHDAY    TO  KOHSKY-BIRTHDAY
           MOVE    RECE62X-AGE         TO  KOHSKY-AGE  
           MOVE    RECE62X-DEATHKBN    TO  KOHSKY-DEATHKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               MOVE    RECE62X-SRYKA   (IDX)
                                       TO  KOHSKY-SRYKA (IDX)
           END-PERFORM
           MOVE    RECE62X-HKN         TO  KOHSKY-HKN
           MOVE    RECE62X-KYURATE     TO  KOHSKY-KYURATE
           MOVE    RECE62X-RJNHKN      TO  KOHSKY-RJNHKN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    RECE62X-KOHID       (IDX) 
                                       TO  KOHSKY-KOHID      (IDX)
               MOVE    RECE62X-KOHNUM      (IDX)
                                       TO  KOHSKY-KOHNUM     (IDX)
               MOVE    RECE62X-KOHHBTNUM   (IDX)
                                       TO  KOHSKY-KOHHBTNUM  (IDX)
               MOVE    RECE62X-KOHFTNJANUM (IDX)
                                       TO  KOHSKY-KOHFTNJANUM(IDX)
               MOVE    RECE62X-KOHJKYSNUM  (IDX)
                                       TO  KOHSKY-KOHJKYSNUM (IDX)
           END-PERFORM
      ***  PERFORM VARYING IDX FROM    1   BY  1
      ***          UNTIL   IDX >       10
      ***      MOVE   RECE62X-HKNCOMBIINF (IDX)
      ***                              TO KOHSKY-HKNCOMBIINF (IDX)
      ***  END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE   RECE62X-HKNCOMBI(IDX)   TO  KOHSKY-HKNCOMBI(IDX)
               MOVE   RECE62X-HKNNUMHC(IDX)   TO  KOHSKY-HKNNUMHC(IDX)
               MOVE   RECE62X-KOHNUMHC(IDX 1) TO  KOHSKY-KOHNUMHC(IDX 1)
               MOVE   RECE62X-KOHNUMHC(IDX 2) TO  KOHSKY-KOHNUMHC(IDX 2)
               MOVE   RECE62X-KOHNUMHC(IDX 3) TO  KOHSKY-KOHNUMHC(IDX 3)
               MOVE   RECE62X-KOHNUMHC(IDX 4) TO  KOHSKY-KOHNUMHC(IDX 4)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                    UNTIL   IDX >       30
               MOVE    RECE62X-SRYKBNINF (IDX)
                                       TO  KOHSKY-SRYKBNINF (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE62X-JNISSUINF (IDX)
                                       TO  KOHSKY-JNISSUINF (IDX)
               MOVE    RECE62X-TOTALTENINF (IDX)
                                       TO  KOHSKY-TOTALTENINF (IDX)
      *********MOVE    RECE62X-FTNMONEYINF(IDX)
      *********                        TO  KOHSKY-FTNMONEYINF (IDX)
               MOVE    RECE62X-FTNMONEY   (IDX)
                                       TO  KOHSKY-FTNMONEY    (IDX)
               MOVE    RECE62X-SHOKUJIINF (IDX)
                                       TO  KOHSKY-SHOKUJIINF (IDX)
           END-PERFORM
           MOVE    RECE62X-SHOSHINTIME TO  KOHSKY-SHOSHINTIME
      **** MOVE    RECE62X-KOUREI      TO  KOHSKY-ZAITAKU
           IF      RECE62X-KOUREI   =  "2" OR "3"
             EVALUATE  RECE62X-DISCOUNTTENTNK
               WHEN       "17"
                 MOVE   "2"     TO   KOHSKY-ZAITAKU
               WHEN       "18"
                 MOVE   "3"     TO   KOHSKY-ZAITAKU
               WHEN       "19"
                 MOVE   "4"     TO   KOHSKY-ZAITAKU
             END-EVALUATE
           END-IF
           MOVE    RECE62X-KOUREI-SAIKEI
                                       TO  KOHSKY-ZAITAKU-SAIKEI
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE62X-SHOKUJIFTNKBN
                                       TO  KOHSKY-SHOKUJIFTNKBN
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "KOHSKY-KEY"        TO  ORC-DBPATH
           IF      FLG-KOHSKY          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                           MCPDATA-REC      
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0720 KOHSKY MCP-RC= "  MCP-RC   
               IF      FLG-KOHSKY          =   1    
                   MOVE    "公費請求ＤＢに追加できませんでした"
                                           TO  WRK-RECEERR
               ELSE
                   MOVE    "公費請求ＤＢに更新できませんでした"
                                           TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
      *
           .
       2002-KOHSKY-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    県単用項目ファイル読込処理
      *****************************************************************
       301-RECE67-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  RECE67-HC06
      *       
           INITIALIZE                      RECE67-REC
           MOVE    RECE62-KEY          TO  RECE67-KEY
           PERFORM 900-RECE67-READ-SEC
           IF      FLG-RECE67          =   ZERO
               MOVE    RECE67-PRTDATA      TO  RECE67-HC06
      *        カスタマイズ項目があるときＯＣＲに反映させる 
               IF ( ( RECE62-TEISYUTUSAKI   =   1           ) AND
                    ( SYS-2005-SYAOCRKBN    =   "2"         )     ) 
               OR ( ( RECE62-TEISYUTUSAKI   =   2           ) AND
                    ( SYS-2005-KKHOCRKBN    =   "2" OR  "3" )     ) 
                   MOVE    RECE62-OCR1            TO  OCR1-AREA
                   MOVE    RECE62-OCR2            TO  OCR2-AREA
                   MOVE    RECE62-OCR3            TO  OCR3-AREA
               END-IF  
           END-IF    
           .
       301-RECE67-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HCN06-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HC06
           MOVE    SPACE               TO  HC06
      *
      *    特別療養費の場合「特別療養費」文字記載をシス管の設定により行う
           IF      RECE62-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-MOJI-KISAIKBN
                   WHEN    ZERO
                       MOVE    "特別療養費"    TO  HC06-MOJI1
                   WHEN    "1"
                       MOVE    "特別療養費"    TO  HC06-MOJI2
               END-EVALUATE
           END-IF
      *
      *    患者番号
           MOVE    RECE62-PTNUM        TO  HC06-PTNUM
      *
      *    提出先
           IF      RECE62-RECESYUBETU  =    "0000"  OR  "8888"
               MOVE    SPACE           TO  HC06-TEISYUTUSAKI
           ELSE    
               EVALUATE    RECE62-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"          TO  HC06-TEISYUTUSAKI
                   WHEN    2
                       MOVE    "２国"          TO  HC06-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  HC06-TEISYUTUSAKI
               END-EVALUATE
           END-IF    
      *
      *    診療年月（診療年月日として退避）
           MOVE    RECE62-SRYYM        TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           MOVE    WRK-SYMD            TO  WRK-SRYYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HC06-SRYY 
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HC06-SRYM 
      *
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  HC06-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  HC06-HOSPCDN
      *
      *    保険種別１
           PERFORM 3110-HKNSBT1-SET-SEC
      *
      *    保険種別２
           PERFORM 3110-HKNSBT2-SET-SEC
      *
      *    本人・家族
           PERFORM 3110-HONKZK-SET-SEC
      *
      *    特別療養費の場合シス管の設定により
           IF      RECE62-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-RECESYUBETU-KISAIKBN
                   WHEN    ZERO
                       CONTINUE
                   WHEN    "1"
                       MOVE    SPACE       TO  HC06-HKNSBT1
                                               HC06-HKNSBT2
                                               HC06-HONKZK
               END-EVALUATE
           END-IF
      *
      *    老人保険−市町村番号
           MOVE    RECE62-RJNFTNJANUM  TO  HC06-RJNFTNJANUM
      *
      *    老人保険−受給者番号
           MOVE    RECE62-RJNJKYSNUM   TO  HC06-RJNJKYSNUM
      *
      *    保険者番号・公費−負担者番号・受給者番号・診療実日数・記号番号
           PERFORM 3110-KANREN-NUM-SET-SEC
      *
      *    給付割合
           PERFORM 3110-KYURATE-SET-SEC
      *
      *    カナ氏名
           IF     RECE62-KANANAME (41:)    NOT =   SPACE
                MOVE    RECE62-KANANAME    TO  HC06-KANANAME2
           ELSE
                MOVE    RECE62-KANANAME     TO  HC06-KANANAME
           END-IF
      *
      *    氏名
           MOVE    RECE62-NAME         TO  HC06-NAME
      *
      *    性別
           EVALUATE    RECE62-SEX
           WHEN    "1"
               MOVE    "１"            TO  HC06-SEXNUM
               MOVE    "男"            TO  HC06-SEXN
           WHEN    "2"
               MOVE    "２"            TO  HC06-SEXNUM
               MOVE    "女"            TO  HC06-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE    RECE62-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI 
           MOVE    LNK-DAY1-GENGO  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  HC06-BIRTHDAY(1:)
           END-IF    
           MOVE    RECE62-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2 (1:2)
                                       TO  HC06-BIRTHDAY(3:2)
           MOVE    LNK-DAY2-EDTYMD2 (5:)
                                       TO  HC06-BIRTHDAY(5:)
      *
      *    特記事項
           PERFORM 3110-TOKKI-SET-SEC
      *
      *    病棟区分 
           PERFORM 3110-BYOTOKBN-SET-SEC
      *
      *    職務上の事由
           EVALUATE    RECE62-HKNNUM
               WHEN    "002"
               WHEN    "031"  THRU  "034"
                   EVALUATE    RECE62-HOJOKBN
                       WHEN    "1"
                       WHEN    "A"
                       WHEN    "D"
                           MOVE    "職務上"              TO  HC06-JIYU
                       WHEN    "2"
                       WHEN    "B"
                       WHEN    "E"
                           MOVE    "下船後３ヶ月以内"    TO  HC06-JIYU
                       WHEN    "3"
                       WHEN    "C"
                       WHEN    "F"
                           MOVE    "通勤災害"            TO  HC06-JIYU
                   END-EVALUATE
           END-EVALUATE        
      *
      *    医療機関所在地
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-ADRS1      TO  HC06-HOSPADRS (1:)
               MOVE    SYS-1902-ADRS2      TO  HC06-HOSPADRS (51:)
           ELSE
               MOVE    SYS-1002-ADRS       TO  HC06-HOSPADRS
           END-IF
      *
      *    医療機関名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC06-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC06-HOSPNAME (41:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC06-HOSPNAME
           END-IF
      *
      *    医療機関ＴＥＬ
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-TEL        TO  HC06-HOSPTEL
           ELSE
               MOVE    SYS-1002-TEL        TO  HC06-HOSPTEL
           END-IF
      *
      *     診療科
      *
      *    主科設定ありのときの記載
           IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                 AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
           OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                 AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               IF      RECE62-SYUKA      NOT =   "XX"
                   IF      RECE62-RECESYUKA NOT =   SPACE
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"                 TO  KANACHK-SYORI 
                       MOVE    RECE62-RECESYUKA    TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           STRING  "（"            DELIMITED   BY  SIZE
                                   KANACHK-OUT-INPUT(1:KANACHK-MAX)    
                                                   DELIMITED   BY  SIZE
                                   RECE62-SYUKANAME
                                                   DELIMITED   BY  SPACE 
                                   "）"            DELIMITED   BY  SIZE
                                                   INTO  HC06-SRYKA
                           END-STRING
                       END-IF
                   ELSE
                       STRING  "（"            DELIMITED   BY  SIZE
                               RECE62-SYUKANAME
                                               DELIMITED   BY  SPACE 
                               "）"            DELIMITED   BY  SIZE
                                               INTO  HC06-SRYKA
                       END-STRING
                   END-IF
               END-IF 
           ELSE
      *        診療科（シス管の設定により編集を行なう）
               MOVE    SPACE               TO  WRK-SRYKANAME
                                               WRK-SRYKACD
               IF      SYS-2005-SRYKAHENKBN
                                           =   ZERO
                   MOVE    SPACE               TO  SYSKANRI-REC
                   INITIALIZE                      SYSKANRI-REC
                   MOVE    "1005"              TO  SYS-KANRICD
                   MOVE    RECE62-LASTSRYKA    TO  SYS-KBNCD
                   MOVE    RECE62-SRYYM        TO  SYS-STYUKYMD(1:6)
                   MOVE    "01"                TO  SYS-STYUKYMD(7:2)
                   MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
                   MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
                   MOVE    "key10"             TO  WRK-MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
                   IF      FLG-KANRIMST    =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1005-REC
                   ELSE
                       INITIALIZE                      SYS-1005-REC
                   END-IF
      *
                   IF      SYS-1001-HOSPSBT1   =   1
      *                旧総合病院のときはレセ電診療科を記載
                       SET     TBL-RECEDENSRYKACD-IDX  TO  1
                       SEARCH  TBL-RECEDENSRYKACD-OCC  
                                       VARYING TBL-RECEDENSRYKACD-IDX
                           AT  END
                               MOVE    SPACE       TO  WRK-SRYKANAME
                           WHEN    SYS-1005-RECESRYKA
                                           =   TBL-RECEDEN-SRYKACD
                                               (TBL-RECEDENSRYKACD-IDX)
                               MOVE    TBL-RECEDEN-SRYKANAME
                                               (TBL-RECEDENSRYKACD-IDX)
                                                   TO  WRK-SRYKANAME
                       END-SEARCH
                   ELSE
                       MOVE    SYS-1005-SRYKANAME  TO  WRK-SRYKANAME
                   END-IF
      *
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"                 TO  KANACHK-SYORI 
                   MOVE    SYS-1005-RECESRYKA  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      KANACHK-RC          =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)    
                                                   TO  WRK-SRYKACD
                   END-IF
      *    
                   STRING  "（"                DELIMITED   BY  SIZE
                           WRK-SRYKACD         DELIMITED   BY  SPACE
                           WRK-SRYKANAME       DELIMITED   BY  SPACE
                           "）"                DELIMITED   BY  SIZE
                                               INTO  HC06-SRYKA
                   END-STRING
               END-IF
           END-IF    
      *    県単用項目
           IF      FLG-RECE67         =   ZERO
               IF      RECE67-HC06-SRYKA  NOT =   SPACE
                   MOVE    RECE67-HC06-SRYKA      TO  HC06-SRYKA
               END-IF
           END-IF    
      *
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE62-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-BEDSU
                   MOVE    WRK-BEDSU           TO  HC06-BEDSU
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-BEDSU
                       MOVE    WRK-BEDSU           TO  HC06-BEDSU
                   END-IF
               END-IF
           END-IF
      *
      *    ＯＣＲ
           IF      CNT-PAGE                =   1
               IF ( ( RECE62-TEISYUTUSAKI   =   1           ) AND
                    ( SYS-2005-SYAOCRKBN    =   "2"         )     ) 
               OR ( ( RECE62-TEISYUTUSAKI   =   2           ) AND
                    ( SYS-2005-KKHOCRKBN    =   "2" OR  "3" )     ) 
      *            レセプト特記事項編集情報に該当項目があるとき
      *            ＯＣＲに反映させる 
                   IF    ( RECE62-2006-FTNJANUMKBN (1)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-FTNJANUMKBN (2)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-FTNJANUMKBN (3)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-FTNJANUMKBN (4)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-JKYSNUMKBN  (1)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-JKYSNUMKBN  (2)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-JKYSNUMKBN  (3)
                                           NOT =   SPACE )
                   OR    ( RECE62-2006-JKYSNUMKBN  (4)
                                           NOT =   SPACE )
                       MOVE    RECE62-OCR1            TO  OCR1-AREA
                       MOVE    RECE62-OCR2            TO  OCR2-AREA
                       MOVE    RECE62-OCR3            TO  OCR3-AREA
                       PERFORM 3302-OCR-HENSYU-SEC
                   END-IF
      *
      *            カスタマイズ項目があるときＯＣＲに反映させる 
                   IF      FLG-RECE67          =   ZERO
                       MOVE    RECE62-OCR1            TO  OCR1-AREA
                       MOVE    RECE62-OCR2            TO  OCR2-AREA
                       MOVE    RECE62-OCR3            TO  OCR3-AREA
                       PERFORM 3301-OCR-HENSYU-SEC
                   END-IF
               END-IF    
      *
               MOVE    RECE62-OCR3     TO  HC06-OCR1
               MOVE    RECE62-OCR2     TO  HC06-OCR2
               MOVE    RECE62-OCR1     TO  HC06-OCR3
           END-IF
      *
      *    県単用項目
      *    但し、レセプト種別不明のときは原因を編集する
           IF      CNT-PAGE            =   1
               IF      RECE62-RECESYUBETU  =    ZERO
                   PERFORM 3101-FUMEI-HENSYU-SEC
               ELSE 
                   IF      RECE62-MISEIKYU-FLG     =   "01"
                       MOVE    WRK-ERR-MSG-7   TO   HC06-HEAD4-G (5:)
                   ELSE 
                    IF      RECE62-SYUKA         =   "XX"
                        MOVE    WRK-ERR-MSG-6   TO   HC06-HEAD4-G (5:)
                    ELSE 
                     IF      FLG-RECE67          =   ZERO
                       INSPECT RECE67-HC06-HEAD1-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE67-HC06-HEAD2-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE67-HC06-HEAD3-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE67-HEAD4-G      REPLACING   ALL "  "
                                                        BY  "　" 
                       MOVE    RECE67-HC06-HEAD1-G TO  HC06-HEAD1-G
                       MOVE    RECE67-HC06-HEAD2-G TO  HC06-HEAD2-G
                       MOVE    RECE67-HC06-HEAD3-G TO  HC06-HEAD3-G
                       MOVE    RECE67-HEAD4-G      TO  HC06-HEAD4-G
                     END-IF
                    END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    新潟県中越沖地震対応 
           IF      CNT-PAGE            =   1
               IF    ( RECE62-PREFNUM      =   "15"   )
               AND   ( RECE62-SRYYM        >=  200709 )
               AND   ( RECE62-SRYYM        <=  200712 )
                   EVALUATE    RECE62-RJNGAIKOHFTN (1)
                       WHEN    1
                           MOVE    "（不詳）"  TO  HC06-HEAD4 (2)(1:8)
                       WHEN    2
                           MOVE    "（災１）"  TO  HC06-HEAD4 (2)(1:8)
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       310-HCN06-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別不明のとき不明の原因を編集処理
      *****************************************************************
       3101-FUMEI-HENSYU-SEC              SECTION.
      *
           EVALUATE    RECE62-ERR-ERRKBN
               WHEN    1
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-1
                   END-IF
                   MOVE    WRK-ERR-MSG-1   TO   HC06-HEAD4-G (5:)
               WHEN    2
               WHEN    3
                   IF      RECE62-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE62-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-2
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-2
                   END-IF
                   MOVE    RECE62-ERR-TEKSTYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKSTYMD-2
                   IF      RECE62-ERR-ERRKBN    =   3
                       MOVE    WRK-ERR-MSG-21  TO   HC06-HEAD4-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-2   TO   HC06-HEAD4-G (5:)
                   END-IF
               WHEN    4
               WHEN    5
                   IF      RECE62-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE62-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-3
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-3
                   END-IF
                   MOVE    RECE62-ERR-TEKEDYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKEDYMD-3
                   IF      RECE62-ERR-ERRKBN    =   5
                       MOVE    WRK-ERR-MSG-31  TO   HC06-HEAD4-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-3   TO   HC06-HEAD4-G (5:)
                   END-IF
               WHEN    6
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-ERR-DAY  TO  WRK-KGK2
                   MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-DAY-4
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-4
                   END-IF
                   MOVE    WRK-ERR-MSG-4   TO   HC06-HEAD4-G (5:)
               WHEN    7
                   MOVE    WRK-ERR-MSG-5   TO   HC06-HEAD4-G (5:)
           END-EVALUATE
           INSPECT HC06-HEAD4-G  REPLACING ALL "  "    BY  "　"
      *
      *    主科未設定のとき     
           IF      RECE62-SYUKA         =   "XX"
               MOVE    HC06-HEAD4-G (5:)    TO  WRK-HC06-HEAD4-G
               MOVE    WRK-ERR-MSG-6        TO  HC06-HEAD4-G (5:)
               MOVE    WRK-HC06-HEAD4-G     TO  HC06-HEAD4-G (19:)
           END-IF
      *
      *    特別療養費の場合「特別療養費」文字記載を行う
           IF      RECE62-HKNNUM       =   "068"   OR  "069"
               MOVE    "特別療養費"    TO  HC06-MOJI1
           END-IF
      *
           .
       3101-FUMEI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HCN061-HEAD-HEN-SEC          SECTION.
      *
           EVALUATE    SYS-2005-NEXTPRTKBN
               WHEN    "0"
                   INITIALIZE                      HC061
                   MOVE    SPACE               TO  HC061
      *
                   MOVE    "（医科入院）"      TO  HC061-TYTLE
      *
      *            患者番号
                   MOVE    RECE62-PTNUM        TO  HC061-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC061-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYY           TO  HC061-SRYY 
                   MOVE    HC06-SRYM           TO  HC061-SRYM 
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC061-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC061-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC061-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC061-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC061-HONKZK
      *
      *            老人保険−市町村番号
                   MOVE    HC06-RJNFTNJANUM    TO  HC061-RJNFTNJANUM
      *
      *            老人保険−受給者番号
                   MOVE    HC06-RJNJKYSNUM     TO  HC061-RJNJKYSNUM
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC061-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC061-KIGO
                   MOVE    HC06-NUM            TO  HC061-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC061-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC061-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC061-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC061-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC061-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC061-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC061-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC061-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC061-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC061-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC061-MOJI1
                   MOVE    HC06-MOJI2          TO  HC061-MOJI2
               WHEN    "1"
                   INITIALIZE                      HC062
                   MOVE    SPACE               TO  HC062
      *
                   MOVE    "（医科入院）"      TO  HC062-TYTLE
      *
      *            患者番号
                   MOVE    RECE62-PTNUM        TO  HC062-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC062-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYY           TO  HC062-SRYY 
                   MOVE    HC06-SRYM           TO  HC062-SRYM 
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC062-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC062-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC062-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC062-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC062-HONKZK
      *
      *            老人保険−市町村番号
                   MOVE    HC06-RJNFTNJANUM    TO  HC062-RJNFTNJANUM
      *
      *            老人保険−受給者番号
                   MOVE    HC06-RJNJKYSNUM     TO  HC062-RJNJKYSNUM
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC062-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC062-KIGO
                   MOVE    HC06-NUM            TO  HC062-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC062-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC062-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC062-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC062-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC062-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC062-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC062-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC062-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC062-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC062-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC062-MOJI1
                   MOVE    HC06-MOJI2          TO  HC062-MOJI2
               WHEN    "2"
                   INITIALIZE                      HC063
                   MOVE    SPACE               TO  HC063
      *
                   MOVE    "（医科入院）"      TO  HC063-TYTLE
      *
      *            患者番号
                   MOVE    RECE62-PTNUM        TO  HC063-PTNUM
      *
      *            提出先
                   MOVE    HC06-TEISYUTUSAKI   TO  HC063-TEISYUTUSAKI
      *
      *            診療年月
                   MOVE    HC06-SRYY           TO  HC063-SRYY 
                   MOVE    HC06-SRYM           TO  HC063-SRYM 
      *
      *            県番
                   MOVE    SYS-1001-PREFNUM    TO  HC063-PREFNUM
      *
      *            医療機関コード
                   MOVE    SYS-1001-HOSPCDN    TO  HC063-HOSPCDN
      *
      *            保険種別１
                   MOVE    HC06-HKNSBT1        TO  HC063-HKNSBT1  
      *
      *            保険種別２
                   MOVE    HC06-HKNSBT2        TO  HC063-HKNSBT2  
      *
      *            本人・家族
                   MOVE    HC06-HONKZK         TO  HC063-HONKZK
      *
      *            老人保険−市町村番号
                   MOVE    HC06-RJNFTNJANUM    TO  HC063-RJNFTNJANUM
      *
      *            老人保険−受給者番号
                   MOVE    HC06-RJNJKYSNUM     TO  HC063-RJNJKYSNUM
      * 
      *            保険者番号・記号番号
                   MOVE    HC06-HKNJANUM       TO  HC063-HKNJANUM
                   MOVE    HC06-KIGO           TO  HC063-KIGO
                   MOVE    HC06-NUM            TO  HC063-NUM
      *     
      *            公費−負担者番号・受給者番号
                   IF      HC06-KOHJKYSNUM1N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM1
                                               TO  HC063-KOHJKYSNUM1
                   ELSE
                       MOVE    HC06-KOHJKYSNUM1N
                                               TO  HC063-KOHJKYSNUM1N
                   END-IF
                   IF      HC06-KOHJKYSNUM2N   =   SPACE
                       MOVE    HC06-KOHJKYSNUM2
                                               TO  HC063-KOHJKYSNUM2
                   ELSE
                       MOVE    HC06-KOHJKYSNUM2N
                                               TO  HC063-KOHJKYSNUM2N
                   END-IF
                   IF      HC06-KOHFTNJANUM1N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM1
                                               TO  HC063-KOHFTNJANUM1
                   ELSE
                       MOVE    HC06-KOHFTNJANUM1N
                                               TO  HC063-KOHFTNJANUM1N
                   END-IF
                   IF      HC06-KOHFTNJANUM2N  =   SPACE
                       MOVE    HC06-KOHFTNJANUM2
                                               TO  HC063-KOHFTNJANUM2
                   ELSE
                       MOVE    HC06-KOHFTNJANUM2N
                                               TO  HC063-KOHFTNJANUM2N
                   END-IF
      *
      *            給付割合
                   MOVE    HC06-KYURATE        TO  HC063-KYURATE
      *
      *           氏名
                   MOVE    HC06-NAME           TO  HC063-NAME
      *
      *            特別療養費の場合の「特別療養費」文字記載
                   MOVE    HC06-MOJI1          TO  HC063-MOJI1
                   MOVE    HC06-MOJI2          TO  HC063-MOJI2
           END-EVALUATE
           .
       310-HCN061-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別１>編集処理
      *****************************************************************
       3110-HKNSBT1-SET-SEC              SECTION.
      *
      *    不明分・自費は編集しない
           IF      RECE62-RECESYUBETU     =    "0000"  OR
                                               "8888"
               CONTINUE
           ELSE                                        
               IF      RECE62-RECESYUBETU  =   6666
                   EVALUATE    RECE62-RECESYUBETU-X (2:1)
                       WHEN    "1"
                           MOVE    "１国"          TO  HC06-HKNSBT1
                       WHEN    "2"
                           MOVE    "２公費"        TO  HC06-HKNSBT1
                       WHEN    "3"
                           MOVE    "３老人"        TO  HC06-HKNSBT1
                       WHEN    "4"
                           MOVE    "４退職"        TO  HC06-HKNSBT1
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE62-RECESYUBETU (2:1)
                       WHEN    "1"
                           EVALUATE    RECE62-TEISYUTUSAKI
                               WHEN    1
                                   MOVE    "１社"      TO  HC06-HKNSBT1
                               WHEN    2
                                   MOVE    "１国"      TO  HC06-HKNSBT1
                           END-EVALUATE
                       WHEN    "2"
                           MOVE    "２公費"        TO  HC06-HKNSBT1
                       WHEN    "3"
                           MOVE    "３老人"        TO  HC06-HKNSBT1
                       WHEN    "4"
                           MOVE    "４退職"        TO  HC06-HKNSBT1
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       310-HKNSBT1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別２>編集処理
      *****************************************************************
       3110-HKNSBT2-SET-SEC              SECTION.
      *
      *    不明分・自費は編集しない
           IF      RECE62-RECESYUBETU     =    "0000"  OR
                                               "8888"
               CONTINUE
           ELSE                                        
               IF      RECE62-RECESYUBETU  =   6666
                   EVALUATE    RECE62-RECESYUBETU-X (3:1)
                       WHEN    "1"
                           MOVE    "１単独"        TO  HC06-HKNSBT2
                       WHEN    "2"
                           MOVE    "２２併"        TO  HC06-HKNSBT2
                       WHEN    "3"
                           MOVE    "３３併"        TO  HC06-HKNSBT2
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE62-RECESYUBETU (3:1)
                       WHEN    "1"
                           MOVE    "１単独"        TO  HC06-HKNSBT2
                       WHEN    "2"
                           MOVE    "２２併"        TO  HC06-HKNSBT2
                       WHEN    "3"
                           MOVE    "３３併"        TO  HC06-HKNSBT2
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       310-HKNSBT2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <本人・家族>編集処理
      *****************************************************************
       3110-HONKZK-SET-SEC             SECTION.
      *
      *    不明分・自費は編集しない
           IF      RECE62-RECESYUBETU     =    "0000"  OR
                                               "8888"
               CONTINUE
           ELSE                                        
               IF      RECE62-RECESYUBETU  =   6666
                   EVALUATE    RECE62-RECESYUBETU-X (4:1)
                       WHEN    "1"
                           MOVE    "１本入"     TO  HC06-HONKZK
                       WHEN    "3"
                           MOVE    "３三入"     TO  HC06-HONKZK
                       WHEN    "5"
                           MOVE    "５家入"     TO  HC06-HONKZK
                       WHEN    "7"
                           MOVE    "７高入９"   TO  HC06-HONKZK
                       WHEN    "9"
                           MOVE    "９高入７"   TO  HC06-HONKZK
                   END-EVALUATE
               ELSE
                   EVALUATE    RECE62-RECESYUBETU (4:1)
                       WHEN    "1"
                           MOVE    "１本入"     TO  HC06-HONKZK
                       WHEN    "3"
                           MOVE    "３三入"     TO  HC06-HONKZK
                       WHEN    "5"
                           MOVE    "５家入"     TO  HC06-HONKZK
                       WHEN    "7"
                           MOVE    "７高入９"   TO  HC06-HONKZK
                       WHEN    "9"
                           MOVE    "９高入７"     TO  HC06-HONKZK
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       310-HONKZK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  主保険−保険者番号８桁
      *          公費−負担者番号  ８桁
      *          記号・番号　診療実日数     編集処理
      *****************************************************************
       3110-KANREN-NUM-SET-SEC        SECTION.
      *
      *    <保険者番号>
           MOVE    SPACE              TO  WRK-HKNJANUM
      *     
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE62-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               MOVE    RECE62-KOHFTNJANUM(SEIHIDX)  TO  WRK-HKNJANUM
           ELSE
               IF  RECE62-TEISYUTUSAKI    =   "2"
                   IF      RECE62-HKNJANUM (1:2)  =   "67"
                       MOVE    RECE62-HKNJANUM  TO  WRK-HKNJANUM
                   ELSE
      *            国保の場合（６桁右詰に編集）
                       MOVE    SPACE              TO  WRK-HKNJANUM(1:2)
                       MOVE    RECE62-HKNJANUM    TO  WRK-HKNJANUM(3:)
                   END-IF
               ELSE
                   IF  RECE62-HKNNUM      =   "001"
      *                政府管掌の場合（４桁右詰に編集）
                       MOVE    SPACE            TO  WRK-HKNJANUM(1:4)
                       MOVE    RECE62-HKNJANUM  TO  WRK-HKNJANUM(5:)
                   ELSE
      *                他の保険の場合（８桁）
                       MOVE    RECE62-HKNJANUM  TO  WRK-HKNJANUM
                   END-IF
               END-IF
           END-IF
             MOVE   WRK-HKNJANUM    TO    HC06-HKNJANUM
      *
      *    <記号・番号>
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE62-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               IF      RECE62-KOHJKYSNUM(SEIHIDX)
                                       NOT =   SPACE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KOHJKYSNUM(SEIHIDX)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KIGO
                   END-IF
               END-IF    
           ELSE
               IF      RECE62-KIGO     NOT =   SPACE 
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KIGO       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KIGO
                   END-IF
               END-IF
               IF      RECE62-NUM      NOT =   SPACE    
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE62-NUM      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-NUM
                   END-IF
               END-IF        
           END-IF
      *
      *    <公費負担者番号・公費受給者番号>
      *    
           IF      WRK-3SYU-KBN           =   ALL "1" AND 
                   RECE62-HKNNUM          =   SPACE            
      *        ３種の場合
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE    
      *        ３種以外の場合
               MOVE    1               TO  IDA
               MOVE    2               TO  IDB
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
           END-IF
      *    公負１
           IF      RECE62-KOHFTNJANUM(IDA)
                                       NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE62-KOHFTNJANUM(IDA)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE62-KOHFTNJANUM(IDA)
                                       TO  HC06-KOHFTNJANUM1
                   ELSE
                       MOVE    RECE62-KOHFTNJANUM(IDA)
                                       TO  HC06-KOHFTNJANUM1N
                   END-IF
               END-IF                        
           END-IF
      *    公負２
           IF      RECE62-KOHFTNJANUM(IDB)
                                       NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE62-KOHFTNJANUM(IDB)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE62-KOHFTNJANUM(IDB)
                                           TO  HC06-KOHFTNJANUM2
                   ELSE
                       MOVE    RECE62-KOHFTNJANUM(IDB)
                                           TO  HC06-KOHFTNJANUM2N
                   END-IF
               END-IF                        
           END-IF
      *    公受１
           IF      RECE62-KOHJKYSNUM(IDA)
                                      NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE62-KOHJKYSNUM(IDA)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE62-KOHJKYSNUM(IDA)
                                           TO  HC06-KOHJKYSNUM1
                   ELSE
                       MOVE    RECE62-KOHJKYSNUM(IDA)
                                           TO  HC06-KOHJKYSNUM1N
                   END-IF
               END-IF
           END-IF                        
      *    公受２
           IF      RECE62-KOHJKYSNUM(IDB)
                                       NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI 
               MOVE    RECE62-KOHJKYSNUM(IDB)
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   IF      KANACHK-SYUBETU =   1
                       MOVE    RECE62-KOHJKYSNUM(IDB)
                                           TO  HC06-KOHJKYSNUM2
                   ELSE
                       MOVE    RECE62-KOHJKYSNUM(IDB)
                                           TO  HC06-KOHJKYSNUM2N
                   END-IF
               END-IF
           END-IF                        
      *
      *    レセプト特記事項編集情報
      *    公負１
           EVALUATE    RECE62-2006-FTNJANUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHFTNJANUM1
                                               HC06-KOHFTNJANUM1N
               WHEN    "02"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-2006-FTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE62-2006-FTNJANUM (IDA)
                                               TO  HC06-KOHFTNJANUM1 
                       MOVE    SPACE           TO  HC06-KOHFTNJANUM1N
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KOHFTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE62-KOHFTNJANUM (IDA) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  HC06-KOHFTNJANUM1 
                           MOVE    SPACE       TO  HC06-KOHFTNJANUM1N
                       END-IF
                   END-IF
           END-EVALUATE
      *    公受１
           EVALUATE    RECE62-2006-JKYSNUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHJKYSNUM1
                                               HC06-KOHJKYSNUM1N
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-2006-JKYSNUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE62-2006-JKYSNUM (IDA)
                                               TO  HC06-KOHJKYSNUM1
                       MOVE    SPACE           TO  HC06-KOHJKYSNUM1N
                   END-IF
           END-EVALUATE
      *    公負２
           EVALUATE    RECE62-2006-FTNJANUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHFTNJANUM2
                                               HC06-KOHFTNJANUM2N
               WHEN    "02"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-2006-FTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE62-2006-FTNJANUM (IDB)
                                               TO  HC06-KOHFTNJANUM2
                       MOVE    SPACE           TO  HC06-KOHFTNJANUM2N
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KOHFTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE62-KOHFTNJANUM (IDB) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  HC06-KOHFTNJANUM2
                           MOVE    SPACE       TO  HC06-KOHFTNJANUM2N
                       END-IF
                   END-IF
           END-EVALUATE
      *    公受２
           EVALUATE    RECE62-2006-JKYSNUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE    SPACE           TO  HC06-KOHJKYSNUM2
                                               HC06-KOHJKYSNUM2N
               WHEN    OTHER
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-2006-JKYSNUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       MOVE    RECE62-2006-JKYSNUM (IDB)
                                               TO  HC06-KOHJKYSNUM2
                       MOVE    SPACE           TO  HC06-KOHJKYSNUM2N
                   END-IF
           END-EVALUATE
      *        県単用項目
           IF      FLG-RECE67              =   ZERO
               IF      RECE67-KOHINF (IDA) NOT =   SPACE
                   MOVE    SPACE               TO   HC06-KOH-TBL1
      *            公負１
                   IF      RECE67-KOHFTNJANUM(IDA)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE67-KOHFTNJANUM(IDA)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                               MOVE    RECE67-KOHFTNJANUM(IDA)
                                               TO  HC06-KOHFTNJANUM1
                           ELSE
                               MOVE    RECE67-KOHFTNJANUM(IDA)
                                               TO  HC06-KOHFTNJANUM1N
                           END-IF
                       END-IF
                   END-IF
      *            公受１
                   IF      RECE67-KOHJKYSNUM(IDA)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE67-KOHJKYSNUM(IDA)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                              MOVE    RECE67-KOHJKYSNUM(IDA)
                                               TO  HC06-KOHJKYSNUM1
                           ELSE
                               MOVE    RECE67-KOHJKYSNUM(IDA)
                                               TO  HC06-KOHJKYSNUM1N
                           END-IF
                       END-IF                        
                   END-IF
               END-IF 
      *
               IF      RECE67-KOHINF (IDB) NOT =   SPACE
                   MOVE    SPACE               TO   HC06-KOH-TBL2
      *            公負２
                   IF      RECE67-KOHFTNJANUM(IDB)
                                       NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE67-KOHFTNJANUM(IDB)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                               MOVE    RECE67-KOHFTNJANUM(IDB)
                                               TO  HC06-KOHFTNJANUM2
                           ELSE
                               MOVE    RECE67-KOHFTNJANUM(IDB)
                                               TO  HC06-KOHFTNJANUM2N
                           END-IF
                       END-IF                        
                   END-IF
      *            公受２
                   IF      RECE67-KOHJKYSNUM(IDB)
                                           NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "1"             TO  KANACHK-SYORI 
                       MOVE    RECE67-KOHJKYSNUM(IDB)
                                               TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           IF      KANACHK-SYUBETU =   1
                              MOVE    RECE67-KOHJKYSNUM(IDB)
                                               TO  HC06-KOHJKYSNUM2
                           ELSE
                               MOVE    RECE67-KOHJKYSNUM(IDB)
                                               TO  HC06-KOHJKYSNUM2N
                           END-IF
                       END-IF                        
                   END-IF
               END-IF                        
           END-IF    
      *
      *    <診療実日数>
           IF      CNT-PAGE        =   1
               IF      RECE62-TOTALTEN (IDX1)     >   ZERO
                  MOVE    RECE62-JNISSU (IDX1)
                                          TO  WRK-JINISSU2Z
                  MOVE    WRK-JINISSU2    TO  HC06-JINISSU
               END-IF
               IF      RECE62-TOTALTEN (IDX2)     >   ZERO
                  IF  (RECE62-JNISSU (IDX1)
                                      NOT  =   RECE62-JNISSU (IDX2))
                  OR  (RECE62-TOTALTEN (IDX1)     =   ZERO)
                      MOVE    RECE62-JNISSU (IDX2)
                                              TO  WRK-JINISSU2Z
                      MOVE    WRK-JINISSU2    TO  HC06-JINISSU1
                  END-IF
                  IF  (RECE62-SRYYM          >=  "200610"  )
                  AND (SYS-1001-PREFNUM       =    33      )
                  AND (RECE62-KOHNUM(1)(1:1)
                                        >=  "1" AND <=  "8")
                      MOVE    RECE62-JNISSU (IDX2)
                                              TO  WRK-JINISSU2Z
                      MOVE    WRK-JINISSU2    TO  HC06-JINISSU1
                  END-IF
               END-IF
               IF  RECE62-FUKUSHIFLG  =  "1"
                   IF  RECE62-JNISSU (IDX2)
                                       NOT =   RECE62-JNISSU (IDX3)
                       MOVE    RECE62-JNISSU (IDX3)
                                               TO  WRK-JINISSU2Z
                       MOVE    WRK-JINISSU2    TO  HC06-JINISSU2
                   END-IF
               ELSE
                   IF      RECE62-TOTALTEN (IDX3)     >   ZERO
                      IF  RECE62-JNISSU (IDX2)
                                          NOT =   RECE62-JNISSU (IDX3)
                          MOVE    RECE62-JNISSU (IDX3)
                                                  TO  WRK-JINISSU2Z
                          MOVE    WRK-JINISSU2    TO  HC06-JINISSU2
                      END-IF
                      IF  (RECE62-SRYYM          >=  "200610"  )
                      AND (SYS-1001-PREFNUM       =    33      )
                      AND (RECE62-KOHNUM(2)(1:1)
                                            >=  "1" AND <=  "8")
                          MOVE    RECE62-JNISSU (IDX3)
                                                  TO  WRK-JINISSU2Z
                          MOVE    WRK-JINISSU2    TO  HC06-JINISSU2
                      END-IF
                   END-IF
               END-IF
      *        県単用項目
               IF      FLG-RECE67           =   ZERO
                   IF      RECE67-JNISSU (IDX1) >   ZERO
                       IF      RECE67-JNISSU (IDX1) =   99
                           MOVE    ZERO            TO  WRK-JINISSU2Z
                       ELSE  
                           MOVE    RECE67-JNISSU (IDX1)
                                                   TO  WRK-JINISSU2Z
                       END-IF
                       MOVE    WRK-JINISSU2    TO  HC06-JINISSU
                   END-IF
                   IF      RECE67-JNISSU (IDX2) >   ZERO
                       IF      RECE67-JNISSU (IDX2) =   99
                           MOVE    ZERO            TO  WRK-JINISSU2Z
                       ELSE  
                           MOVE    RECE67-JNISSU (IDX2)
                                                   TO  WRK-JINISSU2Z
                       END-IF
                       MOVE    WRK-JINISSU2    TO  HC06-JINISSU1
                   END-IF
                   IF      RECE67-JNISSU (IDX3) >   ZERO
                       IF      RECE67-JNISSU (IDX3) =   99
                           MOVE    ZERO            TO  WRK-JINISSU2Z
                       ELSE  
                           MOVE    RECE67-JNISSU (IDX3)
                                                   TO  WRK-JINISSU2Z
                       END-IF
                       MOVE    WRK-JINISSU2    TO  HC06-JINISSU2
                   END-IF
               END-IF
      *
      *        宮崎地方公費対応
               IF    ( RECE62-PREFNUM      =   "45"                  )
               AND   ( RECE62-SRYYM        >=  WRK-CONS-SRYYM-200709 )
               AND   ( RECE62-TEISYUTUSAKI =   "2"                   )
                   IF      RECE62-KOHNUM (IDA) =   "190"
                       MOVE    SPACE               TO  HC06-JINISSU1
                   END-IF
                   IF      RECE62-KOHNUM (IDB) =   "190"
                       MOVE    SPACE               TO  HC06-JINISSU2
                   END-IF
               END-IF
           END-IF
      *
           .
       3110-KANREN-NUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <給付割合>　編集処理
      *****************************************************************
       3110-KYURATE-SET-SEC        SECTION.
      *
           MOVE   SPACE              TO  HC06-KYURATE
      *    国保のみ印字
           IF    ( RECE62-TEISYUTUSAKI =   "2"   )
           AND   ( RECE62-RJNHKNNUM    =   SPACE )
               COMPUTE WRK-KYURATE-9   = 
                                  (100 -   RECE62-KYURATE)
                                           / 10 
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"               TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9       TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
                  IF    ( KANACHK-RC       =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF
      *
      *    神奈川県国保老健は給付割合を記載（暫定的な対応）
           IF    ( RECE62-TEISYUTUSAKI        =   "2"    )
           AND   ( RECE62-PREFNUM             =   "14"   )
           AND   ( RECE62-RJNHKNNUM       NOT =   SPACE  )
           AND  (( RECE62-HKNJANUM-KEY (1:2)  =   "14"   )
           OR    ( RECE62-HKNJANUM-KEY (1:4)  =   "6714" ))
               COMPUTE WRK-KYURATE-9   = 
                                  (100 -   RECE62-KYURATE)
                                           / 10 
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"               TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9       TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
                  IF    ( KANACHK-RC       =   ZERO )
                   AND   ( KANACHK-MAX     >   ZERO )
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF
      *
      *    特別療養費のときシス管の設定より編集
           IF      RECE62-RECESYUBETU  =   6666
               EVALUATE    SYS-2005-KYURATE-KISAIKBN
                   WHEN    ZERO
                       MOVE   "給割１０"           TO  HC06-KYURATE
                   WHEN    "1" 
                       MOVE   "給割０"             TO  HC06-KYURATE
                   WHEN    "2" 
                       MOVE   SPACE                TO  HC06-KYURATE
               END-EVALUATE
           END-IF
      *
      *    レセプト特記事項編集情報
           IF      RECE62-2006-KYURATE >   ZERO
               COMPUTE WRK-KYURATE-9   =
                                       RECE62-2006-KYURATE /   10 
               MOVE   "給割"           TO  HC06-KYURATE
               MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-MAX     >   ZERO )
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KYURATE(5:)
               END-IF
           END-IF
      *
      *    県単用項目
           IF    ( FLG-RECE67          =   ZERO )
           AND   ( RECE67-KYURATE      >   ZERO )
               IF      RECE67-KYURATE      =   999
                   MOVE    10                  TO  WRK-KYURATE-9
               ELSE
                   COMPUTE WRK-KYURATE-9   =   (100 -   RECE67-KYURATE)
                                               / 10
               END-IF
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE   "給割"           TO  HC06-KYURATE
                   MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                   AND     KANACHK-MAX     >   ZERO 
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  HC06-KYURATE(5:)
                   END-IF    
               END-IF
           END-IF    
           .
       3110-KYURATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <特記事項>　編集処理
      *****************************************************************
       3110-TOKKI-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDZ
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX                 >   5     )
                   OR    ( RECE62-TOKKI(IDX)   =   SPACE )
                   OR    ( IDZ                 =   4     ) 
      *
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL ( IDY             >   TBL-TOKKI-MAX  )
                   IF      RECE62-TOKKI(IDX)   =   TBL-TOKKICD(IDY)
                       ADD     1                TO  IDZ
                       MOVE    TBL-TOKKINAME(IDY)
                                                TO  HC06-TOKKI (IDZ)
                       MOVE    TBL-TOKKI-MAX    TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    レセプト特記事項編集情報
           IF      RECE62-2006-TOKKIINF    NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   4
                       OR      IDZ                 =   4     
                       OR      RECE62-2006-TOKKI(IDX)  =   SPACE
                   ADD     1                    TO  IDZ    
                   MOVE    RECE62-2006-TOKKI (IDX)           
                                                TO  HC06-TOKKI (IDZ)
               END-PERFORM
           END-IF
      *
      *    県単用項目
           IF      FLG-RECE67           =   ZERO
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   4
                       OR      IDZ                 =   4     
                       OR      RECE67-HC06-TOKKI(IDX)  =   SPACE
                   ADD     1                    TO  IDZ    
                   MOVE    RECE67-HC06-TOKKI (IDX)           
                                                TO  HC06-TOKKI (IDZ)
               END-PERFORM
           END-IF
      *
           .
       3110-TOKKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <病棟区分>　編集処理                                  
      *****************************************************************
       3110-BYOTOKBN-SET-SEC          SECTION.                         
      *                                                                 
           MOVE     1      TO     IDY5                                  
      *                                                                 
           PERFORM    VARYING   IDY4     FROM    1   BY  1              
                  UNTIL   IDY4                  >   4                   
                    OR    RECE62-BYOTOKBN(IDY4) =  SPACE                
      *                                                                 
                EVALUATE  RECE62-BYOTOKBN(IDY4)                         
                   WHEN "01"                                            
                      MOVE  "０１精神　"   TO  HC06-NYUINKBN(IDY5:10)        
                      ADD    10            TO  IDY5                         
                   WHEN "02"                                            
                      MOVE  "０２結核　"   TO  HC06-NYUINKBN(IDY5:10)        
                      ADD    10            TO  IDY5                         
                   WHEN "07"                                            
                      MOVE  "０７療養　"   TO  HC06-NYUINKBN(IDY5:10)        
                      ADD    10            TO  IDY5                         
                END-EVALUATE                                            
      *                                                                 
           END-PERFORM                                                  
      *                                                                 
           .                                                            
       3110-BYOTOKBN-SET-EXT.                                           
           EXIT.                                                        
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       3301-OCR-HENSYU-SEC              SECTION.
      *
           IF      RECE67-KOHINF (IDA) NOT =   SPACE
      *        公費１負担者番号
               IF      RECE67-KOHFTNJANUM(IDA) =      SPACE
                   MOVE  ZERO               TO      OCR2-FTNJANUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE67-KOHFTNJANUM(IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-FTNJANUM
                   END-IF
               END-IF
      *        公費１受給者番号
               IF      RECE67-KOHJKYSNUM(IDA) =       SPACE
                   MOVE  ZERO               TO      OCR2-JKYSNUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE67-KOHJKYSNUM(IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-JKYSNUM
                   END-IF
               END-IF
           END-IF 
      *
           IF      RECE67-KOHINF (IDB) NOT =   SPACE
      *        公費２負担者番号
               IF      RECE67-KOHFTNJANUM(IDB) =      SPACE
                   MOVE  ZERO               TO      OCR3-FTNJANUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE67-KOHFTNJANUM(IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-FTNJANUM
                   END-IF
               END-IF
      *        公費２受給者番号
               IF      RECE67-KOHJKYSNUM(IDB) =       SPACE
                   MOVE  ZERO               TO      OCR3-JKYSNUM
               ELSE
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE67-KOHJKYSNUM(IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-JKYSNUM
                   END-IF
               END-IF
           END-IF
      *  
      *    診療実日数
           IF      RECE67-JNISSU (IDX1) >   ZERO
               IF      RECE67-JNISSU   (IDX1)  =   99
                   MOVE    ZERO                    TO  OCR1-JNISSU
               ELSE
                   MOVE    RECE67-JNISSU(IDX1)     TO  OCR1-JNISSU
               END-IF
           END-IF
      *    公費１診療実日数
           IF      RECE67-JNISSU (IDX2) >   ZERO
               IF      RECE67-JNISSU   (IDX2)  =   99
                   MOVE    ZERO                    TO  OCR2-JNISSU
               ELSE
                   MOVE    RECE67-JNISSU  (IDX2)   TO  OCR2-JNISSU
               END-IF
           END-IF
      *    公費２診療実日数
           IF      RECE67-JNISSU (IDX3) >   ZERO
               IF      RECE67-JNISSU   (IDX3)  =   99
                   MOVE    ZERO                    TO  OCR3-JNISSU
               ELSE
                   MOVE    RECE67-JNISSU  (IDX3)   TO  OCR3-JNISSU
               END-IF
           END-IF
      *    請求点数
           IF      RECE67-TOTALTEN (IDX1)  >   ZERO
               IF      RECE67-TOTALTEN (IDX1)  =   99999999
                   MOVE    ZERO                    TO  OCR1-SKYTENSU
               ELSE
                   MOVE    RECE67-TOTALTEN(IDX1)   TO  OCR1-SKYTENSU
               END-IF
           END-IF
           IF      RECE67-FTNMONEY (IDX1)  >   ZERO
               IF      RECE67-FTNMONEY (IDX1)  =   999999999
                   MOVE    ZERO                    TO  OCR1-FTNMONEY
               ELSE
                   MOVE    RECE67-FTNMONEY(IDX1)   TO  OCR1-FTNMONEY
               END-IF
           END-IF
      *    公費・請求点数１
           IF      RECE67-TOTALTEN (IDX2)  >   ZERO                     
               IF      RECE67-TOTALTEN (IDX2)  =   99999999
                   MOVE    ZERO                    TO  OCR2-TENSU
               ELSE
                   MOVE    RECE67-TOTALTEN(IDX2)   TO  OCR2-TENSU
               END-IF 
           END-IF
           IF    RECE67-FTNMONEY (IDX2)    >   ZERO
               IF      RECE67-FTNMONEY (IDX2)  =   999999999
                   MOVE    ZERO                    TO  OCR2-FTNMONEY
               ELSE
                   MOVE    RECE67-FTNMONEY(IDX2)   TO  OCR2-FTNMONEY
               END-IF
           END-IF
      *    公費・請求点数２
           IF      RECE67-TOTALTEN (IDX3)  >   ZERO                      
               IF      RECE67-TOTALTEN (IDX3)  =   99999999
                   MOVE    ZERO                    TO  OCR3-TENSU
               ELSE
                   MOVE    RECE67-TOTALTEN(IDX3)   TO  OCR3-TENSU
               END-IF
           END-IF
           IF      RECE67-FTNMONEY (IDX3)  >   ZERO
               IF      RECE67-FTNMONEY (IDX3)  =   999999999
                   MOVE    ZERO                    TO  OCR3-FTNMONEY
               ELSE
                   MOVE    RECE67-FTNMONEY(IDX3)   TO  OCR3-FTNMONEY
               END-IF
           END-IF
      *
           PERFORM 33011-OCR1-HENSYU-SEC
           PERFORM 33012-OCR2-HENSYU-SEC
           PERFORM 33013-OCR3-HENSYU-SEC
           .
       3301-OCR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット１（Ｆ００２）
      *****************************************************************
       33011-OCR1-HENSYU-SEC                   SECTION.
      *
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
      *
           MOVE    OCR1-AREA            TO      RECE62-OCR1           
      *
           .
       33011-OCR1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット２（Ｆ００２）
      *****************************************************************
       33012-OCR2-HENSYU-SEC                   SECTION.
      *
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
      *
           IF      OCR2-AREA-X          =   SPACE
               CONTINUE
           ELSE
      *
               INSPECT OCR2-AREA-X
                           REPLACING    ALL   " "   BY   "0"
      *
      *        チェックディジット３
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(27:15)     TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT3
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT3
               END-IF
      *        チェックディジット４
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(45:7)      TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT4
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT4
               END-IF
      *        チェックディジット６
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(59:5)      TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                  MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT6
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT6
               END-IF
      *        チェックディジット７
               INITIALIZE                           LNK-CHKDGT-AREA
               MOVE    1                    TO      LNK-I-SYORIKBN
               MOVE    OCR2-AREA(43:22)     TO      LNK-I-CODE
               CALL    "ORCSCHKDGT11"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD          =       ZERO
                   MOVE  LNK-O-CHKDGT   TO      OCR2-CHKDGT7
               ELSE
                   MOVE  ZERO           TO      OCR2-CHKDGT7
               END-IF
      *
               IF      OCR2-AREA-X          =   ALL "0"
                   MOVE    SPACE                    TO  OCR2-AREA-X
               END-IF
           END-IF
      *  
           MOVE    OCR2-AREA            TO      RECE62-OCR2
      *
           .
       33012-OCR2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトＯＣＲ領域のセット３（Ｆ００２）
      *****************************************************************
       33013-OCR3-HENSYU-SEC                   SECTION.
      *
           IF      OCR3-AREA-X          =   SPACE
               GO  TO  33013-OCR3-HENSYU-EXT
           END-IF
      *
           INSPECT OCR3-AREA-X
                           REPLACING    ALL   " "   BY   "0"
      *
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
      *
           IF      OCR3-AREA-X          =   ALL "0"
               MOVE    SPACE                TO  RECE62-OCR3
           ELSE 
               MOVE    OCR3-AREA            TO  RECE62-OCR3
           END-IF
      *
           .
       33013-OCR3-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理（レセプト特記事項編集情報から編集）
      *****************************************************************
       3302-OCR-HENSYU-SEC              SECTION.
      *
      *    公費１負担者番号
           EVALUATE    RECE62-2006-FTNJANUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO  OCR2-FTNJANUM
               WHEN    "02"
                   INITIALIZE                   ORCSNUMAREA
                   MOVE    RECE62-2006-FTNJANUM (IDA)
                                            TO  SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =   ZERO
                       MOVE   SNUM-OUTNUM   TO  OCR2-FTNJANUM
                   ELSE
                       MOVE   ZERO          TO  OCR2-FTNJANUM
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KOHFTNJANUM (IDA)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE62-KOHFTNJANUM (IDA) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  OCR2-FTNJANUM
                       ELSE
                           MOVE   ZERO         TO  OCR2-FTNJANUM
                       END-IF
                   ELSE
                       MOVE   ZERO         TO  OCR2-FTNJANUM
                   END-IF
           END-EVALUATE
      *
      *    公費１受給者番号
           EVALUATE    RECE62-2006-JKYSNUMKBN  (IDA)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR2-JKYSNUM
               WHEN    OTHER
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE62-2006-JKYSNUM (IDA)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR2-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR2-JKYSNUM
                   END-IF
           END-EVALUATE
      *
      *    公費２負担者番号
           EVALUATE    RECE62-2006-FTNJANUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR3-FTNJANUM
               WHEN    "02"
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE62-2006-FTNJANUM (IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-FTNJANUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-FTNJANUM
                   END-IF
               WHEN    "03"
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI 
                   MOVE    RECE62-KOHFTNJANUM (IDB)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-SYUBETU =   "1"
                       INITIALIZE                   ORCSNUMAREA
                       MOVE    RECE62-KOHFTNJANUM (IDB) (1:KANACHK-MAX)
                                                TO  SNUM-INX
                       CALL    "ORCSNUM"        USING   ORCSNUMAREA
                       IF      SNUM-RC          =   ZERO
                           MOVE   SNUM-OUTEDT (8:8)
                                               TO  OCR3-FTNJANUM
                       ELSE
                           MOVE   ZERO         TO  OCR3-FTNJANUM
                       END-IF
                   ELSE
                       MOVE   ZERO         TO  OCR3-FTNJANUM
                   END-IF
           END-EVALUATE
      *    公費２受給者番号
           EVALUATE    RECE62-2006-JKYSNUMKBN  (IDB)
               WHEN    SPACE
                   CONTINUE
               WHEN    "01"
                   MOVE  ZERO               TO      OCR3-JKYSNUM
               WHEN    OTHER
                   INITIALIZE                       ORCSNUMAREA
                   MOVE    RECE62-2006-JKYSNUM (IDB)
                                            TO      SNUM-INX
                   CALL    "ORCSNUM"        USING   ORCSNUMAREA
                   IF      SNUM-RC          =       ZERO
                       MOVE   SNUM-OUTNUM
                                        TO      OCR3-JKYSNUM
                   ELSE
                       MOVE   ZERO      TO      OCR3-JKYSNUM
                   END-IF
           END-EVALUATE
      *
           PERFORM 33011-OCR1-HENSYU-SEC
           PERFORM 33012-OCR2-HENSYU-SEC
           PERFORM 33013-OCR3-HENSYU-SEC
           .
       3302-OCR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *
      *    パラメータ０３ファイルより編集
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
           MOVE    ZERO                TO  FLG-TENKI                                
      *     
           INITIALIZE                      RECE631-REC
           MOVE    RECE62-HOSPID       TO  RECE631-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE631-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE631-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE631-PTID
           MOVE    RECE62-RECEKA       TO  RECE631-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE631-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE631-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE631-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE631-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE631-TEKSTYMD
      *
           START   RECE631-FILE
               KEY IS    >=   RECE631-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE631
               NOT INVALID
                   PERFORM 900-RECE631-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE631          =   1                    )
               OR     ( RECE62-HOSPID    NOT =   RECE631-HOSPID       )
               OR     ( RECE62-SRYYM     NOT =   RECE631-SRYYM        )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE631-NYUGAIKBN    )
               OR     ( RECE62-PTID      NOT =   RECE631-PTID         )
               OR     ( RECE62-RECEKA    NOT =   RECE631-RECEKA       )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE631-TEISYUTUSAKI )
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE631-RECESYUBETU  )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE631-HKNJANUM     )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE631-HOJOKBN      )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE631-TEKSTYMD     )
      *    
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE631     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
               END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           IF    ( SYS-2005-SYUBYOHENFLG   =   "3" )
           AND   ( WRK-BO-SYUBYOFLG        =   "1" )
               IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    WRK-KUGIRI3 TO
                                       HC06-BYOMEI(WRK-LINEBYO)(2:52)
               ELSE
                   IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI   
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
      *    連結表示のとき
           IF      SYS-2005-BYOMEIHENKBN    =   "0"
      *        前のデータと同一主病名フラグ・診療開始日・転帰区分
               IF  ( RECE631-SYUBYOFLG    =   WRK-BO-SYUBYOFLG )  AND
                   ( RECE631-SRYYMD       =   WRK-BO-SRYYMD    )  AND
                   ( RECE631-TENKIKBN     =   WRK-BO-TENKIKBN  )  AND 
                   ( RECE631-TENKIYMD     =   WRK-BO-TENKIYMD  )   
      *
      *            病名をつなげると何桁になるかを計算（，＋病名）
                   IF  WRK-CLM           =   ZERO
                       MOVE    WRK-CLM   TO  WRK-WCLM
                   ELSE
                       COMPUTE WRK-WCLM  =   WRK-CLM   +   2
                   END-IF
                   COMPUTE     WRK-WCLM  =
                              ( RECE631-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *
                   EVALUATE    TRUE
      *                傷病名欄に編集
                       WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                            前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
      *                適用欄に編集
                       WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                        前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *            主病名フラグ・診療開始日・転帰区分 が異なる場合
                   IF      WRK-BYO-OLD        =   LOW-VALUE
                       CONTINUE
                   ELSE
                       IF  WRK-LINEBYO       <  WRK-BYOMEI-LINE
                           IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           END-IF
                       ELSE
                           IF  WRK-BYOMEI2   NOT =   SPACE
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI 
                           END-IF
                       END-IF
                   END-IF    
               END-IF
           ELSE
                IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
                    IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                        PERFORM 3221-BYOMEI1-OUT-SEC
                    END-IF
                ELSE
                    IF  WRK-BYOMEI2   NOT =   SPACE
                        MOVE    1             TO  FLG-TENKI
                        PERFORM 3222-BYOMEI2-OUT-SEC
                        MOVE    ZERO          TO  FLG-TENKI
                    END-IF
                END-IF
           END-IF
      *
      *    主病名との区切り線編集処理     
           PERFORM 3211-SYUBYOFLG-HENSYU-SEC
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM         =   WRK-CLM   +   1
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE631-BYOMEIMOJI   *   2
           MOVE    RECE631-BYOMEI      TO  WRK-BYOMEI
      *
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       MOVE    RECE631-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE631-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE631-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       MOVE    RECE631-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE631-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE631-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *     
           MOVE    RECE631-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE631-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE631-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE631-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
           PERFORM 900-RECE631-NEXT-SEC
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *      主病名と副病名の区切り線の編集
      ****************************************************************
       3211-SYUBYOFLG-HENSYU-SEC         SECTION.
      *
      *    主病名編集フラグが区切り有のときのみ処理を行う
           IF      SYS-2005-SYUBYOHENFLG   =   "1" OR  "3"
      *        主病名との区切り線（副傷病名があるときのみ）     
               IF      WRK-BYO-OLD        =   LOW-VALUE
                   CONTINUE
               ELSE
                   IF    ( FLG-RECE631             =   ZERO            )
                   AND   ( RECE631-SYUBYOFLG   NOT =   WRK-BO-SYUBYOFLG)
                   AND   ( WRK-BO-SYUBYOFLG        =   "1"             )
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           ADD     1           TO  WRK-LINEBYO
                           MOVE    WRK-KUGIRI3 TO
                                       HC06-BYOMEI(WRK-LINEBYO)(2:52)
                       ELSE
                           IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           MOVE    1           TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO        TO  FLG-KUGIRI   
                       END-IF
                   END-IF
               END-IF
           END-IF        
      *
           .
       3211-SYUBYOFLG-HENSYU-EXT.
           EXIT.
      ****************************************************************
      *     傷病名１出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
      *    傷病名欄の行カウント
           ADD     1               TO  WRK-LINEBYO
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
               MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
               MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI1-NO 
               END-IF    
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)(7:)
      *        転帰
               MOVE    WRK-BYOMEI1-NO    TO  HC06-TENKI(WRK-LINEBYO)
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "2"
                   MOVE    "死亡"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "3"
                   MOVE    "中止"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    OTHER
                   MOVE    SPACE     TO  HC06-TENKI(WRK-LINEBYO)
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                           TO  HC06-TENKIYMD
                                                    (WRK-LINEBYO)
               END-IF                                     
           END-IF
      *    病名
           MOVE    WRK-BYOMEI1     TO  HC06-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名２出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     1               TO  WRK-LINETEK
      
      *
           IF  WRK-CONTINUE        =   ZERO
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               IF  WRK-LINE            <   10
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
               ELSE    
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (1:2)  TO  KANACHK-MAE-INPUT (2:2)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (4:1)
               END-IF
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-BYOMEI2-NO 
               END-IF    
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               IF  WRK-BYOMEI2-ELSE    =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           ELSE
               IF  WRK-BYOMEI2R-ELSE   =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           END-IF    
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO  (WRK-LINETEK)
           ELSE
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       MOVE    WRK-BYOMEI2 TO
                                           HC061-TEKIYO (WRK-LINETEK)
                   WHEN    "1"
                       MOVE    WRK-BYOMEI2 TO
                                           HC062-TEKIYO (WRK-LINETEK)
                   WHEN    "2"
                       MOVE    WRK-BYOMEI2 TO
                                           HC063-TEKIYO (WRK-LINETEK)
               END-EVALUATE
           END-IF    
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *        診療年月日・転帰　編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-BYOMEI2
                               REPLACING    ALL   "  "   BY   "　"
               IF      CNT-PAGE        =   1
                   MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE    
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    WRK-BYOMEI2 TO
                                               HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-BYOMEI2 TO
                                               HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-BYOMEI2 TO
                                               HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           END-IF
      *
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰　編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    転帰日を編集しないとき
           IF      SYS-2005-TENKIHENKBN    =   ZERO
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2-TENKI
               END-EVALUATE
           ELSE
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2R-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2R-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2R-TENKI
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                               TO  WRK-BYOMEI2R-TENKIYMD
               END-IF                                
           END-IF                                     
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
           EVALUATE  RECE62-SHOSHINTIME
               WHEN  "1"
                     MOVE  "（時間外）"   TO  HC06-SHOSHINTIME
               WHEN  "2"
                     MOVE  "（深夜）"     TO  HC06-SHOSHINTIME
               WHEN  "3"
                     MOVE  "（休日）"     TO  HC06-SHOSHINTIME
               WHEN  "4"
                     MOVE  "時外・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "5"
                     MOVE  "時外・休日"   TO  HC06-SHOSHINTIME
               WHEN  "6"
                     MOVE  "休日・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "7"
                     MOVE  "外・休・深"   TO  HC06-SHOSHINTIME
           END-EVALUATE
      *     
           IF      RECE62-HKNNUM      =   SPACE
               IF      SEIHIDX            >   ZERO 
                   COMPUTE IDX1    =   SEIHIDX   +   1
                   MOVE    2                  TO  IDX2
                   IF      IDX1               =   IDX2
                       COMPUTE  IDX2  =  IDX1  +  1
                   END-IF
               ELSE
                   MOVE    2                  TO  IDX1
                   MOVE    3                  TO  IDX2
               END-IF 
           ELSE 
               MOVE    1                  TO  IDX1
               MOVE    2                  TO  IDX2
           END-IF      
      *
      *    １１初診〜８０他         
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   22
      *        <回数>
               MOVE    RECE62-KAISU(IDX)         TO  WRK-KAISUZ
               MOVE    WRK-KAISU                 TO  HC06-KAISU   (IDX)
      *        <点数・公費点数>
               IF    FLG-JIRITU-HENSYU  =  1
                  COMPUTE WRK-SUM-TENSU   =     RECE62-TENSU(IDX IDX1)
                                          +     RECE62-TENSU(IDX IDX2)
                  MOVE    WRK-SUM-TENSU          TO  WRK-TENSUZ
               ELSE
                  MOVE    RECE62-TENSU(IDX IDX1) TO  WRK-TENSUZ
               END-IF
               MOVE    WRK-TENSU                 TO  HC06-TENSU   (IDX)
               IF      IDX2                      >   ZERO
                   MOVE    RECE62-TENSU(IDX IDX2)
                                                 TO  WRK-KOUTENZ
                   MOVE    WRK-KOUTEN            TO  HC06-KOHTEN  (IDX)
               END-IF
           END-PERFORM
      *
      *    ９０入院            
      *        <入院基本料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
      *        <基本料>
               MOVE    RECE62-NYUKHNRYO (IDX)
                                             TO  WRK-KHNTENZ
               MOVE    WRK-KHNTEN            TO  HC06-NYUKHNTEN (IDX)
      *        <日数>
               MOVE    RECE62-NYUKHNNISSU(IDX)
                                             TO  WRK-KAISUZ
               MOVE    WRK-KAISU             TO  HC06-NYUKAISU  (IDX)
      *        <金額・公費金額>
               IF    FLG-JIRITU-HENSYU  =  1
                   COMPUTE WRK-SUM-TENSU =  RECE62-NYUKHNKEI(IDX IDX1)
                                         +  RECE62-NYUKHNKEI(IDX IDX2)
                   MOVE    WRK-SUM-TENSU     TO  WRK-TENSUZ
               ELSE
                   MOVE    RECE62-NYUKHNKEI(IDX IDX1)
                                             TO  WRK-TENSUZ
               END-IF
               MOVE    WRK-TENSU             TO  HC06-NYUTENSU  (IDX)
               IF      IDX2                  >   ZERO
                   MOVE    RECE62-NYUKHNKEI(IDX IDX2)
                                             TO  WRK-KOUTENZ
                   MOVE    WRK-KOUTEN        TO  HC06-NYUKOHTEN (IDX)
               END-IF
           END-PERFORM
      *
           IF        RECE62-NYUKHNRYO (5)    >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    4   BY  1
                         UNTIL     IDX       >   10
                         OR        RECE62-NYUKHNRYO (IDX)    =   ZERO
      *            <日数>
                   ADD     RECE62-NYUKHNNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額・公費金額>
                   IF    FLG-JIRITU-HENSYU  =  1
                     COMPUTE WRK-SUM-TENSU = RECE62-NYUKHNKEI(IDX IDX1)
                                           + RECE62-NYUKHNKEI(IDX IDX2)
                     ADD     WRK-SUM-TENSU       TO  SUM-NYUKHNKEI (1)
                   ELSE
                     ADD     RECE62-NYUKHNKEI(IDX IDX1)
                                                 TO  SUM-NYUKHNKEI (1)
                   END-IF
                   IF      IDX2                  >   ZERO
                       ADD    RECE62-NYUKHNKEI(IDX IDX2)
                                                 TO  SUM-NYUKHNKEI (2)
                   END-IF
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HC06-NYUKHNTEN (4)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISU             TO  HC06-NYUKAISU  (4)
      *        <金額・公費金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSU             TO  HC06-NYUTENSU  (4)
               IF      IDX2                  >   ZERO
                   MOVE    SUM-NYUKHNKEI (2) TO  WRK-KOUTENZ
                   MOVE    WRK-KOUTEN        TO  HC06-NYUKOHTEN (4)
               END-IF
           END-IF
      *        <入院基本料・加算>
           IF    FLG-JIRITU-HENSYU  =  1
             COMPUTE WRK-SUM-TENSU =  RECE62-NYUKHNKEI2 (IDX1)
                                   +  RECE62-NYUKHNKEI2 (IDX2)
             MOVE    WRK-SUM-TENSU            TO  WRK-TENSUZ
             MOVE    WRK-TENSU                TO  HC06-NYUTENSU  (5)
             IF      WRK-SUM-TENSU              <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
      **               MOVE    "△"        TO  HC06-NYUTENSUN (5 IDZ)
                       MOVE    "▲"        TO  HC06-NYUTENSUN (5 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
             END-IF
           ELSE
             MOVE    RECE62-NYUKHNKEI2 (IDX1) TO  WRK-TENSUZ
             MOVE    WRK-TENSU                TO  HC06-NYUTENSU  (5)
             IF      RECE62-NYUKHNKEI2 (IDX1)   <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
      **               MOVE    "△"        TO  HC06-NYUTENSUN (5 IDZ)
                       MOVE    "▲"        TO  HC06-NYUTENSUN (5 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
             END-IF
           END-IF
           IF      IDX2                    >   ZERO
               MOVE    RECE62-NYUKHNKEI2 (IDX2)
                                               TO  WRK-KOUTENZ
               MOVE    WRK-KOUTEN              TO  HC06-NYUKOHTEN  (5)
               IF      RECE62-NYUKHNKEI2 (IDX2)
                                               <   ZERO
                   PERFORM VARYING IDZ FROM    6   BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-KOUTENZ (IDZ:1) =   SPACE
      **                   MOVE    "△"    TO
                           MOVE    "▲"    TO
                                           HC06-NYUKOHTENN (5 IDZ)
                           MOVE    1           TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *     
      *        <特定入院料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
               IF      RECE62-NYUTOKNISSU(IDX) >   ZERO      
                   COMPUTE IDY     =   IDX     +   5
      *
                   MOVE    "×"              TO  HC06-KAKERU    (IDX)                    
                   MOVE    "日"              TO  HC06-NICHI     (IDX)                    
      *            <基本料>
                   IF      RECE62-NYUTOKRYO-RYAKU (IDX)
                                             =   SPACE
                       MOVE    RECE62-NYUTOKRYO (IDX)
                                             TO  WRK-KHNTENZ
                       MOVE    WRK-KHNTEN    TO  HC06-NYUKHNTEN (IDY)
                   ELSE
                       MOVE    RECE62-NYUTOKRYO-RYAKU (IDX)
                                             TO  HC06-NYUKHNTENN(IDY)
                   END-IF    
      *            <日数>
                   MOVE    RECE62-NYUTOKNISSU(IDX)
                                             TO  WRK-KAISUZ
                   MOVE    WRK-KAISU         TO  HC06-NYUKAISU  (IDY)
      *            <金額・公費金額>
                   IF    FLG-JIRITU-HENSYU  =  1
                     COMPUTE WRK-SUM-TENSU =  RECE62-NYUTOKKEI(IDX IDX1)
                                           +  RECE62-NYUTOKKEI(IDX IDX2)
                     MOVE  WRK-SUM-TENSU              TO  WRK-TENSUZ
                   ELSE
                     MOVE  RECE62-NYUTOKKEI(IDX IDX1) TO  WRK-TENSUZ
                   END-IF
                   MOVE    WRK-TENSU         TO  HC06-NYUTENSU  (IDY)
                   IF      IDX2              >   ZERO
                       MOVE    RECE62-NYUTOKKEI(IDX IDX2)
                                             TO  WRK-KOUTENZ
                       MOVE    WRK-KOUTEN    TO  HC06-NYUKOHTEN (IDY)
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF        RECE62-NYUTOKNISSU (5)  >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    4   BY  1
                         UNTIL     IDX       >   10
                         OR        RECE62-NYUTOKNISSU (IDX)  =   ZERO
      *            <日数>
                   ADD     RECE62-NYUTOKNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額・公費金額>
                   IF    FLG-JIRITU-HENSYU  =  1
                     COMPUTE WRK-SUM-TENSU = RECE62-NYUTOKKEI(IDX IDX1)
                                           + RECE62-NYUTOKKEI(IDX IDX2)
                     ADD     WRK-SUM-TENSU       TO  SUM-NYUKHNKEI (1)
                   ELSE
                     ADD     RECE62-NYUTOKKEI(IDX IDX1)
                                                 TO  SUM-NYUKHNKEI (1)
                   END-IF
                   IF      IDX2                  >   ZERO
                       ADD    RECE62-NYUTOKKEI(IDX IDX2)
                                                 TO  SUM-NYUKHNKEI (2)
                   END-IF
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HC06-NYUKHNTEN (9)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISU             TO  HC06-NYUKAISU  (9)
      *        <金額・公費金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSU             TO  HC06-NYUTENSU  (9)
               IF      IDX2                  >   ZERO
                   MOVE    SUM-NYUKHNKEI (2) TO  WRK-KOUTENZ
                   MOVE    WRK-KOUTEN        TO  HC06-NYUKOHTEN (9)
               END-IF
           END-IF
      *        <特定入院料・その他>
           IF    FLG-JIRITU-HENSYU  =  1
             COMPUTE WRK-SUM-TENSU =  RECE62-NYUTOKKEI2 (IDX1)
                                   +  RECE62-NYUTOKKEI2 (IDX2)
             MOVE    WRK-SUM-TENSU            TO  WRK-TENSUZ
             MOVE    WRK-TENSU                TO  HC06-NYUTENSU  (10)
             IF      WRK-SUM-TENSU            <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
      **               MOVE    "△"        TO
                       MOVE    "▲"        TO
                                           HC06-NYUTENSUN (10 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
             END-IF
           ELSE
             MOVE    RECE62-NYUTOKKEI2 (IDX1) TO  WRK-TENSUZ
             MOVE    WRK-TENSU                TO  HC06-NYUTENSU  (10)
             IF      RECE62-NYUTOKKEI2 (IDX1) <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
      **               MOVE    "△"        TO
                       MOVE    "▲"        TO
                                           HC06-NYUTENSUN (10 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
             END-IF
           END-IF
           IF      IDX2                    >   ZERO
               MOVE    RECE62-NYUTOKKEI2 (IDX2)
                                               TO  WRK-KOUTENZ
               MOVE    WRK-KOUTEN              TO  HC06-NYUKOHTEN  (10)
               IF      RECE62-NYUTOKKEI2 (IDX2)
                                               <   ZERO
                   PERFORM VARYING IDZ FROM    6   BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-KOUTENZ (IDZ:1) =   SPACE
      **                   MOVE    "△"        TO
                           MOVE    "▲"        TO
                                           HC06-NYUKOHTENN (10 IDZ)
                           MOVE    1           TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *        <入院年月日>
           MOVE    RECE62-NYUINYMD     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HC06-NYUINYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HC06-NYUINMM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  HC06-NYUINDD
      *
      *        <医療機関種別>
           EVALUATE    RECE62-HOSPSBT
               WHEN    "1"
                   MOVE    "病"        TO  HC06-HOSPSBT
               WHEN    "2"
                   MOVE    "診"        TO  HC06-HOSPSBT
           END-EVALUATE        
      *
      *       <入院基本料種別>
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       11
                   OR      RECE62-NYUSYUBETU (IDX) =   SPACE
               MOVE    RECE62-NYUSYUBETU (IDX)
                                     TO  HC06-NYUSYUBETU (IDX)
           END-PERFORM
      *
      *    ９７食事            
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   3
               MOVE    RECE62-SHOKUJISYUBETU (IDX)
                                             TO  HC06-SYOKJN  (IDX)          
           END-PERFORM          
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
               IF      RECE62-SHOKUJIKHNRYO (IDX)    >   ZERO      
                   MOVE    RECE62-SHOKUJIKHNRYO (IDX)
                                             TO  WRK-KGK4
                   MOVE    WRK-KGK4          TO  HC06-SYOGAK (IDX)          
                   MOVE    RECE62-SHOKUJIKHNNISSU (IDX)
                                             TO  WRK-KGK2
                   MOVE    WRK-KGK2          TO  HC06-SYOKAISU (IDX)
               END-IF                                        
           END-PERFORM          
      *                                                                 
      *        <基準（生）、特別（生）>
           MOVE     RECE62-SHOKUJISYUBETU-KHN
                                   TO  HC06-SYOKJN-KHN
           MOVE     RECE62-SHOKUJISYUBETU-KHNSEI
                                   TO  HC06-SYOKJN-KHNSEI         
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   2
               IF      RECE62-SHOKUJIKHNRYO-SEI (IDX)  >   0
                   MOVE    RECE62-SHOKUJIKHNRYO-SEI (IDX)
                                             TO  WRK-KGK4
                   MOVE    WRK-KGK4          TO  HC06-SYOGAK-SEI   (IDX)          
                   MOVE    RECE62-SHOKUJIKHNNISSU-SEI (IDX)
                                             TO  WRK-KGK2
                   MOVE    WRK-KGK2          TO  HC06-SYOKAISU-SEI (IDX)
               END-IF 
           END-PERFORM          
      *                                                                
      *                                                                 
      *        <減、免、猶、低所得１、低所得２、３月超>            
      *                                                                
           IF      RECE62-TEISYOTOKU   =   SPACE                       
               CONTINUE                                                
           ELSE                                                        
               EVALUATE    RECE62-TEISYOTOKU                           
                   WHEN    "1"                                         
                   WHEN    "4"                                         
                      MOVE    "低所得１"  TO  HC06-GENGAKUMEI          
                   WHEN    "2"                                         
                      MOVE    "低所得２"  TO  HC06-GENGAKUMEI          
                   WHEN    "3"                                         
                      MOVE    "低所得２　３月超"
                                          TO  HC06-GENGAKUMEI          
               END-EVALUATE                                            
           END-IF                                                      
      *
      *        <請求点・一部負担金・食事日数・請求額・標準負担額>
      *
           IF  WRK-3SYU-KBN           =   ALL "1" AND 
               RECE62-HKNNUM          =   SPACE            
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
           END-IF    
      *    保険（減免）
           EVALUATE    RECE62-GENMENKBN
               WHEN    1
                   IF      RECE62-GENGAKU      >   ZERO
                       MOVE    RECE62-GENGAKU      TO  WRK-KGK6
                       MOVE    WRK-KGK6            TO  HC06-GENMENKIN
                       MOVE    "減額"              TO  HC06-GENMEN
                       MOVE    "円"                TO  HC06-GENMEN2
                   ELSE
                       COMPUTE WRK-KYURATE-9   =
                                       RECE62-GENGAKURATE  /   10 
                       MOVE    WRK-KYURATE-9   TO  WRK-KYURATE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    WRK-KYURATE     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  HC06-GENMEN(5:)
                       END-IF    
                       MOVE   "減額"               TO  HC06-GENMEN(1:4)
                       MOVE   "割"                 TO  HC06-GENMEN(9:2)
                   END-IF
               WHEN    2
                   MOVE    "免除"             TO  HC06-GENMEN
               WHEN    3
                   MOVE    "支払猶予"         TO  HC06-GENMEN
           END-EVALUATE 
      *    保険
           IF      RECE62-TOTALTEN (IDX1)     >   ZERO
               MOVE    RECE62-TOTALTEN (IDX1) TO  WRK-KGK10
               MOVE    WRK-KGK10              TO  HC06-TOTALTEN(1)
               IF    ( RECE62-FTNMONEY (IDX1) >    ZERO   )
               OR    ( RECE62-KOUREI          = "1" OR "2")
                   MOVE    RECE62-FTNMONEY (IDX1) TO  WRK-KGK7
                   IF      RECE62-GENMENKBN   =   ZERO
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY(1)
                   ELSE
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY-GEN
                   END-IF
               END-IF
           END-IF
           IF      RECE62-KOUREI-SAIKEI  >   ZERO
               MOVE    "("                    TO  HC06-FTNMONEYSAI(1:1)
               MOVE   RECE62-KOUREI-SAIKEI   TO  WRK-KGK7
               MOVE   WRK-KGK7                TO  HC06-FTNMONEYSAI(3:8)
               MOVE    ")"                    TO
                                              HC06-FTNMONEYSAI(11:1)
           END-IF    
      *    IF      RECE62-SHOKUJINISSU (IDX1)   >   ZERO
           IF      RECE62-SHOKUJIRYOYOHI(IDX1)  >   ZERO
               IF      RECE62-SHOKUJINISSU (IDX1)  >  ZERO
                   MOVE    RECE62-SHOKUJINISSU (IDX1)
                                               TO  WRK-KGK2
                   MOVE    WRK-KGK2            TO  HC06-SYONISSU(1)
               END-IF
               MOVE    RECE62-SHOKUJIRYOYOHI (IDX1) TO  WRK-KGK6
               MOVE    WRK-KGK6                TO  HC06-SYOTOTALTEN(1)
               IF      RECE62-SHOKUJIFTN (IDX1) >   ZERO
                   MOVE    RECE62-SHOKUJIFTN (IDX1)
                                               TO  WRK-KGK7
                   MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY(1)
               END-IF
           END-IF
      *    公費１
           IF    ( RECE62-TOTALTEN (IDX2) >   ZERO                     )
           AND   ((RECE62-TOTALTEN (IDX2)
                                      NOT =   RECE62-TOTALTEN (IDX1)) OR
                  (RECE62-FTNMONEY (IDX2)
                                      NOT =   RECE62-FTNMONEY (IDX1))  )
               IF    RECE62-FTNMONEY (IDX2)   >   ZERO
                   MOVE    RECE62-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO  HC06-FTNMONEY(2)
               END-IF
           END-IF
           IF    ( RECE62-TOTALTEN (IDX2) >   ZERO                  )
           AND   ( RECE62-FTNMONEY (IDX2) =   RECE62-FTNMONEY (IDX1))
           AND   ( RECE62-KISAIFLG (IDX2) =   "1"                   )
               IF    RECE62-FTNMONEY (IDX2)   >   ZERO
                   MOVE    RECE62-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO  HC06-FTNMONEY(2)
               END-IF
           END-IF
           IF    ( RECE62-TOTALTEN (IDX2)     >   ZERO )
           AND   ( RECE62-FTNMONEY (IDX2)     =   ZERO )
           AND   ( RECE62-ZEROKISAIFLG (IDX2) =   "1"  )
                   MOVE    RECE62-FTNMONEY (IDX2) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO  HC06-FTNMONEY(2)
           END-IF
           IF      RECE62-TOTALTEN (IDX2) >   ZERO                     
           AND     RECE62-TOTALTEN (IDX2)
                                      NOT =   RECE62-TOTALTEN (IDX1)   
               MOVE    RECE62-TOTALTEN (IDX2) TO  WRK-KGK10
               MOVE    WRK-KGK10              TO  HC06-TOTALTEN(2)
           END-IF
           IF    ((RECE62-SRYYM          >=  "200610"  ) AND
                  (SYS-1001-PREFNUM       =    33      ) AND
                  (RECE62-KOHNUM(1)(1:1)
                                    >=  "1" AND <=  "8") AND
                  (RECE62-TOTALTEN (IDX2) >   ZERO     ))
           OR    ((RECE62-SRYYM          >=  "200510"  ) AND
                  (SYS-1001-PREFNUM       =    01      ) AND
                  (RECE62-KOHNUM(1)       =  "051"     ) AND
                  (RECE62-KOHJKYSNUM(1)(1:2)  =
                   "91" OR "92" OR "93" OR "94" OR "95") AND
                  (RECE62-TOTALTEN (IDX2) >   ZERO     ))
               MOVE    RECE62-TOTALTEN (IDX2) TO  WRK-KGK10
               MOVE    WRK-KGK10              TO  HC06-TOTALTEN(2)
           END-IF
      *         
      *    IF    ( RECE62-SHOKUJINISSU (IDX2) >   ZERO                 )
           IF    ( RECE62-SHOKUJIRYOYOHI(IDX2) >   ZERO                )
           AND   ((RECE62-SHOKUJINISSU (IDX2)     NOT =
                                      RECE62-SHOKUJINISSU   (IDX1)) OR
                  (RECE62-SHOKUJIRYOYOHI (IDX2)   NOT =
                                      RECE62-SHOKUJIRYOYOHI (IDX1)) OR
                  (RECE62-SHOKUJIFTN (IDX2)       NOT =
                                      RECE62-SHOKUJIFTN     (IDX1))    )
               IF      RECE62-SHOKUJINISSU (IDX2)  >  ZERO
                   MOVE    RECE62-SHOKUJINISSU (IDX2)
                                           TO  WRK-KGK2 
                   MOVE    WRK-KGK2        TO  HC06-SYONISSU(2)
               END-IF
               MOVE    RECE62-SHOKUJIRYOYOHI (IDX2)
                                           TO  WRK-KGK6
               MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN(2)
               IF      RECE62-SHOKUJIFTN (IDX2) >   ZERO
                   MOVE    RECE62-SHOKUJIFTN (IDX2)
                                           TO  WRK-KGK7
                   MOVE    WRK-KGK7        TO  HC06-SYOFTNMONEY(2)
               END-IF
           ELSE
               IF      RECE62-FUKUSHIFLG-SKJ(1)   =    "1"
                 IF   (SYS-1001-PREFNUM  =  04                    )
                 AND  (RECE62-KOHNUM(1)  = "183" OR "383" OR "483")
                     MOVE    RECE62-SHOKUJINISSU (IDX2)
                                                 TO  WRK-KGK2 
                     MOVE    WRK-KGK2            TO  HC06-SYONISSU(2)
                 ELSE
                     MOVE    RECE62-SHOKUJINISSU (IDX2)
                                                 TO  WRK-KGK2 
                     MOVE    WRK-KGK2            TO  HC06-SYONISSU(2)
                     MOVE    RECE62-SHOKUJIRYOYOHI (IDX2)
                                                 TO  WRK-KGK6
                     MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN(2)
                     MOVE    RECE62-SHOKUJIFTN (IDX2)
                                                 TO  WRK-KGK7
                     MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY(2)
                 END-IF
               END-IF
           END-IF
      *     
      *    公費２
           IF       RECE62-KOH-SAIKEIFLG   =   1
             MOVE    "("                    TO  HC06-FTNMONEY1(3)(1:1)
             MOVE   RECE62-KOH-SAIKEI       TO  WRK-KGK6
             MOVE   WRK-KGK6                TO  HC06-FTNMONEY1(3)(2:7)
             MOVE    ")"                    TO  HC06-FTNMONEY1(3)(9:1)
      *
             MOVE   RECE62-FTNMONEY (IDX3)  TO  WRK-KGK8
             MOVE   WRK-KGK8                TO  HC06-FTNMONEY1(4)
           ELSE
             IF  ( RECE62-TOTALTEN (IDX3) >   ZERO                     )
             AND ((RECE62-TOTALTEN (IDX3)
                                      NOT =   RECE62-TOTALTEN (IDX1)) OR
                  (RECE62-FTNMONEY (IDX3)
                                      NOT =   RECE62-FTNMONEY (IDX1)) OR
                  (RECE62-FTNMONEY (IDX3)
                                      NOT =   RECE62-FTNMONEY (IDX2))  )
      ***        IF    RECE62-FTNMONEY (IDX3)
      ***                             NOT =   RECE62-FTNMONEY (IDX2)
                   IF  RECE62-FTNMONEY (IDX3)   >   ZERO
                       MOVE    RECE62-FTNMONEY (IDX3)
                                           TO  WRK-KGK7
                       MOVE    WRK-KGK7    TO  HC06-FTNMONEY(3)
                   END-IF
      ***        END-IF
             END-IF
             IF    ( RECE62-TOTALTEN (IDX3) >   ZERO                  )
             AND   ( RECE62-FTNMONEY (IDX3) =   RECE62-FTNMONEY (IDX2))
             AND   ( RECE62-KISAIFLG (IDX3) =   "1"                   )
                 IF    RECE62-FTNMONEY (IDX3)   >   ZERO
                   MOVE    RECE62-FTNMONEY (IDX3) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO  HC06-FTNMONEY(3)
                 END-IF
             END-IF
             IF    ( RECE62-TOTALTEN (IDX3)     >   ZERO )
             AND   ( RECE62-FTNMONEY (IDX3)     =   ZERO )
             AND   ( RECE62-ZEROKISAIFLG (IDX3) =   "1"  )
                   MOVE    RECE62-FTNMONEY (IDX3) TO  WRK-KGK7
                   MOVE    WRK-KGK7               TO  HC06-FTNMONEY(3)
             END-IF
           END-IF
           IF      RECE62-TOTALTEN (IDX3) >   ZERO                      
           AND     RECE62-TOTALTEN (IDX3)
                                      NOT =   RECE62-TOTALTEN (IDX2)
               MOVE    RECE62-TOTALTEN (IDX3) TO  WRK-KGK10
               MOVE    WRK-KGK10              TO  HC06-TOTALTEN(3)
           ELSE
               IF      RECE62-FUKUSHIFLG      =   "1"
               AND     RECE62-TOTALTEN (IDX3) =   ZERO
                   MOVE    RECE62-TOTALTEN (IDX3) TO  WRK-KGK10
                   MOVE    WRK-KGK10              TO  HC06-TOTALTEN(3)
               END-IF
           END-IF
           IF    ((RECE62-SRYYM          >=  "200610"  ) AND
                  (SYS-1001-PREFNUM       =    33      ) AND
                  (RECE62-KOHNUM(2)(1:1)
                                    >=  "1" AND <=  "8") AND
                  (RECE62-TOTALTEN (IDX3) >   ZERO     ))
           OR    ((RECE62-SRYYM          >=  "200510"  ) AND
                  (SYS-1001-PREFNUM       =    01      ) AND
                  (RECE62-KOHNUM(2)       =  "051"     ) AND
                  (RECE62-KOHJKYSNUM(2)(1:2)  =
                   "91" OR "92" OR "93" OR "94" OR "95") AND
                  (RECE62-TOTALTEN (IDX3) >   ZERO     ))
           OR    ((RECE62-SRYYM          >=  "200510"  ) AND
                  (SYS-1001-PREFNUM       =    01      ) AND
                  (RECE62-KOHNUM(1)       =  "051"     ) AND
                  (RECE62-KOHJKYSNUM(1)(1:2)  =
                   "91" OR "92" OR "93" OR "94" OR "95") AND
                  (RECE62-TOTALTEN (IDX2) >   ZERO     ) AND
                  (RECE62-KOHHBTNUM(2)    =  "41"      ) AND
                  (RECE62-TOTALTEN (IDX3) >   ZERO     ))
               MOVE    RECE62-TOTALTEN (IDX3) TO  WRK-KGK10
               MOVE    WRK-KGK10              TO  HC06-TOTALTEN(3)
           END-IF
      *         
      *    IF    ( RECE62-SHOKUJINISSU (IDX3) >   ZERO                 )
           IF    ( RECE62-SHOKUJIRYOYOHI(IDX3) >   ZERO                )
           AND   ((RECE62-SHOKUJINISSU (IDX3)     NOT =
                                      RECE62-SHOKUJINISSU   (IDX2)) OR
                  (RECE62-SHOKUJIRYOYOHI (IDX3)   NOT =
                                      RECE62-SHOKUJIRYOYOHI (IDX2)) OR
                  (RECE62-SHOKUJIFTN (IDX3)       NOT =
                                      RECE62-SHOKUJIFTN     (IDX2))    )
               IF      RECE62-SHOKUJINISSU (IDX3)  >  ZERO
                   MOVE    RECE62-SHOKUJINISSU (IDX3)
                                           TO  WRK-KGK2
                   MOVE    WRK-KGK2        TO  HC06-SYONISSU(3)
               END-IF
               MOVE    RECE62-SHOKUJIRYOYOHI (IDX3)
                                           TO  WRK-KGK6
               MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN(3)
               IF      RECE62-SHOKUJIFTN (IDX3) >   ZERO
                   MOVE    RECE62-SHOKUJIFTN (IDX3)
                                           TO  WRK-KGK7
                   MOVE    WRK-KGK7        TO  HC06-SYOFTNMONEY(3)
               END-IF
           ELSE
               IF      RECE62-FUKUSHIFLG-SKJ(2)   =    "1"
                 IF   (SYS-1001-PREFNUM  =  04                    )
                 AND  (RECE62-KOHNUM(2)  = "183" OR "383" OR "483")
                     MOVE    RECE62-SHOKUJINISSU (IDX3)
                                                 TO  WRK-KGK2 
                     MOVE    WRK-KGK2            TO  HC06-SYONISSU(3)
                 ELSE
                     MOVE    RECE62-SHOKUJINISSU (IDX3)
                                                 TO  WRK-KGK2 
                     MOVE    WRK-KGK2            TO  HC06-SYONISSU(3)
                     MOVE    RECE62-SHOKUJIRYOYOHI (IDX3)
                                                 TO  WRK-KGK6
                     MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN(3)
                     MOVE    RECE62-SHOKUJIFTN (IDX3)
                                                 TO  WRK-KGK7
                     MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY(3)
                 END-IF
               END-IF
           END-IF
      *
      *    県単用項目
           IF      FLG-RECE67          =   ZERO
      *        保険
               IF      RECE67-TOTALTEN (IDX1)  >   ZERO
                   IF      RECE67-TOTALTEN (IDX1)  =   99999999
                       MOVE    ZERO                    TO  WRK-KGK10
                   ELSE
                       MOVE    RECE67-TOTALTEN (IDX1)  TO  WRK-KGK10
                   END-IF
                   MOVE    WRK-KGK10               TO  HC06-TOTALTEN(1)
               END-IF
               IF      RECE67-FTNMONEY (IDX1)  >   ZERO
                   IF      RECE67-FTNMONEY (IDX1)  =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-FTNMONEY (IDX1)  TO  WRK-KGK7
                   END-IF
                   IF      RECE62-GENMENKBN    =   ZERO
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY(1)
                   ELSE
                       MOVE    WRK-KGK7           TO  HC06-FTNMONEY-GEN
                   END-IF
               END-IF
               IF      RECE67-SHOKUJINISSU (IDX1)  >   ZERO
                   IF      RECE67-SHOKUJINISSU (IDX1)  =   99
                       MOVE    ZERO                    TO  WRK-KGK2
                   ELSE
                       MOVE    RECE67-SHOKUJINISSU (IDX1)
                                                       TO  WRK-KGK2
                   END-IF
                   MOVE    WRK-KGK2                TO  HC06-SYONISSU(1)
               END-IF
               IF      RECE67-SHOKUJIRYOYOHI (IDX1)
                                                   >   ZERO
                   IF      RECE67-SHOKUJIRYOYOHI (IDX1)
                                                   =   999999999
                       MOVE    ZERO                    TO  WRK-KGK6
                   ELSE
                       MOVE    RECE67-SHOKUJIRYOYOHI (IDX1)
                                                       TO  WRK-KGK6
                   END-IF
                   MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN (1)
               END-IF
               IF      RECE67-SHOKUJIFTN (IDX1)    >   ZERO
                   IF      RECE67-SHOKUJIFTN (IDX1)    =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-SHOKUJIFTN (IDX1)
                                                       TO  WRK-KGK7
                   END-IF 
                   MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY (1)
               END-IF
      *        公費１
               IF      RECE67-TOTALTEN (IDX2)  >   ZERO                     
                   IF      RECE67-TOTALTEN (IDX2)  =   99999999
                       MOVE    ZERO                    TO  WRK-KGK10
                   ELSE
                       MOVE    RECE67-TOTALTEN (IDX2)  TO  WRK-KGK10
                   END-IF
                   MOVE    WRK-KGK10               TO  HC06-TOTALTEN(2)
               END-IF
               IF    RECE67-FTNMONEY (IDX2)    >   ZERO
                   IF      RECE67-FTNMONEY (IDX2)  =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-FTNMONEY (IDX2)  TO  WRK-KGK7
                   END-IF
                   MOVE    WRK-KGK7                TO  HC06-FTNMONEY(2)
               END-IF
               IF     RECE67-SHOKUJINISSU (IDX2)   >   ZERO 
                   IF      RECE67-SHOKUJINISSU (IDX2)  =   99
                       MOVE    ZERO                    TO  WRK-KGK2
                   ELSE
                       MOVE    RECE67-SHOKUJINISSU (IDX2)
                                                       TO  WRK-KGK2
                   END-IF
                   MOVE    WRK-KGK2                TO  HC06-SYONISSU(2)
               END-IF
               IF      RECE67-SHOKUJIRYOYOHI (IDX2)
                                               >   ZERO
                   IF      RECE67-SHOKUJIRYOYOHI (IDX2)
                                                       =   999999999
                       MOVE    ZERO                    TO  WRK-KGK6
                   ELSE
                       MOVE    RECE67-SHOKUJIRYOYOHI (IDX2)
                                                       TO  WRK-KGK6
                   END-IF
                   MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN (2)
               END-IF
               IF      RECE67-SHOKUJIFTN (IDX2)    >   ZERO
                   IF      RECE67-SHOKUJIFTN (IDX2)    =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-SHOKUJIFTN (IDX2)
                                                       TO  WRK-KGK7
                   END-IF 
                   MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY (2)
               END-IF
      *        公費２
               IF      RECE67-TOTALTEN (IDX3)  >   ZERO                      
                   IF      RECE67-TOTALTEN (IDX3)  =   99999999
                       MOVE    ZERO                    TO  WRK-KGK10
                   ELSE
                       MOVE    RECE67-TOTALTEN (IDX3)  TO  WRK-KGK10
                   END-IF
                   MOVE    WRK-KGK10               TO  HC06-TOTALTEN(3)
               END-IF
               IF      RECE67-FTNMONEY (IDX3)  >   ZERO
                   IF      RECE67-FTNMONEY (IDX3)  =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-FTNMONEY (IDX3)  TO  WRK-KGK7
                   END-IF
                   MOVE    WRK-KGK7                TO  HC06-FTNMONEY(3)
               END-IF
               IF     RECE67-SHOKUJINISSU (IDX3)   >   ZERO
                   IF      RECE67-SHOKUJINISSU (IDX3)  =   99
                       MOVE    ZERO                    TO  WRK-KGK2
                   ELSE
                       MOVE    RECE67-SHOKUJINISSU (IDX3)
                                                       TO  WRK-KGK2
                   END-IF 
                   MOVE    WRK-KGK2                TO  HC06-SYONISSU(3)
               END-IF
               IF      RECE67-SHOKUJIRYOYOHI (IDX3)
                                               >   ZERO
                   IF      RECE67-SHOKUJIRYOYOHI (IDX3)
                                                   =   999999999
                       MOVE    ZERO                    TO  WRK-KGK6
                   ELSE
                       MOVE    RECE67-SHOKUJIRYOYOHI (IDX3)
                                                       TO  WRK-KGK6
                   END-IF
                   MOVE    WRK-KGK6            TO  HC06-SYOTOTALTEN(3)
               END-IF
               IF      RECE67-SHOKUJIFTN (IDX3)    >   ZERO
                   IF      RECE67-SHOKUJIFTN (IDX3)    =   999999999
                       MOVE    ZERO                    TO  WRK-KGK7
                   ELSE
                       MOVE    RECE67-SHOKUJIFTN (IDX3)
                                                       TO  WRK-KGK7
                   END-IF 
                   MOVE    WRK-KGK7            TO  HC06-SYOFTNMONEY (3)
               END-IF
           END-IF
      *
      *    宮崎地方公費対応
           IF    ( RECE62-PREFNUM      =   "45"                  )
           AND   ( RECE62-SRYYM        >=  WRK-CONS-SRYYM-200709 )
           AND   ( RECE62-TEISYUTUSAKI =   "2"                   )
               IF      RECE62-KOHNUM (1)   =   "190"
                   MOVE    SPACE               TO  HC06-TOTALTEN(2)
                                                   HC06-FTNMONEY(2)
                                                   HC06-SYONISSU(2)
                                                   HC06-SYOTOTALTEN (2)
                                                   HC06-SYOFTNMONEY (2)
               END-IF
               IF      RECE62-KOHNUM (2)   =   "190"
                   MOVE    SPACE               TO  HC06-TOTALTEN(3)
                                                   HC06-FTNMONEY(3)
                                                   HC06-SYONISSU(3)
                                                   HC06-SYOTOTALTEN (3)
                                                   HC06-SYOFTNMONEY (3)
               END-IF
           END-IF
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<剤>処理
      *****************************************************************
       340-HC101-ZAI-HEN-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-LINE
      *
           IF    ( RECE62-TEISYOTOKU   =   "4" )
           AND   ( RECE62-SEIKATURYOYO =   "1" )
      *        摘要欄に病名が表示されている場合、１行あける
               IF  WRK-LINETEK             >   ZERO
      *
                   IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   MOVE    1           TO  FLG-KUGIRI
                   PERFORM 34110-KUGIRI-LINE-SEC
                   MOVE    ZERO        TO  FLG-KUGIRI
                   MOVE    1           TO  FLG-LINE   
               END-IF
      *
               IF  WRK-LINETEK           >   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
               IF      CNT-PAGE        =   1   
                   MOVE    "老福"          TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE
                   EVALUATE    SYS-2005-NEXTPRTKBN-G
                       WHEN    "0"   
                           MOVE    "老福"  TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "1"   
                           MOVE    "老福"  TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "2"   
                           MOVE    "老福"  TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF    
      *
               MOVE    ZERO            TO  FLG-LINE
           END-IF
      *         
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  WRK-R061-KOHBUNRI-O
           MOVE    ZERO                TO  CNT-RECE661
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE661-REC
           MOVE    RECE62-HOSPID       TO  RECE661-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE661-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE661-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE661-PTID
           MOVE    RECE62-RECEKA       TO  RECE661-RECEKA
           MOVE    RECE62-TEISYUTUSAKI TO  RECE661-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE661-RECESYUBETU
           MOVE    RECE62-HKNJANUM-KEY TO  RECE661-HKNJANUM
           MOVE    RECE62-HOJOKBN-KEY  TO  RECE661-HOJOKBN
           MOVE    RECE62-TEKSTYMD     TO  RECE661-TEKSTYMD
      *
           START   RECE661-FILE
               KEY IS    >=   RECE661-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE661
               NOT INVALID
                   PERFORM 900-RECE661-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE661           =   1 )
               OR     ( RECE62-HOSPID    NOT =   RECE661-HOSPID      )
               OR     ( RECE62-SRYYM     NOT =   RECE661-SRYYM       )
               OR     ( RECE62-NYUGAIKBN NOT =   RECE661-NYUGAIKBN   )
               OR     ( RECE62-PTID      NOT =   RECE661-PTID        )
               OR     ( RECE62-RECEKA    NOT =   RECE661-RECEKA      )
               OR     ( RECE62-TEISYUTUSAKI
                                         NOT =   RECE661-TEISYUTUSAKI)
               OR     ( RECE62-RECESYUBETU
                                         NOT =   RECE661-RECESYUBETU )
               OR     ( RECE62-HKNJANUM-KEY
                                         NOT =   RECE661-HKNJANUM    )
               OR     ( RECE62-HOJOKBN-KEY
                                         NOT =   RECE661-HOJOKBN     )
               OR     ( RECE62-TEKSTYMD  NOT =   RECE661-TEKSTYMD    )
      *
      *        適用欄に剤の記載が必要なとき区切り線がまだ記載されていないとき
               IF      FLG-LINE            =   ZERO
      *            摘要欄に記載がある場合、１行あける
                   IF  WRK-LINETEK             >   ZERO
      *
                       IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       MOVE    1           TO  FLG-KUGIRI
                       PERFORM 34110-KUGIRI-LINE-SEC
                       MOVE    ZERO        TO  FLG-KUGIRI
                   END-IF
                   MOVE    1           TO  FLG-LINE
               END-IF
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE661-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE661-HENSYU-SEC
      *
               MOVE    RECE661-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE661-SRYKBN        TO  WRK-R061-SRYKBN-O
               MOVE    RECE661-KOHBUNRI      TO  WRK-R061-KOHBUNRI-O
      *
               PERFORM 900-RECE661-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE661-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                  TO   FLG-KUGIRI
      *
           IF  CNT-ZAIMEISAISU           =   1
               IF  CNT-RECE661           >   1
      *            公費分離区分が変更している場合、公費区切り線分を追加
                   IF  RECE661-KOHBUNRI  NOT =   WRK-R061-KOHBUNRI-O
                       MOVE    2             TO  FLG-KUGIRI
                   ELSE
      *                診療区分が変更している場合、診療区切り線分を追加
                       IF  RECE661-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                           MOVE    1             TO  FLG-KUGIRI
                       END-IF
                   END-IF
               END-IF
           END-IF    
      *
           IF      WRK-LINETEK          >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
      *
      *        区切り線（改頁後には診療区切り線は表示しない）
               IF      FLG-KUGIRI        =   2
                   PERFORM 34110-KUGIRI-LINE-SEC
               END-IF
           ELSE
      *        区切り線
               IF      FLG-KUGIRI    NOT =   ZERO
                   PERFORM 34110-KUGIRI-LINE-SEC
               END-IF
           END-IF    
      *
      *    診療区分
           IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
           END-IF
      *              
           ADD     1                 TO  WRK-LINETEK
      *
           MOVE    ZERO                  TO  FLG-SRYKBNPRT
           IF      CNT-PAGE          >   1 
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       IF      WRK-LINETEK     =   36  OR  71  OR  106
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
                   WHEN    "1"
                       IF      WRK-LINETEK     =   56
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
                   WHEN    "2"
                       IF      WRK-LINETEK     =   70
                           MOVE    1               TO  FLG-SRYKBNPRT
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( FLG-KUGIRI        =   1 OR 2)
           OR    ( WRK-LINETEK       =   1 )
           OR    ( CNT-RECE661       =   1 )
           OR    ( FLG-SRYKBNPRT     =   1 ) 
               IF      CNT-PAGE        =   1   
                   MOVE    RECE661-SRYKBN  TO  HC06-SRYKBN
                                                       (WRK-LINETEK)
               ELSE                                    
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    RECE661-SRYKBN  TO  HC061-SRYKBN
                                                       (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    RECE661-SRYKBN  TO  HC062-SRYKBN
                                                       (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    RECE661-SRYKBN  TO  HC063-SRYKBN
                                                       (WRK-LINETEK)
                   END-EVALUATE
               END-IF
           END-IF                                    
      *
      *    明細
           INSPECT RECE661-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1   
               MOVE    RECE661-TEKIYO-INF  TO  HC06-TEKIYO (WRK-LINETEK)
           ELSE    
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       MOVE    RECE661-TEKIYO-INF
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "1"
                       MOVE    RECE661-TEKIYO-INF
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "2"
                       MOVE    RECE661-TEKIYO-INF
                                           TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF  
      *
           .
       3410-RECE661-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           EVALUATE    FLG-KUGIRI
           WHEN    2
               PERFORM 34110-KOHNAME-SET-SEC
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-KUGIRI2 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE    
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    WRK-KUGIRI2 
                                       TO  HC061-TEKIYO (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-KUGIRI2 
                                       TO  HC062-TEKIYO (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-KUGIRI2 
                                       TO  HC063-TEKIYO (WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           WHEN    1
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-KUGIRI1 TO  HC06-TEKIYO  (WRK-LINETEK)
               ELSE    
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    WRK-KUGIRI1
                                       TO  HC061-TEKIYO (WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-KUGIRI1
                                       TO  HC062-TEKIYO (WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-KUGIRI1
                                       TO  HC063-TEKIYO (WRK-LINETEK)
                   END-EVALUATE
               END-IF    
           END-EVALUATE
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
           PERFORM 390-PRINT-OUT-SEC
      *     
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HCN061-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
      *    続紙の最大行をセット     
           IF      CNT-PAGE        =   1   
               MOVE    WRK-TEKIYOU1-LINE TO  WRK-TEKIYOU-LINE
           ELSE    
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       MOVE    WRK-TEKIYOU2-LINE TO  WRK-TEKIYOU-LINE
                   WHEN    "1"
                       MOVE    WRK-TEKIYOU3-LINE TO  WRK-TEKIYOU-LINE
                   WHEN    "2"
                       MOVE    WRK-TEKIYOU4-LINE TO  WRK-TEKIYOU-LINE
               END-EVALUATE
           END-IF    
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　公費名セット処理
      *****************************************************************
       34110-KOHNAME-SET-SEC          SECTION.
      *
           MOVE    SPACE              TO  WRK-KUGIRI2-KOHNAME
      *
           MOVE    SPACE              TO  HKNNUM-REC
           MOVE    SYS-1001-HOSPID    TO  HKN-HOSPID
           MOVE    RECE62-KOHNUM (RECE661-KOHBUNRI)
                                      TO  HKN-HKNNUM
           MOVE    WRK-SRYYMD         TO  HKN-TEKSTYMD
                                          HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
           MOVE    "HKNNUM-KEY5"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =   ZERO
               MOVE    "HKNNUM-KEY5"  TO  ORC-DBPATH
               PERFORM 910-HKNMST-NEXT-SEC
           ELSE
               MOVE    1              TO  FLG-HKNNUM
           END-IF
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
           MOVE    "HKNNUM-KEY5"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           IF      FLG-HKNNUM         =   ZERO   
               MOVE    HKN-TANSEIDONAME  TO  WRK-KUGIRI2-KOHNAME
               INSPECT WRK-KUGIRI2-KOHNAME
                       REPLACING    ALL   "  "   BY   "　"
           END-IF
      *
           .
       34110-KOHNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<補足>処理
      *****************************************************************
       350-HC101-HOSOKU-HEN-SEC      SECTION.
      *
      *    県単用項目
           IF      FLG-RECE67           =   ZERO
               INSPECT RECE67-HC06-BODY     REPLACING   ALL "  "
                                                        BY  "　" 
               INSPECT RECE67-HC06-FOOT     REPLACING   ALL "  "
                                                        BY  "　" 
               MOVE    RECE67-HC06-BODY     TO  HC06-BODY
               MOVE    RECE67-HC06-FOOT     TO  HC06-FOOT
           END-IF
      *
           .
       350-HC101-HOSOKU-HEN-EXT.
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
           MOVE    RECE62-HOSPID       TO  RECECOM-HOSPID
           MOVE    RECE62-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE62-PTID         TO  RECECOM-PTID
           MOVE    RECE62-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE62-HKNCOMBI (IDY) =   ZERO
                   IF      RECECOM-HKNCOMBI    =   RECE62-HKNCOMBI(IDY)
                       PERFORM 3601-COMMENT-HENSYU-SEC                 
                       MOVE    10                  TO  IDY
                   END-IF
               END-PERFORM
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
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
           IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
      *
               MOVE    SPACE           TO  WRK-RECECOM-NAME
               EVALUATE    WRK-RECECOM-SJKBN
                   WHEN    "01"    THRU    "07"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    RECECOM-SJKBN   TO  WRK-SJKBN-X
                       MOVE    TBL-RECECOM-NAME (WRK-SJKBN)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "50"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (8)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "51"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (9)
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
                               UNTIL   IDX >       800
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1     =   IDX     -   1
                               COMPUTE IDY2     =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                                TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                                TO  WRK-COMM1
                               MOVE    800      TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G   >   SPACE
      *                摘要欄に１行あける
                     IF      WRK-RECECOM         =   ZERO
                       MOVE    1               TO  WRK-RECECOM
                       IF  WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           ELSE
                               MOVE    1           TO  FLG-KUGIRI
                               PERFORM 34110-KUGIRI-LINE-SEC
                               MOVE    ZERO        TO  FLG-KUGIRI   
                           END-IF
                       END-IF
                     END-IF
      *
      *              症状詳記区分の見出しを記載
                     IF      FLG-RECECOM-SJKBN   =   1
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-RECECOM-NAME
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       ELSE                
                           EVALUATE    SYS-2005-NEXTPRTKBN
                               WHEN    "0"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "1"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    WRK-RECECOM-NAME
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       END-IF
                       MOVE    ZERO            TO  FLG-RECECOM-SJKBN
                     END-IF            
      *                 
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       16
                           OR      WRK-COMMENT (IDW)   =   SPACE
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-COMMENT (IDW)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                       ELSE                
                           EVALUATE    SYS-2005-NEXTPRTKBN
                               WHEN    ZERO
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                               WHEN    "1"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                               WHEN    "2"
                                   MOVE    WRK-COMMENT (IDW)
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                           END-EVALUATE
                       END-IF                
                     END-PERFORM
                 END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       3601-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<保険履歴情報>処理
      *****************************************************************
       370-HC101-HKN-RIREKI-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-RIREKI-MAX  
      *        摘要欄に１行あける
               IF      IDXX                =   1
                   IF  WRK-LINETEK             >   ZERO
                       IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       ELSE
                           MOVE    1               TO  FLG-KUGIRI
                           PERFORM 34110-KUGIRI-LINE-SEC
                           MOVE    ZERO            TO  FLG-KUGIRI
                       END-IF
                   END-IF
               END-IF
      *
      *
               IF      WRK-LINETEK        >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-HKN-RIREKI-TBL (IDXX)   REPLACING   ALL "  "
                                                               BY  "　"
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE                        
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "1"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "2"   
                           MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF                        
           END-PERFORM
      *
           .
       370-HC101-HKN-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<入退院履歴情報・入院基本料種別の続き>処理
      *****************************************************************
       380-HC101-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
      *    最初の行のとき２行あける
           COMPUTE WRK-WLINETEK    =   WRK-LINETEK +   2                                       
           IF  WRK-WLINETEK        >   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     2               TO  WRK-LINETEK     
      *
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
      * 
      *    最初の行のコメントのセット
           MOVE    SPACE               TO  WRK-NYUTAI-RIREKI-AREA
           MOVE    "入退院履歴"        TO  WRK-NYUTAI-MSG
      *          
           INITIALIZE                      RECE68-REC
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
      *
           START   RECE68-FILE
               KEY IS    >=   RECE68-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE68
               NOT INVALID
                   PERFORM 900-RECE68-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE68           =  1                   )
                   OR    ( RECE62-HOSPID    NOT =  RECE68-HOSPID       )
                   OR    ( RECE62-SRYYM     NOT =  RECE68-SRYYM        )
                   OR    ( RECE62-NYUGAIKBN NOT =  RECE68-NYUGAIKBN    )
                   OR    ( RECE62-PTID      NOT =  RECE68-PTID         )
                   OR    ( RECE62-RECEKA    NOT =  RECE68-RECEKA       )
                   OR    ( RECE62-TEISYUTUSAKI
                                             NOT =  RECE68-TEISYUTUSAKI)
                   OR    ( RECE62-RECESYUBETU
                                             NOT =  RECE68-RECESYUBETU )
                   OR    ( RECE62-HKNJANUM-KEY
                                             NOT =  RECE68-HKNJANUM    )
                   OR    ( RECE62-HOJOKBN-KEY
                                             NOT =  RECE68-HOJOKBN     )
                   OR    ( RECE62-TEKSTYMD   NOT =  RECE68-TEKSTYMD    )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE68-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD
                   MOVE    "〜"                TO  WRK-NYUTAI-FL1
               END-IF
      *                          
               IF      RECE68-TAIINYMD (1:6)   
                                           >   RECE62-SRYYM
                   MOVE    SPACE               TO  WRK-NYUTAI-TAIINYMD
                   CONTINUE
               ELSE
                   MOVE    RECE68-TAIINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD
               END-IF
      *
               IF      RECE68-NYUINCHUKBN  =   ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                               BY  "　"  
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       EVALUATE    SYS-2005-NEXTPRTKBN
                           WHEN    ZERO
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "1"
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "2"
                               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                       END-EVALUATE
                   END-IF                    
                   MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
               END-IF
      *
               MOVE    RECE68-NYUINCHUKBN  TO  WRK-NYUINCHUKBN    
      *
               PERFORM 900-RECE68-NEXT-SEC
           END-PERFORM
      *
      *    最後の履歴が入院中のときに編集する
           IF      WRK-NYUTAI-RIREKI-AREA
                                       NOT =   SPACE                   
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1                   TO  WRK-LINETEK
      *
               INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                           BY  "　"  
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC06-TEKIYO (WRK-LINETEK)
               ELSE                    
                   EVALUATE    SYS-2005-NEXTPRTKBN
                       WHEN    ZERO
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                       WHEN    "1"
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                       WHEN    "2"
                           MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC063-TEKIYO(WRK-LINETEK)
                   END-EVALUATE
               END-IF                    
               MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
           END-IF
      *
      *    最後の行の後に１行あける
           IF      WRK-NYUTAI-RIREKI-AREA
                                       =   SPACE                   
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK     
           END-IF
      *
      *       <入院基本料種別>
           IF      RECE62-NYUSYUBETU (12)  NOT =   SPACE
               PERFORM VARYING IDXX    FROM    12   BY  1
                       UNTIL   IDXX    >       20
                       OR      RECE62-NYUSYUBETU (IDXX)    =   SPACE
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1   
                       MOVE    RECE62-NYUSYUBETU (IDXX)
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       EVALUATE    SYS-2005-NEXTPRTKBN
                           WHEN    ZERO
                               MOVE    RECE62-NYUSYUBETU (IDXX)
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "1"
                               MOVE    RECE62-NYUSYUBETU (IDXX)
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "2"
                               MOVE    RECE62-NYUSYUBETU (IDXX)
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                       END-EVALUATE
                   END-IF                    
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK     
           END-IF    
      *
      *       <食事基本料情報（その他）>
           MOVE    ZERO                TO  FLG-SHOKUJI-ETC
           IF      RECE62-SHOKUJIKHNRYO-ETC(1)  NOT =   ZERO
               PERFORM VARYING IDXX    FROM     1   BY  1
                       UNTIL   IDXX    >        3
                       OR      RECE62-SHOKUJIKHNRYO-ETC(IDXX)  =  ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF
                   ADD     1               TO  WRK-LINETEK
      *
                   MOVE    SPACE    TO     WRK-SHOKUJIKHNINF-ETC-AREA
      *
                   MOVE  RECE62-SHOKUJIKHNRYO-ETC(IDXX)
                                           TO  WRK-HENSYU-KIN-9
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-HENSYU-KIN  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:8)
                                           TO  WRK-SHOKUJIKHNRYO-ETC
                   END-IF
      *
                   MOVE  RECE62-SHOKUJIKHNNISSU-ETC(IDXX)
                                           TO  WRK-HENSYU-DAY-9
                   IF      WRK-HENSYU-DAY(1:1) =  "0"
                      MOVE      SPACE      TO  WRK-HENSYU-DAY(1:1)
                   END-IF
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-HENSYU-DAY  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                           TO  WRK-SHOKUJIKHNNISSU-ETC
                   END-IF
      *
                   IF   FLG-SHOKUJI-ETC  =  ZERO
                     MOVE  "食事　" TO   WRK-SHOKUJIKHNRYO-ETC-NM
                     MOVE   1       TO   FLG-SHOKUJI-ETC
                   ELSE
                     MOVE  "　　　" TO   WRK-SHOKUJIKHNRYO-ETC-NM
                   END-IF
                   MOVE    "円×"   TO   WRK-SHOKUJIKHNRYO-ETC-FL
                   MOVE    "日"     TO   WRK-SHOKUJIKHNNISSU-ETC-FL
      *
                   IF      CNT-PAGE        =   1
                       MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC06-TEKIYO (WRK-LINETEK)
                   ELSE
                       EVALUATE    SYS-2005-NEXTPRTKBN
                           WHEN    ZERO
                               MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC061-TEKIYO(WRK-LINETEK)
                           WHEN    "1"
                               MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC062-TEKIYO(WRK-LINETEK)
                           WHEN    "2"
                               MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC063-TEKIYO(WRK-LINETEK)
                       END-EVALUATE
                   END-IF
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
           END-IF
      *
           .
       380-HC101-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<手術歴>処理
      *****************************************************************
       381-HC101-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-OPECOM
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE62-HOSPID       TO  OPECOM-HOSPID
           MOVE    RECE62-PTID         TO  OPECOM-PTID
           MOVE    RECE62-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE62-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "OPECOM-KEY4"       TO  WRK-OPECOM-PATH
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE62-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE62-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE62-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE62-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE62-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE62-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE62-KOHID  (IDY) )
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
                   PERFORM 3811-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3811-OPECOM-HENSYU-SEC
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
           MOVE    RECE62-HOSPID       TO  OPECOM-HOSPID
           MOVE    RECE62-PTID         TO  OPECOM-PTID
           MOVE    RECE62-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE62-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "OPECOM-KEY5"       TO  WRK-OPECOM-PATH
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE62-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE62-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   ELSE
                       IF    ( OPECOM-HKNNUM   =   RECE62-RJNHKNNUM )
                       AND   ( OPECOM-HKNID    =   RECE62-RJNHKNID  )
                           MOVE    1               TO  FLG-OPECOM-CHK
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       4
                                   OR      RECE62-KOHNUM (IDY) =   SPACE
                               IF    ( OPECOM-HKNNUM   
                                               =   RECE62-KOHNUM (IDY) )
                               AND   ( OPECOM-HKNID   
                                               =   RECE62-KOHID  (IDY) )
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
                   PERFORM 3811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3811-OPECOM-HENSYU-SEC
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
       381-HC101-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3811-OPECOM-HENSYU-SEC    SECTION.
      *
           IF      WRK-OPECOM          =   ZERO
      *        傷病名との間に区切り線を出力
               IF      WRK-LINETEK         >   ZERO
                   IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       MOVE    1               TO  FLG-KUGIRI
                       PERFORM 34110-KUGIRI-LINE-SEC
                       MOVE    ZERO            TO  FLG-KUGIRI
                   END-IF
               END-IF
           ELSE
      *        手術歴とコメントの間に改行
               IF    ( WRK-OPECOM-PATH     =   "OPECOM-KEY5" )
               AND   ( WRK-OPECOM          =   1             )
                   IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1                   TO  WRK-LINETEK
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-OPECOM-PATH     =   "OPECOM-KEY4"
               MOVE    1                   TO  WRK-OPECOM
           ELSE
               MOVE    2                   TO  WRK-OPECOM
           END-IF                 
      *
           IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
           IF      CNT-PAGE            =   1   
               MOVE    WRK-OPECOM-HEN  TO  HC06-TEKIYO (WRK-LINETEK)
           ELSE                
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC061-TEKIYO(WRK-LINETEK)
                   WHEN    "1"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC062-TEKIYO(WRK-LINETEK)
                   WHEN    "2"
                       MOVE    WRK-OPECOM-HEN
                                           TO  HC063-TEKIYO(WRK-LINETEK)
               END-EVALUATE
           END-IF                
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       3811-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       390-PRINT-OUT-SEC               SECTION.
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
      *********MOVE    "HCN08.red"         TO
               MOVE    "HCN08V01.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HC06                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               EVALUATE    SYS-2005-NEXTPRTKBN
                   WHEN    ZERO
                       MOVE    "HCN061V03.red" TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC061           TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
                   WHEN    "1"   
                       MOVE    "HCN062V03.red" TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC062           TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
                   WHEN    "2"   
                       MOVE    "HCN063V02.red" TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                       MOVE    HC063           TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
               END-EVALUATE
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
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL   IDXY   >       TBL-RECE-MAX
                   OR      FLG-ERR >       ZERO
      *
               ADD     1                    TO  WRK-RECENUM
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
      *    請求管理ＤＢ更新
           IF      FLG-ERR              =   ZERO
               PERFORM 3904-RECE10-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  SPA-STS
           END-IF
      *
           .
       391-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SYUBETU-SRT1 
      *
           INITIALIZE                  RECE08-REC
           MOVE    "HC06"              TO  RECE08-PRTID
      *->  MOVE    WRK-PARA-SRYYM      TO  RECE08-RECEYM
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    ALL "9"         TO  RECE08-RECEYM
           ELSE
                   MOVE    WRK-PARA-SRYYM  TO  RECE08-RECEYM
           END-IF
           MOVE    WRK-PARA-CREYMD     TO  RECE08-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECE08-CREHMS
           IF      RECE62-TEISYUTUSAKI =   "2"
               IF      RECE62-HKNJANUM-KEY (1:2)   =   "67"
                   MOVE    RECE62-HKNJANUM-KEY (3:2)
                                           TO  RECE08-PREFNUM
               ELSE
                   MOVE    RECE62-HKNJANUM-KEY (1:2)
                                           TO  RECE08-PREFNUM
               END-IF
               IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                   MOVE    "1"             TO  RECE08-PREFKBN
               ELSE
                   MOVE    "2"             TO  RECE08-PREFKBN
                   MOVE    RECE62-SRYYM        TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2)
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       5
                           OR      WRK-1033-STYUKYMD (IDX) =   SPACE
                       IF      WRK-1033-STYUKYMD   (IDX) <=  WRK-SYMD
                       AND     WRK-1033-EDYUKYMD   (IDX) >=  WRK-SYMD      
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       20
                                   OR      WRK-1033-HKNJANUM (IDX IDY)
                                                       =   SPACE     
                               IF      RECE62-HKNJANUM-KEY = 
                                       WRK-1033-HKNJANUM (IDX IDY)
                                   MOVE    "1"     TO  RECE08-PREFKBN
                                   MOVE    20      TO  IDY
                                   MOVE    5       TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF            
                   END-PERFORM
               END-IF        
           ELSE
               MOVE    "0"             TO  RECE08-PREFKBN
               MOVE    "00"            TO  RECE08-PREFNUM
           END-IF
           MOVE    RECE62-HKNJANUM-KEY TO  RECE08-HKNJANUM   
           MOVE    RECE62-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    RECE62-TEISYUTUSAKI TO  RECE08-TEISYUTUSAKI
           IF      RECE62-PTNUM        =   SPACE
               MOVE    ZERO                TO  RECE08-RECESYUBETU
           ELSE      
               MOVE    RECE62-RECESYUBETU  TO  RECE08-RECESYUBETU
           END-IF    
           MOVE    RECE62-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE62-SRYYM        TO  RECE08-SRYYM
           MOVE    TBL-RECE-RENNUM (IDXY)
                                       TO  RECE08-RENNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    WRK-RECENUM         TO  RECE08-RECENUM
           ELSE
               IF      BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                       OR  "05"
                   COMPUTE RECE08-RECENUM  =
                           RECE62-PRTSEQ   *   100 +   
                                               TBL-RECE-RENNUM (IDXY)
               ELSE
                   MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               END-IF
           END-IF
      *
           MOVE    "00"                TO  RECE08-SRYKA
           MOVE    "99999999"          TO  RECE08-HKNJANUM-S
           MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
                                           RECE08-KOUGAKU
      *    主科設定
           IF    ( SYS-2009-SYUKA-KBN  =   "1"                )
           AND   ( RECE62-RECESYUBETU  
                                   NOT =   "8888"             )
      *****                        NOT =   "8888"  AND "0000"
      *****                                        AND "6666" )
               IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO
                                             NOT =   "0" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO
                                             NOT =   "0" ) )
                   MOVE    RECE62-SYUKA        TO  RECE08-SRYKA
               END-IF
           END-IF
      *
      *    編綴順項目の設定（国保特別療養費以外）
           IF    ( RECE62-TEISYUTUSAKI =   "1"    )
           OR    ( RECE62-RECESYUBETU  =   "6666"  OR  "8888"  OR
                                           "0000" )
               CONTINUE
           ELSE
               IF    ( FLG-SYS-2008        =   ZERO   )
               AND   ( FLG-RECE-PUT-UM     =   ZERO   )
                   CONTINUE
               ELSE
                   PERFORM 39011-HENTETU-HENSYU-SEC
               END-IF
           END-IF
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR0720 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE62-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           ELSE    
               IF      RECE08-TEISYUTUSAKI =   "1"
                   EVALUATE    RECE62-HKNNUM                   
                       WHEN    "001"
                           MOVE    "1"             TO  RECE08-HKNKBN
                       WHEN    "002"
                           MOVE    "2"             TO  RECE08-HKNKBN
                       WHEN    "003"
                           MOVE    "3"             TO  RECE08-HKNKBN
                       WHEN    "004"
                           MOVE    "4"             TO  RECE08-HKNKBN
                       WHEN    "031"  THRU  "034"
                           MOVE    "5"             TO  RECE08-HKNKBN
                       WHEN    "006"
                           MOVE    "6"             TO  RECE08-HKNKBN
                       WHEN    "007"               
                           MOVE    "7"             TO  RECE08-HKNKBN
                       WHEN    "063"
                       WHEN    "072"  THRU  "075"
                           MOVE    "8"             TO  RECE08-HKNKBN
                   END-EVALUATE
               END-IF                      
               MOVE    RECE62-KANANAME     TO  RECE08-KANANAME
               MOVE    "1"                 TO  RECE08-NYUGAIKBN
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
      *********        WHEN    "HCN08.red"                  
                       WHEN    "HCN08V01.red"
                           MOVE    RECE08-PRTDATA  TO HC06
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC06-PAGE
                           MOVE    HC06            TO RECE08-PRTDATA
                       WHEN    "HCN061V03.red"                  
                           MOVE    RECE08-PRTDATA  TO HC061
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC061-PAGE
                           MOVE    HC061           TO RECE08-PRTDATA
                       WHEN    "HCN062V03.red"                  
                           MOVE    RECE08-PRTDATA  TO HC062
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC062-PAGE
                           MOVE    HC062           TO RECE08-PRTDATA
                       WHEN    "HCN063V02.red"                  
                           MOVE    RECE08-PRTDATA  TO HC063
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC063-PAGE
                           MOVE    HC063           TO RECE08-PRTDATA
                   END-EVALUATE
               END-IF
      *
               MOVE    "RECEPRT-KEY"       TO  ORC-DBPATH
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    RECE08-REC          TO  MCPDATA-REC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT
               ELSE 
                   DISPLAY "ORCR0720 RECE08=" MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE62-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                  TO  FLG-ERR
               END-IF
           END-IF    
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保編綴設定処理
      *****************************************************************
       39011-HENTETU-HENSYU-SEC     SECTION.
      *
      *    検索用保険者番号
           IF      RECE08-HKNJANUM(1:2)    =   "67"
               MOVE    RECE08-HKNJANUM(3:) TO  RECE08-HKNJANUM-S
           ELSE
               MOVE    RECE08-HKNJANUM     TO  RECE08-HKNJANUM-S
           END-IF
      *
           EVALUATE    SYS-2008-PREFKBN-PRTJYUN
                                   ALSO    RECE08-PREFKBN
               WHEN    "1"         ALSO    "1"
                   MOVE    "10"            TO  RECE08-PREFKBN-SRT
               WHEN    "1"         ALSO    "2"
                   MOVE    "20"            TO  RECE08-PREFKBN-SRT
               WHEN    "2"         ALSO    "1"
                   MOVE    "20"            TO  RECE08-PREFKBN-SRT
               WHEN    "2"         ALSO    "2"
                   MOVE    "10"            TO  RECE08-PREFKBN-SRT
           END-EVALUATE
      *
      *    （県内分のみ）
           IF      RECE08-PREFKBN      =   "1"     
               EVALUATE    SYS-2008-HKNJANUM-PRTJYUN
                   WHEN    "1"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "3" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "2"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "2" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "3"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "3" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF                               
                   WHEN    "4"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "1" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "2" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "5"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "1" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
                   WHEN    "6"
                       IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                           IF      RECE08-HKNJANUM-S (3:1)  =   "3"
                               MOVE    "2" TO RECE08-PREFKBN-SRT(2:)
                           ELSE
                               MOVE    "3" TO RECE08-PREFKBN-SRT(2:)
                           END-IF
                       ELSE
                           MOVE    "1" TO  RECE08-PREFKBN-SRT (2:)
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    所属保険者による保険者印刷区分の設定
           MOVE    "1"             TO  RECE08-HKNJANUM-SRT
           IF    ( SYS-2008-HKNJANUM   =   RECE08-HKNJANUM-S )
           AND   ( RECE08-PREFKBN      =   "1"               )
               EVALUATE    SYS-2008-HKNJANUM-PRTKBN
                   WHEN    "1"
                       MOVE    "0"     TO  RECE08-HKNJANUM-SRT
                   WHEN    "2"
                       MOVE    "9"     TO  RECE08-HKNJANUM-SRT
               END-EVALUATE
           END-IF
      *
      *    レセプト種別印刷区分（詳細設定なしのとき）
           IF      FLG-RECE-PUT-UM     =   ZERO
      *        レセプト種別印刷区分
               EVALUATE    SYS-2008-RECESYUBETU-PRTJYUN
                                   ALSO    RECE62-RECESYUBETU (2:1)
                   WHEN    "1"     ALSO    "1"
                   WHEN    "2"     ALSO    "1"
                   WHEN    "3"     ALSO    "3"
                   WHEN    "4"     ALSO    "3"
                   WHEN    "5"     ALSO    "4"
                   WHEN    "6"     ALSO    "4"
                       MOVE    "1"     TO  RECE08-RECESYUBETU-SRT (1:1)
                   WHEN    "3"     ALSO    "1"
                   WHEN    "5"     ALSO    "1"
                   WHEN    "1"     ALSO    "3"
                   WHEN    "6"     ALSO    "3"
                   WHEN    "2"     ALSO    "4"
                   WHEN    "4"     ALSO    "4"
                       MOVE    "2"     TO  RECE08-RECESYUBETU-SRT (1:1)
                   WHEN    "4"     ALSO    "1"
                   WHEN    "6"     ALSO    "1"
                   WHEN    "2"     ALSO    "3"
                   WHEN    "5"     ALSO    "3"
                   WHEN    "1"     ALSO    "4"
                   WHEN    "3"     ALSO    "4"
                       MOVE    "3"     TO  RECE08-RECESYUBETU-SRT (1:1)
               END-EVALUATE    
               MOVE    RECE62-RECESYUBETU (3:2)
                                       TO  RECE08-RECESYUBETU-SRT (2:2)
               IF      RECE62-RECESYUBETU (4:1)  =   "0"
                   MOVE    "A"         TO  RECE08-RECESYUBETU-SRT (3:1)
               END-IF
      *
               IF      SYS-2008-CHOUKI
                                    NOT =   SPACE
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       4
                           OR      RECE62-ETC-KOHNUM (IDX) =   SPACE
                       IF      RECE62-ETC-KOHNUM (IDX) =  "972" OR "974"
                           MOVE    "1"             TO  RECE08-CHOUKI
                           MOVE    4               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        編綴詳細設定より設定
               IF      RECE08-PREFKBN      =   "1"
                   MOVE    1                   TO  IDXX
               ELSE
                   MOVE    2                   TO  IDXX
               END-IF
      *
      *        ＨＥＡＤ情報の設定
               IF    ( RECE-PUTH-KOUGAKU-TEN (IDXX)
                                        >   ZERO                     )
               AND   ( RECE62-TOTALTEN (1)
                                        >=
                                        RECE-PUTH-KOUGAKU-TEN (IDXX) )
                   MOVE    "1"              TO  RECE08-KOUGAKU
               END-IF
      *
               IF    ( RECE62-ETC-KOHNUM (1)
                                   NOT =   SPACE )
               AND   ( RECE-PUTH-CHOUKI  (IDXX) 
                                   NOT =   SPACE )
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       4
                           OR      RECE62-ETC-KOHNUM (IDX) =   SPACE
                       IF      RECE62-ETC-KOHNUM (IDX) =  "972" OR "974"
                           MOVE    "1"             TO  RECE08-CHOUKI
                           MOVE    4               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
      *
               EVALUATE    RECE-PUTH-KKHKBN (IDXX)
                   WHEN    "01"
                       IF      RECE08-HKNJANUM-S (3:1)
                                               NOT =   "3"
                           MOVE    RECE62-HONKZKKBN
                                               TO  RECE08-HONKZKKBN
                       END-IF 
                   WHEN    "02"
                       IF      RECE08-HKNJANUM-S (3:1)
                                                   =   "3"
                           MOVE    RECE62-HONKZKKBN
                                               TO  RECE08-HONKZKKBN
                       END-IF 
               END-EVALUATE
      *
      *        ＢＯＤＹ情報の設定
               MOVE    ZERO                TO  FLG-HENSYU
      *        公費からの設定
               IF      TBL-KOH-RECE-PUTB-HKNNUM (IDXX 1)
                                       NOT =   SPACE    
      *            長期
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       4
                           OR      RECE62-ETC-KOHNUM (IDX)
                                               =   SPACE
                           OR      FLG-HENSYU  =   1
                       IF      RECE62-ETC-KOHNUM (IDX) 
                                               =  "972" OR "974"
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >   WRK-CONS-KOHLIST-MAX
                                   OR      TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                       =   SPACE
                                   OR      FLG-HENSYU  =   1
                               MOVE    TBL-KOH-RECE-PUTB-REC (IDXX IDY)
                                                   TO  RECE-PUTB-REC
      *
                               IF    ( RECE-PUTB-KOUHI-NUM =   "XXX" )
                               AND   ( RECE62-RECESYUBETU  =
                                               RECE-PUTB-SYUBETU-NYU )
                                   PERFORM 390111-HENTETU-HENSYU-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
      *
      *            その他の公費
                   IF    ( FLG-HENSYU          =   ZERO  )
                   AND   ( RECE62-KOHNUM (1)
                                           NOT =   SPACE )
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       4
                               OR      RECE62-KOHNUM (IDX)
                                                   =   SPACE
                               OR      FLG-HENSYU  =   1
                           IF      RECE62-KOHNUM (IDX) 
                                               >=  "100"   AND <=  "899"
                               PERFORM VARYING IDY FROM    1   BY  1
                                       UNTIL   IDY >
                                               WRK-CONS-KOHLIST-MAX
                                       OR      TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                           =   SPACE
                                       OR      FLG-HENSYU  =   1
                                   MOVE    TBL-KOH-RECE-PUTB-REC
                                                           (IDXX IDY)
                                                   TO  RECE-PUTB-REC
                                   IF    ( RECE62-KOHNUM (IDX) =
                                               RECE-PUTB-KOUHI-NUM   )
                                   AND   ( RECE62-RECESYUBETU  =
                                               RECE-PUTB-SYUBETU-NYU )
                                       PERFORM 390111-HENTETU-HENSYU-SEC
                                   END-IF
                               END-PERFORM
      *
      *                        三歳未満のみ
                               IF      RECE62-RECESYUBETU (4:1)  =   "3"
                                   PERFORM VARYING IDY FROM    1   BY  1
                                           UNTIL   IDY >
                                               WRK-CONS-KOHLIST-MAX
                                           OR  TBL-KOH-RECE-PUTB-HKNNUM
                                                           (IDXX IDY)
                                                               =   SPACE
                                           OR      FLG-HENSYU  =   1
                                       MOVE    TBL-KOH-RECE-PUTB-REC
                                                           (IDXX IDY)
                                                   TO  RECE-PUTB-REC
                                       IF    ( RECE62-KOHNUM (IDX) =
                                               RECE-PUTB-KOUHI-NUM   )
                                       AND   ( RECE62-RECESYUBETU (2:1)
                                                                  =
                                           RECE-PUTB-SYUBETU-NYU (2:1) )
                                           PERFORM
                                               390111-HENTETU-HENSYU-SEC
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-IF    
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        公費からの設定がなかったとき
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-CONS-BODYLIST-MAX
                       OR      TBL-SYUBETU-RECE-PUTB-SYUBETU
                                                       (IDXX IDY)
                                           =   SPACE
                       OR      FLG-HENSYU  =   1
                   MOVE    TBL-SYUBETU-RECE-PUTB-REC (IDXX IDY)
                                           TO  RECE-PUTB-REC
                   IF      RECE62-RECESYUBETU  =
                                               RECE-PUTB-SYUBETU-NYU
                       PERFORM 390111-HENTETU-HENSYU-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       39011-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    詳細設定情報出力処理
      *****************************************************************
       390111-HENTETU-HENSYU-SEC   SECTION.
      *
           MOVE    RECE-PUTB-SYUBETU-SRT1
                                       TO  RECE08-SYUBETU-SRT1
           MOVE    RECE-PUTB-SYUBETU-SRT2
                                       TO  RECE08-SYUBETU-SRT2
           IF       RECE-PUTB-KOUHI-NO >   ZERO
               MOVE    RECE-PUTB-KOUHI-NO  TO  RECE08-KOUHI-NO
           ELSE
               MOVE    500                 TO  RECE08-KOUHI-NO
           END-IF
           MOVE    RECE-PUTB-GRP-NO1   TO  RECE08-GRP-NO1
           MOVE    RECE-PUTB-GRP-NO2   TO  RECE08-GRP-NO2
           EVALUATE    RECE-PUTB-SYUBETU-UNDER-NO1 
               WHEN    "01"
                   COMPUTE WRK-KYURATE-9   =   100 -   RECE62-KYURATE
                   MOVE    WRK-KYURATE-X   TO  RECE08-SYUBETU-UNDER-1
               WHEN    "02"
                   MOVE    RECE62-KYURATE  TO  RECE08-SYUBETU-UNDER-1
               WHEN    "03"
                   MOVE    RECE62-KOHHBTNUM(1)
                                           TO  RECE08-SYUBETU-UNDER-1
               WHEN    "04"
                   IF      RECE62-KOHHBTNUM(1) NUMERIC
                       MOVE    RECE62-KOHHBTNUM(1) TO
                                                   WRK-RECE62-HBTNUM-X
                   ELSE
                       MOVE    "00"                TO
                                                   WRK-RECE62-HBTNUM-X
                   END-IF
                   COMPUTE WRK-HBTNUM-9    =   100 -
                                               WRK-RECE62-HBTNUM-9
                   MOVE    WRK-HBTNUM-X    TO  RECE08-SYUBETU-UNDER-1
               WHEN    "05"
                   MOVE    RECE62-KIGO     TO   RECE08-SYUBETU-UNDER-1-1
                   MOVE    RECE62-NUM      TO   RECE08-SYUBETU-UNDER-1-2
               WHEN    "06"
                   MOVE    RECE62-KOHJKYSNUM (1)
                                           TO  RECE08-SYUBETU-UNDER-1
               WHEN    "07"
                   MOVE    RECE08-HKNJANUM-S
                                           TO  RECE08-SYUBETU-UNDER-1
          END-EVALUATE
           EVALUATE    RECE-PUTB-SYUBETU-UNDER-NO2
               WHEN    "01"
                   COMPUTE WRK-KYURATE-9   =   100 -   RECE62-KYURATE
                   MOVE    WRK-KYURATE-X   TO  RECE08-SYUBETU-UNDER-2
               WHEN    "02"
                   MOVE    RECE62-KYURATE  TO  RECE08-SYUBETU-UNDER-2
               WHEN    "03"
                   MOVE    RECE62-KOHHBTNUM(1)
                                           TO  RECE08-SYUBETU-UNDER-2
               WHEN    "04"
                   IF      RECE62-KOHHBTNUM(1) NUMERIC
                       MOVE    RECE62-KOHHBTNUM(1) TO
                                                   WRK-RECE62-HBTNUM-X
                   ELSE
                       MOVE    "00"                TO
                                                   WRK-RECE62-HBTNUM-X
                   END-IF
                   COMPUTE WRK-HBTNUM-9    =   100 -
                                               WRK-RECE62-HBTNUM-9
                   MOVE    WRK-HBTNUM-X    TO  RECE08-SYUBETU-UNDER-2
          END-EVALUATE
      *
          IF      RECE-PUTH-SRT-01 (IDXX) =   "02"
              MOVE    RECE-PUTB-SYUBETU-SRT1
                                          TO  WRK-RECE-PUTB-SYUBETU-SRT1
              MOVE    WRK-RECE-PUTB-SYUBETU-SRT1-X
                                          TO  RECE08-HKNJANUM-S
              MOVE    RECE-PUTB-SYUBETU-NYU-SRT1
                                          TO  WRK-SYUBETU-SRT1
          ELSE
              MOVE    SPACE               TO  WRK-SYUBETU-SRT1  
          END-IF
      *
          MOVE    1                    TO  FLG-HENSYU
      *
           .
       390111-HENTETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
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
           MOVE    "RECEKANRI-KEY4"    TO  WRK-PATH
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   1
               INITIALIZE                      RECE07-REC
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
               MOVE    RECE08-KOUGAKU      TO  RECE07-KOUGAKU
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
                       WHEN    "04"
                           MOVE    "3"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE07-STYMD
                       WHEN    "05"
                           MOVE    "4"             TO  RECE07-SYORISBT
                   END-EVALUATE
               END-IF
               MOVE    1                   TO  RECE07-PAGE
               MOVE    1                   TO  RECE07-KENSU
               IF      RECE08-CHOUKI       =   "1"
                   ADD     1               TO  RECE07-PAGE-CHOUKI
               END-IF
               IF      RECE08-KOUGAKU      =   "1"  
                   ADD     1               TO  RECE07-PAGE-KOUGAKU
               END-IF
               IF    ( RECE08-CHOUKI       =   "1" )  
               AND   ( RECE08-KOUGAKU      =   "1" )
                   ADD     1               TO  RECE07-PAGE-DUP
               END-IF
               MOVE    SYS-2008-KBNCD      TO  WRK-HSEQNUM
               MOVE    WRK-HSEQNUM         TO  RECE07-HSEQNUM
      *        編綴順項目の設定（国保特別療養費以外）
               IF    ( RECE62-TEISYUTUSAKI =   "1"    )
               OR    ( RECE62-RECESYUBETU  =   "6666"  OR  "8888"  OR
                                               "0000" )
                   CONTINUE
               ELSE
                   IF      FLG-RECE-PUT-UM     =   1
                       MOVE    99999999            TO  RECE07-HSEQNUM
      *                編綴詳細設定より設定
                       IF      RECE08-PREFKBN      =   "1"
                           MOVE    1                   TO  IDXX
                       ELSE
                           MOVE    2                   TO  IDXX
                       END-IF
                       MOVE    RECE08-PREFKBN  TO  RECE07-PREFKBN
                       MOVE    RECE-PUTH-ZAITAKU     (IDXX) 
                                               TO  RECE07-ZAITAKU-KBN
                       MOVE    RECE-PUTH-CHOUKI      (IDXX)
                                               TO  RECE07-CHOUKI-KBN
                       MOVE    RECE-PUTH-SRT-LAST    (IDXX)
                                               TO  RECE07-SRT-LAST
                       MOVE    RECE-PUTH-KOUGAKU-TEN    (IDXX)
                                               TO  RECE07-KOUGAKU-TEN
                   END-IF
               END-IF    
           ELSE
               ADD     1                   TO  RECE07-PAGE
               IF    ( CNT-KENSU           =   1 )
               AND   ( TBL-RECE-RENNUM (IDXY)
                                           =   1 )
                   ADD     1                   TO  RECE07-KENSU
                   IF      RECE08-CHOUKI       =   "1"  
                       ADD     1               TO  RECE07-PAGE-CHOUKI
                   END-IF
                   IF      RECE08-KOUGAKU      =   "1"  
                       ADD     1               TO  RECE07-PAGE-KOUGAKU
                   END-IF
                   IF    ( RECE08-CHOUKI       =   "1" )  
                   AND   ( RECE08-KOUGAKU      =   "1" )
                       ADD     1               TO  RECE07-PAGE-DUP
                   END-IF
               END-IF
           END-IF
           IF    WRK-SYUBETU-SRT1      >   ZERO     
              MOVE    WRK-SYUBETU-SRT1     TO  RECE07-SYUBETU-SRT1
           END-IF   
      *
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "RECEKANRI-KEY"     TO  ORC-DBPATH
           IF      FLG-RECE07          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC 
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC      
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0720 RECEKANRI MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
               MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
       3903-RECE07-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新
      *****************************************************************
       3904-RECE10-HENSYU-SEC        SECTION.
      * 
      *    テスト患者のとき更新しない
           IF      RECE62-TSTPTNUMKBN  =   "1"
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      * 
      *    レセプト種別不明または自費のとき更新しない
           IF      RECE62-RECESYUBETU      =   "0000"  OR  "8888"
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *
      *     主科設定のとき未設定なら更新しない
           IF    ( SYS-2009-SYUKA-KBN  =   "1"  )
           AND   ( RECE62-SYUKA        =   "XX" )
               IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
                   GO  TO  3904-RECE10-HENSYU-EXT
               END-IF
           END-IF
      *          
           INITIALIZE                  RECE10-REC
           MOVE    RECE62-HOSPID       TO  RECE10-HOSPID
           MOVE    RECE62-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE62-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE62-PTID         TO  RECE10-PTID
           IF    ( SYS-2009-SYUKA-KBN  =   "1"    )
      *****AND   ( RECE62-RECESYUBETU
      *****                        NOT =   "6666" )
               IF  (     ( RECE62-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE62-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
                   MOVE    RECE62-SYUKA        TO  RECE10-RECEKA
               ELSE
                   IF      RECE62-RECEKA       =   SPACE
                       MOVE    ZERO            TO  RECE10-RECEKA
                   ELSE
                       MOVE    RECE62-RECEKA   TO  RECE10-RECEKA
                   END-IF
               END-IF
           ELSE
               IF      RECE62-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECE10-RECEKA
               ELSE
                   MOVE    RECE62-RECEKA   TO  RECE10-RECEKA
               END-IF
           END-IF
           MOVE    RECE62-TEISYUTUSAKI TO  RECE10-TEISYUTUSAKI
           MOVE    RECE62-RECESYUBETU  TO  RECE10-RECESYUBETU
           IF      RECE62-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECE10-HKNJANUM
           ELSE
               MOVE    RECE62-HKNJANUM-KEY TO  RECE10-HKNJANUM
           END-IF
           IF      RECE62-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECE10-HOJOKBN-KEY
           ELSE
               MOVE    RECE62-HOJOKBN-KEY  TO  RECE10-HOJOKBN-KEY
           END-IF   
           IF      RECE62-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECE10-TEKSTYMD
           ELSE
               MOVE    RECE62-TEKSTYMD TO  RECE10-TEKSTYMD
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    WRK-PARA-SYSYMD     TO  RECE10-UPDYMD
               IF      RECE10-SKYKBN       =   "2"
                   MOVE    "1"                 TO  RECE10-SKYKBN
               END-IF 
               MOVE    "2"                 TO  RECE10-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  RECE10-SKYKBN
               MOVE    RECE62-PTNUM        TO  RECE10-PTNUM
               MOVE    RECE62-HKNNUM       TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE62-HKNID        TO  RECE10-HKNID
               MOVE    RECE62-RJNHKNNUM    TO  RECE10-RJNHKNNUM
               MOVE    RECE62-RJNHKNID     TO  RECE10-RJNHKNID
               MOVE    RECE62-HONKZKKBN    TO  RECE10-HONKZKKBN
               MOVE    RECE62-HOJOKBN      TO  RECE10-HOJOKBN
               MOVE    RECE62-CONTKBN      TO  RECE10-CONTKBN
               MOVE    RECE62-KIGO         TO  RECE10-KIGO
               MOVE    RECE62-NUM          TO  RECE10-NUM
               MOVE    ZERO                TO  RECE10-SKYYM
                                               RECE10-HENREIYM
                                               RECE10-TEISEIKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                   MOVE    RECE62-KOHNUM (IDX) TO
                                       RECE10-KOHNUM (IDX)
                   MOVE    RECE62-KOHID  (IDX) TO
                                       RECE10-KOHID  (IDX)
               END-PERFORM
               MOVE    RECE62-HKNNUM   TO  RECE10-HKNNUM-S
               IF      RECE62-HKNJANUM-KEY =   SPACE
                   MOVE    ZERO            TO  RECE10-HKNJANUM-S
               ELSE
                   IF      RECE62-HKNJANUM-KEY(1:2)    =   "67"
                       MOVE    RECE62-HKNJANUM-KEY(3:)
                                           TO  RECE10-HKNJANUM-S
                       IF      RECE62-RECESYUBETU  =    "6666"
                           MOVE    "069"       TO  RECE10-HKNNUM-S
                       ELSE
                           MOVE    "067"       TO  RECE10-HKNNUM-S
                       END-IF
                   ELSE
                       MOVE    RECE62-HKNJANUM-KEY
                                           TO  RECE10-HKNJANUM-S
                   END-IF
               END-IF
               MOVE    RECE08-PREFKBN      TO  RECE10-PREFKBN
               MOVE    RECE08-PREFNUM      TO  RECE10-PREFNUM
               MOVE    WRK-PARA-SYSYMD     TO  RECE10-CREYMD
               MOVE    ZERO                TO  RECE10-UPDYMD
           END-IF
      *
      *    治験は請求区分を０：請求しないとする
           IF      RECE10-HKNNUM       >=  "900"
           AND     RECE10-HKNNUM       <=  "969"
                   MOVE    ZERO            TO  RECE10-SKYKBN
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE62-JNISSUINF (IDX)  TO
                                       RECE10-JNISSUINF (IDX)
               MOVE    RECE62-TOTALTENINF (IDX)    TO 
                                       RECE10-TOTALTENINF (IDX)
               MOVE    RECE62-FTNMONEYINF(IDX) TO
                                       RECE10-FTNMONEYINF (IDX)
               MOVE    RECE62-SHOKUJIINF (IDX) TO
                                       RECE10-SHOKUJIINF (IDX)
           END-PERFORM
      *    １１初診〜８０他
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       22
               MOVE    RECE62-SRYKBNINF (IDX)  TO
                                       RECE10-SRYKBNINF (IDX)
           END-PERFORM
      *
      *    ９０入院基本料・加算
           MOVE    ZERO              TO  IDY
           PERFORM   VARYING   IDX   FROM    23   BY  1
                     UNTIL     IDX       >   26
               ADD     1                 TO  IDY
               MOVE    RECE62-NYUKHNRYO  (IDY)
                                         TO  RECE10-KHNTEN (IDX)
               MOVE    RECE62-NYUKHNNISSU(IDY)
                                         TO  RECE10-KAISU  (IDX)
               PERFORM   VARYING   IDY1  FROM    1   BY  1
                         UNTIL     IDY1      >   5
                   MOVE    RECE62-NYUKHNKEI  (IDY IDY1)
                                         TO  RECE10-TENSU  (IDX IDY1)
               END-PERFORM
           END-PERFORM
           IF        RECE62-NYUKHNRYO (5)    >   ZERO
               MOVE    ZERO                  TO  RECE10-KHNTEN (26)
               PERFORM   VARYING   IDY   FROM    5   BY  1
                         UNTIL     IDY       >   10
                         OR        RECE62-NYUKHNRYO (IDY)    =   ZERO
                   ADD     RECE62-NYUKHNNISSU(IDY)
                                                 TO  RECE10-KAISU  (26)
                   PERFORM   VARYING   IDY1  FROM    1   BY  1
                             UNTIL     IDY1      >   5
                       ADD       RECE62-NYUKHNKEI  (IDY IDY1)
                                         TO  RECE10-TENSU  (26 IDY1)
                   END-PERFORM
               END-PERFORM    
           END-IF
           PERFORM   VARYING   IDY1  FROM    1   BY  1
                     UNTIL     IDY1      >   5
               MOVE    RECE62-NYUKHNKEI2 (IDY1)
                                         TO  RECE10-TENSU  (27 IDY1)
           END-PERFORM
      *     
      *    ９２特定入院料・加算
           MOVE    ZERO              TO  IDY
           PERFORM   VARYING   IDX   FROM    28   BY  1
                     UNTIL     IDX       >   31
               ADD     1                 TO  IDY
               IF      RECE62-NYUTOKRYO-RYAKU (IDY)
                                             =   SPACE
                   MOVE    RECE62-NYUTOKRYO  (IDY)
                                         TO  RECE10-KHNTEN (IDX)
               END-IF
               MOVE    RECE62-NYUTOKNISSU(IDY)
                                         TO  RECE10-KAISU  (IDX)
               PERFORM   VARYING   IDY1  FROM    1   BY  1
                         UNTIL     IDY1      >   5
                   MOVE    RECE62-NYUTOKKEI  (IDY IDY1)
                                         TO  RECE10-TENSU  (IDX IDY1)
               END-PERFORM
           END-PERFORM
           IF        RECE62-NYUTOKNISSU (5)  >   ZERO
               MOVE    ZERO                  TO  RECE10-KHNTEN (31)
               PERFORM   VARYING   IDY   FROM    5   BY  1
                         UNTIL     IDY       >   10
                         OR        RECE62-NYUTOKRYO (IDY)    =   ZERO
                   ADD     RECE62-NYUTOKNISSU(IDY)
                                                 TO  RECE10-KAISU  (31)
                   PERFORM   VARYING   IDY1  FROM    1   BY  1
                             UNTIL     IDY1      >   5
                       ADD       RECE62-NYUTOKKEI  (IDY IDY1)
                                         TO  RECE10-TENSU  (31 IDY1)
                   END-PERFORM
               END-PERFORM    
           END-IF
           PERFORM   VARYING   IDY1  FROM    1   BY  1
                     UNTIL     IDY1      >   5
               MOVE    RECE62-NYUTOKKEI2 (IDY1)
                                         TO  RECE10-TENSU  (32 IDY1)
           END-PERFORM
      *    給付割合
           IF      RECE62-KYURATE  >   ZERO
               COMPUTE WRK-KYURATE-9   =
                              (100     -   RECE62-KYURATE)
                                               / 10 
               MOVE    WRK-KYURATE-9   TO  RECE10-KYURATE
           ELSE
               IF      RECE62-TEISYUTUSAKI =   "1"
                   MOVE    ZERO            TO  RECE10-KYURATE
               ELSE
                   MOVE    10              TO  RECE10-KYURATE
               END-IF        
           END-IF             
      *     
           MOVE    RECE62-AGE          TO  RECE10-AGE  
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      RECE62-ETC-KOHNUM (IDX) =   SPACE
               MOVE    RECE62-ETC-KOHNUM     (IDX) TO
                                       RECE10-ETC-KOHNUM (IDX)
               MOVE    RECE62-ETC-JNISSU   (IDX)   TO
                                       RECE10-ETC-JNISSU   (IDX)
               MOVE    RECE62-ETC-TOTALTEN (IDX)   TO
                                       RECE10-ETC-TOTALTEN (IDX)
               MOVE    RECE62-ETC-FTNMONEY (IDX)   TO
                                       RECE10-ETC-FTNMONEY (IDX)
               MOVE    RECE62-ETC-FTNMONEY-FTN (IDX)   TO
                                       RECE10-ETC-FTNMONEY-FTN (IDX)
               MOVE    RECE62-ETC-FTNMONEY-KOJ (IDX)   TO
                                       RECE10-ETC-FTNMONEY-KOJ (IDX)
               MOVE    RECE62-ETC-SHOKUJINISSU  (IDX) 
                                                   TO
                                       RECE10-ETC-SHOKUJINISSU (IDX)
               MOVE    RECE62-ETC-SHOKUJIRYOYOHI (IDX) 
                                                   TO
                                       RECE10-ETC-SHOKUJIRYOYOHI (IDX)
               MOVE    RECE62-ETC-SHOKUJIFTN (IDX) TO
                                       RECE10-ETC-SHOKUJIFTN(IDX)
           END-PERFORM
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE62-SHOKUJIFTNKBN
                                       TO  RECE10-SHOKUJIFTNKBN
      *
      *    新潟県中越沖地震対応 
           IF    ( RECE62-PREFNUM      =   "15"   )
           AND   ( RECE62-SRYYM        >=  200709 )
           AND   ( RECE62-SRYYM        <=  200712 )
               IF      RECE62-RJNGAIKOHFTN (1)
                                           >   ZERO
                   MOVE    RECE62-RJNGAIKOHFTN (1)
                                           TO  RECE10-ETC-YKZFTN (1)
               END-IF
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "SEIKYU-KEY"        TO  ORC-DBPATH
           IF      FLG-SEIKYU          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           CALL    "ORCMCPSUB"              USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC      
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0720 SEIKYU MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
               MOVE    "請求管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "請求管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
       3904-RECE10-HENSYU-EXT.
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
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
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
      *            ジョブ管理終了処理
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
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
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
      *                ジョブ管理終了処理
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
                   MOVE    2                   TO  SPA-STS
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
      *    個別作成時はORCR710で削除済なので処理しない
           IF      WRK-PARA-SYOKBN     =   "1"
      *        診療年月の請求管理削除処理（種別不明分）
               INITIALIZE                  RECE10-REC
               MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "SEIKYU-DEL5"       TO  ORC-DBPATH
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCMCPSUB"              USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC      
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                   MOVE
                   "請求管理削除処理（種別不明分）でエラーとなりました"
                                               TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
      *        診療年月の請求管理削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECE10-REC
                   MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "1"                 TO  RECE10-NYUGAIKBN
                   MOVE    1                   TO  RECE10-TEISYUTUSAKI
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "SEIKYU-DEL6"       TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の請求管理削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                  RECE10-REC
                   MOVE    SYS-1001-HOSPID     TO  RECE10-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "1"                 TO  RECE10-NYUGAIKBN
                   MOVE    2                   TO  RECE10-TEISYUTUSAKI
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "SEIKYU-DEL6"       TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の公費請求書削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  KOHSKY-REC
                   MOVE    SYS-1001-HOSPID     TO  KOHSKY-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    1                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "1"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "KOHSKY-DEL1"       TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月公費請求書削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                  KOHSKY-REC
                   MOVE    SYS-1001-HOSPID     TO  KOHSKY-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    2                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "1"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "KOHSKY-DEL1"        TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                     "公費請求書削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECEDEN-REC
                   MOVE    SYS-1001-HOSPID     TO  RECEDEN-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    1                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "1"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "RECEDEN-DEL1"      TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                  RECEDEN-REC
                   MOVE    SYS-1001-HOSPID     TO  RECEDEN-HOSPID
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM 
                   MOVE    2                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "1"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "RECEDEN-DEL1"      TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC      
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
               MOVE    "HC06"              TO  RECE07-PRTID
               MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    "1"                 TO  RECE07-NYUGAIKBN
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "RECEKANRI-KEY7"    TO  WRK-PATH
               PERFORM 900-RECE07-READ-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-REC          TO  WRK-RECE07-REC
                   MOVE    RECE07-REC          TO  MCPDATA-REC 
                   MOVE    "RECEKANRI-DEL1"    TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC    
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE
                           "レセプト管理削除処理でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月レセプト削除処理（一括分）
                   INITIALIZE                  RECE08-REC
                   MOVE    WRK-RECE07-PRTID    TO  RECE08-PRTID
                   MOVE    WRK-RECE07-SRYYM    TO  RECE08-RECEYM
                   MOVE    WRK-RECE07-CREYMD   TO  RECE08-CREYMD
                   MOVE    WRK-RECE07-CREHMS   TO  RECE08-CREHMS
                   MOVE    WRK-RECE07-NYUGAIKBN
                                               TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   MOVE    "RECEPRT-DEL1"      TO  ORC-DBPATH
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   CALL    "ORCMCPSUB"              USING
                                                    MCPAREA
                                                    ORCMCPAREA
                                                    MCPDATA-REC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" ORC-DBPATH " MCP-RC=" MCP-RC
                       MOVE    "レセプト削除処理でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
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
           CLOSE                       RECE62-FILE
           CLOSE                       RECE62X-FILE
           CLOSE                       RECE631-FILE
           CLOSE                       RECE661-FILE
           CLOSE                       RECE67-FILE
           CLOSE                       RECE68-FILE
           CLOSE                       RECE69-FILE 
      *
      *    未請求解除データの更新
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( FLG-END             =   ZERO      )
               PERFORM 3001-MISEIKYU-FLG-KOUSIN-SEC
           END-IF 
      *
      *****IF      CNT-OUT            =   ZERO 
      *****    MOVE    "対象データがありませんでした" TO  WRK-RECEERR
      *****    PERFORM 500-ERR-HENSYU-SEC
      *****END-IF
           DISPLAY "*** ORCR0720 IN  RECE62 ="  CNT-RECE62
           DISPLAY "*** ORCR0720 IN  RECE62X="  CNT-RECE62X
           DISPLAY "*** ORCR0720 OUT  "  CNT-OUT
           DISPLAY "*** ORCR0720 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
              MOVE    CNT-OUT          TO  JOB-UPDCNT
              CALL    "ORCSJOB"        USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除データの更新処理
      *****************************************************************
       3001-MISEIKYU-FLG-KOUSIN-SEC    SECTION.
      *
           OPEN    INPUT               RECE62-FILE
      *
           MOVE    ZERO                TO  FLG-RECE62
           PERFORM 990-RECE62-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE62  =   1
               IF      RECE62-MISEIKYU-FLG =   "02"
                   PERFORM 30011-MISEIKYU-FLG-03-SEC
               END-IF
      *
               MOVE    RECE62-HOSPID   TO  KEY-N-HOSPID
               MOVE    RECE62-SRYYM    TO  KEY-N-SRYYM
               MOVE    RECE62-NYUGAIKBN
                                       TO  KEY-N-NYUGAIKBN
               MOVE    RECE62-PTID     TO  KEY-N-PTID
               PERFORM UNTIL   FLG-RECE62         =   1
                       OR      RECE62-HOSPID  NOT =   KEY-N-HOSPID
                       OR      RECE62-SRYYM   NOT =   KEY-N-SRYYM
                       OR      RECE62-NYUGAIKBN
                                              NOT =   KEY-N-NYUGAIKBN
                       OR      RECE62-PTID    NOT =   KEY-N-PTID
                   PERFORM 990-RECE62-READ-SEC
               END-PERFORM 
           END-PERFORM
      *
           CLOSE               RECE62-FILE
           .
       3001-MISEIKYU-FLG-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定解除処理
      *****************************************************************
       30011-MISEIKYU-FLG-03-SEC       SECTION.
      *
           DISPLAY "MISEIKYU PTNUM=" RECE62-PTNUM
                   " SRYYM=" RECE62-SRYYM     
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYUKA-REC
           MOVE    RECE62-HOSPID   TO  SYUKA-HOSPID
           MOVE    RECE62-PTID     TO  SYUKA-PTID
           MOVE    RECE62-SRYYM    TO  SYUKA-SRYYM
           MOVE    RECE62-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "03"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SMCNDATE-YMD    TO  SYUKA-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYUKA-UPHMS
           MOVE    "recept9"       TO  SYUKA-TERMID
           MOVE    SPACE           TO  SYUKA-OPID
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "upd4"          TO  MCP-PATHNAME
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC      NOT =   ZERO 
               MOVE    "主科ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
      *
       30011-MISEIKYU-FLG-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       900-RECE62-READ-SEC           SECTION.
      *
           READ    RECE62-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE62
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                 MOVE    ZERO                TO  FLG-RECE62
                 IF      RECE62-SRYYM        <=  WRK-CONS-SRYYM-200609
                     GO  TO  900-RECE62-READ-SEC
                 ELSE
                   IF    ( RECE62-TOTALTEN (1) =   ZERO            )
                   AND   ( RECE62-TOTALTEN (2) =   ZERO            )
                   AND   ( RECE62-TOTALTEN (3) =   ZERO            )
                   AND   ( RECE62-TOTALTEN (4) =   ZERO            )
                   AND   ( RECE62-TOTALTEN (5) =   ZERO            )
                   AND   ( RECE62-SHOKUJIRYOYOHI (1)
                                               =   ZERO            )
                   AND   ( RECE62-SHOKUJIRYOYOHI (2)
                                               =   ZERO            )
                   AND   ( RECE62-SHOKUJIRYOYOHI (3)
                                               =   ZERO            )
                   AND   ( RECE62-SHOKUJIRYOYOHI (4)
                                               =   ZERO            )
                   AND   ( RECE62-SHOKUJIRYOYOHI (5)
                                               =   ZERO             )
                   AND   ( RECE62-RECESYUBETU
                                           NOT =   8888    AND ZERO )
                       GO  TO  900-RECE62-READ-SEC
                   ELSE
                     IF      RECE62-TOTALTEN (1)   =   ZERO
                     AND     RECE62-TOTALTEN (2)   =   ZERO 
                     AND     RECE62-TOTALTEN (3)   =   ZERO 
                     AND     RECE62-TOTALTEN (4)   =   ZERO
                     AND     RECE62-TOTALTEN (5)   =   ZERO
                     AND     RECE62-SHOKUJIRYOYOHI (1)
                                                   =   ZERO
                     AND     RECE62-SHOKUJIRYOYOHI (2)
                                                   =   ZERO
                     AND     RECE62-SHOKUJIRYOYOHI (3)
                                                   =   ZERO
                     AND     RECE62-SHOKUJIRYOYOHI (4)
                                                   =   ZERO
                     AND     RECE62-SHOKUJIRYOYOHI (5)
                                                   =   ZERO
                     AND     RECE62-RECESYUBETU    =   8888
                     AND     RECE62-JIHTEKFLG      =   SPACE
                         GO  TO  900-RECE62-READ-SEC
                     ELSE
                         IF    ( RECE62-PREFNUM      =   "40"    )
                         AND   ( RECE62-TEISYUTUSAKI =   "2"     )
                         AND   ( RECE62-RECESYUBETU  
                                                 NOT =   "6666"  )
                         AND   ( RECE62-SRYYM        <=  "200609")
                           GO  TO  900-RECE62-READ-SEC
                         ELSE
      *
                           DISPLAY "RECE62  PTNUM=" RECE62-PTNUM
      *
                           ADD     1               TO  CNT-RECE62
                           MOVE    RECE62-HOSPID   TO  KEY-N-HOSPID
                           MOVE    RECE62-SRYYM    TO  KEY-N-SRYYM
                           MOVE    RECE62-NYUGAIKBN
                                                   TO  KEY-N-NYUGAIKBN
                           MOVE    RECE62-PTID     TO  KEY-N-PTID
                           MOVE    RECE62-RECEKA   TO  KEY-N-RECEKA
                           MOVE    RECE62-TEISYUTUSAKI TO
                                                   KEY-N-TEISYUTUSAKI
                           MOVE    RECE62-RECESYUBETU
                                                   TO  KEY-N-RECESYUBETU
                           MOVE    RECE62-HKNJANUM-KEY TO
                                                   KEY-N-HKNJANUM-KEY
      *
                           IF   WRK-PARA-SRYYM     =  ZERO
      *                         医療機関ＩＤ編集
                                MOVE  RECE62-SRYYM   TO
                                                  WRK-SYS-SRYYMD(1:6)
                                MOVE  "01"           TO
                                                  WRK-SYS-SRYYMD(7:2)
                                PERFORM 900-SYSKANRI-HENSYU-SEC
                           END-IF
                         END-IF
      *
                     END-IF
                   END-IF
                 END-IF    
           END-READ
      *
           .
       900-RECE62-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ（公費請求書）順検索処理
      *****************************************************************
       990-RECE62X-READ-SEC           SECTION.
      *
           READ    RECE62X-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE62X
               NOT AT  END
      *            テスト患者・レセプト種別不明のとき読み飛ばす
                   IF    ( RECE62X-TSTPTNUMKBN     =   "1"    )
      *************OR    ( RECE62X-RECESYUBETU     =   "0000" )
                       GO  TO  990-RECE62X-READ-SEC
                   END-IF
      *            点数が全てゼロのとき読み飛ばす
      *************IF      RECE62X-TOTALTEN (1) =   ZERO
      *             AND     RECE62X-TOTALTEN (2) =   ZERO 
      *             AND     RECE62X-TOTALTEN (3) =   ZERO 
      *             AND     RECE62X-TOTALTEN (4) =   ZERO
      *             AND     RECE62X-TOTALTEN (5) =   ZERO
      *                 GO  TO  990-RECE62X-READ-SEC
      *************END-IF
      *
                   DISPLAY "RECE62X PTNUM=" RECE62X-PTNUM
      *
                   MOVE    ZERO                TO  FLG-RECE62X
                   ADD     1                   TO  CNT-RECE62X
      *
           END-READ
      *
           .
       990-RECE62X-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ（未請求解除）順検索処理
      *****************************************************************
       990-RECE62-READ-SEC             SECTION.
      *
           READ    RECE62-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE62
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE62
           END-READ
      *
           .
       990-RECE62-READ-EXT.
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
               MOVE    2           TO  SPA-STS
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    医療機関情報（所在地ほか）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  10   )
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
               MOVE    2                   TO  SPA-STS
           END-IF
      *
      *    帳票別指定医療機関編集情報
           IF      FLG-SYS-1900        =   1
               MOVE    ZERO                TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                       UNTIL ( SYSKAN-IDX              >  10   )
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
                  MOVE    SYS-1900-PRTKBN (4)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO   SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (4)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO   SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
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
      *    傷病名情報データ読み
      ****************************************************************
       900-RECE631-NEXT-SEC           SECTION.
      *
           READ    RECE631-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE631
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE631
                   IF      RECE631-TENKIKBN     =   "8"
                       GO  TO  900-RECE631-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       900-RECE631-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    	摘要欄情報データ読み
      ****************************************************************
       900-RECE661-NEXT-SEC           SECTION.
      *
           READ    RECE661-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE661
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE661
                   ADD     1         TO  CNT-RECE661
                   IF      RECE661-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE661-SRYKBN
                   END-IF    
           END-READ
      *
           .
       900-RECE661-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み
      ****************************************************************
       900-RECE68-NEXT-SEC           SECTION.
      *
           READ    RECE68-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE68
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE68
           END-READ
      *
           .
       900-RECE68-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険履歴情報データ読み
      ****************************************************************
       900-RECE69-NEXT-SEC           SECTION.
      *
           READ    RECE69-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE69
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE69
           END-READ
      *
           .
       900-RECE69-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    県単独項目情報データ読み
      ****************************************************************
       900-RECE67-READ-SEC           SECTION.
      *
           READ    RECE67-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE67
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE67
           END-READ
      *
           .
       900-RECE67-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
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
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-PATH            TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "RECEPRT-KEY9"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "RECEPRT-KEY9"      TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "RECEPRT-KEY9"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
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
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KANRIMST
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       910-HKNMST-NEXT-SEC              SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       910-HKNMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（主キー）
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスター読込（主キー）
      *****************************************************************
       900-KOHSKY-INV-SEC         SECTION.
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "KOHSKY-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE   "KOHSKY-KEY"         TO  ORC-DBPATH
               MOVE    "DBFETCH"           TO  MCP-FUNC
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-KOHSKY
               ELSE
                   MOVE    1               TO  FLG-KOHSKY
               END-IF
           ELSE
               MOVE    1                   TO  FLG-KOHSKY
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "KOHSKY-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       900-KOHSKY-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
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
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
      *    編綴管理レコード（管理部）読込
      *****************************************************************
       900-RECE-PUTH-READ-SEC        SECTION.
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_rece_puth" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-RECE-PUTH-READ-N-SEC
           ELSE
               INITIALIZE                  RECE-PUTH-REC
               MOVE    1               TO  FLG-RECE-PUTH
           END-IF
      *
           MOVE    "tbl_rece_puth" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE-PUTH-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴管理レコード（管理部）読込
      *****************************************************************
       900-RECE-PUTH-READ-N-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTH
               MOVE    MCPDATA-REC         TO  RECE-PUTH-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTH
           END-IF
      *
           .
       900-RECE-PUTH-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴管理レコード（明細部）読込
      *****************************************************************
       900-RECE-PUTB-READ-N-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE-PUTB
               MOVE    MCPDATA-REC         TO  RECE-PUTB-REC
           ELSE
               MOVE    1                   TO  FLG-RECE-PUTB
           END-IF
      *
           .
       900-RECE-PUTB-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           .
       900-CLOSE-EXT.
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
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
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
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
