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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト（労災）作成−印刷指示（Ｒ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/08/29  レセプトが選択されていないとき
      *                                     エラーとする
      * 
      *  01.01.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.01.02    NACL-藤原    02/09/20  自賠責の追加
      *  01.01.03    NACL-藤原    03/01/09  通し番号のためのパラメタ追加 
      *  01.01.04    NACL-藤原    03/02/08  確定のとき自動的に情報確認画面を表示
      *  01.01.05    NACL-藤原    03/02/25  プリンタ名をオンラインより渡す
      *  01.01.06    NACL-藤原    03/03/03  再印刷機能追加
      *　入院機能追加　
      *  01.04.01    NACL-藤原    03/06/05  労災・自賠責の入院処理追加
      *  01.04.02    NACL-藤原    03/08/05  ヴで始まるカナ氏名のレセプト
      *                                     を対象とするように修正
      *  01.04.03    NACL-藤原    05/01/21  空白で始まるカナ氏名のレセプト
      *                                     を対象とするように修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *****レセプト明細ファイル シェル領域 
      *****COPY    "CPCOMMONSHELL1.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    帳票出力用パラメタ  
       01  WRK-RECE09-REC. 
           03  WRK-RECE09-KEY.
               05  WRK-RECE09-PRTID        PIC X(04).
               05  WRK-RECE09-SRYYM        PIC X(06).
               05  WRK-RECE09-CREYMD       PIC X(08).
               05  WRK-RECE09-CREHMS       PIC X(06).
               05  WRK-RECE09-PREFKBN      PIC X(01).
               05  WRK-RECE09-TEISYUTUSAKI PIC X(01).
               05  WRK-RECE09-RECESYUBETU  PIC X(04).
               05  WRK-RECE09-NYUGAIKBN    PIC X(01).
               05  WRK-RECE09-PRTJYUN      PIC X(02).
           03  WRK-RECE09-STDATA           PIC X(100).
           03  WRK-RECE09-EDDATA           PIC X(100).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-R04.
           03  SPA-R04-AREA.
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(02).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-FILEKBN             PIC X(40).
                   07  SPA-GMN-CREYMD              PIC X(22).
                   07  SPA-GMN-PAGEH               PIC 9(04).
                   07  SPA-GMN-KENSUH              PIC 9(04).
                   07  SPA-GMN-PRTJYUN-G.
                       09  SPA-GMN-PRTJYUN         PIC X(02).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PRTJYUNMEI      PIC X(22).
                   07  SPA-GMN-PRTJYUNLST          OCCURS  5.
                       09  SPA-GMN-PRTJYUNL        PIC X(02).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PRTJYUNMEIL     PIC X(22).
      *----(01.00.01) LINE ADD START --------------------------------
                   07  SPA-GMN-PREFKBN-G.
                       09  SPA-GMN-PREFKBN         PIC X(01).
                       09  SPA-GMN-F1              PIC X(01).
                       09  SPA-GMN-PREFKBNMEI      PIC X(04).
                   07  SPA-GMN-PREFKBNLST          OCCURS  3.
                       09  SPA-GMN-PREFKBNL        PIC X(01).
                       09  SPA-GMN-F1L             PIC X(01).
                       09  SPA-GMN-PREFKBNMEIL     PIC X(04).
      *----(01.00.01) LINE ADD END   --------------------------------
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-G           OCCURS   25.
                           11  SPA-GMN-TSTDATA     PIC X(100).
                           11  SPA-GMN-TEDDATA     PIC X(100).
                           11  SPA-GMN-TCHK.
                               13  SPA-GMN-TCHK-VALUE
                                                   PIC X(01).
                               13  SPA-GMN-TCHK-STYLE
                                                   PIC X(20).
                   07  SPA-GMN-TBL1.
                       09  SPA-GMN-TPAGE           PIC 9(04)
                                                   OCCURS   25.
                       09  SPA-GMN-TKENSU          PIC 9(04)
                                                   OCCURS   25.
                       09  SPA-GMN-TSYURUINAME     PIC X(40)
                                                   OCCURS   25.
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-WIDGET              PIC X(08). 
                   07  SPA-NAI-1031-PRTNM          PIC X(16).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBL-G           OCCURS  25.
                           11  SPA-NAI-PRTJYUNI    PIC X(02).
                           11  SPA-NAI-TEISYUTUSAKI
                                                   PIC X(01).
                           11  SPA-NAI-SYUBETU-G.
                               13  SPA-NAI-SYUBETU PIC X(04)
                                                   OCCURS 10.
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  STS-RECEERR         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).
               05  FILLER          PIC X(01).
               05  WRK-WIDGET3     PIC 9(02). 
           03  WRK-RIDMSG          PIC X(70).
           03  WRK-Z9              PIC ZZZ9.
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-NYUGAIKBN       PIC X(06).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RECEKBN         PIC X(08).
           03  WRK-PREFKBN         PIC X(01).
           03  WRK-TEISYUTUSAKI    PIC X(01).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-PAGE            PIC ZZZZ9.
      *
           03  WRK-RECE08-PRTJYUN  PIC 9(02).
      *
           03  WRK-PRTKBN          PIC 9(01).             
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *      
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *                             
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".             
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "ORCA-DBPATH".         
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK2002.INC".
      *
      *    レセプト作成指示（印刷順）情報
           COPY    "CPSK2003.INC".
      *    レセプト作成指示（レセプト種別）情報
           COPY    "CPSK2004.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
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
           COPY    "R01.INC".
           COPY    "R02.INC".
           COPY    "R03.INC".
           COPY    "R04.INC".
           COPY    "R98.INC".
           COPY    "R99.INC".
           COPY    "RERR.INC".
           COPY    "RID1.INC".
           COPY    "RID2.INC".
           COPY    "RID3.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R04
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R04         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "RERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "R04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R04
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
                   PERFORM 340-R99-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           INITIALIZE              SPA-R04-AREA
      *　　ファイル作成区分
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
                   MOVE    SPA-R01-SRYYM       TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "3"
                           MOVE    "・労災"        TO  WRK-RECEKBN
                       WHEN    "4"
                           MOVE    "・自賠責"    TO  WRK-RECEKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   STRING  WRK-HENYMDG3(1:16) DELIMITED  BY  SIZE
                           WRK-NYUGAIKBN      DELIMITED  BY  SPACE 
                           WRK-RECEKBN        DELIMITED  BY  SPACE 
                           "一括作成分"       DELIMITED  BY  SIZE
                           INTO    SPA-GMN-FILEKBN
                   END-STRING
               WHEN    "2"
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "3"
                           MOVE    "労災"     TO  WRK-RECEKBN
                       WHEN    "4"
                           MOVE    "自賠責"   TO  WRK-RECEKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   STRING  WRK-NYUGAIKBN      DELIMITED  BY  SPACE 
                           WRK-RECEKBN        DELIMITED  BY  SPACE 
                           "個別作成分"       DELIMITED  BY  SIZE
                           INTO    SPA-GMN-FILEKBN
                   END-STRING
           END-EVALUATE
      *　　ファイル作成日
           IF      SPA-R01-RECE07-CREYMD   NOT =   SPACE
               MOVE    SPA-R01-RECE07-CREYMD
                                           TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1        TO  SPA-GMN-CREYMD
               MOVE    SPA-R01-RECE07-CREHMS(1:2)
                                           TO  SPA-GMN-CREYMD (13:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (15:1)
               MOVE    SPA-R01-RECE07-CREHMS(3:2)
                                           TO  SPA-GMN-CREYMD (16:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (18:1)
               MOVE    SPA-R01-RECE07-CREHMS(5:2)
                                           TO  SPA-GMN-CREYMD (19:2)
           END-IF
      *    印刷順
           MOVE    "03"                  TO  SPA-GMN-PRTJYUNL    (1)
           MOVE    "カナ氏名順"          TO  SPA-GMN-PRTJYUNMEIL (1)
           MOVE    "04"                  TO  SPA-GMN-PRTJYUNL    (2)
           MOVE    "患者番号順"          TO  SPA-GMN-PRTJYUNMEIL (2)
      *
           MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN-G
      *    県内・県外
           MOVE    "1"                   TO  SPA-GMN-PREFKBNL    (1)
           MOVE    "全部"                TO  SPA-GMN-PREFKBNMEIL (1)
      *
           MOVE    SPA-GMN-PREFKBNLST(01)
                                         TO  SPA-GMN-PREFKBN-G
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF                                     
      *
           MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-1031-PRTNM
      *
           PERFORM 3101-PAGE-HENSYU-03-SEC     
      *
           MOVE    4              TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       3101-PAGE-HENSYU-03-SEC            SECTION.
      *
           MOVE    SPA-GMN-PREFKBNLST(01)
                                       TO  SPA-GMN-PREFKBN-G
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-TBL1
                                       SPA-NAI-TBL   
           MOVE    ZERO                TO  SPA-GMN-PAGEH
                                           SPA-GMN-KENSUH                               
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
               MOVE    "F"             TO  SPA-GMN-TCHK-VALUE (IDX)
           END-PERFORM
      *
      *        レセプト作成指示（印刷順）読み込み
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
      *****MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SPA-R01-SRYYM       TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
      * 
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX     
      *****PERFORM UNTIL       (IDX            =   24   )  OR
      *****                    (FLG-SYSKANRI   =   1    )
           PERFORM    UNTIL   FLG-SYSKANRI    =   1
             MOVE    MCPDATA-REC       TO  SYS-2003-REC
             IF      SYS-2003-NYUGAIKBN      =
                                         SPA-R01-RECE07-NYUGAIKBN
               EVALUATE    SPA-R01-RECE07-RECEKBN
                   WHEN    "3"
                       IF      SYS-2003-TEISYUTUSAKI =   "3"
                           MOVE    SYS-2003-DSPSEQ         TO  IDX                            
      *************        MOVE    SYS-2003-KBNCD (1:2)    TO 
                           MOVE    SYS-2003-DSPSEQ (2:2)   TO                         
                                         SPA-NAI-PRTJYUNI    (IDX)
                           MOVE    SYS-2003-TEISYUTUSAKI
                                         TO  SPA-NAI-TEISYUTUSAKI(IDX)
                           MOVE    SYS-2003-SYUBETU-G
                                         TO  SPA-NAI-SYUBETU-G   (IDX)
                           MOVE    SYS-2003-SYURUINAME
                                         TO  SPA-GMN-TSYURUINAME (IDX)
      *************        MOVE    "T"   TO  SPA-GMN-TCHK-VALUE  (IDX)
                       END-IF
                   WHEN    "4"
                       IF      SYS-2003-TEISYUTUSAKI =   "4"
                           ADD     1             TO  IDX  
      *************        MOVE    SYS-2003-KBNCD (1:2)    TO 
                           MOVE    SYS-2003-DSPSEQ (2:2)   TO                         
                                         SPA-NAI-PRTJYUNI    (IDX)
                           MOVE    SYS-2003-TEISYUTUSAKI
                                         TO  SPA-NAI-TEISYUTUSAKI(IDX)
                           MOVE    SYS-2003-SYUBETU-G
                                         TO  SPA-NAI-SYUBETU-G   (IDX)
                           MOVE    SYS-2003-SYURUINAME
                                         TO  SPA-GMN-TSYURUINAME (IDX)
      *************        MOVE    "T"   TO  SPA-GMN-TCHK-VALUE  (IDX)
                       END-IF
               END-EVALUATE
             END-IF  
      *
             MOVE    "SYSKANRI-KEY2"       TO  ORC-DBPATH
             PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *                                      
      *    レセプト種別不明の編集     
           MOVE    "25"              TO  SPA-NAI-PRTJYUNI    (25)
           MOVE    "3"               TO  SPA-NAI-TEISYUTUSAKI(25)
           MOVE    "0000"            TO  SPA-NAI-SYUBETU     (25 1)
           MOVE    "レセプト種別不明"
                                     TO  SPA-GMN-TSYURUINAME (25)
      *****MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (25)
      *
      *    ページ数・件数設定
      *    続紙のときは総件計に加算しない
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       24
                   OR      SPA-NAI-PRTJYUNI(IDX)   =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TPAGE  (IDX)
               MOVE    ZERO                TO  SPA-GMN-TKENSU (IDX)
      *
               INITIALIZE                  RECE07-REC
               MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
               EVALUATE    SPA-R01-RECE07-RECEKBN
                   WHEN    "3"
                       MOVE    "3"             TO  RECE07-TEISYUTUSAKI
                   WHEN    "4"
                       MOVE    "4"             TO  RECE07-TEISYUTUSAKI
               END-EVALUATE        
               MOVE    SPA-NAI-SYUBETU (IDX 1)
                                           TO  RECE07-RECESYUBETU
               MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC                             
               PERFORM 900-RECE07-READ-SEC
               IF      FLG-RECE07          =   ZERO
                   ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (IDX)
                                               SPA-GMN-PAGEH
                   ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(IDX)
                   IF      RECE07-RECESYUBETU (4:1)
                                       NOT =   "3"
                       ADD     RECE07-KENSU    TO  SPA-GMN-KENSUH
                   END-IF    
               END-IF
           END-PERFORM
      *    レセプト種別不明のレセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           EVALUATE    SPA-R01-RECE07-RECEKBN
               WHEN    "3"
                   MOVE    "3"             TO  RECE07-TEISYUTUSAKI
               WHEN    "4"
                   MOVE    "4"             TO  RECE07-TEISYUTUSAKI
           END-EVALUATE        
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   ZERO
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (25)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(25)
                                           SPA-GMN-KENSUH
           END-IF
           .
       3101-PAGE-HENSYU-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"           TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1001"
                   WHEN    "1002"    
                       PERFORM 450-PRINT-SEC
                   WHEN    "8888"
                       CONTINUE    
               END-EVALUATE        
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD      
           .
       330-RID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-R99-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
                                       SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
           .
       340-R99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GIDMSG-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *     個別出力
               WHEN    "CLICKED"       ALSO    "CHK01"
               WHEN    "CLICKED"       ALSO    "CHK02"
               WHEN    "CLICKED"       ALSO    "CHK03"
               WHEN    "CLICKED"       ALSO    "CHK04"
               WHEN    "CLICKED"       ALSO    "CHK05"
               WHEN    "CLICKED"       ALSO    "CHK06"
               WHEN    "CLICKED"       ALSO    "CHK07"
               WHEN    "CLICKED"       ALSO    "CHK08"
               WHEN    "CLICKED"       ALSO    "CHK09"
               WHEN    "CLICKED"       ALSO    "CHK10"
               WHEN    "CLICKED"       ALSO    "CHK11"
               WHEN    "CLICKED"       ALSO    "CHK12"
               WHEN    "CLICKED"       ALSO    "CHK13"
               WHEN    "CLICKED"       ALSO    "CHK14"
               WHEN    "CLICKED"       ALSO    "CHK15"
               WHEN    "CLICKED"       ALSO    "CHK16"
               WHEN    "CLICKED"       ALSO    "CHK17"
               WHEN    "CLICKED"       ALSO    "CHK18"
               WHEN    "CLICKED"       ALSO    "CHK19"
               WHEN    "CLICKED"       ALSO    "CHK20"
               WHEN    "CLICKED"       ALSO    "CHK21"
               WHEN    "CLICKED"       ALSO    "CHK22"
               WHEN    "CLICKED"       ALSO    "CHK23"
               WHEN    "CLICKED"       ALSO    "CHK24"
               WHEN    "CLICKED"       ALSO    "CHK25"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET2         TO  IDX 
                   PERFORM 420-CHK-CHK-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "STDATA01"
               WHEN    "ACTIVATE"      ALSO    "STDATA02"
               WHEN    "ACTIVATE"      ALSO    "STDATA03"
               WHEN    "ACTIVATE"      ALSO    "STDATA04"
               WHEN    "ACTIVATE"      ALSO    "STDATA05"
               WHEN    "ACTIVATE"      ALSO    "STDATA06"
               WHEN    "ACTIVATE"      ALSO    "STDATA07"
               WHEN    "ACTIVATE"      ALSO    "STDATA08"
               WHEN    "ACTIVATE"      ALSO    "STDATA09"
               WHEN    "ACTIVATE"      ALSO    "STDATA10"
               WHEN    "ACTIVATE"      ALSO    "STDATA11"
               WHEN    "ACTIVATE"      ALSO    "STDATA12"
               WHEN    "ACTIVATE"      ALSO    "STDATA13"
               WHEN    "ACTIVATE"      ALSO    "STDATA14"
               WHEN    "ACTIVATE"      ALSO    "STDATA15"
               WHEN    "ACTIVATE"      ALSO    "STDATA16"
               WHEN    "ACTIVATE"      ALSO    "STDATA17"
               WHEN    "ACTIVATE"      ALSO    "STDATA18"
               WHEN    "ACTIVATE"      ALSO    "STDATA19"
               WHEN    "ACTIVATE"      ALSO    "STDATA20"
               WHEN    "ACTIVATE"      ALSO    "STDATA21"
               WHEN    "ACTIVATE"      ALSO    "STDATA22"
               WHEN    "ACTIVATE"      ALSO    "STDATA23"
               WHEN    "ACTIVATE"      ALSO    "STDATA24"
               WHEN    "ACTIVATE"      ALSO    "STDATA25"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   PERFORM 4103-TSTDATA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "EDDATA01"
               WHEN    "ACTIVATE"      ALSO    "EDDATA02"
               WHEN    "ACTIVATE"      ALSO    "EDDATA03"
               WHEN    "ACTIVATE"      ALSO    "EDDATA04"
               WHEN    "ACTIVATE"      ALSO    "EDDATA05"
               WHEN    "ACTIVATE"      ALSO    "EDDATA06"
               WHEN    "ACTIVATE"      ALSO    "EDDATA07"
               WHEN    "ACTIVATE"      ALSO    "EDDATA08"
               WHEN    "ACTIVATE"      ALSO    "EDDATA09"
               WHEN    "ACTIVATE"      ALSO    "EDDATA10"
               WHEN    "ACTIVATE"      ALSO    "EDDATA11"
               WHEN    "ACTIVATE"      ALSO    "EDDATA12"
               WHEN    "ACTIVATE"      ALSO    "EDDATA13"
               WHEN    "ACTIVATE"      ALSO    "EDDATA14"
               WHEN    "ACTIVATE"      ALSO    "EDDATA15"
               WHEN    "ACTIVATE"      ALSO    "EDDATA16"
               WHEN    "ACTIVATE"      ALSO    "EDDATA17"
               WHEN    "ACTIVATE"      ALSO    "EDDATA18"
               WHEN    "ACTIVATE"      ALSO    "EDDATA19"
               WHEN    "ACTIVATE"      ALSO    "EDDATA20"
               WHEN    "ACTIVATE"      ALSO    "EDDATA21"
               WHEN    "ACTIVATE"      ALSO    "EDDATA22"
               WHEN    "ACTIVATE"      ALSO    "EDDATA23"
               WHEN    "ACTIVATE"      ALSO    "EDDATA24"
               WHEN    "ACTIVATE"      ALSO    "EDDATA25"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   PERFORM 4104-TEDDATA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN"
                   PERFORM 4105-PRTJYUN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PREFKBN"
                   PERFORM 4106-PREFKBN-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力開始（個別）チェック処理
      *****************************************************************
       4103-TSTDATA-CHK-SEC          SECTION.
      *
           MOVE    2                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    R04-TSTDATA (IDX)       TO  SPA-GMN-TSTDATA (IDX)
      *
           IF      SPA-GMN-TSTDATA (IDX)  NOT =   SPACE   
               EVALUATE    SPA-GMN-PRTJYUN   
                   WHEN    "03"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "2"
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TSTDATA (IDX)
                           IF      SPA-GMN-TSTDATA (IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "04"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TSTDATA (IDX)(1:1)
                                              <  "0"    OR
                                              >  "9"
                               MOVE    "0001"             TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
               ADD     1                   TO  SPA-GMN-CUR
           END-IF 
           .
       4103-TSTDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力終了（個別）チェック処理
      *****************************************************************
       4104-TEDDATA-CHK-SEC          SECTION.
      *
           MOVE    3                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    R04-TEDDATA (IDX)       TO  SPA-GMN-TEDDATA (IDX)
      *
           IF      SPA-GMN-TEDDATA (IDX)  NOT =   SPACE   
               EVALUATE    SPA-GMN-PRTJYUN   
                   WHEN    "03"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "2"
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEDDATA (IDX)
                           IF      SPA-GMN-TEDDATA (IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "04"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TEDDATA (IDX)(1:1)
                                              <  "0"    OR
                                              >  "9"
                               MOVE    "0001"             TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                           END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
               IF      IDX                 =   25
                   MOVE    99                  TO  SPA-GMN-CUR
               ELSE 
                   ADD     1                   TO  SPA-GMN-CUR
               END-IF
           END-IF 
           .
       4104-TEDDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順入力処理
      *****************************************************************
       4105-PRTJYUN-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    R04-PRTJYUN         TO  SPA-GMN-PRTJYUN-G
      *
           IF      SPA-GMN-PRTJYUNMEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                       OR      SPA-GMN-PRTJYUNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    5                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-HENFLG          =   ZERO     
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF    
           .
       4105-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    県内・県外入力処理
      *****************************************************************
       4106-PREFKBN-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    R04-PREFKBN         TO  SPA-GMN-PREFKBN-G
      *
           IF      SPA-GMN-PREFKBNMEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF    
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
                   OR      SPA-GMN-PREFKBNLST  (IDX)   =   SPACE
               IF      SPA-GMN-PREFKBN NOT =   SPA-GMN-PREFKBNL (IDX)
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO  TO  4106-PREFKBN-CHK-EXT
               END-IF
           END-PERFORM    
      * 
           MOVE    "0001"              TO  SPA-ERRCD
           .
       4106-PREFKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る前処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-RIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "R03"               TO  SPA-SAKIPG
           MOVE    "R04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別出力  入力処理
      *****************************************************************
       420-CHK-CHK-SEC                SECTION.
      *
           MOVE    1                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
      *
           IF      R04-TCHK-VALUE (IDX)   =   "T"
           AND     SPA-GMN-TPAGE  (IDX)   =   ZERO
               MOVE    "0002"                 TO  SPA-ERRCD
           ELSE     
               MOVE    R04-TCHK-VALUE (IDX)   TO
                                          SPA-GMN-TCHK-VALUE (IDX)
           END-IF                               
      *
           ADD     1                  TO  SPA-GMN-CUR
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力前処理
      *****************************************************************
       450-PRINT-GIDMSG-SEC                SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   GO  TO  450-PRINT-GIDMSG-EXT
               END-IF
           END-IF    
      *
      *    レセプトが選択されているか
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
               IF      SPA-GMN-TCHK-VALUE (IDX)    =   "T"
                   MOVE    1               TO  IDZ
                   MOVE    25              TO  IDX
               END-IF
           END-PERFORM
           IF      IDZ                 =   ZERO
               MOVE    "0008"          TO  SPA-ERRCD
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
      *    複数種類のレセプトが選択されているとき
           MOVE    ZERO                TO  WRK-PRTKBN
                                           FLG-ERR
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
                   OR      FLG-ERR     =   1
               IF      SPA-GMN-TCHK-VALUE (IDX)    =   "T"
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "3"
                           EVALUATE    IDX
                               WHEN    1
                               WHEN    2
                               WHEN    25
                                   IF      WRK-PRTKBN  =   ZERO   OR  1
                                       MOVE    1       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                               WHEN    3
                               WHEN    6
                                   IF      WRK-PRTKBN  =   ZERO   OR  2
                                       MOVE    2       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                               WHEN    4
                               WHEN    5
                                   IF      WRK-PRTKBN  =   ZERO   OR  3
                                       MOVE    3       TO  WRK-PRTKBN
                                   ELSE
                                      MOVE    1        TO  FLG-ERR
                                   END-IF         
                               WHEN    7
                                   IF      WRK-PRTKBN  =   ZERO   OR  4
                                       MOVE    4       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                               WHEN    8
                                   IF      WRK-PRTKBN  =   ZERO   OR  5
                                       MOVE    5       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                           END-EVALUATE
                       WHEN    "4"
                           EVALUATE    IDX
                               WHEN    1
                               WHEN    25
                                   IF      WRK-PRTKBN  =   ZERO   OR  1
                                       MOVE    1       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                               WHEN    2
                                   IF      WRK-PRTKBN  =   ZERO   OR  2
                                       MOVE    2       TO  WRK-PRTKBN
                                   ELSE
                                       MOVE    1       TO  FLG-ERR
                                   END-IF         
                           END-EVALUATE
                   END-EVALUATE        
               END-IF
           END-PERFORM
                                         
      *    ガイドメッセージセット
           IF      FLG-ERR                     =   1
               MOVE    "1001"              TO  SPA-RIDCD
           ELSE
               MOVE    "1002"              TO  SPA-RIDCD
           END-IF
      *
           .
       450-PRINT-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       450-PRINT-SEC                SECTION.
      *
      *    ジョブ開始セット
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
      *
      *    帳票ファイル作成処理
           MOVE    ZERO             TO  WRK-RECE08-PRTJYUN  
      *
           EVALUATE    SPA-R01-RECE07-RECEKBN
               WHEN    "3"
                   MOVE    "3"              TO  WRK-TEISYUTUSAKI
               WHEN    "4"
                   MOVE    "4"              TO  WRK-TEISYUTUSAKI
           END-EVALUATE        
           MOVE    "0"              TO  WRK-PREFKBN
           PERFORM 4501-PRTJYUN-PRINT-SEC
      *
      *    レセプト不明分出力
           IF      SPA-ERRCD        =   SPACE
               IF      SPA-GMN-TCHK-VALUE  (25)   =   "T"
               AND     SPA-GMN-TPAGE       (25)   >   ZERO
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "3"
                           MOVE    "3"     TO  WRK-TEISYUTUSAKI
                       WHEN    "4"
                           MOVE    "4"     TO  WRK-TEISYUTUSAKI
                   END-EVALUATE        
                   MOVE    "0"             TO  WRK-PREFKBN
                   MOVE    25              TO  IDX
                   MOVE    1               TO  IDY
                   PERFORM 45011-RECEPRT-HENSYU-SEC
               END-IF
           END-IF
      *
      *    印刷ＤＢのＫＥＹ用     
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
      *    印刷処理
      *
      *****MOVE    "recept7.sh"        TO  SHELL-SHELL
      *    MOVE    SHELL-NAME          TO  SHELL-NAME
      *    MOVE    SPA-TERMID          TO  SHELL-TERMID
      *    MOVE    SPA-R01-RECE07-SRYYM
      *                                TO  SHELL-SRYYM    
      *    MOVE    SPA-SYSYMD          TO  SHELL-SYSYMD
      *    MOVE    "1"                 TO  SHELL-SYOKBN
      *****MOVE    "0"                 TO  SHELL-RECEKBN 
      *     
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
      *****MOVE    RECEERR             TO  SHELL-ERRFILE
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept7.sh"        TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-SHELL-NAME      TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID          TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                       TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD          TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1      TO  SHELLTBL-ARG3 
           MOVE    "1"                 TO  SHELLTBL-ARG4
           IF      SPA-R01-FILEKBN =   "1"
               MOVE    "1"             TO  SHELLTBL-ARG5
           ELSE
               MOVE    "2"             TO  SHELLTBL-ARG5
           END-IF    
           MOVE    RECEERR             TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-KEY      TO  SHELLTBL-ARG7 
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG8
           MOVE    SPA-NAI-1031-PRTNM
                                   TO  SHELLTBL-ARG11
      *                                 
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    1               TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept7.sh"    TO  LNK-PRTKANRI-SHELLID
           IF      SPA-R01-RECE07-NYUGAIKBN    =   "1"
               EVALUATE    SPA-R01-RECE07-RECEKBN
                   WHEN    "3"
                       MOVE    3           TO  LNK-PRTKANRI-PRIORITY
                   WHEN    "4"
                       MOVE    5           TO  LNK-PRTKANRI-PRIORITY
               END-EVALUATE
           ELSE
               EVALUATE    SPA-R01-RECE07-RECEKBN
                   WHEN    "3"
                       MOVE    4           TO  LNK-PRTKANRI-PRIORITY
                   WHEN    "4"
                       MOVE    6           TO  LNK-PRTKANRI-PRIORITY
               END-EVALUATE
           END-IF
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *****MOVE    SYS-1031-PRTNM (5)
      *****                        TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG12       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG13     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG14       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG15       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG16       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG17       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG18       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG19      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG20       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG21      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG22
      *     
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL
                                   TO  MCP-PATH
           CALL    "MCPSUB"        USING
                                   MCPAREA
                                   MCPDATA-REC                         
      *
           COMPUTE SPA-R01-JOBCNT  =   WRK-RECE08-PRTJYUN  +   1
      *                         
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理
      *****************************************************************
       4501-PRTJYUN-PRINT-SEC         SECTION.
      *
      *    印刷順にファイル読み込み
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       24
                   OR      SPA-NAI-PRTJYUNI(IDX)   =   SPACE
                   OR      SPA-ERRCD           NOT =   SPACE    
               IF      SPA-GMN-TCHK-VALUE  (IDX)   =   "T"
               AND     SPA-GMN-TPAGE       (IDX)   >   ZERO
               AND     SPA-NAI-TEISYUTUSAKI (IDX)  =   WRK-TEISYUTUSAKI
                   PERFORM 45011-RECEPRT-HENSYU-SEC  
               END-IF
           END-PERFORM
           .
       4501-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       45011-RECEPRT-HENSYU-SEC         SECTION.
      *
      *    印刷用パラメタ
           INITIALIZE                  WRK-RECE09-REC
      *
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  WRK-RECE09-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  WRK-RECE09-SRYYM
           ELSE
               MOVE    ZERO                TO  WRK-RECE09-SRYYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD   TO  WRK-RECE09-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS   TO  WRK-RECE09-CREHMS
           MOVE    SPA-NAI-SYUBETU (IDX 1)
                                       TO  WRK-RECE09-RECESYUBETU
           MOVE    WRK-TEISYUTUSAKI    TO  WRK-RECE09-TEISYUTUSAKI
           MOVE    WRK-PREFKBN         TO  WRK-RECE09-PREFKBN
           MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
      *****MOVE    "2"                 TO  WRK-RECE09-NYUGAIKBN
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  WRK-RECE09-NYUGAIKBN
           MOVE    SPA-GMN-TSTDATA (IDX)
                                       TO  WRK-RECE09-STDATA
           MOVE    SPA-GMN-TEDDATA (IDX)
                                       TO  WRK-RECE09-EDDATA 
           IF      WRK-RECE09-STDATA   =   SPACE
               IF      SPA-GMN-PRTJYUN =   "03"
      *************MOVE    "Ａ"        TO  WRK-RECE09-STDATA (1:2)
                   MOVE    SPACE       TO  WRK-RECE09-STDATA
               ELSE
                   MOVE    "0"         TO  WRK-RECE09-STDATA (1:1)
               END-IF
           ELSE     
               IF      SPA-GMN-PRTJYUN =   "04"
                   PERFORM VARYING IDZ FROM    20  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-RECE09-STDATA(IDZ:1)
                                                   NOT =   SPACE
                           MOVE    1       TO  IDZ 
                       ELSE  
                           MOVE    "Z"     TO  WRK-RECE09-STDATA(IDZ:1)
                       END-IF
                   END-PERFORM
               END-IF     
           END-IF
           IF      WRK-RECE09-EDDATA       =   SPACE
               IF      SPA-GMN-PRTJYUN     =   "03"
                   MOVE    "ヵ"            TO  WRK-RECE09-EDDATA (1:2)
               ELSE
                   MOVE    "Z"             TO  WRK-RECE09-EDDATA (1:1)
               END-IF
           ELSE     
               IF      SPA-GMN-PRTJYUN     =   "04"
                   PERFORM VARYING IDZ FROM    20  BY  -1
                           UNTIL   IDZ <       1
                       IF      WRK-RECE09-EDDATA(IDZ:1)
                                                       NOT =   SPACE
                           MOVE    1       TO  IDZ 
                       ELSE  
                           MOVE    "Z"     TO  WRK-RECE09-EDDATA(IDZ:1)
                       END-IF
                   END-PERFORM
               END-IF     
           END-IF
      *
      *　　（ＯＲＣＲ０１０１）　
      *****MOVE    "recept7.sh"        TO  SHELL-SHELL
      *    MOVE    WRK-SHELL-NAME      TO  SHELL-NAME
      *    MOVE    SPA-TERMID          TO  SHELL-TERMID
      *    MOVE    SPA-R01-RECE07-SRYYM
      *                                TO  SHELL-SRYYM    
      *    MOVE    SPA-SYSYMD          TO  SHELL-SYSYMD
      *    MOVE    "1"                 TO  SHELL-SYOKBN
      *****MOVE    "0"                 TO  SHELL-RECEKBN
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
      *****MOVE    RECEERR             TO  SHELL-ERRFILE
      *
           OPEN    OUTPUT   RECEERR-FILE
           CLOSE   RECEERR-FILE
      *
           ADD     1               TO  WRK-RECE08-PRTJYUN                 
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "recept7.sh"        TO  WRK-SHELL-SHELL
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-SHELL-NAME      TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID          TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                       TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD          TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1      TO  SHELLTBL-ARG3 
           MOVE    "0"                 TO  SHELLTBL-ARG4
           MOVE    "0"                 TO  SHELLTBL-ARG5
           MOVE    RECEERR             TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-KEY      TO  SHELLTBL-ARG7 
           MOVE    WRK-RECE09-STDATA   TO  SHELLTBL-ARG8
           MOVE    WRK-RECE09-EDDATA   TO  SHELLTBL-ARG9
           MOVE    WRK-RECE08-PRTJYUN  TO  SHELLTBL-ARG10
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL
                                   TO  MCP-PATH
           CALL    "MCPSUB"        USING
                                       MCPAREA
                                       MCPDATA-REC                         
           .
       45011-RECEPRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *                                      
      *    画面移行用のパラメタ
           PERFORM 4901-PARA-HENSYU-SEC
      *
           MOVE    SPACE               TO  R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC 
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           IF      R99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      R99-COUNT            >   ZERO    
                   IF      JOB-UPDCNT           =   ZERO
                       MOVE    "該当のレセプトはありませんでした"
                                                TO  R99-MSG
                   ELSE
                       MOVE    JOB-UPDCNT           TO  WRK-PAGE
                       STRING  "処理は正常に終了しました　処理枚数は"
                                                DELIMITED  BY  SIZE
                               WRK-PAGE         DELIMITED  BY  SIZE
                               "枚です"         DELIMITED  BY  SIZE
                                                INTO  R99-MSG
                       END-STRING
                   END-IF
                   MOVE   100                   TO
                                            R99-PROGRESSBAR-VALUE                             
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE                                        
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "R04"                TO  SPA-MOTOPG
           MOVE    "R99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       4901-PARA-HENSYU-SEC            SECTION.       
      *
           MOVE    "R04"               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           .
       4901-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R04             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R04"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "R04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE     R04
      *
           MOVE    SPA-GMN-FILEKBN     TO  R04-FILEKBN
      *
           MOVE    SPA-GMN-CREYMD      TO  R04-CREYMD  
      *
           MOVE    SPA-GMN-PAGEH       TO  R04-PAGE  
      *
           MOVE    SPA-GMN-KENSUH      TO  R04-KENSU  
      *
           MOVE    SPA-GMN-PRTJYUN-G   TO  R04-PRTJYUN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   5
                   OR          SPA-GMN-PRTJYUNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PRTJYUNLST(IDX) TO  R04-PRTJYUNLST (IDX)
               MOVE    IDX                     TO  R04-PRTJYUN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PREFKBN-G   TO  R04-PREFKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   3
                   OR          SPA-GMN-PREFKBNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PREFKBNLST(IDX) TO  R04-PREFKBNLST (IDX)
               MOVE    IDX                     TO  R04-PREFKBN-COUNT
           END-PERFORM
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       25
               IF      SPA-NAI-PRTJYUNI (IDX)  =   SPACE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  R04-TCHK-STATE    (IDX)
                                               R04-TSTDATA-STATE (IDX)
                                               R04-TEDDATA-STATE (IDX)
               ELSE 
                   MOVE    WIDGET-NORMAL   TO  R04-TCHK-STATE    (IDX)
                                               R04-TSTDATA-STATE (IDX)
                                               R04-TEDDATA-STATE (IDX)
               END-IF    
               MOVE    SPA-GMN-TSTDATA   (IDX) TO  R04-TSTDATA   (IDX)
               MOVE    SPA-GMN-TEDDATA   (IDX) TO  R04-TEDDATA   (IDX)
               MOVE    SPA-GMN-TKENSU    (IDX) TO  R04-TKENSU    (IDX)
               MOVE    SPA-GMN-TPAGE     (IDX) TO  R04-TPAGE     (IDX)
               MOVE    SPA-GMN-TCHK-VALUE(IDX) TO  R04-TCHK-VALUE(IDX) 
               MOVE    SPA-GMN-TSYURUINAME(IDX) TO  R04-TCHK-LABEL(IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    2
                    MOVE  "PREFKBN"    TO  MCP-WIDGET
             WHEN    3
                    MOVE  "PRTJYUNI"   TO  MCP-WIDGET
             WHEN    5
                    MOVE  "STDATA01"     TO  MCP-WIDGET
             WHEN    6
                    MOVE  "EDDATA01"     TO  MCP-WIDGET
             WHEN    4
                    MOVE  "CHK01"       TO  MCP-WIDGET
             WHEN    8
                    MOVE  "STDATA02"     TO  MCP-WIDGET
             WHEN    9
                    MOVE  "EDDATA02"     TO  MCP-WIDGET
             WHEN    7
                    MOVE  "CHK02"       TO  MCP-WIDGET
             WHEN    11
                    MOVE  "STDATA03"     TO  MCP-WIDGET
             WHEN    12
                    MOVE  "EDDATA03"     TO  MCP-WIDGET
             WHEN    10
                    MOVE  "CHK03"       TO  MCP-WIDGET
             WHEN    14
                    MOVE  "STDATA04"     TO  MCP-WIDGET
             WHEN    15
                    MOVE  "EDDATA04"     TO  MCP-WIDGET
             WHEN    13
                    MOVE  "CHK04"       TO  MCP-WIDGET
             WHEN    17
                    MOVE  "STDATA05"     TO  MCP-WIDGET
             WHEN    18
                    MOVE  "EDDATA05"     TO  MCP-WIDGET
             WHEN    16
                    MOVE  "CHK05"       TO  MCP-WIDGET
             WHEN    20
                    MOVE  "STDATA06"     TO  MCP-WIDGET
             WHEN    21
                    MOVE  "EDDATA06"     TO  MCP-WIDGET
             WHEN    19
                    MOVE  "CHK06"       TO  MCP-WIDGET
             WHEN    23
                    MOVE  "STDATA07"     TO  MCP-WIDGET
             WHEN    24
                    MOVE  "EDDATA07"     TO  MCP-WIDGET
             WHEN    22
                    MOVE  "CHK07"       TO  MCP-WIDGET
             WHEN    26
                    MOVE  "STDATA08"     TO  MCP-WIDGET
             WHEN    27
                    MOVE  "EDDATA08"     TO  MCP-WIDGET
             WHEN   25
                    MOVE  "CHK08"       TO  MCP-WIDGET
             WHEN    29
                    MOVE  "STDATA09"     TO  MCP-WIDGET
             WHEN    30
                    MOVE  "EDDATA09"     TO  MCP-WIDGET
             WHEN    28
                    MOVE  "CHK09"       TO  MCP-WIDGET
             WHEN    32
                    MOVE  "STDATA10"    TO  MCP-WIDGET
             WHEN    33
                    MOVE  "EDDATA10"    TO  MCP-WIDGET
             WHEN    31
                    MOVE  "CHK10"      TO  MCP-WIDGET
             WHEN    35
                    MOVE  "STDATA11"    TO  MCP-WIDGET
             WHEN    36
                    MOVE  "EDDATA11"    TO  MCP-WIDGET
             WHEN    34
                    MOVE  "CHK11"      TO  MCP-WIDGET
             WHEN    38
                    MOVE  "STDATA12"    TO  MCP-WIDGET
             WHEN    39
                    MOVE  "EDDATA12"    TO  MCP-WIDGET
             WHEN    37
                    MOVE  "CHK12"      TO  MCP-WIDGET
             WHEN    41
                    MOVE  "STDATA13"    TO  MCP-WIDGET
             WHEN    42
                    MOVE  "EDDATA13"    TO  MCP-WIDGET
             WHEN    40
                    MOVE  "CHK13"      TO  MCP-WIDGET
             WHEN    44
                    MOVE  "STDATA14"    TO  MCP-WIDGET
             WHEN    45
                    MOVE  "EDDATA14"    TO  MCP-WIDGET
             WHEN    43
                    MOVE  "CHK14"      TO  MCP-WIDGET
             WHEN    47
                    MOVE  "STDATA15"    TO  MCP-WIDGET
             WHEN    48
                    MOVE  "EDDATA15"    TO  MCP-WIDGET
             WHEN    46
                    MOVE  "CHK15"      TO  MCP-WIDGET
             WHEN    50
                    MOVE  "STDATA16"    TO  MCP-WIDGET
             WHEN    51
                    MOVE  "EDDATA16"    TO  MCP-WIDGET
             WHEN    49
                    MOVE  "CHK16"      TO  MCP-WIDGET
             WHEN    53
                    MOVE  "STDATA17"     TO  MCP-WIDGET
             WHEN    54
                    MOVE  "EDDATA17"     TO  MCP-WIDGET
             WHEN    52
                    MOVE  "CHK17"      TO  MCP-WIDGET
             WHEN    56
                    MOVE  "STDATA18"     TO  MCP-WIDGET
             WHEN    57
                    MOVE  "EDDATA18"     TO  MCP-WIDGET
             WHEN    55
                    MOVE  "CHK18"      TO  MCP-WIDGET
             WHEN    59
                    MOVE  "STDATA19"     TO  MCP-WIDGET
             WHEN    60
                    MOVE  "EDDATA19"     TO  MCP-WIDGET
             WHEN    58
                    MOVE  "CHK19"      TO  MCP-WIDGET
             WHEN    62
                    MOVE  "STDATA20"    TO  MCP-WIDGET
             WHEN    63
                    MOVE  "EDDATA20"    TO  MCP-WIDGET
             WHEN    61
                    MOVE  "CHK20"       TO  MCP-WIDGET
             WHEN    65
                    MOVE  "STDATA21"    TO  MCP-WIDGET
             WHEN    66
                    MOVE  "EDDATA21"    TO  MCP-WIDGET
             WHEN    64
                    MOVE  "CHK21"       TO  MCP-WIDGET
             WHEN    68
                    MOVE  "STDATA22"    TO  MCP-WIDGET
             WHEN    69
                    MOVE  "EDDATA22"    TO  MCP-WIDGET
             WHEN    67
                    MOVE  "CHK22"       TO  MCP-WIDGET
             WHEN    71
                    MOVE  "STDATA23"    TO  MCP-WIDGET
             WHEN    72
                    MOVE  "EDDATA23"    TO  MCP-WIDGET
             WHEN    70
                    MOVE  "CHK23"       TO  MCP-WIDGET
             WHEN    74
                    MOVE  "STDATA24"    TO  MCP-WIDGET
             WHEN    75
                    MOVE  "EDDATA24"    TO  MCP-WIDGET
             WHEN    73
                    MOVE  "CHK24"       TO  MCP-WIDGET
             WHEN    77
                    MOVE  "STDATA25"    TO  MCP-WIDGET
             WHEN    78
                    MOVE  "EDDATA25"    TO  MCP-WIDGET
             WHEN    76
                    MOVE  "CHK25"       TO  MCP-WIDGET
             WHEN    99
                    MOVE  "B12"        TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "印刷するレセプトがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE  "印刷用パラメタファイルが作成できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "一括出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "個別出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "印刷するレセプトが選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "5555"
                   MOVE    "レセ電処理中です"          TO  WRK-RIDMSG
               WHEN    "7777"
                   MOVE    "レセプト印刷またはレセ電処理中です"
                                                       TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R04"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE
              "種類の違うレセプトが選択してありますが、よろしいですか？"
                                                     TO  WRK-RIDMSG
               WHEN    "1002"
                   MOVE    "印刷をします"        TO  WRK-RIDMSG
               WHEN    "1003"
                   MOVE    "社保レセ電処理をします"  TO  WRK-RIDMSG
               WHEN    "1004"
                   MOVE    "国保レセ電処理をします"  TO  WRK-RIDMSG
               WHEN    "6666"
                   MOVE    "レセプト印刷中です"      TO  WRK-RIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常に終了しました"
                                                     TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
           MOVE    "戻る"               TO  RID1-B01-LABEL
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R04"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YM   (1:5)  NOT NUMERIC
               INSPECT     WRK-YM       REPLACING   ALL " "
                                                    BY  "0"
               MOVE    WRK-YM(1:1)      TO  WRK-WGO
               MOVE    WRK-YM(2:2)      TO  WRK-WYY
               MOVE    WRK-YM(5:2)      TO  WRK-WMM
           ELSE
               MOVE    WRK-YM(1:5)      TO  WRK-WYMD (1:5)
           END-IF
           MOVE    "01"             TO  WRK-WDD
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
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込
      *****************************************************************
       900-RECE07-READ-SEC             SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "RECEKANRI-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "RECEKANRI-KEY"     TO  ORC-DBPATH
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "RECEKANRI-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込（ＫＥＹ２）
      *****************************************************************
       900-RECE07-NEXT-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
      *
           .
       900-RECE07-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      * 
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
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
      ***********************************************************
      *    ＰＵＴ　処理
      ***********************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
