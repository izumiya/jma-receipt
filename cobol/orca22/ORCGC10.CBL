      ******************************************************************
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
      ****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 簡単修正（Ｃ１０）
      *  管理者            : 
      *  04/02/12    NACL−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/05/14  簡単修正時の開始日と
      *                                     保険組合せのチェックの修正
      *  01.00.02    NACL-藤原    05/05/14  入外区分のよる同一病名の
      *                                     チェックを追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ＳＰＡデータ
           SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
       FD  SPASCR-FILE.
       01  SPA-DATA-REC            PIC X(30000).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡパラメタデータ
       01  FILE-NAME1.
           03  FILLE               PIC X(18)   VALUE
                                               "/var/tmp/C02SPASCR".
           03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
           03  FILLER              PIC X(04)   VALUE   ".TXT".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  10.
      * 
       01  SPA-SCR-REC.
           02  SPA-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-PTBYO-//.
      *01  WRK-PTBYOMEI-AREA.
           02  WRK-HAISIYMD-OCC.
               03  WRK-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-PTBYO-//.
           COPY    "CPC02SPASCR.INC".
      *   
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-SPASCR          PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
           03  WRK-ERR-SRYYMD      PIC X(22).
      *                             
           03  WRK-DBPATH          PIC X(64).
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(04).
               05  WRK-KBNMEI      PIC X(42).
               05  WRK-KBNMEITAN   PIC X(10).                            
      *
           03  WRK-TENKIYMD        PIC X(08).
      *
           03  WRK-TEKSTYMD        PIC 9(08).
           03  WRK-TEKEDYMD        PIC 9(08).
      *
           03  WRK-YMD-9.
               05  WRK-YY          PIC 9(04).
               05  WRK-MD          PIC 9(04).
               05  WRK-MD-R        REDEFINES   WRK-MD. 
                   07  WRK-MM      PIC 9(02).
                   07  WRK-DD      PIC 9(02). 
      *
           03  WRK-KANTAN-HENSYU-AREA.
               05  WRK-HENSYU-NUM  PIC X(40).
               05  WRK-HENSYU-NUM-G    OCCURS  5.
                   07  WRK-HENSYU-NUM-O
                                   PIC X(10).
                   07  WRK-STNUM-O PIC 9(05).
                   07  WRK-EDNUM-O PIC 9(05).
               05  WRK-STNUM       PIC X(05).
               05  WRK-EDNUM       PIC X(05).
               05  WRK-HENSYU-DATA-G.
                   07  WRK-HENSYU-DATA-O
                                   PIC X(10)   OCCURS  7.
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".  
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "C02.INC".
           COPY    "C03.INC".
           COPY    "C04.INC".
           COPY    "C05.INC".
           COPY    "C06.INC".
           COPY    "C07.INC".
           COPY    "C08.INC".
           COPY    "C09.INC".
           COPY    "C10.INC".
           COPY    "C11.INC".
           COPY    "CERR.INC".
           COPY    "CID1.INC".
           COPY    "CID2.INC".
           COPY    "CID3.INC".
           COPY    "CID4.INC".
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
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-C02SPA-SET-SEC
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
      *    画面ＳＰＡ書込み
      *****IF      FLG-END             =   ZERO
      *****    PERFORM 1003-C02SPA-OUT-SEC
      *****END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ＳＰＡデータセット処理
      *****************************************************************
      *
       1002-C02SPA-SET-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    INPUT               SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               GO      TO         1002-C02SPA-SET-EXT
           END-IF
      *
           INITIALIZE          SPA-SCR-REC
           INITIALIZE          WRK-DATA-REC
           MOVE    ZERO                TO  FLG-SPASCR
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       FLG-SPASCR  =   1
               READ    SPASCR-FILE
                   AT  END
                       MOVE   1            TO  FLG-SPASCR
                   NOT AT  END
                       ADD    1            TO  IDX
                       MOVE   SPA-DATA-REC     TO  WRK-DATA (IDX)
               END-READ
           END-PERFORM
      *
           MOVE    WRK-DATA-REC        TO  SPA-SCR-REC 
      *
           CLOSE                       SPASCR-FILE
           .
       1002-C02SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-C02SPA-OUT-SEC             SECTION.
      *
           MOVE    SPA-TERMID          TO  FILE-TERMID
      *
           OPEN    OUTPUT              SPASCR-FILE
           IF      STS-SPASCR     NOT =   ZERO
               DISPLAY "C02SPASCR OUT OPEN ERR:"   STS-SPASCR
      ***      STOP    " "
               GO  TO  1003-C02SPA-SET-EXT
           END-IF
           MOVE    SPACE              TO         WRK-DATA-REC
      *
           MOVE    SPA-SCR-REC        TO  WRK-DATA-REC 
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   10  )  OR
                              (WRK-DATA (IDX)     =   SPACE)
      *
               MOVE    WRK-DATA (IDX)      TO  SPA-DATA-REC
               WRITE                       SPA-DATA-REC
               IF      STS-SPASCR     NOT =   ZERO
                   DISPLAY "C02SPASCR OUT WRITE ERR:"   STS-SPASCR
      ***          STOP    " "
                   GO  TO  1003-C02SPA-SET-EXT
                END-IF
           END-PERFORM
      *
           CLOSE                       SPASCR-FILE
           .
       1003-C02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "C10    "           TO  MCP-WINDOW.
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
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    検索画面スパ編集処理     
           INITIALIZE              SPA-C10
      *
           MOVE    "-"             TO  SPA-GMN-C10-TENKIKBNL        (1)
           MOVE    "取消"          TO  SPA-GMN-C10-TENKIKBNMEIL     (1)
           MOVE    "1"             TO  SPA-GMN-C10-TENKIKBNL        (2)
           MOVE    "治ゆ"          TO  SPA-GMN-C10-TENKIKBNMEIL     (2)
           MOVE    "2"             TO  SPA-GMN-C10-TENKIKBNL        (3)
           MOVE    "死亡"          TO  SPA-GMN-C10-TENKIKBNMEIL     (3)
           MOVE    "3"             TO  SPA-GMN-C10-TENKIKBNL        (4)
           MOVE    "中止"          TO  SPA-GMN-C10-TENKIKBNMEIL     (4)
           MOVE    "8"             TO  SPA-GMN-C10-TENKIKBNL        (5)
           MOVE    "移行"          TO  SPA-GMN-C10-TENKIKBNMEIL     (5)
           MOVE    5               TO  SPA-GMN-C10-TENKIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-UTAGAIFLGL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-UTAGAIFLGL       (2)
           MOVE    "疑い"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (2)
           MOVE    "2"             TO  SPA-GMN-C10-UTAGAIFLGL       (3)
           MOVE    "急性"          TO  SPA-GMN-C10-UTAGAIFLGMEIL    (3)
           MOVE    "3"             TO  SPA-GMN-C10-UTAGAIFLGL       (4)
           MOVE    "急性かつ疑い"  TO  SPA-GMN-C10-UTAGAIFLGMEIL    (4)
           MOVE    4               TO  SPA-GMN-C10-UTAGAIFLG-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-SYUBYOFLGL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-SYUBYOFLGMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-SYUBYOFLGL       (2)
           MOVE    "主病名である"  TO  SPA-GMN-C10-SYUBYOFLGMEIL    (2)
           MOVE    2               TO  SPA-GMN-C10-SYUBYOFLG-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-MANSEIKBNL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-MANSEIKBNMEIL    (1)
           MOVE    "03"            TO  SPA-GMN-C10-MANSEIKBNL       (2)
           MOVE    "皮膚科特定疾患指導管理料(１)"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (2)
           MOVE    "04"            TO  SPA-GMN-C10-MANSEIKBNL       (3)
           MOVE    "皮膚科特定疾患指導管理料(２)"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (3)
           MOVE    "05"            TO  SPA-GMN-C10-MANSEIKBNL       (4)
           MOVE    "特定疾患療養指導料／老人慢性疾患生活指導料"
                                   TO  SPA-GMN-C10-MANSEIKBNMEIL    (4)
           MOVE    4               TO  SPA-GMN-C10-MANSEIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-NYUGAIKBNL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-NYUGAIKBNMEIL    (1)
           MOVE    "1"             TO  SPA-GMN-C10-NYUGAIKBNL       (2)
           MOVE    "入院"          TO  SPA-GMN-C10-NYUGAIKBNMEIL    (2)
           MOVE    "2"             TO  SPA-GMN-C10-NYUGAIKBNL       (3)
           MOVE    "入院外"        TO  SPA-GMN-C10-NYUGAIKBNMEIL    (3)
           MOVE    3               TO  SPA-GMN-C10-NYUGAIKBN-MAX
      *
           MOVE    "-"             TO  SPA-GMN-C10-HKNCOMBIL       (1)
           MOVE    "取消"          TO  SPA-GMN-C10-HKNCOMBIMEIL    (1)
           MOVE    1               TO  SPA-GMN-C10-HKNCOMBI-MAX
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C02-HKNCOMBI-MAX
               ADD     1               TO  SPA-GMN-C10-HKNCOMBI-MAX  
               MOVE   SPA-GMN-C02-HKNCOMBILST (IDX)
                                       TO
                                       SPA-GMN-C10-HKNCOMBILST
                                           (SPA-GMN-C10-HKNCOMBI-MAX)
           END-PERFORM    
           MOVE    1               TO  SPA-GMN-C10-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C10-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-SYORI-EXT.
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    C10-TENKIKBN         TO  SPA-GMN-C10-TENKIKBN-G
           MOVE    C10-TENKIYMD         TO  SPA-GMN-C10-TENKIYMD
           MOVE    C10-UTAGAIFLG        TO  SPA-GMN-C10-UTAGAIFLG-G
           MOVE    C10-SYUBYOFLG        TO  SPA-GMN-C10-SYUBYOFLG-G
           MOVE    C10-MANSEIFLG        TO  SPA-GMN-C10-MANSEIKBN-G
           MOVE    C10-NYUGAIKBN        TO  SPA-GMN-C10-NYUGAIKBN-G
           MOVE    C10-HKNCOMBI         TO  SPA-GMN-C10-HKNCOMBI-G
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
           PERFORM 4106-TENKIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4107-TENKIYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4101-UTAGAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4102-SYUBYOFLG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4103-MANSEI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4104-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4105-HKNCOMBI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑い　入力処理
      *****************************************************************
       4101-UTAGAI-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-UTAGAIFLG   TO  WRK-KBN
           PERFORM 5001-UTAGAIFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C10-UTAGAIFLGMEI
           IF      SPA-GMN-C10-UTAGAIFLG =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-UTAGAIFLGMEI   =   SPACE    
                  MOVE    "0018"               TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4101-UTAGAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病入力処理
      *****************************************************************
       4102-SYUBYOFLG-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-SYUBYOFLG
                                        TO  WRK-KBN
           PERFORM 5001-SYUBYOFLG-HENSYU-SEC
           MOVE    WRK-KBNMEI              TO  SPA-GMN-C10-SYUBYOFLGMEI
           IF      SPA-GMN-C10-SYUBYOFLG  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-SYUBYOFLGMEI     =   SPACE
                   MOVE    "0019"               TO  SPA-ERRCD
               END-IF
           END-IF        
           .
       4102-SYUBYOFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性　入力処理
      *****************************************************************
       4103-MANSEI-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-MANSEIKBN
                                       TO  WRK-KBN
           PERFORM 5001-MANSEIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-MANSEIKBNMEI
      *                                 
           IF      SPA-GMN-C10-MANSEIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-MANSEIKBNMEI   =   SPACE    
                  MOVE    "0021"               TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4103-MANSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       4104-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C10-NYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-NYUGAIKBNMEI
      *                                 
           IF      SPA-GMN-C10-NYUGAIKBN =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C10-NYUGAIKBNMEI   =   SPACE    
                   MOVE    "0022"               TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4104-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ入力処理
      *****************************************************************
       4105-HKNCOMBI-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-C10-HKNCOMBI
                                       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-C10-HKNCOMBIMEI
           ELSE
               MOVE    SPA-GMN-C10-HKNCOMBI
                                           TO  WRK-KBN
               PERFORM 5001-HKNCOMBI-HENSYU-SEC
               IF      WRK-KBNMEI      =   SPACE
                   MOVE    "0009"               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4105-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分  入力処理
      *****************************************************************
       4106-TENKIKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-C10-TENKIKBN
                                       TO  WRK-KBN
           PERFORM 5001-TENKIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C10-TENKIKBNMEI
      *                                 
           IF      SPA-GMN-C10-TENKIKBN
                                       =   SPACE   OR  "-"
               MOVE    SPACE               TO  SPA-NAI-C10-TENKIYMD
               MOVE    SPACE               TO  SPA-GMN-C10-TENKIYMD
           ELSE
               IF      SPA-GMN-C10-TENKIKBNMEI
                                           =   SPACE
                   MOVE    "0024"              TO  SPA-ERRCD
                   GO  TO  4106-TENKIKBN-CHK-EXT
               END-IF
           END-IF
      *     
           IF    ( SPA-GMN-C10-TENKIKBN NOT =   SPACE   AND "-" )
           AND   ( SPA-GMN-C10-TENKIYMD     =   SPACE           )
               MOVE    SPA-NAI-C02-SYSYMD   TO  SPA-NAI-C10-TENKIYMD
               MOVE    SPA-GMN-C02-SYSYMD   TO  SPA-GMN-C10-TENKIYMD
           END-IF    
           .
       4106-TENKIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰年月日  入力処理
      *****************************************************************
       4107-TENKIYMD-CHK-SEC            SECTION.
      *     
           MOVE    SPACE              TO  SPA-NAI-C10-TENKIYMD
           IF      SPA-GMN-C10-TENKIYMD
                                      =   SPACE
               IF      SPA-GMN-C10-TENKIKBN   NOT =   SPACE   AND "-"
                   MOVE    "0036"          TO  SPA-ERRCD
               END-IF    
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-C10-TENKIYMD    TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      SNUM-RC             >   ZERO
                   MOVE    SPA-GMN-C10-TENKIYMD    TO  SGDAY-INDAY
               ELSE
                   EVALUATE    SNUM-SEISUU
                       WHEN    1
                       WHEN    2
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-DD
                       WHEN    3
                           MOVE    SPA-NAI-C02-SYSYMD  TO  WRK-YMD-9
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                       WHEN    4
                           MOVE    SNUM-OUTNUM         TO  WRK-MD
                           IF    ( WRK-MM              =   12   )
                           AND   ( SPA-NAI-C02-SYSYMD (5:2)
                                                       =   "01" )
                               MOVE    SPA-NAI-C02-SYSYMD-BACK (1:4)
                                                       TO  WRK-YY
                           ELSE
                               MOVE    SPA-NAI-C02-SYSYMD (1:4)
                                                       TO  WRK-YY
                           END-IF
                       WHEN    OTHER
                           MOVE    SPA-GMN-C10-TENKIYMD
                                                       TO  WRK-YMD-9
                   END-EVALUATE
                   MOVE    WRK-YMD-9       TO  SGDAY-INDAY
               END-IF
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-C10-TENKIYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-C10-TENKIYMD
                   IF      SPA-GMN-C10-TENKIKBN    =   SPACE
                       MOVE    "0037"          TO  SPA-ERRCD
                   END-IF    
               ELSE
                   MOVE    "0016"          TO  SPA-ERRCD
               END-IF
           END-IF    
           .
       4107-TENKIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-C10-TENKIKBN    =   SPACE )
           AND   ( SPA-NAI-C10-TENKIYMD    =   SPACE )
           AND   ( SPA-GMN-C10-UTAGAIFLG   =   SPACE )
           AND   ( SPA-GMN-C10-SYUBYOFLG   =   SPACE )
           AND   ( SPA-GMN-C10-MANSEIKBN   =   SPACE )
           AND   ( SPA-GMN-C10-NYUGAIKBN   =   SPACE )
           AND   ( SPA-GMN-C10-HKNCOMBI    =   SPACE )
               MOVE    "0001"                  TO  SPA-ERRCD
               GO  TO  4101-KAKUTEI-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               MOVE    SPA-C01-STNUM-O (IDX)
                                       TO  WRK-STNUM-O (IDX)
               MOVE    SPA-C01-EDNUM-O (IDX)
                                       TO  WRK-EDNUM-O (IDX)
           END-PERFORM
      *
           MOVE    SPACE                   TO  WRK-HENSYU-DATA-G
           MOVE    SPA-GMN-C10-TENKIKBN    TO  WRK-HENSYU-DATA-O (1)
           MOVE    SPA-NAI-C10-TENKIYMD    TO  WRK-HENSYU-DATA-O (2)
           MOVE    SPA-GMN-C10-UTAGAIFLG   TO  WRK-HENSYU-DATA-O (3)
           MOVE    SPA-GMN-C10-SYUBYOFLG   TO  WRK-HENSYU-DATA-O (4)
           MOVE    SPA-GMN-C10-MANSEIKBN   TO  WRK-HENSYU-DATA-O (5)
           MOVE    SPA-GMN-C10-NYUGAIKBN   TO  WRK-HENSYU-DATA-O (6)
           MOVE    SPA-GMN-C10-HKNCOMBI    TO  WRK-HENSYU-DATA-O (7)
      *
      *    転帰・保険適用と開始日のチェック
           IF    ( WRK-HENSYU-DATA-O (1)   NOT =   SPACE         )
           OR    ( WRK-HENSYU-DATA-O (6)   NOT =   SPACE         )
           OR    ( WRK-HENSYU-DATA-O (7)   NOT =   SPACE AND "-" )
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       5
                   IF      WRK-STNUM-O (IDXX)  >   ZERO
                       PERFORM VARYING IDX     FROM
                                           WRK-STNUM-O (IDXX)  BY  1
                               UNTIL   IDX     >   WRK-EDNUM-O (IDXX)
                           PERFORM 41011-KANTAN-SYUSEI-DABURI-CHK-SEC
                           IF      SPA-ERRCD   NOT =   SPACE
                               GO  TO  4101-KAKUTEI-CHK-EXT
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    簡単修正転帰・保険適用チェック処理
      *****************************************************************
       41011-KANTAN-SYUSEI-DABURI-CHK-SEC         SECTION.
      *
      *    転記日のセット
           MOVE    SPA-NAI-C02-TTENKIYMD (IDX)
                                   TO  WRK-TENKIYMD
           IF      WRK-HENSYU-DATA-O (1)
                                   =   SPACE
               CONTINUE
           ELSE
               IF      WRK-HENSYU-DATA-O (1)
                                       =   "-"
                   MOVE    SPACE                   TO  WRK-TENKIYMD
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (2)   TO  WRK-TENKIYMD
               END-IF
           END-IF
      * 
      *    転帰と開始日のチェック
           IF      WRK-HENSYU-DATA-O (1)
                               NOT =   "-" AND SPACE  
               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                       >    WRK-HENSYU-DATA-O (2)
                   MOVE    "0084"      TO  SPA-ERRCD
                   GO  TO  41011-KANTAN-SYUSEI-DABURI-CHK-EXT
               END-IF
           END-IF
      *    保険適用と開始日のチェック
           IF     WRK-HENSYU-DATA-O (7)  
                               NOT =   "-" AND SPACE  
               IF    ( SPA-NAI-C02-TSRYYMD (IDX)
                                   <=   WRK-TEKEDYMD )
                   CONTINUE
               ELSE    
                   MOVE    "0085"     TO  SPA-ERRCD
                   MOVE    SPA-NAI-C02-TSRYYMD (IDX)
                                               TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
                   GO  TO  41011-KANTAN-SYUSEI-DABURI-CHK-EXT
               END-IF
           END-IF
      *
      *    マスタとのチェック
           INITIALIZE                  PTBYO-KEY
           MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-NAI-C02-TSRYKA  (IDX)
                                       TO  PTBYO-SRYKA
           MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                       TO  PTBYO-BYOMEI
           MOVE    SPA-NAI-C02-TNYUGAIKBN (IDX)
                                       TO  PTBYO-NYUGAIKBN
           IF      WRK-HENSYU-DATA-O (6)   =   SPACE
               CONTINUE
           ELSE 
               IF      WRK-HENSYU-DATA-O (6)   =   "-"
                   MOVE    SPACE               TO  PTBYO-NYUGAIKBN
               ELSE
                   MOVE    WRK-HENSYU-DATA-O (6)
                                               TO  PTBYO-NYUGAIKBN
               END-IF
           END-IF     
           IF      PTBYO-NYUGAIKBN     =   SPACE 
               MOVE    "PTBYOMEI-KEY13"    TO  WRK-DBPATH
           ELSE
               MOVE    "PTBYOMEI-KEY21"    TO  WRK-DBPATH
           END-IF    
           PERFORM 900-PTBYO-INV-SEC
      *
           PERFORM                 UNTIL   FLG-PTBYOMEI    =   1
                                   OR      SPA-ERRCD   NOT =   SPACE
               IF      SPA-NAI-C02-TSRYYMD (IDX)   =   PTBYO-SRYYMD
               AND     SPA-NAI-C02-TSRYKA  (IDX)   =   PTBYO-SRYKA
               AND     SPA-NAI-C02-TRENNUM (IDX)   =   PTBYO-RENNUM
                   CONTINUE
               ELSE
                   EVALUATE    TRUE
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           <   PTBYO-SRYYMD
                           IF      WRK-TENKIYMD    =   SPACE
                               MOVE    "0038"          TO  SPA-ERRCD
                           ELSE
                               IF     WRK-TENKIYMD >   PTBYO-SRYYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                       WHEN    SPA-NAI-C02-TSRYYMD (IDX)
                                           >   PTBYO-SRYYMD
                           IF      PTBYO-TENKIYMD  =   SPACE
                               MOVE    "0038"      TO  SPA-ERRCD
                           ELSE
                               IF      SPA-NAI-C02-TSRYYMD (IDX)
                                               <   PTBYO-TENKIYMD
                                   MOVE    "0038"      TO  SPA-ERRCD
                               END-IF
                           END-IF
                   END-EVALUATE
               END-IF
      *
               IF      SPA-ERRCD           =   "0038"
                   MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-ERR-SRYYMD
                   MOVE    SPA-GMN-C02-TBYOMEI (IDX)
                                               TO  WRK-ERR-BYOMEI
               END-IF
      *
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       41011-KANTAN-SYUSEI-DABURI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C10"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       450-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  450-KAKUTEI-EXT
           END-IF
      *
      *    修正項目をパラメタに設定     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       7
               MOVE    WRK-HENSYU-DATA-O (IDX)
                                       TO  SPA-C01-HENSYU-DATA-O (IDX)
           END-PERFORM
           MOVE    "OK"            TO  SPA-CIDCD
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    疑いフラグ編集処理
      *****************************************************************
       5001-UTAGAIFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-UTAGAIFLG-MAX
                   IF      WRK-KBN (1:1) 
                                  =   SPA-GMN-C10-UTAGAIFLGL (IDY)
                       MOVE    SPA-GMN-C10-UTAGAIFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-UTAGAIFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-UTAGAIFLG-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性フラグ編集処理
      *****************************************************************
       5001-MANSEIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE 
               IF      WRK-KBN  =   "-"
                   MOVE    SPA-GMN-C10-MANSEIKBNMEIL (1)
                                          TO  WRK-KBNMEI
               ELSE
                   PERFORM VARYING IDY FROM   2   BY  1
                           UNTIL   IDY >      SPA-GMN-C10-MANSEIKBN-MAX
                       IF      WRK-KBN (1:2)
                                      =   SPA-GMN-C10-MANSEIKBNL (IDY)
                           MOVE    SPA-GMN-C10-MANSEIKBNMEIL (IDY)
                                              TO  WRK-KBNMEI
                           MOVE    SPA-GMN-C10-MANSEIKBN-MAX
                                              TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       5001-MANSEIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    転帰区分編集処理
      *****************************************************************
       5001-TENKIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-TENKIKBN-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C10-TENKIKBNL (IDY)
                       MOVE    SPA-GMN-C10-TENKIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-TENKIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主病フラグ編集処理
      *****************************************************************
       5001-SYUBYOFLG-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-SYUBYOFLG-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C10-SYUBYOFLGL (IDY)
                       MOVE    SPA-GMN-C10-SYUBYOFLGMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-SYUBYOFLG-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C10-NYUGAIKBN-MAX
                       OR      SPA-GMN-C10-NYUGAIKBNL (IDY) =   SPACE
                   IF      WRK-KBN    =   SPA-GMN-C10-NYUGAIKBNL (IDY)
                       MOVE    SPA-GMN-C10-NYUGAIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C10-NYUGAIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-NYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       5001-HKNCOMBI-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
           MOVE    ZERO               TO  WRK-TEKEDYMD
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE 
               IF      WRK-KBN  =   "-"
                   MOVE    SPA-GMN-C10-HKNCOMBIMEIL (1)
                                          TO  WRK-KBNMEI
               ELSE
                   PERFORM VARYING IDY FROM   1   BY  1
                           UNTIL   IDY >      SPA-GMN-C02-HKNCOMBI-MAX
                       IF      WRK-KBN        =
                                          SPA-GMN-C02-HKNCOMBIL (IDY)
                           MOVE    SPA-GMN-C02-HKNCOMBIMEIL (IDY)
                                          TO  WRK-KBNMEI
                           MOVE    SPA-NAI-C02-TEKSTYMD     (IDY)
                                          TO  WRK-TEKSTYMD
                           MOVE    SPA-NAI-C02-TEKEDYMD     (IDY)
                                          TO  WRK-TEKEDYMD
                           MOVE    SPA-GMN-C02-HKNCOMBI-MAX
                                          TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       5001-HKNCOMBI-HENSYU-EXT.
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
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C10    "           TO  MCP-WINDOW
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
           MOVE    SPACE               TO  C10
           MOVE    SPA-GMN-C10-TENKIYMD       TO  C10-TENKIYMD
      *
           MOVE    SPA-GMN-C10-TENKIKBN-G TO  C10-TENKIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-TENKIKBN-MAX
               MOVE    SPA-GMN-C10-TENKIKBNLST (IDX)
                                           TO  C10-TENKIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-TENKIKBN-MAX
                                       TO  C10-TENKIKBN-COUNT
      *
           MOVE    SPA-GMN-C10-UTAGAIFLG-G TO  C10-UTAGAIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-UTAGAIFLG-MAX
               MOVE    SPA-GMN-C10-UTAGAIFLGLST (IDX)
                                           TO  C10-UTAGAIFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-UTAGAIFLG-MAX
                                       TO  C10-UTAGAIFLG-COUNT    
      *
           MOVE    SPA-GMN-C10-SYUBYOFLG-G TO  C10-SYUBYOFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-SYUBYOFLG-MAX
               MOVE    SPA-GMN-C10-SYUBYOFLGLST (IDX)
                                           TO  C10-SYUBYOFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-SYUBYOFLG-MAX
                                       TO  C10-SYUBYOFLG-COUNT  
      *
           MOVE    SPA-GMN-C10-MANSEIKBN-G TO  C10-MANSEIFLG
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-MANSEIKBN-MAX
               MOVE    SPA-GMN-C10-MANSEIKBNLST (IDX)
                                           TO  C10-MANSEIFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-MANSEIKBN-MAX
                                       TO  C10-MANSEIFLG-COUNT
      *
           MOVE    SPA-GMN-C10-NYUGAIKBN-G TO  C10-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-NYUGAIKBN-MAX
               MOVE    SPA-GMN-C10-NYUGAIKBNLST (IDX)
                                           TO  C10-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-NYUGAIKBN-MAX 
                                       TO  C10-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-C10-HKNCOMBI-G TO  C10-HKNCOMBI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL       IDX         >   SPA-GMN-C10-HKNCOMBI-MAX
               MOVE    SPA-GMN-C10-HKNCOMBILST (IDX)
                                           TO  C10-HKNCOMBI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C10-HKNCOMBI-MAX
                                       TO  C10-HKNCOMBI-COUNT
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
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-C10-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-C10-CUR
             WHEN    1
                     MOVE  "TENKIKBN"   TO  MCP-WIDGET
             WHEN    2
                     MOVE  "TENKIYMD"   TO  MCP-WIDGET
             WHEN    3
                     MOVE  "UTAGAIFLG"  TO  MCP-WIDGET
             WHEN    4
                     MOVE  "SYUBYOFLG"  TO  MCP-WIDGET
             WHEN    5
                     MOVE  "MANSEIFLG"  TO  MCP-WIDGET
             WHEN    6
                     MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    7
                     MOVE  "HKNCOMBI"   TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "TENKIKBN"
                   IF      SPA-GMN-C10-TENKIKBN    =   SPACE   OR  "-"
                       MOVE    3           TO  SPA-GMN-C10-CUR
                   ELSE
                       MOVE    2           TO  SPA-GMN-C10-CUR
                   END-IF 
               WHEN    "TENKIYMD"
                   MOVE    3                   TO  SPA-GMN-C10-CUR
               WHEN    "UTAGAIFLG"
                   MOVE    4                   TO  SPA-GMN-C10-CUR
               WHEN    "SYUBYOFLG"
                   MOVE    5                   TO  SPA-GMN-C10-CUR
               WHEN    "MANSEIFLG"
                   MOVE    6                   TO  SPA-GMN-C10-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    7                   TO  SPA-GMN-C10-CUR
               WHEN    "HKNCOMBI"
                   MOVE    1                   TO  SPA-GMN-C10-CUR
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C10-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
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
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。"  DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0009"
                   MOVE    "該当の保険組合せがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
               MOVE "開始日が該当の保険組合せの適用日の範囲外です"
                                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "年号入力エラー"            TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "数字で入力してください"    TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "転帰日付入力エラー"        TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "日付入力エラー"            TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "疑い入力エラー"            TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "主病名入力エラー"          TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "慢性区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "転帰区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "転帰年月日が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "転帰年月日が入力してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "開始日付以前の転帰日付が入力してあります"
                                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "転帰区分が入力してあります"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "転帰区分が入力してありません"
                                               TO  SPA-ERRMSG
               WHEN    "0038"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は"                    DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "に登録してあります。転帰日等を確認して下さい"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0084"
                 MOVE
                     "入力した転帰日以降の診療開始日の病名が指定"
                                                   TO  SPA-ERRMSG
                 MOVE
                     "してあります"                TO  SPA-ERRMSG (43:)
               WHEN    "0085"
                   STRING
                       WRK-ERR-BYOMEI          DELIMITED   BY  SPACE
                       "は開始日が"            DELIMITED   BY  SIZE 
                       WRK-ERR-SRYYMD          DELIMITED   BY  SPACE
                       "ですので該当の保険適用は範囲外です"
                                               DELIMITED   BY  SIZE 
                                               INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  CERR
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C10"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
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
      *    患者病名マスター読込（主キー）
      *****************************************************************
       900-PTBYO-INV-SEC         SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-DBPATH          TO  ORC-DBPATH
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-DBPATH          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PTBYO-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読込
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
