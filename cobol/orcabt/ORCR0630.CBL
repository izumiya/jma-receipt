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
       PROGRAM-ID.             ORCR0630.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院）
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/12/11    NaCl−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.03.07  未実施減算の略称追加
      *  01.00.02    NACL-門脇    03.03.10  食事区分”４”の対応追加
      *  01.00.03    NACL-門脇    03.03.12  選定入院料対応
      *  01.00.04    NACL-門脇    03.03.20  入院料日数計算修正
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE62-FILE     ASSIGN  RECE62PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE62-KEY
                                   FILE    STATUS  IS  STS-RECE62.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE62-FILE.
       01  RECE62-REC. 
           COPY    "CPRCF062.INC".
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
                                   BY         //RECE62//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE62                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-NYUINACT                            PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-RECE62                              PIC 9(01).
           03  FLG-NYUKHN                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-NYURYAKUSHO                         PIC 9(01).
           03  FLG-GAIHAKU                             PIC 9(01).
           03  FLG-MIJISI-GENSAN                       PIC 9(01).       
           03  FLG-MIJISI-INNAI                        PIC 9(01).       
           03  FLG-MIJISI-IRYO                         PIC 9(01).       
           03  FLG-MIJISI-JYOKU                        PIC 9(01).       
           03  FLG-SENTEICDCHG                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TBL-OCC                             OCCURS  2.
               05  CNT-IN                              PIC 9(06).
               05  CNT-MOD                             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                                     PIC 9(03).
           03  IDY                                     PIC 9(03).
           03  IDZ                                     PIC 9(03).
	   03  IDX1                                    PIC 9(03).
           03  IDX2                                    PIC 9(03).
           03  HC-IDX                                  PIC 9(03).
           03  KH-IDX                                  PIC 9(03).
           03  NYUINACT-IDX                            PIC 9(03).
           03  NYUINACT-IDX2                           PIC 9(03).
           03  ACCT-IDX                                PIC 9(03).
           03  SRYACT-IDX                              PIC 9(03).
           03  ID1                                     PIC 9(03).
           03  GENSAN-IDX                              PIC 9(03).
           03  TENTEK-IDX                              PIC 9(03).
      *
      *    実日数カウント用領域
       01  TBL-JNISSU.
           03  TBL-JNISSU-HKN       OCCURS   5.
               05  TBL-JNISSU-DAY                      PIC X(01)
                                                       OCCURS   31.
      *    合計点数（診療行為）集計用領域
       01  TBL-TOTALTEN.
           03  TBL-TOTALTEN-HKN     OCCURS   5.
               05  TBL-TOTALTEN-ACCT                   PIC S9(08).
      *
       01  WRK-DAY-TABLE.
           03  WRK-DAY-T                               OCCURS   31.
               05  WRK-DAY1                            PIC 9(04).
               05  WRK-DAY1-NYUTENTTLKBN               PIC 9(03).
               05  WRK-DAY1-SENTEI                     PIC 9(04).
               05  WRK-DAY2                            PIC 9(04).
               05  WRK-DAY4                            PIC 9(04).
               05  WRK-DAY41                           PIC 9(04).
               05  WRK-DAY5                            PIC 9(04).
               05  WRK-DAY6                            PIC 9(04).
               05  WRK-DAY6-ZAITEN                     PIC 9(08).
               05  WRK-ACCT90                          PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IN                                  PIC X(01).
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC X(06).
               05  WRK-PARA-TERMID                     PIC X(16).
               05  WRK-PARA-SYSYMD                     PIC X(08).
               05  WRK-PARA-JOBID                      PIC 9(07).
               05  WRK-PARA-SHELLID                    PIC X(08).
       01  WRK-TENTEK.
           03  WRK-TENTEK-SYUTEN                       PIC 9(07).
           03  WRK-TENTEK-ZAIKAISU                     PIC 9(04).
           03  WRK-TENTEK-TABLE.
               05  WRK-TENTEK-TBL                      OCCURS  10.
                   07  WRK-TENTEK-YKZTEN               PIC 9(07).
                   07  WRK-TENTEK-KIZAITEN             PIC 9(07).
           03  WRK-TENTEK-FLG-TABLE.
               05  WRK-TENTEK-FLG-TBL                  OCCURS  10.
                   07  WRK-TENTEK-FLG                  PIC 9(01)
                                                           OCCURS 5.
      *
       01  WRK-CONS-AREA.
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(64)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
           03  WRK-RECEERR                             PIC X(200). 
      *
      *    入院点数算定用ワークエリア
       01  WRK-NYUIN-TENSU-AREA.
           03  WRK-KHNTEN                              PIC 9(08). 
           03  WRK-SENTEI-KHNTEN                       PIC 9(08).
           03  WRK-SENTEI-ZAITEN                       PIC 9(08).
           03  WRK-KSNTEN                              PIC S9(08).
           03  WRK-NYUTENTTLKBN                        PIC 9(03).
           03  WRK-NYUKHNRYO                           PIC 9(09).
           03  WRK-NYUTOKRYO                           PIC 9(09).
           03  WRK-MIJISI                              PIC 9(09).
           03  WRK-SHOKUJIRYOYO                        PIC 9(01).
           03  WRK-TOKUKANRIKASAN                      PIC 9(01).
           03  WRK-SRYCD                               PIC 9(09).
           03  WRK-RYAKUSHO                            PIC X(14).
           03  WRK-SRYYMD                              PIC X(08).       
           03  WRK-SRYYMD-TAIHI                        PIC X(08).   
           03  WRK-SENTEI-KEISU                        PIC 9V99.    
           03  WRK-GAIHAKU-SRYCD                       PIC X(09).
           03  WRK-GAIHAKU-KHNTEN                      PIC 9(08).
      *
           COPY    "MCPAREA".
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
           COPY    "CPSK5000.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
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
      *    入院略称
       01  NYURYAKUSHO-REC.
           COPY    "CPNYUINRYAKUSHO.INC".
      *
      *    選定入院料
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
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
      *    入院点滴一括更新サブ
           COPY    "CPORCSTENTEK.INC".
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
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCR0630"      TO  JOB-PGID
           MOVE    "診療実日数編集"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
      *    医療機関減算の取得（システム管理テーブルより）
           MOVE     ZERO               TO  WRK-SHOKUJIRYOYO
                                           WRK-TOKUKANRIKASAN
      *
           MOVE     ZERO               TO  FLG-MIJISI-INNAI             
                                           FLG-MIJISI-IRYO              
                                           FLG-MIJISI-JYOKU             
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5000"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  ORC-DBYMD (1:6)
           MOVE    "01"                TO  ORC-DBYMD (7:2)
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               PERFORM  SYSKANRI-READ-SEC
               PERFORM  VARYING   GENSAN-IDX  FROM  1  BY  1
                        UNTIL     GENSAN-IDX   >   4
                    IF    SYS-5000-GENKBN(GENSAN-IDX)   =   "2"    
                        EVALUATE  GENSAN-IDX
                            WHEN  2
                                MOVE    1     TO    FLG-MIJISI-INNAI    
                            WHEN  3
                                MOVE    1     TO    FLG-MIJISI-IRYO     
                            WHEN  4
                                MOVE    1     TO    FLG-MIJISI-JYOKU    
                        END-EVALUATE
                    END-IF
               END-PERFORM
      *
               IF  SYS-5000-SHOKUJIRYOYOCD   =  "197000110"
                      MOVE     1         TO   WRK-SHOKUJIRYOYO
                      IF  SYS-5000-SHOKUKBN(1) =  "1"
                           MOVE  ZERO    TO   WRK-TOKUKANRIKASAN
                      ELSE
                           MOVE  1       TO   WRK-TOKUKANRIKASAN
                      END-IF
               ELSE
                      MOVE     2         TO   WRK-SHOKUJIRYOYO
               END-IF
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
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
                   MOVE    "RECE620"           TO  RECE62PARA-FILE-ID
                   MOVE    WRK-PARA-TERMID     TO  RECE62PARA-TERMID
               ELSE
      *    請求書処理
                   MOVE    "RECE62X"           TO  RECE62PARA-FILE-ID
                   MOVE    WRK-CONS-TERMID     TO  RECE62PARA-TERMID
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
           MOVE    ZERO                TO  FLG-RECE62
      *
           OPEN    I-O                 RECE62-FILE
      *
           PERFORM    RECE62-READ-SEC
      *
           PERFORM                     UNTIL   FLG-RECE62  =   1
               PERFORM     210-HENSYU-SEC
      *
               PERFORM     RECE62-READ-SEC
           END-PERFORM
      *
           CLOSE   RECE62-FILE
           .
       200-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           MOVE       SPACE             TO     TBL-JNISSU
           INITIALIZE                          TBL-JNISSU
      *
           MOVE       SPACE             TO     TBL-TOTALTEN
           INITIALIZE                          TBL-TOTALTEN
      *
           MOVE       SPACE             TO     WRK-DAY-TABLE
           INITIALIZE                          WRK-DAY-TABLE
      *
           MOVE       ZERO              TO     FLG-MIJISI-GENSAN        
      *
      ***  PERFORM    VARYING    IDX    FROM   1   BY   1
      ***             UNTIL     (IDX      >   10)  OR
      ***                       (RECE62-SRYKA(IDX)   =   SPACE)
               PERFORM    RECE62-HENSYU-SEC
               PERFORM    RECE62-HENSYU2-SEC
               PERFORM    RECE62-HENSYU3-SEC
               PERFORM    RECE62-HENSYU4-SEC
               PERFORM    RECE62-HENSYU9-SEC
      ***  END-PERFORM
      *
           PERFORM     JNISSU-SET-SEC
      *
           PERFORM     RECE62-REWRITE-SEC
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE62-HENSYU-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "1"  OR  "2"  OR  
                                                "4"  OR  "5"  OR
                                                "6"   
                       PERFORM    NYUIN-DAY-TAIHI-SEC
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL   IDX1    >       10
                          OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                 IF        WRK-DAY5(IDY)   =   RECE62-HKNCOMBI (IDX1)
                      MOVE    ZERO            TO    FLG-SHOSAISIN
                      IF   WRK-DAY1(IDY)   NOT =    ZERO    
                               MOVE    1      TO    FLG-SHOSAISIN
                      END-IF
                      PERFORM   RECE62-TENSOU11-SEC
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY7"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY7"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                IF   ACCT-SRYKBN  =  "11"  OR
                                                     "13"  OR  "14"
                                    PERFORM   SHOSAISIN-HANTEI-SEC
                                END-IF
                                PERFORM   RECE62-TENSOU12-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    =        ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                MOVE    ZERO     TO    FLG-SHOSAISIN
                                IF   ACCT-SRYKBN  =  "11"  OR  "99" 
                                    PERFORM   SHOSAISIN-HANTEI-SEC
                                END-IF
                                PERFORM   RECE62-TENSOU12-SEC
                           END-IF
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"   
                                    PERFORM   NYUINRYO-TAIHI-SEC
                                END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE     "SRYACCT-KEY7"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       RECE62-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    会計フラグ退避処理                                          *
      ******************************************************************
       NYUIN-DAY-TAIHI-SEC         SECTION.
      *
           PERFORM    VARYING   IDY   FROM   1   BY   1
                                    UNTIL    IDY    >    31
      *
             IF       NACCT-DAY(IDY)   NOT =  ZERO 
      *
               IF     NACCT-ZAISKBKBN    =    "1"  
                  IF     NACCT-SRYCDTOTAL    =    099999916
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1-SENTEI(IDY)
                  ELSE
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY1(IDY) 
                    IF  NACCT-SRYKBN  =  "90"                           
                       MOVE   903  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                    ELSE                                                
                       MOVE   920  TO  WRK-DAY1-NYUTENTTLKBN(IDY)       
                    END-IF
                  END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "2"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY2(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "4"  
                 IF    NACCT-SRYCDTOTAL  =    197000570 
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY41(IDY) 
                 ELSE 
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY4(IDY) 
                 END-IF
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "5"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY5(IDY) 
               END-IF 
      *
               IF     NACCT-ZAISKBKBN    =    "6"  
                    MOVE   NACCT-DAY(IDY)   TO  WRK-DAY6(IDY) 
                    IF  NACCT-SRYCDTOTAL  =  190080470   OR             
                                             190799670   OR             
                                             190080570   OR             
                                             190799770   OR             
                                             190111690   OR             
                                             190809090   OR             
                                             190111790   OR             
                                             190809190                  
                      CONTINUE                                          
                    ELSE                                                
                      ADD    NACCT-ZAITEN   TO  WRK-DAY6-ZAITEN(IDY)
                    END-IF
               END-IF 
      *
             END-IF
      *
           END-PERFORM.
      *
       NYUIN-DAY-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU11-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF   FLG-SHOSAISIN        =       1
                       MOVE    "1"   TO   TBL-JNISSU-DAY(1 IDY)
               END-IF
           END-IF
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      WRK-DAY5(IDY)  =   RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                         MOVE    "1"   TO   TBL-JNISSU-DAY(KH-IDX IDY)
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       RECE62-TENSOU11-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計テーブルの内容から診療行為テーブルを読み込み、
      *    診療実日数のカウントを行える剤であるかの判定
      *****************************************************************
       SHOSAISIN-HANTEI-SEC                 SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)  OR
                                      (FLG-SHOSAISIN     =    1)
                       MOVE    SPACE         TO   TNS-TENSU-REC
                       INITIALIZE                 TNS-TENSU-REC
                       MOVE    SRY-SRYCD(SRYACT-IDX) TO   TNS-SRYCD
                       MOVE    ACCT-SRYYM         TO  ORC-DBYMD(1:6)
                       MOVE    "01"               TO  ORC-DBYMD(7:2)
                       PERFORM  TENSU-READ-SEC
                       IF       FLG-TENSU      =    ZERO
                         IF ( (TNS-JITUDAY    =     2)    AND
                              (TNS-DAYCNT     =     1   OR   2) ) OR
                            (  SRY-SRYCD(SRYACT-IDX) =  "099409901" OR
                                                        "099999902" OR
                                                        "099110001" OR
                                                        "099120001")
                             MOVE    1      TO    FLG-SHOSAISIN
                         END-IF
                       END-IF
                       MOVE    "DBCLOSE"          TO  MCP-FUNC
                       MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"        USING
                                                  MCPAREA
                                                  ORCMCPAREA
                                                  MCPDATA-REC
                   END-PERFORM
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
      *
       SHOSAISHIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU12-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
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
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      ACCT-HKNCOMBI  =   RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF   FLG-SHOSAISIN      =    1
                      PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                 UNTIL    ACCT-IDX   >      31
                             IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                                 MOVE    "1"       TO
                                      TBL-JNISSU-DAY(KH-IDX ACCT-IDX)
                             END-IF
                      END-PERFORM
                   END-IF
                 END-IF
               END-IF  
           END-PERFORM
           .
       RECE62-TENSOU12-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計の入院料等の退避処理
      *****************************************************************
       NYUINRYO-TAIHI-SEC                 SECTION.
      *
           IF        ACCT-SRYCDTOTAL    =    190080470
                                        OR   190799670
                                        OR   190080570
                                        OR   190799770
                                        OR   190111690
                                        OR   190809090
                                        OR   190111790
                                        OR   190809190
                CONTINUE
           ELSE
                PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                                 UNTIL    ACCT-IDX   >      31
                   IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                         ADD   ACCT-ZAITEN  TO  WRK-ACCT90(ACCT-IDX)
                   END-IF
                END-PERFORM
           END-IF
      *
           .
       NYUINRYO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE62-HENSYU2-SEC                SECTION.
      *
           PERFORM      MIJISI-GET-SEC
      *
           PERFORM      SENTEI-KEISU-GET-SEC
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
      *            入院基本料と加減算点数の取得
                   PERFORM   NYUINKHN-GET-SEC
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM      RECE62-TENSOU21-SEC
                              END-IF
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                            UNTIL   IDX1    >       10
                            OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM      RECE62-TENSOU22-SEC
                              END-IF
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY7"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY7"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"  
                                    PERFORM      RECE62-TENSOU23-SEC
                                END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE     "SRYACCT-KEY7"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       RECE62-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    未実施減算額取得
      *****************************************************************
       MIJISI-GET-SEC                 SECTION.
      *
           MOVE     ZERO               TO  WRK-MIJISI
      *
           IF   FLG-MIJISI-INNAI  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190080570"   TO   TNS-SRYCD
               ELSE
                    MOVE    "190799770"   TO   TNS-SRYCD
               END-IF
               MOVE    RECE62-SRYYM       TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  COMPUTE   WRK-MIJISI   =   WRK-MIJISI
                                         +   TNS-TEN
               END-IF
               MOVE    "DBCLOSE"          TO  MCP-FUNC
               MOVE    "TENSU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"        USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
      *
           END-IF
      *
           IF   FLG-MIJISI-IRYO   =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111690"   TO   TNS-SRYCD
               ELSE
                    MOVE    "190809090"   TO   TNS-SRYCD
               END-IF
               MOVE    RECE62-SRYYM       TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  COMPUTE   WRK-MIJISI   =   WRK-MIJISI
                                         +   TNS-TEN
               END-IF
               MOVE    "DBCLOSE"          TO  MCP-FUNC
               MOVE    "TENSU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"        USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
      *
           END-IF
      *
           IF   FLG-MIJISI-JYOKU  =   1
      *
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    "190111790"   TO   TNS-SRYCD
               ELSE
                    MOVE    "190809190"   TO   TNS-SRYCD
               END-IF
               MOVE    RECE62-SRYYM       TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                  COMPUTE   WRK-MIJISI   =   WRK-MIJISI
                                         +   TNS-TEN
               END-IF
               MOVE    "DBCLOSE"          TO  MCP-FUNC
               MOVE    "TENSU-KEY"        TO  ORC-DBPATH
               CALL    "ORCMCPSUB"        USING
                                          MCPAREA
                                          ORCMCPAREA
                                          MCPDATA-REC
      *
           END-IF
      *
           .
       MIJISI-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    選定外泊時の係数取得
      **************************************************************
       SENTEI-KEISU-GET-SEC           SECTION.
      *
           MOVE      ZERO           TO     WRK-SENTEI-KEISU
      *
           IF   RECE62-SRYYM    <=    "200303"
               MOVE   0.1           TO     WRK-SENTEI-KEISU
           END-IF
      *
           IF ( RECE62-SRYYM    >=    "200304" )   AND
              ( RECE62-SRYYM    <=    "200403" )
               MOVE   0.05           TO    WRK-SENTEI-KEISU
           END-IF
      *
           IF   RECE62-SRYYM    >=    "200404"
               MOVE   ZERO          TO     WRK-SENTEI-KEISU
           END-IF
      *
           .
      *
       SENTEI-KEISU-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU21-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      PERFORM      NYUINACCT-GET-SEC
           END-IF
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      WRK-DAY5(IDY)  =   RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO   KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      NYUINACCT-GET-SEC
                 END-IF
               END-IF  
           END-PERFORM
           .
       RECE62-TENSOU21-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU22-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                      MOVE    1          TO        KH-IDX
                      PERFORM      NYUINACCT-GET2-SEC
           END-IF
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      WRK-DAY5(IDY)  =   RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO   KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                      PERFORM      NYUINACCT-GET2-SEC
                 END-IF
               END-IF  
           END-PERFORM
           .
       RECE62-TENSOU22-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の計算
      **************************************************************
       NYUINACCT-GET-SEC           SECTION.
      *
      *    入院料と外泊を比較して点数算定
      *
           EVALUATE   WRK-NYUTENTTLKBN
               WHEN   903
                  MOVE      ZERO   TO  WRK-NYUKHNRYO
                  IF    WRK-DAY1-SENTEI(IDY)   NOT  =  ZERO
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     WRK-SENTEI-ZAITEN  TO  
                                       WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   3
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN  *  WRK-SENTEI-KEISU)
                                                     +  0.5
                       WHEN   4
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN         *  0.15)
                                                     +  0.5
                    END-EVALUATE
                  ELSE
                    EVALUATE   WRK-DAY2(IDY)
                       WHEN   ZERO
                         MOVE     NACCT-ZAITEN   TO  
                                       WRK-NYUKHNRYO
                       WHEN   1
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                       WHEN   2
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                       WHEN   4
                         COMPUTE  WRK-NYUKHNRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                    END-EVALUATE
                  END-IF
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  WRK-MIJISI
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  MOVE     ZERO     TO    FLG-ARI
                  PERFORM  VARYING  ID1    FROM    1   BY  1
                                UNTIL   ID1                   > 10
                                  OR    RECE62-NYUKHNRYO(ID1) = ZERO
                                  OR    FLG-ARI               = 1
                      IF  RECE62-NYUKHNRYO(ID1)  =  WRK-NYUKHNRYO
                            IF   KH-IDX =  1
                            OR   RECE62-HKNNUM  =  SPACE
                              ADD   1   TO   RECE62-NYUKHNNISSU(ID1)
                            END-IF
                            ADD   WRK-NYUKHNRYO
                                      TO   RECE62-NYUKHNKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                  END-PERFORM  
                  IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUKHNNISSU(ID1)
                      MOVE   WRK-NYUKHNRYO   TO  RECE62-NYUKHNRYO(ID1)
                      ADD    WRK-NYUKHNRYO   
                                       TO  RECE62-NYUKHNKEI(ID1 KH-IDX)
                  END-IF
               WHEN   920
                  MOVE      ZERO   TO  WRK-NYUTOKRYO
                  MOVE      ZERO   TO  FLG-GAIHAKU
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         MOVE     NACCT-ZAITEN   TO  
                                       WRK-NYUTOKRYO
                     WHEN   1
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                     WHEN   2
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.3)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                     WHEN   4
                         COMPUTE  WRK-NYUTOKRYO =
                                 (WRK-KHNTEN    *  0.15)
                                                +  0.5
                         MOVE   1   TO   FLG-GAIHAKU
                  END-EVALUATE
                  EVALUATE   WRK-DAY2(IDY)
                     WHEN   ZERO
                         COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                                  RECE62-NYUTOKKEI2(KH-IDX)  -
                                  WRK-MIJISI
                         MOVE     1      TO    FLG-MIJISI-GENSAN        
                  END-EVALUATE
                  IF  WRK-DAY6(IDY)          NOT =  ZERO
                  AND WRK-DAY6-ZAITEN(IDY)   NOT =  ZERO
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-DAY6-ZAITEN(IDY)
                  END-IF
                  IF  WRK-ACCT90(IDY)        NOT =  ZERO
                      COMPUTE  WRK-NYUTOKRYO = WRK-NYUTOKRYO
                                             + WRK-ACCT90(IDY)
                  END-IF
                  IF     FLG-GAIHAKU  =  ZERO
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE62-NYUTOKRYO(ID1)        = ZERO   
                            AND RECE62-NYUTOKRYO-RYAKU(ID1)  = SPACE )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO-RYAKU(ID1)  =  WRK-RYAKUSHO
                            IF   KH-IDX =  1
                            OR   RECE62-HKNNUM  =  SPACE
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE   WRK-RYAKUSHO    TO  RECE62-NYUTOKRYO-RYAKU
                                                                   (ID1)
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                    END-IF
                  ELSE
                    MOVE     ZERO     TO    FLG-ARI
                    PERFORM  VARYING  ID1    FROM    1   BY  1
                        UNTIL ( ID1                          > 10    )  
                          OR  ( RECE62-NYUTOKRYO(ID1)        = ZERO   
                            AND RECE62-NYUTOKRYO-RYAKU(ID1)  = SPACE )
                          OR  ( FLG-ARI                      = 1     )
                      IF  RECE62-NYUTOKRYO(ID1)  =  WRK-NYUTOKRYO
                            IF   KH-IDX =  1
                            OR   RECE62-HKNNUM  =  SPACE
                              ADD   1   TO   RECE62-NYUTOKNISSU(ID1)
                            END-IF
                            ADD   WRK-NYUTOKRYO
                                      TO   RECE62-NYUTOKKEI(ID1 KH-IDX)
                            MOVE  1   TO   FLG-ARI
                      END-IF
                    END-PERFORM  
                    IF  FLG-ARI  =  ZERO  
                      ADD        1           TO  RECE62-NYUTOKNISSU(ID1)
                      MOVE   WRK-NYUTOKRYO   TO  RECE62-NYUTOKRYO(ID1)
                      ADD    WRK-NYUTOKRYO   
                                       TO  RECE62-NYUTOKKEI(ID1 KH-IDX)
                    END-IF
                  END-IF
           END-EVALUATE
      *
           .
       NYUINACCT-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料加減算点数の計算
      **************************************************************
       NYUINACCT-GET2-SEC           SECTION.
      *
           IF  NACCT-SRYCDTOTAL       =  190080470   OR                 
                                         190799670   OR                 
                                         190080570   OR                 
                                         190799770   OR                 
                                         190111690   OR                 
                                         190809090   OR                 
                                         190111790   OR                 
                                         190809190                      
              CONTINUE                                                  
           ELSE                                                         
      *
              EVALUATE   WRK-DAY1-NYUTENTTLKBN(IDY)
                 WHEN   903
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +  NACCT-ZAITEN
              END-EVALUATE
      *
           END-IF
      *
           .
       NYUINACCT-GET2-EXT.
           EXIT.
      *
      **************************************************************
      *    入院基本料と加減算点数の取得
      **************************************************************
       NYUINKHN-GET-SEC           SECTION.
      *
           IF   NACCT-SRYKBN  =  "90"                                   
                 MOVE   903   TO  WRK-NYUTENTTLKBN                      
           ELSE                                                         
                 MOVE   920   TO  WRK-NYUTENTTLKBN                      
           END-IF                                                       
      *
           MOVE    ZERO               TO   WRK-KHNTEN
           MOVE    SPACE              TO   WRK-RYAKUSHO
           MOVE    ZERO               TO   NYUINACT-IDX2
           MOVE    ZERO               TO   WRK-KSNTEN
           MOVE    ZERO               TO   WRK-SENTEI-KHNTEN
           MOVE    ZERO               TO   WRK-SENTEI-ZAITEN
           MOVE    SPACE              TO   WRK-GAIHAKU-SRYCD
           MOVE    ZERO               TO   WRK-GAIHAKU-KHNTEN
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =    ZERO
               MOVE    "NYUINACT-KEY9"     TO   ORC-DBPATH
               PERFORM NYUINACT-READ-SEC
           ELSE
               MOVE    1              TO   FLG-NYUINACT
           END-IF
      *
           IF      FLG-NYUINACT        =   ZERO
      *
               MOVE      ZERO          TO  FLG-NYUKHN  
               PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                 UNTIL  (FLG-NYUKHN         =   1)    OR
                        (NYUINACT-IDX       >   5)    OR
                        (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
                     MOVE    SPACE         TO   TNS-TENSU-REC
                     INITIALIZE                 TNS-TENSU-REC
                     MOVE    NSRY-SRYCD(NYUINACT-IDX) TO   TNS-SRYCD
                     MOVE    NACCT-SRYYM        TO  ORC-DBYMD(1:6)
                     MOVE    "01"               TO  ORC-DBYMD(7:2)
                     PERFORM  TENSU-READ-SEC
                     IF       FLG-TENSU      =    ZERO
                         IF   TNS-DATAKBN    =    1
                              MOVE   TNS-TEN   TO   WRK-KHNTEN
      *                       初期加算（及び長期減算）の点数取得
                              COMPUTE   WRK-KSNTEN  =  NACCT-ZAITEN
                                                    -  WRK-KHNTEN
      *                       看護比率基準未達時の入院基本点数の取得
                              PERFORM  GAIHAKU-NYUINKHN-SET-SEC
                              IF   WRK-GAIHAKU-KHNTEN  NOT =  ZERO
                                 MOVE   WRK-GAIHAKU-KHNTEN
                                               TO   WRK-KHNTEN
                              END-IF
                              MOVE   NYUINACT-IDX
                                               TO   NYUINACT-IDX2
                              MOVE   1         TO   FLG-NYUKHN
                         END-IF
                     END-IF                  
                     MOVE    "DBCLOSE"          TO  MCP-FUNC
                     MOVE    "TENSU-KEY"        TO  ORC-DBPATH
                     CALL    "ORCMCPSUB"        USING
                                                MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
               END-PERFORM
      *
               IF    WRK-NYUTENTTLKBN     =   920  
               AND   NYUINACT-IDX2    NOT =   ZERO
      *
                 MOVE    SPACE         TO   NYURYAKUSHO-REC
                 INITIALIZE                 NYURYAKUSHO-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2) 
                                       TO   NYURYAKUSHO-SRYCD
                 MOVE    NACCT-SRYYM   TO   ORC-DBYMD(1:6)
                 MOVE    "01"          TO   ORC-DBYMD(7:2)
                 PERFORM  NYURYAKUSHO-READ-SEC
                 IF       FLG-NYURYAKUSHO    =    ZERO
                   MOVE     NYURYAKUSHO-RYAKUSHO
                                       TO  WRK-RYAKUSHO
                 END-IF                  
                 MOVE    "DBCLOSE"               TO  MCP-FUNC
                 MOVE    "NYUINRYAKUSHO-KEY"     TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"             USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
      *
               END-IF 
      *
      *        選定入院料の算定
      *
               IF    NYUINACT-IDX2          NOT   =   ZERO
      *
                 MOVE    SPACE                TO  SENTEICDCHG-REC
                 INITIALIZE                       SENTEICDCHG-REC
                 MOVE    NSRY-SRYCD(NYUINACT-IDX2)
                                              TO  SENCHG-IPNSRYCD
                 MOVE    SENTEICDCHG-REC      TO  MCPDATA-REC
                 MOVE    "DBSELECT"           TO  MCP-FUNC
                 MOVE    "SENTEICDCHG-KEY2"   TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"         USING
                                             MCPAREA
                                             ORCMCPAREA
                                             MCPDATA-REC
                 IF      MCP-RC             =    ZERO
                     MOVE  "SENTEICDCHG-KEY2" TO  ORC-DBPATH
                     PERFORM  SENTEICDCHG-READ-SEC
                 ELSE
                     MOVE    1                TO  FLG-SENTEICDCHG
                 END-IF
      *
                 IF  FLG-SENTEICDCHG   =   ZERO
      *
                    MOVE    SPACE               TO    TNS-TENSU-REC
                    INITIALIZE                        TNS-TENSU-REC
                    MOVE    SENCHG-SENTEISRYCD  TO    TNS-SRYCD
                    MOVE    NACCT-SRYYM         TO    ORC-DBYMD(1:6)
                    MOVE    "01"                TO    ORC-DBYMD(7:2)
                    PERFORM  TENSU-READ-SEC
                    IF       FLG-TENSU      =    ZERO
                       MOVE     TNS-TEN         TO    WRK-SENTEI-KHNTEN
                       COMPUTE  WRK-SENTEI-ZAITEN  =  WRK-SENTEI-KHNTEN
                                                   +  WRK-KSNTEN
                    END-IF  
                    MOVE    "DBCLOSE"            TO  MCP-FUNC
                    MOVE    "TENSU-KEY"          TO  ORC-DBPATH
                    CALL    "ORCMCPSUB"          USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
      *
                 END-IF
      *
                 MOVE    "DBCLOSE"               TO  MCP-FUNC
                 MOVE    "SENTEICDCHG-KEY2"      TO  ORC-DBPATH
                 CALL    "ORCMCPSUB"             USING
                                                 MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
      *
               END-IF
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACT-KEY9"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       NYUINKHN-GET-EXT.
           EXIT.
      *
      **************************************************************
      *    外泊時の入院基本料取得
      **************************************************************
       GAIHAKU-NYUINKHN-SET-SEC           SECTION.
      *
           EVALUATE  TNS-SRYCD
               WHEN  "190078210"
                     MOVE  "190077410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078310"
                     MOVE  "190077510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078410"
                     MOVE  "190077610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078510"
                     MOVE  "190077710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078610"
                     MOVE  "190077810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078710"
                     MOVE  "190077910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078810"
                     MOVE  "190078010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190078910"
                     MOVE  "190078110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190082410"
                     MOVE  "190081710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082510"
                     MOVE  "190081810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082610"
                     MOVE  "190081910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082710"
                     MOVE  "190082010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082810"
                     MOVE  "190082110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190082910"
                     MOVE  "190082210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190083010"
                     MOVE  "190082310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190084310"
                     MOVE  "190083610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084410"
                     MOVE  "190083710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084510"
                     MOVE  "190083810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084610"
                     MOVE  "190083910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084710"
                     MOVE  "190084010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084810"
                     MOVE  "190084110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190084910"
                     MOVE  "190084210"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190087410"
                     MOVE  "190085910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087510"
                     MOVE  "190086010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087610"
                     MOVE  "190086110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087710"
                     MOVE  "190086210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087810"
                     MOVE  "190086310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190087910"
                     MOVE  "190086410"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088010"
                     MOVE  "190086510"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088110"
                     MOVE  "190086610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088210"
                     MOVE  "190086710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088310"
                     MOVE  "190086810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088410"
                     MOVE  "190086910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088510"
                     MOVE  "190087010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088610"
                     MOVE  "190087110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088710"
                     MOVE  "190087210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190088810"
                     MOVE  "190087310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190094710"
                     MOVE  "190093710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094810"
                     MOVE  "190093810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190094910"
                     MOVE  "190093910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095010"
                     MOVE  "190094010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190095110"
                     MOVE  "190094110"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190797410"
                     MOVE  "190796610"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797510"
                     MOVE  "190796710"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797610"
                     MOVE  "190796810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797710"
                     MOVE  "190796910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797810"
                     MOVE  "190797010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190797910"
                     MOVE  "190797110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798010"
                     MOVE  "190797210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190798110"
                     MOVE  "190797310"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190803510"
                     MOVE  "190802810"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803610"
                     MOVE  "190802910"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803710"
                     MOVE  "190803010"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803810"
                     MOVE  "190803110"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190803910"
                     MOVE  "190803210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804010"
                     MOVE  "190803310"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190804110"
                     MOVE  "190803410"      TO   WRK-GAIHAKU-SRYCD
      *
               WHEN  "190091210"
                     MOVE  "190090210"      TO   WRK-GAIHAKU-SRYCD
               WHEN  "190091310"
                     MOVE  "190090310"      TO   WRK-GAIHAKU-SRYCD
      *
           END-EVALUATE
      *
           IF    WRK-GAIHAKU-SRYCD    NOT =   SPACE
               MOVE    SPACE         TO   TNS-TENSU-REC
               INITIALIZE                 TNS-TENSU-REC
               MOVE    WRK-GAIHAKU-SRYCD  TO  TNS-SRYCD
               MOVE    NACCT-SRYYM        TO  ORC-DBYMD(1:6)
               MOVE    "01"               TO  ORC-DBYMD(7:2)
               PERFORM  TENSU-READ-SEC
               IF       FLG-TENSU      =    ZERO
                        MOVE   TNS-TEN    TO  WRK-GAIHAKU-KHNTEN
               END-IF                  
           END-IF
      *
           .
       GAIHAKU-NYUINKHN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU23-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
                 MOVE    1          TO        KH-IDX
                 PERFORM    VARYING   ACCT-IDX   FROM   1    BY   1
                            UNTIL    ACCT-IDX   >      31
                        IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                            PERFORM      SRYACCT-GET-SEC
                        END-IF
                 END-PERFORM
           END-IF
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      ACCT-HKNCOMBI  =   RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
                                UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO   KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
      *
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                     PERFORM    VARYING  ACCT-IDX   FROM   1   BY   1
                                UNTIL    ACCT-IDX   >      31
                          IF  ACCT-DAY(1 ACCT-IDX)   NOT =    ZERO
                              PERFORM      SRYACCT-GET-SEC
                          END-IF
                     END-PERFORM
                 END-IF
               END-IF  
           END-PERFORM
      *
           .
       RECE62-TENSOU23-EXT.
           EXIT.
      *
      **************************************************************
      *    診療会計の入院基本料加減算点数の計算
      **************************************************************
       SRYACCT-GET-SEC           SECTION.
      *
           EVALUATE   WRK-DAY1-NYUTENTTLKBN(ACCT-IDX)
              WHEN   903
                 IF        ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -  ACCT-ZAITEN
                 ELSE
                    COMPUTE  RECE62-NYUKHNKEI2(KH-IDX)  =
                             RECE62-NYUKHNKEI2(KH-IDX)  +  ACCT-ZAITEN
                 END-IF
              WHEN   920
                 IF        ACCT-SRYCDTOTAL    =    190080470
                                              OR   190799670
                                              OR   190080570
                                              OR   190799770
                                              OR   190111690
                                              OR   190809090
                                              OR   190111790
                                              OR   190809190
                    COMPUTE  RECE62-NYUTOKKEI2(KH-IDX)  =
                             RECE62-NYUTOKKEI2(KH-IDX)  -  ACCT-ZAITEN
                 END-IF
           END-EVALUATE
      *
           .
       SRYACCT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE62-HENSYU3-SEC                SECTION.
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN          =    "1"  
                 AND    NACCT-SRYCDTOTAL   NOT   =    099999916
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                         AND WRK-DAY1-NYUTENTTLKBN(IDY) = 903
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM   NYUINKHN-GET3-SEC
                              END-IF
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
      *
           INITIALIZE                            NYUINACCT-REC
           MOVE    RECE62-HOSPID         TO      NACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN      TO      NACCT-NYUGAIKBN
           MOVE    RECE62-PTID           TO      NACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)     TO      NACCT-SRYKA
           MOVE    RECE62-SRYYM          TO      NACCT-SRYYM
           MOVE    NYUINACCT-REC         TO      MCPDATA-REC
           MOVE    "DBSELECT"            TO      MCP-FUNC
           MOVE    "NYUINACCT-KEY7"      TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACCT-KEY7"  TO      ORC-DBPATH
               PERFORM  NYUINACCT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACCT  =    "1")
      *
                 IF     NACCT-ZAISKBKBN    =    "6"  
      *
                   PERFORM  VARYING  IDY    FROM    1   BY  1
                                  UNTIL   IDY     >       31
      *
                      PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                         IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                              IF   NACCT-ZAITEN    NOT =    ZERO  
                              AND  NACCT-DAY(IDY)  NOT =    ZERO  
                                   PERFORM   NYUINKHN-GET3-SEC
                              END-IF
                         END-IF
      *
                      END-PERFORM
      *
                   END-PERFORM
      *
                 END-IF
      *
                 MOVE          "NYUINACCT-KEY7"  TO   ORC-DBPATH
                 PERFORM        NYUINACCT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACCT-KEY7"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
      *+++
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY7"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY7"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                                IF   ACCT-SRYKBN  =  "90" OR "92"  
                                    PERFORM   ACCTKHN-GET3-SEC
                                END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE     "SRYACCT-KEY7"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
      *    未実施減算（システム管理設定分）
      *
           IF   FLG-MIJISI-GENSAN   =   1
      *
               IF   FLG-MIJISI-INNAI   =   1
                 IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190080570      TO   WRK-SRYCD
                 ELSE
                    MOVE    190799770      TO   WRK-SRYCD
                 END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-IRYO    =   1
                 IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111690      TO   WRK-SRYCD
                 ELSE
                    MOVE    190809090      TO   WRK-SRYCD
                 END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   RYAKUSYO-HENSYU-SEC
               END-IF
      *
               IF   FLG-MIJISI-JYOKU   =   1
                 IF    RECE62-RJNHKNNUM    =    SPACE
                    MOVE    190111790      TO   WRK-SRYCD
                 ELSE
                    MOVE    190809190      TO   WRK-SRYCD
                 END-IF
                 MOVE    WRK-SRYYMD-TAIHI  TO   WRK-SRYYMD
      *
                 PERFORM   RYAKUSYO-HENSYU-SEC
               END-IF
      *
           END-IF
      *
           .
       RECE62-HENSYU3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得
      **************************************************************
       NYUINKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPID       TO   NSRY-HOSPID
           MOVE    NACCT-NYUGAIKBN    TO   NSRY-NYUGAIKBN
           MOVE    NACCT-PTID         TO   NSRY-PTID
      ***  MOVE    NACCT-SRYKA        TO   NSRY-SRYKA
           MOVE    NACCT-SRYYM        TO   NSRY-SRYYM
           MOVE    NACCT-ZAINUM       TO   NSRY-ZAINUM
           MOVE    NYUINACT-REC       TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "NYUINACT-KEY9"    TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =       ZERO
      *
               MOVE    "NYUINACT-KEY9"  TO      ORC-DBPATH
               PERFORM  NYUINACT-READ-SEC
               PERFORM  UNTIL  (FLG-NYUINACT  =    "1")
      *
                 PERFORM   VARYING  NYUINACT-IDX  FROM 1  BY  1
                      UNTIL  (NYUINACT-IDX       >   5)    OR
                             (NSRY-SRYCD(NYUINACT-IDX)  =  SPACE)
      *
                       MOVE    NSRY-SRYCD(NYUINACT-IDX) 
                                              TO  WRK-SRYCD
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD-TAIHI(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD-TAIHI(7:2)
                       MOVE    NACCT-SRYYM    TO  WRK-SRYYMD(1:6)
                       MOVE    "01"           TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   RYAKUSYO-HENSYU-SEC
      *
                       MOVE    "DBCLOSE"               TO  MCP-FUNC
                       MOVE    "NYUINRYAKUSHO-KEY"     TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"             USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                 END-PERFORM
      *
                 MOVE          "NYUINACT-KEY9"  TO   ORC-DBPATH
                 PERFORM        NYUINACT-READ-SEC
               END-PERFORM
      *
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINACT-KEY9"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       NYUINKHN-GET3-EXT.
           EXIT.
      *
      **************************************************************
      *    入院略称の取得（診療会計）
      **************************************************************
       ACCTKHN-GET3-SEC           SECTION.
      *
           INITIALIZE                           SRYACT-REC
           MOVE    ACCT-HOSPID          TO      SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN       TO      SRY-NYUGAIKBN
           MOVE    ACCT-PTID            TO      SRY-PTID
      ***  MOVE    ACCT-SRYKA           TO      SRY-SRYKA
           MOVE    ACCT-SRYYM           TO      SRY-SRYYM
           MOVE    ACCT-ZAINUM          TO      SRY-ZAINUM
           MOVE    SRYACT-REC           TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACT-KEY9"        TO      ORC-DBPATH
           CALL    "ORCMCPSUB"          USING   MCPAREA
                                                ORCMCPAREA
                                                MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACT-KEY9"    TO      ORC-DBPATH
               PERFORM  SRYACT-READ-SEC
               PERFORM  UNTIL  FLG-SRYACT    =    1
                   PERFORM   VARYING   SRYACT-IDX  FROM  1   BY   1
                             UNTIL    (SRYACT-IDX    >   5)  OR
                                      (SRY-SRYCD(SRYACT-IDX)
                                                         =   SPACE)
      *
                       MOVE    SRY-SRYCD(SRYACT-IDX)
                                                  TO  WRK-SRYCD
                       MOVE    ACCT-SRYYM         TO  WRK-SRYYMD(1:6)
                       MOVE    "01"               TO  WRK-SRYYMD(7:2)
      *
                       PERFORM   RYAKUSYO-HENSYU-SEC
      *
                       MOVE    "DBCLOSE"               TO  MCP-FUNC
                       MOVE    "NYUINRYAKUSHO-KEY"     TO  ORC-DBPATH
                       CALL    "ORCMCPSUB"             USING
                                                       MCPAREA
                                                       ORCMCPAREA
                                                       MCPDATA-REC
                   END-PERFORM
                   MOVE     "SRYACT-KEY9"    TO   ORC-DBPATH
                   PERFORM   SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       ACCTKHN-GET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院略称の編集
      *****************************************************************
       RYAKUSYO-HENSYU-SEC                SECTION.
      *
           MOVE    SPACE         TO   NYURYAKUSHO-REC
           INITIALIZE                 NYURYAKUSHO-REC
      *
           MOVE    WRK-SRYCD     TO   NYURYAKUSHO-SRYCD
           MOVE    WRK-SRYYMD    TO   ORC-DBYMD
           PERFORM  NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
              MOVE     ZERO     TO    FLG-ARI
              PERFORM  VARYING  ID1    FROM    1   BY  1
                    UNTIL   ID1                   > 20
                      OR    RECE62-NYUSYUBETU(ID1) = SPACE
                      OR    FLG-ARI                = 1 
                 IF  RECE62-NYUSYUBETU(ID1)  
                            =  NYURYAKUSHO-RYAKUSHO
                     MOVE  1   TO   FLG-ARI
                 END-IF
              END-PERFORM  
              IF  FLG-ARI  =  ZERO  
                     MOVE   NYURYAKUSHO-RYAKUSHO
                               TO   RECE62-NYUSYUBETU(ID1)
              END-IF
           END-IF                  
      *
           .
       RYAKUSYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE62-HENSYU4-SEC                SECTION.
      *
           PERFORM  VARYING  IDY    FROM    1   BY  1
                          UNTIL   IDY     >       31
      *
              PERFORM  VARYING  IDX1    FROM    1   BY  1
                             UNTIL   IDX1    >       10
                             OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
      *
                 IF  WRK-DAY5(IDY)  =  RECE62-HKNCOMBI (IDX1)
                      IF   WRK-DAY4(IDY)   NOT =    ZERO  
                           PERFORM   NYUINKHN-GET4-SEC
                      END-IF
                 END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           .
       RECE62-HENSYU4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称の取得
      **************************************************************
       NYUINKHN-GET4-SEC           SECTION.
      *
           EVALUATE   WRK-DAY4(IDY)
             WHEN  1
                EVALUATE   WRK-SHOKUJIRYOYO
                  WHEN  1
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  NYUINKHN-SET4-SEC
                     END-IF
                  WHEN  2
                     MOVE    "197000710"  TO  WRK-SRYCD
                     PERFORM  NYUINKHN-SET4-SEC
                END-EVALUATE
             WHEN  2
             WHEN  3
             WHEN  4
                     MOVE    "197000110"  TO  WRK-SRYCD
                     PERFORM  NYUINKHN-SET4-SEC
                     IF    WRK-TOKUKANRIKASAN  =  1
                        MOVE    "197000270"  TO  WRK-SRYCD
                        PERFORM  NYUINKHN-SET4-SEC
                     END-IF
                     IF    WRK-DAY41(IDY)  =  1
                        MOVE    "197000570"  TO  WRK-SRYCD
                        PERFORM  NYUINKHN-SET4-SEC
                     END-IF
           END-EVALUATE
      *
           .
       NYUINKHN-GET4-EXT.
           EXIT.
      *
      **************************************************************
      *    食事略称のセット
      **************************************************************
       NYUINKHN-SET4-SEC           SECTION.
      *
           MOVE    SPACE          TO   NYURYAKUSHO-REC
           INITIALIZE                  NYURYAKUSHO-REC
           MOVE    WRK-SRYCD      TO   NYURYAKUSHO-SRYCD
           MOVE    RECE62-SRYYM   TO   ORC-DBYMD(1:6)
           MOVE    "01"           TO   ORC-DBYMD(7:2)
           PERFORM  NYURYAKUSHO-READ-SEC
           IF       FLG-NYURYAKUSHO    =    ZERO
                MOVE     ZERO     TO    FLG-ARI
                PERFORM  VARYING  ID1    FROM    1   BY  1
                       UNTIL   ID1                   > 3
                         OR    RECE62-SHOKUJISYUBETU(ID1) = SPACE
                         OR    FLG-ARI                = 1 
                    IF  RECE62-SHOKUJISYUBETU(ID1)  
                               =  NYURYAKUSHO-RYAKUSHO
                        MOVE  1   TO   FLG-ARI
                    END-IF
                END-PERFORM  
                IF  FLG-ARI  =  ZERO  
                    MOVE   NYURYAKUSHO-RYAKUSHO
                             TO   RECE62-SHOKUJISYUBETU(ID1)
                END-IF
           END-IF                  
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "NYUINRYAKUSHO-KEY"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       NYUINKHN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの編集（Ｆ００２）
      *****************************************************************
       RECE62-HENSYU9-SEC                SECTION.
      *
      *     点滴薬剤（器材）まとめ処理
           PERFORM    TENTEK-MATOME-SEC
      *
           INITIALIZE    WRK-TENTEK-FLG-TABLE
      *
           INITIALIZE                           SRYACCT-REC
           MOVE    RECE62-HOSPID        TO      ACCT-HOSPID
           MOVE    RECE62-NYUGAIKBN     TO      ACCT-NYUGAIKBN
           MOVE    RECE62-PTID          TO      ACCT-PTID
      ***  MOVE    RECE62-SRYKA(IDX)    TO      ACCT-SRYKA
           MOVE    RECE62-SRYYM         TO      ACCT-SRYYM
           MOVE    SRYACCT-REC          TO      MCPDATA-REC
           MOVE    "DBSELECT"           TO      MCP-FUNC
           MOVE    "SRYACCT-KEY7"       TO      ORC-DBPATH
           CALL    "ORCMCPSUB"           USING   MCPAREA
                                                 ORCMCPAREA
                                                 MCPDATA-REC
           IF      MCP-RC               =       ZERO
               MOVE    "SRYACCT-KEY7"   TO      ORC-DBPATH
               PERFORM  SRYACCT-READ-SEC
               PERFORM  UNTIL
                  (FLG-SRYACCT        =    1)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       10
                           OR      RECE62-HKNCOMBI (IDX1)  =   ZERO 
                       IF      ACCT-HKNCOMBI   =
                                               RECE62-HKNCOMBI (IDX1)
                           MOVE   IDX1          TO   TENTEK-IDX
                           IF   (ACCT-ZAITEN    NOT =    ZERO)    AND
                                (ACCT-ZAIKAISU  NOT =    ZERO)
                               PERFORM   RECE62-TENSOU9-SEC
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE     "SRYACCT-KEY7"   TO   ORC-DBPATH
                   PERFORM   SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACCT-KEY7"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       RECE62-HENSYU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト中間ファイルの項目転送（Ｆ００２）
      *****************************************************************
       RECE62-TENSOU9-SEC                 SECTION.
      *
           IF    (RECE62-HKNNUM          NOT =   SPACE)   OR
                 (RECE62-RJNHKNNUM       NOT =   SPACE)
               IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                 OR "96" OR "97"
                   CONTINUE
               ELSE
                 IF        ACCT-SRYCDTOTAL    =    630010002
      *                                       OR   190080470
      *                                       OR   190799670
      *                                       OR   190080570
      *                                       OR   190799770
      *                                       OR   190111690
      *                                       OR   190809090
      *                                       OR   190111790
      *                                       OR   190809190
                   COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                             TBL-TOTALTEN-ACCT(1) -
                             (ACCT-ZAITEN  *  
                              ACCT-ZAIKAISU)
                 ELSE
                   IF        WRK-TENTEK-FLG(TENTEK-IDX 1)  =   ZERO
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                       MOVE  1        TO   WRK-TENTEK-FLG(TENTEK-IDX 1)
                   END-IF
                   IF        ACCT-SRYKBN     =    "33"
                       PERFORM   TENTEK-SYUTEN-SEC
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 WRK-TENTEK-SYUTEN
                   ELSE
                       COMPUTE   TBL-TOTALTEN-ACCT(1) =  
                                 TBL-TOTALTEN-ACCT(1) +
                                 (ACCT-ZAITEN  *  
                                  ACCT-ZAIKAISU)
                   END-IF
                 END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO             TO   HC-IDX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      RECE62-HKNCOMBI (IDX1)   =   ZERO 
               IF      ACCT-HKNCOMBI   =
                                        RECE62-HKNCOMBI (IDX1)
                   MOVE    IDX1            TO  HC-IDX
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM          
      *
           MOVE       ZERO              TO   KH-IDX
           PERFORM    VARYING   IDX2    FROM   1   BY   1
             UNTIL   (IDX2      >       4)
               MOVE       ZERO           TO   KH-IDX
               IF     RECE62-KOHNUMHC(HC-IDX IDX2)  NOT =  SPACE
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(1)
                      MOVE    2          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(2)
                      MOVE    3          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(3)
                      MOVE    4          TO        KH-IDX
                 END-IF
                 IF   RECE62-KOHNUMHC(HC-IDX IDX2) =  RECE62-KOHNUM(4)
                      MOVE    5          TO        KH-IDX
                 END-IF
      *
                 IF   KH-IDX             NOT =    ZERO
                   IF        ACCT-SRYKBN   =    "90" OR "92" OR "95"  
                                                     OR "96" OR "97"
                       CONTINUE
                   ELSE
                     IF        ACCT-SRYCDTOTAL    =    630010002
      *                                           OR   190080470
      *                                           OR   190799670
      *                                           OR   190080570
      *                                           OR   190799770
      *                                           OR   190111690
      *                                           OR   190809090
      *                                           OR   190111790
      *                                           OR   190809190
                       COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                TBL-TOTALTEN-ACCT(KH-IDX) -
                                (ACCT-ZAITEN * 
                                 ACCT-ZAIKAISU)
                     ELSE
                       IF        WRK-TENTEK-FLG(TENTEK-IDX KH-IDX)
                                                             =   ZERO
                           COMPUTE   TBL-TOTALTEN-ACCT(KH-IDX) =  
                                     TBL-TOTALTEN-ACCT(KH-IDX) +
                                     WRK-TENTEK-YKZTEN(TENTEK-IDX) +
                                     WRK-TENTEK-KIZAITEN(TENTEK-IDX)
                           MOVE  1       TO   WRK-TENTEK-FLG
                                                   (TENTEK-IDX KH-IDX)
                       END-IF
                       IF        ACCT-SRYKBN     =    "33"
                           PERFORM   TENTEK-SYUTEN-SEC
                           COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                    TBL-TOTALTEN-ACCT(KH-IDX) +
                                    WRK-TENTEK-SYUTEN
                       ELSE
                           COMPUTE  TBL-TOTALTEN-ACCT(KH-IDX) =
                                    TBL-TOTALTEN-ACCT(KH-IDX) +
                                   (ACCT-ZAITEN * 
                                    ACCT-ZAIKAISU)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
               END-IF  
           END-PERFORM
      *
           .
       RECE62-TENSOU9-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の薬剤（器材）点数算定
      *****************************************************************
       TENTEK-MATOME-SEC                    SECTION.
      *
           INITIALIZE                  WRK-TENTEK-TABLE
      *
           PERFORM     VARYING   TENTEK-IDX  FROM  1  BY  1
                         UNTIL  (TENTEK-IDX  >  10)    OR
                                (RECE62-HKNCOMBI(TENTEK-IDX) = ZERO)
               INITIALIZE                  ORCSTENTEKAREA
               MOVE    "2"                 TO  ORCSTEN-KBN
               MOVE    RECE62-PTID        TO  ORCSTEN-PTID
               MOVE    RECE62-SRYYM       TO  ORCSTEN-SRYYM
               MOVE    RECE62-HKNCOMBI(TENTEK-IDX)
                                          TO  ORCSTEN-HKNCOMBI
               MOVE    RECE62-HOSPID      TO  SPA-HOSPID
               CALL    "ORCSTENTEK"        USING
                                           SPA-AREA
                                           ORCSTENTEKAREA
      *        薬剤点数と器材点数
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       31
      *
                        IF  ORCSTEN-ZAITEN(IDX1)  NOT =  ZERO
                            ADD    ORCSTEN-ZAITEN(IDX1)
                                   TO  WRK-TENTEK-YKZTEN(TENTEK-IDX)
                        END-IF
               END-PERFORM
      *
               PERFORM  VARYING  IDX1    FROM    1   BY  1
                          UNTIL  IDX1     >       50
                           OR    ORCSTEN-KIZ-ZAITEN(IDX1)  =  ZERO
      *
                        MOVE    ZERO   TO   WRK-TENTEK-ZAIKAISU
                        PERFORM  VARYING  IDX2    FROM    1   BY  1
                              UNTIL   IDX2     >       31
                            ADD    ORCSTEN-KIZ-DAY(IDX1 IDX2)
                                   TO  WRK-TENTEK-ZAIKAISU
                        END-PERFORM
      *
                        COMPUTE  WRK-TENTEK-KIZAITEN(TENTEK-IDX)  =
                                 WRK-TENTEK-KIZAITEN(TENTEK-IDX)  +
                                 ( ORCSTEN-KIZ-ZAITEN(IDX1) *
                                   WRK-TENTEK-ZAIKAISU ) 
      *
               END-PERFORM
           END-PERFORM
      *
           .
       TENTEK-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    点滴注射の手技点数算定
      *****************************************************************
       TENTEK-SYUTEN-SEC                    SECTION.
      *
      *診療行為テーブルから診療種別区分の取得
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPID        TO   SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN     TO   SRY-NYUGAIKBN
           MOVE    ACCT-PTID          TO   SRY-PTID
      ***  MOVE    ACCT-SRYKA         TO   SRY-SRYKA
           MOVE    ACCT-SRYYM         TO   SRY-SRYYM
           MOVE    ACCT-ZAINUM        TO   SRY-ZAINUM
           MOVE    SRYACT-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "SRYACT-KEY9"      TO   ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC             =    ZERO
                 MOVE    "SRYACT-KEY9"    TO   ORC-DBPATH
                 PERFORM  SRYACT-READ-SEC
           ELSE
                 MOVE    1                TO   FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                   TO   WRK-TENTEK-SYUTEN
      *
           IF   FLG-SRYACT   =    ZERO
                EVALUATE  SRY-SRYSYUKBN
                    WHEN  "330"
                    WHEN  "331"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-SYUTEN1 * ACCT-ZAIKAISU)
                    WHEN  "340"
                       COMPUTE WRK-TENTEK-SYUTEN = WRK-TENTEK-SYUTEN +
                               (ACCT-ZAITEN * ACCT-ZAIKAISU)
                END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSE"               TO  MCP-FUNC
           MOVE    "SRYACT-KEY9"           TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
      *
           .
       TENTEK-SYUTEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    実日数の項目転送（Ｆ００２）
      *****************************************************************
       JNISSU-SET-SEC                    SECTION.
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      5
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     31
                    IF    TBL-JNISSU-DAY(IDX IDY)  NOT =  SPACE
                          ADD    1      TO    RECE62-JNISSU(IDX)
                    END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
               PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI(IDX IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI(IDX IDY)
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             RECE62-NYUKHNKEI2(IDY)
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             RECE62-NYUTOKKEI2(IDY)
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    1   BY   1
                          UNTIL      IDY    >     5
                    COMPUTE  RECE62-TOTALTEN(IDY)  = 
                             RECE62-TOTALTEN(IDY)  + 
                             TBL-TOTALTEN-ACCT(IDY)
           END-PERFORM
      *
           PERFORM    VARYING    IDX    FROM   1   BY   1
                      UNTIL      IDX    >      10
               PERFORM    VARYING    IDY    FROM    2   BY   1
                          UNTIL      IDY    >     5
                 IF  ( RECE62-TOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-TOTALTEN(1)   NOT =  
                                         RECE62-TOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI(IDX IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI(IDX IDY)
                 END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM    VARYING    IDY    FROM    2   BY   1
                      UNTIL      IDY    >     5
                 IF  ( RECE62-TOTALTEN(IDY) NOT =  ZERO       )
                 AND ( RECE62-TOTALTEN(1)   NOT =  
                                         RECE62-TOTALTEN(IDY) )
                    CONTINUE
                 ELSE
                    MOVE    ZERO    TO   RECE62-NYUKHNKEI2(IDY)
                    MOVE    ZERO    TO   RECE62-NYUTOKKEI2(IDY)
                 END-IF
           END-PERFORM
      *
           .
       JNISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理（Ｆ００２）
      *****************************************************************
       RECE62-REWRITE-SEC                SECTION.
      *
           REWRITE   RECE62-REC
      *
           ADD     1                   TO  CNT-MOD (IDZ)
      *
           .
       RECE62-WRITE-EXT.
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
           DISPLAY "RECE620 IN  :" CNT-IN   (1) 
           DISPLAY "        MOD :" CNT-MOD  (1) 
           DISPLAY "RECE62X IN  :" CNT-IN   (2) 
           DISPLAY "        MOD :" CNT-MOD  (2) 
           DISPLAY "*** ORCR0630 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスター読込
      *****************************************************************
       NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       NYUINACCT-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    入院診療行為マスター読込
      **************************************************************
       NYUINACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
               MOVE    ZERO            TO  FLG-NYUINACT
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       NYUINACT-ERAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       SRYACCT-READ-SEC           SECTION.
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
      *    診療行為読込
      *****************************************************************
       SRYACT-READ-SEC           SECTION.
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
      *    点数マスター読込（主キー）
      *****************************************************************
       TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "TENSU-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "TENSU-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       TENSU-READ-EXT.
           EXIT.
      *     
      *****************************************************************
      *    入院略称マスター読込（主キー）
      *****************************************************************
       NYURYAKUSHO-READ-SEC        SECTION.
      *
           MOVE    NYURYAKUSHO-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "NYUINRYAKUSHO-KEY" TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  NYURYAKUSHO-REC
                   MOVE    ZERO                TO  FLG-NYURYAKUSHO
               ELSE
                   MOVE    1                   TO  FLG-NYURYAKUSHO
                   INITIALIZE                  NYURYAKUSHO-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-NYURYAKUSHO
               INITIALIZE                  NYURYAKUSHO-REC
           END-IF
      *
           .
       NYURYAKUSHO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    選定療養マスター読込
      *****************************************************************
       SENTEICDCHG-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
               MOVE    ZERO            TO  FLG-SENTEICDCHG
           ELSE
               INITIALIZE                  SENTEICDCHG-REC
               MOVE    1               TO  FLG-SENTEICDCHG
           END-IF
      *
           .
       SENTEICDCHG-READ-EXT.
           EXIT.
      *
      **************************************************************
      *    システム管理マスタ読み込み処理
      **************************************************************
       SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                                           SYS-5000-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
                                           SYS-5000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       SYSKANRI-READ-EXT.
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
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       RECE62-READ-SEC            SECTION.
      *
           READ    RECE62-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-RECE62
               NOT  AT  END
                   ADD     1     TO    CNT-IN (IDZ)
           END-READ
           .
       RECE62-READ-EXT.
           EXIT.
      *
