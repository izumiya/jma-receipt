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
       PROGRAM-ID.             ORCR0820.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                              （レセプト種別編集）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    05/01/08  同月に継続なしで２回入院した
      *                                     場合の入院年月日の記載方法
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
           SELECT  RECE88-FILE     ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE88-KEY
                                   FILE    STATUS  IS  STS-RECE88.
           SELECT  RECE88X-FILE    ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE88X-KEY
                                   FILE    STATUS  IS  STS-RECE88.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE88-FILE.
       01  RECE88-REC. 
           COPY    "CPRCF088.INC".
      *
       FD  RECE88X-FILE.
       01  RECE88X-REC. 
           COPY    "CPRCF088.INC"  REPLACING  //RECE88//
                                   BY         //RECE88X//.
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
                                   BY         //RECE82//.
      *
      *     入退院履歴情報ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01//
                                   BY         //RECE88//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEERR//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82              PIC X(02).
           03  STS-RECE88              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RECE82              PIC 9(01). 
           03  FLG-RECE88              PIC 9(01). 
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK23        PIC 9(01).
           03  FLG-PTNYUINRRK22        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-RECE88              PIC 9(06).
           03  CNT-OUT                 PIC 9(06). 
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(03). 
           03  IDX                     PIC 9(03). 
           03  IDX1                    PIC 9(03). 
           03  IDX2                    PIC 9(03). 
           03  IDX3                    PIC 9(03). 
           03  IDX4                    PIC 9(02).
           03  IDX5                    PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
      *
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-DBPATH                PIC X(64).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM         PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-RYOSTYMD            PIC X(08).
           03  WRK-RYOEDYMD            PIC X(08).
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
      *    ワーク入退院履歴
       01  WRK-RECE88-REC. 
           COPY    "CPRCF088.INC"  REPLACING  //RECE88//
                                   BY         //WRK-RECE88//.
      *
      *    ワーク入院履歴
       01  WRK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNYUINRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    病棟情報                                                     
           COPY    "CPSK5001.INC".                                      
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
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
                           USING   COMMAND-PARAM.
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
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       FLG-AREA
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCR0820"          TO  JOB-PGID
           MOVE    "レセプト種別編集（労災・自賠責）"    
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
      *
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
      *
           MOVE    "RECEERR"           TO  RECEERR-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEERR-TERMID
      *
           OPEN    I-O                 RECE82-FILE
      *
           OPEN    OUTPUT              RECE88X-FILE
           CLOSE                       RECE88X-FILE
           OPEN    I-O                 RECE88-FILE
      *
           PERFORM 900-RECE82-READ-SEC
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-HENSYU-SEC
      *
           PERFORM 900-RECE82-READ-SEC
           .
      *     
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    編集処理
      *****************************************************************
       210-HENSYU-SEC                  SECTION.
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      RECE82-ERR-ERRKBN   >   ZERO
               CONTINUE
           ELSE
               EVALUATE    RECE82-HKNKBN   ALSO    RECE82-SAIGAIKBN
                   WHEN    "1"             ALSO    "1"
                       MOVE    7081            TO  RECE82-RECESYUBETU
                   WHEN    "1"             ALSO    "2"
                       MOVE    7082            TO  RECE82-RECESYUBETU
                   WHEN    "2"             ALSO    "1"
                       MOVE    7101            TO  RECE82-RECESYUBETU
                   WHEN    "2"             ALSO    "2"
                       MOVE    7102            TO  RECE82-RECESYUBETU
                   WHEN    "4"             ALSO    ANY
                       MOVE    7011            TO  RECE82-RECESYUBETU
               END-EVALUATE
           END-IF 
      *
      *    入退院履歴情報編集・病棟番号・種別セット処理
           PERFORM 2101-RECE88-HENSYU-SEC
      *
           REWRITE RECE82-REC
           ADD     1     TO    CNT-OUT
           .
       210-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2101-RECE88-HENSYU-SEC                   SECTION.
      *                                                                
           INITIALIZE                      WRK-BYOTOKBN-TABLE          
                                           WRK-SHONUM
           MOVE         SPACE       TO     WRK-RYOSTYMD
                                           WRK-RYOEDYMD
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    RECE82-HOSPID       TO  PTNYUINRRK-HOSPID
           MOVE    RECE82-PTID         TO  PTNYUINRRK-PTID
           MOVE    RECE82-SRYYM        TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    RECE82-SRYYM        TO  PTNYUINRRK-TAIINYMD (1:6)
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
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   RECE82-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   RECE82-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック（会計に対象の組合せがないときは全て）
               IF      RECE82-DAY-AREA     =   ALL "0"
                   MOVE    "1"   TO   RECE82-NYUINRYOFLG
      *
                   MOVE    RECE82-SRYYM   TO   WRK-RYOSTYMD(1:6)
                   MOVE    IDX4           TO   WRK-RYOSTYMD(7:2)
                   MOVE    RECE82-SRYYM   TO   WRK-RYOEDYMD(1:6)
                   MOVE    IDX5           TO   WRK-RYOEDYMD(7:2)
                   IF   RECE82-RYOSTYMD  =   SPACE
                       MOVE    WRK-RYOSTYMD  TO   RECE82-RYOSTYMD
                   ELSE
                       IF  WRK-RYOSTYMD  <  RECE82-RYOSTYMD
                          MOVE    WRK-RYOSTYMD  TO   RECE82-RYOSTYMD
                       END-IF
                   END-IF
                   IF   RECE82-RYOEDYMD  =   SPACE
                       MOVE    WRK-RYOEDYMD  TO   RECE82-RYOEDYMD
                   ELSE
                       IF  WRK-RYOEDYMD  >  RECE82-RYOEDYMD
                          MOVE    WRK-RYOEDYMD  TO   RECE82-RYOEDYMD
                       END-IF
                   END-IF
      *
                   PERFORM 21011-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF      RECE82-DAY (IDX)
                                           =   RECE82-HKNCOMBI
                           PERFORM 21011-PTNYUINRRK-HENSYU-SEC
                           MOVE   IDX5                 TO  IDX
                       END-IF
                   END-PERFORM
               END-IF        
      *                 
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM        
      *                 
           MOVE    "PTNYUINRRK-KEY23"  TO  ORC-DBPATH
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING   MCPAREA
                                               ORCMCPAREA
                                               MCPDATA-REC
           .
       2101-RECE88-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       21011-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    病棟区分セット
           PERFORM    210112-BYOTOKBN-SET-SEC
      *
      *    診療科情報のセット
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       20
                   OR      PTNYUINRRK-NYUINKA
                                           =   RECE82-SRYKA (IDW)
               IF      RECE82-SRYKA (IDW)  =   SPACE  
                   MOVE    PTNYUINRRK-NYUINKA  TO  RECE82-SRYKA (IDW)
                   MOVE    20          TO  IDW
               END-IF
           END-PERFORM
      *
      *    入院年月日
           IF      RECE82-NYUINYMD    =   SPACE
               MOVE    PTNYUINRRK-NYUINYMD TO  RECE82-NYUINYMD
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           END-IF
      *
      *    入退院履歴情報
           PERFORM 210111-RECE88-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE88          =   1                            
               MOVE    PTNYUINRRK-REC  TO  WRK-PTNYUINRRK-REC
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    RECE82-HOSPID   TO  PTNYUINRRK-HOSPID
               MOVE    RECE82-PTID     TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNYUINRRK-SHONUM
                                       TO  PTNYUINRRK-SHONUM
               MOVE    RECE82-SRYYM    TO  PTNYUINRRK-NYUINYMD (1:6)
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
               IF    ( PTNYUINRRK-NYUINYMD <    RECE82-NYUINYMD )
               AND   ( PTNYUINRRK-SHONUM   =    WRK-SHONUM      )
                   MOVE    PTNYUINRRK-NYUINYMD  TO  RECE82-NYUINYMD
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 210111-RECE88-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "PTNYUINRRK-KEY22"  TO  ORC-DBPATH
               MOVE    "DBCLOSE"           TO  MCP-FUNC
               CALL    "ORCMCPSUB"         USING   MCPAREA
                                                   ORCMCPAREA
                                                   MCPDATA-REC
           END-IF
           .
       21011-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       210111-RECE88-WRITE-SEC         SECTION.
      *
           MOVE    RECE82-HOSPID       TO  RECE88-HOSPID
           MOVE    RECE82-SRYYM        TO  RECE88-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE88-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE88-PTID
           MOVE    RECE82-RECEKA       TO  RECE88-RECEKA
           MOVE    RECE82-HKNID        TO  RECE88-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE88-TEKSTYMD
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE88-NYUINYMD
           MOVE    RECE88-REC          TO  WRK-RECE88-REC          
           PERFORM 900-RECE88-READ-SEC
           IF      FLG-RECE88          =   1
               MOVE    WRK-RECE88-REC      TO  RECE88-REC 
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE88-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE88-NYUINCHUKBN
               WRITE   RECE88-REC
               ADD     1                   TO  CNT-RECE88
           END-IF    
           .
       210111-RECE88-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟番号・種別セット                                              
      *****************************************************************
       210112-BYOTOKBN-SET-SEC        SECTION. 
      *                       
      *    病棟情報                                           
           MOVE    SPACE                 TO  SYSKANRI-REC     
           INITIALIZE                        SYSKANRI-REC     
           MOVE    "5001"                TO  SYS-KANRICD      
           MOVE    PTNYUINRRK-BTUNUM     TO  SYS-KBNCD        
           MOVE    PTNYUINRRK-TENNYUYMD  TO  ORC-DBYMD
           PERFORM 900-SYSKANRI-READ-SEC                      
           IF      FLG-SYSKANRI        =   ZERO               
               MOVE  MCPDATA-REC           TO  SYS-5001-REC     
      *        病棟番号・種別
               MOVE    RECE82-SRYYM        TO  WRK-SRYYYMM
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   IF      RECE82-BYOTONUM(IDX3)   =  SPACE
                       MOVE    IDX3            TO  WRK-SRYDD
                       IF       PTNYUINRRK-TENNYUYMD  <=  WRK-SRYYMD
                       AND      PTNYUINRRK-TENSTUYMD  >=  WRK-SRYYMD
                           MOVE  SYS-5001-BTU-NUM
                                           TO  RECE82-BYOTONUM    (IDX3)
                           MOVE  SYS-5001-BTU-SBT
                                           TO  RECE82-BYOTOSYUBETU(IDX3)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .                                                           
       210112-BYOTOKBN-SET-EXT.                                               
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
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE82-FILE
                   RECE88-FILE
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
           DISPLAY "RECE82  CNT-IN  :" CNT-IN
           DISPLAY "RECE88  CNT-OUT :" CNT-RECE88
           DISPLAY "RECE82  CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0820 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ファイルＲＥＡＤ処理
      *****************************************************************
       900-RECE82-READ-SEC            SECTION.
      *
           READ    RECE82-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
           END-READ
           .
       900-RECE82-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴読込
      *****************************************************************
       900-RECE88-READ-SEC            SECTION.
      *
           READ    RECE88-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE88
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE88
           END-READ
           .
       900-RECE88-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２３）
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
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
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
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
       900-SYSKANRI-READ-EXT.
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
