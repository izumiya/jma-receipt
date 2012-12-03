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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSPRTCTRL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : クライアント印刷制御情報組み立て
      *  コンポーネント名  : GROUP & UUID 作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  PRT-OUT-FILE    ASSIGN  IN-ASS
                                   FILE    STATUS  IS  STS-OUT.
       DATA                    DIVISION.
       FILE                        SECTION.
       FD  PRT-OUT-FILE.
       01  PRT-OUT-REC.
           03  PRT-OUT-PRTDATA     PIC X(512).
       WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-PRTCTRL             PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(04).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(04). 
           03  IDY                     PIC 9(04). 
      *
       01  IN-ASS.
           03  FILLER              PIC X(512).
       01  STS-AREA.
           03  STS-OUT             PIC X(02).
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).  
               05  WRK-SYSDD           PIC 9(02).  
           03  WRK-FILE-TYPE           PIC X(05).
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *
           COPY    "ORCA-SYSTEM".
           COPY    "MCPDATA.INC".
      **************************************************************************
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    印刷制御マスタ
       01  PRTCTRL-REC.
           COPY    "CPPRTCTRL.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRTCTRL.INC".
           COPY    "MCPAREA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA
               ORCSPRTCTRLAREA
               MCPAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *???
      *    DISPLAY "ORCSPRTCTRL ST ="
      *???     
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       INDEX-AREA
                                       WRK-LNK-AREA
      *

      *
           PERFORM 100-AGENTGET-SEC
           IF      ORCSPRTCTRL-MODE    = "C"
               PERFORM 100-CANCEL-SEC
           ELSE
               PERFORM 100-APIADD-SEC
             IF    ORCSPRTCTRL-BATCH-FLG NOT = 1
               MOVE    1               TO  SPA-CLIENT-PRT-FLG
             END-IF
           END-IF
      *
      *
      *    DISPLAY "ORCSPRTCTRL ED ="
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    端末種別取得処理
      *****************************************************************
       100-AGENTGET-SEC            SECTION.
      *    AGENT 取得
           INITIALIZE                      ORCA-SYSTEM
           MOVE    "GETVALUE"          TO  MCP-FUNC
           MOVE    MCP-TERM            TO  ORCA-SYSTEM-ID
           MOVE    10                  TO  ORCA-SYSTEM-NUM
           MOVE    "agent"             TO  ORCA-SYSTEM-KEY(1)
           MOVE    ORCA-SYSTEM         TO  MCPDATA-REC
           MOVE    "system"            TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE    MCPDATA-REC         TO  ORCA-SYSTEM
           ELSE
               INITIALIZE                      ORCA-SYSTEM
           END-IF
      *
           DISPLAY "ORCA-SYSTEM-VALUE(1)(1:9) = " 
                      ORCA-SYSTEM-VALUE(1)(1:9)
      *
           IF    ORCA-SYSTEM-VALUE(1)(1:9) = "glclient/"
                 MOVE  ".ps"           TO  WRK-FILE-TYPE
           ELSE
                 MOVE  ".pdf"          TO  WRK-FILE-TYPE
           END-IF
      *
           .
       100-AGENTGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIキャンセル情報設定処理
      *****************************************************************
       100-CANCEL-SEC              SECTION.
      *    
           IF    ORCSPRTCTRL-BATCH-FLG NOT = 1
             MOVE    2                   TO  SPA-CLIENT-PRT-FLG
           END-IF
           MOVE    SPACE                 TO  IN-ASS
           STRING  "/tmp/"             DELIMITED   BY  SIZE
                   SPA-HOSPNUM         DELIMITED   BY  SPACE
                   "ONLINE"            DELIMITED   BY  SIZE
                   "0001"              DELIMITED   BY  SIZE
                   SPA-PRT-GROUP       DELIMITED   BY  SPACE
                   "0099"              DELIMITED   BY  SIZE
                   "99999"             DELIMITED   BY  SIZE
                   "00001"             DELIMITED   BY  SIZE
                   SPA-PRT-UUID        DELIMITED   BY  SPACE
                   ".api"              DELIMITED   BY  SPACE
                 INTO    IN-ASS
           OPEN OUTPUT             PRT-OUT-FILE
           MOVE    "DUMMY"             TO  PRT-OUT-PRTDATA
           WRITE    PRT-OUT-REC
           CLOSE    PRT-OUT-FILE
      *    
           INITIALIZE        PRTCTRL-REC
           MOVE    SPA-HOSPNUM         TO  PRTCTRL-HOSPNUM
           MOVE    SPA-TERMID          TO  PRTCTRL-TERMID
           MOVE    "ONLINE"            TO  PRTCTRL-TBL-KEY
           MOVE    1                   TO  PRTCTRL-RENNUM
           MOVE    SPA-PRT-GROUP       TO  PRTCTRL-TBL-GROUP
           MOVE    SPA-PRT-UUID        TO  PRTCTRL-TBL-UUID
           MOVE    1                   TO  PRTCTRL-DATA-KBN
           MOVE    99                  TO  PRTCTRL-SHORI-RENNUM
           MOVE    1                   TO  PRTCTRL-PAGE
      *
           PERFORM 900-PRTCTRL-INSERT-SEC
      *
           .
       100-CANCEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    API情報設定処理
      *****************************************************************
       100-APIADD-SEC              SECTION.
      *
           IF      ORCSPRTCTRL-BATCH-FLG = 1
               PERFORM 1001-BATCH-SEC
           ELSE
               PERFORM 1001-ONLINE-SEC
           END-IF
           .
       100-APIADD-EXT.
           EXIT.
      *
      *****************************************************************
      *     バッチ設定
      *****************************************************************
       1001-BATCH-SEC              SECTION.
      *
      * バッチの場合は、GROUP id は、親側で設定
      * UIDは、使わない
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SRYYM
           MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *    
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF  ORCSPRTCTRL-TITLE-I(IDX)  =  SPACE
                 COMPUTE IDY           = IDX - 1
                 MOVE    SPACE     TO  ORCSPRTCTRL-PATH-O(IDX)
                 STRING  "orca00/print?filename="
                                           DELIMITED   BY  SIZE
                         SPA-HOSPNUM       DELIMITED   BY  SPACE
                         ORCSPRTCTRL-BATCH-FLG
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-TBL-KEY(IDY)
                                           DELIMITED   BY  SPACE
                         "9999"
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-TBL-GROUP(IDY)
                                           DELIMITED   BY  SPACE
                         "0099" 
                                           DELIMITED   BY  SIZE
                         "99999"
                                           DELIMITED   BY  SIZE
                         "00001"
                                           DELIMITED   BY  SIZE
                         WRK-FILE-TYPE
                                           DELIMITED   BY  SPACE
                 INTO    ORCSPRTCTRL-PATH-O(IDX)
                 MOVE    "印刷ダミー"      TO
                                       ORCSPRTCTRL-TITLE-O(IDX)
                 MOVE    SPA-CLIENT-SHOWDIALOG   TO
                                       ORCSPRTCTRL-SHOWDIALOG(IDX)
                 MOVE    100               TO
                                       ORCSPRTCTRL-NRETRY(IDX)
      *
                   MOVE  11            TO  IDX
               ELSE
                 MOVE    SPACE     TO  ORCSPRTCTRL-PATH-O(IDX)
                 STRING  "orca00/print?filename="
                                           DELIMITED   BY  SIZE
                         SPA-HOSPNUM       DELIMITED   BY  SPACE
                         ORCSPRTCTRL-BATCH-FLG
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-TBL-KEY(IDX)
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-RENNUM(IDX)
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-TBL-GROUP(IDX)
                                           DELIMITED   BY  SPACE
                         IDX    
                                           DELIMITED   BY  SPACE
                         ORCSPRTCTRL-EDPAGE(IDX)
                                           DELIMITED   BY  SIZE
                         ORCSPRTCTRL-STPAGE(IDX)
                                           DELIMITED   BY  SIZE
                         WRK-FILE-TYPE
                                           DELIMITED   BY  SPACE
                 INTO    ORCSPRTCTRL-PATH-O(IDX)
                 MOVE    ORCSPRTCTRL-TITLE-I(IDX)      TO
                                       ORCSPRTCTRL-TITLE-O(IDX)
                 MOVE    SPA-CLIENT-SHOWDIALOG   TO
                                       ORCSPRTCTRL-SHOWDIALOG(IDX)
                 MOVE    100               TO
                                       ORCSPRTCTRL-NRETRY(IDX)
               END-IF
           END-PERFORM
      *
           .
       1001-BATCH-EXT.
           EXIT.
      *
      *****************************************************************
      *     オンライン設定
      *****************************************************************
       1001-ONLINE-SEC             SECTION.
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    SPACE               TO  SPA-PRT-GROUP            
           MOVE    WRK-SYSYMD          TO
                                   SPA-PRT-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   SPA-PRT-GROUP (9:)
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
           MOVE    "ONLINE"        TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SPA-PRT-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SRYYM
           MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *    
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF  ORCSPRTCTRL-TITLE-I(IDX)  =  SPACE
                 MOVE    SPACE     TO  ORCSPRTCTRL-PATH-O(IDX)
                 STRING  "orca00/print?filename="
                                           DELIMITED   BY  SIZE
                         SPA-HOSPNUM       DELIMITED   BY  SPACE
                         ORCSPRTCTRL-BATCH-FLG
                                           DELIMITED   BY  SPACE
                         "ONLINE"
                                           DELIMITED   BY  SIZE
                         LNK-PRTKANRI-RENNUM
                                           DELIMITED   BY  SPACE
                         SPA-PRT-GROUP     DELIMITED   BY  SPACE
                         "0099" 
                                           DELIMITED   BY  SIZE
                         "99999"
                                           DELIMITED   BY  SIZE
                         "00001"
                                           DELIMITED   BY  SIZE
                         SPA-PRT-UUID      DELIMITED   BY  SPACE
                         WRK-FILE-TYPE
                                           DELIMITED   BY  SPACE
                 INTO    ORCSPRTCTRL-PATH-O(IDX)
                 MOVE    "印刷ダミー"      TO
                                       ORCSPRTCTRL-TITLE-O(IDX)
                 MOVE    SPA-CLIENT-SHOWDIALOG   TO
                                       ORCSPRTCTRL-SHOWDIALOG(IDX)
                 MOVE    100               TO
                                       ORCSPRTCTRL-NRETRY(IDX)
      *
                   MOVE  11            TO  IDX
               ELSE
                 MOVE    SPACE     TO  ORCSPRTCTRL-PATH-O(IDX)
                 STRING  "orca00/print?filename="
                                           DELIMITED   BY  SIZE
                         SPA-HOSPNUM       DELIMITED   BY  SPACE
                         ORCSPRTCTRL-BATCH-FLG
                                           DELIMITED   BY  SPACE
                         "ONLINE"
                                           DELIMITED   BY  SIZE
                         LNK-PRTKANRI-RENNUM
                                           DELIMITED   BY  SPACE
                         SPA-PRT-GROUP     DELIMITED   BY  SPACE
                         IDX    
                                           DELIMITED   BY  SPACE
                         "99999"
                                           DELIMITED   BY  SIZE
                         "00001"
                                           DELIMITED   BY  SIZE
                         SPA-PRT-UUID      DELIMITED   BY  SPACE
                         WRK-FILE-TYPE
                                           DELIMITED   BY  SPACE
                 INTO    ORCSPRTCTRL-PATH-O(IDX)
                 MOVE    ORCSPRTCTRL-TITLE-I(IDX)      TO
                                       ORCSPRTCTRL-TITLE-O(IDX)
                 MOVE    SPA-CLIENT-SHOWDIALOG   TO
                                       ORCSPRTCTRL-SHOWDIALOG(IDX)
                 MOVE    100               TO
                                       ORCSPRTCTRL-NRETRY(IDX)
               END-IF
           END-PERFORM
      *
           .
       1001-ONLINE-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ書き込み
      *****************************************************************
       900-PRTCTRL-INSERT-SEC            SECTION.
      *
           MOVE    "tbl_prtctrl"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    PRTCTRL-REC         TO  MCPDATA-REC
           PERFORM 900-DBINSERT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTCTRL-REC
               MOVE    ZERO                TO  FLG-PRTCTRL
           ELSE
               MOVE    1                   TO  FLG-PRTCTRL
           END-IF
           .
       900-PRTCTRL-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル追加処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBINSERT-EXT.
           EXIT.

