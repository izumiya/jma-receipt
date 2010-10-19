      ******************************************************************
      * ORCA -- Online Receipt Computer Advantage
      *
      * Copyright (C)  2000-2001 JMA.         
      *
      * This module is part of ORCA.
      *
      *     ORCA is distributed in the hope that it will be useful, but
      * WITHOUT ANY WARRANTY.  No author or distributor accepts 
      * responsibility to anyone for the consequences of using it or for
      * whether it serves any particular purpose or works at all, unless
      * he says so in writing.
      * Refer to the GNU General Public License for full details. 
      *
      *     Everyone is granted permission to copy, modify and
      * redistribute ORCA, but only under the conditions described in
      * the GNU General Public License.  A copy of this license is
      * supposed to have been given to you along with ORCA so you can
      * know your rights and responsibilities.  It should be in a file
      * named COPYING.  Among other things, the copyright notice and 
      * this notice must be preserved on all copies. 
      ******************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0030.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/05/10  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  LF04-FILE       ASSIGN  FILE-NAME
                                   FILE    STATUS  IS  STS-LF04.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  LF04-FILE.
       01  IN-LF04-REC             PIC X(30000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-NAME.
      *    03                      PIC X(14)  VALUE  "/var/tmp/claim". 
      *    03  FILE-HOSPNUM        PIC 9(02).   
           03  FILE-PTID           PIC X(80).   
      *    03                      PIC X(06)  VALUE  ".txt".
      *
       01  WRK-DATA-REC.
           03  WRK-DATA            PIC X(30000)
                                   OCCURS  50.
      *                            OCCURS  6.
      *
       01  LF04-REC.        
           COPY    "CPRLF004.INC".
      *     
018000*    フラグ領域
       01  STS-AREA.
           03  STS-LF04            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-PTID    PIC  X(80).
               05  WRK-PARA-SRYKA   PIC  X(02).
               05  WRK-PARA-SRYYMD  PIC  X(08).
               05  WRK-PARA-HKNCOMBINUM
                                    PIC  X(04). 
               05  WRK-PARA-HOSPNUM PIC  9(02).
      *
           03  WRK-IN               PIC X(01).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *
       01  WRK-LF04-REC. 
           COPY    "CPRLF004.INC"    REPLACING  //LF04//
                                     BY         //WRK-LF04//.
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      
      *
      *    claim連携リターンエリア
       01  WRK-ERROR-AREA.
           03  WRK-KBN-CODE            PIC X(01).
           03  WRK-ERR-CODE            PIC 9(02).
           03  WRK-ERR-MSG             PIC X(100).
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
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
      *PROCEDURE                   DIVISION.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *            UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  WRK-ERROR-AREA
           MOVE    SPACE               TO  WRK-DATA-REC 
                                           WRK-LF04-REC
      *
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    COMMAND-PARAM       TO  WRK-PARA
      *
      *???
           DISPLAY "PARA " WRK-PARA "##"
      *???
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM 
           MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           MOVE    WRK-PARA-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM 
           MOVE    "2"                 TO  SPA-NYUGAIKBN 
      *     
           MOVE    WRK-PARA-PTID       TO  FILE-PTID
           OPEN    INPUT    LF04-FILE
           MOVE    ZERO                TO  IDX
      *     
      ************
           PERFORM UNTIL    FLG-END    =   1
               READ                    LF04-FILE
                   AT  END
                       MOVE    1           TO  FLG-END
                   NOT AT  END
                      ADD     1           TO  IDX
                      IF      IDX         >   100
                           CONTINUE
                       ELSE
           DISPLAY "L0030 IDX = " "[" IDX "]"
                           MOVE    IN-LF04-REC   TO  WRK-DATA
                                                       (IDX)
                       END-IF
               END-READ
      ************
      *        MOVE    WRK-DATA (IDX)      TO  OUT-LF04-REC
      *        WRITE                       OUT-LF04-REC
      *        IF      STS-LF04        NOT =   ZERO
      *            DISPLAY "*** ORCL0030  ERR="   STS-LF04
      *            ACCEPT  WRK-IN
      *            MOVE    10             TO  IDX
      *        END-IF
           END-PERFORM
           MOVE    WRK-DATA-REC           TO  WRK-LF04-REC
           DISPLAY "L0030 WRK-LF04-CRE001-DPERTID = " 
                  "[" WRK-LF04-CRE001-DPERTID "]" 
           IF      WRK-LF04-IJICD(18 1)   =  LOW-VALUE
                DISPLAY "L0030 LOW-VALUE(18 1) = " 
           END-IF
           CLOSE   LF04-FILE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           CALL    "ORCL0031"          USING   WRK-LF04-REC
                                               WRK-ERROR-AREA
      *
      *    MOVE    WRK-LF04-REC        TO  LF04-REC
      *     
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCL0030 END ***"
      *    ACCEPT  WRK-IN
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
