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
       PROGRAM-ID.             ORCSLDUSER.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : ユーザー一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/08/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDXT                PIC 9(05).
           03  IDXU                PIC 9(05).
      *
       01  SGYOUMU-CODE            PIC 9(03).
      *
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システムユーザ
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
           COPY    "ORCA-SYSTEM".
      *
           COPY    "ORCA-SYSTEM"   REPLACING  //ORCA-SYSTEM//
                                   BY         //TERMIDLST//.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSLDUSER.INC".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   SLDUSER-AREA
                                   SPA-AREA.
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
      *
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-LISTENTRY-SEC
      *
           MOVE    ZERO            TO  SLDUSER-OTHERS
           PERFORM VARYING IDXU    FROM    1   BY  1
                   UNTIL ( IDXU    >   SLDUSER-MAX )
      *
               IF    ( SLDUSER-HOSPNUM (IDXU)      =   SPA-HOSPNUM )
                AND  ( SLDUSER-LD      (IDXU)      =   SLDUSER-MY-LD )
                AND  ( SLDUSER-TERMID  (IDXU)  NOT =   MCP-TERM )
      *
                   COMPUTE SLDUSER-OTHERS  =  SLDUSER-OTHERS + 1
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    端末一覧取得処理
      *****************************************************************
       2001-LISTENTRY-SEC              SECTION.
      *
           INITIALIZE                  TERMIDLST
           MOVE    SPA-TERMID      TO  TERMIDLST-ID
           MOVE    TERMIDLST       TO  MCPDATA-REC
           MOVE    "LISTENTRY"     TO  MCP-FUNC
           MOVE    "system"        TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TERMIDLST
               MOVE    TERMIDLST-NUM   TO  SLDUSER-MAX
               PERFORM VARYING IDXT    FROM    1   BY  1
                       UNTIL ( IDXT    >   100 )
                        OR   ( IDXT    >   TERMIDLST-NUM )
                   PERFORM 20011-GETVALUE-SEC
               END-PERFORM
           END-IF
      *
           .
       2001-LISTENTRY-EXT.
           EXIT.
      *****************************************************************
      *    端末情報取得処理
      *****************************************************************
       20011-GETVALUE-SEC              SECTION.
      *
           INITIALIZE                  ORCA-SYSTEM
           MOVE    TERMIDLST-KEY (IDXT)
                                       TO  ORCA-SYSTEM-ID
           MOVE    10                  TO  ORCA-SYSTEM-NUM
           MOVE    "in_process"        TO  ORCA-SYSTEM-KEY (1)
           MOVE    "window"            TO  ORCA-SYSTEM-KEY (2)
           MOVE    "user"              TO  ORCA-SYSTEM-KEY (3)
           MOVE    "event"             TO  ORCA-SYSTEM-KEY (4)
           MOVE    "access_time"       TO  ORCA-SYSTEM-KEY (5)
           MOVE    "process_time"      TO  ORCA-SYSTEM-KEY (6)
           MOVE    "total_proc"        TO  ORCA-SYSTEM-KEY (7)
           MOVE    "host"              TO  ORCA-SYSTEM-KEY (8)
           MOVE    "widget"            TO  ORCA-SYSTEM-KEY (9)
           MOVE    "create_time"       TO  ORCA-SYSTEM-KEY (10)
           MOVE    ORCA-SYSTEM         TO  MCPDATA-REC
           MOVE    "GETVALUE"          TO  MCP-FUNC
           MOVE    "system"            TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF    ( MCP-RC          =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  ORCA-SYSTEM
      *
               MOVE    TERMIDLST-KEY   (IDXT)
                                       TO  SLDUSER-TERMID (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (1)
                                       TO  SLDUSER-IN-PROCESS (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (2)
                                       TO  SLDUSER-WINDOW (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (3)
                                       TO  SLDUSER-USER (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (4)
                                       TO  SLDUSER-EVENT (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (5)
                                       TO  SLDUSER-ACCESS-TIME (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (6)
                                       TO  SLDUSER-PROCESS-TIME (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (7)
                                       TO  SLDUSER-TOTAL-PROC (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (8)
                                       TO  SLDUSER-HOST (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (9)
                                       TO  SLDUSER-WIDGET (IDXT)
               MOVE    ORCA-SYSTEM-VALUE (10)
                                       TO  SLDUSER-CREATE-TIME (IDXT)
               PERFORM 900-SYSUSER-KEY-SEL-SEC
               MOVE    SYSUSER-HOSPNUM TO  SLDUSER-HOSPNUM (IDXT)
      *
               MOVE    SPA-AREA        TO  WSPA-AREA
               PERFORM 800-GYOUMUGET-SEC
               MOVE    WSPA-AREA       TO  SPA-AREA
               MOVE    SGYOUMU-CODE    TO  SLDUSER-LD (IDXT)
           END-IF
      *
           .
       20011-GETVALUE-EXT.
           EXIT.
      *****************************************************************
      *    業務取得処理
      *****************************************************************
       800-GYOUMUGET-SEC              SECTION.
      *
           MOVE    ZERO            TO  SGYOUMU-CODE
      *
           MOVE    SLDUSER-WINDOW (IDXT)
                                   TO  SPA-SAKIPG
      *
           IF    ( SPA-SAKIPG(1:1)     =   "X" )
               IF    (  SPA-SAKIPG(2:1)     IS  NUMERIC )
                  OR (  SPA-SAKIPG(1:4)     =   "XERR" )
                  OR (  SPA-SAKIPG(1:3)     =   "XID" )
                   CONTINUE
               ELSE
                   MOVE    "?"         TO  SPA-SAKIPG(1:1)
               END-IF
           END-IF
      *
           CALL    "ORCSGYOUMUGET"     USING
                                       SPA-AREA
                                       SGYOUMU-CODE
      *
           .
       800-GYOUMUGET-EXT.
           EXIT.
      *****************************************************************
      *    ユーザー環境検索処理
      *****************************************************************
       900-SYSUSER-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSUSER
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SLDUSER-USER (IDXT) TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
      *
           MOVE    "tbl_sysuser"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSUSER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
