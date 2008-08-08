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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCHCG016S02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 指定診療行為件数調サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  06/06/20    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/28  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ZAINUM                  PIC 9(08).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX-DAY                     PIC 9(02).
      *
       01  WRK-AREA.
           03  WRK-COMB-HKNCOMBINUM        PIC 9(04).
           03  WRK-HBTSRYKA-G.
               05  WRK-HBTSRYKA-HBTKA      PIC X(02)   OCCURS  31.
               05  WRK-HBTSRYKA-MAX        PIC 9(05).
               05  WRK-HBTSRYKA-OCC        OCCURS  100.
                   07  WRK-HBTSRYKA        PIC X(02).
                   07  WRK-HBTSRYKA-DAY    PIC 9(01)   OCCURS  31.
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                 PIC 9(03)   OCCURS  31.
      *
       01  TCOMBI-AREA.
           03  TCOMBI-MAX                  PIC 9(05).
           03  TCOMBI-HKNCOMBI-OCC         OCCURS  50.
               05  TCOMBI-HKNCOMBI         PIC 9(04).
               05  TCOMBI-HKNCOMBI-CNT     PIC 9(03).
               05  TCOMBI-HKNCOMBI-DAY     PIC 9(01)
                                           OCCURS  31.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-ZAINUM-MAX            PIC 9(05) VALUE  200.
           03  CONST-HBTSRYKA-MAX          PIC 9(05) VALUE  100.
           03  CONST-SRYCD-MAX             PIC 9(05) VALUE 20.
           03  CONST-TCOMBI-MAX            PIC 9(05) VALUE 50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院診療会計マスター
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCHCG016S02.INC".
       01  LNKNACCT-REC.
           COPY    "CPNYUINACCT.INC"
                                   REPLACING   //NACCT//
                                   BY          //LNKNACCT//.
           COPY    "COMMON-SPA".
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           HCG016S02-AREA
           LNKNACCT-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR         TO  HCG016S02-RC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       HCG016S02-OT-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    LNKNACCT-REC        TO  NYUINACCT-REC
      *
           PERFORM 2001-HBTSRYKA-HEN-SEC
      *
           PERFORM 2001-TCOMBI-HEN-SEC
      *
           PERFORM 2001-NACCT-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    日別診療科編集処理
      *****************************************************************
       2001-HBTSRYKA-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-HBTSRYKA-G
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               IF    ( PTNYUINRRK-TENNYUYMD (1:6)  <   NACCT-SRYYM )
                   MOVE    1           TO  WRK-STIDX
               ELSE
                   MOVE    PTNYUINRRK-TENNYUYMD (7:2)
                                       TO  WRK-STIDX
               END-IF
      *
               IF    ( PTNYUINRRK-TENSTUYMD (1:6)  >   NACCT-SRYYM )
                   MOVE    31          TO  WRK-EDIDX
               ELSE
                   MOVE    PTNYUINRRK-TENSTUYMD (7:2)
                                       TO  WRK-EDIDX
               END-IF
      *
               PERFORM VARYING IDX-DAY     FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX-DAY     >       WRK-EDIDX )
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-HBTSRYKA-HBTKA (IDX-DAY)
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( WRK-HBTSRYKA-HBTKA (IDX-DAY)    NOT =   ZERO )
      *
                   PERFORM VARYING IDX5    FROM    1   BY  1
                       UNTIL ( IDX5    >   WRK-HBTSRYKA-MAX )
                       OR    ( WRK-HBTSRYKA (IDX5)
                                       = WRK-HBTSRYKA-HBTKA (IDX-DAY) )
      *
                       CONTINUE
      *
                   END-PERFORM
      *
                   MOVE    ZERO        TO  IDX6
      *
                   IF    ( IDX5    >   WRK-HBTSRYKA-MAX )
      *
                       IF    ( WRK-HBTSRYKA-MAX 
                                               <   CONST-HBTSRYKA-MAX )
                           COMPUTE WRK-HBTSRYKA-MAX
                               =   WRK-HBTSRYKA-MAX    +   1
      *
                           MOVE    WRK-HBTSRYKA-MAX    TO  IDX6
      *
                           MOVE    WRK-HBTSRYKA-HBTKA (IDX-DAY)
                                       TO  WRK-HBTSRYKA (IDX6)
                       END-IF
                   ELSE
                       MOVE    IDX5    TO  IDX6
                   END-IF
      *
                   IF    ( IDX6        >   ZERO )
                       MOVE    1   TO  WRK-HBTSRYKA-DAY (IDX6 IDX-DAY)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *
           .
       2001-HBTSRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ情報編集処理
      *****************************************************************
       2001-TCOMBI-HEN-SEC             SECTION.
      *
           INITIALIZE                  TCOMBI-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   31 )
      *
               IF    ( NACCT-DAY (IDX1)    >   ZERO )
      *
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   TCOMBI-MAX )
                           OR    ( NACCT-DAY (IDX1)
                                           =   TCOMBI-HKNCOMBI (IDX2) )
      *
                       CONTINUE
      *
                   END-PERFORM
      *
                   IF    (     IDX2                >   TCOMBI-MAX )
                       IF    ( CONST-TCOMBI-MAX    >   TCOMBI-MAX )
                           COMPUTE TCOMBI-MAX  =   TCOMBI-MAX  +   1
                           MOVE    NACCT-DAY (IDX1)
                                           TO  TCOMBI-HKNCOMBI
                                                       (TCOMBI-MAX)
                           COMPUTE TCOMBI-HKNCOMBI-CNT (TCOMBI-MAX)
                               =   TCOMBI-HKNCOMBI-CNT (TCOMBI-MAX)
                               +   1
                           MOVE    1       TO  TCOMBI-HKNCOMBI-DAY
                                                       (TCOMBI-MAX IDX1)
                       END-IF
                   ELSE
                           COMPUTE TCOMBI-HKNCOMBI-CNT (IDX2)
                               =   TCOMBI-HKNCOMBI-CNT (IDX2)
                               +   1
                           MOVE    1       TO  TCOMBI-HKNCOMBI-DAY
                                                       (IDX2 IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-TCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報編集処理
      *****************************************************************
       2001-NACCT-EDIT-SEC             SECTION.
      *
           MOVE    ZERO                TO  CNT-ZAINUM
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   TCOMBI-MAX )
      *
               MOVE    TCOMBI-HKNCOMBI (IDX3)  TO  WRK-COMB-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               INITIALIZE                  HCM34S01-AREA
               MOVE    NACCT-HOSPNUM        TO  HCM34S01-HOSPNUM
               MOVE    NACCT-PTID          TO  HCM34S01-PTID
               MOVE    NACCT-SRYYM         TO  HCM34S01-SANTEIYM
               MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
               MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
      *
               IF    ( COMB-HKNNUM         =     "971" OR "973" )
      *
                   CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                     SPA-AREA
               ELSE
                   CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                     SPA-AREA
               END-IF
               MOVE    HCM34S01-RC         TO  FLG-ERR
      *
               PERFORM 20011-HCG016S02-OT-EDIT-SEC
      *
           END-PERFORM
      *
           .
       2001-NACCT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       20011-HCG016S02-OT-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   HCM34S01-HENKYAKU-MAX )
      *
               IF    ( HCM34S01-ZAISKBKBN (IDX4)   NOT =   "3" )
                AND  ( HCM34S01-ZAISKBKBN (IDX4)   NOT =   "5" )
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   WRK-HBTSRYKA-MAX )
      *
                       MOVE    ZERO    TO  WRK-DAY-G
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( WRK-HBTSRYKA-DAY (IDX7 IDX-DAY )
                                               > ZERO )
                            AND  ( HCM34S01-DAY     (IDX4 IDX-DAY )
                                               > ZERO )
                               MOVE    HCM34S01-DAY     (IDX4 IDX-DAY )
                                       TO  WRK-DAY (IDX-DAY)
                           END-IF
                       END-PERFORM
      *
                       IF    ( WRK-DAY-G   NOT =   ZERO )
                           IF    ( CONST-ZAINUM-MAX     >   CNT-ZAINUM )
                               PERFORM 200111-HCG016S02-OT-EDIT-SEC
                           ELSE
                               DISPLAY "NACCT ZAINUM OVER:" NACCT-PTID
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       20011-HCG016S02-OT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計情報追加処理
      *****************************************************************
       200111-HCG016S02-OT-EDIT-SEC    SECTION.
      *
           COMPUTE CNT-ZAINUM  =   CNT-ZAINUM  +   1
      *
           MOVE    CNT-ZAINUM  TO  HCG016S02-HENKYAKU-MAX
      *
           MOVE    WRK-HBTSRYKA    (IDX7)
                               TO  HCG016S02-SRYKA (CNT-ZAINUM)
      *
           MOVE    COMB-HKNCOMBINUM
                               TO  HCG016S02-HKNCOMBI
                                                   (CNT-ZAINUM)
           MOVE    HCM34S01-SRYKBN (IDX4)
                               TO  HCG016S02-SRYKBN
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SRYSYUKBN (IDX4)
                               TO  HCG016S02-SRYSYUKBN
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-ZAITEN (IDX4)
                               TO  HCG016S02-ZAITEN
                                                   (CNT-ZAINUM)
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               COMPUTE HCG016S02-ZAIKAISU (CNT-ZAINUM)
                   =   HCG016S02-ZAIKAISU (CNT-ZAINUM)
                   +   WRK-DAY (IDX-DAY)
      *
               MOVE    WRK-DAY (IDX-DAY)
                               TO  HCG016S02-DAY
                                          (CNT-ZAINUM IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    HCM34S01-MEISAISU (IDX4)
                               TO  HCG016S02-MEISAISU
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SRYCDTOTAL (IDX4)
                               TO  HCG016S02-SRYCDTOTAL
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-SURYOUTOTAL (IDX4)
                               TO  HCG016S02-SURYOUTOTAL
                                                   (CNT-ZAINUM)
      *
           MOVE    HCM34S01-RUIKEITOTAL (IDX4)
                               TO  HCG016S02-RUIKEITOTAL
                                                   (CNT-ZAINUM)
      *
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8    >   CONST-SRYCD-MAX )
                    OR   ( HCM34S01-SRYCD (IDX4 IDX8)
                                   =   SPACE )
      *
               MOVE    HCM34S01-SURYO (IDX4 IDX8)
                                   TO  HCG016S02-SURYO
                                                   (CNT-ZAINUM IDX8)
      *
               MOVE    HCM34S01-SRYCD (IDX4 IDX8)
                                   TO  HCG016S02-SRYCD
                                                   (CNT-ZAINUM IDX8)
      *
           END-PERFORM
      *
           .
       200111-HCG016S02-OT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
      *
           MOVE    NACCT-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    NACCT-PTID          TO  PTNYUINRRK-PTID
           MOVE    NACCT-SRYYM         TO  PTNYUINRRK-TENNYUYMD
           MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:2)
           MOVE    NACCT-SRYYM         TO  PTNYUINRRK-TENSTUYMD
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理(KEY41)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                    TO  FLG-HKNCOMBI
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    NACCT-HOSPNUM           TO  COMB-HOSPNUM
           MOVE    NACCT-PTID              TO  COMB-PTID
           MOVE    WRK-COMB-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
