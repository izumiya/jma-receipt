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
       PROGRAM-ID.             ORCSDTCHK01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : チェックマスタ−患者病名チェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/02    NACL−太田    新規作成
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-QDTCHK                  PIC 9(01).
           03  FLG-CHKMST-BYOMEI           PIC 9(01).
           03  FLG-MATCH                   PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-LEN                     PIC 9(05).
           03  IDX-ST                      PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-PATH.
               05  WRK-MCP-BLOCKS          PIC S9(9)   BINARY.
               05  WRK-MCP-RNAME           PIC S9(9)   BINARY.
               05  WRK-MCP-PNAME           PIC S9(9)   BINARY.
           03  WRK-STR-LEN                 PIC 9(05).
           03  WRK-STR                     PIC X(500).
           03  WRK-BYOMEICD-G.
               05  WRK-BYOMEICD-OCC        OCCURS  21.
                   07  WRK-BYOMEICD        PIC X(07).
                   07  WRK-BYOMEICD-SEP    PIC X(01).
           03  WRK-CHKMST-KEY              PIC X(200).
           03  WRK-PTBYOMEI-KEY            PIC X(200).
      *
       01  LEN-AREA.
           03  LEN-CHKMST                  PIC 9(05).
           03  LEN-PTBYOMEI                PIC 9(05).
      *
      *    病名退避ワーク
       01  TBYOMEI-AREA.
           03  TBYOMEI-IDX1                PIC 9(05).
           03  TBYOMEI-IDX2                PIC 9(05).
           03  TBYOMEI-STIDX               PIC 9(05).
           03  TBYOMEI-MAX                 PIC 9(05).
           03  TBYOMEI-OCC                 OCCURS 101.
               05  TBYOMEI-BYOMEI          PIC X(160).
               05  TBYOMEI-KEY.
                   07  TBYOMEI-ERRFLG      PIC 9(01).
                   07  TBYOMEI-SRYYMD      PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-TBYOMEI-MAX           PIC 9(05)   VALUE   100.
           03  CONST-STRLEN-MAX            PIC 9(05)   VALUE   200.
           03  CONST-MICODE                PIC X(07)   VALUE "0000999".
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    データチェッククエリー
       01  QDTCHK-REC.
           COPY    "CPQDTCHK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSDTCHK01.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
       SDTCHK01AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       LEN-AREA
                                       TBYOMEI-AREA
      *
           MOVE    ZERO            TO  FLG-CHKMST-BYOMEI
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    PATH-QUERY-DTCHK-KEY2   TO  WRK-MCP-PATH
           IF    ( SDTCHK01-CHKPTN (1)     =   "1" )
               IF    ( SDTCHK01-CHKPTN (2)     =   " " )
                   IF    ( SDTCHK01-CHKPTN (3) =   " " )
                     AND ( SDTCHK01-CHKPTN (4) =   " " )
                     AND ( SDTCHK01-CHKPTN (5) =   " " )
      *
                       MOVE    PATH-QUERY-DTCHK-KEY3
                                           TO  WRK-MCP-PATH
      *
                   ELSE
                       MOVE    PATH-QUERY-DTCHK-KEY2
                                           TO  WRK-MCP-PATH
                   END-IF
               ELSE
                       MOVE    PATH-QUERY-DTCHK-KEY
                                           TO  WRK-MCP-PATH
               END-IF
           ELSE
               MOVE    PATH-QUERY-DTCHK-KEY4
                                           TO  WRK-MCP-PATH
           END-IF
      *
           PERFORM 900-QDTCHK-KEY-SEL-SEC
      *
           PERFORM UNTIL ( FLG-QDTCHK      NOT =   ZERO )
                    OR   ( FLG-CHKMST-BYOMEI   =   1    )
      *
               PERFORM 2001-BYOMEI-CHK-SEC
               IF    ( FLG-MATCH    =  1 )
                   PERFORM 2001-TAISYO-CHK-SEC
                   IF    ( FLG-TAISYO  =   1 )
                       MOVE    1           TO  FLG-CHKMST-BYOMEI
                   ELSE
                       PERFORM 2001-ERRINF-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-CHKMST-BYOMEI   NOT =   1    )
                   PERFORM 900-QDTCHK-KEY-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    PATH-QUERY-DTCHK-KEY2   TO  MCP-PATH
           PERFORM 910-DBCLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       2001-BYOMEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-MATCH
      *
           IF    ( QDTCHK-CD               NOT =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)     NOT =   CONST-MICODE )
      *
                IF   ( SDTCHK01-CHKPTN (1)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
      *
                       PERFORM 20011-BYOMEICDKEY-HEN-SEC
                       MOVE    QDTCHK-CD       TO  WRK-CHKMST-KEY
                       MOVE    WRK-BYOMEICD-G  TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
                IF   ( SDTCHK01-CHKPTN (2)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD               NOT =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)         =   CONST-MICODE )
      *
                IF   ( SDTCHK01-CHKPTN (3)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD                   =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)     NOT =   CONST-MICODE )
      *
                IF   ( SDTCHK01-CHKPTN (4)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           IF    ( QDTCHK-CD                   =   CONST-MICODE )
            AND  ( QDTCHK-BYOMEICD (1)         =   CONST-MICODE )
      *
                IF   ( SDTCHK01-CHKPTN (5)     =   1 )
                    IF   ( FLG-MATCH           =   ZERO )
                       MOVE    QDTCHK-BYOMEI   TO  WRK-CHKMST-KEY
                       MOVE    QDTCHK-PTBYOMEI TO  WRK-PTBYOMEI-KEY
                       PERFORM 20011-TOTUGO-SEC
                    END-IF
                END-IF
      *
           END-IF
      *
           .
       2001-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードKEY編集処理
      *****************************************************************
       20011-BYOMEICDKEY-HEN-SEC      SECTION.
      *
           INITIALIZE                  WRK-BYOMEICD-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                    OR   ( QDTCHK-BYOMEICD (IDX0)  =   SPACE )
      *
               MOVE    QDTCHK-BYOMEICD (IDX0)
                                   TO  WRK-BYOMEICD (IDX0)
      *
               IF    ( IDX0        >   1 )
                   MOVE    "-"     TO  WRK-BYOMEICD-SEP (IDX0 - 1)
               END-IF
      *
           END-PERFORM
      *
           .
       20011-BYOMEICDKEY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    突合処理
      *****************************************************************
       20011-TOTUGO-SEC                SECTION.
      *
           EVALUATE    QDTCHK-COMPKBN
           WHEN    "2"
               IF    ( WRK-CHKMST-KEY  =   WRK-PTBYOMEI-KEY )
                       MOVE    1   TO  FLG-MATCH
               END-IF
           WHEN    "3"
           WHEN    "4"
      *
               MOVE    WRK-CHKMST-KEY      TO  WRK-STR
               PERFORM 800-STRLEN-SEC
               MOVE    WRK-STR-LEN         TO  LEN-CHKMST
      *
               MOVE    WRK-PTBYOMEI-KEY    TO  WRK-STR
               PERFORM 800-STRLEN-SEC
               MOVE    WRK-STR-LEN         TO  LEN-PTBYOMEI
      *
               IF    ( LEN-PTBYOMEI   >=   LEN-CHKMST )
                   IF    ( QDTCHK-COMPKBN  =   "3" )
                       IF    ( WRK-PTBYOMEI-KEY (1:LEN-CHKMST)
                                           =   WRK-CHKMST-KEY )
                           MOVE    1   TO  FLG-MATCH
                       END-IF
                   ELSE
                       COMPUTE IDX-ST
                           =   LEN-PTBYOMEI
                           -   LEN-CHKMST
                           +   1
                       IF    ( WRK-PTBYOMEI-KEY (IDX-ST :)
                                           =   WRK-CHKMST-KEY )
                           MOVE    1   TO  FLG-MATCH
                       END-IF
                   END-IF
               END-IF
           WHEN    OTHER
               MOVE    1           TO  FLG-MATCH
           END-EVALUATE
      *
           .
       20011-TOTUGO-EXT.
           EXIT.
      *****************************************************************
      *    病名対象チェック
      *****************************************************************
       2001-TAISYO-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-TAISYO
      *
           IF    ( QDTCHK-SRYYMD (1:6)        <=   SDTCHK01-KJNYMD(1:6))
               IF    ( QDTCHK-TENKIKBN         =   SPACE )
                   IF    ( QDTCHK-NYUGAIKBN    =   SPACE )
                    OR   ( QDTCHK-NYUGAIKBN    =   SDTCHK01-NYUGAIKBN )
                           MOVE    1       TO  FLG-TAISYO
                   END-IF
               ELSE
                   IF    ( QDTCHK-TENKIYMD (1:6)
                                              >=   SDTCHK01-KJNYMD(1:6))
                       IF    ( QDTCHK-NYUGAIKBN
                                               =   SPACE )
                        OR   ( QDTCHK-NYUGAIKBN
                                               =   SDTCHK01-NYUGAIKBN )
                           MOVE    1       TO  FLG-TAISYO
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラー情報編集処理
      *****************************************************************
       2001-ERRINF-SEC                 SECTION.
      *
           MOVE    ZERO            TO  TBYOMEI-IDX1
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1            >   TBYOMEI-MAX )
                    OR   ( TBYOMEI-IDX1    >   ZERO    )
      *
               IF    ( TBYOMEI-BYOMEI (IDX1)   =   QDTCHK-BYOMEI )
                   MOVE    IDX1    TO  TBYOMEI-IDX1
               END-IF
      *
           END-PERFORM
      *
           IF    ( TBYOMEI-IDX1        =   ZERO )
               IF    ( TBYOMEI-MAX    >=   CONST-TBYOMEI-MAX )
                   GO  TO  2001-ERRINF-EXT
               END-IF
      *
               COMPUTE TBYOMEI-MAX
                   =   TBYOMEI-MAX     +   1
      *
               MOVE    QDTCHK-BYOMEI   TO  TBYOMEI-BYOMEI (TBYOMEI-MAX)
               MOVE    "00000000"      TO  TBYOMEI-SRYYMD (TBYOMEI-MAX)
      *
               MOVE    TBYOMEI-MAX     TO  TBYOMEI-IDX1
      *
           END-IF
      *
      *    適用期間が正しい場合、入外区分の誤りとして
      *    2"を、適用期間が誤っている場合は適用期間の誤りとして
      *    "1"を設定
           IF    ( QDTCHK-TENKIKBN      =   SPACE )
               IF    ( QDTCHK-SRYYMD (1:6) <=   SDTCHK01-KJNYMD(1:6))
                       MOVE    2   TO  TBYOMEI-ERRFLG (TBYOMEI-IDX1)
               ELSE
                   IF    ( TBYOMEI-ERRFLG (TBYOMEI-IDX1)  <=   1 )
                       MOVE    1   TO  TBYOMEI-ERRFLG (TBYOMEI-IDX1)
                   END-IF
               END-IF
           ELSE
               IF    ( QDTCHK-SRYYMD   (1:6)
                                           <=  SDTCHK01-KJNYMD(1:6))
                AND  ( QDTCHK-TENKIYMD (1:6)
                                           >=  SDTCHK01-KJNYMD(1:6))
                       MOVE    2   TO  TBYOMEI-ERRFLG (TBYOMEI-IDX1)
               ELSE
                   IF    ( TBYOMEI-ERRFLG (TBYOMEI-IDX1)  <=   1 )
                       MOVE    1   TO  TBYOMEI-ERRFLG (TBYOMEI-IDX1)
                   END-IF
               END-IF
           END-IF
      *
           IF    ( QDTCHK-SRYYMD   >   TBYOMEI-SRYYMD  (TBYOMEI-IDX1))
               MOVE    QDTCHK-SRYYMD
                                   TO  TBYOMEI-SRYYMD  (TBYOMEI-IDX1)
           END-IF
      *
           .
       2001-ERRINF-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           INITIALIZE                      SDTCHK01-OT
           MOVE    FLG-CHKMST-BYOMEI   TO  SDTCHK01-O-OK
      *
           IF    ( SDTCHK01-O-OK       =   ZERO )
               PERFORM 3001-TBYOMEI-SORT-SEC
           END-IF
      *
           MOVE    TBYOMEI-MAX         TO  SDTCHK01-O-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TBYOMEI-MAX )
      *
               MOVE    TBYOMEI-ERRFLG (IDX0)
                                       TO  SDTCHK01-O-ERRFLG (IDX0)
               MOVE    TBYOMEI-SRYYMD (IDX0)
                                       TO  SDTCHK01-O-SRYYMD (IDX0)
               MOVE    TBYOMEI-BYOMEI (IDX0)
                                       TO  SDTCHK01-O-BYOMEI (IDX0)
      *
           END-PERFORM
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ＳＯＲＴ処理
      *****************************************************************
       3001-TBYOMEI-SORT-SEC           SECTION.
      *
      *    ソート用ワークについて並び替えを行う
           PERFORM VARYING TBYOMEI-IDX1    FROM    2   BY  1
                   UNTIL ( TBYOMEI-IDX1    >   TBYOMEI-MAX )
               COMPUTE TBYOMEI-STIDX       =   TBYOMEI-IDX1    -   1
               MOVE    TBYOMEI-OCC (TBYOMEI-IDX1)
                                   TO  TBYOMEI-OCC
                                            (CONST-TBYOMEI-MAX + 1)
               PERFORM VARYING TBYOMEI-IDX2
                       FROM    TBYOMEI-STIDX   BY  -1
                       UNTIL ( TBYOMEI-IDX2    <   1 )
                          OR ( TBYOMEI-KEY (CONST-TBYOMEI-MAX + 1)
                                   <=  TBYOMEI-KEY (TBYOMEI-IDX2) )
                       MOVE    TBYOMEI-OCC (TBYOMEI-IDX2)
                           TO  TBYOMEI-OCC (TBYOMEI-IDX2 + 1)
               END-PERFORM
               MOVE    TBYOMEI-OCC (CONST-TBYOMEI-MAX + 1)
                           TO  TBYOMEI-OCC (TBYOMEI-IDX2 + 1)
           END-PERFORM
      *
           .
       3001-TBYOMEI-SORT-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       800-STRLEN-SEC                  SECTION.
      *
           MOVE    ZERO    TO          WRK-STR-LEN
      *
           PERFORM VARYING IDX-LEN FROM    1   BY  1
                   UNTIL ( IDX-LEN             >   CONST-STRLEN-MAX )
                    OR   ( WRK-STR (IDX-LEN :) =   SPACE )
      *
               COMPUTE    WRK-STR-LEN  =   WRK-STR-LEN +   1
      *
           END-PERFORM
      *
           .
       800-STRLEN-EXT.
           EXIT.
      *****************************************************************
      *    データチェッククエリ検索処理(KEY)
      *****************************************************************
       900-QDTCHK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-QDTCHK
      *
           INITIALIZE                      QDTCHK-REC
      *
           MOVE    SDTCHK01-HOSPID     TO  QDTCHK-SELHOSPID
           MOVE    SDTCHK01-PTID       TO  QDTCHK-SELPTID
           MOVE    SDTCHK01-SRYKA      TO  QDTCHK-SELSRYKA
           MOVE    SDTCHK01-CHKKBN     TO  QDTCHK-SELCHKKBN
           MOVE    SDTCHK01-SRYCD      TO  QDTCHK-SELSRYCD
           MOVE    SDTCHK01-KJNYMD     TO  QDTCHK-SELYUKOSTYMD
                                           QDTCHK-SELYUKOEDYMD
           MOVE    SDTCHK01-CDKBN      TO  QDTCHK-SELCDKBN
      *
           MOVE    QDTCHK-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-PATH        TO  MCP-PATH
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  QDTCHK-REC
           ELSE
               INITIALIZE                  QDTCHK-REC
               MOVE    1               TO  FLG-QDTCHK
           END-IF
      *
           .
       900-QDTCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェッククエリFETCH処理(KEY)
      *****************************************************************
       900-QDTCHK-KEY-FET-SEC          SECTION.
      *
           MOVE    WRK-MCP-PATH        TO  MCP-PATH
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  QDTCHK-REC
           ELSE
               INITIALIZE                  QDTCHK-REC
               MOVE    1               TO  FLG-QDTCHK
           END-IF
      *
           .
       900-QDTCHK-KEY-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
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
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
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
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
      *    MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
      *    CALL    "MCPSUB"            USING
      *                                MCPAREA
      *                                MCPDATA-REC
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
