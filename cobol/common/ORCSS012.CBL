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
       PROGRAM-ID.                     ORCSS012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : チェックマスタ病名編集サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/01/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-HAISHI                  PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDXI                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-IKOCD                   PIC X(07).
           03  WRK-TBYOMEICD-G.
               05  WRK-TBYOMEICD-OCC       OCCURS  21.
                   07  WRK-TBYOMEICD       PIC X(07).
                   07  WRK-TBYOMEICD-SEP   PIC X(01).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-MKCD-MAX          PIC 9(05)
                                       VALUE 21.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    病名編集サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSS012.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SS012-AREA
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO        TO  FLG-HAISHI
      *
           INITIALIZE                      ORCSBYOMEIAREA
           MOVE    "1"                 TO  LNK-BYO-SYORI
           MOVE    SS012-I-BYOMEI      TO  LNK-BYO-BYOMEI
           CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                           SPA-AREA
           IF    ( LNK-BYO-BYOMEIHENFLG    =   SPACE ) 
               IF    ( LNK-BYO-IKOSAKICD   NOT =   SPACE )
                   INITIALIZE              WRK-TBYOMEICD-G
                   PERFORM VARYING IDX0    FROM 1 BY 1
                           UNTIL ( IDX0    >  CONST-MKCD-MAX )
                            OR   ( LNK-BYO-TBYOMEICD (IDX0)
                                           = SPACE )
                       MOVE    LNK-BYO-TBYOMEICD (IDX0)
                                       TO  WRK-TBYOMEICD (IDX0)
                   END-PERFORM
                   MOVE    LNK-BYO-IKOSAKICD   TO  WRK-IKOCD
                   MOVE    LNK-BYO-IKOSAKICD-INDEX
                                               TO  IDXI
                   INITIALIZE              ORCSBYOMEIAREA
                   MOVE    "2"         TO  LNK-BYO-SYORI
                   PERFORM VARYING IDX0 FROM 1 BY 1
                           UNTIL ( IDX0 >   CONST-MKCD-MAX )
                            OR   ( WRK-TBYOMEICD (IDX0)
                                        = SPACE )
                       MOVE WRK-TBYOMEICD (IDX0)
                                       TO  LNK-BYO-TBYOMEICD (IDX0)
                   END-PERFORM
                   MOVE    WRK-IKOCD   TO  LNK-BYO-TBYOMEICD (IDXI)
                   CALL    "ORCSBYOMEI" USING  ORCSBYOMEIAREA
                                               SPA-AREA
               ELSE
                   IF    ( LNK-BYO-HAISIYMD-ERR  NOT = ZERO )
                       MOVE    1       TO  FLG-HAISHI
                   END-IF
               END-IF
           END-IF
      *
           INITIALIZE              WRK-TBYOMEICD-G
           PERFORM VARYING IDX0    FROM 1 BY 1
                   UNTIL ( IDX0    >  CONST-MKCD-MAX )
                    OR   ( LNK-BYO-TBYOMEICD (IDX0)
                                   = SPACE )
               MOVE    LNK-BYO-TBYOMEICD (IDX0)
                               TO  WRK-TBYOMEICD (IDX0)
               IF    ( IDX0    <   CONST-MKCD-MAX )
                   IF    ( LNK-BYO-TBYOMEICD (IDX0 + 1)
                               NOT =   SPACE )
                       MOVE    "-"     TO  WRK-TBYOMEICD-SEP (IDX0)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    LNK-BYO-BYOMEI      TO  SS012-O-BYOMEI
           MOVE    WRK-TBYOMEICD-G     TO  SS012-BYOMEICD-G
           MOVE    LNK-BYO-BYOMEIHENFLG
                                       TO  SS012-BYOMEIHENFLG
           MOVE    FLG-HAISHI          TO  SS012-HAISHI
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *
           .
       300-END-EXT.
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
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
