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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCXRMST2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新処理
      *  コンポーネント名  : 更新プログラム実行処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/22    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-太田     02/03/18  点数マスタレイアウト変更
      *                                     対応
      * 02.05.00     NACL-竹田    05/04/05  バージョン依存緩和処理追加
      * 03.00.00     NACL-伊藤    06/05/26  MONFUNC対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：パラメータファイル
           SELECT  IN-FILE         ASSIGN  PGEXIST01
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
       
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力ファイル
       FD  IN-FILE.
       01  IN-REC.
           03  IN-FIL                  PIC X(01).
      *
      *
       WORKING-STORAGE             SECTION.
      *
       01  PGEXIST01                   PIC X(256).
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-OUT                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-DBKANRI             PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
           03  FLG-CHECK               PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
      *
           03  WRK-IN                  PIC X(01). 
      *
           03  WRK-UPFILE              PIC X(100).
           03  WRK-UPFILE2             PIC X(100).
      *
        01 LNK-AREA.
           03  LNK-DBFILE              PIC X(13).
           03  LNK-UPFILE              PIC X(100).
      *---(01.00.01) LINE ADD START -------------------------------------
           03  LNK-DBRVERSION          PIC X(21).
      *---(01.00.01) LINE ADD END   -------------------------------------
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    ＤＢ管理情報
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
           COPY    "CPHOMEDIR.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  PARA-REC.
           03  PARA-DBFILE               PIC X(13).
           03  PARA-MASTER-KBN           PIC X(02).
           03  PARA-SEPARATE             PIC X(01).
           03  PARA-DBRVERVION           PIC X(21).
           03  PARA-UPPG                 PIC X(10).
           03  PARA-UPFILE               PIC X(100).
      *
       PROCEDURE                       DIVISION
           USING
           PARA-REC.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      ************ UNTIL   FLG-IN      =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
      *
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *    IF      MCP-RC      NOT =   ZERO
      *        DISPLAY "DBOPEN ERR"
      *    END-IF.
      *
      *    入力　初期処理
      *    OPEN    INPUT           IN-FILE
      *    IF      STS-IN          NOT =   ZERO
      *        MOVE    1           TO  FLG-IN
      *        DISPLAY "*(ORCXRMST2)* IN-FILE OPN STS["
      *                                STS-IN "]"
      *    END-IF
      *
      *    レコード管理ファイル読込み
      *    IF      FLG-IN              =   ZERO
      *        PERFORM 900-IN-READ-SEC
      *    END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-UPFILE
           STRING  PARA-UPFILE         DELIMITED  BY  "*"
                                       INTO    WRK-UPFILE
           END-STRING
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1     BY  1
                   UNTIL       IDX     >   100
               IF      WRK-UPFILE(IDX:1)    =   "/"
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
      *
           ADD     1                   TO  IDY
           MOVE    SPACE               TO  WRK-UPFILE2
           MOVE    WRK-UPFILE(IDY:)    TO  WRK-UPFILE2
      *
           MOVE    PARA-DBFILE         TO  LNK-DBFILE
           MOVE    WRK-UPFILE2         TO  LNK-UPFILE
      *---(01.00.01) LINE ADD START -------------------------------------
           MOVE    PARA-DBRVERVION     TO  LNK-DBRVERSION
      *---(01.00.01) LINE ADD END   -------------------------------------
      *
      *    更新プログラム存在チェック
           PERFORM 2001-PGEXIST-CHECK
           IF      FLG-CHECK           NOT =  ZERO
                   DISPLAY "*(ORCXMST2)* MODULE NOT FOUND:" PARA-UPPG
                   PERFORM 100-DBOPEN-SEC
                   GO  TO              200-MAIN-EXT
           END-IF
      *    更新プログラム実行
           CALL    PARA-UPPG           USING
                                       LNK-AREA
      *
      *    DB OPEN
           PERFORM 100-DBOPEN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "DBOPEN ERR"
           END-IF.
      *
      *    ＤＢ管理マスタ検索
           INITIALIZE                  DBKANRI-REC
      *    MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    "ORCADB"            TO  DBKANRI-KANRICD
           MOVE    PARA-MASTER-KBN     TO  DBKANRI-KANRICD(7:2)
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbkanri"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM                     910-DBKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-DBKANRI
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
      *    ＤＢ管理マスタ更新
           IF      FLG-DBKANRI         =   ZERO
               MOVE    PARA-DBRVERVION TO  DBKANRI-DBRVERSION1
               MOVE    DBKANRI-REC     TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_dbkanri"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"       USING
                                           MCPAREA
                                           MCPDATA-REC
               IF      MCP-RC          NOT =   ZERO
                    DISPLAY "*(ORCXMST2)* DBUPDATE ERR:" MCP-RC
               END-IF
           END-IF
      *
      *    レコード管理ファイル読込み
      *****PERFORM 900-IN-READ-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新プログラム存在チェック
      *****************************************************************
       2001-PGEXIST-CHECK              SECTION.
           STRING  HOMEDIR-NAME        DELIMITED  BY  SPACE
                   PARA-UPPG           DELIMITED  BY  SPACE
                   ".so"               DELIMITED  BY  SIZE
                                       INTO   PGEXIST01 
           END-STRING
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF      STS-IN          NOT =   ZERO
               STRING  HOMEDIR-NAME        DELIMITED  BY  SPACE
                       "patch-lib/"        DELIMITED  BY  SIZE
                       PARA-UPPG           DELIMITED  BY  SPACE
                       ".so"               DELIMITED  BY  SIZE
                                           INTO   PGEXIST01 
               END-STRING
               OPEN    INPUT           IN-FILE
               IF      STS-IN          NOT =   ZERO
                   MOVE    1           TO  FLG-CHECK
                   DISPLAY "*(ORCXRMST2)* IN-FILE OPN STS["
                                           STS-IN "]"
               ELSE
                   CLOSE    IN-FILE
               END-IF
           ELSE
               CLOSE    IN-FILE
           END-IF
           .
       2001-PGEXIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
      **** CLOSE                       IN-FILE
      *
      *    DISPLAY "*(ORCXRMST2)* INPUT  /I   CNT[" CNT-IN  "]"
      *    ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ファイル読込み処理
      *****************************************************************
      *900-IN-READ-SEC          SECTION.
      *
      *   READ    IN-FILE
      *       AT  END
      *           MOVE    1               TO  FLG-IN
      *       NOT AT  END
      *           MOVE    ZERO            TO  FLG-IN
      *           ADD     1               TO  CNT-IN
      *   END-READ
      *
      *   .
      *900-IN-READ-EXT.
      *   EXIT.
      *****************************************************************
      *    ＤＢ管理ファイル読込
      *****************************************************************
       910-DBKANRI-READ-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
               MOVE    ZERO            TO  FLG-DBKANRI
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           .
       910-DBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           .
       900-DBCLOSE-EXT.
           EXIT.
      *
