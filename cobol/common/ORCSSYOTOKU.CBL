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
       PROGRAM-ID.             ORCSSYOTOKU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 患者所得情報編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-門脇    09/07/28  所得者情報の機能追加
      *                                     （標準負担額のみの減額）
      *
      *  04.08.01    NACL-門脇    15/10/21  現物給付対象外の低所得（食事）対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-TNKRRK            PIC 9(01).
           03  FLG-TSYRRK            PIC 9(01).
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-PTINF             PIC 9(01).
           03  FLG-NYUINACCT         PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-CHECK             PIC 9(01).
           03  FLG-ARI               PIC 9(01).
           03  FLG-OK                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(02).
           03  IDX2                  PIC 9(02).
           03  IDX3                  PIC 9(02).
           03  IDX-ST                PIC 9(02).
           03  IDX-ED                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DAY-TABLE.
             05  WRK-DAY-T             OCCURS   31.
               07  WRK-DAY-SYU-TNK     PIC X(01).
               07  WRK-DAY-SYU-TSY     PIC X(01).
               07  WRK-DAY-CHI-TNK     PIC X(01).
               07  WRK-DAY-CHI-TSY     PIC X(01).
               07  WRK-DAY-SKJ-TNK     PIC X(01).
               07  WRK-DAY-SKJ-TSY     PIC X(01).
      *
               07  WRK-DAY-KOH966      PIC X(01).
               07  WRK-DAY-KOH967      PIC X(01).
               07  WRK-DAY-KOH956      PIC X(01).
               07  WRK-DAY-KOH957      PIC X(01).
      *
               07  WRK-DAY-KOH968      PIC X(01).
               07  WRK-DAY-KOH976      PIC X(01).
      *
               07  WRK-DAY-KOH012      PIC X(01).
               07  WRK-DAY-KOH025      PIC X(01).
      *
               07  WRK-DAY-HKNCOMBI    PIC 9(04).
               07  WRK-DAY-HKNNUM      PIC X(03).
               07  WRK-DAY-KOH1HKNNUM  PIC X(03).
               07  WRK-DAY-KOH2HKNNUM  PIC X(03).
               07  WRK-DAY-KOH3HKNNUM  PIC X(03).
               07  WRK-DAY-KOH4HKNNUM  PIC X(03).
               07  WRK-DAY-HKNKOHSBTKBN
                                       PIC X(01).
               07  WRK-DAY-HOJOKBN     PIC X(01).
      *
               07  WRK-DAY-NENREIFLG   PIC X(01).
      *
           03  WRK-SYOTOKU-YMD.
               05  WRK-SYOTOKU-YM      PIC 9(06).
               05  WRK-SYOTOKU-DD      PIC 9(02).
      *
           03  WRK-BIRTHDAY            PIC X(08).
           03  WRK-NENREI              PIC 9(03).
      *
           03  WRK-FIRST-DAY           PIC 9(02).
           03  WRK-FIRST-SKJKBN        PIC X(01).
      *
           03  WRK-CHECK-TABLE.
             05  WRK-CHECK-T           OCCURS   31.
               07  WRK-CHECK-HKNCOMBI  PIC 9(04).
               07  WRK-CHECK-FLG       PIC 9(01).
      *
      *    固定値
       01  WRK-CONS-AREA.
      *
           03  WRK-CONS-SRYYM-200610   PIC 9(06)   VALUE   200610.
           03  WRK-CONS-SRYYM-200604   PIC 9(06)   VALUE   200604.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    低１
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    低２
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    特定疾患等算定判定サブ
           COPY    "CPORCSSRHKOH.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSYOTOKU.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSSYOTOKUAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-SYOTOKU-RC
           INITIALIZE                      LNK-SYOTOKU-HENKYAKU-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
           IF    LNK-SYOTOKU-RC   NOT =   ZERO
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-SYOTOKU-RC       =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    処理年月チェック
           IF      LNK-SYOTOKU-SRYYM    =   SPACE
               MOVE    1                 TO  LNK-SYOTOKU-RC
           END-IF
      *
      *    患者ＩＤチェック
           IF  LNK-SYOTOKU-RC  =  ZERO
             IF    LNK-SYOTOKU-PTID     =   ZERO
               MOVE    2                 TO  LNK-SYOTOKU-RC
             ELSE
               MOVE    SPACE                TO  WRK-BIRTHDAY
               INITIALIZE                       PTINF-REC
               MOVE    LNK-SYOTOKU-HOSPNUM  TO  PTINF-HOSPNUM
               MOVE    LNK-SYOTOKU-PTID     TO  PTINF-PTID
               MOVE    PTINF-REC            TO  MCPDATA-REC
               MOVE    "DBSELECT"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC  =  ZERO
                  MOVE      "tbl_ptinf"     TO  MCP-TABLE
                  MOVE      "key"           TO  MCP-PATHNAME
                  PERFORM    910-PTINF-READ-SEC
                  IF   FLG-PTINF  =  ZERO
                     MOVE    PTINF-BIRTHDAY TO  WRK-BIRTHDAY
                  ELSE
                     MOVE    2              TO  LNK-SYOTOKU-RC
                  END-IF
               ELSE
                     MOVE    2              TO  LNK-SYOTOKU-RC
               END-IF
               MOVE    "tbl_ptinf"          TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
             END-IF
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    低１
           INITIALIZE                        TNKRRK-REC
           MOVE    LNK-SYOTOKU-HOSPNUM   TO  TNKRRK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID      TO  TNKRRK-PTID
           MOVE    TNKRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_tnkrrk"          TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING   MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC                =   ZERO
              MOVE   "tbl_tnkrrk"        TO  MCP-TABLE
              MOVE   "key3"              TO  MCP-PATHNAME
              PERFORM 910-TNKRRK-READ-SEC
              PERFORM   UNTIL
                     (FLG-TNKRRK         =      1             ) OR
                     (TNKRRK-EDYMD(1:6)  <   LNK-SYOTOKU-SRYYM)
                 IF  (TNKRRK-STYMD(1:6)  <=  LNK-SYOTOKU-SRYYM)
                 AND (LNK-SYOTOKU-SRYYM  <=  TNKRRK-EDYMD(1:6))
                   IF LNK-SYOTOKU-SRYYM  >=  WRK-CONS-SRYYM-200604
                      PERFORM  210-TNK-HENSYU-SEC
                   ELSE
                      PERFORM  211-TNK-HENSYU2-SEC
                   END-IF
                 END-IF
                 MOVE   "tbl_tnkrrk"     TO  MCP-TABLE
                 MOVE   "key3"           TO  MCP-PATHNAME
                 PERFORM 910-TNKRRK-READ-SEC
              END-PERFORM
           ELSE
              MOVE    3                  TO  LNK-SYOTOKU-RC
           END-IF
           MOVE     "tbl_tnkrrk"         TO  MCP-TABLE
           MOVE     "key3"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    低２
           INITIALIZE                        TSYRRK-REC
           MOVE    LNK-SYOTOKU-HOSPNUM   TO  TSYRRK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID      TO  TSYRRK-PTID
           MOVE    TSYRRK-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_tsyrrk"          TO  MCP-TABLE
           MOVE    "key3"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING   MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC              =   ZERO
              MOVE   "tbl_tsyrrk"        TO  MCP-TABLE
              MOVE   "key3"              TO  MCP-PATHNAME
              PERFORM 910-TSYRRK-READ-SEC
              PERFORM   UNTIL
                     (FLG-TSYRRK            =      1             ) OR
                     (TSYRRK-NINEDYMD(1:6)  <   LNK-SYOTOKU-SRYYM)
                 IF  (TSYRRK-NINSTYMD(1:6)  <=  LNK-SYOTOKU-SRYYM   )
                 AND (LNK-SYOTOKU-SRYYM     <=  TSYRRK-NINEDYMD(1:6))
                   IF LNK-SYOTOKU-SRYYM     >=  WRK-CONS-SRYYM-200604
                      PERFORM  220-TSY-HENSYU-SEC
                   ELSE
                      PERFORM  221-TSY-HENSYU2-SEC
                   END-IF
                 END-IF
                 MOVE   "tbl_tsyrrk"     TO  MCP-TABLE
                 MOVE   "key3"           TO  MCP-PATHNAME
                 PERFORM 910-TSYRRK-READ-SEC
              END-PERFORM
           ELSE
              MOVE    4                  TO  LNK-SYOTOKU-RC
           END-IF
           MOVE     "tbl_tsyrrk"         TO  MCP-TABLE
           MOVE     "key3"               TO  MCP-PATHNAME
           PERFORM   910-DB-CLOSE-SEC
      *
      *    結果編集
           IF        LNK-SYOTOKU-RC      =   ZERO
      *
             PERFORM   VARYING   IDX     FROM    1   BY  1
                        UNTIL    IDX      >     31
      *
              EVALUATE WRK-DAY-SYU-TSY(IDX) ALSO WRK-DAY-SYU-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    SPACE              ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                       MOVE   "2"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SYUKBN(IDX)
                 WHEN    "2"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SYUKBN(IDX)
              END-EVALUATE
      *
              EVALUATE WRK-DAY-CHI-TSY(IDX) ALSO WRK-DAY-CHI-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                       MOVE   "2"     TO    LNK-SYOTOKU-CHIKBN(IDX)
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-CHIKBN(IDX)
              END-EVALUATE
      *
              EVALUATE WRK-DAY-SKJ-TSY(IDX) ALSO WRK-DAY-SKJ-TNK(IDX)
                 WHEN    SPACE              ALSO    SPACE
                       MOVE   SPACE   TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    SPACE              ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    SPACE              ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "2"                ALSO    SPACE
                    IF   LNK-SYOTOKU-SYUKBN(IDX) = "1" OR "4"
                       MOVE   SPACE   TO    LNK-SYOTOKU-SKJKBN(IDX)
                    ELSE
                       MOVE   "2"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                    END-IF
                 WHEN    "2"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "2"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "3"                ALSO    SPACE
                    IF   LNK-SYOTOKU-SYUKBN(IDX) = "1" OR "4"
                       MOVE   SPACE   TO    LNK-SYOTOKU-SKJKBN(IDX)
                    ELSE
                       MOVE   "3"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                    END-IF
                 WHEN    "3"                ALSO    "1"
                       MOVE   "1"     TO    LNK-SYOTOKU-SKJKBN(IDX)
                 WHEN    "3"                ALSO    "4"
                       MOVE   "4"     TO    LNK-SYOTOKU-SKJKBN(IDX)
              END-EVALUATE
      *
             END-PERFORM
      *
             PERFORM   VARYING   IDX     FROM    1   BY  1
                        UNTIL    IDX      >     31
                 MOVE  LNK-SYOTOKU-SKJKBN(IDX)
                                      TO    LNK-SYOTOKU-SKJKBN-OLD(IDX)
             END-PERFORM
      *
             MOVE      ZERO           TO    FLG-CHECK
             IF  (LNK-SYOTOKU-SRYYM      >=  "201501")
             AND (LNK-SYOTOKU-SKJCHKFLG   =  "1"     )
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF  LNK-SYOTOKU-SKJKBN(IDX) NOT = SPACE
                       MOVE    1      TO    FLG-CHECK
                       MOVE   31      TO    IDX
                 END-IF
               END-PERFORM
               IF    FLG-CHECK   =   1
                 PERFORM  230-NENREI-CHECK-SEC
               END-IF
             END-IF
      *
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低１編集
      *****************************************************************
       210-TNK-HENSYU-SEC            SECTION.
      *
           IF   TNKRRK-STYMD(1:6)  <  LNK-SYOTOKU-SRYYM
               MOVE    1                   TO   IDX-ST
           ELSE
               MOVE    TNKRRK-STYMD(7:2)   TO   IDX-ST
           END-IF
           IF   LNK-SYOTOKU-SRYYM  <  TNKRRK-EDYMD(1:6)
               MOVE    31                  TO   IDX-ED
           ELSE
               MOVE    TNKRRK-EDYMD(7:2)   TO   IDX-ED
           END-IF
      *
           PERFORM   VARYING   IDX     FROM   IDX-ST   BY  1
                      UNTIL    IDX      >     IDX-ED
      *
              MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
              MOVE    IDX                  TO  WRK-SYOTOKU-DD
      *
              IF    TNKRRK-HKNTEKKBN  = "0" OR "1"
                 MOVE    "1"     TO    WRK-DAY-SYU-TNK(IDX)
                 IF  (LNK-SYOTOKU-SRYYM    >=  WRK-CONS-SRYYM-200610 )
                 AND (TNKRRK-RRI-FUKUSHI-KBN  =   "1"                )
                     MOVE  "4"   TO    WRK-DAY-SYU-TNK(IDX)
                 END-IF
              END-IF
      *
              IF    TNKRRK-HKNTEKKBN  = "0" OR "2" OR "4"
                 MOVE    "1"     TO    WRK-DAY-CHI-TNK(IDX)
              END-IF
      *
              IF  (TNKRRK-HKNTEKKBN = "0" OR "1" OR "3" OR "4")
              AND (TNKRRK-SKJNINSTYMD   NOT =  SPACE          )
              AND (TNKRRK-SKJNINSTYMD      <=  WRK-SYOTOKU-YMD)
              AND (TNKRRK-EDYMD            >=  WRK-SYOTOKU-YMD)
                 MOVE    "1"     TO    WRK-DAY-SKJ-TNK(IDX)
                 IF  (LNK-SYOTOKU-SRYYM    >=  WRK-CONS-SRYYM-200610 )
                 AND (TNKRRK-RRI-FUKUSHI-KBN  =   "1"                )
                     MOVE  "4"   TO    WRK-DAY-SKJ-TNK(IDX)
                 END-IF
              END-IF
      *
           END-PERFORM
      *
           .
       210-TNK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    低２編集
      *****************************************************************
       220-TSY-HENSYU-SEC            SECTION.
      *
           IF   TSYRRK-NINSTYMD(1:6)  <  LNK-SYOTOKU-SRYYM
               MOVE    1                      TO   IDX-ST
           ELSE
               MOVE    TSYRRK-NINSTYMD(7:2)   TO   IDX-ST
           END-IF
           IF   LNK-SYOTOKU-SRYYM     <  TSYRRK-NINEDYMD(1:6)
               MOVE    31                     TO   IDX-ED
           ELSE
               MOVE    TSYRRK-NINEDYMD(7:2)   TO   IDX-ED
           END-IF
      *
           PERFORM   VARYING   IDX     FROM   IDX-ST   BY  1
                      UNTIL    IDX      >     IDX-ED
      *
              MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
              MOVE    IDX                  TO  WRK-SYOTOKU-DD
      *
              IF    TSYRRK-HKNTEKKBN  = "0" OR "1"
                 MOVE    "2"     TO    WRK-DAY-SYU-TSY(IDX)
              END-IF
      *
              IF    TSYRRK-HKNTEKKBN  = "0" OR "2" OR "4"
                 MOVE    "2"     TO    WRK-DAY-CHI-TSY(IDX)
              END-IF
      *
              IF  (TSYRRK-HKNTEKKBN = "0" OR "1" OR "3" OR "4")
              AND (TSYRRK-SKJNINSTYMD   NOT =  SPACE          )
              AND (TSYRRK-SKJNINSTYMD      <=  WRK-SYOTOKU-YMD)
              AND (TSYRRK-NINEDYMD         >=  WRK-SYOTOKU-YMD)
                 MOVE    "2"     TO    WRK-DAY-SKJ-TSY(IDX)
                 IF  (TSYRRK-SKJGNSTYMD NOT =  SPACE          )
                 AND (TSYRRK-SKJGNSTYMD    <=  WRK-SYOTOKU-YMD)
                 AND (TSYRRK-NINEDYMD      >=  WRK-SYOTOKU-YMD)
                   MOVE  "3"     TO    WRK-DAY-SKJ-TSY(IDX)
                 END-IF
              END-IF
      *
           END-PERFORM
      *
           .
       220-TSY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    低１編集（２００６年３月まで）
      *****************************************************************
       211-TNK-HENSYU2-SEC            SECTION.
      *
           MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
           MOVE    01                   TO  WRK-SYOTOKU-DD
      *
           IF  (TNKRRK-STYMD     <=  WRK-SYOTOKU-YMD)
           AND (WRK-SYOTOKU-YMD  <=  TNKRRK-EDYMD   )
      *
              PERFORM   VARYING   IDX     FROM    1   BY  1
                         UNTIL    IDX      >     31
                  MOVE    "1"     TO    WRK-DAY-SYU-TNK(IDX)
                  MOVE    "1"     TO    WRK-DAY-CHI-TNK(IDX)
                  MOVE    "1"     TO    WRK-DAY-SKJ-TNK(IDX)
              END-PERFORM
      *
           END-IF
      *
           .
       211-TNK-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    低２編集（２００６年３月まで）
      *****************************************************************
       221-TSY-HENSYU2-SEC            SECTION.
      *
           MOVE    LNK-SYOTOKU-SRYYM    TO  WRK-SYOTOKU-YM
           MOVE    01                   TO  WRK-SYOTOKU-DD
      *
           IF  (TSYRRK-NINSTYMD  <=  WRK-SYOTOKU-YMD)
           AND (WRK-SYOTOKU-YMD  <=  TSYRRK-NINEDYMD)
      *
              PERFORM   VARYING   IDX     FROM    1   BY  1
                         UNTIL    IDX      >     31
      *
                  MOVE    "2"     TO    WRK-DAY-SYU-TSY(IDX)
                  MOVE    "2"     TO    WRK-DAY-CHI-TSY(IDX)
      *
                  MOVE    "2"     TO    WRK-DAY-SKJ-TSY(IDX)
                  IF  (TSYRRK-SKJGNSTYMD NOT =  SPACE          )
                  AND (TSYRRK-SKJGNSTYMD    <=  WRK-SYOTOKU-YMD)
                  AND (TSYRRK-NINEDYMD      >=  WRK-SYOTOKU-YMD)
                    MOVE  "3"     TO    WRK-DAY-SKJ-TSY(IDX)
                  END-IF
      *
              END-PERFORM
      *
           END-IF
      *
           .
       221-TSY-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢者・７０歳未満判定
      *****************************************************************
       230-NENREI-CHECK-SEC            SECTION.
      *
      *    患者年齢
           MOVE    ZERO                   TO  WRK-NENREI
           INITIALIZE                         ORCSAGECHKAREA
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  AGECHK-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  AGECHK-PTID
           MOVE    LNK-SYOTOKU-SRYYM      TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"                   TO  AGECHK-SRYYMD(7:2)
           MOVE    WRK-BIRTHDAY           TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"           USING  ORCSAGECHKAREA
                                                 SPA-AREA
           IF      AGECHK-RC  =  ZERO
              MOVE    AGECHK-NENREI       TO  WRK-NENREI
           END-IF
      *
      *    入院会計の保険組み合わせ
           INITIALIZE                         NYUINACCT-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  NACCT-PTID
           MOVE    LNK-SYOTOKU-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_nyuinacct"   TO  MCP-TABLE
              MOVE      "key17"           TO  MCP-PATHNAME
              PERFORM    910-NYUINACCT-READ-SEC
              IF   FLG-NYUINACCT  =  ZERO
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                   IF     NACCT-DAY(IDX2) NOT =  ZERO 
                     MOVE NACCT-DAY(IDX2) TO  WRK-DAY-HKNCOMBI(IDX2)
                   END-IF
                END-PERFORM
              END-IF
           END-IF
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
              IF   WRK-DAY-HKNCOMBI(IDX2)  NOT =  ZERO
                INITIALIZE                         HKNCOMBI-REC
                MOVE    LNK-SYOTOKU-HOSPNUM    TO  COMB-HOSPNUM
                MOVE    LNK-SYOTOKU-PTID       TO  COMB-PTID
                MOVE    WRK-DAY-HKNCOMBI(IDX2) TO  COMB-HKNCOMBINUM
                MOVE    HKNCOMBI-REC           TO  MCPDATA-REC
                MOVE    "DBSELECT"             TO  MCP-FUNC
                MOVE    "tbl_hkncombi"         TO  MCP-TABLE
                MOVE    "key"                  TO  MCP-PATHNAME
                CALL    "ORCDBMAIN"            USING  MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
                IF      MCP-RC  =  ZERO
                   MOVE      "tbl_hkncombi"    TO  MCP-TABLE
                   MOVE      "key"             TO  MCP-PATHNAME
                   PERFORM    910-HKNCOMBI-READ-SEC
                   IF   FLG-HKNCOMBI  =  ZERO
                     MOVE  COMB-HKNNUM      TO WRK-DAY-HKNNUM(IDX2)
                     MOVE  COMB-KOH1HKNNUM  TO WRK-DAY-KOH1HKNNUM(IDX2)
                     MOVE  COMB-KOH2HKNNUM  TO WRK-DAY-KOH2HKNNUM(IDX2)
                     MOVE  COMB-KOH3HKNNUM  TO WRK-DAY-KOH3HKNNUM(IDX2)
                     MOVE  COMB-KOH4HKNNUM  TO WRK-DAY-KOH4HKNNUM(IDX2)
                     MOVE  COMB-SYU-HKNKOHSBTKBN
                                       TO WRK-DAY-HKNKOHSBTKBN(IDX2)
                     MOVE  COMB-HOJOKBN     TO WRK-DAY-HOJOKBN(IDX2)
                   END-IF
                END-IF
                MOVE    "tbl_hkncombi"         TO  MCP-TABLE
                MOVE    "key"                  TO  MCP-PATHNAME
                PERFORM  910-DB-CLOSE-SEC
              END-IF
           END-PERFORM
      *
      *    公費単独の老人保険入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "968"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH968(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    高齢非該当入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "976"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH976(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    認定証入力
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "966"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH966(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "967"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH967(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "956"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH956(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "957"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH957(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    生保・残留邦人
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "012"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH012(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           INITIALIZE                         PTKOHINF-REC
           MOVE    LNK-SYOTOKU-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-SYOTOKU-PTID       TO  PTKOH-PTID
           MOVE    "025"                  TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM   VARYING   IDX2    FROM    1   BY  1
                           UNTIL    IDX2     >     31
                  MOVE  LNK-SYOTOKU-SRYYM TO  WRK-SYOTOKU-YM
                  MOVE  IDX2              TO  WRK-SYOTOKU-DD
                  IF  (PTKOH-TEKSTYMD  <=  WRK-SYOTOKU-YMD)
                  AND (WRK-SYOTOKU-YMD <=  PTKOH-TEKEDYMD )
                  AND (PTKOH-SAKJOKBN   =  SPACE          )
                     MOVE   "1"           TO  WRK-DAY-KOH025(IDX2)
                  END-IF
                END-PERFORM
                MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                MOVE    "key4"            TO  MCP-PATHNAME
                PERFORM  910-PTKOHINF-READ-SEC
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"         TO  MCP-TABLE
           MOVE    "key4"                 TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    高齢者・７０歳未満判定
           MOVE      ZERO                 TO  WRK-FIRST-DAY
           MOVE      SPACE                TO  WRK-FIRST-SKJKBN
           INITIALIZE                         WRK-CHECK-TABLE
           PERFORM   VARYING   IDX2    FROM    1   BY  1
                      UNTIL    IDX2     >     31
              IF   WRK-DAY-HKNCOMBI(IDX2)  NOT =  ZERO
                 IF  (WRK-DAY-HKNKOHSBTKBN(IDX2) =  "8" OR "9")
                 OR  (WRK-DAY-HKNNUM(IDX2)       =
                                "971" OR "973" OR "975" OR
                                "068" OR "069" OR "040"       )
                     MOVE   "9"           TO  WRK-DAY-NENREIFLG(IDX2)
                     IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***              DISPLAY "DAY=" IDX2 " 9:SKJKBN(IDX2)="
      ***                       LNK-SYOTOKU-SKJKBN(IDX2)
                       MOVE   SPACE       TO  LNK-SYOTOKU-SKJKBN(IDX2)
                     END-IF
                 ELSE
                   IF  ( WRK-NENREI                  >=  70   )
                   OR  ((WRK-DAY-HKNNUM(IDX2)         = SPACE) AND
                        (WRK-DAY-KOH1HKNNUM(IDX2) NOT = SPACE) AND
                        (WRK-DAY-KOH2HKNNUM(IDX2)     = SPACE) AND
                        (WRK-DAY-KOH3HKNNUM(IDX2)     = SPACE) AND
                        (WRK-DAY-KOH4HKNNUM(IDX2)     = SPACE) AND
                        (WRK-DAY-KOH968(IDX2)         = "1"  ))
                   OR  ( WRK-DAY-HKNNUM(IDX2)         = "039" )
                     MOVE   "1"           TO  WRK-DAY-NENREIFLG(IDX2)
                   ELSE
                     MOVE   "2"           TO  WRK-DAY-NENREIFLG(IDX2)
                   END-IF
                 END-IF
      *
                 IF      WRK-DAY-NENREIFLG(IDX2)  =  "1"
                   IF    WRK-DAY-HKNNUM(IDX2)  =  SPACE OR "039"
                     CONTINUE
                   ELSE
                     IF  ( WRK-DAY-HKNNUM(IDX2)     =
                                                   "060" OR  "067" )
                     OR  ((WRK-DAY-HKNNUM(IDX2) NOT =
                                                   "060" AND "067") AND
                          (WRK-DAY-HOJOKBN(IDX2)    =  SPACE      ))
                       IF  WRK-DAY-KOH976(IDX2)     = "1"
                          MOVE   "2"      TO  WRK-DAY-NENREIFLG(IDX2)
                       END-IF
                     END-IF
                   END-IF
                 END-IF
      *
                 IF      WRK-DAY-NENREIFLG(IDX2)  =  "2"
                   IF    WRK-DAY-HKNNUM(IDX2)  =  SPACE
      ***          OR    WRK-DAY-KOH012(IDX2)  =  "1"
      ***          OR    WRK-DAY-KOH025(IDX2)  =  "1"
                     CONTINUE
                   ELSE
                     PERFORM  2301-NENREI-CHECK2-SEC
                   END-IF
                 END-IF
      *
                 IF  LNK-SYOTOKU-HKNCOMBI(1)  NOT =  ZERO
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL   IDX3    >  10
                     IF   WRK-DAY-HKNCOMBI(IDX2) =
                                     LNK-SYOTOKU-HKNCOMBI(IDX3)
                        IF ( WRK-FIRST-DAY                =  ZERO  )
                        OR ((WRK-FIRST-DAY            NOT =  ZERO ) AND
                            (WRK-FIRST-SKJKBN             =  SPACE) AND
                            (LNK-SYOTOKU-SKJKBN(IDX2) NOT =  SPACE))
                        OR ((WRK-FIRST-DAY            NOT =  ZERO ) AND
                            (WRK-FIRST-SKJKBN             =  "2"  ) AND
                            (LNK-SYOTOKU-SKJKBN(IDX2)     =  "3"  ))
                           MOVE   IDX2      TO  WRK-FIRST-DAY
                           MOVE   LNK-SYOTOKU-SKJKBN(IDX2)
                                            TO  WRK-FIRST-SKJKBN
                        END-IF
                        MOVE      10        TO  IDX3
                     END-IF
                   END-PERFORM
                 END-IF
              ELSE
                     IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***              DISPLAY "DAY=" IDX2 " 0:SKJKBN(IDX2)="
      ***                       LNK-SYOTOKU-SKJKBN(IDX2)
                       MOVE   SPACE       TO  LNK-SYOTOKU-SKJKBN(IDX2)
                     END-IF
              END-IF
           END-PERFORM
      *
           MOVE      WRK-FIRST-DAY        TO  LNK-SYOTOKU-SKJIDX
      *
           .
       230-NENREI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢者・７０歳未満判定２
      *****************************************************************
       2301-NENREI-CHECK2-SEC            SECTION.
      *
           MOVE      ZERO       TO   FLG-ARI
      *
           IF   (WRK-DAY-KOH966(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
           IF   (WRK-DAY-KOH967(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
           IF   (WRK-DAY-KOH956(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE    1          TO   FLG-ARI
           END-IF
      *
      *    特定疾病給付対象療養の診療チェック
           IF   (WRK-DAY-KOH957(IDX2)  =  "1" )
           AND  (FLG-ARI               =  ZERO)
             MOVE     ZERO      TO   FLG-OK
             PERFORM  VARYING  IDX3  FROM  1  BY  1
                       UNTIL  (IDX3                     >  31 )
                        OR    (WRK-CHECK-HKNCOMBI(IDX3) = ZERO)
                        OR    (FLG-OK                   =   1 )
               IF  WRK-DAY-HKNCOMBI(IDX2) = WRK-CHECK-HKNCOMBI(IDX3)
                 IF   WRK-CHECK-FLG(IDX3) = 1
                    CONTINUE
                 ELSE
                    IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***             DISPLAY "DAY=" IDX2 " 2-957:SKJKBN(IDX2)="
      ***                      LNK-SYOTOKU-SKJKBN(IDX2)
                      MOVE   SPACE   TO   LNK-SYOTOKU-SKJKBN(IDX2)
                    END-IF
                 END-IF
                 MOVE    1      TO   FLG-OK
               END-IF
             END-PERFORM
             IF    FLG-OK  =  ZERO
             AND   IDX3   <=   31
               INITIALIZE                         SSRHKOH-AREA
               MOVE   LNK-SYOTOKU-PTID        TO  SSRHKOH-PTID
               MOVE   "1"                     TO  SSRHKOH-NYUGAIKBN
               MOVE   LNK-SYOTOKU-SRYYM       TO  SSRHKOH-SRYYM
               MOVE   WRK-DAY-HKNCOMBI(IDX2)  TO  SSRHKOH-HKNCOMBI
               CALL   "ORCSSRHKOH"            USING  SSRHKOH-AREA
                                                     SPA-AREA
               IF  SSRHKOH-RC  =  1
                 CONTINUE
               ELSE
                 IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***          DISPLAY "DAY=" IDX2 " 2-957:SKJKBN(IDX2)="
      ***                   LNK-SYOTOKU-SKJKBN(IDX2)
                   MOVE   SPACE   TO   LNK-SYOTOKU-SKJKBN(IDX2)
                 END-IF
               END-IF
               MOVE  WRK-DAY-HKNCOMBI(IDX2)
                                TO   WRK-CHECK-HKNCOMBI(IDX3)
               IF  SSRHKOH-RC  =  1
                 MOVE    1      TO   WRK-CHECK-FLG(IDX3)
               ELSE
                 MOVE    ZERO   TO   WRK-CHECK-FLG(IDX3)
               END-IF
             END-IF
             MOVE    1          TO   FLG-ARI
           END-IF
      *
      ***> IF    FLG-ARI  =  ZERO
      ***>     IF  LNK-SYOTOKU-SKJKBN(IDX2) NOT = SPACE
      ***        DISPLAY "DAY=" IDX2 " 2:SKJKBN(IDX2)="
      ***                 LNK-SYOTOKU-SKJKBN(IDX2)
      ***>       MOVE   SPACE   TO   LNK-SYOTOKU-SKJKBN(IDX2)
      ***>     END-IF
      ***> END-IF
      *
           .
       2301-NENREI-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得１マスタ読み込み
      *****************************************************************
       910-TNKRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       910-TNKRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得２マスタ読み込み
      *****************************************************************
       910-TSYRRK-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       910-TSYRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       910-PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計読込
      *****************************************************************
       910-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
