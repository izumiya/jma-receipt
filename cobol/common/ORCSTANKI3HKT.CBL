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
       PROGRAM-ID.                 ORCSTANKI3HKT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 短期滞在３包括
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/08/28    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
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
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TANKI3                  PIC 9(01).
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
      *
      *    コンスタント値
       01  CONSTANT-AREA.
      *
           03   CONS-TANKI3HKT-AREA.
      *      血液学的検査判断料
             05   CONS-HKT1                PIC X(09) VALUE "160061810".
      *      生化学的検査（１）判断料
             05   CONS-HKT2                PIC X(09) VALUE "160061910".
      *      免疫学的検査判断料
             05   CONS-HKT3                PIC X(09) VALUE "160062110".
      *
           03  CONS-TANKI3HKT-AREA-R  REDEFINES  CONS-TANKI3HKT-AREA.
             05   CONS-TANKI3HKT-TBL      OCCURS  3.
                07   CONS-TANKI3HKT-SRYCD  PIC X(09).
      *
      *    一時領域
       01  WRK-AREA.
      *
      *    短期滞在手術等基本料３判定用
           03  WRK-TANKI3-DAY              PIC X(01)   OCCURS  31.
      *    算定日再定義
           03  WRK-SANTEI-YMD.
               05  WRK-SANTEI-YY           PIC 9(04).
               05  WRK-SANTEI-MM           PIC 9(02).
               05  WRK-SANTEI-DD           PIC 9(02).
      *    月末日取得用
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSTANKI3HKT.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTANKI3HKT-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-TANKI3HKT-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-TANKI3HKT-RC
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-TANKI3HKT-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           IF      LNK-TANKI3HKT-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    患者マスター検索
           INITIALIZE                         PTINF-REC
           MOVE    LNK-TANKI3HKT-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-TANKI3HKT-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    1                  TO  LNK-TANKI3HKT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    算定年月日チェック
           IF      LNK-TANKI3HKT-SANTEIYMD  =   SPACE
               MOVE    2                  TO  LNK-TANKI3HKT-RC
               GO  TO  100-PRM-CHK-EXT
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
           IF    LNK-TANKI3HKT-SRYCD    =   CONS-TANKI3HKT-SRYCD(1) OR
                                            CONS-TANKI3HKT-SRYCD(2) OR
                                            CONS-TANKI3HKT-SRYCD(3)
      *
             MOVE    LNK-TANKI3HKT-SANTEIYMD  TO  WRK-SANTEI-YMD
      *
      *      月末日を取得
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY7-AREA
             MOVE    "71"                   TO  LNK-DAY7-IRAI
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-DAY7-YM
             CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
             IF      STS-DAY-RC1            =   ZERO
                 MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
             END-IF
      *
      *      短期滞在手術基本料３の入院日について取得
             INITIALIZE                         ORCSNYUINDAY-AREA
             MOVE    LNK-TANKI3HKT-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
             MOVE    LNK-TANKI3HKT-PTID     TO  LNK-NYUINDAY-PTID
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-NYUINDAY-SANTEIYM
             CALL    "ORCSNYUINDAY"     USING   ORCSNYUINDAY-AREA
                                                SPA-AREA
      *
             MOVE     ZERO                  TO  FLG-TANKI3
             PERFORM     VARYING    IDX   FROM   1   BY   1
               UNTIL     IDX    >   WRK-GETSUMATSU-DD
                   IF    LNK-NYUINDAY-TANKI3-KBN(IDX)   =   "1"
                      MOVE    1             TO  FLG-TANKI3
                   END-IF
                   IF    FLG-TANKI3         =   1
                      MOVE   "1"            TO  WRK-TANKI3-DAY(IDX)
                   END-IF
             END-PERFORM
      *
      **       PERFORM   VARYING  IDX   FROM  1  BY  1
      **         UNTIL   IDX   >   31
      **            DISPLAY  "IDX = " IDX
      **            DISPLAY  "WRK-TANKI3-DAY(IDX) = " WRK-TANKI3-DAY(IDX)
      **       END-PERFORM
      *
             IF    WRK-TANKI3-DAY(WRK-SANTEI-DD)   =   "1"
                MOVE    "1"                 TO    LNK-TANKI3HKT-KBN
             END-IF
      *
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
