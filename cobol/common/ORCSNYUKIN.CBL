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
       PROGRAM-ID.             ORCSNYUKIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : まとめ入金サブ
      *  管理者            : 
      *  作成日付    作業者        記述
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
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MISYU-CNT      PIC 9(05).
           03  WRK-SYUNOU-DBPATH  PIC X(20).
           03  WRK-NYUKIN-ZAN     PIC 9(08).
           03  WRK-MISYU          PIC 9(07).
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSNYUKIN.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSNYUKINAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  LNK-SCNYUKIN-OUT-AREA
           MOVE    ZERO                TO  LNK-SCNYUKIN-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-SCNYUKIN-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-SCNYUKIN-RC      =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
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
           IF    LNK-SCNYUKIN-SYORIKBN    =   "1"   OR   "2"
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    LNK-SCNYUKIN-KOUSHINKBN  =   "1"   OR   "2"
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    LNK-SCNYUKIN-NYKNKNRKBN   = "1" OR "2" OR "3" OR
                                             "4" OR "5"
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    LNK-SCNYUKIN-NYUKIN-MONEY   = ZERO
               MOVE    10              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SCNYUKIN-HOSPID     TO  PTINF-HOSPID
           MOVE    LNK-SCNYUKIN-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    11              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    振り分け対象未収データの存在チェック（収納マスター検索）
           INITIALIZE                      SYUNOU-REC
           MOVE    LNK-SCNYUKIN-HOSPID     TO  SYU-HOSPID
           MOVE    LNK-SCNYUKIN-PTID       TO  SYU-PTID
           MOVE    LNK-SCNYUKIN-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    "SYUNOU-KEY22"          TO  ORC-DBPATH
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    11              TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    "SYUNOU-KEY22"   TO  ORC-DBPATH
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU          NOT =   ZERO
                    MOVE    11          TO  LNK-SCNYUKIN-RC
                    GO  TO  100-PRM-CHK-EXT
               ELSE
      *            未収データのカウント
                   MOVE      ZERO          TO       WRK-MISYU-CNT
                   PERFORM   UNTIL    FLG-SYUNOU  =   1
                       ADD      1         TO   WRK-MISYU-CNT
                       MOVE    "SYUNOU-KEY22"   TO  ORC-DBPATH
                       PERFORM  910-SYUNOU-READ-SEC
                   END-PERFORM
                   IF   WRK-MISYU-CNT          =   ZERO
                        MOVE   11              TO  LNK-SCNYUKIN-RC
                   END-IF
                   IF   WRK-MISYU-CNT              >   100
                        MOVE   12              TO  LNK-SCNYUKIN-RC
                   END-IF
               END-IF
           END-IF
      *
      *    収納クローズ
           MOVE    "SYUNOU-KEY22"           TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
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
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
      *    収納読み込み用ＫＥＹの指定
           MOVE    SPACE                   TO  WRK-SYUNOU-DBPATH
           EVALUATE    LNK-SCNYUKIN-NYKNKNRKBN
               WHEN    "2"
               WHEN    "4"
                   MOVE   "SYUNOU-KEY22"   TO  WRK-SYUNOU-DBPATH
               WHEN    "3"
               WHEN    "5"
                   MOVE   "SYUNOU-KEY23"   TO  WRK-SYUNOU-DBPATH
           END-EVALUATE
      *
      *
      *    収納マスターから未収金情報の取得
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-SCNYUKIN-HOSPID     TO  SYU-HOSPID
           MOVE    LNK-SCNYUKIN-PTID       TO  SYU-PTID
           MOVE    LNK-SCNYUKIN-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE    WRK-SYUNOU-DBPATH       TO  ORC-DBPATH
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    11                  TO  LNK-SCNYUKIN-RC
               GO  TO  100-PRM-CHK-EXT
           ELSE
               MOVE    WRK-SYUNOU-DBPATH   TO  ORC-DBPATH
               PERFORM 910-SYUNOU-READ-SEC
      *        未収データの取得（入金額の振り分け）
               MOVE      LNK-SCNYUKIN-NYUKIN-MONEY
                                           TO  WRK-NYUKIN-ZAN
               MOVE      ZERO              TO  IDX
               PERFORM   UNTIL   (FLG-SYUNOU  =   1)  OR
                                 (WRK-NYUKIN-ZAN =  ZERO)
                   MOVE    ZERO            TO  WRK-MISYU
                   ADD     1               TO  IDX
                   COMPUTE  WRK-MISYU =  SYU-SKYMONEY  -  
                                         SYU-NYUKIN-TOTAL
                   MOVE   SYU-DENPNUM  TO  
                                          LNK-SCNYUKIN-DENPNUM(IDX)
                   MOVE   SYU-SKYMONEY TO  
                                          LNK-SCNYUKIN-SKYMONEY(IDX)
                   IF      WRK-NYUKIN-ZAN >=   WRK-MISYU
                       MOVE   WRK-MISYU    TO
                                       LNK-SCNYUKIN-FURIWAKEMONEY(IDX)
                       COMPUTE   WRK-NYUKIN-ZAN =
                                       WRK-NYUKIN-ZAN  -  WRK-MISYU
                   ELSE
                       MOVE   WRK-NYUKIN-ZAN  TO
                                       LNK-SCNYUKIN-FURIWAKEMONEY(IDX)
                       MOVE   ZERO            TO   WRK-NYUKIN-ZAN
                   END-IF
                   IF    LNK-SCNYUKIN-KOUSHINKBN   =   "2"
                       PERFORM   300-SYUNOU-UPD-SEC
                       PERFORM   310-SYUMEI-INS-SEC
                   END-IF
                   MOVE    WRK-SYUNOU-DBPATH   TO  ORC-DBPATH
                   PERFORM  910-SYUNOU-READ-SEC
               END-PERFORM
           END-IF
      *    収納クローズ
           MOVE    WRK-SYUNOU-DBPATH   TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
      *    入金振り分け残額の設定
           IF      WRK-NYUKIN-ZAN      NOT =   ZERO
               MOVE  WRK-NYUKIN-ZAN    TO  LNK-SCNYUKIN-FURIWAKEZAN
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       300-SYUNOU-UPD-SEC                SECTION.
      *
      *    入金振り分け額セット
           ADD     LNK-SCNYUKIN-FURIWAKEMONEY(IDX)
                                       TO  SYU-NYUKIN-TOTAL
           ADD     1                   TO  SYU-NYUKIN-KAISU
           IF      SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL
                   MOVE   "2"          TO  SYU-DENPJTIKBN
           ELSE
                   MOVE   "1"          TO  SYU-DENPJTIKBN
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
      *    収納更新
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              NOT =   ZERO
                   DISPLAY "ORCSNYUKIN SYUNOU UPD ERR:"  MCP-RC  "."
                   MOVE    13          TO  LNK-SCNYUKIN-RC
           END-IF
      *
           .
       300-SYUNOU-UPD-EXT.
           EXIT.
      *
      *************************************************************
      *    収納明細作成
      *************************************************************
       310-SYUMEI-INS-SEC           SECTION.
      *
      *    新規作成の収納明細マスタに必要な伝票番号枝番取得
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY2"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =    ZERO
                 MOVE    "SYUMEI-KEY2"      TO  ORC-DBPATH
                 PERFORM 910-SYUMEI-READ-SEC
           ELSE
                 MOVE    1                    TO  FLG-SYUMEI
           END-IF
           IF      FLG-SYUMEI    =     1
                 CONTINUE
           ELSE
                 PERFORM   VARYING  IDX1  FROM   1  BY   1
                     UNTIL (FLG-SYUMEI   =      1 )
                            MOVE      "SYUMEI-KEY2"  TO  ORC-DBPATH
                            PERFORM   910-SYUMEI-READ-SEC
                 END-PERFORM
      *    収納明細クローズ
           MOVE     "SYUMEI-KEY2"    TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
           END-IF
      *
      *収納明細作成処理
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           MOVE    IDX1                TO  SMEI-DENPEDANUM
      *入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *請求金額
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *入返金区分
           MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    LNK-SCNYUKIN-FURIWAKEMONEY(IDX)
                                       TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    LNK-SCNYUKIN-NYUKIN-YMD
                                       TO  SMEI-NYUHEN-YMD
      *入金方法
           MOVE    "01"                TO  SMEI-NYUKIN-HOHO
      *状態区分
           MOVE    "2"                 TO  SMEI-JOUTAIKBN
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "SYUMEI-KEY"        TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              NOT =   ZERO
                   DISPLAY "ORCSNYUKIN SYUMEI INS ERR:"  MCP-RC  "."
                   MOVE    14          TO  LNK-SCNYUKIN-RC
           END-IF
           .
       310-SYUMEI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "PTINF-KEY"         TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "PTINF-KEY"         TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                            MCPAREA
                                            ORCMCPAREA
                                            MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   INITIALIZE                     PTINF-REC
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                 PTINF-REC
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "PTINF-KEY"        TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       910-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUNOU-REC
               MOVE    ZERO                TO  FLG-SYUNOU
           ELSE
               INITIALIZE                      SYUNOU-REC
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター読込
      *****************************************************************
       910-SYUMEI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       910-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
