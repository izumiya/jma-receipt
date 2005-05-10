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
       PROGRAM-ID.             ORCSNYUKINS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : まとめ入金サブ（未収まとめ入金画面用）
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
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
           COPY    "CPORCSNYUKINS.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSNYUKINAREAS
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-SCNYUKINS-RC
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
           IF       (LNK-SCNYUKINS-RC   NOT =   ZERO)
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-SCNYUKINS-RC      =   ZERO
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
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    LNK-SCNYUKINS-HOSPID    TO  PTINF-HOSPID
           MOVE    LNK-SCNYUKINS-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-SCNYUKINS-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
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
      *    収納更新
           PERFORM    VARYING  IDX   FROM  1  BY  1
             UNTIL    (IDX        >   LNK-SCNYUKINS-MAX )   OR
                      (IDX        >   400)                     OR
                      (LNK-SCNYUKINS-RC   NOT =    ZERO)
                       PERFORM   300-SYUNOU-UPD-SEC
                       PERFORM   310-SYUMEI-INS-SEC
           END-PERFORM
      *
      *    収納クローズ
           MOVE    WRK-SYUNOU-DBPATH   TO  ORC-DBPATH
           PERFORM   910-DB-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       300-SYUNOU-UPD-SEC                SECTION.
      *
           INITIALIZE                          SYUNOU-REC
           MOVE    LNK-SCNYUKINS-HOSPID    TO  SYU-HOSPID
           MOVE    LNK-SCNYUKINS-PTID      TO  SYU-PTID
           MOVE    LNK-SCNYUKINS-NYUGAIKBN(IDX)
                                           TO  SYU-NYUGAIKBN
           MOVE    LNK-SCNYUKINS-DENPNUM(IDX)
                                           TO  SYU-DENPNUM
           MOVE    "SYUNOU-KEY"            TO  ORC-DBPATH
           PERFORM 910-SYUNOU-SELECT-SEC
           IF      FLG-SYUNOU              NOT =   ZERO
               MOVE    11                  TO  LNK-SCNYUKINS-RC
           ELSE
               MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU             NOT =   ZERO
                   MOVE   11                  TO  LNK-SCNYUKINS-RC
               ELSE
                   ADD     LNK-SCNYUKINS-NYUKIN-MONEY(IDX)
                                              TO  SYU-NYUKIN-TOTAL
                   IF      LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
                       ADD     1              TO  SYU-NYUKIN-KAISU
                   END-IF
                   IF      SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL
                       MOVE   "2"          TO  SYU-DENPJTIKBN
                   ELSE
                       MOVE   "1"          TO  SYU-DENPJTIKBN
                   END-IF
      *
                   MOVE    SMCNDATE-YMD    TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  SYU-UPHMS
      *
      *           収納更新
                  MOVE    SYUNOU-REC          TO  MCPDATA-REC
                  MOVE    "DBUPDATE"          TO  MCP-FUNC
                  MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
                  CALL    "ORCMCPSUB"         USING
                                              MCPAREA
                                              ORCMCPAREA
                                       MCPDATA-REC
                   IF      MCP-RC              NOT =   ZERO
                       DISPLAY "ORCSNYUKINS SYUNOU UPD ERR:"
                                MCP-RC  "."
                       MOVE    11          TO  LNK-SCNYUKINS-RC
                   END-IF
               END-IF
           END-IF
      *
      *    収納クローズ
           MOVE    "SYUNOU-KEY"         TO  ORC-DBPATH
           PERFORM    910-DB-CLOSE-SEC
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
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
               MOVE    "1"             TO  SMEI-NYUHEN-KBN
           ELSE
               MOVE    "2"             TO  SMEI-NYUHEN-KBN
           END-IF
      *入返金額
           MOVE    LNK-SCNYUKINS-NYUKIN-MONEY(IDX)
                                       TO  SMEI-NYUHEN-MONEY
      *入返金日
           MOVE    LNK-SCNYUKINS-NYUKIN-YMD
                                       TO  SMEI-NYUHEN-YMD
      *入金方法
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
               MOVE    "01"            TO  SMEI-NYUKIN-HOHO
           ELSE
               MOVE    SPACE           TO  SMEI-NYUHEN-KBN
           END-IF
      *状態区分
           IF  LNK-SCNYUKINS-NYUKIN-MONEY(IDX)  >  ZERO
               MOVE    "2"             TO  SMEI-JOUTAIKBN
           ELSE
               MOVE    "4"             TO  SMEI-JOUTAIKBN
           END-IF
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
                   DISPLAY "ORCSNYUKINS SYUMEI INS ERR:"  MCP-RC  "."
                   MOVE    12          TO  LNK-SCNYUKINS-RC
           END-IF
           .
       310-SYUMEI-INS-EXT.
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
           MOVE    "DBCLOSE"            TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
