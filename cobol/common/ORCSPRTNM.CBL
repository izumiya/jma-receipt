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
       PROGRAM-ID.             ORCSPRTNM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : オンライン帳票名・出力先プリンタ名取得
      *  返却エラーコード  : 00:正常
      *                    　10:対象データなし
      *                      99:パラメータ不正
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03.06.27    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.00.01     NACL-多々納  04/12/16   カルテ３号紙（入院）追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-NYUGAIKBN       PIC X(01).
      *
           03  WRK-KBNCD           PIC X(08).
           03  WRK-PRTPG           PIC X(20).
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *　　帳票名システム管理（外来）
           COPY    "CPSK1032.INC".
      *　　帳票名システム管理（入院）
           COPY    "CPSK1035.INC".
      *
      *    出力先プリンタ名割り当て情報（外来）
           COPY  "CPSK1031.INC".
      *
      *    出力先プリンタ名割り当て情報（入院）
           COPY  "CPSK1034.INC".
      *
           COPY    "MCPDATA.INC".
      * 
           COPY    "CPORCMCP.INC".  
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSPRTNMAREA
           SPA-AREA
           SYSKANRI-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           MOVE    ZERO                TO  ORCSPRTNM-RC
      *
      *    基準日
           IF      ORCSPRTNM-SRYYMD    =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-SRYYMD
           ELSE
               MOVE    ORCSPRTNM-SRYYMD    TO  WRK-SRYYMD
           END-IF
      *
      *    入外区分
           IF      ORCSPRTNM-NYUGAIKBN     =   SPACE  OR  ZERO
               MOVE    SPA-NYUGAIKBN           TO  WRK-NYUGAIKBN
           ELSE
               MOVE    ORCSPRTNM-NYUGAIKBN     TO  WRK-NYUGAIKBN
           END-IF
      *
      *    パラメタチェック
           EVALUATE    ORCSPRTNM-KBN
               WHEN    "1"
      *            オンライン帳票名取得
                   PERFORM 100-PGNAME-SYORI-SEC
               WHEN    "2"
      *            出力先プリンタ名取得
                   PERFORM 200-PRTNAME-SYORI-SEC
               WHEN    OTHER
                   MOVE    99              TO  ORCSPRTNM-RC
           END-EVALUATE
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    オンライン帳票名取得処理
      *****************************************************************
       100-PGNAME-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  ORCSPRTNM-PRTPG
           MOVE    SPACE               TO  WRK-PRTPG
      *
      *    職員毎のプログラム名取得
           MOVE    ORCSPRTNM-KBNCD     TO  WRK-KBNCD
           MOVE    SPA-STAFFCD         TO  WRK-KBNCD(1:5)
           PERFORM 1001-PGNAME-SET-SEC
      *
           IF      WRK-PRTPG           =   SPACE
      *        標準のプログラム名取得
               MOVE    ORCSPRTNM-KBNCD     TO  WRK-KBNCD
               PERFORM 1001-PGNAME-SET-SEC
           END-IF
      *
           IF      WRK-PRTPG           =   SPACE
      *        標準のプログラム名とする
               PERFORM 11011-HYOJUN-PRTPG-SEC
               IF      ORCSPRTNM-PRTPG        =   SPACE
                   MOVE    10                 TO  ORCSPRTNM-RC
               END-IF
           ELSE
               MOVE    WRK-PRTPG           TO  ORCSPRTNM-PRTPG
           END-IF
      *
           .
       110-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名システム管理検索処理
      *****************************************************************
       11011-HYOJUN-PRTPG-SEC                SECTION.
      *
           IF      WRK-NYUGAIKBN       =   "1"
      *        入院
               EVALUATE    ORCSPRTNM-KBNCD
      *            カルテ
                   WHEN    "00000001"
                       MOVE    "ORCHCN01"      TO  ORCSPRTNM-PRTPG
      *            退院証明書
                   WHEN    "00000002"
                       MOVE    "ORCHCN02"      TO  ORCSPRTNM-PRTPG
      *            請求書
                   WHEN    "00000003"
                       MOVE    "ORCHCN03"      TO  ORCSPRTNM-PRTPG
      *            カルテ（３号紙）
                   WHEN    "00000004"
                       MOVE    "ORCHCN33"      TO  ORCSPRTNM-PRTPG
      *            入院処方箋
                   WHEN    "00000005"
                       MOVE    "ORCHC501"      TO  ORCSPRTNM-PRTPG
      *            注射箋
                   WHEN    "00000006"
                       MOVE    "ORCHC502"      TO  ORCSPRTNM-PRTPG
      *            指示箋
                   WHEN    "00000007"
                       MOVE    "ORCHC503"      TO  ORCSPRTNM-PRTPG
               END-EVALUATE
           ELSE
      *        外来
               EVALUATE    ORCSPRTNM-KBNCD
      *            カルテ
                   WHEN    "00000001"
                       MOVE    "ORCHC01"       TO  ORCSPRTNM-PRTPG
      *            処方せん
                   WHEN    "00000002"
                       MOVE    "ORCHCM19"      TO  ORCSPRTNM-PRTPG
      *            請求書
                   WHEN    "00000003"
                       MOVE    "ORCHC03"       TO  ORCSPRTNM-PRTPG
      *            薬剤情報
                   WHEN    "00000004"
                       MOVE    "ORCHC30"       TO  ORCSPRTNM-PRTPG
      *            カルテ（３号紙）
                   WHEN    "00000005"
                       MOVE    "ORCHC33"       TO  ORCSPRTNM-PRTPG
               END-EVALUATE
           END-IF
      *
           .
       11011-HYOJUN-PRTPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名システム管理検索処理
      *****************************************************************
       1001-PGNAME-SET-SEC                SECTION.
      *
           IF      WRK-NYUGAIKBN       =   "1"
      *        入院
               PERFORM 10012-1035-SET-SEC
           ELSE
      *        外来
               PERFORM 10011-1032-SET-SEC
           END-IF
      *
           .
       1001-PGNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名システム管理検索処理（外来）
      *****************************************************************
       10011-1032-SET-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-1032-REC
           INITIALIZE                      SYS-1032-REC
           MOVE    "1032"              TO  SYS-1032-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1032-KBNCD
      *
           MOVE    SYS-1032-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1032-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1032-REC
      *        独自開発分
               IF      SYS-1032-ORCACSTCHK     =   "T"
                   MOVE    SYS-1032-ORCACSTNM  TO  WRK-PRTPG
               ELSE
      *        ＯＲＣＡ標準分
                   MOVE    SYS-1032-ORCAORGNM  TO  WRK-PRTPG
               END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       10011-1032-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名システム管理検索処理（入院）
      *****************************************************************
       10012-1035-SET-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-1035-REC
           INITIALIZE                      SYS-1035-REC
           MOVE    "1035"              TO  SYS-1035-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1035-KBNCD
      *
           MOVE    SYS-1035-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1035-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1035-REC
      *        独自開発分
               IF      SYS-1035-ORCACSTCHK     =   "T"
                   MOVE    SYS-1035-ORCACSTNM  TO  WRK-PRTPG
               ELSE
      *        ＯＲＣＡ標準分
                   MOVE    SYS-1035-ORCAORGNM  TO  WRK-PRTPG
               END-IF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       10011-1032-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名取得
      *****************************************************************
       200-PRTNAME-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYSKANRI-REC
           MOVE    ZERO                TO  FLG-OK
      *
      *    職員毎の出力先プリンタ名取得
           MOVE    SPACE               TO  WRK-KBNCD
           MOVE    SPA-STAFFCD         TO  WRK-KBNCD(1:5)
           PERFORM 2001-PRTNAME-SET-SEC
      *
           IF      FLG-OK              =   ZERO
      *        標準の取得
               MOVE    "*"                 TO  WRK-KBNCD
               PERFORM 2001-PRTNAME-SET-SEC
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    10                  TO  ORCSPRTNM-RC
           END-IF
      *
           .
       200-PRTNAME-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名取得処理
      *****************************************************************
       2001-PRTNAME-SET-SEC                SECTION.
      *
           IF      WRK-NYUGAIKBN       =   "1"
      *        入院
               PERFORM 20012-1034-SET-SEC
           ELSE
      *        外来
               PERFORM 20011-1031-SET-SEC
           END-IF
      *
           .
       2001-PRTNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名検索処理（外来）
      *****************************************************************
       20011-1031-SET-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                      SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1031-KBNCD
      *
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1031-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
               MOVE    1                   TO  FLG-OK
               MOVE    SYS-1031-REC        TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       20011-1031-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名検索処理（入院）
      *****************************************************************
       20012-1034-SET-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-1034-REC
           INITIALIZE                      SYS-1034-REC
           MOVE    "1034"              TO  SYS-1034-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-1034-KBNCD
      *
           MOVE    SYS-1034-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           MOVE    WRK-SRYYMD          TO  ORC-DBYMD
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1034-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1034-REC
               MOVE    1                   TO  FLG-OK
               MOVE    SYS-1034-REC        TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       20012-1034-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
