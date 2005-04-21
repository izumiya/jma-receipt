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
       PROGRAM-ID.             ORCGM02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 業務選択（Ｍ０２）
      *  管理者            : 
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/04  105 保険者マスタの追加      
      *  01.01.01    MCC-多々納   01/09/20  JOINの修正      
      *  01.01.02    MCC-藤原     01/09/27  MVERの追加  
      *  01.02.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更  
      *  01.02.02    MCC-多々納   02/02/22  MID1の追加
      *  01.02.03    MCC-太田     02/06/20  106 人名辞書マスタの追加
      *  01.02.04    NACL-森脇    02/07/30  107 薬剤情報マスタの追加
      *  01.02.05    NACL-伊藤    04/02/05  M97の追加
      *  01.02.06    NACL-森脇    04/08/04  108 住所マスタの追加
      *  01.02.07    NACL-多々納  05/01/21  109 ヘルプマスタ追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
       01  SPA-M02.
           03  SPA-M02-AREA.
               05  SPA-CTLSEQ          PIC 9(02).
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-GSEL    PIC 9(03).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  IDX                   PIC 9(03).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-GNO             PIC X(03).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  PRO-ID              PIC X(10).
           03  PID                 PIC X(10).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    システム管理マスタ領域
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".     
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *****COPY    "MCPAREA.INC".
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "M96.INC".
           COPY    "M97.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           MOVE    SPA-FREE        TO  SPA-M02
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SYS-1010-REC    TO  SPA-SYS-1010-REC
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "M02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-M02-AREA
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               PERFORM 320-SYS-1010-SPASET-SEC
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       320-SYS-1010-SPASET-SEC              SECTION.
      *
      *    職員情報
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-OPID            TO  SYS-1010-USERID
           MOVE    SPA-SYSYMD          TO  ORC-DBYMD
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "SYSKANRI-KEY3"     TO  WRK-PATH
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
      *2001.9.27 強制プロテクト（ＰＧ未作成）
           MOVE    "0"                 TO  SYS-1010-GSRAUTH(3:1)
                                           SYS-1010-GSRAUTH(4:1)
                                           SYS-1010-GSRAUTH(18:1)
                                           SYS-1010-GSRAUTH(26:1)
                                           SYS-1010-GSRAUTH(32:1)
                                           SYS-1010-GSRAUTH(41:1)
                                           SYS-1010-GSRAUTH(46:1)
                                           SYS-1010-GSRAUTH(47:1)
      *2001.9.27 強制プロテクト（ＰＧ未作成）END
      *
      *TEST ヘルプを追加するまで
           MOVE    "1"                  TO SYS-1010-GSRAUTH(69:1)
      *TEST ヘルプを追加するまで
      *****MOVE    SYS-1010-REC         TO  SPA-WORK   
           .
       320-SYS-1010-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    各業務移動へ
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM 400-GYOMU-SEL-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-KEY-ENT-SEC
           END-EVALUATE
      *
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4101-KEY-ENT-SEC            SECTION.
      *
           MOVE    M02-SELNUM       TO  SPA-GMN-GSEL
      *
           IF      SPA-GMN-GSEL    NOT =   ZERO
               IF      SYS-1010-GSRAUTH(1:1)     =  "0"
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-GSEL        TO  WRK-GNO
                   PERFORM 420-GYOMU-START-SEC
               END-IF
           END-IF
           .
       4101-KEY-ENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       400-GYOMU-SEL-SEC            SECTION.
      *
           IF      MCP-WIDGET(1:1)     =   "G"
               MOVE   MCP-WIDGET(2:3)      TO  WRK-GNO
           END-IF
      *
           PERFORM 420-GYOMU-START-SEC
           .
       400-GYOMU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務起動　処理
      *****************************************************************
       420-GYOMU-START-SEC         SECTION.
      *
          MOVE    SPACE               TO  SPA-SAKIPG
          EVALUATE    WRK-GNO
      * システム管理マスタ
               WHEN    "101"
                   IF      SYS-1010-GSRAUTH(61:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "W01"           TO  SPA-SAKIPG
                   END-IF
      * 点数マスタ
               WHEN    "102"
                   IF      SYS-1010-GSRAUTH(62:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "Z01"           TO  SPA-SAKIPG
                   END-IF
      * チェックマスタ
               WHEN    "103"
                   IF      SYS-1010-GSRAUTH(63:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "X01"           TO  SPA-SAKIPG
                   END-IF
      * 保険番号マスタ
               WHEN    "104"
                   IF      SYS-1010-GSRAUTH(64:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "V01"           TO  SPA-SAKIPG
                   END-IF
      * 保険者マスタ
               WHEN    "105"
                   IF      SYS-1010-GSRAUTH(65:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N01"           TO  SPA-SAKIPG
                   END-IF
      * 人名辞書マスタ
               WHEN    "106"
                   IF      SYS-1010-GSRAUTH(66:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N11"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.04)----START-----------------
      * 薬剤情報マスタ
               WHEN    "107"
                   IF      SYS-1010-GSRAUTH(67:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N21"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.04)----END-------------------
      *----(01.02.06)----START-----------------
      * 住所マスタ
               WHEN    "108"
                   IF      SYS-1010-GSRAUTH(68:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N31"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.06)----END-------------------
      * ヘルプマスタ
               WHEN    "109"
      *!!          IF      SYS-1010-GSRAUTH(69:1)    =  "0"
      *!!              MOVE    "0002"          TO  SPA-ERRCD
      *!!          ELSE
                       MOVE    "N41"           TO  SPA-SAKIPG
      *!!          END-IF
              WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE.
      *
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  LINKAREA
      *
               IF      SPA-SAKIPG      NOT =  SPACE
                   MOVE    SPACE               TO  SPA-MOTOPG2
                   MOVE    "M02"               TO  SPA-MOTOPG
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .     
      *
       420-GYOMU-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "M02"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "M01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "M02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  M02
      *      *
           IF      SYS-1010-GSRAUTH(1:1)    =  "0"
               CONTINUE
           ELSE
               MOVE    SPA-GMN-GSEL        TO  M02-SELNUM
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *****MOVE    SPA-WORK            TO  SYS-1010-REC
      *
      *----(01.02.06)----START-----------------
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       9
               MOVE    WIDGET-INSENSITIVE      TO  M02-STATE-O (IDX)
           END-PERFORM           
      *----(01.02.06)----END-------------------
      *
           IF      SYS-1010-GSRAUTH(1:1)    =  "0"
               CONTINUE
           ELSE 
               IF      SYS-1010-GSRAUTH(61:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G101-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(62:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G102-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(63:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G103-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(64:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G104-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(65:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G105-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(66:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G106-STATE
               END-IF
      *----(01.02.04)----START-----------------
               IF      SYS-1010-GSRAUTH(67:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G107-STATE
               END-IF
      *----(01.02.04)----END-------------------
      *----(01.02.06)----START-----------------
               IF      SYS-1010-GSRAUTH(68:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G108-STATE
               END-IF
      *----(01.02.06)----END-------------------
      *!!      IF      SYS-1010-GSRAUTH(69:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G109-STATE
      *!!      END-IF
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SELNUM"    TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "この業務は選択できません"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M02"                TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH            TO  ORC-DBPATH
               CALL    "ORCMCPSUB"          USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
