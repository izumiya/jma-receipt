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
       PROGRAM-ID.                     ORCGW60.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : データチェック機能情報２
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/09    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
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
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
       01  SPA-W60.
           03  SPA-GMN-CUR                     PIC 9(03).
           03  SPA-GMN-AREA.
               05  SPA-GMN-BYOSRYKAKBN-G.
                   07  SPA-GMN-BYOSRYKAKBN     PIC X(01).
                   07  SPA-GMN-BYOSRYKAKBNFL   PIC X(01).
                   07  SPA-GMN-BYOSRYKAKBNMEI  PIC X(08).
               05  SPA-GMN-SYUBYOCHK-G.
                   07  SPA-GMN-SYUBYOCHK       PIC X(01).
                   07                          PIC X(01).
                   07  SPA-GMN-SYUBYOCHKMEI    PIC X(08).
               05  SPA-GMN-UTAGAIKKN           PIC 9(01).
               05  SPA-GMN-KYUSEIKKNKBN-G.
                   07  SPA-GMN-KYUSEIKKNKBN    PIC X(01).
                   07  SPA-GMN-KYUSEIKKNKBNFL  PIC X(01).
                   07  SPA-GMN-KYUSEIKKNKBNMEI PIC X(08).
               05  SPA-GMN-CHKMTRX-G.
                   07  SPA-GMN-CHKMTRX         PIC X(01)
                                               OCCURS  5.
               05  SPA-GMN-TENKITEKBYOKBN-G.
                   07  SPA-GMN-TENKITEKBYOKBN  PIC X(01).
                   07  SPA-GMN-TENKITEKBYOKBNFL  PIC X(01).
                   07  SPA-GMN-TENKITEKBYOKBNMEI PIC X(18).
               05  SPA-GMN-YBKENSAYAKUKBN-G.
                   07  SPA-GMN-YBKENSAYAKUKBN  PIC X(01).
                   07  SPA-GMN-YBKENSAYAKUKBNFL PIC X(01).
                   07  SPA-GMN-YBKENSAYAKUKBNMEI PIC X(30).
               05  SPA-GMN-YBUTAGAIKBN-G.
                   07  SPA-GMN-YBUTAGAIKBN     PIC X(01).
                   07  SPA-GMN-YBUTAGAIKBNFL   PIC X(01).
                   07  SPA-GMN-YBUTAGAIKBNMEI  PIC X(30).
               05  SPA-GMN-SRYHEIWRNKBN-G.
                   07  SPA-GMN-SRYHEIWRNKBN    PIC X(01).
                   07                          PIC X(01).
                   07  SPA-GMN-SRYHEIWRNKBNMEI PIC X(30).
               05  SPA-GMN-REIGAICHKKBN-G.
                   07  SPA-GMN-REIGAICHKKBN    PIC X(01).
                   07  SPA-GMN-REIGAICHKKBNFL  PIC X(01).
                   07  SPA-GMN-REIGAICHKKBNMEI PIC X(18).
               05  SPA-GMN-PRTSRTKBN-G.
                   07  SPA-GMN-PRTSRTKBN       PIC X(01).
                   07  SPA-GMN-PRTSRTKBNFL     PIC X(01).
                   07  SPA-GMN-PRTSRTKBNMEI    PIC X(20).
               05  SPA-GMN-PRTPTLSTKBN-G.
                   07  SPA-GMN-PRTPTLSTKBN     PIC X(01).
                   07  SPA-GMN-PRTPTLSTKBNFL   PIC X(01).
                   07  SPA-GMN-PRTPTLSTKBNMEI  PIC X(20).
               05  SPA-GMN-ERR990009KBN-G.
                   07  SPA-GMN-ERR990009KBN    PIC X(01).
                   07  SPA-GMN-ERR990009KBNFL  PIC X(01).
                   07  SPA-GMN-ERR990009KBNMEI PIC X(20).
           03  SPA-GMN-CMB-AREA.
               05  SPA-GMN-BYOSRYKAKBNLST-G.
                   07  SPA-GMN-BYOSRYKAKBN-MAX     PIC 9(03).
                   07  SPA-GMN-BYOSRYKAKBNLST      OCCURS   2.
                       09  SPA-GMN-BYOSRYKAKBNL    PIC X(01).
                       09  SPA-GMN-BYOSRYKAKBNFLL  PIC X(01).
                       09  SPA-GMN-BYOSRYKAKBNMEIL PIC X(08).
               05  SPA-GMN-SYUBYOCHKLST-G.
                   07  SPA-GMN-SYUBYOCHK-MAX       PIC 9(03).
                   07  SPA-GMN-SYUBYOCHKLST        OCCURS   2.
                       09  SPA-GMN-SYUBYOCHKL      PIC X(01).
                       09  SPA-GMN-SYUBYOCHKFLL    PIC X(01).
                       09  SPA-GMN-SYUBYOCHKMEIL   PIC X(08).
               05  SPA-GMN-KYUSEIKKNKBNLST-G.
                   07  SPA-GMN-KYUSEIKKNKBN-MAX    PIC 9(03).
                   07  SPA-GMN-KYUSEIKKNKBNLST     OCCURS   15.
                       09  SPA-GMN-KYUSEIKKNKBNL   PIC X(01).
                       09  SPA-GMN-KYUSEIKKNKBNFLL PIC X(01).
                       09  SPA-GMN-KYUSEIKKNKBNMEIL PIC X(08).
               05  SPA-GMN-TENKITEKBYOKBNLST-G.
                   07  SPA-GMN-TENKITEKBYOKBN-MAX  PIC 9(03).
                   07  SPA-GMN-TENKITEKBYOKBNLST   OCCURS   2.
                       09  SPA-GMN-TENKITEKBYOKBNL PIC X(01).
                       09  SPA-GMN-TENKITEKBYOKBNFLL PIC X(01).
                       09  SPA-GMN-TENKITEKBYOKBNMEIL PIC X(18).
               05  SPA-GMN-YBKENSAYAKUKBNLST-G.
                   07  SPA-GMN-YBKENSAYAKUKBN-MAX  PIC 9(03).
                   07  SPA-GMN-YBKENSAYAKUKBNLST   OCCURS   2.
                       09  SPA-GMN-YBKENSAYAKUKBNL PIC X(01).
                       09  SPA-GMN-YBKENSAYAKUKBNFLL  PIC X(01).
                       09  SPA-GMN-YBKENSAYAKUKBNMEIL PIC X(30).
               05  SPA-GMN-YBUTAGAIKBNLST-G.
                   07  SPA-GMN-YBUTAGAIKBN-MAX     PIC 9(03).
                   07  SPA-GMN-YBUTAGAIKBNLST      OCCURS   2.
                       09  SPA-GMN-YBUTAGAIKBNL    PIC X(01).
                       09  SPA-GMN-YBUTAGAIKBNFLL  PIC X(01).
                       09  SPA-GMN-YBUTAGAIKBNMEIL PIC X(30).
               05  SPA-GMN-SRYHEIWRNKBNLST-G.
                   07  SPA-GMN-SRYHEIWRNKBN-MAX    PIC 9(03).
                   07  SPA-GMN-SRYHEIWRNKBNLST     OCCURS   2.
                       09  SPA-GMN-SRYHEIWRNKBNL   PIC X(01).
                       09  SPA-GMN-SRYHEIWRNKBNFLL PIC X(01).
                       09  SPA-GMN-SRYHEIWRNKBNMEIL PIC X(30).
               05  SPA-GMN-REIGAICHKKBNLST-G.
                   07  SPA-GMN-REIGAICHKKBN-MAX    PIC 9(03).
                   07  SPA-GMN-REIGAICHKKBNLST     OCCURS   2.
                       09  SPA-GMN-REIGAICHKKBNL   PIC X(01).
                       09  SPA-GMN-REIGAICHKKBNFLL PIC X(01).
                       09  SPA-GMN-REIGAICHKKBNMEIL PIC X(18).
               05  SPA-GMN-PRTSRTKBNLST-G.
                   07  SPA-GMN-PRTSRTKBN-MAX       PIC 9(03).
                   07  SPA-GMN-PRTSRTKBNLST        OCCURS   4.
                       09  SPA-GMN-PRTSRTKBNL      PIC X(01).
                       09  SPA-GMN-PRTSRTKBNFLL    PIC X(01).
                       09  SPA-GMN-PRTSRTKBNMEIL   PIC X(20).
               05  SPA-GMN-PRTPTLSTKBNLST-G.
                   07  SPA-GMN-PRTPTLSTKBN-MAX     PIC 9(03).
                   07  SPA-GMN-PRTPTLSTKBNLST      OCCURS   2.
                       09  SPA-GMN-PRTPTLSTKBNL    PIC X(01).
                       09  SPA-GMN-PRTPTLSTKBNFLL  PIC X(01).
                       09  SPA-GMN-PRTPTLSTKBNMEIL PIC X(20).
               05  SPA-GMN-ERR990009KBNLST-G.
                   07  SPA-GMN-ERR990009KBN-MAX    PIC 9(03).
                   07  SPA-GMN-ERR990009KBNLST     OCCURS   5.
                       09  SPA-GMN-ERR990009KBNL   PIC X(01).
                       09  SPA-GMN-ERR990009KBNFLL PIC X(01).
                       09  SPA-GMN-ERR990009KBNMEIL
                                                   PIC X(20).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-MTRX                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG                  PIC X(70).
           03  WRK-KANRICD                 PIC X(04).
           03  WRK-STYUKYMD                PIC X(08).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
      *
       01  CONST-AREA.
           03  CONST-CHKMTRX-MAX           PIC 9(03)   VALUE 5.
           03  CONST-CHKMTRX-VAL           PIC X(05)   VALUE "1 111".
           03  CONST-CHKMTRX-R REDEFINES   CONST-CHKMTRX-VAL.
               05  CONST-CHKMTRX           PIC X(01)   OCCURS  5.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1101.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK1103.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "W01.INC".
           COPY    "W02.INC".
           COPY    "W03.INC".
           COPY    "W04.INC".
           COPY    "W05.INC".
           COPY    "W06.INC".
           COPY    "W07.INC".
           COPY    "W08.INC".
           COPY    "W09.INC".
           COPY    "W10.INC".
           COPY    "W11.INC".
           COPY    "W111.INC".
           COPY    "W12.INC".
           COPY    "W121.INC".
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W201.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W32.INC".
           COPY    "W33.INC".
           COPY    "W34.INC".
           COPY    "W35.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W64.INC".
           COPY    "W65.INC".
           COPY    "W66.INC".
           COPY    "W67.INC".
           COPY    "W96.INC".
           COPY    "W98.INC".
           COPY    "W981.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W60
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W60         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W60    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-W60
           INITIALIZE                  SPA-W60
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    CONST-CHKMTRX (IDX-MTRX)
                                   TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
           END-PERFORM
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    SPACE           TO  WRK-STYUKYMD
           PERFORM 3101-SPASET-SEC
      *
           PERFORM 3102-SPASET-SEC
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  WRK-CMB-CD
           PERFORM 4200-BYOSRYKAKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-BYOSRYKAKBNLST (1)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-BYOSRYKAKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SYUBYOCHK TO  WRK-CMB-CD
           PERFORM 4200-SYUBYOCHKLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYUBYOCHKLST (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SYUBYOCHK-G
           END-IF
      *
           IF    ( SPA-GMN-UTAGAIKKN    =   ZERO )
               MOVE    2            TO  SPA-GMN-UTAGAIKKN
           END-IF
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN TO  WRK-CMB-CD
           PERFORM 4200-KYUSEIKKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KYUSEIKKNKBN-G
           END-IF
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN TO  WRK-CMB-CD
           PERFORM 4200-TENKITEKBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (1)
                                       TO  SPA-GMN-TENKITEKBYOKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-TENKITEKBYOKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN TO  WRK-CMB-CD
           PERFORM 4200-YBKENSAYAKUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (1)
                                       TO  SPA-GMN-YBKENSAYAKUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBKENSAYAKUKBN-G
           END-IF
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO  WRK-CMB-CD
           PERFORM 4200-YBUTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBUTAGAIKBNLST (1)
                                       TO  SPA-GMN-YBUTAGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-YBUTAGAIKBN-G
           END-IF
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (1)
                                       TO  SPA-GMN-SRYHEIWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-SRYHEIWRNKBN-G
           END-IF
      *
           MOVE    SPA-GMN-ERR990009KBN TO  WRK-CMB-CD
           PERFORM 4200-ERR990009KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERR990009KBNLST (1)
                                       TO  SPA-GMN-ERR990009KBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-ERR990009KBN-G
           END-IF
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO  WRK-CMB-CD
           PERFORM 4200-REIGAICHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-REIGAICHKKBNLST (1)
                                       TO  SPA-GMN-REIGAICHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-REIGAICHKKBN-G
           END-IF
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTSRTKBN-G
           END-IF
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO  WRK-CMB-CD
           PERFORM 4200-PRTPTLSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTPTLSTKBNLST (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTPTLSTKBN-G
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理(システム管理より)
      *****************************************************************
       3101-SPASET-SEC                 SECTION.
      *
           INITIALIZE                          SYS-1101-REC
           MOVE    "1101"                  TO  SYS-1101-KANRICD
           MOVE    "*"                     TO  SYS-1101-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD
                                           TO  SYS-1101-STYUKYMD
                                               SYS-1101-EDYUKYMD
           MOVE    SYS-1101-REC            TO  SYSKANRI-REC
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           ELSE
               PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           END-IF
           IF    ( FLG-SYSKANRI        =   ZERO )
      *
               MOVE    SYSKANRI-REC        TO  SYS-1101-REC
      *
               PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                       UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
                   MOVE    SYS-1101-BYOCHKPTN(IDX-MTRX)
                                       TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
               END-PERFORM
      *
               MOVE    SYS-1101-BYOSRYKAKBN
                                       TO  SPA-GMN-BYOSRYKAKBN
      *
               MOVE    SYS-1101-SYUBYOCHKKBN
                                       TO  SPA-GMN-SYUBYOCHK
      *
               IF    ( SYS-1101-UTAGAIKKN-X  IS NUMERIC )
      *
                   MOVE    SYS-1101-UTAGAIKKN
                                       TO  SPA-GMN-UTAGAIKKN
               ELSE
                   MOVE    2           TO  SPA-GMN-UTAGAIKKN
               END-IF
      *
               IF    ( SYS-1101-KYUSEIKKN-X  IS NUMERIC )
      *
                   EVALUATE    SYS-1101-KYUSEIKKN
                   WHEN    0
                       MOVE    "0"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    1
                       MOVE    "1"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    2
                       MOVE    "2"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    3
                       MOVE    "3"     TO  SPA-GMN-KYUSEIKKNKBN
                   WHEN    4
                       MOVE    "4"     TO  SPA-GMN-KYUSEIKKNKBN
                   END-EVALUATE
      *
               END-IF
      *
               MOVE    SYS-1101-PRTSRTKBN
                                       TO  SPA-GMN-PRTSRTKBN
      *
               MOVE    SYS-1101-PRTPTLSTKBN
                                       TO  SPA-GMN-PRTPTLSTKBN
      *
               MOVE    SYS-1101-YBUTAGAIKBN
                                       TO  SPA-GMN-YBUTAGAIKBN
      *
               MOVE    SYS-1101-SRYHEIWRNKBN
                                       TO  SPA-GMN-SRYHEIWRNKBN
      *
               MOVE    SYS-1101-REIGAICHKKBN
                                       TO  SPA-GMN-REIGAICHKKBN
      *
               MOVE    SYS-1101-YBKENSAYAKUKBN
                                       TO  SPA-GMN-YBKENSAYAKUKBN
               MOVE    SYS-1101-TENKITEKBYOKBN
                                       TO  SPA-GMN-TENKITEKBYOKBN
      *
               MOVE    SYS-1101-STYUKYMD
                                       TO  WRK-STYUKYMD
      *
           END-IF
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理(チェックマスタ４より)
      *****************************************************************
       3102-SPASET-SEC                 SECTION.
      *
           INITIALIZE                          CK4-1103-REC
           MOVE    "1103"                  TO  CK4-1103-KANRICD
           MOVE    WRK-STYUKYMD            TO  CK4-1103-STYUKYMD
                                               CK4-1103-EDYUKYMD
           MOVE    CK4-1103-REC            TO  CHK004-REC
           PERFORM 900-CHK004-KEY2-SEL-SEC
           MOVE    CHK004-REC              TO  CK4-1103-REC
      *
           PERFORM UNTIL ( FLG-CHK004      NOT =   ZERO )
      *
               EVALUATE    CK4-1103-KBNCD
               WHEN    "99"
                   MOVE    CK4-1103-ERRKBN (9)
                                           TO  SPA-GMN-ERR990009KBN
               END-EVALUATE
      *
               PERFORM 900-CHK004-KEY2-FET-SEC
               MOVE    CHK004-REC      TO  CK4-1103-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3102-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    診療科別チェックコンボ設定処理
           PERFORM 3101-BYOSRYKAKBNLST-SEC
      *
      *    主病名チェックコンボ設定処理
           PERFORM 3101-SYUBYOCHKLST-SEC
      *
      *    急性病名期間チェックコンボ設定処理
           PERFORM 3101-KYUSEIKKNKBNLST-SEC
      *
      *    過月転帰済適応病名コンボ設定処理
           PERFORM 3101-TENKITEKBYOKBNLST-SEC
      *
      *    検査薬剤対象コンボ設定処理
           PERFORM 3101-YBKENSAYAKUKBNLST-SEC
      *
      *    薬剤と病名疑いコンボ設定処理
           PERFORM 3101-YBUTAGAIKBNLST-SEC
      *
      *    併用算定警告エラーコンボ設定処理
           PERFORM 3101-SRYHEIWRNKBNLST-SEC
      *
      *    その他のチェックエラーコンボ設定処理
           PERFORM 3101-ERR990009KBNLST-SEC
      *
      *    患者毎の例外設定使用コンボ設定処理
           PERFORM 3101-REIGAICHKKBNLST-SEC
      *
      *    帳票出力順コンボ設定処理
           PERFORM 3101-PRTSRTKBNLST-SEC
      *
      *    患者別チェックリスト設定処理
           PERFORM 3101-PRTPTLSTKBNLST-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェックコンボ設定処理
      *****************************************************************
       3101-BYOSRYKAKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-BYOSRYKAKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-BYOSRYKAKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-BYOSRYKAKBNL    (1)
           MOVE    "する"          TO  SPA-GMN-BYOSRYKAKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-BYOSRYKAKBNL    (2)
           MOVE    "しない"        TO  SPA-GMN-BYOSRYKAKBNMEIL (2)
      *
           MOVE    SPA-GMN-BYOSRYKAKBNLST  (1)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
      *
           . 
       3101-BYOSRYKAKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェックコンボ設定処理
      *****************************************************************
       3101-SYUBYOCHKLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SYUBYOCHKLST-G
      *
           MOVE    2               TO  SPA-GMN-SYUBYOCHK-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SYUBYOCHKL    (1)
           MOVE    "する"          TO  SPA-GMN-SYUBYOCHKMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SYUBYOCHKL    (2)
           MOVE    "しない"        TO  SPA-GMN-SYUBYOCHKMEIL (2)
      *
           MOVE    SPA-GMN-SYUBYOCHKLST  (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
      *
           . 
       3101-SYUBYOCHKLST-EXT.
           EXIT.
      *****************************************************************
      *    急性病名期間チェックコンボ設定処理
      *****************************************************************
       3101-KYUSEIKKNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-KYUSEIKKNKBNLST-G
      *
           MOVE    5               TO  SPA-GMN-KYUSEIKKNKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-KYUSEIKKNKBNL    (1)
           MOVE    "しない"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-KYUSEIKKNKBNL    (2)
           MOVE    "１ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (2)
      *
           MOVE    "2"             TO  SPA-GMN-KYUSEIKKNKBNL    (3)
           MOVE    "２ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (3)
      *
           MOVE    "3"             TO  SPA-GMN-KYUSEIKKNKBNL    (4)
           MOVE    "３ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (4)
      *
           MOVE    "4"             TO  SPA-GMN-KYUSEIKKNKBNL    (5)
           MOVE    "４ヶ月"        TO  SPA-GMN-KYUSEIKKNKBNMEIL (5)
      *
           MOVE    SPA-GMN-KYUSEIKKNKBNLST  (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
      *
           . 
       3101-KYUSEIKKNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名コンボ設定処理
      *****************************************************************
       3101-TENKITEKBYOKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-TENKITEKBYOKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-TENKITEKBYOKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-TENKITEKBYOKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-TENKITEKBYOKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-TENKITEKBYOKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-TENKITEKBYOKBNMEIL (2)
      *
           MOVE    SPA-GMN-TENKITEKBYOKBNLST  (1)
                                   TO  SPA-GMN-TENKITEKBYOKBN-G
      *
           . 
       3101-TENKITEKBYOKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象コンボ設定処理
      *****************************************************************
       3101-YBKENSAYAKUKBNLST-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-YBKENSAYAKUKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBKENSAYAKUKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBKENSAYAKUKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-YBKENSAYAKUKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBKENSAYAKUKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-YBKENSAYAKUKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBNLST  (1)
                                   TO  SPA-GMN-YBKENSAYAKUKBN-G
      *
           . 
       3101-YBKENSAYAKUKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑いコンボ設定処理
      *****************************************************************
       3101-YBUTAGAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-YBUTAGAIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-YBUTAGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-YBUTAGAIKBNL    (1)
           MOVE    "適応病名とする"
                                   TO  SPA-GMN-YBUTAGAIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-YBUTAGAIKBNL    (2)
           MOVE    "適応病名としない"
                                   TO  SPA-GMN-YBUTAGAIKBNMEIL (2)
      *
           MOVE    SPA-GMN-YBUTAGAIKBNLST  (1)
                                   TO  SPA-GMN-YBUTAGAIKBN-G
      *
           . 
       3101-YBUTAGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラーコンボ設定処理
      *****************************************************************
       3101-SRYHEIWRNKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-SRYHEIWRNKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-SRYHEIWRNKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-SRYHEIWRNKBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-SRYHEIWRNKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-SRYHEIWRNKBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-SRYHEIWRNKBNMEIL (2)
      *
           MOVE    SPA-GMN-SRYHEIWRNKBNLST  (1)
                                   TO  SPA-GMN-SRYHEIWRNKBN-G
      *
           . 
       3101-SRYHEIWRNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    その他エラーコンボ設定処理
      *****************************************************************
       3101-ERR990009KBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-ERR990009KBNLST-G
      *
           MOVE    2               TO  SPA-GMN-ERR990009KBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-ERR990009KBNL    (1)
           MOVE    "チェックする"
                                   TO  SPA-GMN-ERR990009KBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-ERR990009KBNL    (2)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-ERR990009KBNMEIL (2)
      *
           MOVE    SPA-GMN-ERR990009KBNLST  (1)
                                   TO  SPA-GMN-ERR990009KBN-G
      *
           . 
       3101-ERR990009KBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定使用コンボ設定処理
      *****************************************************************
       3101-REIGAICHKKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-REIGAICHKKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-REIGAICHKKBN-MAX
      *
           MOVE    "0"             TO  SPA-GMN-REIGAICHKKBNL    (1)
           MOVE    "使用しない"    TO  SPA-GMN-REIGAICHKKBNMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-REIGAICHKKBNL    (2)
           MOVE    "使用する"      TO  SPA-GMN-REIGAICHKKBNMEIL (2)
      *
           MOVE    SPA-GMN-REIGAICHKKBNLST  (1)
                                   TO  SPA-GMN-REIGAICHKKBN-G
      *
           . 
       3101-REIGAICHKKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順コンボ設定処理
      *****************************************************************
       3101-PRTSRTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-PRTSRTKBNLST-G
      *
           MOVE    4               TO  SPA-GMN-PRTSRTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-PRTSRTKBNL    (1)
           MOVE    "患者番号順"    TO  SPA-GMN-PRTSRTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-PRTSRTKBNL    (2)
           MOVE    "カナ氏名順"    TO  SPA-GMN-PRTSRTKBNMEIL (2)
      *
           MOVE    "3"                 TO  SPA-GMN-PRTSRTKBNL    (3)
           MOVE    "保険、患者番号順"  TO  SPA-GMN-PRTSRTKBNMEIL (3)
      *
           MOVE    "4"                 TO  SPA-GMN-PRTSRTKBNL    (4)
           MOVE    "保険、カナ氏名順"  TO  SPA-GMN-PRTSRTKBNMEIL (4)
      *
           MOVE    SPA-GMN-PRTSRTKBNLST  (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
      *
           . 
       3101-PRTSRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト設定処理
      *****************************************************************
       3101-PRTPTLSTKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-PRTPTLSTKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-PRTPTLSTKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-PRTPTLSTKBNL    (1)
           MOVE    "作成しない"    TO  SPA-GMN-PRTPTLSTKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-PRTPTLSTKBNL    (2)
           MOVE    "作成する"      TO  SPA-GMN-PRTPTLSTKBNMEIL (2)
      *
           MOVE    SPA-GMN-PRTPTLSTKBNLST  (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
      *
           . 
       3101-PRTPTLSTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               IF    ( SPA-WIDCD            =   "1002" )
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 403-DEL-SEC
      *    チェックマトリクス
           WHEN    "CLICKED"       ALSO    "CHKMTRX01"
           WHEN    "CLICKED"       ALSO    "CHKMTRX02"
           WHEN    "CLICKED"       ALSO    "CHKMTRX03"
           WHEN    "CLICKED"       ALSO    "CHKMTRX04"
           WHEN    "CLICKED"       ALSO    "CHKMTRX05"
               PERFORM  4100-CHKMTRX-CHK-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 480-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
      *    WHEN    "XXXX"
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマトリクスチェック処理
      *****************************************************************
       4100-CHKMTRX-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    ZERO            TO  IDX-MTRX
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHKMTRX01"
               MOVE    1           TO  IDX-MTRX
           WHEN    "CHKMTRX02"
               MOVE    2           TO  IDX-MTRX
           WHEN    "CHKMTRX03"
               MOVE    3           TO  IDX-MTRX
           WHEN    "CHKMTRX04"
               MOVE    4           TO  IDX-MTRX
           WHEN    "CHKMTRX05"
               MOVE    5           TO  IDX-MTRX
           END-EVALUATE
      *
      *
           IF    ( IDX-MTRX        >   ZERO )
            AND  ( SPA-GMN-CHKMTRX (IDX-MTRX)  =   " " )
               MOVE    "1"         TO  SPA-GMN-CHKMTRX (IDX-MTRX)
           ELSE
               MOVE    " "         TO  SPA-GMN-CHKMTRX (IDX-MTRX)
           END-IF
      *
           .
       4100-CHKMTRX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    W60-BYOSRYKAKBN     TO  SPA-GMN-BYOSRYKAKBN-G
           MOVE    W60-SYUBYOCHK       TO  SPA-GMN-SYUBYOCHK-G
           MOVE    W60-UTAGAIKKN       TO  SPA-GMN-UTAGAIKKN
           MOVE    W60-KYUSEIKKNKBN    TO  SPA-GMN-KYUSEIKKNKBN-G
           MOVE    W60-TENKITEKBYOKBN  TO  SPA-GMN-TENKITEKBYOKBN-G
           MOVE    W60-YBKENSAYAKUKBN  TO  SPA-GMN-YBKENSAYAKUKBN-G
           MOVE    W60-YBUTAGAIKBN     TO  SPA-GMN-YBUTAGAIKBN-G
           MOVE    W60-SRYHEIWRNKBN    TO  SPA-GMN-SRYHEIWRNKBN-G
           MOVE    W60-ERR990009KBN    TO  SPA-GMN-ERR990009KBN-G
           MOVE    W60-REIGAICHKKBN    TO  SPA-GMN-REIGAICHKKBN-G
           MOVE    W60-PRTSRTKBN       TO  SPA-GMN-PRTSRTKBN-G
           MOVE    W60-PRTPTLSTKBN     TO  SPA-GMN-PRTPTLSTKBN-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           PERFORM 4101-BYOSRYKAKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SYUBYOCHK-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-UTAGAIKKN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-KYUSEIKKNKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-TENKITEKBYOKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-YBKENSAYAKUKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-YBUTAGAIKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-SRYHEIWRNKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-ERR990009KBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-REIGAICHKKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-PRTSRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 4101-PRTPTLSTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェック区分チェック処理
      *****************************************************************
       4101-BYOSRYKAKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  WRK-CMB-CD
           PERFORM 4200-BYOSRYKAKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-BYOSRYKAKBNLST (1)
                                   TO  SPA-GMN-BYOSRYKAKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-BYOSRYKAKBN-G
           END-IF
      *
           .
      *
       4101-BYOSRYKAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェック区分チェック処理
      *****************************************************************
       4101-SYUBYOCHK-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SYUBYOCHK   TO  WRK-CMB-CD
           PERFORM 4200-SYUBYOCHKLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SYUBYOCHKLST (2)
                                   TO  SPA-GMN-SYUBYOCHK-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SYUBYOCHK-G
           END-IF
      *
           .
      *
       4101-SYUBYOCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い期間チェック処理
      ***************************************************************
       4101-UTAGAIKKN-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-UTAGAIKKN   >= 1 )
            AND  ( SPA-GMN-UTAGAIKKN   <= 9 )
               CONTINUE
           ELSE
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4101-UTAGAIKKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性期間区分チェック処理
      *****************************************************************
       4101-KYUSEIKKNKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN TO  WRK-CMB-CD
           PERFORM 4200-KYUSEIKKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (1)
                                   TO  SPA-GMN-KYUSEIKKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-KYUSEIKKNKBN-G
           END-IF
      *
           .
      *
       4101-KYUSEIKKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名区分チェック処理
      *****************************************************************
       4101-TENKITEKBYOKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN TO  WRK-CMB-CD
           PERFORM 4200-TENKITEKBYOKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (1)
                                   TO  SPA-GMN-TENKITEKBYOKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-TENKITEKBYOKBN-G
           END-IF
      *
           .
      *
       4101-TENKITEKBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象区分チェック処理
      *****************************************************************
       4101-YBKENSAYAKUKBN-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN TO  WRK-CMB-CD
           PERFORM 4200-YBKENSAYAKUKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (1)
                                   TO  SPA-GMN-YBKENSAYAKUKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBKENSAYAKUKBN-G
           END-IF
      *
           .
      *
       4101-YBKENSAYAKUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑い区分チェック処理
      *****************************************************************
       4101-YBUTAGAIKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO  WRK-CMB-CD
           PERFORM 4200-YBUTAGAIKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-YBUTAGAIKBNLST (1)
                                   TO  SPA-GMN-YBUTAGAIKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-YBUTAGAIKBN-G
           END-IF
      *
           .
      *
       4101-YBUTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラー区分チェック処理
      *****************************************************************
       4101-SRYHEIWRNKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO  WRK-CMB-CD
           PERFORM 4200-SRYHEIWRNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (1)
                                   TO  SPA-GMN-SRYHEIWRNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-SRYHEIWRNKBN-G
           END-IF
      *
           .
      *
       4101-SRYHEIWRNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他エラー区分チェック処理
      *****************************************************************
       4101-ERR990009KBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-ERR990009KBN TO  WRK-CMB-CD
           PERFORM 4200-ERR990009KBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-ERR990009KBNLST (1)
                                   TO  SPA-GMN-ERR990009KBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-ERR990009KBN-G
           END-IF
      *
           .
      *
       4101-ERR990009KBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定チェック処理
      *****************************************************************
       4101-REIGAICHKKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO  WRK-CMB-CD
           PERFORM 4200-REIGAICHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-REIGAICHKKBNLST (1)
                                   TO  SPA-GMN-REIGAICHKKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-REIGAICHKKBN-G
           END-IF
      *
           .
      *
       4101-REIGAICHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順区分チェック処理
      *****************************************************************
       4101-PRTSRTKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO  WRK-CMB-CD
           PERFORM 4200-PRTSRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTSRTKBNLST (1)
                                   TO  SPA-GMN-PRTSRTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTSRTKBN-G
           END-IF
      *
           .
      *
       4101-PRTSRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト区分チェック処理
      *****************************************************************
       4101-PRTPTLSTKBN-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO  WRK-CMB-CD
           PERFORM 4200-PRTPTLSTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     =   SPACE )
               MOVE    SPA-GMN-PRTPTLSTKBNLST (1)
                                   TO  SPA-GMN-PRTPTLSTKBN-G
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-PRTPTLSTKBN-G
           END-IF
      *
           .
      *
       4101-PRTPTLSTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科別チェック区分コンボチェック処理
      *****************************************************************
       4200-BYOSRYKAKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-BYOSRYKAKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-BYOSRYKAKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-BYOSRYKAKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BYOSRYKAKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名チェック区分コンボチェック処理
      *****************************************************************
       4200-SYUBYOCHKLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SYUBYOCHK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SYUBYOCHKL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SYUBYOCHKLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SYUBYOCHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性期間区分コンボチェック処理
      *****************************************************************
       4200-KYUSEIKKNKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-KYUSEIKKNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KYUSEIKKNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-KYUSEIKKNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KYUSEIKKNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    過月転帰済適応病名区分コンボチェック処理
      *****************************************************************
       4200-TENKITEKBYOKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-TENKITEKBYOKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-TENKITEKBYOKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-TENKITEKBYOKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TENKITEKBYOKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検査薬剤対象区分コンボチェック処理
      *****************************************************************
       4200-YBKENSAYAKUKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB     >   SPA-GMN-YBKENSAYAKUKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBKENSAYAKUKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBKENSAYAKUKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBKENSAYAKUKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名疑い区分コンボチェック処理
      *****************************************************************
       4200-YBUTAGAIKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-YBUTAGAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-YBUTAGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-YBUTAGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YBUTAGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定警告エラー区分コンボチェック処理
      *****************************************************************
       4200-SRYHEIWRNKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-SRYHEIWRNKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-SRYHEIWRNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-SRYHEIWRNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYHEIWRNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    その他エラー区分コンボチェック処理
      *****************************************************************
       4200-ERR990009KBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-ERR990009KBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-ERR990009KBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-ERR990009KBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-ERR990009KBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者毎の例外設定コンボチェック処理
      *****************************************************************
       4200-REIGAICHKKBNLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-REIGAICHKKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-REIGAICHKKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-REIGAICHKKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-REIGAICHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力順区分コンボチェック処理
      *****************************************************************
       4200-PRTSRTKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-PRTSRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PRTSRTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PRTSRTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PRTSRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者別チェックリスト区分コンボチェック処理
      *****************************************************************
       4200-PRTPTLSTKBNLST-CHK-SEC     SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-PRTPTLSTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PRTPTLSTKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-PRTPTLSTKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-PRTPTLSTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W60"               TO  SPA-MOTOPG
      *
           MOVE    "W01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-W60
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    CONST-CHKMTRX (IDX-MTRX)
                                   TO  SPA-GMN-CHKMTRX (IDX-MTRX)
      *
           END-PERFORM
      *
           MOVE    2               TO  SPA-GMN-UTAGAIKKN
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           PERFORM 40311-DEL-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 40312-DEL-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       4031-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理 (SYS 1101)
      *****************************************************************
       40311-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1101-REC
           INITIALIZE                      SYS-1101-REC
           MOVE    "1101"                  TO  SYS-1101-KANRICD
           MOVE    "*"                     TO  SYS-1101-KBNCD
           MOVE    SPA-W01-SEL-STYUKYMD    TO  SYS-1101-STYUKYMD
           MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYS-1101-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1101-HOSPNUM
           MOVE    SYS-1101-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4003"              TO  SPA-ERRCD
           END-IF
      *
           .
       40311-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理 (CHK004 1103)
      *****************************************************************
       40312-DEL-SEC                              SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM    TO   CHK004-HOSPNUM
           MOVE    "1103"         TO   CHK004-KANRICD
           MOVE    "%"            TO   CHK004-KBNCD
           IF    ( SPA-W01-SELNUM          >   ZERO )
               MOVE    SPA-W01-SEL-STYUKYMD
                                  TO   CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           ELSE
               MOVE    SPA-W01-STYUKYMD
                                  TO   CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           END-IF
      *
           MOVE    CHK004-REC     TO   MCPDATA-REC
           MOVE    "tbl_chk004"   TO   MCP-TABLE
           MOVE    "del1"         TO   MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4004"     TO  SPA-ERRCD
           END-IF
      *
           .
       40312-DEL-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 4801-KANREN-CHK
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "1001"              TO  SPA-WIDCD
      *
           .
       480-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    システム管理1101更新処理
           PERFORM 4901-KOUSHIN-SEC
      *
      *    チェックマスタ４(1103)更新処理
           PERFORM 4902-KOUSHIN-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE   "JOIN"           TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理（SYSKANRI 1101)
      *****************************************************************
       4901-KOUSHIN-SEC           SECTION.
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               INITIALIZE                  SYS-1101-REC
               MOVE    "1101"          TO  SYS-1101-KANRICD
               MOVE    "*"             TO  SYS-1101-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SYS-1101-STYUKYMD
                                           SYS-1101-EDYUKYMD
               MOVE    SYS-1101-REC    TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           ELSE
               INITIALIZE                  SYS-1101-REC
               MOVE    "1101"          TO  SYS-1101-KANRICD
               MOVE    "*"             TO  SYS-1101-KBNCD
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1101-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SYS-1101-EDYUKYMD
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    SPA-GMN-CHKMTRX (IDX-MTRX)
                                       TO  SYS-1101-BYOCHKPTN(IDX-MTRX)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-BYOSRYKAKBN TO  SYS-1101-BYOSRYKAKBN
      *
           MOVE    SPA-GMN-SYUBYOCHK   TO  SYS-1101-SYUBYOCHKKBN
      *
           MOVE    SPA-GMN-UTAGAIKKN   TO  SYS-1101-UTAGAIKKN
      *
           EVALUATE    SPA-GMN-KYUSEIKKNKBN
           WHEN    "0"
               MOVE    0               TO  SYS-1101-KYUSEIKKN
           WHEN    "1"
               MOVE    1               TO  SYS-1101-KYUSEIKKN
           WHEN    "2"
               MOVE    2               TO  SYS-1101-KYUSEIKKN
           WHEN    "3"
               MOVE    3               TO  SYS-1101-KYUSEIKKN
           WHEN    "4"
               MOVE    4               TO  SYS-1101-KYUSEIKKN
           END-EVALUATE
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN
                                       TO   SYS-1101-TENKITEKBYOKBN
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN  TO   SYS-1101-YBKENSAYAKUKBN
      *
           MOVE    SPA-GMN-YBUTAGAIKBN TO   SYS-1101-YBUTAGAIKBN
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN TO   SYS-1101-SRYHEIWRNKBN
      *
           MOVE    SPA-GMN-REIGAICHKKBN TO   SYS-1101-REIGAICHKKBN
      *
           MOVE    SPA-GMN-PRTSRTKBN   TO   SYS-1101-PRTSRTKBN
      *
           MOVE    SPA-GMN-PRTPTLSTKBN TO   SYS-1101-PRTPTLSTKBN
      *
           MOVE    SYS-1101-REC            TO  SYSKANRI-REC
      *
           IF    ( SPA-W01-SELNUM          >   ZERO )
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W01-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4001"              TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC            TO  MCPDATA-REC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理（CHK004 1103)
      *****************************************************************
       4902-KOUSHIN-SEC            SECTION.
      *
           PERFORM 40312-DEL-SEC
      *
           IF    ( SPA-ERRCD      =    SPACE )
               INITIALIZE                  CK4-1103-REC
               MOVE    SPA-HOSPNUM     TO  CK4-1103-HOSPNUM
               MOVE    "1103"          TO  CK4-1103-KANRICD
               MOVE    "99"            TO  CK4-1103-KBNCD
               MOVE    SPA-W01-STYUKYMD
                                       TO  CK4-1103-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD
                                       TO  CK4-1103-EDYUKYMD
               MOVE    SPA-GMN-ERR990009KBN
                                       TO  CK4-1103-ERRKBN (9)
               MOVE    CK4-1103-REC    TO  MCPDATA-REC
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "4005"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4902-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック
      *****************************************************************
       4801-KANREN-CHK                 SECTION.
      *
           IF    ( SPA-GMN-CHKMTRX-G   =   SPACE )
               MOVE    "0101"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4801-KANREN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W60    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           INITIALIZE                  W60
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   4 )
      *
               MOVE    WIDGET-NORMAL
                                       TO  W60-CHKMTRXNIL-STATE
                                                         (IDX-MTRX)
               MOVE    "whitebutton"   TO  W60-CHKMTRXNIL-STYLE
                                                         (IDX-MTRX)
           END-PERFORM
      *
           PERFORM VARYING IDX-MTRX    FROM    1   BY  1
                   UNTIL ( IDX-MTRX    >   CONST-CHKMTRX-MAX )
      *
               MOVE    "whitebutton"
                                   TO  W60-CHKMTRX-STYLE (IDX-MTRX)
      *
               IF    ( SPA-GMN-CHKMTRX (IDX-MTRX)  =   "1" )
                   MOVE    "○"    TO  W60-MTRX    (IDX-MTRX)
               ELSE
                   MOVE    "×"    TO  W60-MTRX    (IDX-MTRX)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-BYOSRYKAKBN-G   TO  W60-BYOSRYKAKBN
      *
           MOVE    SPA-GMN-BYOSRYKAKBN-MAX
                                           TO  W60-BYOSRYKAKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-BYOSRYKAKBN-MAX )
      *
               MOVE    SPA-GMN-BYOSRYKAKBNLST (IDX0)
                                       TO  W60-BYOSRYKAKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SYUBYOCHK-G     TO  W60-SYUBYOCHK
      *
           MOVE    SPA-GMN-SYUBYOCHK-MAX
                                           TO  W60-SYUBYOCHK-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SYUBYOCHK-MAX )
      *
               MOVE    SPA-GMN-SYUBYOCHKLST (IDX0)
                                           TO  W60-SYUBYOCHK-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-UTAGAIKKN       TO  W60-UTAGAIKKN
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN-G   TO  W60-KYUSEIKKNKBN
      *
           MOVE    SPA-GMN-KYUSEIKKNKBN-MAX
                                           TO  W60-KYUSEIKKNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-KYUSEIKKNKBN-MAX )
      *
               MOVE    SPA-GMN-KYUSEIKKNKBNLST (IDX0)
                                       TO  W60-KYUSEIKKNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN-G   TO  W60-TENKITEKBYOKBN
      *
           MOVE    SPA-GMN-TENKITEKBYOKBN-MAX
                                       TO  W60-TENKITEKBYOKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-TENKITEKBYOKBN-MAX )
      *
               MOVE    SPA-GMN-TENKITEKBYOKBNLST (IDX0)
                                       TO  W60-TENKITEKBYOKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN-G   TO  W60-YBKENSAYAKUKBN
      *
           MOVE    SPA-GMN-YBKENSAYAKUKBN-MAX
                                       TO  W60-YBKENSAYAKUKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBKENSAYAKUKBN-MAX )
      *
               MOVE    SPA-GMN-YBKENSAYAKUKBNLST (IDX0)
                                       TO  W60-YBKENSAYAKUKBN-ITEM(IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YBUTAGAIKBN-G   TO  W60-YBUTAGAIKBN
      *
           MOVE    SPA-GMN-YBUTAGAIKBN-MAX
                                       TO  W60-YBUTAGAIKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-YBUTAGAIKBN-MAX )
      *
               MOVE    SPA-GMN-YBUTAGAIKBNLST (IDX0)
                                       TO  W60-YBUTAGAIKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN-G   TO  W60-SRYHEIWRNKBN
      *
           MOVE    SPA-GMN-SRYHEIWRNKBN-MAX
                                       TO  W60-SRYHEIWRNKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-SRYHEIWRNKBN-MAX )
      *
               MOVE    SPA-GMN-SRYHEIWRNKBNLST (IDX0)
                                       TO  W60-SRYHEIWRNKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-ERR990009KBN-G   TO  W60-ERR990009KBN
      *
           MOVE    SPA-GMN-ERR990009KBN-MAX
                                       TO  W60-ERR990009KBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-ERR990009KBN-MAX )
      *
               MOVE    SPA-GMN-ERR990009KBNLST (IDX0)
                                       TO  W60-ERR990009KBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-REIGAICHKKBN-G   TO  W60-REIGAICHKKBN
      *
           MOVE    SPA-GMN-REIGAICHKKBN-MAX
                                       TO  W60-REIGAICHKKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-REIGAICHKKBN-MAX )
      *
               MOVE    SPA-GMN-REIGAICHKKBNLST (IDX0)
                                       TO  W60-REIGAICHKKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTSRTKBN-G     TO  W60-PRTSRTKBN
      *
           MOVE    SPA-GMN-PRTSRTKBN-MAX
                                       TO  W60-PRTSRTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-PRTSRTKBN-MAX )
      *
               MOVE    SPA-GMN-PRTSRTKBNLST (IDX0)
                                       TO  W60-PRTSRTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTPTLSTKBN-G   TO  W60-PRTPTLSTKBN
      *
           MOVE    SPA-GMN-PRTPTLSTKBN-MAX
                                       TO  W60-PRTPTLSTKBN-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-PRTPTLSTKBN-MAX )
      *
               MOVE    SPA-GMN-PRTPTLSTKBNLST (IDX0)
                                       TO  W60-PRTPTLSTKBN-ITEM (IDX0)
      *
           END-PERFORM
      *
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W60-B03-STATE
      *
           IF    ( SPA-W01-SELNUM         <=   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W60-B03-STATE
           END-IF
      *
           IF    ( SPA-W01-SEL-STYUKYMD    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W60-B03-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "BYOSRYKAKBN"   TO  MCP-WIDGET
           WHEN    002
               MOVE    "UTAGAIKKN"     TO  MCP-WIDGET
           WHEN    003
               MOVE    "CHKMTRX01"     TO  MCP-WIDGET
           WHEN    004
               MOVE    "CHKMTRX02"     TO  MCP-WIDGET
           WHEN    005
               MOVE    "CHKMTRX03"     TO  MCP-WIDGET
           WHEN    006
               MOVE    "CHKMTRX04"     TO  MCP-WIDGET
           WHEN    007
               MOVE    "CHKMTRX05"     TO  MCP-WIDGET
           WHEN    008
               MOVE    "PRTSRTKBN"     TO  MCP-WIDGET
           WHEN    009
               MOVE    "PRTPTLSTKBN"   TO  MCP-WIDGET
           WHEN    010
               MOVE    "YBUTAGAIKBN"   TO  MCP-WIDGET
           WHEN    011
               MOVE    "SYUBYOCHK"     TO  MCP-WIDGET
           WHEN    012
               MOVE    "YBKENSAYAKUKBN" TO MCP-WIDGET
           WHEN    013
               MOVE    "TENKITEKBYOKBN" TO MCP-WIDGET
           WHEN    014
               MOVE    "SRYHEIWRNKBN"   TO  MCP-WIDGET
           WHEN    015
               MOVE    "KYUSEIKKNKBN"  TO  MCP-WIDGET
           WHEN    016
               MOVE    "REIGAICHKKBN"   TO  MCP-WIDGET
           WHEN    017
               MOVE    "ERR990009KBN"   TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BYOSRYKAKBN"
               MOVE    11              TO  SPA-GMN-CUR
           WHEN    "SYUBYOCHK"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "UTAGAIKKN"
               MOVE    15              TO  SPA-GMN-CUR
           WHEN    "KYUSEIKKNKBN"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "CHKMTRX01"
               MOVE    04              TO  SPA-GMN-CUR
           WHEN    "CHKMTRX02"
               MOVE    05              TO  SPA-GMN-CUR
           WHEN    "CHKMTRX03"
               MOVE    06              TO  SPA-GMN-CUR
           WHEN    "CHKMTRX04"
               MOVE    07              TO  SPA-GMN-CUR
           WHEN    "CHKMTRX05"
               MOVE    13              TO  SPA-GMN-CUR
           WHEN    "TENKITEKBYOKBN"
               MOVE    12              TO  SPA-GMN-CUR
           WHEN    "YBKENSAYAKUKBN"
               MOVE    10              TO  SPA-GMN-CUR
           WHEN    "YBUTAGAIKBN"
               MOVE    14              TO  SPA-GMN-CUR
           WHEN    "SRYHEIWRNKBN"
               MOVE    17              TO  SPA-GMN-CUR
           WHEN    "ERR990009KBN"
               MOVE    16              TO  SPA-GMN-CUR
           WHEN    "REIGAICHKKBN"
               MOVE    08              TO  SPA-GMN-CUR
           WHEN    "PRTSRTKBN"
               MOVE    09              TO  SPA-GMN-CUR
           WHEN    "PRTPTLSTKBN"
               MOVE    212             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               STRING   "疑い病名チェック期間は"
                                       DELIMITED   BY  SIZE
                        "１〜９ヶ月の期間で設定してください"
                                       DELIMITED   BY  SIZE
               INTO     SPA-ERRMSG
               END-STRING
           WHEN    "0101"
               MOVE    "病名チェック条件を設定してください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"              THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W60"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"    TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W60"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key8"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key8"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    CHK004-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               INITIALIZE                  CHK004-REC
               MOVE    1               TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               INITIALIZE                  CHK004-REC
               MOVE    1               TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                  SECTION.
      *
           MOVE    "DBFETCH"       TO   MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC            SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

