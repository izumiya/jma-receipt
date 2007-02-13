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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 施設基準情報（Ｗ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正   
      *  02.00.01    NACL-多々納  02/11/12  一括入力 
      *  02.00.02    NACL-多々納  03/10/02  新規登録・削除追加
      *  02.00.03    NACL-多々納  06/04/24  MONFUNC 対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W03.
           03  SPA-W03-AREA.
               05  SPA-GMN-PAGE        PIC 9(04).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(04).
               05  SPA-GMN-PAGE-MAX    PIC 9(04).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO            OCCURS  25.
                           11  SPA-GMN-TCD         PIC X(04).
                           11  SPA-GMN-TNAME       PIC X(100).
                           11  SPA-GMN-TSSTKIJUN   PIC 9(01).
               05  SPA-GMN-SELCD                   PIC X(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO            OCCURS  1000.
                           11  SPA-NAI-TSSTKIJUN   PIC 9(01).
      *
                   07  SPA-SYORIFLG                PIC 9(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
           03  FLG-SELCD                          PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
           03  IDZ1                              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-KBNCD           PIC 9(01).
           03  WRK-MAX-X.
               05  WRK-MAX         PIC 9(04). 
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(08).
               05  WRK-WIDGET2     PIC 9(02).
               05                  PIC X(03). 
           03  WRK-YMD             PIC X(09). 
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
           03  WRK-EDYUKYMD        PIC X(08).
      *
           03  WRK-TCD             PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    施設基準名
           COPY  "CPSK0031.INC".
      *    施設基準コードフラグ情報
           COPY  "CPSK1006.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "W13.INC".
           COPY    "W14.INC".
           COPY    "W15.INC".
           COPY    "W16.INC".
           COPY    "W17.INC".
           COPY    "W18.INC".
           COPY    "W19.INC".
           COPY    "W20.INC".
           COPY    "W21.INC".
           COPY    "W22.INC".
           COPY    "W23.INC".
           COPY    "W24.INC".
           COPY    "W25.INC".
           COPY    "W26.INC".
           COPY    "W27.INC".
           COPY    "W28.INC".
           COPY    "W29.INC".
           COPY    "W30.INC".
           COPY    "W31.INC".
           COPY    "W60.INC".
           COPY    "W61.INC".
           COPY    "W62.INC".
           COPY    "W63.INC".
           COPY    "W98.INC".
           COPY    "W97.INC".
           COPY    "W99.INC".
           COPY    "WERR.INC".
           COPY    "WID1.INC".
           COPY    "WID2.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W03
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
           MOVE    SPA-W03         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF 
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W03    "           TO  MCP-WINDOW.
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
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
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
      *
           INITIALIZE              SPA-W03-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      * 
      *    施設基準情報
           MOVE    SPACE               TO  SYS-0031-REC 
           INITIALIZE                      SYS-0031-REC
           MOVE    "0031"              TO  SYS-0031-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
           MOVE    SYS-0031-REC            TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  WRK-MAX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC        TO  SYS-0031-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    施設基準情報
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *    対象のシステム管理がない場合、新規とする
           IF      FLG-SYSKANRI        =   1
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               PERFORM 3102-SINKI-CHK-SEC
           ELSE
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  1000
               MOVE    ZERO                TO  SPA-SYORIFLG
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       500
                   ADD     1               TO  IDX
                   MOVE    SYS-1006-SSTKIJUN  (IDY)
                                           TO  SPA-NAI-TSSTKIJUN (IDX)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    Ｈ１８年４月以降は、１００とする
           IF      SPA-W01-EDYUKYMD    >=  "20060401"
               IF      IDX                 <   1000
                   MOVE    1000            TO  IDX
               END-IF
           END-IF
      *
           END-IF
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
      *
           COMPUTE SPA-GMN-PAGE-MAX    =   (SPA-GMN-MAX - 1)     /
                                            25  + 1      
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準新規編集処理
      *****************************************************************
       3102-SINKI-CHK-SEC            SECTION.
      *
      *    施設基準情報
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-EDYUKYMD
           PERFORM     UNTIL   FLG-SYSKANRI   =   1
      *D               OR      IDX            >=  1000
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SPA-W01-STYUKYMD    >   SYS-1006-EDYUKYMD
                   IF      WRK-EDYUKYMD        <   SYS-1006-EDYUKYMD
      *                別の日があった場合、クリア
                       MOVE    ZERO                TO  IDX
                       INITIALIZE                  SPA-NAI-TBL
                       MOVE    SYS-1006-EDYUKYMD   TO  WRK-EDYUKYMD
                   END-IF
      *            前の日付あり（新規）
                   MOVE    1               TO  SPA-SYORIFLG
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >   500
                       ADD     1               TO  IDX
                       MOVE    SYS-1006-SSTKIJUN  (IDY)
                                               TO  SPA-NAI-TSSTKIJUN
                                                              (IDX)
                   END-PERFORM
                   MOVE    SYS-1006-EDYUKYMD   TO  WRK-EDYUKYMD
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key17"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    施設基準があるかの判定
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL   IDY         >   IDX
               MOVE    SPACE               TO  SYS-0031-REC
               INITIALIZE                      SYS-0031-REC
               MOVE    "0031"              TO  SYS-0031-KANRICD
               MOVE    IDY                 TO  SYS-0031-KBNCD (1:4)
               MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
               MOVE    SYS-0031-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  SPA-NAI-TSSTKIJUN
                                                                (IDY)
               END-IF
           END-PERFORM
      *    対象がない時、１レコード分とする
           IF      IDX                 =   ZERO
               MOVE    1               TO  SPA-SYORIFLG
               MOVE    1000            TO  IDX
           END-IF
      *
           .
       3102-SINKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準編集処理
      *****************************************************************
       3101-SSTKIJUN-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-TBL
      *
           COMPUTE IDX =   (SPA-GMN-PAGE    -   1)   *   25
      * 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       25
               ADD     1                           TO  IDX
               IF      IDX     >   SPA-GMN-MAX
                   MOVE    25                      TO  IDY
               ELSE
                   MOVE    IDX             TO  SPA-GMN-TCD       (IDY)
                   MOVE    SPA-NAI-TSSTKIJUN   (IDX)
                                           TO  SPA-GMN-TSSTKIJUN (IDY)
                   MOVE    SPACE           TO  SYS-0031-REC
                   INITIALIZE                  SYS-0031-REC
                   MOVE    "0031"          TO  SYS-0031-KANRICD
                   MOVE    SPA-GMN-TCD (IDY)
                                           TO  SYS-0031-KBNCD (1:4)
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-STYUKYMD
                   MOVE    SPA-W01-STYUKYMD    TO  SYS-0031-EDYUKYMD
                   MOVE    SYS-0031-REC    TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-0031-REC
                       MOVE    SYS-0031-SSTKIJUN
                                               TO  SPA-GMN-TNAME(IDY)
                   END-IF
               END-IF 
           END-PERFORM      
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
      *        登録
               WHEN    "1001"
               WHEN    "1003"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
      *        削除
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  410-KAKUTEI-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        カーソル移動
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM  409-CURSET-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   ZERO
               IF      (SPA-GMN-SELCD  NOT =   SPACE )  AND
                       (FLG-SELCD          =   ZERO  )
      *            コード表示へ
                   PERFORM 4103-SELCD-HENSYU-SEC
               END-IF
               IF      (SPA-GMN-SELCD  NOT =   SPACE )  AND
                       (SPA-ERRCD          =   SPACE  ) AND
                       (WRK-MCP-WIDGET     =   "SELCD")
      *            カーソル移動
                   PERFORM 4103-SELCD-CUR-SEC
               END-IF
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    W03-SELCD           TO  SPA-GMN-SELCD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   25
               MOVE    W03-SSTKIJUN (IDX)  TO  SPA-GMN-TSSTKIJUN (IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM  4101-SELCD-CHK-SEC
      *
           PERFORM  4101-SSTKIJUN-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W03"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択コードチェック処理
      *****************************************************************
       4101-SELCD-CHK-SEC                         SECTION.
      *
           IF      SPA-GMN-SELCD       =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-SELCD       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SEISUU         >   4     )   OR
                      (SNUM-SYOSUU         >   ZERO  )
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               ELSE
                   IF    ( SNUM-OUTEDT     =   ZERO )
                       MOVE    SPACE       TO  SPA-GMN-SELCD
                   ELSE
                       MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-SELCD
                   END-IF
               END-IF
           END-IF
           .
       4101-SELCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　施設基準チェック
      *****************************************************************
       4101-SSTKIJUN-CHK-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-SELCD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   25  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE IDZ             =   IDX
                                       +   (SPA-GMN-PAGE - 1 ) * 25
      *        施設基準名があること
               IF      SPA-GMN-TNAME (IDX)     =   SPACE
               AND     SPA-GMN-TSSTKIJUN (IDX) =   1
                   MOVE    "0002"             TO  SPA-ERRCD
                   MOVE    IDX                TO  SPA-GMN-CUR
               END-IF
      *
               IF      SPA-GMN-TCD (IDX)   =   SPA-GMN-SELCD
                   MOVE    1               TO  FLG-SELCD
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   EVALUATE    SPA-GMN-TSSTKIJUN (IDX)
                       WHEN    ZERO
                       WHEN    1
                           MOVE  SPA-GMN-TSSTKIJUN (IDX)
                                         TO  SPA-NAI-TSSTKIJUN (IDZ)
                       WHEN    OTHER
                           MOVE    "0002"             TO  SPA-ERRCD
                           MOVE    IDX                TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       4101-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コード表示へ処理
      *****************************************************************
       4103-SELCD-HENSYU-SEC             SECTION.
      *
           MOVE    SPA-GMN-SELCD       TO  WRK-TCD
           COMPUTE WRK-PAGE            =  ((WRK-TCD - 1  )
                                       /   25 )
                                       +   1
           IF      WRK-PAGE            >   SPA-GMN-PAGE-MAX
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    90                  TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-PAGE            TO  SPA-GMN-PAGE
               PERFORM 3101-SSTKIJUN-HENSYU-SEC
           END-IF
           .
       4103-SELCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    カーソル移動処理
      *****************************************************************
       4103-SELCD-CUR-SEC             SECTION.
      *    カーソル
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   25  )
               IF      SPA-GMN-TCD (IDX)   =   SPA-GMN-SELCD
                   MOVE    IDX             TO  SPA-GMN-CUR
                   MOVE    25              TO  IDX
               END-IF
           END-PERFORM
           MOVE    SPACE           TO  SPA-GMN-SELCD
      *
           .
       4103-SELCD-CUR-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0006"             TO  SPA-ERRCD
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  440-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   SPA-GMN-PAGE-MAX
               MOVE    "0007"             TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1       
      *
           PERFORM 3101-SSTKIJUN-HENSYU-SEC  
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
           MOVE    1                TO  FLG-TOUROKU
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               IF      SPA-SYORIFLG     =   ZERO
                   MOVE    "1001"           TO  SPA-WIDCD
               ELSE
                   MOVE    "1003"           TO  SPA-WIDCD
               END-IF
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      * 
      *    施設基準情報削除
           IF      SPA-SYORIFLG        =   ZERO
      *        更新の時のみ
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                      SYS-1006-REC
               MOVE    "1006"              TO  SYS-1006-KANRICD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
               MOVE    SYS-1006-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "del11"             TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPDATA-REC
           END-IF
      *
      *    新規作成
           MOVE    ZERO                TO  IDX
                                           WRK-KBNCD
           PERFORM             UNTIL   IDX            >=  SPA-GMN-MAX
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                      SYS-1006-REC
               MOVE    "1006"                  TO  SYS-1006-KANRICD
               MOVE    SPA-W01-STYUKYMD        TO  SYS-1006-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD        TO  SYS-1006-EDYUKYMD
               ADD     1                       TO  WRK-KBNCD
               MOVE    WRK-KBNCD               TO  SYS-1006-KBNCD
      *    
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       500
                   ADD     1               TO  IDX
                   MOVE    SPA-NAI-TSSTKIJUN (IDX)
                                           TO  SYS-1006-SSTKIJUN  (IDY)
               END-PERFORM
      *
               MOVE    SYS-1006-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"              TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               MCPDATA-REC
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W03 SYSKANRI INS ERR:" MCP-RC
                            ",KEY:" SYS-1006-KEY
                   MOVE    "1003"                  TO  SPA-ERRCD
                   GO  TO  490-KOUSHIN-EXT
               END-IF
           END-PERFORM          
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4901-KANRENCHK-EXT
           END-IF
      *
      *****MOVE    SPACE                TO  SPA-ERRCD
      *
      *    PERFORM VARYING IDX FROM    1   BY  1
      *            UNTIL   IDX >       25
      *        EVALUATE    SPA-GMN-TSSTKIJUN (IDX)
      *            WHEN    ZERO
      *            WHEN    1  
      *                IF      SPA-GMN-TNAME     (IDX) =   SPACE
      *                AND     SPA-GMN-TSSTKIJUN (IDX) =   "1"
      *                   MOVE    "0002"             TO  SPA-ERRCD
      *                   GO  TO  4901-KANRENCHK-EXT
      *                END-IF
      *            WHEN    OTHER
      *                MOVE    "0002"             TO  SPA-ERRCD
      *                GO  TO  4901-KANRENCHK-EXT
      *        END-EVALUATE
      *****END-PERFORM      
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル移動処理
      *****************************************************************
       409-CURSET-SEC                               SECTION.
      *
           MOVE    90                  TO  SPA-GMN-CUR
           .
       409-CURSET-EXT.
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
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1006-EDYUKYMD
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1006-KEY 
               GO  TO  4031-DEL-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
     *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W03
      *
           COMPUTE IDY     =   SPA-GMN-PAGE    *   25
           COMPUTE IDZ     =   IDY             -   24
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDZ1     FROM    IDZ   BY  1
                   UNTIL       IDZ1     >       IDY
               ADD     1                        TO  IDX
               MOVE    SPA-GMN-TCD        (IDX) TO  W03-CD       (IDX)
               MOVE    SPA-GMN-TSSTKIJUN  (IDX) TO  W03-SSTKIJUN (IDX)
               MOVE    SPA-GMN-TNAME      (IDX) TO  W03-DSPNAME  (IDX)
      ***         MOVE    SPA-NAI-TSSTKIJUN  (IDZ1)
      ***                                       TO  W03-SSTKIJUN (IDX)
           END-PERFORM   
      *
           MOVE    SPA-GMN-STYUKYMD     TO  W03-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD     TO  W03-EDYUKYMD
      *
           MOVE    SPA-GMN-SELCD        TO  W03-SELCD
      *    業務権限非活性処理
           PERFORM 510-GSRAUTH-SEC
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
      *    開始日がゼロの場合、削除できないこととする
           IF      SPA-W01-STYUKYMD    =   SPACE  OR  ZERO
               MOVE    WIDGET-INSENSITIVE  TO  W03-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  W03-B03-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    WRK-MCP-WIDGET      TO  WRK-WIDGET
               IF      WRK-WIDGET2(1:2)    NOT NUMERIC
                   MOVE    ZERO            TO  WRK-WIDGET2
               END-IF
               MOVE    WRK-WIDGET2         TO  SPA-GMN-CUR
               ADD     1                   TO  SPA-GMN-CUR
               IF      SPA-GMN-CUR         =   26
                   MOVE    90              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "SSTKIJUN01"    TO  MCP-WIDGET
                WHEN   2
                    MOVE  "SSTKIJUN02"    TO  MCP-WIDGET
                WHEN   3
                    MOVE  "SSTKIJUN03"    TO  MCP-WIDGET
                WHEN   4
                    MOVE  "SSTKIJUN04"    TO  MCP-WIDGET
                WHEN   5
                    MOVE  "SSTKIJUN05"    TO  MCP-WIDGET
                WHEN   6
                    MOVE  "SSTKIJUN06"    TO  MCP-WIDGET
                WHEN   7
                    MOVE  "SSTKIJUN07"    TO  MCP-WIDGET
                WHEN   8
                    MOVE  "SSTKIJUN08"    TO  MCP-WIDGET
                WHEN   9
                    MOVE  "SSTKIJUN09"    TO  MCP-WIDGET
                WHEN   10
                    MOVE  "SSTKIJUN10"    TO  MCP-WIDGET
                WHEN   11
                    MOVE  "SSTKIJUN11"    TO  MCP-WIDGET
                WHEN   12
                    MOVE  "SSTKIJUN12"    TO  MCP-WIDGET
                WHEN   13
                    MOVE  "SSTKIJUN13"    TO  MCP-WIDGET
                WHEN   14
                    MOVE  "SSTKIJUN14"    TO  MCP-WIDGET
                WHEN   15
                    MOVE  "SSTKIJUN15"    TO  MCP-WIDGET
                WHEN   16
                    MOVE  "SSTKIJUN16"    TO  MCP-WIDGET
                WHEN   17
                    MOVE  "SSTKIJUN17"    TO  MCP-WIDGET
                WHEN   18
                    MOVE  "SSTKIJUN18"    TO  MCP-WIDGET
                WHEN   19
                    MOVE  "SSTKIJUN19"    TO  MCP-WIDGET
                WHEN   20
                    MOVE  "SSTKIJUN20"    TO  MCP-WIDGET
                WHEN   21
                    MOVE  "SSTKIJUN21"    TO  MCP-WIDGET
                WHEN   22
                    MOVE  "SSTKIJUN22"    TO  MCP-WIDGET
                WHEN   23
                    MOVE  "SSTKIJUN23"    TO  MCP-WIDGET
                WHEN   24
                    MOVE  "SSTKIJUN24"    TO  MCP-WIDGET
                WHEN   25
                    MOVE  "SSTKIJUN25"    TO  MCP-WIDGET
                 WHEN   90
                    MOVE  "SELCD"         TO  MCP-WIDGET
                 WHEN   99
                    MOVE  "B12"        TO  MCP-WIDGET
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択コード入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "入力エラー"               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "適用コードに入力がありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "選択コードはありません"
                                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W03"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
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
                   MOVE    "登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1003"
                   MOVE    "新規の登録処理をします"
                                               TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします"
                                               TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W03"                TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

