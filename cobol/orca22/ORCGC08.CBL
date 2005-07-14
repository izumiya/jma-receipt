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
       PROGRAM-ID.                 ORCGC08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 雛形一覧（Ｃ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/31    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
      *
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-CIDMSG          PIC X(80).
      *
           03  WRK-COMMENT         PIC X(400).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    雛形
       01  EXAMPLE-REC.
           COPY    "CPEXAMPLE.INC".
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
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "C02.INC".
           COPY    "C03.INC".
           COPY    "C04.INC".
           COPY    "C05.INC".
           COPY    "C06.INC".
           COPY    "C07.INC".
           COPY    "C08.INC".
           COPY    "C09.INC".
           COPY    "C10.INC".
           COPY    "C11.INC".
           COPY    "CERR.INC".
           COPY    "CID1.INC".
           COPY    "CID2.INC".
           COPY    "CID3.INC".
           COPY    "CID4.INC".
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
           INITIALIZE                  STS-AREA
      *    INITIALIZE                  CNT-AREA
      *    INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
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
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "C08"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG(1:4)
      *            確認画面より
               WHEN    "CID1"
                   PERFORM 3003-CID1-SET-SEC
                   MOVE    SPACE           TO  SPA-CIDCD
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-C08-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-CID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0104"
      *            雛形削除の確認
                   IF      SPA-CID1-FLG        =   "OK"
                       PERFORM 2401-COMMENT-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-C08-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-CID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-C08-AREA
      *
      *     文例編集
            PERFORM 3101-BUNREI-HEN-SEC
      *
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    文例編集処理
      *****************************************************************
       3101-BUNREI-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-C08-BUNNREI-G
           MOVE    ZERO                TO  SPA-C08-BUNREIMAX
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID
           MOVE    "1"                 TO  EXAMPLE-KUBUN
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "EXAMPLE-KEY1"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "EXAMPLE-KEY1"     TO  ORC-DBPATH
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL      (FLG-EXAMPLE        =   1 ) OR
                              (IDX                >=  100)
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-C08-BUNREIMAX
               MOVE    EXAMPLE-RENNUM      TO  SPA-C08-TRENNUM  (IDX)
               MOVE    EXAMPLE-COMMENT     TO  SPA-C08-TCOMMENT (IDX)
      *
               MOVE    "EXAMPLE-KEY1"      TO  ORC-DBPATH
               PERFORM 930-EXAMPLE-READ-SEC
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-C08-SELNUM
      *
           .
       3101-BUNREI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  C08
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-C08-BUNREIMAX
               MOVE    SPA-C08-TRENNUM (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  C08-TRENNUM (IDX)
               MOVE    SPA-C08-TCOMMENT(IDX)  TO  C08-TCOMMENT(IDX)
      *
               IF      SPA-C08-SELNUM     =   SPA-C08-TRENNUM (IDX)
                   MOVE    "T"             TO  C08-BUNREI-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  C08-BUNREI-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-C08-BUNREIMAX  TO  C08-BUNREI-COUNT
           MOVE    ZERO               TO  C08-BUNREI-ROW
      *
           MOVE    SPA-C08-SELNUM     TO  C08-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           EVALUATE    SPA-C08-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 240-COMMENT-DEL-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    1               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    2               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
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
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        明細選択
               WHEN    ANY             ALSO    "BUNREILIST"
                   PERFORM 440-BUNREILIST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-C08-CUR
      *
           MOVE    C08-SELNUM         TO  SPA-C08-SELNUM
           MOVE    ZERO                TO  SPA-C08-TBL
      *
           IF      SPA-C08-SELNUM     =   ZERO
               GO      TO      420-SELNUM-EXT
           END-IF
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-C08-BUNREIMAX
              IF      SPA-C08-TRENNUM  (IDX)  =   SPA-C08-SELNUM
                  MOVE    IDX                      TO  SPA-C08-TBL
              END-IF
           END-PERFORM
           IF      SPA-C08-TBL        =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      420-SELNUM-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-C08-CUR
      *
           .
       420-SELNUM-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       440-BUNREILIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-C08-BUNREIMAX
              IF      C08-BUNREI-SELECT (IDX)   =  "T"
                  IF      SPA-C08-TRENNUM  (IDX)  =   SPA-C08-SELNUM
                      MOVE    SPA-C08-TRENNUM  (IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-C08-TRENNUM  (IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-C08-SELNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-C08-SELNUM
           END-IF
      *
           MOVE    SPA-C08-SELNUM     TO  C08-SELNUM
           PERFORM 420-SELNUM-SEC
      *
           .
       440-BUNREILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
           IF      SPA-C08-SELNUM     =   ZERO
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *
           MOVE    SPA-C08-SELNUM     TO  C08-SELNUM
           PERFORM 420-SELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *    雛形有無
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-C08-TRENNUM (SPA-C08-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           IF      FLG-EXAMPLE         =   ZERO
               IF      FLG-OK              =   1
      *            コメント追加
                   MOVE    SPA-C07-COMMENT    TO  WRK-COMMENT
                   MOVE    SPACE               TO  SPA-C07-COMMENT
                   STRING  WRK-COMMENT         DELIMITED   BY  SPACE
                           EXAMPLE-COMMENT     DELIMITED   BY  SPACE
                           INTO                SPA-C07-COMMENT
                   END-STRING
               ELSE
      *            コメント置換え
                   MOVE    EXAMPLE-COMMENT     TO  SPA-C07-COMMENT
                   MOVE    SPA-C08-TRENNUM (SPA-C08-TBL)
                                               TO  SPA-C07-BUNNUM
               END-IF
               PERFORM 210-BACK
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       240-COMMENT-DEL-SEC                   SECTION.
      *
           IF      SPA-C08-TBL        =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      240-COMMENT-DEL-EXT
           END-IF
      *
           MOVE    "0104"              TO  SPA-CIDCD
           .
       240-COMMENT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       2401-COMMENT-DELSYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPID          TO  EXAMPLE-HOSPID
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-C08-TRENNUM (SPA-C08-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "EXAMPLE-KEY"       TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "C08 EXAMPLE DEL ERR:" MCP-RC "."
           END-IF
      *
      *     文例編集
            PERFORM 3101-BUNREI-HEN-SEC
      *
            MOVE    1                  TO  SPA-C08-CUR
           .
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C07"              TO  SPA-SAKIPG
           MOVE    "C08"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"             TO  MCP-PUTTYPE
           MOVE   "C07"               TO  MCP-WINDOW
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "C08"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "保険組合せ選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "選択番号を選択して下さい"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  CERR
           MOVE    SPA-ERRCD           TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  CERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C08"              TO  SPA-MOTOPG
           MOVE    "CERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD
               WHEN    "0104"
                   MOVE    "選択されている文例を削除します。"
                                       TO  WRK-CIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-CIDMSG(35:)
               WHEN    OTHER
                   MOVE    SPA-CIDCD
                                       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-CID1-FLG
      *
           MOVE    SPACE               TO  CID1
           MOVE    SPA-CIDCD           TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG          TO  CID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C08"              TO  SPA-MOTOPG
           MOVE    "CID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "CID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形読込
      *****************************************************************
       930-EXAMPLE-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  EXAMPLE-REC
               MOVE    ZERO                TO  FLG-EXAMPLE
           ELSE
               INITIALIZE                      EXAMPLE-REC
               MOVE    1                   TO  FLG-EXAMPLE
           END-IF
      *
           .
       930-EXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCMCPSUB"   USING
                                MCPAREA
                                ORCMCPAREA
                                MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
