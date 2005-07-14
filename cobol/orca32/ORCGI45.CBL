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
       PROGRAM-ID.                 ORCGI45.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : 雛形一覧（Ｉ４５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    NACL-多々納  04/05/06  会計照会と同じ機能とする
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
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I4SCR-SPA".
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
           03  WRK-JIDMSG          PIC X(80).
      *
           03  WRK-COMMENT         PIC X(400).
      *    コラムリスト位置
           03  WRK-I45-BUNREI-ROW      PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
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
           COPY    "I41.INC".
           COPY    "I42.INC".
           COPY    "I43.INC".
           COPY    "I44.INC".
           COPY    "I45.INC".
           COPY    "I48.INC".
           COPY    "I49.INC".
           COPY    "I4ERR.INC".
           COPY    "I4ID1.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-I41
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU
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
           MOVE    SPA-I4KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I41         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
      *    INITIALIZE                  CNT-AREA
      *    INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
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
           MOVE   "I45"               TO  MCP-WINDOW.
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
           EVALUATE    SPA-MOTOPG
      *            確認画面より
               WHEN    "I4ID1"
                   PERFORM 3003-I4ID1-SET-SEC
                   MOVE    SPACE           TO  SPA-IIDCD
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-I45-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-I4ID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0104"
      *            雛形削除の確認
                   IF      SPA-I4ID-FLG        =   "OK"
                       PERFORM 2401-COMMENT-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-I45-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-I4ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-I45-AREA
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
           MOVE    ZERO                TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-I45-BUNNREI-G
           MOVE    ZERO                TO  SPA-I45-BUNREIMAX
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
               MOVE    IDX                 TO  SPA-I45-BUNREIMAX
               MOVE    EXAMPLE-RENNUM      TO  SPA-I45-TRENNUM  (IDX)
               MOVE    EXAMPLE-COMMENT     TO  SPA-I45-TCOMMENT (IDX)
      *
               MOVE    "EXAMPLE-KEY1"      TO  ORC-DBPATH
               PERFORM 930-EXAMPLE-READ-SEC
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-I45-SELNUM
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
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-I45-BUNREI-ROW
           ELSE
               MOVE    I45-BUNREI-ROW     TO  WRK-I45-BUNREI-ROW
           END-IF
      *
      *
           MOVE    SPACE               TO  I45
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  I45-BUNREI-ROW
           MOVE    ZERO                TO  I45-BUNREI-ROWATTR
           IF      WRK-I45-BUNREI-ROW NOT =   ZERO
               MOVE    WRK-I45-BUNREI-ROW TO  I45-BUNREI-ROW
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I45-BUNREIMAX
               MOVE    SPA-I45-TRENNUM (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  I45-TRENNUM (IDX)
               MOVE    SPA-I45-TCOMMENT(IDX)   TO  I45-TCOMMENT(IDX)
      *
               IF      SPA-I45-SELNUM     =   SPA-I45-TRENNUM (IDX)
                   MOVE    "T"             TO  I45-BUNREI-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  I45-BUNREI-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-I45-BUNREIMAX  TO  I45-BUNREI-COUNT
      *
           MOVE    SPA-I45-SELNUM     TO  I45-SELNUM
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
           EVALUATE    SPA-I45-CUR
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
           MOVE    1                   TO  SPA-I45-CUR
      *
           MOVE    I45-SELNUM         TO  SPA-I45-SELNUM
           MOVE    ZERO                TO  SPA-I45-TBL
      *
           IF      SPA-I45-SELNUM     =   ZERO
               GO      TO      420-SELNUM-EXT
           END-IF
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-I45-BUNREIMAX
              IF      SPA-I45-TRENNUM  (IDX)  =   SPA-I45-SELNUM
                  MOVE    IDX                      TO  SPA-I45-TBL
              END-IF
           END-PERFORM
           IF      SPA-I45-TBL        =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      420-SELNUM-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-I45-CUR
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
                   UNTIL       IDX     >   SPA-I45-BUNREIMAX
              IF      I45-BUNREI-SELECT (IDX)   =  "T"
                  IF      SPA-I45-TRENNUM  (IDX)  =   SPA-I45-SELNUM
                      MOVE    SPA-I45-TRENNUM  (IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-I45-TRENNUM  (IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-I45-SELNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-I45-SELNUM
           END-IF
      *
           MOVE    SPA-I45-SELNUM     TO  I45-SELNUM
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
           IF      SPA-I45-SELNUM     =   ZERO
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *
           MOVE    SPA-I45-SELNUM     TO  I45-SELNUM
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
           MOVE    SPA-I45-TRENNUM (SPA-I45-TBL)
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
                   MOVE    SPA-I43-COMMENT     TO  WRK-COMMENT
                   MOVE    SPACE               TO  SPA-I43-COMMENT
                   STRING  WRK-COMMENT         DELIMITED   BY  SPACE
                           EXAMPLE-COMMENT     DELIMITED   BY  SPACE
                           INTO                SPA-I43-COMMENT
                   END-STRING
               ELSE
      *            コメント置換え
                   MOVE    EXAMPLE-COMMENT     TO  SPA-I43-COMMENT
                   MOVE    SPA-I45-TRENNUM (SPA-I45-TBL)
                                               TO  SPA-I43-BUNNUM
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
           IF      SPA-I45-TBL        =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      240-COMMENT-DEL-EXT
           END-IF
      *
           MOVE    "0104"              TO  SPA-IIDCD
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
           MOVE    SPA-I45-TRENNUM (SPA-I45-TBL)
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
               DISPLAY "I45 EXAMPLE DEL ERR:" MCP-RC "."
           END-IF
      *
      *     文例編集
            PERFORM 3101-BUNREI-HEN-SEC
      *
            MOVE    1                  TO  SPA-I45-CUR
           .
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I43"               TO  SPA-SAKIPG
           MOVE    "I45"               TO  SPA-MOTOPG
      *
      *****MOVE   "BACK"               TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE   "I43"                TO  MCP-WINDOW
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
           IF      SPA-IIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I45"              TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  I4ERR
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I45"               TO  SPA-MOTOPG
           MOVE    "I4ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
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
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-IIDCD
               WHEN    "0104"
                   MOVE    "選択されている文例を削除します。"
                                       TO  WRK-JIDMSG
                   MOVE    "よろしいですか？"
                                       TO  WRK-JIDMSG(35:)
               WHEN    OTHER
                   MOVE    SPA-IIDCD
                                       TO  WRK-JIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-I4ID-FLG
      *
           MOVE    SPACE               TO  I4ID1
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  I4ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I45"               TO  SPA-MOTOPG
           MOVE    "I4ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
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
