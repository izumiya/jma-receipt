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
       PROGRAM-ID.                 ORCGMUID.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 収集データアップロードユーザ設定（ＭＵＩＤ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/10/11    多々納        新規作成
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
      *    データ
           SELECT  USER-FILE       ASSIGN  FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-DATA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    データ
       FD  USER-FILE.
       01  USER-REC                     PIC X(600).
      *
       WORKING-STORAGE             SECTION.
      *
      * データ領域
       01  FILE-NAME.
           03  FILLER              PIC X(40)   VALUE
           "/etc/jma-receipt/das-upload.d/das-upload".
           03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
           03  FILLER              PIC X(05)   VALUE   ".conf".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DATA            PIC X(02).
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-DATA-REC1.
           03  FILLER             PIC X(11)   VALUE
               'config = {'.
       01  WRK-DATA-REC2.
           03  FILLER             PIC X(45)   VALUE
               '"Host" => "https://orca-das.orca.med.or.jp",'.
       01  WRK-DATA-REC3.
           03  FILLER             PIC X(33)   VALUE
               '"Path" => "/orca_das/upload.cgi",'.
       01  WRK-DATA-REC4.
           03  FILLER             PIC X(23)   VALUE
               '"CertificateCAFile" => '.
           03  FILLER             PIC X(39)   VALUE
               '"/etc/ssl/certs/orca-project-ca-1.crt",'.
       01  WRK-DATA-REC5.
           03  FILLER             PIC X(11)   VALUE
               '"User" => "'.
       01  WRK-DATA-REC52.
           03  FILLER             PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC6.
           03  FILLER             PIC X(15)   VALUE
               '"Password" => "'.
       01  WRK-DATA-REC62.
           03  FILLER             PIC X(01)   VALUE
               '"'.
       01  WRK-DATA-REC7.
           03  FILLER             PIC X(11)   VALUE
               "}".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *
           COPY    "MCPDATA.INC".
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
           COPY    "M98.INC".
           COPY    "MUID.INC".
      *
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  STS-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
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
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-M01             TO  SPA-FREE
           .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
      *        終了時のメッセージ
               IF      SPA-MUID-END         =   1
                   PERFORM 210-BACK
               ELSE
                   MOVE    SPACE               TO  SPA-MOTOPG
                   PERFORM 5001-MAPCUR-SEC
               END-IF
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "MUID"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
      *            確認画面より
                   PERFORM 30010-MID1-SET-SEC
                   MOVE    SPACE               TO  SPA-MIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       30010-MID1-SET-SEC          SECTION.
      *
           .
       30010-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-MUID-AREA
           MOVE    1                   TO  SPA-MUID-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        登録更新
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 212-TOROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-MUID-CUR
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    MUID-USER           TO  SPA-MUID-USER
           MOVE    MUID-PASSWORD       TO  SPA-MUID-PASSWORD
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
           IF      SPA-MUID-USER   NOT =   SPACE
               PERFORM 4102-USER-CHK-SEC
           END-IF
      *
           IF      SPA-MUID-PASSWORD   NOT =   SPACE
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4102-PASSWORD-CHK-SEC
               END-IF
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザーチェック処理
      *****************************************************************
       4102-USER-CHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-MUID-USER       TO  KANACHK-MAE-INPUT
           MOVE    30                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-MUID-CUR
           END-IF
           .
       4102-USER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パスワードチェック処理
      *****************************************************************
       4102-PASSWORD-CHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-MUID-PASSWORD   TO  KANACHK-MAE-INPUT
           MOVE    30                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )  AND
                  (KANACHK-RC2         =   ZERO  )
               CONTINUE
           ELSE
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-MUID-CUR
           END-IF
           .
       4102-PASSWORD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       212-TOROKU-SEC                  SECTION.
      *
           MOVE    ZERO                TO  SPA-MUID-END
           MOVE    ZERO                TO  SPA-MUID-CUR
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF     (SPA-MUID-USER       NOT =   SPACE)  OR
                  (SPA-MUID-PASSWORD   NOT =   SPACE)
      *        必須入力チェック
               IF      SPA-MUID-USER       =   SPACE
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-MUID-CUR
               ELSE
                   IF      SPA-MUID-PASSWORD   =   SPACE
                       MOVE    "0004"          TO  SPA-ERRCD
                       MOVE    2               TO  SPA-MUID-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2120-TXT-DATA-OUT-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1000"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-MUID-END
           END-IF
           .
       212-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2120-TXT-DATA-OUT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM
      *
           OPEN    OUTPUT              USER-FILE
           IF      STS-DATA        NOT =   ZERO
               MOVE    "9000"          TO  SPA-ERRCD
               GO      TO      2120-TXT-DATA-OUT-EXT
           END-IF
           MOVE    SPACE               TO  USER-REC
           MOVE    WRK-DATA-REC1       TO  USER-REC
           PERFORM 21201-DATA-WRITE-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC2       TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC3       TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC4       TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC5       DELIMITED  BY  SIZE
                       SPA-MUID-USER       DELIMITED  BY  SPACE
                       WRK-DATA-REC52      DELIMITED  BY  SIZE
                                       INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC6       DELIMITED  BY  SIZE
                       SPA-MUID-PASSWORD   DELIMITED  BY  SPACE
                       WRK-DATA-REC62      DELIMITED  BY  SIZE
                                       INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC7       TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           CLOSE                       USER-FILE
           .
       2120-TXT-DATA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       21201-DATA-WRITE-SEC            SECTION.
      *
           WRITE                       USER-REC
           IF      STS-DATA        NOT =   ZERO
               MOVE    "9001"          TO  SPA-ERRCD
           END-IF
     *
           .
       21201-DATA-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M98"               TO  SPA-SAKIPG
           MOVE    "MUID"              TO  SPA-MOTOPG
      *
           MOVE    "M98"               TO  MCP-WINDOW
           MOVE    "JOIN"              TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
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
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-MIDCD       NOT =   SPACE )
               PERFORM 520-MIDSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    "CURRENT"           TO  MCP-PUTTYPE
               MOVE    "MUID"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  MUID
           INITIALIZE                  MUID
      *
           MOVE    SPA-MUID-USER       TO  MUID-USER
           MOVE    SPA-MUID-PASSWORD   TO  MUID-PASSWORD
      *
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-MUID-CUR        =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
           EVALUATE    SPA-MUID-CUR
               WHEN    1
                   MOVE    "USER"              TO  MCP-WIDGET
               WHEN    2
                   MOVE    "PASSWORD"          TO  MCP-WIDGET
               WHEN    9
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           MOVE    1                   TO  SPA-MUID-CUR
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "USER"
                   MOVE    2               TO  SPA-MUID-CUR
               WHEN    "PASSWORD"
                   MOVE    9               TO  SPA-MUID-CUR
           END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "ＵＳＥＲ入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "ＵＳＥＲを入力して下さい。"
                                                TO  SPA-ERRMSG
                WHEN    "0003"
                    MOVE    "ＰＡＳＳＷＯＲＤ入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0004"
                    MOVE    "ＰＡＳＳＷＯＲＤを入力して下さい。"
                                                TO  SPA-ERRMSG
                WHEN    "1000"
                    MOVE    "データ出力が正常終了しました。"
                                                TO  SPA-ERRMSG
                WHEN    "9000"
                    MOVE    "データＯＰＥＮがエラーです"
                                                TO  SPA-ERRMSG
                WHEN    "9001"
                    MOVE    "データ出力がエラーです"
                                                TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE               TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET.
           MOVE    "MUID"              TO  SPA-MOTOPG.
           MOVE    "MERR"              TO  SPA-SAKIPG.
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "MERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  MID1
      *
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "MUID"              TO  SPA-MOTOPG
           MOVE    "MID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "MID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
