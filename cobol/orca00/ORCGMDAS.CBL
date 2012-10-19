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
       PROGRAM-ID.                 ORCGMDAS.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メニュー
      *  コンポーネント名  : 情報（ＭＤＡＳ）
      *  管理者            : 
      *  09/11/04    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
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
       01  USER-REC                PIC X(600).
      *
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
      *    診療行為用共通パラメタ
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DATA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-USER            PIC 9(01).
           03  FLG-JOBLOG          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-STR-EDIT        PIC X(50).
      *
       01  WRK-DATA-REC1.
           03  FILLER              PIC X(11)   VALUE
               'config = {'.
       01  WRK-DATA-REC2.
           03  FILLER             PIC X(45)   VALUE
               '"Host" => "https://orca-das.orca.med.or.jp",'.
       01  WRK-DATA-REC3.
           03  FILLER              PIC X(33)   VALUE
               '"Path" => "/orca_das/upload.cgi",'.
       01  WRK-DATA-REC32.
           03  FILLER              PIC X(40)   VALUE
               '"PathProf" => "/das_profile/upload.cgi",'.
       01  WRK-DATA-REC33.
           03  FILLER              PIC X(37)   VALUE
               '"PathInf" => "/infection/upload.cgi",'.
       01  WRK-DATA-REC4.
           03  FILLER              PIC X(23)   VALUE
               '"CertificateCAFile" => '.
           03  FILLER             PIC X(39)   VALUE
               '"/etc/ssl/certs/orca-project-ca-1.crt",'.
       01  WRK-DATA-REC5.
           03  FILLER              PIC X(17)   VALUE
               '"HospitalID" => "'.
       01  WRK-DATA-REC52.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC6.
           03  FILLER              PIC X(21)   VALUE
               '"ApplicationDay" => "'.
       01  WRK-DATA-REC62.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC7.
           03  FILLER              PIC X(20)   VALUE
               '"ClerkNameKana" => "'.
       01  WRK-DATA-REC72.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC8.
           03  FILLER              PIC X(16)   VALUE
               '"ClerkName" => "'.
       01  WRK-DATA-REC82.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC9.
           03  FILLER              PIC X(17)   VALUE
               '"ClerkEmail" => "'.
       01  WRK-DATA-REC92.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC10.
           03  FILLER              PIC X(20)   VALUE
               '"HospitalClass" => "'.
       01  WRK-DATA-REC102.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC11.
           03  FILLER              PIC X(22)   VALUE
               '"GeneralHospital" => "'.
       01  WRK-DATA-REC112.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC12.
           03  FILLER              PIC X(21)   VALUE
               '"SickbedGeneral" => "'.
       01  WRK-DATA-REC122.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC13.
           03  FILLER              PIC X(26)   VALUE
               '"SickbedMedTreatment" => "'.
       01  WRK-DATA-REC132.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC14.
           03  FILLER              PIC X(20)   VALUE
               '"SickbedMental" => "'.
       01  WRK-DATA-REC142.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC15.
           03  FILLER              PIC X(26)   VALUE
               '"SickbedTuberculosis" => "'.
       01  WRK-DATA-REC152.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC16.
           03  FILLER              PIC X(19)   VALUE
               '"SickbedTotal" => "'.
       01  WRK-DATA-REC162.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC17.
           03  FILLER              PIC X(20)   VALUE
               '"MainlyInstall" => "'.
       01  WRK-DATA-REC172.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC18.
           03  FILLER              PIC X(26)   VALUE
               '"MainlyMedDepartment" => "'.
       01  WRK-DATA-REC182.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC19.
           03  FILLER              PIC X(19)   VALUE
               '"CommunityMed" => "'.
       01  WRK-DATA-REC192.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC20.
           03  FILLER              PIC X(10)   VALUE
               '"DPC" => "'.
       01  WRK-DATA-REC202.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC21.
           03  FILLER              PIC X(18)   VALUE
               '"HomeMedCare" => "'.
       01  WRK-DATA-REC212.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC22.
           03  FILLER              PIC X(18)   VALUE
               '"OpeningTime" => "'.
       01  WRK-DATA-REC222.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC23.
           03  FILLER              PIC X(15)   VALUE
               '"Postcode" => "'.
       01  WRK-DATA-REC232.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC24.
           03  FILLER              PIC X(11)   VALUE
               '"User" => "'.
       01  WRK-DATA-REC242.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC25.
           03  FILLER              PIC X(15)   VALUE
               '"Password" => "'.
       01  WRK-DATA-REC252.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC26.
           03  FILLER              PIC X(16)   VALUE
               '"Agreement" => "'.
       01  WRK-DATA-REC262.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC27.
           03  FILLER              PIC X(17)   VALUE
               '"SendResult" => "'.
       01  WRK-DATA-REC272.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC28.
           03  FILLER              PIC X(15)   VALUE
               '"SendDate" => "'.
       01  WRK-DATA-REC282.
           03  FILLER              PIC X(01)   VALUE
               '"'.
       01  WRK-DATA-REC29.
           03  FILLER              PIC X(11)   VALUE
               "}".
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PROFILE".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
       01  SHELL-AREA.
           05  SHELL-NAME          PIC X(32) VALUE
                                   "scripts/allways/das-profile.sh".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブログ
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *
      *    シェル起動
           COPY    "CPSHELLTBL.INC".
           COPY    "CPMKPASS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
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
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
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
           INITIALIZE                  STS-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      * 
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
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
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 320-MID1-SET-SEC
               WHEN    "MID2"
                   PERFORM 320-MID2-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-MIDCD
           IF      SPA-MID1-FLG        =   "OK"
      *        申請データを出力する
               MOVE    SPA-HOSPNUM     TO  FILE-HOSPNUM
               OPEN    OUTPUT              USER-FILE
               IF      STS-DATA        NOT =   ZERO
                   MOVE    "9000"          TO  SPA-ERRCD
               ELSE
                   PERFORM 2120-TXT-DATA-OUT-SEC
               END-IF
               CLOSE                       USER-FILE
      *        シェルを起動する
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 212-SHELL-OUT-SEC
               END-IF
           END-IF
      *
           .
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ２）ＯＫ処理
      *****************************************************************
       320-MID2-SET-SEC            SECTION.
      *
           IF      SPA-MIDCD       NOT =   "0000"
               GO  TO  320-MID2-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MDAS"          TO  SPA-MOTOPG
      *
           MOVE    "M98"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       320-MID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       212-SHELL-OUT-SEC           SECTION.
      *
      *    シェル起動処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    SHELL-NAME      TO  MKPASS-IN-01
      *     MOVE    SPA-TERMID      TO  SHELLTBL-ARG1
      *     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
      *     MOVE    WRK-CONS-JOB-JOBID
      *                             TO  SHELLTBL-ARG3
      *     MOVE    WRK-CONS-JOB-SHELLID
      *                             TO  SHELLTBL-ARG4
      *     MOVE    SPA-HOSPID      TO  SHELLTBL-ARG5
      *     MOVE    0               TO  SHELLTBL-ARG6
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG2
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  MDAS-DURATION
               MOVE    "1000"          TO  SPA-MIDCD
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       212-SHELL-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況
      *****************************************************************
       213-JYOKYO-SEC              SECTION.
      *
           PERFORM 900-JOBLOG-READ-SEC
      *
           IF      FLG-JOBLOG          =   1
               MOVE    5               TO  MDAS-DURATION
               MOVE    SPACE           TO  SPA-MIDCD
           ELSE
               MOVE    SPACE           TO  SPA-MIDCD
               MOVE    ZERO            TO  MDAS-DURATION
               IF      JOBLOG-ERRCD        =   "000"
                                           OR  "401"
                                           OR  "403"
                                           OR  "404"
                                           OR  "999"
                   STRING  "0"                 DELIMITED BY SIZE
                           JOBLOG-ERRCD        DELIMITED BY SIZE
                           INTO                SPA-MIDCD
                   END-STRING
               END-IF
           END-IF
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       213-JYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "9000"
                    MOVE    "設定ファイルのＯＰＥＮに失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    "9001"
                    MOVE    "設定ファイルの出力に失敗しました。"
                                           TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE          TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
      *
           MOVE    "B01"           TO  MCP-WIDGET.
           MOVE    "MDAS"          TO  SPA-MOTOPG.
           MOVE    "MERR"          TO  SPA-SAKIPG.
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        同意しない
               WHEN    "CLICKED"   ALSO    "B05"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-FILECHECK-SEC
      *        同意する
               WHEN    "CLICKED"   ALSO    "B08"
      *2012.10 サーベイ簡易設定改修
      *        メッセージダイアログからの復帰と同様になりすます
                   IF      SPA-MOTOPG      =   "M00"
                        MOVE    "9102"          TO  SPA-MIDCD
                   END-IF
      *
                   MOVE    "OK"            TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        タイマーイベント
               WHEN    "CLICKED"   ALSO    "pandatimer1"
                   PERFORM 213-JYOKYO-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *2012.10 サーベイ簡易設定改修
           IF      SPA-MOTOPG      =   "M00"
               MOVE    "M00"           TO  SPA-SAKIPG
               MOVE    "M00"           TO  MCP-WINDOW
           ELSE
               MOVE    "M98"           TO  SPA-SAKIPG
               MOVE    "M98"           TO  MCP-WINDOW
           END-IF
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請ファイルチェック処理
      *****************************************************************
       210-FILECHECK-SEC           SECTION.
      *
           INITIALIZE                  SPA-MUID-AREA
           MOVE    ZERO            TO  FLG-OK
      *
      *    申請データがあればチェックする
           MOVE    SPA-HOSPNUM     TO  FILE-HOSPNUM
           OPEN    INPUT               USER-FILE
           IF      STS-DATA            =   ZERO
               PERFORM 3120-TXT-DATA-IN-SEC
           END-IF
           CLOSE             USER-FILE
      *
           MOVE    SPA-HOSPID      TO  SPA-MUID-HOSPID
           MOVE    SPA-SYSYMDWH    TO  SPA-MUID-SINSEIYMD
           MOVE    SPA-SYSYMD      TO  SPA-MUID-NAI-SINSEIYMD
      *
      *    申請してなければ戻る
           IF      FLG-OK              =   ZERO
               MOVE    SPACE           TO  SPA-MID1-FLG
               PERFORM 210-BACK
           ELSE
               PERFORM 220-MID1SET-SEC
           END-IF
      *
           .
       210-FILECHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データセット処理
      *****************************************************************
       3120-TXT-DATA-IN-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-USER
           MOVE    SPACE           TO  USER-REC
           PERFORM UNTIL   FLG-USER    =   1
                   OR      FLG-OK      =   1
               READ    USER-FILE       NEXT
                   AT  END
                       MOVE    1           TO  FLG-USER
                   NOT AT  END
                       IF      USER-REC(1:17)  =   WRK-DATA-REC5
                           UNSTRING  USER-REC(18:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-HOSPID
                           END-UNSTRING
                           IF      SPA-HOSPID  NOT =   SPA-MUID-HOSPID
                               MOVE    1       TO  FLG-USER
                           END-IF
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC7
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-KANANAME
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:16)  =   WRK-DATA-REC8
                           UNSTRING  USER-REC(17:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-NAME
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:17)  =   WRK-DATA-REC9
                           UNSTRING  USER-REC(18:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-EMAIL
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC10
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-HOSPSBT
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:22)  =   WRK-DATA-REC11
                           UNSTRING  USER-REC(23:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-HOSPSBT1
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:21)  =   WRK-DATA-REC12
                           UNSTRING  USER-REC(22:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-BEDSUIPPAN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC13
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-BEDSURYOYO
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC14
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-BEDSUSEISIN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC15
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-BEDSUKEKKAKU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:19)  =   WRK-DATA-REC16
                           UNSTRING  USER-REC(20:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-BEDSU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC17
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-SETTI
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC18
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-SYUKA
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:19)  =   WRK-DATA-REC19
                           UNSTRING  USER-REC(20:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-CIIKI
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:10)  =   WRK-DATA-REC20
                           UNSTRING  USER-REC(11:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-DPCKBN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:18)  =   WRK-DATA-REC21
                           UNSTRING  USER-REC(19:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-ZAITAKU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:18)  =   WRK-DATA-REC22
                           UNSTRING  USER-REC(19:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-NAI-KAIINYM
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:15)  =   WRK-DATA-REC23
                           UNSTRING  USER-REC(16:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-POST
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:11)  =   WRK-DATA-REC24
                           UNSTRING  USER-REC(12:) DELIMITED  BY  '",'
                                             INTO  SPA-MUID-USER
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:15)  =   WRK-DATA-REC25
                           UNSTRING  USER-REC(16:) DELIMITED  BY  '",'
                                             INTO  WRK-STR-EDIT
                           END-UNSTRING
                           UNSTRING  WRK-STR-EDIT
                                             DELIMITED  BY  '"'
                                             INTO  SPA-MUID-PASSWORD
                           END-UNSTRING
                       END-IF
                       IF    ( USER-REC(1:17)  =   WRK-DATA-REC27 )
                       AND   ( USER-REC(18:3)  =   "000" )
                           MOVE    1           TO  FLG-OK
                       END-IF
               END-READ
           END-PERFORM
      *
           .
       3120-TXT-DATA-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       2120-TXT-DATA-OUT-SEC       SECTION.
      *
           MOVE    SPACE           TO  USER-REC
           MOVE    WRK-DATA-REC1   TO  USER-REC
           PERFORM 21201-DATA-WRITE-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC2   TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC3   TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC32  TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC33  TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC4   TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC5           DELIMITED  BY  SIZE
                       SPA-MUID-HOSPID         DELIMITED  BY  SPACE
                       WRK-DATA-REC52          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC6           DELIMITED  BY  SIZE
                       SPA-MUID-NAI-SINSEIYMD  DELIMITED  BY  SPACE
                       WRK-DATA-REC62          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC7           DELIMITED  BY  SIZE
                       SPA-MUID-KANANAME       DELIMITED  BY  SPACE
                       WRK-DATA-REC72          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC8           DELIMITED  BY  SIZE
                       SPA-MUID-NAME           DELIMITED  BY  SPACE
                       WRK-DATA-REC82          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC9           DELIMITED  BY  SIZE
                       SPA-MUID-EMAIL          DELIMITED  BY  SPACE
                       WRK-DATA-REC92          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC10          DELIMITED  BY  SIZE
                       SPA-MUID-HOSPSBT        DELIMITED  BY  SPACE
                       WRK-DATA-REC102         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC11          DELIMITED  BY  SIZE
                       SPA-MUID-HOSPSBT1       DELIMITED  BY  SPACE
                       WRK-DATA-REC112         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC12          DELIMITED  BY  SIZE
                       SPA-MUID-BEDSUIPPAN     DELIMITED  BY  SPACE
                       WRK-DATA-REC122         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC13          DELIMITED  BY  SIZE
                       SPA-MUID-BEDSURYOYO     DELIMITED  BY  SPACE
                       WRK-DATA-REC132         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC14          DELIMITED  BY  SIZE
                       SPA-MUID-BEDSUSEISIN    DELIMITED  BY  SPACE
                       WRK-DATA-REC142         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC15          DELIMITED  BY  SIZE
                       SPA-MUID-BEDSUKEKKAKU   DELIMITED  BY  SPACE
                       WRK-DATA-REC152         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC16          DELIMITED  BY  SIZE
                       SPA-MUID-BEDSU          DELIMITED  BY  SPACE
                       WRK-DATA-REC162         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC17          DELIMITED  BY  SIZE
                       SPA-MUID-SETTI          DELIMITED  BY  SPACE
                       WRK-DATA-REC172         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC18          DELIMITED  BY  SIZE
                       SPA-MUID-SYUKA          DELIMITED  BY  SPACE
                       WRK-DATA-REC182         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC19          DELIMITED  BY  SIZE
                       SPA-MUID-CIIKI          DELIMITED  BY  SPACE
                       WRK-DATA-REC192         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC20          DELIMITED  BY  SIZE
                       SPA-MUID-DPCKBN         DELIMITED  BY  SPACE
                       WRK-DATA-REC202         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC21          DELIMITED  BY  SIZE
                       SPA-MUID-ZAITAKU        DELIMITED  BY  SPACE
                       WRK-DATA-REC212         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC22          DELIMITED  BY  SIZE
                       SPA-MUID-NAI-KAIINYM    DELIMITED  BY  SPACE
                       WRK-DATA-REC222         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC23          DELIMITED  BY  SIZE
                       SPA-MUID-POST           DELIMITED  BY  SPACE
                       WRK-DATA-REC232         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC24          DELIMITED  BY  SIZE
                       SPA-MUID-USER           DELIMITED  BY  SPACE
                       WRK-DATA-REC242         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC25          DELIMITED  BY  SIZE
                       SPA-MUID-PASSWORD       DELIMITED  BY  SPACE
                       WRK-DATA-REC252         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC26          DELIMITED  BY  SIZE
                       "0"                     DELIMITED  BY  SPACE
                       WRK-DATA-REC262         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
      *ここから下の項目はスクリプトでセットするらしい
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC27          DELIMITED  BY  SIZE
                       WRK-DATA-REC272         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC28          DELIMITED  BY  SIZE
                       WRK-DATA-REC282         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-DATA-REC29      TO  USER-REC
               PERFORM 21201-DATA-WRITE-SEC
           END-IF
      *
           .
       2120-TXT-DATA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       21201-DATA-WRITE-SEC        SECTION.
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
      *    確認メッセージ表示処理
      *****************************************************************
       220-MID1SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID1
      *
           MOVE    "1001"          TO  MID1-ID1CODE
           MOVE
           "申請情報（参加しない）を登録（送信）しますか？"
                                   TO  MID1-ID1MSG
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID1SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       220-MID2SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID2
      *
           MOVE    SPA-MIDCD       TO  MID2-ID1CODE
      *
           EVALUATE    SPA-MIDCD
               WHEN    "1000"
                   MOVE
       "登録作業中です。２〜３秒待ってからボタンをクリックして下さい。"
                                       TO  MID2-ID1MSG
               WHEN    "0000"
                   MOVE    "申請情報を登録（送信）しました。"
                                       TO  MID2-ID1MSG
               WHEN    "0401"
                   MOVE    "定点参加番号またはパスワードが不正です。"
                                       TO  MID2-ID1MSG
               WHEN    "0403"
                   MOVE    "医療機関ＩＤが不正です。"
                                       TO  MID2-ID1MSG
               WHEN    "0404"
                   MOVE    "ファイルが見つかりません。"
                                       TO  MID2-ID1MSG
               WHEN    "0999"
                   MOVE    "収集サーバが見つかりません。"
                                       TO  MID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID2"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID2"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブログ読み込み
      *****************************************************************
       900-JOBLOG-READ-SEC         SECTION.
      *
           INITIALIZE                  JOBLOG-REC
           MOVE    SPA-HOSPNUM     TO  JOBLOG-HOSPNUM
           MOVE    "das-profile"   TO  JOBLOG-PROGNAME
           MOVE    JOBLOG-REC      TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_joblog"    TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JOBLOG
                   MOVE    MCPDATA-REC     TO  JOBLOG-REC
               ELSE
                   MOVE    1               TO  FLG-JOBLOG
               END-IF
           ELSE
               MOVE    1               TO  FLG-JOBLOG
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
            .
       900-JOBLOG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
