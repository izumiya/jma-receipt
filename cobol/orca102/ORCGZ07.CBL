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
       PROGRAM-ID.                               ORCGZ07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 自院コード期限切れ変更（Ｚ０７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/12/12  MONFUNC対応 他
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
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア(検索画面用）
           COPY    "Z01SCR-SPA".
      *
      *    画面ＳＰＡエリア
      *
       01  SPA-Z07.
           03  SPA-GMN-CUR                 PIC 9(02).
           03  SPA-GMN-MAX                 PIC 9(04).
           03  SPA-GMN-PAGE                PIC 9(02).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-YUKOYMD          PIC X(09).
               05  SPA-GMN-TEN-LIST.
                   07  SPA-GMN-TEN-LST-OCC         OCCURS   200.
                       09  SPA-GMN-TNUM            PIC 9(03).
                       09  SPA-GMN-TINPUTCD        PIC X(20).
                       09  SPA-GMN-TSRYCD          PIC X(09).
                       09  SPA-GMN-TNAME           PIC X(100).
                       09  SPA-GMN-TYUKOSTYMD      PIC X(09).
                       09  SPA-GMN-TYUKOEDYMD      PIC X(09).
      *
                       09  SPA-NAI-TYKZKBN         PIC X(01).
                       09  SPA-NAI-TNAME           PIC X(100).
      *
                   07  SPA-TENSU-MAX               PIC 9(04).
      *
               05  SPA-GMN-IN-AREA.
                   07  SPA-GMN-SELNUM              PIC 9(003).
                   07  SPA-GMN-OLD-SRYCD           PIC X(09).
                   07  SPA-GMN-OLD-NAME            PIC X(100).
                   07  SPA-NAI-OLD-NAME            PIC X(100).
                   07  SPA-NAI-OLD-YKZKBN          PIC X(01).
      *
                   07  SPA-GMN-SRYCD-G.
                       09  SPA-GMN-SRYCD           PIC X(09).
                       09  SPA-GMN-FIL             PIC X(11).
                   07  SPA-GMN-NAME            PIC X(100).
      *
               05  SPA-GMN-INPUTCD-G.
                   07  SPA-GMN-INPUTCD-OCC     OCCURS  10.
                       09  SPA-GMN-INPUTCD     PIC X(20).
                       09  SPA-GMN-CDSYU       PIC X(01).
      *
               05  SPA-GMN-NEXT            PIC 9(01).
      *
             03  SPA-NAI-AREA.
                 05  SPA-NAI-YUKOYMD       PIC X(08).
      *
                 05  SPA-NEXT-YUKOYMD      PIC X(08).
      *
                 05  SPA-NAI-CURIDX        PIC 9(02).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-INPUTCD                     PIC 9(01).
           03  FLG-SRYCDCHG                     PIC 9(01).
      *
           03  FLG-KANJI                       PIC 9(01).
           03  FLG-KENSAKU                     PIC 9(01).
           03  FLG-ERR                         PIC 9(01).
           03  FLG-OK                          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDX-C                             PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-ZZZ                 PIC ZZZ.
      *
           03  WRK-IDX2                PIC 9(02).
      *
           03  WRK-STR                 PIC 9(04).
      *
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-INPUTCD             PIC X(20).
      *    パス名
           03  WRK-PATH                PIC X(64).
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-MAE-SRYCD           PIC X(09).
      *
           03  WRK-TNS-YUKOSTYMD       PIC X(08).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
            COPY    "CPINPUTCD.INC".
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           COPY    "Z01.INC".
           COPY    "Z02.INC".
           COPY    "Z03.INC".
           COPY    "Z04.INC".
           COPY    "Z05.INC".
           COPY    "Z06.INC".
           COPY    "Z07.INC".
           COPY    "Z08.INC".
           COPY    "Z90.INC".
           COPY    "Z91.INC".
           COPY    "Z97.INC".
           COPY    "Z98.INC".
           COPY    "Z99.INC".
           COPY    "ZERR.INC".
           COPY    "ZID1.INC".
           COPY    "ZID2.INC".
           COPY    "ZID3.INC".
           COPY    "Z100.INC".
           COPY    "ZFIL.INC".
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
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            IDX-AREA
           INITIALIZE                            WRK-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z07
      *
           MOVE    SPACE                         TO  SPA-ERRCD
           MOVE    ZERO                          TO  FLG-END
      *
           EVALUATE    MCP-STATUS       ALSO     MCP-EVENT
               WHEN    "LINK"           ALSO     ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z07             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z07    "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG(1:3)
               WHEN    "Z98"
      *            一覧検索
                   PERFORM 320-Z98-SPASET-SEC
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
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
           INITIALIZE                      SPA-Z07
      *    システム日付の末日を有効日とする
           MOVE    SPA-SYSYMD          TO  SPA-NAI-YUKOYMD
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SYSYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  SPA-NAI-YUKOYMD
           END-IF
           MOVE    SPA-NAI-YUKOYMD     TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOYMD
      *
      *    次回有効日を決定する
      *    有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  SPA-NEXT-YUKOYMD
           ELSE
               MOVE    SPA-NAI-YUKOYMD     TO  SPA-NEXT-YUKOYMD
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           IF      SPA-TENSU-MAX       >   ZERO
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    期限切れ点数マスタ一覧表編集処理
      *****************************************************************
       3101-TENSU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-TEN-LIST
      *
           IF      SPA-GMN-PAGE        =   1   OR  ZERO
               MOVE    1                   TO  WRK-STR
           ELSE
               COMPUTE WRK-STR             =  (SPA-GMN-PAGE
                                           -  1   )
                                           *  200
                                           +  1
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ALL "0"             TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOYMD     TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WRK-MAE-SRYCD
           PERFORM UNTIL    (FLG-TENSU     =   1  )  OR
                            (IDX           >=  200)
               MOVE    1               TO  FLG-OK
               IF      FLG-OK              =   1
      *
      *        テーブル編集
                   ADD     1                   TO  IDY
                   IF      IDY                 >=  WRK-STR
                       ADD     1               TO  IDX
      *
                       MOVE    IDX             TO  SPA-GMN-TNUM (IDX)
                       MOVE    TNS-SRYCD       TO  SPA-GMN-TSRYCD
                                                                (IDX)
                       MOVE    TNS-NAME        TO  SPA-NAI-TNAME
                                                                (IDX)
                       MOVE    TNS-NAME        TO  SPA-GMN-TNAME
                                                                (IDX)
      *                老人
                       IF      TNS-ROUTEKKBN   =   2
                           MOVE    SPACE           TO  SPA-GMN-TNAME
                                                                (IDX)
                           STRING  "［老］"
                               TNS-NAME            DELIMITED   BY  SIZE
                               INTO                SPA-GMN-TNAME(IDX)
                           END-STRING
                       END-IF
      *
                       MOVE    TNS-YUKOSTYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOSTYMD
                                                                (IDX)
                       MOVE    TNS-YUKOEDYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOEDYMD
                                                                (IDX)
      *                薬剤区分
                       MOVE    TNS-YKZKBN      TO  SPA-NAI-TYKZKBN
                                                                (IDX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    最大件数編集
           MOVE    IDX                 TO  SPA-TENSU-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-TENSU-MAX
      *        入力コード検索
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-GMN-TSRYCD(IDX) TO  ICD-SRYCD
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM 920-INPUTCD-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-INPUTCD         =   ZERO
                   MOVE    ICD-INPUTCD     TO  SPA-GMN-TINPUTCD (IDX)
               END-IF
           END-PERFORM
      *
      *
           IF     (IDX                 >=   200 )  AND
                  (FLG-TENSU           =   ZERO)
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           .
       3101-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98-SPASET-SEC              SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
               MOVE    SPACE               TO  SPA-GMN-SRYCD-G
               MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-SRYCD
               PERFORM 40041-TENSU-CHK-SEC
           ELSE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
       320-Z98-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXT-PAGE-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-KENSAKU-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        有効年月日
               WHEN    "ACTIVATE"       ALSO    "YUKOYMD"
                   PERFORM 4001-YUKOYMD-CHK-SEC
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM 4002-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "TENSU_LIST"
                   PERFORM 4003-TENSU-LIST-SEC
      *        診療コード
               WHEN    "ACTIVATE"       ALSO    "SRYCD"
                   PERFORM 4004-SRYCD-CHK-SEC
      *        その他一括
               WHEN    OTHER
                   PERFORM 4000-ENTRY-SYORI-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効年月日チェック処理
      *****************************************************************
       4001-YUKOYMD-CHK-SEC                        SECTION.
      *
           MOVE    Z07-YUKOYMD         TO  SPA-GMN-YUKOYMD
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-YUKOYMD     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-YUKOYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-YUKOYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4001-YUKOYMD-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           IF      SPA-TENSU-MAX       >   ZERO
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4001-YUKOYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効年月日チェック処理
      *****************************************************************
       4002-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    Z07-SELNUM          TO  SPA-GMN-SELNUM
           IF      SPA-GMN-SELNUM      <   1   OR
                                       >   SPA-TENSU-MAX
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4002-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-TSRYCD (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-OLD-SRYCD
           MOVE    SPA-GMN-TNAME  (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-OLD-NAME
           MOVE    SPA-NAI-TNAME  (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-OLD-NAME
           MOVE    SPA-NAI-TYKZKBN(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-OLD-YKZKBN
      *    入力コード編集
           INITIALIZE                      SPA-GMN-INPUTCD-G
           MOVE    SPACE               TO  SPA-GMN-SRYCD-G
           MOVE    SPACE               TO  SPA-GMN-NAME
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL   (FLG-INPUTCD    =   1    )  OR
                       (IDX            >=  10   )
               ADD     1                   TO  IDX
               MOVE    ICD-INPUTCD         TO  SPA-GMN-INPUTCD (IDX)
               MOVE    ICD-CDSYU           TO  SPA-GMN-CDSYU   (IDX)
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
      *
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    経過措置コード判定
           PERFORM 40021-SRYCDCHG-CHK-SEC
      *
           .
       4002-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   経過措置コード判定処理
      *****************************************************************
       40021-SRYCDCHG-CHK-SEC                    SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHG-IPNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
      *        点数マスタ編集
               MOVE    CHG-RJNSRYCD        TO   SPA-GMN-SRYCD
               PERFORM 40041-TENSU-CHK-SEC
               MOVE    4                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SRYCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       40021-SRYCDCHG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   行選択処理
      *****************************************************************
       4003-TENSU-LIST-SEC                    SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-TENSU-MAX
              IF      Z07-TENSU-SELECT (IDX)  =  "T"
                  IF      IDX                 =   SPA-GMN-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX             TO  SPA-GMN-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    SPA-GMN-SELNUM      TO  Z07-SELNUM
               PERFORM 4002-SELNUM-CHK-SEC
           END-IF
      *
           .
       4003-TENSU-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コードチェック処理
      *****************************************************************
       4004-SRYCD-CHK-SEC           SECTION.
      *
           MOVE    Z07-SRYCD           TO  SPA-GMN-SRYCD-G
      *
           MOVE    ZERO                TO  FLG-KENSAKU
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    ZERO                TO  FLG-KANJI
      *    コード内容チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYCD-G     TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-SRYCD-G
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO    4004-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (SPA-GMN-SRYCD       NUMERIC  )
               PERFORM 40041-TENSU-CHK-SEC
               GO      TO    4004-SRYCD-CHK-EXT
           END-IF
      *
      *    検索画面へ
           MOVE    1                   TO  FLG-KENSAKU
           MOVE    SPA-GMN-SRYCD-G     TO  WRK-INPUTCD
           PERFORM 4091-Z98-KENSAKU-SEC
      *
           .
       4004-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       40041-TENSU-CHK-SEC                 SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-TENSU           =   1
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO      TO      40041-TENSU-CHK-EXT
           END-IF
      *
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
      *
      *    入力コードチェック
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO      TO      40041-TENSU-CHK-EXT
           END-IF
           MOVE    4                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-NAI-CURIDX
      *
           .
       40041-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       4000-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       4000-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    関連処理
           PERFORM 4103-KANREI-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    Z07-INPUTCD01       TO  SPA-GMN-INPUTCD (01)
           MOVE    Z07-INPUTCD02       TO  SPA-GMN-INPUTCD (02)
           MOVE    Z07-INPUTCD03       TO  SPA-GMN-INPUTCD (03)
           MOVE    Z07-INPUTCD04       TO  SPA-GMN-INPUTCD (04)
           MOVE    Z07-INPUTCD05       TO  SPA-GMN-INPUTCD (05)
           MOVE    Z07-INPUTCD06       TO  SPA-GMN-INPUTCD (06)
           MOVE    Z07-INPUTCD07       TO  SPA-GMN-INPUTCD (07)
           MOVE    Z07-INPUTCD08       TO  SPA-GMN-INPUTCD (08)
           MOVE    Z07-INPUTCD09       TO  SPA-GMN-INPUTCD (09)
           MOVE    Z07-INPUTCD10       TO  SPA-GMN-INPUTCD (10)
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    同一連番がある時、１つとする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   10
                       IF     (IDX         NOT =   IDY )  AND
                              (SPA-GMN-INPUTCD (IDY)
                                           NOT =   SPACE )  AND
                              (SPA-GMN-INPUTCD (IDX)
                                               =   SPA-GMN-INPUTCD(IDY))
                           MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    空白ずめ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=  10
               IF      SPA-GMN-INPUTCD (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD-OCC (IDX + 1 )
                                               TO  SPA-GMN-INPUTCD-OCC
                                                                 (IDX)
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD-OCC
                                                            (IDX + 1)
               END-IF
           END-PERFORM
      *
      *    他の点数マスタに存在しないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-NAI-CURIDX
                   PERFORM 41021-INPUTCD-CHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    存在チェック処理
      *****************************************************************
       41021-INPUTCD-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    ZERO                TO  FLG-KANJI
      *    コード内容チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-INPUTCD (IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-INPUTCD (IDX)
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   IF      KANACHK-MAX         >   10
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        半角
               IF      KANACHK-SYUBETU     =   1
                   IF      SPA-GMN-INPUTCD(IDX)(1:KANACHK-MAX)
                                                NUMERIC
      *                数字桁数チェック
                       IF      KANACHK-MAX         <   4   OR  >  6
                           MOVE    "0009"              TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                S,Pチェック
                       IF      SPA-GMN-INPUTCD(IDX)(1:1)
                                                   =   "S"   OR  "P"
                           MOVE    "0010"          TO  SPA-ERRCD
                       END-IF
      *                #,$チェック
                       IF      SPA-GMN-INPUTCD(IDX)(1:1)
                                                   =   "#"   OR  "$"
                           MOVE    "0011"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    41021-INPUTCD-CHK-EXT
           END-IF
      *    記号は入力不可とする
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  IDX-C
                   UNTIL      (IDY         >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX-C               =   1
                   IF       SPA-GMN-INPUTCD(IDX)(IDY:1)
                                               =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                       MOVE    "0011"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF     SPA-GMN-INPUTCD(IDX) (IDY:2)
                                               =   "＋"   OR  "−"
                                               OR  "／"   OR  "！"
                                               OR  "．"   OR  "＊"
                       MOVE    "0011"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    41021-INPUTCD-CHK-EXT
           END-IF
      *
      *    セットコード存在チェック
           PERFORM 410112-DB-INPUTCD-CHK-SEC
      *
           .
       41021-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード存在チェック
      *****************************************************************
       410112-DB-INPUTCD-CHK-SEC        SECTION.
      *
      *    入力コード使用チェック
           INITIALIZE                  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-INPUTCD(IDX)
                                       TO  ICD-INPUTCD
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  ICD-CDSYU
           ELSE
      *        全桁数字は、コード種別を桁数で決定
               IF      SPA-GMN-INPUTCD(IDX)(1:KANACHK-MAX)
                                           NUMERIC
                   EVALUATE    KANACHK-MAX
                       WHEN    4
                           MOVE    "4"                 TO  ICD-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  ICD-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  ICD-CDSYU
                       WHEN    OTHER
                           MOVE    "A"                 TO  ICD-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  ICD-CDSYU
               END-IF
           END-IF
      *
           MOVE    ICD-CDSYU           TO  SPA-GMN-CDSYU (IDX)
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (SPA-GMN-INPUTCD(IDX)
                                       =   ICD-INPUTCD )
               IF      ICD-SRYCD       NOT =   SPA-GMN-OLD-SRYCD
                   MOVE    "0013"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410112-DB-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連処理
      *****************************************************************
       4103-KANREI-SEC              SECTION.
      *
      *    表示キーチェック
           IF     (SPA-GMN-INPUTCD(1)   NOT =   SPACE )  AND
                  (SPA-GMN-INPUTCD(1)(3:)   =   SPACE )  AND
                  (SPA-GMN-CDSYU  (1)       =   "J"   )
      *        表示のキーが全角１文字数字はエラー
               IF      SPA-GMN-INPUTCD(1)(1:2)     >=  "０"  AND
                                                   <=  "９"
                   MOVE    "0012"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
                   MOVE    01              TO  SPA-NAI-CURIDX
               END-IF
           END-IF
      *
           .
       4103-KANREI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "Z01"               TO  SPA-SAKIPG
           MOVE    "Z07"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC                   SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
      *        初期画面へ
               PERFORM 310-SPASET-SEC
           ELSE
               INITIALIZE                      SPA-GMN-IN-AREA
               INITIALIZE                      SPA-GMN-INPUTCD-G
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ検索処理
      *****************************************************************
       407-NEXT-PAGE-SEC          SECTION.
      *
           IF      SPA-GMN-NEXT        =   ZERO
               GO      TO      407-NEXT-PAGE-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           .
       407-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面へ処理
      *****************************************************************
       409-KENSAKU-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCD
      *
           IF     (SPA-GMN-OLD-SRYCD   NOT =   SPACE )  AND
                  (SPA-GMN-SRYCD           =   SPACE )
               MOVE    SPA-NAI-OLD-NAME(1:18)  TO  WRK-INPUTCD
               MOVE    1                       TO  FLG-KANJI
           END-IF
           PERFORM 4091-Z98-KENSAKU-SEC
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面へ処理
      *****************************************************************
       4091-Z98-KENSAKU-SEC          SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           INITIALIZE                  SPA-Z98GMN-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           EVALUATE    SPA-GMN-OLD-SRYCD(1:1)
               WHEN    "1"
      *        診療行為
                   MOVE    "8"             TO  SPA-Z98-GMN-SYORI
               WHEN    "6"
      *        薬剤
                   EVALUATE    SPA-NAI-OLD-YKZKBN
                       WHEN    "4"
      *                    注射
                           MOVE    "5"         TO  SPA-Z98-GMN-SYORI
                       WHEN    "6"
      *                    外用
                           MOVE    "4"         TO  SPA-Z98-GMN-SYORI
                       WHEN    OTHER
      *                    内服
                           MOVE    "3"         TO  SPA-Z98-GMN-SYORI
                   END-EVALUATE
               WHEN    "7"
      *            器材
                   MOVE    "7"             TO  SPA-Z98-GMN-SYORI
           END-EVALUATE
      *
           IF      WRK-INPUTCD         NOT =   SPACE
      *********MOVE    "3"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "2"                 TO  SPA-Z98-TYPE
      *        すべてを対象とする
               MOVE    "1"                 TO  SPA-Z98-GMN-ALL
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
                   MOVE    "1"                 TO  SPA-Z98-TYPE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-Z98-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-Z98-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-Z98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-Z98-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           ELSE
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           END-IF
      *
      *
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z07"               TO  SPA-Z98-MOTOPG
      *
           IF      SPA-Z98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4091-Z98-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-Z98-SET-SEC                           SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z07             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z07
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *    診療コード変更
           IF      SPA-GMN-SRYCD       =   SPACE
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
      *        点数マスタ存在チェック
               PERFORM 40041-TENSU-CHK-SEC
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
           PERFORM 4121-KOUSIN-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
      *    再表示へ
           PERFORM 3101-TENSU-HENSYU-SEC
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                      SPA-GMN-IN-AREA
           INITIALIZE                      SPA-GMN-INPUTCD-G
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード登録処理
      *****************************************************************
       4121-KOUSIN-SYORI-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        入力コード検索
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-GMN-INPUTCD (IDX)
                                           TO  ICD-INPUTCD
               MOVE    SPA-GMN-CDSYU   (IDX)
                                           TO  ICD-CDSYU
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-INPUTCD-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        入力コード更新
               IF      (FLG-INPUTCD        =   ZERO )  AND
                       (SPA-GMN-OLD-SRYCD  =   ICD-SRYCD )
      *            診療コード変更
                   MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
                   MOVE    IDX                 TO  ICD-DSPSEQ
      *
                   MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
                   MOVE    INPUTCD-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0100"          TO  SPA-ERRCD
                       DISPLAY "Z07 INPUTCD UPD ERR:" MCP-RC
                               ",KEY:" ICD-KEY
                   END-IF
               ELSE
                   IF      FLG-INPUTCD     =   ZERO
                       MOVE    "0101"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        入力コード追加
               IF       FLG-INPUTCD        =   1
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
                   MOVE    SPA-GMN-INPUTCD (IDX)
                                               TO  ICD-INPUTCD
                   MOVE    SPA-GMN-CDSYU   (IDX)
                                               TO  ICD-CDSYU
                   MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
                   MOVE    IDX                 TO  ICD-DSPSEQ
      *
                   MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
                   MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
                   MOVE    INPUTCD-REC         TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0100"          TO  SPA-ERRCD
                       DISPLAY "Z07 INPUTCD INS ERR:" MCP-RC
                               ",KEY:" ICD-KEY
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    前の診療コードの連番付け直し
           PERFORM 41211-OLD-INPUTCD-SEC
      *
           .
       4121-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前の診療コードの連番付け直し処理
      *****************************************************************
       41211-OLD-INPUTCD-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL    FLG-INPUTCD    =   1
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  ICD-DSPSEQ
      *
               MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
               MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "0100"          TO  SPA-ERRCD
                   DISPLAY "Z07 INPUTCD UPD ERR:" MCP-RC
                           ",KEY:" ICD-KEY
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
      *
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41211-OLD-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z07    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  Z07
           INITIALIZE                      Z07
      *
           MOVE    SPA-GMN-YUKOYMD     TO  Z07-YUKOYMD
           MOVE    SPA-GMN-SELNUM      TO  Z07-SELNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  Z07-OLDSRYCD
           MOVE    SPA-GMN-OLD-NAME    TO  Z07-OLDNAME
           MOVE    SPA-GMN-SRYCD       TO  Z07-SRYCD
           MOVE    SPA-GMN-NAME        TO  Z07-NAME
      *
           MOVE    SPA-GMN-INPUTCD(01) TO  Z07-INPUTCD01
           MOVE    SPA-GMN-INPUTCD(02) TO  Z07-INPUTCD02
           MOVE    SPA-GMN-INPUTCD(03) TO  Z07-INPUTCD03
           MOVE    SPA-GMN-INPUTCD(04) TO  Z07-INPUTCD04
           MOVE    SPA-GMN-INPUTCD(05) TO  Z07-INPUTCD05
           MOVE    SPA-GMN-INPUTCD(06) TO  Z07-INPUTCD06
           MOVE    SPA-GMN-INPUTCD(07) TO  Z07-INPUTCD07
           MOVE    SPA-GMN-INPUTCD(08) TO  Z07-INPUTCD08
           MOVE    SPA-GMN-INPUTCD(09) TO  Z07-INPUTCD09
           MOVE    SPA-GMN-INPUTCD(10) TO  Z07-INPUTCD10
      *
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-TENSU-MAX
               MOVE    SPA-GMN-TNUM (IDX)      TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  Z07-TNUM  (IDX)
               MOVE    SPA-GMN-TINPUTCD(IDX)   TO  Z07-TINPUTCD (IDX)
               MOVE    SPA-GMN-TSRYCD  (IDX)   TO  Z07-TSRYCD  (IDX)
               MOVE    SPA-GMN-TNAME   (IDX)   TO  Z07-TNAME  (IDX)
               MOVE    SPA-GMN-TYUKOSTYMD (IDX)
                                               TO  Z07-TYUKOSTYMD(IDX)
               MOVE    SPA-GMN-TYUKOEDYMD (IDX)
                                               TO  Z07-TYUKOEDYMD(IDX)
               IF      SPA-GMN-SELNUM          =   IDX
                   MOVE    "T"             TO  Z07-TENSU-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  Z07-TENSU-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-TENSU-MAX       TO  Z07-TENSU-COUNT
      *
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               MOVE    "次ページ有"        TO  Z07-NEXT-MSG-VALUE
               MOVE    "red"               TO  Z07-NEXT-MSG-STYLE
           ELSE
               MOVE    SPACE               TO  Z07-NEXT-MSG-VALUE
               MOVE    SPACE               TO  Z07-NEXT-MSG-STYLE
           END-IF
      *
      *    カーソルセット
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "YUKOYMD"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYCD"         TO  MCP-WIDGET
               WHEN    04
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    SPA-NAI-CURIDX  TO  MCP-WIDGET(8:2)
               WHEN    12
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       50011-CUR-SET-SEC              SECTION. 
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOYMD"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SRYCD"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    OTHER
                   IF      WRK-MCP-WIDGET(1:7)    =   "INPUTCD"
                       MOVE    WRK-MCP-WIDGET(8:2)    TO  SPA-NAI-CURIDX
                       IF      SPA-NAI-CURIDX         =   10
                           MOVE    12                 TO  SPA-GMN-CUR
                       ELSE
                           ADD     1                  TO  SPA-NAI-CURIDX
                           MOVE    4                  TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    1                      TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効日付入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "対象のデータがありません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象が２００件以上あります"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "全角・半角の混在はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "点数マスタが存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "既に自院コードが登録されています。"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "漢字コードは５文字までです"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "入力コードは４桁から６桁で入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "Ｐ，Ｓは使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "入力コードには使用できない記号があります。"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE
                   "表示入力コードに全角１文字の数字は使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE
                   "この入力コードは既に別の診療コードで使われています"
                                                 TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "別の診療コードで使われています。"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "新診療行為コードを入力して下さい"
                                                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "ZERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       910-TENSU-NEXT-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
      *********INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       920-INPUTCD-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
