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
       PROGRAM-ID.                 ORCGM97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : プログラム更新（Ｍ９７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/16    NACL-森脇     新規作成
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
       WORKING-STORAGE             SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
           COPY    "ORCA-DBPATH".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPMKPASS.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-M97.
           03  SPA-M97-AREA.
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-ONKBN   PIC X(01).
                   07  SPA-GMN-OFFKBN  PIC X(01).
      *
                   07  SPA-GMN-OFFNAIYO.
                       09  SPA-GMN-CD         PIC X(01).
                       09  SPA-GMN-F1         PIC X(01).
                       09  SPA-GMN-MEI        PIC X(12).
      *
                   07  SPA-GMN-NAIYOLIST      OCCURS  99.
                       09  SPA-GMN-TEIKYOYMD  PIC X(09).
                       09  SPA-GMN-SYORIFLG   PIC X(08).
                       09  SPA-GMN-NAIYO      PIC X(100).
                   07  SPA-GMN-NAIYOLIST-MAX  PIC 9(02).
      *
                   07  SPA-GMN-MESSEGE.
                       09  SPA-GMN-MSG-STYLE  PIC X(20).
                       09  SPA-GMN-MSG        PIC X(122).
      *
               05  SPA-GMN-OFFNAIYO-AREA.
                   07  SPA-GMN-OFFLIST        OCCURS  2.
                       09  SPA-GMN-CDL        PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-MEIL       PIC X(12).
                   07  SPA-GMN-OFFLIST-MAX    PIC 9(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PGKANRI         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PGKANRI         PIC 9(02).
      *
      *    処理区分領域
       01  SKBN-AREA.
           03  SKBN-RUN            PIC X(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH          PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-MIDMSG          PIC X(80).
      *
       01  SHELL-AREA.
           03  SHELL-NAME1         PIC X(41)  VALUE
               "scripts/allways/program_upgrade_online.sh".
           03  SHELL-NAME2         PIC X(42)  VALUE
               "scripts/allways/program_upgrade_offline.sh".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    ＰＧ管理
       01  PGKANRI-REC.
           COPY    "CPPGKANRI.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
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
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-M97
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M97         TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG      =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD   NOT     =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "M97"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
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
                   PERFORM 210-BACK
      *        情報削除
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 410-JOBDEL-SEC
      *        情報
      *         WHEN    "CLICKED"   ALSO    "B09"
      *             PERFORM 400-JYOUHOU-SEC
      *        状況
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 400-JYOKYO-SET-SEC
      *        更新
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 410-KAKUNIN-SEC
      *        オンライン・オフライン
               WHEN    "CLICKED"   ALSO    "ONKBN"
                   MOVE    M97-ONKBN       TO  SPA-GMN-ONKBN
      *             MOVE    2               TO  SPA-GMN-CUR
      *         WHEN    "CLICKED"   ALSO    "OFFKBN"
      *             MOVE    M97-OFFKBN-NAIYO    TO  SPA-GMN-OFFKBN
      *             IF      SPA-GMN-OFFKBN      =   "T"
      *                 MOVE  3                 TO  SPA-GMN-CUR
      *             ELSE
                       MOVE  99                TO  SPA-GMN-CUR
      *             END-IF
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "OFFNAIYO"
                   PERFORM  400-OFFNAIYO-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
           IF      SKBN-RUN   =   "RUN"
               MOVE    "0301"          TO  SPA-MIDCD
               PERFORM 520-MIDSET-SEC
           ELSE
               PERFORM 830-GAMENN-SET6-SEC
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
      *    確認画面ＯＫ処理
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 320-MID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
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
           INITIALIZE              SPA-M97-AREA
      *
           MOVE    "T"             TO  SPA-GMN-ONKBN
           MOVE    "F"             TO  SPA-GMN-OFFKBN
      *
           MOVE    "1"             TO  SPA-GMN-CDL(1)
           MOVE    "フロッピー"    TO  SPA-GMN-MEIL(1)
           MOVE    "2"             TO  SPA-GMN-CDL(2)
           MOVE    "ＣＤ−ＲＯＭ"  TO  SPA-GMN-MEIL(2)
           MOVE    2               TO  SPA-GMN-OFFLIST-MAX
      *
           MOVE    ZERO            TO  SPA-GMN-NAIYOLIST-MAX
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           PERFORM 400-JYOKYO-SET-SEC
      *
           IF      SPA-GMN-NAIYOLIST-MAX   =  ZERO
               MOVE    SPACE           TO  SPA-GMN-MSG
               MOVE    SPACE           TO  SPA-GMN-MSG-STYLE
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
     *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "0202"
                   IF      SPA-MID1-FLG    =   "OK"
      *            更新処理
                       PERFORM 420-KOUSHIN-SEC
                   END-IF
               WHEN    "0204"
                   IF      SPA-MID1-FLG    =   "OK"
      *                削除処理
                       PERFORM 820-JOBKANRI-DEL-SEC
                   END-IF
               WHEN    "0301"
                   IF      SPA-MID1-FLG    =   "OK"
      *                戻る処理
                       PERFORM 830-GAMENN-SET6-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
           .
      *
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報入手
      *****************************************************************
       400-JYOUHOU-SEC             SECTION.
      *
      *    プログラム更新処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
      *    オンライン
           IF      SPA-GMN-ONKBN   =   "T"
               MOVE    SHELL-NAME1     TO  MKPASS-IN-01
               MOVE    "1"             TO  SHELLTBL-ARG1
           END-IF
      *    オフライン
      *     IF      SPA-GMN-OFFKBN  =   "T"
      *         MOVE    SHELL-NAME2     TO  MKPASS-IN-01
      *         EVALUATE    SPA-GMN-CD
      *             WHEN    "1"
      *                 MOVE    "1"             TO  SHELLTBL-ARG1
      *                 MOVE    "1"             TO  SHELLTBL-ARG2
      *             WHEN    "2"
      *                 MOVE    "1"             TO  SHELLTBL-ARG1
      *                 MOVE    "2"             TO  SHELLTBL-ARG2
      *             WHEN    OTHER
      *                 GO  TO  400-JYOUHOU-SEC-EXT
      *         END-EVALUATE
      *     END-IF
      *
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SHELLTBL        TO  MCPDATA-REC
      *
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
      *
           .
       400-JYOUHOU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況更新
      *****************************************************************
       400-JYOKYO-SET-SEC          SECTION.
      *
           MOVE    ZERO            TO  CNT-PGKANRI
      *
           PERFORM 900-PGKANRI-SELECT-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   FLG-PGKANRI =   1
                   OR      IDX         >   99
      *    日付変換
               MOVE    PGKANRI-RELEASEYMD  TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEIKYOYMD(IDX)
      *    更新フラグ
               IF      PGKANRI-UPDATEFLG   =   "T"
                   MOVE    "　　済　"      TO  SPA-GMN-SYORIFLG(IDX)
               ELSE
                   MOVE    "　　　　"      TO  SPA-GMN-SYORIFLG(IDX)
               END-IF
      *    内容
               MOVE    PGKANRI-COMMENT     TO  SPA-GMN-NAIYO(IDX)
      *
               COMPUTE CNT-PGKANRI     =   CNT-PGKANRI +   1
               PERFORM 900-PGKANRI-FETCH-SEC
           END-PERFORM
      *
           MOVE    CNT-PGKANRI         TO  SPA-GMN-NAIYOLIST-MAX
      *
           PERFORM 900-PGKANRI-CLOSE-SEC
      *
           IF      SPA-GMN-NAIYOLIST-MAX   =  ZERO
               MOVE    SPACE         TO  SKBN-RUN
               MOVE    "更新データはありません。"
                                     TO  SPA-GMN-MSG
               MOVE    "blue"        TO  SPA-GMN-MSG-STYLE
               GO  TO  400-JYOKYO-SET-EXT
           END-IF
      *
      *    メッセージ編集
           PERFORM 820-JOBKANRI-READ1-SEC
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    SPACE           TO  SKBN-RUN
                   MOVE    "プログラム更新は異常終了しました。"
                                           TO  SPA-GMN-MSG
                   MOVE    "red"           TO  SPA-GMN-MSG-STYLE
               ELSE
                   IF      JOB-ERRCD(1:2)    =   "01"
                       MOVE    SPACE         TO  SKBN-RUN
                       MOVE    "更新データはありませんでした。"
                                             TO  SPA-GMN-MSG
                       MOVE    "blue"        TO  SPA-GMN-MSG-STYLE
                   ELSE
                       IF  (JOB-ENDTIME       NOT =   SPACE)  AND
                           (JOB-ENDYMD        NOT =   SPACE)
                           MOVE    SPACE        TO  SKBN-RUN
                       MOVE    "プログラム更新は正常に終了しました。"
                                                TO  SPA-GMN-MSG
                           MOVE    "blue"       TO  SPA-GMN-MSG-STYLE
                       ELSE
                           MOVE    "プログラム更新中です。"
                                                TO  SPA-GMN-MSG
                           MOVE    "blue"       TO  SPA-GMN-MSG-STYLE
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-MSG
               MOVE    SPACE           TO  SPA-GMN-MSG-STYLE
           END-IF
           .
       400-JYOKYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフライン処理コンボ
      *****************************************************************
       400-OFFNAIYO-CHK-SEC        SECTION.
      *
      *    変化なし
           IF     (M97-OFF-NAIYO   NOT =   SPACE) AND
                  (M97-OFF-NAIYO       =   SPA-GMN-OFFNAIYO)
      *
               IF      SPA-GMN-OFFLIST-MAX    >  ZERO
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
               GO  TO  400-OFFNAIYO-CHK-EXT
           END-IF
      *
           MOVE    M97-OFF-NAIYO   TO  SPA-GMN-OFFNAIYO
      *
           IF      SPA-GMN-CD      =   "1"     OR    "2"
               MOVE    99              TO  SPA-GMN-CUR
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           END-IF
           .
      *
       400-OFFNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       削除処理
      *****************************************************************
       410-JOBDEL-SEC              SECTION.
      *
           PERFORM 820-JOBKANRI-READ1-SEC
      *
           IF  SJOBKANRI-RETURN    =   ZERO
               MOVE    "0204"          TO  SPA-MIDCD
               PERFORM 520-MIDSET-SEC
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           END-IF
           .
      *
       410-JOBDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       410-KAKUNIN-SEC             SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-KAKUNIN-EXT
           END-IF
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "0203"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           ELSE
               MOVE    "0202"          TO  SPA-MIDCD
               PERFORM 520-MIDSET-SEC
           END-IF
           .
      *
       410-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       420-KOUSHIN-SEC             SECTION.
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
      *    プログラム更新処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
      *    オンライン
           DISPLAY "SPA-GMN-ONKBN=" SPA-GMN-ONKBN
           IF      SPA-GMN-ONKBN       =   "T"
               MOVE    SHELL-NAME1     TO  MKPASS-IN-01
               MOVE    "2"             TO  SHELLTBL-ARG1
           END-IF
      *
      *    オフライン
      *     IF      SPA-GMN-OFFKBN      =   "T"
      *         MOVE    SHELL-NAME2     TO  MKPASS-IN-01
      *         EVALUATE    SPA-GMN-CD
      *             WHEN    "1"
      *                 MOVE    "2"         TO  SHELLTBL-ARG1
      *                 MOVE    "1"         TO  SHELLTBL-ARG2
      *             WHEN    "2"
      *                 MOVE    "2"         TO  SHELLTBL-ARG1
      *                 MOVE    "2"         TO  SHELLTBL-ARG2
      *             WHEN    OTHER
      *                 GO  TO  420-KOUSHIN-EXT
      *         END-EVALUATE
      *     END-IF
      *
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SHELLTBL        TO  MCPDATA-REC
      *
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
           CALL    "MCPSUB"            USING
                                       MCPAREA
                                       MCPDATA-REC
           DISPLAY "420-KOUSHIN-END"
           .
      *
       420-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD        =   ALL "9"  OR   SPACE
               MOVE    SPACE           TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
      *
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 520-MIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           PERFORM 500-GMNHEN-SEC
           MOVE    SPACE           TO  LINKAREA
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "M97"           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
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
           INITIALIZE                  M97
      *
      *    オンライン・オフライン
           MOVE    SPA-GMN-ONKBN   TO  M97-ONKBN
           MOVE    SPA-GMN-OFFKBN  TO  M97-OFFKBN-NAIYO
      *
      *    オフラインコンボ
      *     IF      SPA-GMN-OFFKBN  =   "T"
      *         MOVE    SPA-GMN-OFFNAIYO    TO  M97-OFF-NAIYO
      *         PERFORM VARYING IDX FROM    1       BY  1
      *                 UNTIL   IDX >       SPA-GMN-OFFLIST-MAX
      *             MOVE    SPA-GMN-OFFLIST(IDX)
      *                                     TO  M97-OFF-LIST(IDX)
      *         END-PERFORM
      *         MOVE    SPA-GMN-OFFLIST-MAX TO  M97-OFF-COUNT
      *         MOVE    WIDGET-NORMAL       TO  M97-OFF-STATE
      *     ELSE
               MOVE    WIDGET-INSENSITIVE  TO  M97-OFF-STATE
               MOVE    WIDGET-INSENSITIVE  TO  M97-OFFKBN-STATE
      *     END-IF
      *
      *    コラムリスト
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX >       SPA-GMN-NAIYOLIST-MAX
               MOVE    SPA-GMN-TEIKYOYMD(IDX)
                                       TO  M97-NAIYO-0(IDX)
               MOVE    SPA-GMN-SYORIFLG(IDX)
                                       TO  M97-NAIYO-1(IDX)
               MOVE    SPA-GMN-NAIYO(IDX)
                                       TO  M97-NAIYO-2(IDX)
           END-PERFORM
           MOVE    SPA-GMN-NAIYOLIST-MAX   TO  M97-NAIYO-COUNT
      *
      *    メッセージ
           MOVE    SPA-GMN-MSG         TO  M97-MSG-NAIYO
           MOVE    SPA-GMN-MSG-STYLE   TO  M97-MSG-STYLE
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "ONKBN"           TO  MCP-WIDGET
               WHEN    2
                   MOVE  "OFFKBN"          TO  MCP-WIDGET
               WHEN    3
                   MOVE  "OFFNAIYO"        TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET
           END-EVALUATE
           .
      *
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
                   MOVE    "入力エラー"
                                            TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "更新処理が実行中の為、削除できません"
                                            TO  SPA-ERRMSG
               WHEN    "0005"
                    MOVE    "ＪＯＢ管理情報は、ありません"
                                            TO  SPA-ERRMSG
               WHEN    "0203"
                   MOVE    "プログラム更新は実行中です。"
                                            TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE            TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
      *
           PERFORM 830-GAMENN-SET4-SEC
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
           EVALUATE    SPA-MIDCD
               WHEN    "0202"
               MOVE    "プログラム更新を実行します。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    "0204"
                   MOVE    "ジョブ管理を削除します。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    "0301"
                   MOVE    "プログラム更新中です。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    OTHER
                   MOVE    SPA-MIDCD       TO  WRK-MIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MID1
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
           MOVE    WRK-MIDMSG          TO  MID1-ID1MSG
      *
           PERFORM 830-GAMENN-SET5-SEC
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ1-SEC      SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
           .
      *
       820-JOBKANRI-READ1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ2-SEC      SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    SPACE           TO  SKBN-RUN
               ELSE
                   IF      JOB-ERRCD(1:2)      =   "01"
                       MOVE    SPACE           TO  SKBN-RUN
                   ELSE
                       IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                              (JOB-ENDYMD      NOT =   SPACE)
                           MOVE    SPACE           TO  SKBN-RUN
                       ELSE
                           MOVE    "RUN"           TO  SKBN-RUN
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE            TO  SKBN-RUN
           END-IF
           .
      *
       820-JOBKANRI-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除
      *****************************************************************
       820-JOBKANRI-DEL-SEC        SECTION.
      *
      *     PERFORM 820-JOBKANRI-READ1-SEC
      *
      *     IF      SJOBKANRI-RETURN    =   ZERO
      *         IF     (JOB-ENDTIME     NOT =   SPACE)  AND
      *                (JOB-ENDYMD      NOT =   SPACE)
      *            ジョブ管理削除
                   MOVE    "DEL"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-JOBID
                   MOVE    "PRGMNT"        TO  JOB-SHELLID
                   CALL    "ORCSJOB"           USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
      *         ELSE
      *             MOVE    "0004"          TO  SPA-ERRCD
      *         END-IF
      *     END-IF
           .
      *
       820-JOBKANRI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成
      *****************************************************************
       820-JOBKANRI-JBS-SEC        SECTION.
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
           .
      *
       820-JOBKANRI-JBS-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ERR
      *****************************************************************
       830-GAMENN-SET4-SEC         SECTION.
      *
           MOVE    "B01"           TO  MCP-WIDGET
           MOVE    "M97"           TO  SPA-MOTOPG
           MOVE    "MERR"          TO  SPA-SAKIPG
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面警告
      *****************************************************************
       830-GAMENN-SET5-SEC         SECTION.
      *
           MOVE    "B12"           TO  MCP-WIDGET
           MOVE    "M97"           TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET5-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る
      *****************************************************************
       830-GAMENN-SET6-SEC         SECTION.
      *
           MOVE    "M00"           TO  SPA-SAKIPG
           MOVE    "M97"           TO  SPA-MOTOPG
           MOVE    "M00"           TO  MCP-WINDOW
           MOVE    "CHANGE"        TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET6-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＧ管理マスタ読み込み（キー）
      *****************************************************************
       900-PGKANRI-SELECT-SEC      SECTION.
      *
           INITIALIZE                  PGKANRI-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "PGKANRI-ALL"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO        TO  FLG-PGKANRI
               PERFORM 900-PGKANRI-FETCH-SEC
           ELSE
               MOVE    1           TO  FLG-PGKANRI
           END-IF
            .
      *
       900-PGKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＧ管理マスタ読み込み（キー）
      *****************************************************************
       900-PGKANRI-FETCH-SEC       SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "PGKANRI-ALL"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-PGKANRI
               MOVE    MCPDATA-REC     TO  PGKANRI-REC
           ELSE
               MOVE    1           TO  FLG-PGKANRI
           END-IF
           .
      *
       900-PGKANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＧ管理マスタ読み込み（キー）
      *****************************************************************
       900-PGKANRI-CLOSE-SEC       SECTION.
      *
           MOVE    "DBCLOSE"       TO  MCP-FUNC
           MOVE    "PGKANRI-ALL"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
      *
       900-PGKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
           CALL   "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
