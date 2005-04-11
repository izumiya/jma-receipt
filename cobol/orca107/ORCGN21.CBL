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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGN21.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 薬剤情報（Ｎ２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/25    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 00.01.01     NACL-森脇     02/08/28  検索開始文字数の変更
      *                                      検索結果表示の変更
      * 00.01.02     NACL-森脇     02/08/29  初期表示追加
      * 00.01.03     NACL-森脇     02/10/11  ーボタン追加
      * 00.01.04     NACL-森脇     02/10/11  登録時のクリア処理変更
      * 00.01.05     NACL-森脇     02/11/07  記号の半角全角混在チェック
      * 01.01.01     NACL-森脇     02/12/09  検索文字変更
      * 01.01.02     NACL-森脇     02/12/12  検索文字引数クリア
      * 01.01.03     NACL-森脇     03/01/08  画面入力文字数変更
      * 01.01.04     NACL-森脇     03/05/30  初期値変更
      * 02.01.01     NACL-森脇     03/06/13  一括更新
      * 02.01.02     NACL-森脇     04/10/05  半角全角チェック追加
      * 02.01.03     NACL-森脇     04/10/22  CLOSE処理
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                DIVISION.
      *
       WORKING-STORAGE         SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N21COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-N21.
           03  SPA-N21-AREA.
             05  SPA-GMN-PAGE  PIC 9(02).
             05  SPA-GMN-LINE  PIC 9(02).
             05  SPA-GMN-MAX   PIC 9(05).
             05  SPA-GMN-CUR   PIC 9(02).
      *
             05  SPA-GMN-AREA.
               07  SPA-GMN-KENCD       PIC X(21).
               07  SPA-GMN-KENNAIYO    PIC X(27).
               07  SPA-GMN-KENNAME     PIC X(101).
               07  SPA-GMN-SELNUM      PIC 9(03).
      *
               07  SPA-GMN-TBL.
                 09  SPA-GMN-TBL-G     OCCURS  200.
                   11  SPA-GMN-TNO     PIC X(05).
                   11  SPA-GMN-TNAME   PIC X(100).
                 09  SPA-GMN-TCD       PIC X(09)   OCCURS  200.
      *
      *----(01.01.03)--UPD-START---
               07  SPA-GMN-ENTRY.
                 09  SPA-GMN-NAME      PIC X(80).
                 09  SPA-GMN-EFFECT    PIC X(186).
                 09  SPA-GMN-COLOR     PIC X(136).
                 09  SPA-GMN-SHAPE     PIC X(136).
                 09  SPA-GMN-MARK      PIC X(136).
                 09  SPA-GMN-CAUTION   PIC X(410).
                 09  SPA-GMN-PHOTO     PIC X(256).
      *----(01.01.03)--UPD-END-----
      *
             05  SPA-NAI-AREA.
               07  SPA-NAI-SRYCD       PIC X(09).
      *
       01  FLG-AREA.
           03  FLG-END         PIC 9(01).
           03  FLG-TENSU       PIC 9(01).
           03  FLG-YAKUJYO     PIC 9(01).
           03  FLG-INPUTCD     PIC 9(01).
           03  FLG-OK          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MOJI    PIC 9(06).
           03  CNT-DAT     PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX         PIC 9(03).
           03  IDY         PIC 9(03).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD     PIC X(08).
           03  SYS-TIME    PIC X(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD       PIC X(9).
           03  WRK-NAME        PIC X(26).
           03  WRK-CDSYU       PIC X(01).
           03  WRK-CREYMD      PIC X(08).
      *
           03  WRK-TNSPATH     PIC X(64).
           03  WRK-IPCPATH     PIC X(64).
      *
           03  WRK-GMN-CUR     PIC 9(02).
           03  WRK-N21IDMSG    PIC X(70).
           03  WRK-ZZZZ9       PIC ZZZZ9.
      *
      *    カナ検索用
       01  WRK-PIN-AREA.
      *----(01.01.01)--UPD-START---
      *     03  WRK-PIN         PIC X(02).
           03  WRK-PIN         PIC 9(02).
      *----(01.01.01)--UPD-END-----
           03  WRK-PED.
             05  WRK-PEDX      PIC X(02)   OCCURS  13.
           03  WRK-PKANA       PIC X(02).
           03  WRK-PMAX        PIC 9(02).
      *
      *----(00.01.03)--ADD-START----
      *    カナ検索用テーブル
       01  PIN-TBL-AREA.
           03  KANA-TBL.
             05  FILLER        PIC X(10)   VALUE "アイウエオ".
             05  FILLER        PIC X(10)   VALUE "カキクケコ".
             05  FILLER        PIC X(10)   VALUE "サシスセソ".
             05  FILLER        PIC X(10)   VALUE "タチツテト".
             05  FILLER        PIC X(10)   VALUE "ナニヌネノ".
             05  FILLER        PIC X(10)   VALUE "ハヒフヘホ".
             05  FILLER        PIC X(10)   VALUE "マミムメモ".
             05  FILLER        PIC X(06)   VALUE "ヤユヨ".
             05  FILLER        PIC X(10)   VALUE "ラリルレロ".
             05  FILLER        PIC X(10)   VALUE "ワヲン゜゛".
             05  FILLER        PIC X(08)   VALUE "ーＳＢＣ".
           03  KANA-TBL-R      REDEFINES   KANA-TBL.
             05  PKANA         PIC X(02)   OCCURS  52.
      *----(00.01.03)--ADD-END------
      *
      *----(00.01.02)--ADD-START----
       01  MSG-AREA.
           03  MSG-CAUTION.
             05  FILLER        PIC X(34)   VALUE
                          "医師の指示通りに服用してください。".
             05  FILLER        PIC X(01)   VALUE X"0A".
             05  FILLER        PIC X(40)   VALUE
                          "発疹・かゆみ等の過敏症が現れた時は服用を".
             05  FILLER        PIC X(30)   VALUE
                          "中止し、医師に相談して下さい。".
      *----(00.01.02)--ADD-END------
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    薬剤情報マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "N21.INC".
           COPY    "N21ERR.INC".
           COPY    "N21ID1.INC".
      *
       PROCEDURE           DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC            SECTION.
      *
           MOVE    SPA-COMMON  TO  SPA-AREA
           MOVE    SPA-WORK    TO  SPA-N21KYOUTU
           MOVE    SPA-FREE    TO  SPA-N21
      *
           MOVE    SPACE       TO  SPA-ERRCD
           MOVE    ZERO        TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF          FLG-END     =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-N21         TO  SPA-FREE
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           .
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC            SECTION.
      *
           INITIALIZE          FLG-AREA
           INITIALIZE          CNT-AREA
           INITIALIZE          IDX-AREA
           INITIALIZE          WRK-AREA
      *
      *    エラー画面より
           IF          SPA-MOTOPG  =   "N21ERR"
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 530-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF          FLG-END     =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF          SPA-ERRCD   NOT     =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *    ＰＵＴ処理
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "N21"               TO  MCP-WINDOW
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
       200-GMNSENI             SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM 450-BACK
      *    クリア
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 460-CLEAR-SEC
      *    削除
               WHEN    "CLICKED"   ALSO    "B03"
                   PERFORM 470-DEL-SEC
      *    登録
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 480-TOUROKU-SEC
      *    カーソル移動
               WHEN    "CLICKED"   ALSO    "B10"
                   PERFORM 440-CURMOVE-SEC
           END-EVALUATE
      *
      *    カナ入力
           IF          MCP-EVENT    =   "CLICKED"
               AND     MCP-WIDGET(1:3)  =   "KEN"
      *----(01.01.02)--ADD-START---
               MOVE    ZERO             TO  WRK-PIN
      *----(01.01.02)--ADD-END-----
      *----(01.01.01)--ADD-START---
               IF      MCP-WIDGET(5:)   =   SPACE
                   MOVE    MCP-WIDGET(4:1)  TO  WRK-PIN(2:1)
               ELSE
                   MOVE    MCP-WIDGET(4:2)  TO  WRK-PIN(1:2)
               END-IF
      *----(01.01.01)--ADD-END-----
               PERFORM 490-KEN-PHEN-SEC
           END-IF
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY               SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "KENCD"
                   PERFORM 4110-KENCD-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4111-SELNUM-SEC
               WHEN    ANY             ALSO    "KEKKALST"
                   PERFORM 4113-KEKKALST-SEC
               WHEN    OTHER
                   PERFORM 430-TEXTCHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC          SECTION.
      *
      *    他のＬＤより
           IF          LINKAREA    NOT     =   SPACE
               MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
           END-IF
      *
      *    確認画面ＯＫ処理
           EVALUATE    SPA-MOTOPG
               WHEN    "N21ID1"
                   PERFORM 320-N21ID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    SPACE       TO  SPA-N21ID1-FLG
           MOVE    SPACE       TO  SPA-N21IDCD
      *
      *    スパ初期編集処理
           PERFORM 310-SPASET-SEC
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC          SECTION.
      *
           INITIALIZE          SPA-N21-AREA
           INITIALIZE          WRK-PIN-AREA
      *
           MOVE    2           TO  SPA-GMN-CUR
           . 
      *
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（Ｎ２１ＩＤ１）ＯＫ処理
      *****************************************************************
       320-N21ID1-SET-SEC      SECTION.
      *
           IF      SPA-N21ID1-FLG    =   "OK"
               EVALUATE      SPA-N21IDCD
      *            削除処理
                   WHEN  "1002"
                       PERFORM 471-DEL-SEC
      *            更新処理
                   WHEN  "1001"
                       PERFORM 481-KOUSHIN-SEC
               END-EVALUATE
           ELSE
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    SPACE       TO  SPA-N21ID1-FLG
           MOVE    SPACE       TO  SPA-N21IDCD
           .
      *
       320-N21ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索コード処理
      *****************************************************************
       4110-KENCD-SEC          SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    N21-KENCD   TO  SPA-GMN-KENCD
      *
           IF          SPA-GMN-KENCD   =   SPACE
               MOVE    3           TO  SPA-GMN-CUR
               GO  TO  4110-KENCD-EXT
           END-IF
      *
      *    入力コードマスタ判定
           PERFORM 4120-INPUTCD-CDSYU-HAN-SEC
      *
           IF          SPA-ERRCD   NOT     =   SPACE
               GO  TO  4110-KENCD-EXT
           END-IF
      *
      *    入力コードセット
           PERFORM 4121-INPUTCD-JYOKEN-HAN-SEC
      *
      *----(00.01.01)--ADD-START----
      *     IF          SPA-ERRCD   NOT     =   SPACE
      *         GO  TO  4110-KENCD-EXT
      *     END-IF
      *
           EVALUATE  SPA-GMN-MAX
               WHEN  ZERO
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN  1
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN  OTHER
                   MOVE    3           TO  SPA-GMN-CUR
           END-EVALUATE
      *----(00.01.01)--ADD-END------
           .
      *
       4110-KENCD-EXT.
           EXIT.
      *
      *****************************************************************
      *     選択番号処理
      *****************************************************************
       4111-SELNUM-SEC         SECTION.
      *
           MOVE    3           TO  SPA-GMN-CUR
      *
           MOVE    N21-SELNUM  TO  SPA-GMN-SELNUM
      *
           IF          SPA-GMN-SELNUM  >   ZERO
      *    番号チェック
               IF          SPA-GMN-SELNUM  >   SPA-GMN-MAX
                   MOVE    "0008"          TO  SPA-ERRCD
                   GO  TO  4111-SELNUM-EXT
               END-IF
      *
      *----(00.01.01)--ADD-START----
               IF          SPA-GMN-MAX     =   1
                   MOVE    4           TO  SPA-GMN-CUR
                   MOVE    ZERO        TO  SPA-GMN-SELNUM
                   GO  TO  4111-SELNUM-EXT
               END-IF
      *----(00.01.01)--ADD-END------
      *
               MOVE    SPA-GMN-TNAME(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-KENNAME
               MOVE    SPA-GMN-TCD(SPA-GMN-SELNUM)
                                       TO  SPA-GMN-KENCD
               MOVE    SPA-GMN-KENCD   TO  SPA-NAI-SRYCD
      *
      *        薬剤情報マスタ読み込み
               PERFORM 800-YAKUJYO-SET-SEC
           END-IF
      *
           MOVE    4           TO  SPA-GMN-CUR
           .
      *
       4111-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    結果リスト チェック処理
      *****************************************************************
       4113-KEKKALST-SEC        SECTION.
      *
      *----(00.01.01)--ADD-START----
               IF          SPA-GMN-MAX     =   1
                   GO  TO  4113-KEKKALST-EXT
               END-IF
      *----(00.01.01)--ADD-END------
      *
      *    行選択
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       200
               IF    N21-SELECT(IDX)  =   "T"
                   MOVE    IDX             TO  SPA-GMN-SELNUM
                   MOVE    200     TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-TNAME(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KENNAME
           MOVE    SPA-GMN-TCD(SPA-GMN-SELNUM)
                                   TO  SPA-GMN-KENCD
           MOVE    SPA-GMN-KENCD   TO  SPA-NAI-SRYCD
      *
      *    薬剤情報マスタ読み込み
           PERFORM 800-YAKUJYO-SET-SEC
      *
           MOVE    4           TO  SPA-GMN-CUR
           .
      *
       4113-KEKKALST-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタ判定
      *****************************************************************
       4120-INPUTCD-CDSYU-HAN-SEC      SECTION.
      *
      *    漢字入力チェック
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    SPA-GMN-KENCD   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
      *----(02.01.02)--UPD-START---
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-KENCD
      *    全角半角混在エラー
           IF    ( KANACHK-RC          NOT =   ZERO )  OR
                 ( KANACHK-RC2             =   7    )
               MOVE    "0005"          TO  SPA-ERRCD
               GO  TO  4120-INPUTCD-CDSYU-HAN-EXT
           END-IF
      *----(02.01.02)--UPD-END-----
      *
      *    種別判定
           EVALUATE    KANACHK-SYUBETU
      *        全角
               WHEN    2
                   IF          KANACHK-MAX     >   20
                       MOVE    "0004"      TO  SPA-ERRCD
                       GO  TO  4120-INPUTCD-CDSYU-HAN-EXT
                   END-IF
                   MOVE    "J"         TO  WRK-CDSYU
                   MOVE    "INPUTCD-KEY"   TO  WRK-IPCPATH
      *        半角
               WHEN    1
      *            コード種別を桁数で決定
                   IF     (SPA-GMN-KENCD(1:KANACHK-MAX)    NUMERIC)
                       EVALUATE    KANACHK-MAX
                           WHEN    4
                               MOVE    "4"         TO  WRK-CDSYU
                           WHEN    5
                               MOVE    "5"         TO  WRK-CDSYU
                           WHEN    6
                               MOVE    "6"         TO  WRK-CDSYU
                           WHEN    OTHER
                               MOVE    "6"         TO  WRK-CDSYU
                       END-EVALUATE
                       MOVE    "INPUTCD-KEY2"      TO  WRK-IPCPATH
                   ELSE
                       IF          SPA-GMN-KENCD(1:1)  =   "P"  OR  "S"
                           MOVE    "0007"      TO  SPA-ERRCD
                           GO  TO  4120-INPUTCD-CDSYU-HAN-EXT
                       END-IF
                       MOVE    "A"             TO  WRK-CDSYU
                       MOVE    "INPUTCD-KEY"   TO  WRK-IPCPATH
                   END-IF
           END-EVALUATE
      *
        .
       4120-INPUTCD-CDSYU-HAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードセット処理
      *****************************************************************
       4121-INPUTCD-JYOKEN-HAN-SEC     SECTION.
      *
      *    入力コード読み込み
           INITIALIZE              INPUTCD-REC
           MOVE    SPA-HOSPID      TO  ICD-HOSPID
           MOVE    WRK-CDSYU       TO  ICD-CDSYU
           MOVE    SPA-GMN-KENCD   TO  ICD-INPUTCD
           MOVE    INPUTCD-REC     TO  MCPDATA-REC
           MOVE    WRK-IPCPATH     TO  ORC-DBPATH
      *
           PERFORM 900-INPUTCD-SELECT-SEC
      *
           IF          FLG-INPUTCD     =   ZERO
      *        存在したとき
               PERFORM 4122-INPUTCDLIST-SET-SEC
           ELSE
      *        存在しなかったとき
               IF          WRK-CDSYU   =   "J"
                   MOVE    ICD-INPUTCD     TO  WRK-NAME
                   PERFORM 800-TENSULIST-SET-SEC
               ELSE
      *----(00.01.01)--ADD-START----
                   IF          KANACHK-MAX     =   9
                       IF       SPA-GMN-KENCD(1:1)     =   "6"
                           MOVE    SPA-GMN-KENCD   TO  WRK-SRYCD
                           PERFORM 800-TENSU-READ-SEC
                           IF          FLG-TENSU   NOT     =   ZERO
                               MOVE    1           TO  SPA-GMN-MAX
                               MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
                           ELSE
                               MOVE    TNS-NAME    TO  SPA-GMN-KENNAME
                               MOVE    TNS-SRYCD   TO  SPA-NAI-SRYCD
                               MOVE    TNS-SRYCD   TO  SPA-GMN-KENCD
                               PERFORM 800-YAKUJYO-SET-SEC
                           END-IF
                       ELSE
                           MOVE    1           TO  SPA-GMN-MAX
                           MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
                       END-IF
                   ELSE
                       MOVE    1           TO  SPA-GMN-MAX
                       MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
                   END-IF
      *----(00.01.01)--ADD-END------
               END-IF
           END-IF
      *----(02.01.03)--ADD-START---
           PERFORM 900-INPUTCD-CLOSE-SEC
      *----(02.01.03)--ADD-END-----
           .
      *
       4121-INPUTCD-JYOKEN-HAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード点数セット処理
      *****************************************************************
       4122-INPUTCDLIST-SET-SEC    SECTION.
      *
           MOVE    ZERO        TO  IDX
      *
           PERFORM UNTIL       IDX     =   200
                   OR  FLG-INPUTCD     NOT     =   ZERO
      *        入力コード同一コードチェック
               PERFORM 4123-INPUTCDLIST-CHK-SEC
               IF          FLG-OK      NOT    =   1
                   MOVE    ICD-SRYCD   TO  WRK-SRYCD
      *            点数マスタ読み込み
                   PERFORM 800-TENSU-READ-SEC
                   IF          FLG-TENSU   =   ZERO
                       COMPUTE IDX     =   IDX     +   1
                       MOVE    IDX             TO  WRK-ZZZZ9
                       MOVE    WRK-ZZZZ9       TO  SPA-GMN-TNO(IDX)
                       MOVE    TNS-NAME        TO  SPA-GMN-TNAME(IDX)
                       MOVE    TNS-SRYCD       TO  SPA-GMN-TCD(IDX)
                   END-IF
               END-IF
               PERFORM 900-INPUTCD-FETCH-SEC
           END-PERFORM
      *
           IF          IDX     =   1
               MOVE    SPA-GMN-TCD(IDX)    TO  SPA-NAI-SRYCD
               MOVE    SPA-GMN-TCD(IDX)    TO  SPA-GMN-KENCD
               MOVE    SPA-GMN-TNAME(IDX)  TO  SPA-GMN-KENNAME
               PERFORM 800-YAKUJYO-SET-SEC
               MOVE    ZERO        TO  SPA-GMN-MAX
               INITIALIZE          SPA-GMN-TBL
           ELSE
      *----(00.01.01)--ADD-START----
               IF          IDX         =   0
                   MOVE    1           TO  SPA-GMN-MAX
                   MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
               ELSE
                   MOVE    IDX         TO  SPA-GMN-MAX
               END-IF
      *----(00.01.01)--ADD-END------
           END-IF
      *
      *----(02.01.03)--UPD-START---
      *     PERFORM 900-INPUTCD-CLOSE-SEC
      *----(02.01.03)--UPD-END-----
           .
      *
       4122-INPUTCDLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード同一コードチェック
      *****************************************************************
       4123-INPUTCDLIST-CHK-SEC    SECTION.
      *
           MOVE    ZERO        TO  FLG-OK
           MOVE    ZERO        TO  IDY
      *
           IF          ICD-SRYCD(1:1)  NOT     =   "6"
               MOVE    1        TO  FLG-OK
               GO  TO  4123-INPUTCDLIST-CHK-EXT
           END-IF
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   200
                   OR          SPA-GMN-TCD(IDY)    =   SPACE
                   OR          FLG-OK     =   1
               IF      ICD-SRYCD       =   SPA-GMN-TCD(IDY)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           .
      *
       4123-INPUTCDLIST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テキストチェック
      *****************************************************************
       430-TEXTCHK-SEC         SECTION.
      *
           MOVE    N21-NAME        TO  SPA-GMN-NAME
           MOVE    N21-EFFECT      TO  SPA-GMN-EFFECT
           MOVE    N21-COLOR       TO  SPA-GMN-COLOR
           MOVE    N21-SHAPE       TO  SPA-GMN-SHAPE
           MOVE    N21-MARK        TO  SPA-GMN-MARK
           MOVE    N21-CAUTION     TO  SPA-GMN-CAUTION
           MOVE    N21-PHOTO       TO  SPA-GMN-PHOTO
      *
      *----(02.01.02)--UPD-START---
      *    名前半角文字混在チェック
           IF          SPA-GMN-NAME    NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    80              TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-NAME    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    4           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *
      *    効能効果半角文字混在チェック
           IF          SPA-GMN-EFFECT  NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    186             TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-EFFECT  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-EFFECT
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    5           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *
      *    色半角文字混在チェック
           IF          SPA-GMN-COLOR   NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    136             TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-COLOR   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-COLOR
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    6           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *
      *    形半角文字混在チェック
           IF          SPA-GMN-SHAPE   NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    136             TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-SHAPE   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SHAPE
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    7           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *
      *----(00.01.05)--ADD-START----
      *    記号半角文字混在チェック
           IF          SPA-GMN-MARK    NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    136             TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-MARK    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MARK
               IF    ( KANACHK-SYUBETU         =   ZERO )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0005"      TO  SPA-ERRCD
                   MOVE    8           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *----(00.01.05)--ADD-END------
      *
      *    注意事項半角文字混在チェック
           IF          SPA-GMN-CAUTION NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    410             TO  KANACHK-MAX-CNT
               MOVE    SPA-GMN-CAUTION TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-CAUTION
               IF    ( KANACHK-SYUBETU     NOT =   2    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
           END-IF
      *----(02.01.02)--UPD-END-----
      *
      *    画像半角文字混在チェック
           IF          SPA-GMN-PHOTO   NOT =   SPACE
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-PHOTO   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-PHOTO
               IF    ( KANACHK-SYUBETU     NOT =   1    )  OR
                     ( KANACHK-RC          NOT =   ZERO )  OR
                     ( KANACHK-RC2             =   7    )
                   MOVE    "0003"      TO  SPA-ERRCD
               ELSE
                   IF          KANACHK-RC2     =   8
                       MOVE    "0013"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF          SPA-ERRCD   NOT     =   SPACE
                   MOVE    10          TO  SPA-GMN-CUR
                   GO  TO  430-TEXTCHK-EXT
               END-IF
      *
      *        ファイル存在チェック
               INITIALIZE              ORCSFILECHKAREA
               MOVE    SPA-GMN-PHOTO   TO  FILECHK-NAME
               CALL    "ORCSFILECHK"       USING
                                           ORCSFILECHKAREA
               IF          FILECHK-RC      NOT     =   0
                   MOVE    "0014"      TO  SPA-ERRCD
                   MOVE    10          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       430-TEXTCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル移動処理
      *****************************************************************
       440-CURMOVE-SEC         SECTION.
      *
      *    テキストチェック
           PERFORM 430-TEXTCHK-SEC
      *
           IF          SPA-ERRCD   NOT     =   SPACE
               GO  TO  440-CURMOVE-SEC-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    10
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    99
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    OTHER
                   COMPUTE SPA-GMN-CUR =   SPA-GMN-CUR +   1
           END-EVALUATE
           .
      *
       440-CURMOVE-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       450-BACK                SECTION.
      *
           MOVE    "M02"       TO  SPA-SAKIPG
           MOVE    "N21"       TO  SPA-MOTOPG
      *
           MOVE    SPACE       TO  LINKAREA
      *
           INITIALIZE          SPA-KYOTUKEY
           MOVE    SPA-KYOUTU  TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"     TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG  TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1           TO  FLG-END
      *
           .
      *
       450-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       460-CLEAR-SEC                   SECTION.
      *
      *    画面初期化
           PERFORM 310-SPASET-SEC
      *
           .
      *
       460-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       470-DEL-SEC             SECTION.
      *
           IF          SPA-NAI-SRYCD     =   SPACE
               MOVE    "0011"      TO  SPA-ERRCD
           ELSE
               MOVE    "1002"      TO  SPA-N21IDCD
           END-IF
           .
      *
       470-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       471-DEL-SEC             SECTION.
      *
           MOVE    SPACE       TO  YAKUJYO-REC
           INITIALIZE              YAKUJYO-REC
      *
           MOVE    SPA-HOSPID      TO  YAKUJYO-HOSPID
           MOVE    SPA-NAI-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
      *
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "YAKUJYO-KEY"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF          MCP-RC      NOT     =   ZERO
               MOVE    "1002"      TO  SPA-ERRCD
               GO  TO  471-DEL-EXT
           END-IF
      *
      *----(00.01.04)--UPD-START----
           IF          SPA-GMN-SELNUM  =   ZERO
      *        画面初期化
               PERFORM 310-SPASET-SEC
           ELSE
               INITIALIZE          SPA-GMN-ENTRY
               MOVE    3       TO  SPA-GMN-CUR
           END-IF
      *----(00.01.04)--UPD-END------
           .
      *
       471-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       480-TOUROKU-SEC             SECTION.
      *
      *    チェック
           IF          SPA-NAI-SRYCD   =   SPACE
               MOVE    "0011"   TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           PERFORM 430-TEXTCHK-SEC
      *
           IF          SPA-ERRCD    NOT    =   SPACE
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           IF          SPA-GMN-ENTRY   =   SPACE
               MOVE    "0010"   TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  480-TOUROKU-EXT
           END-IF
      *
           MOVE    "1001"       TO  SPA-N21IDCD
           .
      *
       480-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       481-KOUSHIN-SEC         SECTION.
      *
      *    薬剤情報マスタ読み込み
           PERFORM 900-YAKUJYO-READ-SEC
      *
           MOVE    YAKUJYO-CREYMD  TO  WRK-CREYMD
      *
           MOVE    SPACE       TO  YAKUJYO-REC
           INITIALIZE              YAKUJYO-REC
      *
           MOVE    SPA-HOSPID      TO  YAKUJYO-HOSPID
           MOVE    SPA-NAI-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
      *
           MOVE    SPA-GMN-NAME        TO  YAKUJYO-NAME
           MOVE    SPA-GMN-EFFECT      TO  YAKUJYO-EFFECT
           MOVE    SPA-GMN-CAUTION     TO  YAKUJYO-CAUTION
           MOVE    SPA-GMN-PHOTO       TO  YAKUJYO-PHOTO
           MOVE    SPA-GMN-COLOR       TO  YAKUJYO-COLOR
           MOVE    SPA-GMN-SHAPE       TO  YAKUJYO-SHAPE
           MOVE    SPA-GMN-MARK        TO  YAKUJYO-MARK
      *
           IF          FLG-YAKUJYO     =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               ACCEPT  SYS-TIME        FROM    TIME
               MOVE    SYS-TIME        TO  YAKUJYO-UPHMS
               MOVE    SPA-SYSYMD      TO  YAKUJYO-UPYMD
               MOVE    WRK-CREYMD      TO  YAKUJYO-CREYMD
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    SPA-SYSYMD      TO  YAKUJYO-CREYMD
           END-IF
           DISPLAY "YAKUJYO-UPYMD=" YAKUJYO-UPYMD
           DISPLAY "YAKUJYO-CREYMD=" YAKUJYO-CREYMD
      *
           MOVE    "YAKUJYO-KEY"   TO  ORC-DBPATH
      *
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  481-KOUSHIN-EXT
           END-IF
      *
      *----(00.01.04)--UPD-START----
           IF          SPA-GMN-SELNUM  =   ZERO
      *        画面初期化
               PERFORM 310-SPASET-SEC
           ELSE
               INITIALIZE          SPA-GMN-ENTRY
               MOVE    3       TO  SPA-GMN-CUR
           END-IF
      *----(00.01.04)--UPD-END------
           .
      *
       481-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナボタン検索処理
      *****************************************************************
       490-KEN-PHEN-SEC        SECTION.
      *
           MOVE    2           TO  SPA-GMN-CUR
      *
           MOVE    PKANA(WRK-PIN)  TO  WRK-PKANA
      *
           EVALUATE    WRK-PKANA
      *        ゜編集
               WHEN    "゜"
                   PERFORM 491-KEN-PHEN-SEC
      *        ゛編集
               WHEN    "゛" 
                   PERFORM 492-KEN-PHEN-SEC
      *        小文字変換
               WHEN    "Ｓ"
                   PERFORM 493-KEN-PHEN-SEC
      *        一文字後退
               WHEN    "Ｂ"
                   IF          WRK-PMAX    >   0
                       MOVE    SPACE       TO  WRK-PEDX(WRK-PMAX)
                       COMPUTE WRK-PMAX    =   WRK-PMAX    -   1
                   END-IF
      *        クリア
               WHEN    "Ｃ"
                   MOVE    SPACE       TO  SPA-GMN-KENNAIYO
                   INITIALIZE          WRK-PIN-AREA
                   GO  TO  490-KEN-PHEN-EXT
      *        通常
               WHEN    OTHER
                   COMPUTE WRK-PMAX    =   WRK-PMAX    +   1
                   MOVE    WRK-PKANA   TO  WRK-PEDX(WRK-PMAX)
           END-EVALUATE
      *
           IF          WRK-PMAX    >   13
               MOVE    "0009"   TO  SPA-ERRCD
               GO  TO  490-KEN-PHEN-EXT
           END-IF
      *
           MOVE    WRK-PED     TO  SPA-GMN-KENNAIYO
      *
      *----(00.01.01)--ADD-START----
           IF          WRK-PMAX    >=  2
               MOVE    SPA-GMN-KENNAIYO    TO  WRK-NAME
               INITIALIZE          SPA-GMN-AREA
               INITIALIZE          SPA-NAI-AREA
               PERFORM 800-TENSULIST-SET-SEC
               MOVE    WRK-NAME    TO  SPA-GMN-KENNAIYO
      *
      *         IF          SPA-ERRCD   NOT     =   SPACE
      *             GO  TO  490-KEN-PHEN-EXT
      *         END-IF
      *
               EVALUATE  SPA-GMN-MAX
                   WHEN  ZERO
                       MOVE    4           TO  SPA-GMN-CUR
                   WHEN  OTHER
                       MOVE    3           TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *----(00.01.01)--ADD-END------
             .
      *
       490-KEN-PHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナボタン検索処理゜編集
      *****************************************************************
       491-KEN-PHEN-SEC        SECTION.
      *
      *    ゜編集
           IF          WRK-PMAX    =   0
               GO  TO  491-KEN-PHEN-EXT
           END-IF
      *
           EVALUATE    WRK-PEDX(WRK-PMAX)
               WHEN    "ハ"
                   MOVE    "パ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヒ"
                   MOVE    "ピ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "フ"
                   MOVE    "プ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヘ"
                   MOVE    "ペ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ホ"
                   MOVE    "ポ"        TO  WRK-PEDX(WRK-PMAX)
           END-EVALUATE
           .
      *
       491-KEN-PHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナボタン検索処理゛編集
      *****************************************************************
       492-KEN-PHEN-SEC        SECTION.
      *
      *    ゛編集
           IF          WRK-PMAX    =   0
               GO  TO  492-KEN-PHEN-EXT
           END-IF
      *
           EVALUATE    WRK-PEDX(WRK-PMAX)
               WHEN    "ウ"
                   MOVE    "ヴ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "カ"
                   MOVE    "ガ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "キ"
                   MOVE    "ギ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ク"
                   MOVE    "グ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ケ"
                   MOVE    "ゲ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "コ"
                   MOVE    "ゴ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "サ"
                   MOVE    "ザ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "シ"
                   MOVE    "ジ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ス"
                   MOVE    "ズ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "セ"
                   MOVE    "ゼ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ソ"
                   MOVE    "ゾ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "タ"
                   MOVE    "ダ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "チ"
                   MOVE    "ヂ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ツ"
                   MOVE    "ヅ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "テ"
                   MOVE    "デ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ト"
                   MOVE    "ド"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ハ"
                   MOVE    "バ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヒ"
                   MOVE    "ビ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "フ"
                   MOVE    "ブ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヘ"
                   MOVE    "ベ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ホ"
                   MOVE    "ボ"        TO  WRK-PEDX(WRK-PMAX)
           END-EVALUATE
           .
      *
       492-KEN-PHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    小文字変換処理
      *****************************************************************
       493-KEN-PHEN-SEC        SECTION.
      *
           IF          WRK-PMAX    =   0
               GO  TO  493-KEN-PHEN-EXT
           END-IF
      *
           EVALUATE    WRK-PEDX(WRK-PMAX)
               WHEN    "ア"
                   MOVE    "ァ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "イ"
                   MOVE    "ィ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ウ"
                   MOVE    "ゥ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "エ"
                   MOVE    "ェ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "オ"
                   MOVE    "ォ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "カ"
                   MOVE    "ヵ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ツ"
                   MOVE    "ッ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヤ"
                   MOVE    "ャ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ユ"
                   MOVE    "ュ"        TO  WRK-PEDX(WRK-PMAX)
               WHEN    "ヨ"
                   MOVE    "ョ"        TO  WRK-PEDX(WRK-PMAX)
           END-EVALUATE
           .
      *
       493-KEN-PHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN          SECTION.
      *
      *    画面編集処理
           PERFORM 500-GMNHEN-SEC
      *
      *    エラーメッセージ表示処理
           IF          SPA-ERRCD   NOT     =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *    ガイドメッセージ表示処理
           IF          SPA-N21IDCD     NOT     =   SPACE
               PERFORM 520-N21IDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE       TO  LINKAREA
      *
           MOVE    "CURRENT"   TO  MCP-PUTTYPE.
           MOVE    "N21    "   TO  MCP-WINDOW.
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-KENCD       TO  N21-KENCD
           MOVE    SPA-GMN-KENNAIYO    TO  N21-KENNAIYO
           MOVE    SPA-GMN-KENNAME     TO  N21-KENNAME
           MOVE    SPA-GMN-SELNUM      TO  N21-SELNUM
           MOVE    SPA-GMN-NAME        TO  N21-NAME
           MOVE    SPA-GMN-EFFECT      TO  N21-EFFECT
           MOVE    SPA-GMN-COLOR       TO  N21-COLOR
           MOVE    SPA-GMN-SHAPE       TO  N21-SHAPE
           MOVE    SPA-GMN-MARK        TO  N21-MARK
           MOVE    SPA-GMN-CAUTION     TO  N21-CAUTION
           MOVE    SPA-GMN-PHOTO       TO  N21-PHOTO
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
               MOVE    SPA-GMN-TNO(IDX)    TO  N21-TNO(IDX)
               MOVE    SPA-GMN-TNAME(IDX)  TO  N21-TNAME(IDX)
               IF          IDX         =   SPA-GMN-SELNUM
                   MOVE    "T"         TO  N21-SELECT(IDX)
               ELSE
                   MOVE    "F"         TO  N21-SELECT(IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX     TO  N21-COUNT
      *
           PERFORM 530-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC          SECTION.
      *
           MOVE    SPACE       TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "全て全角で入力して下さい"  
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "全て半角で入力して下さい"  
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
               "全角の入力コードは１０文字までで入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "半角と全角の混在はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
               "数字のみの入力コードは４桁から６桁で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    
               "入力コードは始まりがＳ、Ｐ以外のコードにして下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "範囲外の番号が入力されています"
                                           TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "文字数が多すぎます"
                                           TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "データを入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "コードを入力して下さい"
                                           TO  SPA-ERRMSG
      *----(00.01.01)--ADD-START----
      *         WHEN    "0012"
      *             MOVE    "該当データがありません"
      *                                     TO  SPA-ERRMSG
      *----(00.01.01)--ADD-END------
               WHEN    "0013"
                   MOVE    "フルパスに空白が入力されています"
                                           TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "フルパスを正しく入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "登録できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE       TO  N21ERR
           MOVE    SPA-ERRCD   TO  N21ERR-ERRCODE
           MOVE    SPA-ERRMSG  TO  N21ERR-ERRMSG
           MOVE    "B01"       TO  MCP-WIDGET
      *
           MOVE    "N21"       TO  SPA-MOTOPG
           MOVE    "N21ERR"    TO  SPA-SAKIPG
      *
           MOVE    "NEW"       TO  MCP-PUTTYPE
           MOVE    "N21ERR"    TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1           TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示処理
      *****************************************************************
       520-N21IDSET-SEC        SECTION.
      *
           MOVE    SPACE       TO  WRK-N21IDMSG
      *
           EVALUATE    SPA-N21IDCD
               WHEN    "1001"
                   MOVE    "薬剤情報を登録します"  TO  WRK-N21IDMSG
               WHEN    "1002"
                   MOVE    "薬剤情報を削除します"  TO  WRK-N21IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N21ID1
           MOVE    SPA-N21IDCD     TO  N21ID1-ID1CODE
           MOVE    WRK-N21IDMSG    TO  N21ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N21"       TO  SPA-MOTOPG
           MOVE    "N21ID1"    TO  SPA-SAKIPG
      *
           MOVE    "NEW"       TO  MCP-PUTTYPE
           MOVE    "N21ID1"    TO  MCP-WINDOW
      *
      *    ＰＵＴ処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1           TO  FLG-END
           .
      *
       520-N21IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       530-MAPCUR-SEC         SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2
                   MOVE  "KENCD"       TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SELNUM"      TO  MCP-WIDGET
               WHEN    4
                   MOVE  "NAME"        TO  MCP-WIDGET
               WHEN    5
                   MOVE  "EFFECT"      TO  MCP-WIDGET
               WHEN    6
                   MOVE  "COLOR"       TO  MCP-WIDGET
               WHEN    7
                   MOVE  "SHAPE"       TO  MCP-WIDGET
               WHEN    8
                   MOVE  "MARK"        TO  MCP-WIDGET
               WHEN    9
                   MOVE  "CAUTION"     TO  MCP-WIDGET
               WHEN    10
                   MOVE  "PHOTO"       TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       530-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    リストセット処理
      *****************************************************************
       800-TENSULIST-SET-SEC   SECTION.
      *
      *----(00.01.01)--ADD-START----
           MOVE    ZERO        TO  SPA-GMN-MAX
           INITIALIZE          SPA-GMN-TBL
      *----(00.01.01)--ADD-END------
      *
      *    点数マスタ読み込み
           INITIALIZE              TNS-TENSU-REC
           MOVE    WRK-NAME        TO  TNS-NAME
           MOVE    WRK-NAME        TO  TNS-KANANAME
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
           MOVE    "TENSU-KEY5"    TO  WRK-TNSPATH
           PERFORM 900-TENSU-SELECT-SEC
      *
      *----(00.01.01)--ADD-START----
           IF          FLG-TENSU   NOT     =   ZERO
               MOVE    1           TO  SPA-GMN-MAX
               MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
               PERFORM 900-TENSU-CLOSE-SEC
               GO  TO  800-TENSULIST-SET-EXT
           END-IF
      *----(00.01.01)--ADD-END------
      *
           MOVE    ZERO        TO  IDX
      *
           PERFORM UNTIL       IDX     =    200
                   OR  FLG-TENSU       NOT     =   ZERO
               COMPUTE IDX     =   IDX     +   1
               MOVE    IDX             TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  SPA-GMN-TNO(IDX)
               MOVE    TNS-NAME        TO  SPA-GMN-TNAME(IDX)
               MOVE    TNS-SRYCD       TO  SPA-GMN-TCD(IDX)
               PERFORM 900-TENSU-FETCH-SEC
           END-PERFORM
      *
           IF          IDX     =   1
               MOVE    SPA-GMN-TCD(IDX)    TO  SPA-NAI-SRYCD
               MOVE    SPA-GMN-TCD(IDX)    TO  SPA-GMN-KENCD
               MOVE    SPA-GMN-TNAME(IDX)  TO  SPA-GMN-KENNAME
               PERFORM 800-YAKUJYO-SET-SEC
               MOVE    ZERO        TO  SPA-GMN-MAX
               INITIALIZE          SPA-GMN-TBL
           ELSE
      *----(00.01.01)--ADD-START----
               IF          IDX     =   0
                   MOVE    1           TO  SPA-GMN-MAX
                   MOVE    "該当データがありません"
                                       TO  SPA-GMN-TNAME(1)
               ELSE
                   MOVE    IDX         TO  SPA-GMN-MAX
               END-IF
      *----(00.01.01)--ADD-END------
           END-IF
      *
           PERFORM 900-TENSU-CLOSE-SEC
           .
      *
       800-TENSULIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報セット処理
      *****************************************************************
       800-YAKUJYO-SET-SEC     SECTION.
      *
      *    薬剤情報マスタ読み込み
           PERFORM 900-YAKUJYO-READ-SEC
      *
      *    薬剤情報セット
           MOVE    YAKUJYO-NAME        TO  SPA-GMN-NAME
           MOVE    YAKUJYO-EFFECT      TO  SPA-GMN-EFFECT
           MOVE    YAKUJYO-CAUTION     TO  SPA-GMN-CAUTION
           MOVE    YAKUJYO-PHOTO       TO  SPA-GMN-PHOTO
           MOVE    YAKUJYO-COLOR       TO  SPA-GMN-COLOR
           MOVE    YAKUJYO-SHAPE       TO  SPA-GMN-SHAPE
           MOVE    YAKUJYO-MARK        TO  SPA-GMN-MARK
      *
      *----(00.01.02)--ADD-START----
      *----(00.01.04)--ADD-START----
           IF          FLG-YAKUJYO     =   1
               MOVE    SPA-GMN-KENNAME     TO  SPA-GMN-NAME
               MOVE    MSG-CAUTION         TO  SPA-GMN-CAUTION
               GO  TO  800-YAKUJYO-SET-EXT
           END-IF
      *----(00.01.04)--ADD-END------
      *----(00.01.02)--ADD-END------
           .
       800-YAKUJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み処理
      *****************************************************************
       800-TENSU-READ-SEC      SECTION.
      *
      *    点数マスタ読み込み
           INITIALIZE              TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
           MOVE    "TENSU-KEY"     TO  WRK-TNSPATH
           PERFORM 900-TENSU-SELECT-SEC
           PERFORM 900-TENSU-CLOSE-SEC
           .
      *
       800-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-SELECT-SEC    SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-TNSPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF          MCP-RC      =   ZERO
               MOVE    ZERO        TO  FLG-TENSU
               PERFORM 900-TENSU-FETCH-SEC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
            .
       900-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-FETCH-SEC     SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    WRK-TNSPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
      *
           IF          MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み（キー）
      *****************************************************************
       900-TENSU-CLOSE-SEC     SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-TNSPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           .
      *
       900-TENSU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-SELECT-SEC  SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-IPCPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           IF          MCP-RC      =   ZERO
               MOVE    ZERO        TO  FLG-INPUTCD
               PERFORM 900-INPUTCD-FETCH-SEC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
            .
       900-INPUTCD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-FETCH-SEC   SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    WRK-IPCPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
      *
           IF          MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-INPUTCD
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
           .
      *
       900-INPUTCD-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスタ読み込み（キー）
      *****************************************************************
       900-INPUTCD-CLOSE-SEC   SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-IPCPATH     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           .
      *
       900-INPUTCD-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC  SECTION.
      *
           INITIALIZE          YAKUJYO-REC
           MOVE    SPA-HOSPID      TO  YAKUJYO-HOSPID
           MOVE    SPA-NAI-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "YAKUJYO-KEY"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
      *
           IF          MCP-RC      =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "YAKUJYO-KEY"   TO  ORC-DBPATH
               CALL    "ORCMCPSUB"     USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1           TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1           TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "YAKUJYO-KEY"   TO  ORC-DBPATH
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
            .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW          SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCMCPSUB"     USING
                                   MCPAREA
                                   ORCMCPAREA
                                   MCPDATA-REC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
