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
       PROGRAM-ID.             ORCGN31.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 住所マスタ設定
      *  コンポーネント名  : 住所マスタ登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/08/04    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 01.01.01     NACL-森脇     04/10/04  全角半角チェック追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N31COMMON-SPA".
      *
           COPY    "N31SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(05).
           03  IDXC                PIC 9(04).
           03  IDXL                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH          PIC X(64).
           03  WRK-N31IDMSG        PIC X(70).
      *
           03  WRK-LPUBCD          PIC X(05).
           03  WRK-RENNUM          PIC 9(02).
      *
       01  WRK-ADRS-AREA.
           03  WRK-ADRS-G.
               05  WRK-ADRS1           PIC X(32)   VALUE
                   "01ホッカイドウ        北海道    ".
               05  WRK-ADRS2           PIC X(32)   VALUE
                   "02アオモリケン        青森県    ".
               05  WRK-ADRS3           PIC X(32)   VALUE
                   "03イワテケン          岩手県    ".
               05  WRK-ADRS4           PIC X(32)   VALUE
                   "04ミヤギケン          宮城県    ".
               05  WRK-ADRS5           PIC X(32)   VALUE
                   "05アキタケン          秋田県    ".
               05  WRK-ADRS6           PIC X(32)   VALUE
                   "06ヤマガタケン        山形県    ".
               05  WRK-ADRS7           PIC X(32)   VALUE
                   "07フクシマケン        福島県    ".
               05  WRK-ADRS8           PIC X(32)   VALUE
                   "08イバラキケン        茨城県    ".
               05  WRK-ADRS9           PIC X(32)   VALUE
                   "09トチギケン          栃木県    ".
               05  WRK-ADRS10          PIC X(32)   VALUE
                   "10グンマケン          群馬県    ".
               05  WRK-ADRS11          PIC X(32)   VALUE
                   "11サイタマケン        埼玉県    ".
               05  WRK-ADRS12          PIC X(32)   VALUE
                   "12チバケン            千葉県    ".
               05  WRK-ADRS13          PIC X(32)   VALUE
                   "13トウキョウト        東京都    ".
               05  WRK-ADRS14          PIC X(32)   VALUE
                   "14カナガワケン        神奈川県  ".
               05  WRK-ADRS15          PIC X(32)   VALUE
                   "15ニイガタケン        新潟県    ".
               05  WRK-ADRS16          PIC X(32)   VALUE
                   "16トヤマケン          富山県    ".
               05  WRK-ADRS17          PIC X(32)   VALUE
                   "17イシカワケン        石川県    ".
               05  WRK-ADRS18          PIC X(32)   VALUE
                   "18フクイケン          福井県    ".
               05  WRK-ADRS19          PIC X(32)   VALUE
                   "19ヤマナシケン        山梨県    ".
               05  WRK-ADRS20          PIC X(32)   VALUE
                   "20ナガノケン          長野県    ".
               05  WRK-ADRS21          PIC X(32)   VALUE
                   "21ギフケン            岐阜県    ".
               05  WRK-ADRS22          PIC X(32)   VALUE
                   "22シズオカケン        静岡県    ".
               05  WRK-ADRS23          PIC X(32)   VALUE
                   "23アイチケン          愛知県    ".
               05  WRK-ADRS24          PIC X(32)   VALUE
                   "24ミエケン            三重県    ".
               05  WRK-ADRS25          PIC X(32)   VALUE
                   "25シガケン            滋賀県    ".
               05  WRK-ADRS26          PIC X(32)   VALUE
                   "26キョウトフ          京都府    ".
               05  WRK-ADRS27          PIC X(32)   VALUE
                   "27オオサカフ          大阪府    ".
               05  WRK-ADRS28          PIC X(32)   VALUE
                   "28ヒョウゴケン        兵庫県    ".
               05  WRK-ADRS29          PIC X(32)   VALUE
                   "29ナラケン            奈良県    ".
               05  WRK-ADRS30          PIC X(32)   VALUE
                   "30ワカヤマケン        和歌山県  ".
               05  WRK-ADRS31          PIC X(32)   VALUE
                   "31トットリケン        鳥取県    ".
               05  WRK-ADRS32          PIC X(32)   VALUE
                   "32シマネケン          島根県    ".
               05  WRK-ADRS33          PIC X(32)   VALUE
                   "33オカヤマケン        岡山県    ".
               05  WRK-ADRS34          PIC X(32)   VALUE
                   "34ヒロシマケン        広島県    ".
               05  WRK-ADRS35          PIC X(32)   VALUE
                   "35ヤマグチケン        山口県    ".
               05  WRK-ADRS36          PIC X(32)   VALUE
                   "36トクシマケン        徳島県    ".
               05  WRK-ADRS37          PIC X(32)   VALUE
                   "37カガワケン          香川県    ".
               05  WRK-ADRS38          PIC X(32)   VALUE
                   "38エヒメケン          愛媛県    ".
               05  WRK-ADRS39          PIC X(32)   VALUE
                   "39コウチケン          高知県    ".
               05  WRK-ADRS40          PIC X(32)   VALUE
                   "40フクオカケン        福岡県    ".
               05  WRK-ADRS41          PIC X(32)   VALUE
                   "41サガケン            佐賀県    ".
               05  WRK-ADRS42          PIC X(32)   VALUE
                   "42ナガサキケン        長崎県    ".
               05  WRK-ADRS43          PIC X(32)   VALUE
                   "43クマモトケン        熊本県    ".
               05  WRK-ADRS44          PIC X(32)   VALUE
                   "44オオイタケン        大分県    ".
               05  WRK-ADRS45          PIC X(32)   VALUE
                   "45ミヤザキケン        宮崎県    ".
               05  WRK-ADRS46          PIC X(32)   VALUE
                   "46カゴシマケン        鹿児島県  ".
               05  WRK-ADRS47          PIC X(32)   VALUE
                   "47オキナワケン        沖縄県    ".
           03  WRK-ADRS-R          REDEFINES   WRK-ADRS-G.
               05  WRK-ADRS-TBL        OCCURS  47.
                   07  WRK-ADRSCD          PIC X(02).
                   07  WRK-ADRSKANA        PIC X(20).
                   07  WRK-ADRSNAME        PIC X(10).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理基本情報
           COPY    "CPSK1001.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *
       PROCEDURE               DIVISION    USING
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
           MOVE    SPA-WORK        TO  SPA-N31KYOUTU
           MOVE    SPA-FREE        TO  SPA-N31SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-N31SCRAREA  TO  SPA-FREE
           MOVE    SPA-N31KYOUTU   TO  SPA-WORK
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "N31ERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "N31"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
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
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "N31ID1"
                   PERFORM 330-NID1-SET-SEC
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
      *    確認画面（ＮＩＤ１）ＯＫ処理
      *****************************************************************
       330-NID1-SET-SEC            SECTION.
      *
           IF      SPA-N31ID1-FLG    =   "OK"
               IF      SPA-N31IDCD       =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-N31ID1-FLG
           MOVE    SPACE           TO  SPA-N31IDCD
           .
       330-NID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-N31-AREA
                                       SPA-N31KYOUTU
      *
      *    都道府県ＣＤコンボセット
           PERFORM VARYING IDXC    FROM    1   BY      1
                   UNTIL   IDXC    >       47
               MOVE    WRK-ADRSCD(IDXC)    TO  SPA-GMN-PREFCDL(IDXC)
               MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFMEIL(IDXC)
           END-PERFORM
           MOVE    47              TO  SPA-GMN-PREFCD-MAX
      *
           PERFORM 320-SPASET-PREFCD-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県初期編集処理
      *****************************************************************
       320-SPASET-PREFCD-SEC       SECTION.
      *
      *    システム管理基本情報読み込み
           PERFORM 900-SYS1001-READ-SEC
      *
      *    都道府県コード初期値セット
           IF      FLG-SYSKANRI    =   ZERO
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL   IDXC    >       SPA-GMN-PREFCD-MAX
                   IF      SYS-1001-PREFNUM    =   SPA-GMN-PREFCDL(IDXC)
                       MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                                   TO  SPA-GMN-PREFCD-G
                       MOVE    WRK-ADRSKANA(IDXC)  TO  SPA-GMN-PREFKANA
                       MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFNAME
                       MOVE    SPA-GMN-PREFCD-MAX  TO  IDXC
                   END-IF
               END-PERFORM
           END-IF
           . 
       32O-SPASET-PREFCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DEL-SEC
      *        前ページ
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 404-BACKPAGE-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 405-NEXTPAGE-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
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
           MOVE    N31-PREFCD      TO  SPA-GMN-PREFCD-G
           MOVE    N31-POST        TO  SPA-GMN-POST
           MOVE    N31-SELNUM      TO  SPA-GMN-SELNUM
           MOVE    N31-PREFKANA    TO  SPA-GMN-PREFKANA
           MOVE    N31-CITYKANA    TO  SPA-GMN-CITYKANA
           MOVE    N31-TOWNKANA    TO  SPA-GMN-TOWNKANA
           MOVE    N31-PREFNAME    TO  SPA-GMN-PREFNAME
           MOVE    N31-CITYNAME    TO  SPA-GMN-CITYNAME
           MOVE    N31-TOWNNAME    TO  SPA-GMN-TOWNNAME
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PREFCD"
      *            都道府県番号チェック
                   PERFORM 4100-PREFCD-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "POST"
      *            郵便番号入力
                   PERFORM 4100-POST-SET-SEC
               WHEN    "ACTIVATE"      ALSO    "CITYKANA"
      *            市町村名カナチェック
                   PERFORM 4100-CITYKANA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "TOWNKANA"
      *            町域名カナチェック
                   PERFORM 4100-TOWNKANA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "CITYNAME"
      *            市町村名チェック
                   PERFORM 4100-CITYNAME-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "TOWNNAME"
      *            町域名チェック
                   PERFORM 4100-TOWNNAME-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
      *            選択番号処理
                   PERFORM 4100-SELNUM-SET-SEC
               WHEN    ANY             ALSO    "CYOIKILST"
      *            町域リストチェック
                   PERFORM 4100-CYOIKILST-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    郵便番号入力
      *****************************************************************
       4100-POST-SET-SEC           SECTION.
      *
      *    郵便番号チェック
           PERFORM 4100-POST-CHK-SEC
           IF    SPA-ERRCD         NOT =   SPACE
               GO  TO  4100-POST-SET-EXT
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
                                       SPA-NAI-NEXT
                                       SPA-NAI-BACK
           MOVE    SPACE           TO  SPA-GMN-CITYKANA
                                       SPA-GMN-TOWNKANA
                                       SPA-GMN-CITYNAME
                                       SPA-GMN-TOWNNAME
      *
      *    都道府県番号初期値セット
           PERFORM 320-SPASET-PREFCD-SEC
      *
      *    該当町域リストセット
           PERFORM 4100-CYOIKILST-SET-SEC
           .
      *
       4100-POST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     郵便番号チェック
      *****************************************************************
       4100-POST-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-POST    =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  4100-POST-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-POST        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-POST
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )  OR
                 ( KANACHK-SYUBETU     =   2    )
      *        全角半角混在エラー
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
      *
       4100-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域リストセット
      *****************************************************************
       4100-CYOIKILST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-CYOIKI-G
           MOVE    ZERO            TO  IDX
           MOVE    ZERO            TO  IDX2
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "ADRS-KEY6"     TO  WRK-DBPATH
           PERFORM 900-ADRS-SELECT-SEC
      *
           PERFORM UNTIL  (FLG-ADRS    =   1   )  OR
                          (IDX         >=  100  )
               ADD     1               TO  IDX2
               IF      SPA-NAI-NEXT    <   IDX2
                   ADD     1               TO  IDX
                   MOVE    IDX             TO  SPA-GMN-CYOIKI-MAX
                   MOVE    IDX             TO  SPA-GMN-CYO-CD(IDX)
                   MOVE    ADRS-POST       TO  SPA-GMN-CYO-POST(IDX)
                   MOVE    ADRS-EDITADRS-NAME
                                           TO  SPA-GMN-CYO-ADRS(IDX)
                   MOVE    ADRS-LPUBCD     TO  SPA-GMN-CYO-LPUBCD(IDX)
                   MOVE    ADRS-EDANUM     TO  SPA-GMN-CYO-EDANUM(IDX)
                   IF      ADRS-LPUBCD     =   "00000"
                       MOVE    "ユ"        TO  SPA-GMN-CYO-HEN(IDX)
                   END-IF
               END-IF
               PERFORM 900-ADRS-FETCH-SEC
           END-PERFORM
      *
           PERFORM 900-ADRS-CLOSE-SEC
      *
      *    該当する町域が１００件以上のとき次ページ使用可
           IF    ( SPA-GMN-CYOIKI-MAX      =   100  )  AND
                 ( FLG-ADRS                =   ZERO )
               MOVE    SPA-NAI-NEXT    TO  SPA-NAI-BACK
                                           SPA-NAI-NEXT2
                                           SPA-NAI-BACK2
               MOVE    IDX2            TO  SPA-NAI-NEXT
           ELSE
               MOVE    SPA-NAI-NEXT    TO  SPA-NAI-NEXT2
               MOVE    ZERO            TO  SPA-NAI-NEXT
           END-IF
      *
      *    該当する町域がないときは新規登録
           EVALUATE    SPA-GMN-CYOIKI-MAX
             WHEN      ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
             WHEN      OTHER
               MOVE    2               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           MOVE    SPA-GMN-POST    TO  SPA-NAI-POST
           .
      *
       4100-CYOIKILST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域リスト選択
      *****************************************************************
       4100-CYOIKILST-CHK-SEC      SECTION.
      *
      *    行選択
           PERFORM VARYING     IDXL    FROM    1   BY  1
                   UNTIL       IDXL    >       100
               IF    N31-CYOIKI-SELECT(IDXL)   =   "T"
                   MOVE    IDXL            TO  SPA-GMN-SELNUM
                   MOVE    100             TO  IDXL
               END-IF
           END-PERFORM
      *
      *    選択番号入力
           PERFORM 4100-SELNUM-SET-SEC
      *
           .
      *
       4100-CYOIKILST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力
      *****************************************************************
       4100-SELNUM-SET-SEC         SECTION.
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4100-SELNUM-SET-EXT
           END-IF
      *
      *    選択番号チェック
           PERFORM 4100-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-SELNUM-SET-EXT
           END-IF
      *
           INITIALIZE                  ADRS-REC
           MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                   TO  ADRS-LPUBCD
           MOVE    SPA-GMN-CYO-POST(SPA-GMN-SELNUM)
                                   TO  ADRS-POST
           MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                   TO  ADRS-EDANUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "ADRS-KEY"      TO  WRK-DBPATH
           PERFORM 900-ADRS-SELECT-SEC
      *
           IF      FLG-ADRS        =   ZERO
               MOVE    ADRS-POST       TO  SPA-GMN-POST
               MOVE    ADRS-CITYKANA   TO  SPA-GMN-CITYKANA
               MOVE    ADRS-TOWNKANA   TO  SPA-GMN-TOWNKANA
               MOVE    ADRS-CITYNAME   TO  SPA-GMN-CITYNAME
               MOVE    ADRS-TOWNNAME   TO  SPA-GMN-TOWNNAME
      *
               PERFORM VARYING IDXC    FROM    1   BY  1
                       UNTIL   IDXC    >       SPA-GMN-PREFCD-MAX
                   IF      ADRS-PREFNAME   =   SPA-GMN-PREFMEIL(IDXC)
                       MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                                   TO  SPA-GMN-PREFCD-G
                       MOVE    WRK-ADRSKANA(IDXC)  TO  SPA-GMN-PREFKANA
                       MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFNAME
                       MOVE    SPA-GMN-PREFCD-MAX  TO  IDXC
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 900-ADRS-CLOSE-SEC
      *
      *    町域リストクリア
           INITIALIZE                  SPA-GMN-CYOIKI-G
           MOVE    1               TO  SPA-GMN-CYOIKI-MAX
           MOVE    1               TO  SPA-GMN-CYO-CD(1)
           MOVE    ADRS-POST       TO  SPA-GMN-CYO-POST(1)
           MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN-CYO-ADRS(1)
           MOVE    ADRS-LPUBCD     TO  SPA-GMN-CYO-LPUBCD(1)
           MOVE    ADRS-EDANUM     TO  SPA-GMN-CYO-EDANUM(1)
           IF      ADRS-LPUBCD     =   "00000"
               MOVE    "ユ"        TO  SPA-GMN-CYO-HEN(1)
           END-IF
      *
           MOVE    1               TO  SPA-GMN-SELNUM
           MOVE    ZERO            TO  SPA-NAI-BACK
                                       SPA-NAI-NEXT
           MOVE    4               TO  SPA-GMN-CUR
           .
      *
       4100-SELNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック
      *****************************************************************
       4100-SELNUM-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-SELNUM      >   SPA-GMN-CYOIKI-MAX )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    都道府県番号チェック
      *****************************************************************
       4100-PREFCD-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-PREFCD      =   SPACE )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4100-PREFCD-CHK-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   SPA-GMN-PREFCD-MAX )
               IF      SPA-GMN-PREFCD  =   SPA-GMN-PREFCDL(IDXC)
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                               TO  SPA-GMN-PREFCD-G
                   MOVE    WRK-ADRSKANA(IDXC)  TO  SPA-GMN-PREFKANA
                   MOVE    WRK-ADRSNAME(IDXC)  TO  SPA-GMN-PREFNAME
                   MOVE    SPA-GMN-PREFCD-MAX  TO  IDXC
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
               GO  TO  4100-PREFCD-CHK-EXT
           END-IF
      *
           MOVE    4               TO  SPA-GMN-CUR
           .
      *
       4100-PREFCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    市町村名カナチェック
      *****************************************************************
       4100-CITYKANA-CHK-SEC       SECTION.
      *
           MOVE    5               TO  SPA-GMN-CUR
      *
           IF    ( SPA-GMN-CITYKANA    =   SPACE )
               GO  TO  4100-CITYKANA-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-CITYKANA    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-CITYKANA
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )  OR
                 ( KANACHK-SYUBETU     =   1    )
      *        全角半角混在エラー
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO  TO  4100-CITYKANA-CHK-EXT
           END-IF
      *
           .
      *
       4100-CITYKANA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域名カナチェック
      *****************************************************************
       4100-TOWNKANA-CHK-SEC       SECTION.
      *
           MOVE    6               TO  SPA-GMN-CUR
      *
           IF    ( SPA-GMN-TOWNKANA    =   SPACE )
               GO  TO  4100-TOWNKANA-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-TOWNKANA    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOWNKANA
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )  OR
                 ( KANACHK-SYUBETU     =   1    )
      *        全角半角混在エラー
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    5               TO  SPA-GMN-CUR
               GO  TO  4100-TOWNKANA-CHK-EXT
           END-IF
      *
           .
      *
       4100-TOWNKANA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    市町村名チェック
      *****************************************************************
       4100-CITYNAME-CHK-SEC       SECTION.
      *
           MOVE    7               TO  SPA-GMN-CUR
      *
           IF    ( SPA-GMN-CITYNAME    =   SPACE )
               GO  TO  4100-CITYNAME-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-CITYNAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-CITYNAME
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )  OR
                 ( KANACHK-SYUBETU     =   1    )
      *        全角半角混在エラー
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
               GO  TO  4100-CITYNAME-CHK-EXT
           END-IF
      *
           .
      *
       4100-CITYNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    町域名チェック
      *****************************************************************
       4100-TOWNNAME-CHK-SEC       SECTION.
      *
           MOVE    99              TO  SPA-GMN-CUR
      *
           IF    ( SPA-GMN-TOWNNAME    =   SPACE )
               GO  TO  4100-TOWNNAME-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-TOWNNAME    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TOWNNAME
           IF    ( KANACHK-RC      NOT =   ZERO )  OR
                 ( KANACHK-RC2         =   7    )  OR
                 ( KANACHK-SYUBETU     =   1    )
      *        全角半角混在エラー
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
               GO  TO  4100-TOWNNAME-CHK-EXT
           END-IF
           .
      *
       4100-TOWNNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック
      *****************************************************************
       4100-KOUSHINMAE-CHK-SEC     SECTION.
      *
           PERFORM 4100-POST-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-PREFCD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-CITYKANA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-TOWNKANA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-CITYNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
      *
           PERFORM 4100-TOWNNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUSHINMAE-CHK-EXT
           END-IF
           .
      *
       4100-KOUSHINMAE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新後セット
      *****************************************************************
       4100-KOUSHINGO-SET-SEC      SECTION.
      *
           MOVE    SPA-NAI-POST    TO  SPA-GMN-POST
           MOVE    SPA-NAI-NEXT2   TO  SPA-NAI-NEXT
           MOVE    SPA-NAI-BACK2   TO  SPA-NAI-BACK
           MOVE    ZERO            TO  SPA-GMN-SELNUM
                                       SPA-NAI-NEXT2
                                       SPA-NAI-BACK2
           MOVE    SPACE           TO  SPA-GMN-CITYKANA
                                       SPA-GMN-TOWNKANA
                                       SPA-GMN-CITYNAME
                                       SPA-GMN-TOWNNAME
                                       SPA-NAI-POST
      *
      *    都道府県番号初期値セット
           PERFORM 320-SPASET-PREFCD-SEC
      *
      *    該当町域リストセット
           PERFORM 4100-CYOIKILST-SET-SEC
           .
      *
       4100-KOUSHINGO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M02"           TO  SPA-SAKIPG
           MOVE    "N31"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
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
      *    削除確認画面表示処理
      *****************************************************************
       402-CLEAR-SEC               SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .    
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                 SECTION.
      *
           PERFORM 4100-POST-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  403-DEL-EXT
           END-IF
      *
           IF    ( SPA-GMN-SELNUM  =   ZERO )  OR
                 ( SPA-GMN-SELNUM  >   SPA-GMN-CYOIKI-MAX )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  403-DEL-EXT
           END-IF
      *
           IF      SPA-GMN-CYO-HEN(SPA-GMN-SELNUM)
                                   NOT =   "ユ"
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO  TO  403-DEL-EXT
           END-IF
      *
           MOVE    "1002"          TO  SPA-N31IDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                SECTION.
      *
           MOVE    SPACE           TO  ADRS-REC
           INITIALIZE                  ADRS-REC
           MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                   TO  ADRS-LPUBCD
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                   TO  ADRS-EDANUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "ADRS-KEY"      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
      *
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1002"          TO  SPA-ERRCD
               GO  TO  4031-DEL-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前ページ表示処理
      *****************************************************************
       404-BACKPAGE-SEC            SECTION.
      *
           IF      SPA-NAI-BACK    =   ZERO
               GO  TO  404-BACKPAGE-EXT
           END-IF
      *
           IF    ( SPA-GMN-CYOIKI-MAX      =   100  )
               COMPUTE SPA-NAI-NEXT    =   SPA-NAI-BACK    -   100
           ELSE
               MOVE    SPA-NAI-BACK    TO  SPA-NAI-NEXT
           END-IF
      *
           PERFORM 4100-CYOIKILST-SET-SEC
      *
           .
       404-BACKPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ表示処理
      *****************************************************************
       405-NEXTPAGE-SEC            SECTION.
      *
           IF      SPA-NAI-NEXT    =   ZERO
               GO  TO  405-NEXTPAGE-EXT
           END-IF
      *
           PERFORM 4100-CYOIKILST-SET-SEC
      *
           .
       405-NEXTPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           IF    ( SPA-GMN-POST(1:)    =   SPACE ) OR
                 ( SPA-GMN-POST(2:)    =   SPACE ) OR
                 ( SPA-GMN-POST(3:)    =   SPACE ) OR
                 ( SPA-GMN-POST(4:)    =   SPACE ) OR
                 ( SPA-GMN-POST(5:)    =   SPACE ) OR
                 ( SPA-GMN-POST(6:)    =   SPACE ) OR
                 ( SPA-GMN-POST(7:)    =   SPACE )
               MOVE    "0011"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO  TO  490-TOUROKU-EXT
           END-IF
      *
      *    更新前チェック
           PERFORM 4100-KOUSHINMAE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  490-TOUROKU-EXT
           END-IF
      *
           MOVE    "1001"          TO  SPA-N31IDCD
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
           MOVE    SPACE           TO  WRK-LPUBCD
      *
      *    連番と地方公共団体コードセット
           IF      SPA-GMN-SELNUM  =   ZERO
               MOVE    "00000"         TO  WRK-LPUBCD
      *
               MOVE    SPACE           TO  ADRS-REC
               INITIALIZE                  ADRS-REC
               MOVE    SPA-GMN-POST    TO  ADRS-POST
               MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
               MOVE    "ADRS-KEY7"     TO  WRK-DBPATH
               MOVE    ADRS-REC        TO  MCPDATA-REC
               PERFORM 900-ADRS-SELECT-SEC
               PERFORM 900-ADRS-CLOSE-SEC
      *
               IF      FLG-ADRS        =   ZERO
                   IF      ADRS-EDANUM     >=  99
                       MOVE    "0012"          TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   ELSE
                       COMPUTE WRK-RENNUM  =   ADRS-EDANUM +   1
                   END-IF
               ELSE
                   COMPUTE WRK-RENNUM  =   ADRS-EDANUM +   1
               END-IF
           ELSE
               MOVE    SPA-GMN-CYO-EDANUM(SPA-GMN-SELNUM)
                                       TO  WRK-RENNUM
               MOVE    SPA-GMN-CYO-LPUBCD(SPA-GMN-SELNUM)
                                       TO  WRK-LPUBCD
           END-IF
      *
      *    追加処理
           PERFORM 490-INSERT-SEC
      *
      *    更新後セット
           PERFORM 4100-KOUSHINGO-SET-SEC
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       490-INSERT-SEC              SECTION.
      *
           MOVE    SPACE           TO  ADRS-REC
           INITIALIZE                  ADRS-REC
           MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
           MOVE    SPA-GMN-POST    TO  ADRS-POST
           MOVE    WRK-RENNUM      TO  ADRS-EDANUM
           MOVE    "ADRS-KEY"      TO  WRK-DBPATH
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-SELECT-SEC
           PERFORM 900-ADRS-CLOSE-SEC
      *
      *    該当データがあるときは更新、ないときは追加
           IF      FLG-ADRS        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    SPACE           TO  ADRS-REC
               INITIALIZE                  ADRS-REC
               MOVE    WRK-LPUBCD      TO  ADRS-LPUBCD
               MOVE    SPA-GMN-POST    TO  ADRS-POST
               MOVE    WRK-RENNUM      TO  ADRS-EDANUM
           END-IF
      *
           MOVE    SPA-GMN-PREFKANA    TO  ADRS-PREFKANA
           MOVE    SPA-GMN-CITYKANA    TO  ADRS-CITYKANA
           MOVE    SPA-GMN-TOWNKANA    TO  ADRS-TOWNKANA
           MOVE    SPA-GMN-PREFNAME    TO  ADRS-PREFNAME
           MOVE    SPA-GMN-CITYNAME    TO  ADRS-CITYNAME
           MOVE    SPA-GMN-TOWNNAME    TO  ADRS-TOWNNAME
      *
      *    編集カナ住所
           MOVE    SPACE           TO  ADRS-EDITADRS-KANA
           STRING  ADRS-PREFKANA       DELIMITED  BY  SPACE
                   ADRS-CITYKANA       DELIMITED  BY  SPACE
                   ADRS-TOWNKANA       DELIMITED  BY  SPACE
                                       INTO    ADRS-EDITADRS-KANA
      *
      *    編集住所
           MOVE    SPACE           TO  ADRS-EDITADRS-NAME
           STRING  ADRS-PREFNAME       DELIMITED  BY  SPACE
                   ADRS-CITYNAME       DELIMITED  BY  SPACE
                   ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                       INTO    ADRS-EDITADRS-NAME
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "ADRS-KEY"          TO  ORC-DBPATH
           CALL    "ORCMCPSUB"             USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
           IF      MCP-RC              NOT =   ZERO
               MOVE    "1001"              TO  SPA-ERRCD
               GO  TO  490-INSERT-EXT
           END-IF
           .
       490-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    エラー
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *    メッセージ
           IF      SPA-N31IDCD     NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "N31    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  N31
      *
           MOVE    SPA-GMN-POST        TO  N31-POST
           MOVE    SPA-GMN-SELNUM      TO  N31-SELNUM
           MOVE    SPA-GMN-PREFCD-G    TO  N31-PREFCD
           MOVE    SPA-GMN-PREFKANA    TO  N31-PREFKANA
           MOVE    SPA-GMN-CITYKANA    TO  N31-CITYKANA
           MOVE    SPA-GMN-TOWNKANA    TO  N31-TOWNKANA
           MOVE    SPA-GMN-PREFNAME    TO  N31-PREFNAME
           MOVE    SPA-GMN-CITYNAME    TO  N31-CITYNAME
           MOVE    SPA-GMN-TOWNNAME    TO  N31-TOWNNAME
      *
      *    都道府県ＣＤコンボセット
           PERFORM VARYING IDXC    FROM    1   BY      1
                   UNTIL   IDXC    >       SPA-GMN-PREFCD-MAX
               MOVE    SPA-GMN-PREFCD-LIST(IDXC)
                                       TO  N31-PREFCD-ITEM(IDXC)
           END-PERFORM
           MOVE    SPA-GMN-PREFCD-MAX  TO  N31-PREFCD-COUNT
      *
      *    町域リスト
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL   IDXL    >       SPA-GMN-CYOIKI-MAX
               MOVE    SPA-GMN-CYO-HEN(IDXL)
                                       TO  N31-CYO-HEN(IDXL)
               MOVE    SPA-GMN-CYO-CD(IDXL)
                                       TO  N31-CYO-CD(IDXL)
               MOVE    SPA-GMN-CYO-POST(IDXL)
                                       TO  N31-CYO-POST(IDXL)
               MOVE    SPA-GMN-CYO-ADRS(IDXL)
                                       TO  N31-CYO-ADRS(IDXL)
               IF      IDXL     =   SPA-GMN-SELNUM
                   MOVE    "T"         TO  N31-CYOIKI-SELECT(IDXL)
               ELSE
                   MOVE    "F"         TO  N31-CYOIKI-SELECT(IDXL)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-CYOIKI-MAX  TO  N31-CYOIKI-COUNT
      *
      *    前頁ボタン非活性
           IF      SPA-NAI-BACK    =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  N31-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  N31-B06-STATE
           END-IF
      *
      *    次頁ボタン非活性
           IF      SPA-NAI-NEXT    =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  N31-B07-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  N31-B07-STATE
           END-IF
      *
      *    カーソルのセット
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
             WHEN      "0001"
               MOVE    "入力された郵便番号は登録されていません"
                                       TO  SPA-ERRMSG
             WHEN      "0002"
               MOVE    "範囲内の番号を選択してください"
                                       TO  SPA-ERRMSG
             WHEN      "0003"
               MOVE    "都道府県番号を入力してください"
                                       TO  SPA-ERRMSG
             WHEN      "0004"
               MOVE    "市町村名カナ入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0005"
               MOVE    "町域名カナ入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0006"
               MOVE    "市町村名入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0007"
               MOVE    "町域名入力エラー"
                                       TO  SPA-ERRMSG
             WHEN      "0011"
               MOVE    "郵便番号を正しく入力してください"
                                       TO  SPA-ERRMSG
             WHEN      "0012"
               MOVE    "連番は９９までしか登録できません"
                                       TO  SPA-ERRMSG
             WHEN      "0013"
               MOVE    "正しい都道府県番号を選択して下さい"
                                       TO  SPA-ERRMSG
             WHEN      "0014"
               MOVE    "この住所は削除が許可されていません"
                                       TO  SPA-ERRMSG
             WHEN      "1001"
               MOVE    "更新処理ができませんでした"
                                       TO  SPA-ERRMSG
             WHEN      "1002"
               MOVE    "削除処理ができませんでした"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N31ERR
           MOVE    SPA-ERRCD       TO  N31ERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  N31ERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "N31"           TO  SPA-MOTOPG
           MOVE    "N31ERR"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N31ERR"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
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
           MOVE    SPACE           TO  WRK-N31IDMSG
      *
           EVALUATE    SPA-N31IDCD
             WHEN      "1001"
               MOVE    "登録処理をします"  TO  WRK-N31IDMSG
             WHEN      "1002"
               MOVE    "削除処理をします"  TO  WRK-N31IDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  N31ID1
           MOVE    SPA-N31IDCD     TO  N31ID1-ID1CODE
           MOVE    WRK-N31IDMSG    TO  N31ID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "N31"           TO  SPA-MOTOPG
           MOVE    "N31ID1"        TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "N31ID1"        TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          EVALUATE    SPA-GMN-CUR
            WHEN      01
              MOVE    "POST"           TO   MCP-WIDGET
            WHEN      02
              MOVE    "SELNUM"         TO   MCP-WIDGET
            WHEN      03
              MOVE    "PREFCD"         TO   MCP-WIDGET
            WHEN      04
              MOVE    "CITYKANA"       TO   MCP-WIDGET
            WHEN      05
              MOVE    "TOWNKANA"       TO   MCP-WIDGET
            WHEN      06
              MOVE    "CITYNAME"       TO   MCP-WIDGET
            WHEN      07
              MOVE    "TOWNNAME"       TO   MCP-WIDGET
            WHEN      99
              MOVE    "B12"            TO   MCP-WIDGET
          END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理基本情報読込
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           MOVE    ZERO            TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    SPA-SYSYMD      TO  ORC-DBYMD
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
               CALL    "ORCMCPSUB"         USING
                                           MCPAREA
                                           ORCMCPAREA
                                           MCPDATA-REC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "SYSKANRI-KEY"  TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
      *
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスター読込
      *****************************************************************
       900-ADRS-SELECT-SEC         SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ADRS-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           .
      *
       900-ADRS-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスターFETCH
      *****************************************************************
       900-ADRS-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-ADRS
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           .
      *
       900-ADRS-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスターCLOSE
      *****************************************************************
       900-ADRS-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    WRK-DBPATH      TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           .
      *
       900-ADRS-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL   "ORCMCPSUB"          USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC.
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
