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
       PROGRAM-ID.             ORCGP026.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 特記事項情報（Ｐ０２６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/07/22    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.00    NACL-多々納  09/04/09  Ｈ２１．５対応
      *  04.05.00    NACL-多々納  09/07/28  Ｈ２１年１０月対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA            PIC 9(01).
           03  FLG-END             PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-N               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYM.
                   07  WRK-SYY         PIC 9(04).
                   07  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-NYUGAI-G.
               05  WRK-NYUGAIKBN       PIC X(01).
               05  WRK-NYUGAIF         PIC X(01).
               05  WRK-NYUGAIMEI       PIC X(18).
      *
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-SELNUM          PIC 9(04).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    特記事項一覧
       01  TBL-TOKKI-AREA.
           03  FILLER              PIC X(15)   VALUE   "01 ０１公".
           03  FILLER              PIC X(15)   VALUE   "02 ０２長".
           03  FILLER              PIC X(15)   VALUE   "03 ０３長処".
           03  FILLER              PIC X(15)   VALUE   "04 ０４後保".
           03  FILLER              PIC X(15)   VALUE   "07 ０７老併".
           03  FILLER              PIC X(15)   VALUE   "08 ０８老健".
           03  FILLER              PIC X(15)   VALUE   "09 ０９施".
           03  FILLER              PIC X(15)   VALUE   "10 １０第三".
           03  FILLER              PIC X(15)   VALUE   "11 １１薬治".
           03  FILLER              PIC X(15)   VALUE   "12 １２器治".
           03  FILLER              PIC X(15)   VALUE   "13 １３先進".
           03  FILLER              PIC X(15)   VALUE   "14 １４制超".
           03  FILLER              PIC X(15)   VALUE   "15 １５経過".
           03  FILLER              PIC X(15)   VALUE   "16 １６長２".
           03  FILLER              PIC X(15)   VALUE   "17 １７上位".
           03  FILLER              PIC X(15)   VALUE   "18 １８一般".
           03  FILLER              PIC X(15)   VALUE   "19 １９低所".
           03  FILLER              PIC X(15)   VALUE   "20 ２０二割".
           03  FILLER              PIC X(15)   VALUE   "21 ２１高半".
           03  FILLER              PIC X(15)   VALUE   "22 ２２多上".
           03  FILLER              PIC X(15)   VALUE   "23 ２３多一".
           03  FILLER              PIC X(15)   VALUE   "24 ２４多低".
           03  FILLER              PIC X(15)   VALUE   "25 ２５出産".
       01  TBL-TOKKI-AREA-R        REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI-OCC       OCCURS   23.
               05  TBL-TOKK-CD     PIC X(02).
               05  TBL-TOKK-K      PIC X(01).
               05  TBL-TOKK-NAIYO  PIC X(12).
       01  TBL-TOKK-MAX            PIC 9(02)   VALUE   23.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
       01  HZN-PARA-REC.
           COPY    "CPPARA.INC"
                              REPLACING  //PARA-// BY //HZN-PARA-//.
      *
      *   患者特記事項
       01  LNK-PTTOKKI-AREA.
           02  LNK-PTTOKKI-REC           OCCURS  50.
           COPY    "CPPTTOKKI.INC"
                            REPLACING  //PTTOKKI-// BY //LNK-PTTOKKI-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
      *
       01  SCRAREA.
           COPY    "P02.INC".
           COPY    "P02A.INC".
      *****COPY    "P02B.INC".
      *****COPY    "P02C.INC".
           COPY    "P02D.INC".
           COPY    "P02E.INC".
           COPY    "P02F.INC".
           COPY    "P014.INC".
           COPY    "P015.INC".
           COPY    "P98.INC".
           COPY    "P981.INC".
      *****COPY    "P99.INC".
           COPY    "P0201.INC".
           COPY    "P982.INC".
           COPY    "P100.INC".
           COPY    "P02X.INC".
           COPY    "P02S.INC".
           COPY    "P02Q.INC".
           COPY    "P02Q1.INC".
           COPY    "P02Y.INC".
           COPY    "P02Y1.INC".
           COPY    "P02G.INC".
           COPY    "P02G2.INC".
           COPY    "P02K.INC".
           COPY    "PERR.INC".
           COPY    "PID1.INC".
           COPY    "CPAPIMOD.INC".
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
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
           IF      SPA-FLG-END         =   ZERO
               PERFORM 5001-MAPCUR-SEC
      *********PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    パラメタファイル検索処理
      **** PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    初期画面編集
           PERFORM 310-SPASET-SEC
      *
           INITIALIZE                  SPA-GMN26-INAREA
           MOVE    1                   TO  SPA-GMN26-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
      *320-PARAFILE-KENSAKU-SEC             SECTION.
      *
      *    INITIALIZE                  LNK-PTTOKKI-AREA
      *
      *    INITIALIZE                      PARA-REC
      *    MOVE    "P02"               TO  PARA-GYOUMUID
      *    MOVE    SPA-TERMID          TO  PARA-TERMID
      *    MOVE    "PTTOKKI"           TO  PARA-FILEMEI
      *
      *    MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
      *    MOVE    PARA-REC            TO  MCPDATA-REC
      *    MOVE    "tbl_para"          TO  MCP-TABLE
      *    MOVE    "key5"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_para"          TO  MCP-TABLE
      *        MOVE    "key5"              TO  MCP-PATHNAME
      *        PERFORM 990-PARA-READ-SEC
      *    ELSE
      *        INITIALIZE                  PARA-REC
      *        MOVE    1               TO  FLG-PARA
      *    END-IF
      *
      *    MOVE    ZERO                TO  WRK-TOK-MAX
      *    PERFORM
      *        UNTIL   FLG-PARA            =   1
      *        ADD     1                   TO  WRK-TOK-MAX
      *        IF      WRK-TOK-MAX         <=  50
      *            MOVE    PARA-DATA-REC   TO  LNK-PTTOKKI-REC
      *                                                  (WRK-TOK-MAX)
      *        END-IF
      *        MOVE    "tbl_para"          TO  MCP-TABLE
      *        MOVE    "key5"              TO  MCP-PATHNAME
      *        PERFORM 990-PARA-READ-SEC
      *    END-PERFORM
      *    MOVE    "tbl_para"          TO  MCP-TABLE
      *    MOVE    "key5"              TO  MCP-PATHNAME
      **** PERFORM 990-DBCLOSE-SEC
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    初期編集内容
           INITIALIZE                  SPA-GMN26-INIT
           MOVE    "0 入院・入院外"    TO  SPA-GMN26-NYUGAI-LST (1)
           MOVE    "1 入院"            TO  SPA-GMN26-NYUGAI-LST (2)
           MOVE    "2 入院外"          TO  SPA-GMN26-NYUGAI-LST (3)
           MOVE    3                   TO  SPA-GMN26-NYUMAX
      *    特記事項
           MOVE    ZERO                TO  SPA-GMN26-TOKMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-TOKK-MAX
               MOVE    TBL-TOKKI-OCC (IDX) TO  SPA-GMN26-TOKCD-LST (IDX)
               MOVE    IDX                 TO  SPA-GMN26-TOKMAX
           END-PERFORM
      *
      *    一覧表変換編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN26-TMAX
               MOVE    SPA-NAI26-TNYUGIKBN (IDX)   TO  WRK-NYUGAIKBN
               PERFORM 3101-NYUGAIKBN-HEN-SEC
               MOVE    WRK-NYUGAIMEI           TO  SPA-GMN26-TNYUGI(IDX)
      *
               MOVE    SPA-NAI26-TSTYM (IDX)   TO  WRK-SYM
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD(1:7)         TO  SPA-GMN26-TSTYM (IDX)
      *
               MOVE    SPA-NAI26-TEDYM (IDX)   TO  WRK-SYM
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD(1:7)         TO  SPA-GMN26-TEDYM (IDX)
           END-PERFORM
      *
      *    前回の選択番号は反映させない
           INITIALIZE                  SPA-GMN26-INAREA
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分設定処理
      *****************************************************************
       3101-NYUGAIKBN-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NYUGAIMEI
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      WRK-NYUGAIKBN   =   SPA-GMN26-NYUGAI-LST (IDX-N)
                                                              (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)
                                               TO  WRK-NYUGAI-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           .
       3101-NYUGAIKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN26-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN26-CUR
               WHEN    01
                   MOVE    "P026_SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P026_NYUGAI"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P026_CD"            TO  MCP-WIDGET
               WHEN    04
                   MOVE    "P026_NAIYO"         TO  MCP-WIDGET
               WHEN    05
                   MOVE    "P026_STYM"          TO  MCP-WIDGET
               WHEN    06
                   MOVE    "P026_EDYM"          TO  MCP-WIDGET
               WHEN    07
                   MOVE    "P026_TOKCD"         TO  MCP-WIDGET
               WHEN    10
                   MOVE    "BTO2"                TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P026_SELNUM"
                   MOVE    2           TO  SPA-GMN26-CUR
               WHEN    "P026_NYUGAI"
                   MOVE    3           TO  SPA-GMN26-CUR
               WHEN    "P026_CD"
                   MOVE    4           TO  SPA-GMN26-CUR
               WHEN    "P026_NAIYO"
                   MOVE    5           TO  SPA-GMN26-CUR
               WHEN    "P026_STYM"
                   MOVE    6           TO  SPA-GMN26-CUR
               WHEN    "P026_EDYM"
                   MOVE    10          TO  SPA-GMN26-CUR
               WHEN    "P026_TOKCD"
                   MOVE    5           TO  SPA-GMN26-CUR
           END-EVALUATE
      *
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    更新
               WHEN    "CLICKED"       ALSO    "BTO2"
                   PERFORM 411-KOUSIN-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "BTO1"
                   PERFORM 412-DELETE-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN26-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       411-KOUSIN-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =    SPACE
               IF      SPA-GMN26-SELNUM    =   ZERO
                   IF      SPA-GMN26-TMAX      >=  50
                       MOVE    "2620"          TO  SPA-ERRCD
                   ELSE
                       ADD     1                   TO  SPA-GMN26-TMAX
                       MOVE    SPA-GMN26-TMAX      TO  IDX
                   END-IF
               ELSE
                   MOVE    SPA-GMN26-SELNUM    TO  IDX
               END-IF
           END-IF
           IF      SPA-ERRCD           =    SPACE
               MOVE    SPA-GMN26-NYUGAI    TO  SPA-NAI26-TNYUGIKBN(IDX)
               MOVE    SPA-GMN26-NYUGAI-MEI
                                           TO  SPA-GMN26-TNYUGI (IDX)
               MOVE    SPA-GMN26-CD        TO  SPA-GMN26-TCD  (IDX)
               MOVE    SPA-GMN26-NAIYO     TO  SPA-GMN26-TNAIYO (IDX)
               MOVE    SPA-GMN26-STYM      TO  SPA-GMN26-TSTYM (IDX)
               MOVE    SPA-GMN26-EDYM      TO  SPA-GMN26-TEDYM (IDX)
               MOVE    SPA-NAI26-STYM      TO  SPA-NAI26-TSTYM (IDX)
               MOVE    SPA-NAI26-EDYM      TO  SPA-NAI26-TEDYM (IDX)
      *
               INITIALIZE                  SPA-GMN26-INAREA
               MOVE    1                   TO  SPA-GMN26-CUR
           END-IF
      *
           .
       411-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       412-DELETE-SEC          SECTION.
      *
           IF       SPA-GMN26-SELNUM   NOT =   ZERO
               MOVE    SPA-GMN26-SELNUM    TO  IDX
               INITIALIZE                  SPA-GMN26-TOKK-OCC (IDX)
      *        空白行削除
               PERFORM 4121-LIST-HEN-SEC
               INITIALIZE                  SPA-GMN26-INAREA
               MOVE    1                   TO  SPA-GMN26-CUR
           END-IF
      *
           .
       412-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    空白行削除処理
      *****************************************************************
       4121-LIST-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO   SPA-GMN26-TMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
              IF     (SPA-GMN26-TNYUGI (IDX)   =   SPACE )
                 AND (IDX              <   50   )
                   MOVE    SPA-GMN26-TOKK-OCC (IDX + 1)
                                       TO  SPA-GMN26-TOKK-OCC (IDX)
                   INITIALIZE              SPA-GMN26-TOKK-OCC (IDX + 1)
              END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
              IF     (SPA-GMN26-TNYUGI (IDX)   NOT =   SPACE )
                   ADD     1               TO  SPA-GMN26-TMAX
                   MOVE    SPA-GMN26-TMAX  TO  SPA-GMN26-TNUM (IDX)
              END-IF
           END-PERFORM
           .
       4121-LIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    リスト選択
               WHEN    ANY             ALSO    "P026_TOKLST"
                   PERFORM 41012-TOKLST-SYORI-SEC
      *    番号選択
               WHEN    "ACTIVATE"      ALSO    "P026_SELNUM"
                   PERFORM 41013-SELNUM-SEC
               WHEN    OTHER
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN26-CUR
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記事項一覧選択
      *****************************************************************
       41012-TOKLST-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN26-TMAX
               IF      P02-P026-TOK-SELECT (IDX)  =   "T"
                   IF      IDX             =   SPA-GMN26-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX         TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN26-SELNUM
           END-IF
      *
           IF      SPA-GMN26-SELNUM    >   ZERO
               MOVE    SPA-GMN26-SELNUM    TO   P02-P026-SELNUM
               PERFORM 41013-SELNUM-SEC
           END-IF
      *
           .
       41012-TOKLST-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41013-SELNUM-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN26-INAREA
      *
           MOVE    P02-P026-SELNUM     TO  SPA-GMN26-SELNUM
      *
           IF      SPA-GMN26-SELNUM    >   SPA-GMN26-TMAX
               MOVE    "2601"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN26-CUR
           ELSE
               IF      SPA-GMN26-SELNUM        =   ZERO
                   MOVE    SPA-GMN26-NYUGAI-LST (1)
                                               TO  SPA-GMN26-NYUGAI-G
               ELSE
                   PERFORM 410131-TOKKI-HEN-SEC
               END-IF
           END-IF
      *
      *
           .
       41013-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    特記内容編集処理
      *****************************************************************
       410131-TOKKI-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN26-SELNUM    TO  IDX
      *
           MOVE    SPA-NAI26-TNYUGIKBN (IDX)    TO  WRK-NYUGAIKBN
           PERFORM 3101-NYUGAIKBN-HEN-SEC
           MOVE    WRK-NYUGAI-G            TO  SPA-GMN26-NYUGAI-G
      *
           MOVE    SPA-GMN26-TCD   (IDX)   TO  SPA-GMN26-CD
           MOVE    SPA-GMN26-TNAIYO(IDX)   TO  SPA-GMN26-NAIYO
           MOVE    SPA-GMN26-TSTYM (IDX)   TO  SPA-GMN26-STYM
           MOVE    SPA-GMN26-TEDYM (IDX)   TO  SPA-GMN26-EDYM
      *
           MOVE    SPA-NAI26-TSTYM (IDX)   TO  SPA-NAI26-STYM
           MOVE    SPA-NAI26-TEDYM (IDX)   TO  SPA-NAI26-EDYM
           MOVE    SPA-GMN26-TCD   (IDX)   TO  SPA-MAE26-CD
           .
       410131-TOKKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU     NOT =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02-P026-NYUGAI     TO  SPA-GMN26-NYUGAI-G
           MOVE    P02-P026-CD         TO  SPA-GMN26-CD
           MOVE    P02-P026-NAIYO      TO  SPA-GMN26-NAIYO
           MOVE    P02-P026-STYM       TO  SPA-GMN26-STYM
           MOVE    P02-P026-EDYM       TO  SPA-GMN26-EDYM
      *
           MOVE    P02-P026-TOKCD      TO  SPA-GMN26-TOKCD-G
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    入外区分
           PERFORM 41012-NYUGAI-CHK-SEC
      *    レセ電
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CD-CHK-SEC
           END-IF
      *    紙レセプト
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-NAIYO-CHK-SEC
           END-IF
      *    特記リスト
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-TOKCD-G   NOT =   SPACE
                   PERFORM 41012-TOKCD-CHK-SEC
               END-IF
           END-IF
      *    開始年月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-STYM-CHK-SEC
           END-IF
      *    開始年月
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-EDYM-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック処理
      *****************************************************************
       41012-NYUGAI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-N   FROM    1   BY  1
                   UNTIL      (IDX-N   >   SPA-GMN26-NYUMAX )
                          OR  (FLG-OK  =   1                )
               IF      SPA-GMN26-NYUGAI    =   SPA-GMN26-NYUGAI-LST
                                                      (IDX-N) (1:1)
                   MOVE    SPA-GMN26-NYUGAI-LST (IDX-N)
                                               TO  SPA-GMN26-NYUGAI-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2602"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN26-CUR
           END-IF
           .
       41012-NYUGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセ電区分チェック処理
      *****************************************************************
       41012-CD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-CD        =   SPACE
               CONTINUE
           ELSE
      *        数値チェック
               INITIALIZE                       ORCSNUMAREA
               MOVE    SPA-GMN26-CD         TO  SNUM-INX
               CALL    "ORCSNUM"                USING
                                                ORCSNUMAREA
               IF     (SNUM-RC          NOT =   ZERO  )   OR
                      (SNUM-MINKBN      NOT =   SPACE )   OR
                      (SNUM-SYOKBN      NOT =   SPACE )
                   MOVE    "2603"           TO  SPA-ERRCD
                   MOVE    3                TO  SPA-GMN26-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)TO  SPA-GMN26-CD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-CD    NOT =   SPA-MAE26-CD )
                  AND (SPA-GMN26-CD    NOT =   SPACE     )
                   PERFORM 410121-TOKCD-HEN-SEC
               END-IF
               MOVE    SPA-GMN26-CD        TO  SPA-MAE26-CD
           END-IF
      *!!!
      *H21.5 特記追加分の入外区分設定
           IF      SPA-GMN26-CD        =   "22"
                                       OR  "23"
                                       OR  "24"
                                       OR  "25"
      *        入院
               MOVE    SPA-GMN26-NYUGAI-LST (2)
                                           TO  SPA-GMN26-NYUGAI-G
           END-IF
      *!!!
           .
       41012-CD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記内容編集
      *****************************************************************
       410121-TOKCD-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T       >   SPA-GMN26-TOKMAX )
                          OR  (FLG-OK      =   1                )
               IF      SPA-GMN26-CD        =   SPA-GMN26-TOKCDL (IDX-T)
                   MOVE    SPA-GMN26-NAIYOL (IDX-T)
                                               TO  SPA-GMN26-NAIYO
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           .
       410121-TOKCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    紙レセプト内容チェック処理
      *****************************************************************
       41012-NAIYO-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-NAIYO     =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN26-NAIYO     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN26-NAIYO
               IF     (KANACHK-RC      NOT =   ZERO  ) 
                   MOVE    "2604"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN26-CUR
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN26-NAIYO
               END-IF
           END-IF
           .
       41012-NAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特記内容編集処理
      *****************************************************************
       41012-TOKCD-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T       >   SPA-GMN26-TOKMAX )
                          OR  (FLG-OK      =   1                )
               IF      SPA-GMN26-TOKCD     =   SPA-GMN26-TOKCDL (IDX-T)
                   MOVE    SPA-GMN26-TOKCDL (IDX-T)
                                               TO  SPA-GMN26-CD
                   MOVE    SPA-GMN26-NAIYOL (IDX-T)
                                               TO  SPA-GMN26-NAIYO
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "2607"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN26-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN26-TOKCD-G
           END-IF
           .
       41012-TOKCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始年日チェック処理
      *****************************************************************
       41012-STYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-STYM      =   SPACE
               MOVE    ZERO                TO  SPA-NAI26-STYM
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN26-STYM      TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN26-STYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI26-STYM
               ELSE
                   MOVE    "2605"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *H21.5 特記追加分
           IF     (SPA-GMN26-CD        =   "22"
                                       OR  "23"
                                       OR  "24"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "200905"
                   MOVE    "2612"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *!!!
      *H21.10 特記追加分
           IF     (SPA-GMN26-CD        =   "25"  )
           AND    (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI26-STYM  NOT =   ZERO  )
               IF      SPA-NAI26-STYM      <   "200910"
                   MOVE    "2613"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *!!!
      *
           .
       41012-STYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了年日チェック処理
      *****************************************************************
       41012-EDYM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN26-EDYM      =   SPACE
               MOVE    ZERO                TO  SPA-NAI26-EDYM
               IF      SPA-GMN26-STYM      NOT =   SPACE
                   MOVE    "999999"            TO  SPA-GMN26-EDYM
               END-IF
           END-IF
           IF      SPA-GMN26-EDYM      =   "999999"
               MOVE    ALL "9"             TO  SPA-NAI26-EDYM
           END-IF
           IF      SPA-GMN26-EDYM      =   SPACE
                                       OR  "999999"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN26-EDYM      TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN26-EDYM
                   MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI26-EDYM
               ELSE
                   MOVE    "2606"              TO  SPA-ERRCD
                   MOVE    06                  TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           .
       41012-EDYM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
      *    必須チェック
           IF      SPA-GMN26-NAIYO     =   SPACE
               MOVE    "2608"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN26-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN26-STYM      =   SPACE )
                   OR (SPA-GMN26-EDYM      =   SPACE )
                   MOVE    "2609"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN26-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-NAI26-STYM      >   SPA-NAI26-EDYM)
                   MOVE    "2610"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN26-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN26-CD    NOT =   SPACE
      *            レセ電コードの重複チェック
                   PERFORM 41091-JYUFUKU-CHK-SEC
               END-IF
           END-IF
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセ電コードの重複チェック処理
      ****************************************************************
       41091-JYUFUKU-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN26-TMAX )
                           OR (SPA-ERRCD   NOT =   SPACE  )
               IF      SPA-GMN26-CD    =   SPA-GMN26-TCD (IDX)
                   IF     (SPA-GMN26-SELNUM    NOT =   ZERO )
                     AND  (SPA-GMN26-SELNUM        =   IDX  )
                       CONTINUE
                   ELSE
                       IF     (SPA-NAI26-STYM  <   SPA-NAI26-TSTYM(IDX))
                         OR   (SPA-NAI26-EDYM  >   SPA-NAI26-TEDYM(IDX))
                           CONTINUE
                       ELSE
                           MOVE    "2611"          TO  SPA-ERRCD
                           MOVE    6               TO  SPA-GMN26-CUR
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       41091-SYOTOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       800-SE-WAFORMAT-YMDEDIT-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMD
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-SYM
               WHEN    ZERO
               WHEN    SPACE
                   MOVE    ALL "0"         TO  WRK-HENYMD
               WHEN    "999999"
                   MOVE    "999999 "       TO  WRK-HENYMD(1:7)
               WHEN    OTHER
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYM             TO  LNK-DAY2-YMD(1:6)
                   MOVE    "01"                TO  LNK-DAY2-YMD(7:2)
                   CALL    "ORCSDAY"           USING
                                                   STS-AREA-DAY
                                                   LNK-DAY2-AREA
                   IF    STS-DAY-RC1           =   ZERO
                      MOVE    LNK-DAY2-EDTYMD1     TO  WRK-HENYMD
                   ELSE
                      MOVE    SPACE                TO  WRK-HENYMD
                      MOVE    1                    TO  FLG-ERR
                   END-IF
           END-EVALUATE
           .
       800-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  HZN-PARA-REC
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HZN-PARA-REC        TO  PARA-REC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
