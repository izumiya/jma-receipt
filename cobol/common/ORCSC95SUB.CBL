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
       PROGRAM-ID.                 ORCSC95SUB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 初期共通ＳＰＡ編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/09/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL=多々納  12/03/XX  平成２４年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1007.INC".
      *
           COPY    "CPSK1008.INC".
      *
           COPY    "CPSK1038.INC".
      *
           COPY    "CPSK1030.INC".
      *
           COPY    "CPSK1014.INC".
      *    ＣＬＡＩＭ接続情報 
           COPY    "CPSK9000.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    初期ＳＰＡ編集処理パラメタ
           COPY    "CPORCSC95.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSC95AREA
           SPA-AREA
           SPA-K01KYOUTU
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           EVALUATE    LNK-SC95-KBN
      *        初期共通ＳＰＡ編集
               WHEN    "1"
               WHEN    "9"
                   PERFORM 100-K01KYOUTU-SEC
      *        訂正時診療日変更
               WHEN    "4"
                   PERFORM 200-TEISEISYORI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    共通ＳＰＡ編集処理
      *****************************************************************
       100-K01KYOUTU-SEC            SECTION.
      *
      *    システム管理検索（医療機関情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        請求書発行
               MOVE    SYS-1001-SKYPRTFLG  TO  SPA-SKYPRTFLG-SYS
      *        診療費明細書発行
               MOVE    SYS-1001-MEIPRTFLG  TO  SPA-MEIPRTFLG
      *        院外処方せん発行フラグ
               MOVE    SYS-1001-SYOHOPRTFLG    TO  SPA-SYOHOPRTFLG
      *        前回処方表示フラグ
               MOVE    SYS-1001-ZENSYOHOFLG    TO  SPA-ZENSYOHOFLG
      *        処方区分
               MOVE    SYS-1001-SYOHOKBN       TO  SPA-SYOHOKBN
      *
               IF      SPA-NYUGAIKBN       =   "1"
      *            前回処方表示フラグ（しないとする）
                   MOVE    "1"                     TO  SPA-ZENSYOHOFLG
      *            処方区分（すべて院内とする）
                   MOVE    "0"                     TO  SPA-SYOHOKBN
               END-IF
      *
      *        病院種別
               MOVE    SYS-1001-HOSPSBT        TO  SPA-HOSPSBT
               IF      SPA-BEDKBN              =   ZERO
                   CONTINUE
               ELSE
      *        病床数
                   MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
      *        病床数（一般）
                   IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                          (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                       MOVE    ZERO                TO  SPA-BEDSUIPN
                   ELSE
                       MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
                   END-IF
               END-IF
      *
      *        薬剤情報発行フラグ
               MOVE    SYS-1001-YAKUJYOPRTFLG  TO  SPA-YAKJYOPRTFLG
               IF      SYS-1001-YAKUJYOPRTFLG  =   SPACE
                   MOVE    ZERO                TO  SPA-YAKJYOPRTFLG
               END-IF
      *        お薬手帳発行フラグ
               MOVE    SYS-1001-OKUSURIPRTFLG  TO  SPA-OKUSURIPRTFLG
               IF      SYS-1001-OKUSURIPRTFLG  =   SPACE
                   MOVE    ZERO                TO  SPA-OKUSURIPRTFLG
               END-IF
      *        予約票帳発行フラグ
               MOVE    SYS-1001-YYKHYOPRTFLG   TO  SPA-YYKHYOPRTFLG
               IF      SYS-1001-YYKHYOPRTFLG   =   SPACE
                   MOVE    ZERO                TO  SPA-YYKHYOPRTFLG
               END-IF
      *        消費税端数設定
               MOVE    SYS-1001-TAX-HASU-KBN   TO  SPA-TAX-HASU-KBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    職員情報より発行フラグ再設定
           IF     (LNK-SC95-PG         =   "K02" )
              AND (SPA-NYUGAIKBN       =   "2"   )
               PERFORM 1001-PRTFLG-HEN-SEC
           END-IF
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
      *
      *        相互作用チェック期間
               MOVE    SYS-1008-INTERACT-KIKAN TO  SPA-INTERACT-KIKAN
      *
      *        外来管理加算チェック
               MOVE    SYS-1008-GAIKANRIKBN-CHK
                                           TO  SPA-GAIKANRIKBN-CHK
      *        初期は、チェックありとする
               IF      SYS-1008-GAIKANRIKBN-CHK    =   SPACE
                   MOVE    "1"             TO  SPA-GAIKANRIKBN-CHK
               END-IF
      *
      *        育児栄養加算自動算定
               MOVE    SYS-1008-IKUJIEIYO-FLG
                                           TO  SPA-IKUJIEIYO-FLG
      *        育児栄養加算自動算定科
               MOVE    SYS-1008-IKUJIEIYO-SRYKA
                                           TO  SPA-IKUJIEIYO-SRYKA
      *        外来カーソル移動
               MOVE    SYS-1008-GAI-CURSOR-SET-1
                                           TO  SPA-GAI-CURSOR-SET
      *        入院初診料自動発生
               MOVE    SYS-1008-NYU-SYOSIN
                                           TO  SPA-NYU-SYOSIN
      *        数量ゼロ入力
               IF      SYS-1008-SURYOZERO-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-SURYOZERO-FLG
               ELSE
                   MOVE    SYS-1008-SURYOZERO-FLG
                                           TO  SPA-SURYOZERO-FLG
               END-IF
      *        入院調剤技術基本料チェック
               IF      SYS-1008-CHOKICHK-FLG   =   SPACE
                   MOVE    ZERO            TO  SPA-CHOKICHK-FLG
               ELSE
                   MOVE    SYS-1008-CHOKICHK-FLG
                                           TO  SPA-CHOKICHK-FLG
               END-IF
      *        時間外加算特例乳幼児
               IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-JIKANTOKU-NYU
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                           TO  SPA-JIKANTOKU-NYU
               END-IF
      *        画像診断管理加算１（写真診断）
               IF      SYS-1008-GAZOUKSN1-FLG  =   SPACE
                   MOVE    "1"             TO  SPA-GAZOUKSN1
               ELSE
                   MOVE    SYS-1008-GAZOUKSN1-FLG
                                           TO  SPA-GAZOUKSN1
               END-IF
      *        最終来院日からの期間
               IF      SYS-1008-LAST-KIKAN-X   =   SPACE
                   MOVE    ZERO            TO  SPA-LAST-KIKAN
               ELSE
                   MOVE    SYS-1008-LAST-KIKAN
                                           TO  SPA-LAST-KIKAN
               END-IF
      *        時間外加算乳幼児チェック
               IF      SYS-1008-JIKANTOKU-CHK  =   SPACE
                   MOVE    ZERO            TO  SPA-JIKANTOKU-CHK
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-CHK
                                           TO  SPA-JIKANTOKU-CHK
               END-IF
               IF      SPA-JIKANTOKU-CHK   =   "1"
      *            曜日により判定
                   IF      SPA-SYSYMD-YOBI     =   "6"
      *                土曜
                       MOVE    SYS-1008-JIKANCHK-SAT (1)
                                               TO  SPA-JIKANCHK-TIME (1)
                       MOVE    SYS-1008-JIKANCHK-SAT (2)
                                               TO  SPA-JIKANCHK-TIME (2)
                   ELSE
      *                土曜以外
                       MOVE    SYS-1008-JIKANCHK-WEEK(1)
                                               TO  SPA-JIKANCHK-TIME (1)
                       MOVE    SYS-1008-JIKANCHK-WEEK (2)
                                               TO  SPA-JIKANCHK-TIME (2)
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-JIKANCHK-TIME-G
               END-IF
      *        患者取消確認チェック
               IF      SYS-1008-CLEARCHK-FLG   =   SPACE
                   MOVE    ZERO            TO  SPA-CLEARCHK-FLG
               ELSE
                   MOVE    SYS-1008-CLEARCHK-FLG
                                           TO  SPA-CLEARCHK-FLG
               END-IF
      *        中途終了展開時の自動発生
               IF      SYS-1008-CHUTOTENKAI-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-CHUTOTENKAI-FLG
               ELSE
                   MOVE    SYS-1008-CHUTOTENKAI-FLG
                                           TO  SPA-CHUTOTENKAI-FLG
               END-IF
      *        薬剤情報提供料チェック
               IF      SYS-1008-YAKJYOCHK-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-YAKJYOCHK-FLG
               ELSE
                   MOVE    SYS-1008-YAKJYOCHK-FLG
                                           TO  SPA-YAKJYOCHK-FLG
               END-IF
      *        訂正時の自動発生
      *        外来
               IF      SPA-NYUGAIKBN       =   "2"
                   IF      SYS-1008-TEISEIAUTO-FLG   =   SPACE
                       MOVE    "1"             TO  SPA-TEISEI-FLG
                   ELSE
                       MOVE    SYS-1008-TEISEIAUTO-FLG
                                               TO  SPA-TEISEI-FLG
                   END-IF
               ELSE
      *        入院
                   IF      SYS-1008-TEINYUAUTO-FLG =   SPACE
      *            未設定は外来から
                       IF      SYS-1008-TEISEIAUTO-FLG =   SPACE
                           MOVE    "1"             TO  SPA-TEISEI-FLG
                       ELSE
                           MOVE    SYS-1008-TEISEIAUTO-FLG
                                                   TO  SPA-TEISEI-FLG
                       END-IF
                   ELSE
                       MOVE    SYS-1008-TEINYUAUTO-FLG
                                               TO  SPA-TEISEI-FLG
                   END-IF
               END-IF
      *        訂正時の自動発生（特定疾患処方管理）
               IF      SYS-1008-TEISEITOKU-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-TEISEITOKU-FLG
               ELSE
                   MOVE    SYS-1008-TEISEITOKU-FLG
                                           TO  SPA-TEISEITOKU-FLG
               END-IF
      *        平成１７年４月以降有効とする
               IF      SPA-SRYYMD          <   "20050401"
                   MOVE    "1"             TO  SPA-YAKJYOCHK-FLG
                   MOVE    "1"             TO  SPA-TEISEI-FLG
                   MOVE    "0"             TO  SPA-TEISEITOKU-FLG
               END-IF
      *        病名表示区分
               IF      SYS-1008-BYOHYOKBN-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-BYOHYOKBN-FLG
               ELSE
                   MOVE    SYS-1008-BYOHYOKBN-FLG
                                           TO  SPA-BYOHYOKBN-FLG
               END-IF
      *        検体検査加算自動発生
               IF      SYS-1008-KENSAAUTO-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-KENSAAUTO-FLG
               ELSE
                   MOVE    SYS-1008-KENSAAUTO-FLG
                                           TO  SPA-KENSAAUTO-FLG
               END-IF
      *        前回保険組合せ相違チェック
               IF      SYS-1008-ZENHKNCHK-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-ZENHKNCHK-FLG
               ELSE
                   MOVE    SYS-1008-ZENHKNCHK-FLG
                                           TO  SPA-ZENHKNCHK-FLG
               END-IF
      *        病名疾患区分からの自動発生
               IF      SYS-1008-BYOMEIAUTO-FLG   =   SPACE
      *            未設定は算定する
                   MOVE    "1"             TO  SPA-BYOMEIAUTO-FLG
               ELSE
                   MOVE    SYS-1008-BYOMEIAUTO-FLG
                                           TO  SPA-BYOMEIAUTO-FLG
               END-IF
      *        入院時初回加算の自動発生
               IF      SYS-1008-NYUKENSA-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-NYUKENSA-FLG
               ELSE
                   MOVE    SYS-1008-NYUKENSA-FLG
                                           TO  SPA-NYUKENSA-FLG
               END-IF
      *        診察料自動発生
               IF      SYS-1008-INITAUTO-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-INITAUTO-FLG
               ELSE
                   MOVE    SYS-1008-INITAUTO-FLG
                                           TO  SPA-INITAUTO-FLG
               END-IF
      *        手帳加算確認チェック
               IF      SYS-1008-TECHOKBN-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-TECHOKBN-FLG
               ELSE
                   MOVE    SYS-1008-TECHOKBN-FLG
                                           TO  SPA-TECHOKBN-FLG
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-INTERACT-KIKAN
               MOVE    "1"                 TO  SPA-GAIKANRIKBN-CHK
      *
               MOVE    ZERO                TO  SPA-IKUJIEIYO-FLG
               MOVE    SPACE               TO  SPA-IKUJIEIYO-SRYKA
               MOVE    ZERO                TO  SPA-GAI-CURSOR-SET
               MOVE    ZERO                TO  SPA-NYU-SYOSIN
               MOVE    ZERO                TO  SPA-SURYOZERO-FLG
               MOVE    ZERO                TO  SPA-CHOKICHK-FLG
               MOVE    ZERO                TO  SPA-JIKANTOKU-NYU
               MOVE    "1"                 TO  SPA-GAZOUKSN1
               MOVE    ZERO                TO  SPA-LAST-KIKAN
               MOVE    ZERO                TO  SPA-JIKANCHK-TIME-G
               MOVE    ZERO                TO  SPA-JIKANTOKU-CHK
               MOVE    ZERO                TO  SPA-CLEARCHK-FLG
               MOVE    "1"                 TO  SPA-CHUTOTENKAI-FLG
               MOVE    "1"                 TO  SPA-YAKJYOCHK-FLG
               MOVE    "1"                 TO  SPA-TEISEI-FLG
               MOVE    "0"                 TO  SPA-TEISEITOKU-FLG
               MOVE    "1"                 TO  SPA-BYOHYOKBN-FLG
      *        検体検査加算自動発生
               MOVE    "0"                 TO  SPA-KENSAAUTO-FLG
               MOVE    "0"                 TO  SPA-ZENHKNCHK-FLG
               MOVE    "1"                 TO  SPA-BYOMEIAUTO-FLG
               MOVE    "0"                 TO  SPA-NYUKENSA-FLG
      *        診察料自動発生
               MOVE    "1"                 TO  SPA-INITAUTO-FLG
               MOVE    "0"                 TO  SPA-TECHOKBN-FLG
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診察料自動発生なしは、時間外加算なし
           IF      SPA-INITAUTO-FLG    =   "0"
               MOVE    0                    TO  SPA-GAI-JIKANKBN
           END-IF
      *
      *    診療科
           INITIALIZE                  SPA-GMN-SRYKA-AREA
      *
           MOVE    SPACE               TO  SYS-KEY
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKAMEIL(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入外区分
      ***  MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    分散
           MOVE    "0"                 TO  SPA-BUNSAN
      *    院外処方区分
           MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
      *
      *    施設基準検索
           PERFORM 1009-SISETU-KIJUN-SEC
      *    ステム管理共通（1007,1038)
           PERFORM 1002-SYSKANRI-KYOTU-SEC
      *
      *    包括診療行為算定
           PERFORM 10010-HKTSANTEI-SEC
      *
      *    ＣＬＡＩＭ展開の診察料変換
           IF     (SPA-NYUGAIKBN       =   "2"   )
               PERFORM 10011-CLAIM-HEN-SEC
           END-IF
           .
       100-K01KYOUTU-EXT.
           EXIT.
      *****************************************************************
      *    システム管理診療行為情報区分編集処理
      *****************************************************************
       1002-SYSKANRI-KYOTU-SEC         SECTION.
      *
      *    システム管理検索
           MOVE    SPACE               TO  SYS-1007-REC
           INITIALIZE                  SYS-1007-REC
           MOVE    "1007"              TO  SYS-1007-KANRICD
           MOVE    "*"                 TO  SYS-1007-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1007-HOSPNUM
           MOVE    SYS-1007-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1007-REC
               INITIALIZE                  SYS-1007-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1007-REC
      *
               MOVE    SYS-1007-SANTEI (1:1)   TO  SPA-CHOKI-FLG
               MOVE    SYS-1007-SANTEI (2:1)   TO  SPA-YAKJYO-1
               MOVE    SYS-1007-SANTEI (3:1)   TO  SPA-YAKJYO-2
               MOVE    SYS-1007-SANTEI (4:1)   TO  SPA-YAKJYO-3
      *        療養担当手当
               MOVE    SYS-1007-SANTEI (5:1)   TO  SPA-RYOYOU-FLG
      *        特定薬剤治療管理料有無
               MOVE    SYS-1007-SANTEI (6:1)   TO  SPA-TOKUTEI-FLG
      *        平成１７年４月以降有効とする
               IF      SPA-SRYYMD          <   "20050401"
      *            薬剤情報提供料
                   IF      SPA-YAKJYO-1    =   2
                       MOVE    1               TO  SPA-YAKJYO-1
                   END-IF
                   IF      SPA-YAKJYO-2    =   2
                       MOVE    1               TO  SPA-YAKJYO-2
                   END-IF
                   IF      SPA-YAKJYO-3    =   2
                       MOVE    1               TO  SPA-YAKJYO-3
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    システム管理検索（診療行為一括入金管理情報）
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1038-REC
               INITIALIZE                  SYS-1038-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1038-REC
      *        複数科まとめ集計
               MOVE    SYS-1038-KAGRPFLG       TO  SPA-KAGRPFLG
      *        ＤＯ初期表示
               MOVE    SYS-1038-DOSRYKBNFLG    TO  SPA-DOSRYKBNFLG
      *        中途表示一覧
               MOVE    SYS-1038-WKSRYHYOFLG    TO  SPA-WKSRYHYOFLG
      *        同日再診・訂正選択
               MOVE    SYS-1038-TEISEIFLG      TO  SPA-TEISEIFLG
      *        皮下筋肉注射料変換選択
               MOVE    SYS-1038-CHG310FLG      TO  SPA-CHG310FLG
      *        腫瘍マーカ一覧表示
               MOVE    SYS-1038-AKUSEIHYOFLG   TO  SPA-AKUSEIHYOFLG
      *        投薬診療区分変換選択
               MOVE    SYS-1038-CHGSRYSYU-FLG  TO  SPA-CHGSRYSYU-FLG
      *        残量廃棄区分
               MOVE    SYS-1038-HAIKIKBN       TO  SPA-HAIKIKBN-FLG
      *        精神療法２０未満加算区分
               MOVE    SYS-1038-SEI20KSNKBN    TO  SPA-SEI20KSNKBN
           END-IF
           IF      SPA-KAGRPFLG        =   SPACE
               MOVE    "0"                 TO  SPA-KAGRPFLG
           END-IF
           IF      SPA-DOSRYKBNFLG     =   SPACE
               MOVE    "00"                TO  SPA-DOSRYKBNFLG
           END-IF
           IF      SPA-WKSRYHYOFLG     =   SPACE
               MOVE    "0"                 TO  SPA-WKSRYHYOFLG
           END-IF
           IF      SPA-TEISEIFLG       =   SPACE
               MOVE    "0"                 TO  SPA-TEISEIFLG
           END-IF
           IF      SPA-CHG310FLG       =   SPACE
               MOVE    "0"                 TO  SPA-CHG310FLG
           END-IF
           IF      SPA-AKUSEIHYOFLG    =   SPACE
               MOVE    "0"                 TO  SPA-AKUSEIHYOFLG
           END-IF
      *    投薬診療区分変換選択
           IF      SPA-CHGSRYSYU-FLG   =   SPACE
               MOVE    "0"                 TO  SPA-CHGSRYSYU-FLG
           END-IF
      *    残量廃棄区分
           IF      SPA-HAIKIKBN-FLG    =   SPACE
               MOVE    "1"                 TO  SPA-HAIKIKBN-FLG
           END-IF
      *    Ｈ２２年４月から反映
           IF      SPA-SRYYMD          <   "20100401"
               MOVE    "1"                 TO  SPA-HAIKIKBN-FLG
           END-IF
      *    精神療法２０未満加算区分
           IF     (SPA-SEI20KSNKBN     =   SPACE )
      *       Ｈ２２年４月から反映
              OR  (SPA-SRYYMD          <   "20100401")
               MOVE    "0"                 TO  SPA-SEI20KSNKBN
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1002-SYSKANRI-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ展開の診察料変換設定処理
      *****************************************************************
       10011-CLAIM-HEN-SEC            SECTION.
      *
      *    ＣＬＡＩＭ展開の診察料変換
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-9000-REC
               INITIALIZE                  SYS-9000-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
      *        外来診察料展開
               MOVE   SYS-9000-SINSATU-FLG TO  SPA-SINSATU-FLG
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-SINSATU-FLG     =   SPACE
               MOVE    "0"             TO  SPA-SINSATU-FLG
           END-IF
           .
       10011-CLAIM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準設定処理
      *****************************************************************
       1009-SISETU-KIJUN-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1006-REC
           INITIALIZE                      SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索（施設基準）
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1006-REC
               INITIALIZE                  SYS-1006-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
               IF      SYS-1006-KBNCD      =   "1"
      *            小児科外来診療科
                   IF      SYS-1006-SSTKIJUN (121) =   1
                       MOVE    1               TO  SPA-SYONIKA
                   ELSE
                       MOVE    0               TO  SPA-SYONIKA
                   END-IF
      *            老人外来総合診察
                   IF      SYS-1006-SSTKIJUN (128) =   1
                       MOVE    1               TO  SPA-ROUMANSEI
                   ELSE
                       MOVE    0               TO  SPA-ROUMANSEI
                   END-IF
      *            時間外特例医療機関
                   IF      SYS-1006-SSTKIJUN (011) =   1
                       MOVE    1               TO  SPA-JIKANTOKU
                   END-IF
               END-IF
               IF      SYS-1006-KBNCD      =   "2"
      *            電子化加算
                   IF      SYS-1006-SSTKIJUN (100) =   1
                       MOVE    1               TO  SPA-DENSIKA
                   ELSE
                       MOVE    0               TO  SPA-DENSIKA
                   END-IF
      *            夜間・早朝加算
                   IF      SYS-1006-SSTKIJUN (172) =   1
                       MOVE    1               TO  SPA-YAKANKBN
                   ELSE
                       MOVE    0               TO  SPA-YAKANKBN
                   END-IF
      *Ｈ２２年４月改正
      *            地域医療貢献加算(754 - 500)
                   IF      SYS-1006-SSTKIJUN (254) =   1
                       MOVE    1               TO  SPA-CHIIKKBN
                   ELSE
                       MOVE    0               TO  SPA-CHIIKKBN
                   END-IF
      *            明細書発行体制等加算(755 - 500 )
                   IF      SYS-1006-SSTKIJUN (255) =   1
                       MOVE    1               TO  SPA-MEISAKBN
                   ELSE
                       MOVE    0               TO  SPA-MEISAKBN
                   END-IF
               END-IF
      *H24.4改正
               IF      SYS-1006-KBNCD      =   "7"
      *            時間外対応加算
                   IF      SPA-SRYYMD      >=  "20120401"
                       IF      SPA-CHIIKKBN    =   1
                           MOVE    2               TO  SPA-CHIIKKBN
                       END-IF
                       EVALUATE    TRUE
      *                    時間外対応加算１(3001)
                           WHEN    SYS-1006-SSTKIJUN (001) =   1
                               MOVE    1               TO  SPA-CHIIKKBN
      *                    時間外対応加算３(3002)
                           WHEN    SYS-1006-SSTKIJUN (002) =   1
                               IF      SPA-CHIIKKBN    =   ZERO
                                   MOVE    3           TO  SPA-CHIIKKBN
                               END-IF
                       END-EVALUATE
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    時間外特例医療機関の施設基準はすべてありとする
           MOVE    1                   TO  SPA-JIKANTOKU
      *
      *    後発医薬品への変更可署名
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1030-REC
               INITIALIZE                  SYS-1030-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1030-REC
               MOVE    SYS-1030-KOUHATUKA  TO  SPA-KOUHATUKA
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-KOUHATUKA       =   SPACE
               MOVE    "0"                 TO  SPA-KOUHATUKA
          END-IF
           .
       1009-SISETU-KIJUN-EXT.
      *
      *****************************************************************
      *    包括診療行為算定処理
      *****************************************************************
       10010-HKTSANTEI-SEC            SECTION.
      *
      *    包括診療行為算定
           MOVE    SPACE               TO  SYS-1014-REC
           INITIALIZE                  SYS-1014-REC
           MOVE    "1014"              TO  SYS-1014-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1014-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1014-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1014-HOSPNUM
           MOVE    SYS-1014-REC        TO  MCPDATA-REC
      *
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
           MOVE    ZERO                TO  SPA-HKTSANTEI-FLG
           MOVE    ZERO                TO  SPA-HKTSANTEI-202
           MOVE    ZERO                TO  SPA-HKTSANTEI-203
           MOVE    ZERO                TO  SPA-HKTSANTEI-204
           MOVE    ZERO                TO  SPA-HKTSANTEI-205
           MOVE    ZERO                TO  SPA-HKTSANTEI-206
           MOVE    ZERO                TO  SPA-HKTNYUIN-FLG
           PERFORM UNTIL  (FLG-SYSKANRI    =   1  )
               MOVE    MCPDATA-REC         TO  SYS-1014-REC
               IF      SYS-1014-SANTEI-HOU     =   "1"
                                               OR  "2"
      *            入院料の包括
                   IF      SYS-1014-KBNCD      >=  "501"
                       MOVE    1               TO  SPA-HKTNYUIN-FLG
                   ELSE
                       MOVE    1               TO  SPA-HKTSANTEI-FLG
      *                小児科外来診察料
                       IF      SYS-1014-KBNCD      =   "202"
                           MOVE    1               TO  SPA-HKTSANTEI-202
                       END-IF
      *                生活習慣病管理料
                       IF      SYS-1014-KBNCD      =   "203"
                           MOVE    1               TO  SPA-HKTSANTEI-203
                       END-IF
      *                在宅時医学総合管理料
                       IF      SYS-1014-KBNCD      =   "204"
                           MOVE    1               TO  SPA-HKTSANTEI-204
                       END-IF
      *                特定施設入居時等医学管理料
                       IF      SYS-1014-KBNCD      =   "205"
                           MOVE    1               TO  SPA-HKTSANTEI-205
                       END-IF
      *                在宅末期医療総合診察料
                       IF      SYS-1014-KBNCD      =   "206"
                           MOVE    1               TO  SPA-HKTSANTEI-206
                       END-IF
                   END-IF
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10010-HKTSANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報より発行フラグ再設定処理
      *****************************************************************
       1001-PRTFLG-HEN-SEC            SECTION.
      *
      *    当日の職員情報より業務識別の判定
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING  SPA-OPID            DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL   FLG-SYSKANRI        =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-USERID     =   SPA-OPID
      *            請求書発行フラグ
                   IF      SYS-1010-SKYPRTFLG  NOT =   SPACE
                       MOVE    SYS-1010-SKYPRTFLG  TO  SPA-SKYPRTFLG-SYS
                   END-IF
      *            院外処方せん発行フラグ
                   IF      SYS-1010-SYOHOPRTFLG  NOT =   SPACE
                       MOVE    SYS-1010-SYOHOPRTFLG  TO  SPA-SYOHOPRTFLG
                   END-IF
      *            薬剤情報発行フラグ
                   IF      SYS-1010-YAKUJYOPRTFLG  NOT =   SPACE
                       MOVE    SYS-1010-YAKUJYOPRTFLG
                                                   TO  SPA-YAKJYOPRTFLG
                   END-IF
      *            診療費明細書発行
                   IF      SYS-1010-MEIPRTFLG  NOT =   SPACE
                       MOVE    SYS-1010-MEIPRTFLG
                                                   TO  SPA-MEIPRTFLG
                   END-IF
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-PRTFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正ワーク診療行為作成処理
      *****************************************************************
       200-TEISEISYORI-SEC              SECTION.
      *
      *    システム管理検索（医療機関情報）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        病院種別
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               IF      SPA-BEDKBN          =   ZERO
                   CONTINUE
               ELSE
      *        病床数
                   MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
      *        病床数（一般）
                   IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                          (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                       MOVE    ZERO                TO  SPA-BEDSUIPN
                   ELSE
                       MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
                   END-IF
               END-IF
      *        消費税端数設定
               MOVE    SYS-1001-TAX-HASU-KBN   TO  SPA-TAX-HASU-KBN
           ELSE
               MOVE    SPACE                   TO  SPA-TAX-HASU-KBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科
           INITIALIZE                  SPA-GMN-SRYKA-AREA
      *
           MOVE    SPACE               TO  SYS-KEY
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKAMEIL(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
      *    施設基準検索
           PERFORM 1009-SISETU-KIJUN-SEC
      *    ステム管理共通（1007,1038)
           PERFORM 1002-SYSKANRI-KYOTU-SEC
      *    システム管理訂正分(1008）
           PERFORM 2001-SYSKANRI-SAISET-SEC
      *    包括診療行為算定
           PERFORM 10010-HKTSANTEI-SEC
      *
          .
       200-TEISEISYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日のその他システム管理編集
      *****************************************************************
       2001-SYSKANRI-SAISET-SEC               SECTION.
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
      *
      *        外来管理加算チェック
               MOVE    SYS-1008-GAIKANRIKBN-CHK
                                           TO  SPA-GAIKANRIKBN-CHK
      *        初期は、チェックありとする
               IF      SYS-1008-GAIKANRIKBN-CHK    =   SPACE
                   MOVE    "1"             TO  SPA-GAIKANRIKBN-CHK
               END-IF
      *        検体検査加算自動発生
               IF      SYS-1008-KENSAAUTO-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-KENSAAUTO-FLG
               ELSE
                   MOVE    SYS-1008-KENSAAUTO-FLG
                                           TO  SPA-KENSAAUTO-FLG
               END-IF
      *        入院調剤技術基本料チェック
               IF      SYS-1008-CHOKICHK-FLG   =   SPACE
                   MOVE    ZERO            TO  SPA-CHOKICHK-FLG
               ELSE
                   MOVE    SYS-1008-CHOKICHK-FLG
                                           TO  SPA-CHOKICHK-FLG
               END-IF
      *        時間外加算特例乳幼児
               IF      SYS-1008-JIKANTOKU-NYU-FLG  =   SPACE
                   MOVE    ZERO            TO  SPA-JIKANTOKU-NYU
               ELSE
                   MOVE    SYS-1008-JIKANTOKU-NYU-FLG
                                           TO  SPA-JIKANTOKU-NYU
               END-IF
      *        画像診断管理加算１（写真診断）
               IF      SYS-1008-GAZOUKSN1-FLG  =   SPACE
                   MOVE    "1"             TO  SPA-GAZOUKSN1
               ELSE
                   MOVE    SYS-1008-GAZOUKSN1-FLG
                                           TO  SPA-GAZOUKSN1
               END-IF
      *        薬剤情報提供料チェック
               IF      SYS-1008-YAKJYOCHK-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-YAKJYOCHK-FLG
               ELSE
                   MOVE    SYS-1008-YAKJYOCHK-FLG
                                           TO  SPA-YAKJYOCHK-FLG
               END-IF
      *        訂正時の自動発生
               IF      SYS-1008-TEISEIAUTO-FLG   =   SPACE
                   MOVE    "1"             TO  SPA-TEISEI-FLG
               ELSE
                   MOVE    SYS-1008-TEISEIAUTO-FLG
                                           TO  SPA-TEISEI-FLG
               END-IF
      *        訂正時の自動発生（特定疾患処方管理）
               IF      SYS-1008-TEISEITOKU-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-TEISEITOKU-FLG
               ELSE
                   MOVE    SYS-1008-TEISEITOKU-FLG
                                           TO  SPA-TEISEITOKU-FLG
               END-IF
      *        平成１７年４月以降有効とする
               IF      SPA-SRYYMD          <   "20050401"
                   MOVE    "1"             TO  SPA-YAKJYOCHK-FLG
                   MOVE    "1"             TO  SPA-TEISEI-FLG
                   MOVE    "0"             TO  SPA-TEISEITOKU-FLG
               END-IF
      *        手帳加算確認チェック
               IF      SYS-1008-TECHOKBN-FLG   =   SPACE
                   MOVE    "0"             TO  SPA-TECHOKBN-FLG
               ELSE
                   MOVE    SYS-1008-TECHOKBN-FLG
                                           TO  SPA-TECHOKBN-FLG
               END-IF
           ELSE
               MOVE    "1"                 TO  SPA-GAIKANRIKBN-CHK
      *        検体検査加算自動発生
               MOVE    "0"                 TO  SPA-KENSAAUTO-FLG
               MOVE    "0"                 TO  SPA-TECHOKBN-FLG
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-SYSKANRI-SAISET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
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
      *
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
