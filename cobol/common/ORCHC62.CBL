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
       PROGRAM-ID.             ORCHC62.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳印刷
      *  コンポーネント名  : お薬手帳印刷（ＨＣ６２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/05    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    帳票領域
           COPY    "HCM62.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
     *
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-ZAI             PIC 9(03).
           03  CNT-SRY             PIC 9(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(11).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE     PIC X(54).
               05  WRK-HENNAIYO    PIC X(26).
               05  WRK-YKZNAME     PIC X(100).
               05  WRK-YOUHOU      PIC X(100).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
           03  WRK-HENSYUDATA      PIC X(100).
      *
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRY-TBL         OCCURS  50.
               05  WRK-SRYSURYO    PIC 9(05)V9(05).
               05  WRK-SRYKAISU    PIC 9(03).
               05  WRK-INPUTCOMENT PIC X(80).
      *
      *    印刷編集領域
       01  WRK-LINE-AREA.
           03  WRK-LINE-1.
               05  WRK-PTNAME      PIC X(54).
           03  WRK-LINE-2.
               05  FILLER          PIC X(06)   VALUE   "処方日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-SRYYMDG     PIC X(04).
               05  WRK-SRYYMDY     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "年".
               05  WRK-SRYYMDM     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "月".
               05  WRK-SRYYMDD     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  FILLER          PIC X(08)   VALUE   "保険医名".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-DRNAME      PIC X(18).
           03  WRK-LINE-3.
               05  WRK-HOSPNAME    PIC X(54).
           03  WRK-LINE-4.
               05  WRK-HOSPTEL     PIC X(15).
           03  WRK-LINE-5.
               05  FILLER          PIC X(54)   VALUE
               "＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHC62.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ORCHC62AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票ＨＥＡＤ情報編集処理
           PERFORM 210-HEAD-HENSYU-SEC
      *
      *    データがなくなるまで
           MOVE    ZERO            TO  CNT-ZAI
           PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                   UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *        帳票ＢＯＤＹ情報編集処理
               PERFORM 220-BODY-HENSYU-SEC
           END-PERFORM
      *
      *    データがなければ処理抜ける
           IF      CNT-ZAI         =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    帳票ＦＯＯＴ情報編集処理
           PERFORM 430-FOOT-SET-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤ情報編集処理
      *****************************************************************
       210-HEAD-HENSYU-SEC         SECTION.
      *
      *    診療日
           MOVE    ORCHC62-SRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  WRK-SRYYMDG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-SRYYMDY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-SRYYMDM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-SRYYMDD
      *
      *    患者氏名
           MOVE    SPACE           TO  WRK-PTNAME
           IF      SPA-NAME        NOT =   SPACE
               STRING  SPA-NAME        DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           ELSE
               STRING  SPA-KANANAME    DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           END-IF
           INSPECT WRK-PTNAME          REPLACING  ALL "  "  BY  "　"
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
           .
       210-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
      *    コメントは対象外
      *     IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
      *         GO  TO  220-BODY-HENSYU-EXT
      *     END-IF
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
      *
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *
           MOVE    ZERO            TO  CNT-SRY
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *
           MOVE    WRK-LINE-5      TO  HCM62-SEN(CNT-LINE)
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *     IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "001" )
      *         GO  TO  230-NAIYO-HENSYU-EXT
      *     END-IF
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHC62-SRYKAISU(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                   TO  WRK-INPUTCOMENT(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *
      *    １件目だけ番号表示
           IF      CNT-SRY         =   1
               MOVE    CNT-ZAI         TO  WRK-Z9
               MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
               MOVE    ")"             TO  WRK-HENLINE(3:1)
           END-IF
      *
           IF      TNS-SRYCD(1:3)  =   "001"
      *        用法編集
               PERFORM 240-YOUHOU-HENSYU-SEC
           ELSE
      *        名称編集
               PERFORM 240-YKZNAME-HENSYU-SEC
      *        数量編集
               PERFORM 240-SURYOU-HENSYU-SEC
           END-IF
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       240-YOUHOU-HENSYU-SEC       SECTION.
      *
           STRING  "【"            DELIMITED   BY  SIZE
                   TNS-NAME        DELIMITED   BY  SPACE
                   "】"            DELIMITED   BY  SIZE
                   INTO            WRK-YOUHOU
           END-STRING
      *
           MOVE    WRK-YOUHOU      TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       240-YKZNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKZNAME
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
               MOVE    TNS-NAME            TO  WRK-YKZNAME
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
                   MOVE    TNS-NAME        TO  WRK-YKZNAME
               ELSE
                   MOVE    WRK-INPUTCOMENT(CNT-SRY)
                                           TO  WRK-YKZNAME
               END-IF
           END-IF
      *
           MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YKZNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       240-SURYOU-HENSYU-SEC       SECTION.
      *
      *    薬剤、その他材料、用法以外は処理抜ける
           IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
                 ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
                 ( WRK-SRYCD(1:3)  NOT =   "001" )
               GO  TO  240-SURYOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  WRK-SURYOU
           END-IF
      *
           STRING  " "             DELIMITED  BY  SIZE
                   WRK-SURYOU      DELIMITED  BY  SPACE
                   " "             DELIMITED  BY  SIZE
                   TNS-TANINAME    DELIMITED  BY  SPACE
                   INTO            WRK-HENNAIYO
           END-STRING
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-SURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       240-KAISUU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF    ( WRK-SRYKBN      =   "21" )  AND
                 ( FLG-SOURYO      =   1    )
               MOVE    "23"            TO  WRK-SRYKBN
           END-IF
      *
      *    内服か頓服でなければ処理抜ける
           IF    ( WRK-SRYKBN  NOT =   "21" )  AND
                 ( WRK-SRYKBN  NOT =   "22" )
               GO  TO  240-KAISUU-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-ZAIKAIKEI   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-KAISUU
           END-IF
      *
           EVALUATE    WRK-SRYKBN
      *        内服
               WHEN    "21"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "日分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        頓服
               WHEN    "22"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
           END-EVALUATE
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁チェック　処理
      *****************************************************************
       250-PAGE-CHK-SEC            SECTION.
      *
      *    ページ件数を超えていたら印刷
           IF      CNT-LINE    =   68
               PERFORM 400-PRT-WRITE-SEC
               INITIALIZE                  HCM62
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 410-HEAD-SET-SEC
           ELSE
      *        ページ件数を超えていたら改ページ
               IF      CNT-LINE    =   34
                   PERFORM 410-HEAD-SET-SEC
               END-IF
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           .
       250-PAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-SP
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-SP
               END-IF
               IF      FLG-SP      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   11
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-MHEN-SYORI-SET-SEC      SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    0              TO  IDX-STR1
      *
      *    最大文字を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   23 )
                   OR     (WRK-HENNAIYO(IDX-STR:)    =   SPACE)
               ADD     1               TO  IDX-STR
               ADD     1               TO  IDX-STR1
           END-PERFORM
      *
           COMPUTE IDX-STR     =   54  -   IDX-STR1
      *
      *    文字が重なる場合は１行下に表示する
           IF    ( HCM62-NAIYO2(CNT-LINE)              NOT =   SPACE )
           OR    ( HCM62-NAIYO1(CNT-LINE)(IDX-STR:)    NOT =   SPACE )
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           ELSE
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           END-IF
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    4              TO  IDX-STR1
      *
      *    編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   100 )
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        全角セット
               MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                       TO  WRK-HENLINE(IDX-STR1:2)
               ADD     2               TO  IDX-STR
               ADD     2               TO  IDX-STR1
      *        最大字で改行
               IF         (IDX-STR1        >=  54)
                   OR    ((IDX-STR         <=  100)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    WRK-HENLINE     TO  HCM62-NAIYO1(CNT-LINE)
                   MOVE    SPACE           TO  WRK-HENLINE
                   MOVE    4               TO  IDX-STR1
               END-IF
           END-PERFORM
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(16)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62-NAIYO2(CNT-LINE)
           END-STRING
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-5      TO  HCM62-SEN(CNT-LINE)
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＦＯＯＴセット処理
      *****************************************************************
       430-FOOT-SET-SEC            SECTION.
      *
      *    医療機関名称セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-3      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-4      TO  HCM62-NAIYO1(CNT-LINE)
      *
           .
       430-FOOT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
      *     MOVE    LNK-DAY2-EDTYMD3
      *                             TO  WRK-HENYMDG
      *     INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF    ( TNS-SRYCD(1:1)      =   "6"  )  AND
                 ( FLG-TENSU           =   ZERO )
               PERFORM 900-TNSP-READ-SEC
           ELSE
               INITIALIZE                      TNSP-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TNSP-READ-SEC           SECTION.
      *
           INITIALIZE                  TNSP-REC
           MOVE    WRK-SRYCD       TO  TNSP-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNSP-REC        TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TNSP
                   MOVE    MCPDATA-REC     TO  TNSP-REC
               ELSE
                   MOVE    1               TO  FLG-TNSP
               END-IF
           ELSE
               MOVE    1               TO  FLG-TNSP
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-TNSP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC           SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    ORCHC62-DRCD    TO  SYS-1010-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-NAME   TO  WRK-DRNAME
      *
           .
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    ORCHC62-SRYYMD  TO  ORCSPRTNM-SRYYMD
           MOVE    "2"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1901-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1902-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
