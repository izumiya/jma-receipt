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
       PROGRAM-ID.             ORCSCSETHEN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : セットマスタ展開サブ
      *                     （Ｋ０２，Ｋ０２Ｎ，Ｋ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-SETSYORI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
           03  IDX-IP                  PIC 9(04).
           03  IDX-YAK                 PIC 9(04).
           03  IDX-YAKUSOKU            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYCD           PIC X(09).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
           03  WRK-INPUTCD         PIC X(10).
      *
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *    エラーコード
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRCD1          PIC X(04).
           03  WRK-ERRCD2          PIC X(04).
      *    約束セット位置
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *画面最大行
       01  WRK-GYO-MAX                 PIC 9(04)   VALUE   200.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
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
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
           COPY    "MCPAREA".
      *
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *
           COPY    "CPORCSCSETHEN.INC".
      *    画面ＳＰＡ領域
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
           ORCSCSETHENAREA
           SPA-AREA
           SPA-K02
           SPA-SCR-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
      *
           MOVE    ORCSCSETHEN-SRYCD   TO  WRK-SRYCD
      *
           EVALUATE    ORCSCSETHEN-SRYCD(1:1)
      *        セット展開処理
               WHEN    "P"
                   PERFORM 100-PSET-TENKAI-SEC
      *        約束処方セット展開処理
               WHEN    "S"
                   PERFORM 200-SSET-TENKAI-SEC
           END-EVALUATE
      * 
           .
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    セット展開処理
      *****************************************************************
       100-PSET-TENKAI-SEC          SECTION.
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           INITIALIZE                  WRK-IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           MOVE    ZERO                TO  IDX-IP
           MOVE    WRK-SRYCD(1:6)      TO  WRK-SETCD
      *
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPID          TO  ISET-HOSPID
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 910-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRCD1
           MOVE    SPACE               TO  WRK-ERRCD2
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE ) OR
                             (SPA-GMN-IDX      >   WRK-GYO-MAX )
      *        セット展開
               MOVE    1               TO  FLG-SETSYORI
               MOVE    ZERO            TO  FLG-ERR
      *
               ADD     1               TO  IDX-IP
      *        編集
               MOVE    ZERO            TO  FLG-SYORI
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
               EVALUATE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                   WHEN    "S"
      *                セット内に約束セットがある時
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       PERFORM 1001-INSET-HEN-SEC
                       PERFORM 1003-YAKUSOKU-SYORI-SEC
                   WHEN    "."
      *                回数入力で剤終了とする
                       IF       SPA-GMN-IDX        >   1
                           MOVE    "1"             TO  SPA-COM-ZAIEND
                                                      (SPA-GMN-IDX - 1)
                       END-IF
      *                診療種別区分名称チェック
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
                       PERFORM 510-SRYKBN-MEI-SEC
                   WHEN    OTHER
      *                診療コード編集
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       PERFORM 1001-INSET-HEN-SEC
                       PERFORM 1001-SRYCD-SYORI-SEC
               END-EVALUATE
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
      *            回数入力で剤終了とする
                   MOVE    "1"             TO  SPA-COM-ZAIEND
                                                      (SPA-GMN-IDX)
               END-IF
      *        エラーがない時、チェック済みとする
               IF     (FLG-ERR             =   ZERO )  AND
                      (SPA-ERRCD           =   SPACE)
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                           TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 910-INPUTSET-READ-SEC
      *
               IF      (FLG-INPUTSET     =   ZERO  )  AND
                       (SPA-ERRCD        =   SPACE )
      *
      *            行を挿入して追加する
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *   エラーの為展開できない
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    100-PSET-TENKAI-EXT
           END-IF
           IF      SPA-GMN-IDX         >   WRK-GYO-MAX
               MOVE    WRK-GYO-MAX         TO  SPA-GMN-IDX
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0050"              TO  SPA-ERRCD
               GO      TO    100-PSET-TENKAI-EXT
           END-IF
      *    最終行を回数入力とする
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
               MOVE    "1"             TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *        セットの時前に編集
               IF      WRK-ERRCD       =   SPACE
                   MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
               ELSE
                   MOVE    WRK-ERRCD   TO  SPA-ERRCD
                   MOVE    "1"         TO  SPA-COM-CUR (WRK-GMN-IDX)
               END-IF
           END-IF
      *
      *    約束セットがあった場合、編集する
           MOVE    ZERO                TO  IDX-IP
           PERFORM VARYING     IDX-YAK FROM    1   BY  1
                   UNTIL      (IDX-YAK     >   IDX-YAKUSOKU) OR
                              (SPA-ERRCD   NOT =   SPACE )
               COMPUTE SPA-GMN-IDX         =   WRK-IDX-YAK (IDX-YAK)
                                           +   IDX-IP
               MOVE    ZERO                TO  IDX-IP
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                               TO  WRK-SRYCD
      *            約束処方セット展開処理
                   PERFORM 200-SSET-TENKAI-SEC
               END-IF
           END-PERFORM
      *
      *    数量ゼロエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD2      NOT =   SPACE
                   MOVE    WRK-ERRCD2          TO  SPA-ERRCD
               END-IF
           END-IF
      *    マスタなしエラー
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       100-PSET-TENKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード編集処理
      *****************************************************************
       1001-INSET-HEN-SEC          SECTION.
      *
           IF      ISET-SURYO1     =   ZERO
      *        数量ゼロフラグ
               IF     (SPA-SURYOZERO-FLG   =   ZERO ) OR
                      (FLG-SYORI           =   1    ) OR
                      (ISET-INPUTCD        =   "799990070"  OR
                                               "770020070" )  OR
                      (ISET-INPUTCD(1:1)
                                       NOT =   "6"   AND
                                               "7" )
                   MOVE    1           TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    ZERO        TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                   MOVE    "1"         TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                   MOVE    "A001"      TO  WRK-ERRCD2
                   MOVE    1           TO  FLG-ERR
               END-IF
           ELSE
               MOVE    ISET-SURYO1     TO  SPA-COM-SURYO (SPA-GMN-IDX)
           END-IF
           MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
           IF      ISET-SURYO2     =   ZERO
               MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           ELSE
               MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
           END-IF
           IF      ISET-KAISU      =   ZERO
               MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
           END-IF
           MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                   TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
           IF      ISET-INPUTCD        =   "810000001"
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                      (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                      (SPA-GMN-IDX)
           ELSE
               MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
               IF      ISET-INPUTCD(1:2)   =   "83" 
                   MOVE    "1"         TO  SPA-COM-MEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
      *
           .
       1001-INSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード展開処理
      *****************************************************************
       1001-SRYCD-SYORI-SEC          SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    ORCSCSETHEN-PG      TO  LNK-SC92-PG
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
      *    当月入院有無
           MOVE    SPA-K02N-NYUINFLG   TO  LNK-SC92-NYUINFLG
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *    点数マスタなしのエラーは表示する
           IF      SPA-ERRCD           =   "0001"
                IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                    MOVE    "S001"          TO  WRK-ERRCD
                END-IF
                MOVE    1               TO  FLG-ERR
                MOVE    SPA-ERRCD       TO  WRK-ERRCD1
                MOVE    SPACE           TO  SPA-ERRCD
                MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *    警告の時、表示する
           IF     (LNK-SC92-ERRCD3  (1:1)   =   "K"  )  OR
                  (LNK-SC92-ERRCD6  (1:1)   =   "K"  )
               MOVE    SPACE           TO  LNK-SC92-ERRAREA
               MOVE    SPACE           TO  SPA-COM-KZMFLG (SPA-GMN-IDX)
           END-IF
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うので
      *    算定回数のチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (SPA-GMN-IDX)
                                               =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(SPA-GMN-IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (SPA-GMN-IDX)
                                               =   1      )
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
           END-IF
      *
      *    エラー時、追加しない
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              SPA-GMN-TBLREC  (SPA-GMN-IDX)
               INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
               IF      WRK-GMN-IDX         =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                           -   1
               END-IF
           ELSE
      *        保険外金額判定
               PERFORM 4101191-JIHI-HEN-SEC
      *        入力コードの画面用再編集
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           .
       1001-SRYCD-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
      *    自賠責の時、対象外
           IF     (SPA-HKNKBN          =   1    )  AND
                  (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (SPA-COM-SRYKBN (SPA-GMN-IDX)
                                       =   "80"  )  AND
                  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                       =   "09591"  OR
                                           "09592"  OR
                                           "09593"  )
               GO     TO      4101191-JIHI-HEN-EXT
           END-IF
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)  =   "095"  OR
                                                          "096"
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-JIHIMONEY
                                                         (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
                                                         (SPA-GMN-IDX)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
               END-IF
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-JIHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    ORCSCSETHEN-PG      TO  LNK-SC94-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    セット内の約束セット展開処理
      *****************************************************************
       1003-YAKUSOKU-SYORI-SEC          SECTION.
      *
      *    入力コードの画面用再編集
           PERFORM 3101-INPUTCD-HEN-SEC
      *    約束名称編集
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-INPUTCD
           PERFORM 41011921-INPSET-MEI-SEC
           MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                      (SPA-GMN-IDX)
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      1003-YAKUSOKU-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-YAKUSOKU
           ADD     1                   TO  IDX-YAKUSOKU
           IF      IDX-YAKUSOKU        >   20
                MOVE    "0007"         TO  SPA-ERRCD
           ELSE
               MOVE    SPA-GMN-IDX     TO  WRK-IDX-YAK (IDX-YAKUSOKU)
           END-IF
           .
       1003-YAKUSOKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束セット展開処理
      *****************************************************************
       200-SSET-TENKAI-SEC     SECTION.
      *
      *    約束の時、１行前に診療種別があること
           IF     (SPA-GMN-IDX         =   1   )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1) NOT =   "." )
               MOVE    "0048"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      200-SSET-TENKAI-EXT
           END-IF
      *
      *    約束名称編集
           MOVE    WRK-SRYCD(1:6)      TO  WRK-INPUTCD
           PERFORM 41011921-INPSET-MEI-SEC
           MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                      (SPA-GMN-IDX)
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      200-SSET-TENKAI-EXT
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
           MOVE    WRK-SRYCD(1:6)      TO  WRK-SETCD
      *    入力セットマスタ検索
           MOVE    SPACE               TO  INPUTSET-REC
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPID          TO  ISET-HOSPID
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
               PERFORM 910-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )
      *
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
      *
                   ADD     1               TO  IDX-IP
      *
      *            編集
                   MOVE    1               TO  FLG-SYORI
                   MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                           TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                   PERFORM 1001-INSET-HEN-SEC
      *
      *            画面表示なし
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
      *
      *            約束処方の、数量を計算する
                   COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                           =   SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                                           *   SPA-COM-SURYO
                                                         (WRK-GMN-IDX)
                   MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                               TO  SPA-COM-SURYO2
                                                         (SPA-GMN-IDX)
      *            回数変更
                   MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                   MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU2
                                                         (SPA-GMN-IDX)
      *
      *            点数マスタ編集・基本チェック
                   PERFORM 1001-SRYCD-SYORI-SEC
      *
                   MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
                   PERFORM 910-INPUTSET-READ-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "INPUTSET-KEY2"     TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    200-SSET-TENKAI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    最終行を回数入力とする
           MOVE    "1"                 TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *    セットの時前に編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
           ELSE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (WRK-GMN-IDX)
           END-IF
      *
           .
       200-SSET-TENKAI-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPID          TO  ICD-HOSPID
           MOVE    "6"                 TO  ICD-CDSYU
      *
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       41011921-INPSET-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   WRK-GYO-MAX
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           IF      SPA-GMN-IDX         >=  WRK-GYO-MAX
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GYO-MAX
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-GMN-TBL (IDY) 
                                           TO  SPA-GMN-TBL(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41014-GYOINSN-EXT.
           EXIT.
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       910-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       910-INPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    "INPUTCD-KEY"       TO  WRK-PATH
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  ORC-DBPATH
               PERFORM 9301-INPUTCD-NEXT-SEC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    WRK-PATH            TO  ORC-DBPATH
           CALL    "ORCMCPSUB"          USING
                                        MCPAREA
                                        ORCMCPAREA
                                        MCPDATA-REC
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       9301-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCMCPSUB"         USING
                                       MCPAREA
                                       ORCMCPAREA
                                       MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       9301-INPUTCD-NEXT-EXT.
           EXIT.
      *
