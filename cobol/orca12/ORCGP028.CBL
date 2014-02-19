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
       PROGRAM-ID.             ORCGP028.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者個別設定（Ｐ０２８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/07/22    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  13/11/26  患者個別設定追加対応
      *  04.07.00    NACL-多々納  14/02/XX  患者介護情報追加対応
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
           03  FLG-HKNJAINFCARE    PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-SELNUM          PIC 9(04).
      *
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-MAX             PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    介護保険者号マスタ
       01  HKNJAINFCARE-REC.
           COPY    "CPHKNJAINFCARE.INC".
      *
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
           COPY    "ORCA12SCRAREA.INC".
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
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
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
           MOVE    1                   TO  SPA-GMN28-CUR
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
      *    介護保険情報編集
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
              AND (SPA-GMN28-CRHSELNUM =   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護保険情報編集初期編集処理
      *****************************************************************
       3101-CARAHKN-INIT-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRHKN-MAX
               IF     (SPA-NAI28-TCRHSTRYMD (IDX)  <=  SPA-SYSYMD)
                  AND (SPA-NAI28-TCRHENDYMD (IDX)  >=  SPA-SYSYMD)
                   IF      SPA-GMN28-TCRHKNJANUM (IDX) NOT =   SPACE
                       MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                           TO  SPA-GMN28-CR-HKNJANUM
                       PERFORM 41012-CR-HKNJANUM-CHK-SEC
                       STRING  "保険者番号　"
                               SPA-GMN28-CR-HKNJANUM
                                           DELIMITED  BY  SPACE
                                           INTO   SPA-P028-CAREHKNMEI
                       END-STRING
                   END-IF
                   MOVE    SPACE               TO  SPA-GMN28-CR-HKNJANUM
                   MOVE    SPA-GMN28-CRHKN-MAX TO  IDX
               END-IF
           END-PERFORM
           .
       3101-CARAHKN-INIT-EXT.
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
                  (SPA-GMN28-CUR       =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN28-CUR
               WHEN    01
                   MOVE    "P028_PATIENTID1"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "P028_AGREEMENT"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "P028_MNOTE_QRKBN"   TO  MCP-WIDGET
               WHEN    04
                   MOVE    "P028_MNOTE_CSV"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "P028_JMAMRVER"      TO  MCP-WIDGET
      *
               WHEN    10
                   MOVE    "P028_CRHSELNUM"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "P028_CRHKNJANUM"    TO  MCP-WIDGET
               WHEN    12
                   MOVE    "P028_CRHIHKNJANUM"  TO  MCP-WIDGET
               WHEN    13
                   MOVE    "P028_CHTEKSTYMD"   TO  MCP-WIDGET
               WHEN    14
                   MOVE    "P028_CHTEKEDYMD"   TO  MCP-WIDGET
               WHEN    15
                   MOVE    "BCRH2"              TO  MCP-WIDGET
               WHEN    20
                   MOVE    "P028_CRSELNUM"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "P028_CAREJOTAI"     TO  MCP-WIDGET
               WHEN    22
                   MOVE    "P028_CRNINYMD"      TO  MCP-WIDGET
               WHEN    23
                   MOVE    "P028_CRTEKSTYMD"    TO  MCP-WIDGET
               WHEN    24
                   MOVE    "P028_CRTEKEDYMD"    TO  MCP-WIDGET
               WHEN    25
                   MOVE    "BCRN2"              TO  MCP-WIDGET
      *
               WHEN    90
                   MOVE    "B08"                TO  MCP-WIDGET
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
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "P028_PATIENTID1"
                   MOVE    2           TO  SPA-GMN28-CUR
               WHEN    "P028_AGREEMENT"
                   MOVE    3           TO  SPA-GMN28-CUR
               WHEN    "P028_MNOTE_QRKBN"
                   MOVE    4           TO  SPA-GMN28-CUR
               WHEN    "P028_MNOTE_CSV"
                   MOVE    5           TO  SPA-GMN28-CUR
               WHEN    "P028_JMAMRVER"
      *******      MOVE    90          TO  SPA-GMN28-CUR
                   MOVE    10          TO  SPA-GMN28-CUR
      *
               WHEN    "P028_CRHSELNUM"
                   MOVE    11          TO  SPA-GMN28-CUR
               WHEN    "P028_CRHKNJANUM"
                   MOVE    12          TO  SPA-GMN28-CUR
               WHEN    "P028_CRHIHKNJANUM"
                   MOVE    13          TO  SPA-GMN28-CUR
               WHEN    "P028_CHTEKSTYMD"
                   MOVE    14          TO  SPA-GMN28-CUR
               WHEN    "P028_CHTEKEDYMD"
                   MOVE    15          TO  SPA-GMN28-CUR
                   IF     (SPA-GMN28-CR-HKNJANUM   =   SPACE )
                      AND (SPA-GMN28-CR-HIHKNJANUM =   SPACE )
                      AND (SPA-GMN28-CHTEKSTYMD    =   SPACE )
                      AND (SPA-GMN28-CHTEKEDYMD    =   SPACE )
                       MOVE    20          TO  SPA-GMN28-CUR
                   END-IF
      *
               WHEN    "P028_CRSELNUM"
                   MOVE    21          TO  SPA-GMN28-CUR
               WHEN    "P028_CAREJOTAI"
                   MOVE    22          TO  SPA-GMN28-CUR
               WHEN    "P028_CRNINYMD"
                   MOVE    23          TO  SPA-GMN28-CUR
               WHEN    "P028_CRTEKSTYMD"
                   MOVE    24          TO  SPA-GMN28-CUR
               WHEN    "P028_CRTEKEDYMD"
                   IF     (SPA-GMN28-CAREJOTAI     =   SPACE )
                     AND  (SPA-GMN28-CRNINYMD      =   SPACE )
                     AND  (SPA-GMN28-CRTEKSTYMD    =   SPACE )
                     AND  (SPA-GMN28-CRTEKEDYMD    =   SPACE )
                       MOVE    90          TO  SPA-GMN28-CUR
                   ELSE
                       MOVE    25          TO  SPA-GMN28-CUR
                   END-IF
           END-EVALUATE
      *
           .
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
      *
      *H26.1
      *    介護保険者登録
               WHEN    "CLICKED"       ALSO    "BCRH2"
                   PERFORM 4801-CRHKN-TOUROKU-SEC
      *    介護保険者削除
               WHEN    "CLICKED"       ALSO    "BCRH1"
                   PERFORM 4802-CRHKN-DELETE-SEC
      *    介護認定登録
               WHEN    "CLICKED"       ALSO    "BCRN2"
                   PERFORM 4801-CRNIN-TOUROKU-SEC
      *    介護認定削除
               WHEN    "CLICKED"       ALSO    "BCRN1"
                   PERFORM 4802-CRNIN-DELETE-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN28-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
      *        介護保険者リスト選択
               WHEN    ANY             ALSO    "P028_CAREHKNLST"
                   PERFORM 4101-CAREHKNLST-SYORI-SEC
      *        介護リスト選択
               WHEN    ANY             ALSO    "P028_CARENINLST"
                   PERFORM 4101-CARELST-SYORI-SEC
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
      *    介護保険者リスト選択処理
      *****************************************************************
       4101-CAREHKNLST-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRHKN-MAX
               IF      P02-P028-CRHKNLST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-GMN28-CRHSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN28-CRHSELNUM
           END-IF
      *
           IF      SPA-GMN28-CRHSELNUM NOT =   ZERO
               MOVE    SPA-GMN28-CRHSELNUM TO  P02-P028-CRHSELNUM
               PERFORM 41012-CRHSELNUM-CHK-SEC
               MOVE    11                  TO  SPA-GMN28-CUR
           END-IF
           .
       4101-CAREHKNLST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護リスト選択処理
      *****************************************************************
       4101-CARELST-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN28-CRNIN-MAX
               IF      P02-P028-CARELST-SELECT (IDX)  =   "T"
                   IF      IDX               =   SPA-GMN28-CRSELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN28-CRSELNUM
           END-IF
      *
           IF      SPA-GMN28-CRSELNUM  NOT =   ZERO
               MOVE    SPA-GMN28-CRSELNUM  TO  P02-P028-CRSELNUM
               PERFORM 41012-CRSELNUM-CHK-SEC
               MOVE    21                  TO  SPA-GMN28-CUR
           END-IF
           .
       4101-CARELST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN28-CUR
      *
           .
       400-GMN-INPUT-EXT.
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
      *    登録チェック
           IF      FLG-TOUROKU         =   1
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
           END-IF
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02-P028-MNOTE-QRKBN
                                       TO  SPA-GMN28-MNOTE-QRKBN-G
           MOVE    P02-P028-MNOTE-CSV
                                       TO  SPA-GMN28-MNOTE-CSV-G
      *    地域連携ＩＤ
           MOVE    P02-P028-PATIENTID1 TO  SPA-GMN28-PATIENTID1
      *    情報提供同意有無
           MOVE    P02-P028-AGREEMENT
                                       TO  SPA-GMN28-AGREEMENT-G
      *    日医データフォーマット区分
           MOVE    P02-P028-JMAMRVER
                                       TO  SPA-GMN28-JMAMRVER-G
      *
      *    介護保険情報
           MOVE    P02-P028-CRHSELNUM
                                       TO  SPA-GMN28-CRHSELNUM
           MOVE    P02-P028-CRHKNJANUM
                                       TO  SPA-GMN28-CR-HKNJANUM
           MOVE    P02-P028-CRHIHKNJANUM
                                       TO  SPA-GMN28-CR-HIHKNJANUM
           MOVE    P02-P028-CHTEKSTYMD
                                       TO  SPA-GMN28-CHTEKSTYMD
           MOVE    P02-P028-CHTEKEDYMD
                                       TO  SPA-GMN28-CHTEKEDYMD
      *    介護認定者情報
           MOVE    P02-P028-CRSELNUM
                                       TO  SPA-GMN28-CRSELNUM
           MOVE    P02-P028-CAREJOTAI
                                       TO  SPA-GMN28-CAREJOTAICD-G
           MOVE    P02-P028-CRNINYMD   
                                       TO  SPA-GMN28-CRNINYMD
           MOVE    P02-P028-CRTEKSTYMD
                                       TO  SPA-GMN28-CRTEKSTYMD
           MOVE    P02-P028-CRTEKEDYMD
                                       TO  SPA-GMN28-CRTEKEDYMD
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    地域連携ＩＤ
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-PATIENTID1-CHK-SEC
           END-IF
      *    情報提供同意有無
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 41012-AGREEMENT-CHK-SEC
           END-IF
      *    お薬手帳ＱＲ区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MNOTE-QRKBN-CHK-SEC
           END-IF
      *    お薬手帳ＣＳＶ区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-MNOTE-CSV-CHK-SEC
           END-IF
      *    日医データフォーマット区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-JMAMRVER-CHK-SEC
           END-IF
      *    介護保険情報
      *    選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRHSELNUM-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CR-HKNJANUM-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CR-HIHKNJANUM-CHK-SEC
           END-IF
      *    開始日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CHTEKSTYMD-CHK-SEC
           END-IF
      *    終了日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CHTEKEDYMD-CHK-SEC
           END-IF
      *    介護認定者情報
      *    選択番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRSELNUM-CHK-SEC
           END-IF
      *    要介護状態区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CAREJOTAICD-CHK-SEC
           END-IF
      *    認定日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRNINYMD-CHK-SEC
           END-IF
      *    開始日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRTEKSTYMD-CHK-SEC
           END-IF
      *    終了日付
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-CRTEKEDYMD-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地域連携ＩＤチェック処理
      *****************************************************************
       4101-PATIENTID1-CHK-SEC          SECTION.
      *
           IF      SPA-GMN28-PATIENTID1    NOT =   SPACE
               MOVE    SPA-GMN28-PATIENTID1    TO  WRK-MAE-INPUT
               MOVE    20                      TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-PATIENTID1
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2801"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN28-CUR
               END-IF
           END-IF
      *
           .
       4101-PATIENTID1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-AGREEMENT-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-AGREEMENT =   SPACE
               MOVE    SPACE               TO  SPA-GMN28-AGREEMENT-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-AGREEMENT-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-AGREEMENT
                                       =   SPA-GMN28-AGREEMENT-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-AGREEMENT-LST (IDX)
                                           TO  SPA-GMN28-AGREEMENT-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2802"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-AGREEMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-MNOTE-QRKBN-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-MNOTE-QRKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-MNOTE-QRKBN-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-QRKBN-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-MNOTE-QRKBN
                                       =   SPA-GMN28-MNOTE-QRKBN-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-MNOTE-QRKBN-LST (IDX)
                                           TO  SPA-GMN28-MNOTE-QRKBN-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2803"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-MNOTE-QRKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳ＱＲ区分チェック処理
      *****************************************************************
       41012-MNOTE-CSV-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-MNOTE-CSV   =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-MNOTE-CSV-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-CSV-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-MNOTE-CSV
                                       =   SPA-GMN28-MNOTE-CSV-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-MNOTE-CSV-LST (IDX)
                                           TO  SPA-GMN28-MNOTE-CSV-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2804"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-MNOTE-CSV-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日医データフォーマット区分チェック処理
      *****************************************************************
       41012-JMAMRVER-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-JMAMRVER  =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-JMAMRVER-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-JMAMRVER-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-JMAMRVER
                                       =   SPA-GMN28-JMAMRVER-LST
                                                      (IDX) (1:1)
                       MOVE    SPA-GMN28-JMAMRVER-LST (IDX)
                                           TO  SPA-GMN28-JMAMRVER-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2805"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-JMAMRVER-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    介護選択番号チェック処理
      *****************************************************************
       41012-CRHSELNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRHSELNUM  NOT =   SPA-MAE28-CRHSELNUM
               IF      SPA-GMN28-CRHSELNUM  =   ZERO
                   MOVE    SPA-GMN28-CRHSELNUM  TO  SPA-MAE28-CRHSELNUM
                   INITIALIZE                  SPA-GMN28-CRHKN-ING
      *
                   IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *                  システム日で有効な保険情報を記載する
                         PERFORM 3101-CARAHKN-INIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-GMN28-CRHSELNUM NOT =   ZERO   )
             AND  (SPA-GMN28-CRHSELNUM NOT =   SPA-MAE28-CRHSELNUM)
               IF      SPA-GMN28-CRHSELNUM >   SPA-GMN28-CRHKN-MAX
                   MOVE    "2810"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               ELSE
                   PERFORM 410121-CRHKNINF-HEN-SEC
               END-IF
           END-IF
           .
       41012-CRHSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者介護保険者情報編集処理
      *****************************************************************
       410121-CRHKNINF-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN28-CRHSELNUM TO  SPA-MAE28-CRHSELNUM
           MOVE    SPA-GMN28-CRHSELNUM TO  IDX
      *
           MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                       TO  SPA-GMN28-CR-HKNJANUM
           MOVE    SPA-GMN28-TCRHIHKNJANUM (IDX)
                                       TO  SPA-GMN28-CR-HIHKNJANUM
           MOVE    SPA-GMN28-TCRHSTRYMD (IDX)
                                       TO  SPA-GMN28-CHTEKSTYMD
           MOVE    SPA-GMN28-TCRHENDYMD (IDX)
                                       TO  SPA-GMN28-CHTEKEDYMD
           MOVE    SPA-NAI28-TCRHSTRYMD (IDX)
                                       TO  SPA-NAI28-CHTEKSTYMD
           MOVE    SPA-NAI28-TCRHENDYMD (IDX)
                                       TO  SPA-NAI28-CHTEKEDYMD
      *
           MOVE    SPA-NAI28-TCHKNSKBN (IDX)
                                       TO  SPA-NAI28-CHKNSKBN
      *
           PERFORM 41012-CR-HKNJANUM-CHK-SEC
           .
       410121-CRHKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    介護保険情報チェック処理
      *****************************************************************
       41012-CR-HKNJANUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CR-HKNJANUM   =   SPACE
               GO      TO      41012-CR-HKNJANUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-P028-CAREHKNMEI
           MOVE    SPACE               TO  SPA-P028-CRHKNJANAME
           MOVE    SPACE               TO  SPA-P028-CRHKNJAADRS
           MOVE    SPACE               TO  SPA-P028-CRHKNJAADRS2
           MOVE    SPACE               TO  SPA-P028-CRHKNJATEL
           IF      SPA-GMN28-CR-HKNJANUM   NOT =   SPACE
               MOVE    SPA-GMN28-CR-HKNJANUM   TO  WRK-MAE-INPUT
               MOVE    6                       TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-CR-HKNJANUM
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2811"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN28-CUR
                   GO      TO      41012-CR-HKNJANUM-CHK-EXT
               END-IF
           END-IF
           IF      SPA-GMN28-CR-HKNJANUM   =   SPACE
               GO      TO      41012-CR-HKNJANUM-CHK-EXT
           END-IF
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINFCARE-REC
      **** MOVE    SPA-HOSPNUM           TO  HKNJAC-HKNJANUM
           MOVE    SPA-GMN28-CR-HKNJANUM TO  HKNJAC-HKNJANUM
      *
           MOVE    HKNJAINFCARE-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_care"   TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf_care" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINFCARE-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINFCARE-REC
               MOVE    1               TO  FLG-HKNJAINFCARE
           END-IF
           MOVE    "tbl_hknjainf_care" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険者番号あり
           IF      FLG-HKNJAINFCARE    =   ZERO
               MOVE    HKNJAC-HKNJANAME    TO  SPA-P028-CRHKNJANAME
               MOVE    HKNJAC-ADRS         TO  SPA-P028-CRHKNJAADRS
               MOVE    HKNJAC-BANTI        TO  SPA-P028-CRHKNJAADRS2
               MOVE    HKNJAC-TEL          TO  SPA-P028-CRHKNJATEL
           END-IF
           .
       41012-CR-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護被保険情報チェック処理
      *****************************************************************
       41012-CR-HIHKNJANUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CR-HIHKNJANUM   NOT =   SPACE
               MOVE    SPA-GMN28-CR-HIHKNJANUM TO  WRK-MAE-INPUT
               MOVE    10                      TO  WRK-MAX
               PERFORM 891-HALF-CHK-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN28-CR-HIHKNJANUM
               IF      FLG-ERR         NOT =   ZERO
                   MOVE    "2812"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CR-HIHKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険開始日付チェック処理
      *****************************************************************
       41012-CHTEKSTYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CHTEKSTYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CHTEKSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CHTEKSTYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CHTEKSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CHTEKSTYMD
               ELSE
                   MOVE    "2815"          TO  SPA-ERRCD
                   MOVE    13              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険終了日付チェック処理
      *****************************************************************
       41012-CHTEKEDYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CHTEKEDYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CHTEKEDYMD
           ELSE
      *
               IF      SPA-GMN28-CHTEKEDYMD    =   "9999999"
                   MOVE    ALL "9"         TO  SPA-NAI28-CHTEKEDYMD
                   GO      TO      41012-CHTEKEDYMD-CHK-EXT
               END-IF
      *
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CHTEKEDYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CHTEKEDYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CHTEKEDYMD
               ELSE
                   MOVE    "2816"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN28-CUR
                   GO      TO      41012-CHTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN28-CHTEKSTYMD    NOT =   SPACE
               IF      SPA-GMN28-CHTEKEDYMD    =   SPACE
                   IF     WRK-MCP-WIDGET       =   "P028_CHTEKEDYMD"
                       MOVE    ALL "9"         TO  SPA-NAI28-CHTEKEDYMD
                       MOVE    "9999999"       TO  SPA-GMN28-CHTEKEDYMD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-NAI28-CHTEKSTYMD    NOT =   SPACE )  AND
                  (SPA-NAI28-CHTEKEDYMD    NOT =   SPACE )
      *        開始日＞終了日はエラー
               IF      SPA-NAI28-CHTEKSTYMD    >   SPA-NAI28-CHTEKEDYMD
                   MOVE    "2817"              TO  SPA-ERRCD
                   MOVE    14                  TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CHTEKEDYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4802-CRHKN-DELETE-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN28-CRHSELNUM  =   ZERO
                   MOVE    "2824"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               ELSE
      *            削除処理
                   PERFORM 48021-CRHKN-TBLDEL-SEC
               END-IF
           END-IF
           .
       4802-CRHKN-DELETE-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報削除処理
      ****************************************************************
       48021-CRHKN-TBLDEL-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   SPA-GMN28-CRHKN-MAX)
               IF     (SPA-GMN28-CRHSELNUM =   IDX )
                   MOVE    SPA-GMN28-CR-HKNLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-HKNLST-OCC(IDX)
                   INITIALIZE              SPA-GMN28-CR-HKNLST-OCC
                                                              (IDX + 1)
               ELSE
                   IF      SPA-GMN28-TCHNUM (IDX)  =   ZERO
                       MOVE    SPA-GMN28-CR-HKNLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-HKNLST-OCC(IDX)
                       INITIALIZE          SPA-GMN28-CR-HKNLST-OCC
                                                              (IDX + 1)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-GMN28-CRHKN-MAX
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   10   )
               IF     (SPA-GMN28-TCRHKNJANUM (IDX) =   SPACE)
                AND   (SPA-GMN28-TCRHIHKNJANUM(IDX) =  SPACE)
                   INITIALIZE          SPA-GMN28-CR-HKNLST-OCC (IDX)
               ELSE
                   MOVE    IDX             TO  SPA-GMN28-TCHNUM (IDX)
                   ADD     1               TO  SPA-GMN28-CRHKN-MAX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN28-CRHKN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRHSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRHSELNUM
           MOVE    10                  TO  SPA-GMN28-CUR
      *
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
      *
           .
       48021-CRHKN-TBLDEL-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4801-CRHKN-TOUROKU-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-CRHKN-TOUROKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4802-CRHKN-TOUROKU-SEC
           END-IF
           .
       4801-CRHKN-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録前関連チェック処理
      ****************************************************************
       4801-CRHKN-TOUROKU-CHK-SEC      SECTION.
      *
      *    必須チェック
           IF     (SPA-GMN28-CR-HKNJANUM   =   SPACE )
             AND  (SPA-GMN28-CR-HIHKNJANUM =   SPACE )
             AND  (SPA-ERRCD               =   SPACE )
               MOVE    "2813"          TO  SPA-ERRCD
               MOVE    11              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-NAI28-CHTEKSTYMD =   SPACE)
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2818"          TO  SPA-ERRCD
               MOVE    13              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-NAI28-CHTEKEDYMD =   SPACE)
             AND  (SPA-ERRCD            =   SPACE )
               MOVE    "2819"          TO  SPA-ERRCD
               MOVE    14              TO  SPA-GMN28-CUR
           END-IF
      *
      *    期間重複チェック
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-GMN28-CRHKN-MAX)
                       OR    (SPA-ERRCD    NOT =   SPACE )
               IF     (SPA-GMN28-CRHSELNUM     NOT =   IDY )
                   IF      (SPA-NAI28-TCRHENDYMD (IDY)
                                               <   SPA-NAI28-CHTEKSTYMD)
                     OR    (SPA-NAI28-TCRHSTRYMD (IDY)
                                               >   SPA-NAI28-CHTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  SPA-ERRCD
                       MOVE    13              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN28-CRHSELNUM =   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
      *        新規追加
               IF      SPA-GMN28-CRHKN-MAX >=  10
                   MOVE    "2814"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       4801-CRHKN-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護保険者情報登録処理
      ****************************************************************
       4802-CRHKN-TOUROKU-SEC             SECTION.
      *
           IF      SPA-GMN28-CRHSELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN28-CRHKN-MAX
               MOVE    SPA-GMN28-CRHKN-MAX TO  IDX
           ELSE
               MOVE    SPA-GMN28-CRHSELNUM TO  IDX
               IF      SPA-NAI28-CHKNSKBN  NOT =   SPACE
                   MOVE    "2"             TO  SPA-NAI28-CHKNSKBN
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN28-CR-HKNLST-OCC (IDX)
           MOVE    IDX                 TO  SPA-GMN28-TCHNUM (IDX)
      *
           MOVE    SPA-GMN28-CHTEKSTYMD
                                       TO  SPA-GMN28-TCRHSTRYMD (IDX)
           MOVE    SPA-NAI28-CHTEKSTYMD
                                       TO  SPA-NAI28-TCRHSTRYMD (IDX)
           MOVE    SPA-GMN28-CHTEKEDYMD
                                       TO  SPA-GMN28-TCRHENDYMD (IDX)
           MOVE    SPA-NAI28-CHTEKEDYMD
                                       TO  SPA-NAI28-TCRHENDYMD (IDX)
      *
           MOVE    SPA-GMN28-CR-HKNJANUM
                                       TO  SPA-GMN28-TCRHKNJANUM (IDX)
           MOVE    SPA-GMN28-CR-HIHKNJANUM
                                       TO  SPA-GMN28-TCRHIHKNJANUM (IDX)
      *
           MOVE    SPA-NAI28-CHKNSKBN  TO  SPA-NAI28-TCHKNSKBN (IDX)
      *
           INITIALIZE                  SPA-GMN28-CRHKN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRHSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRHSELNUM
           MOVE    10                  TO  SPA-GMN28-CUR
      *
           IF     (SPA-GMN28-CRHKN-MAX >   ZERO )
      *        システム日で有効な保険情報を記載する
               PERFORM 3101-CARAHKN-INIT-SEC
           END-IF
      *
           .
       4802-CRHKN-TOUROKU-EXT.
           EXIT.
      *
      *
      *
      *****************************************************************
      *    介護選択番号チェック処理
      *****************************************************************
       41012-CRSELNUM-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRSELNUM  NOT =   SPA-MAE28-CRSELNUM
               IF      SPA-GMN28-CRSELNUM  =   ZERO
                   MOVE    SPA-GMN28-CRSELNUM  TO  SPA-MAE28-CRSELNUM
                   INITIALIZE                  SPA-GMN28-CRNIN-ING
               END-IF
           END-IF
      *
           IF     (SPA-GMN28-CRSELNUM  NOT =   ZERO   )
             AND  (SPA-GMN28-CRSELNUM  NOT =   SPA-MAE28-CRSELNUM)
               IF      SPA-GMN28-CRSELNUM  >   SPA-GMN28-CRNIN-MAX
                   MOVE    "2820"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               ELSE
                   PERFORM 410121-CRNINTEI-HEN-SEC
               END-IF
           END-IF
           .
       41012-CRSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者介護認定者編集処理
      *****************************************************************
       410121-CRNINTEI-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN28-CRSELNUM  TO  SPA-MAE28-CRSELNUM
           MOVE    SPA-GMN28-CRSELNUM  TO  IDX
      *
           MOVE    SPA-NAI28-TCAREJOTAI (IDX)
                                       TO  SPA-GMN28-CAREJOTAI
           MOVE    SPA-GMN28-TCRNINYMD (IDX)
                                       TO  SPA-GMN28-CRNINYMD
           MOVE    SPA-GMN28-TCRSTRYMD (IDX)
                                       TO  SPA-GMN28-CRTEKSTYMD
           MOVE    SPA-GMN28-TCRENDYMD (IDX)
                                       TO  SPA-GMN28-CRTEKEDYMD
           MOVE    SPA-NAI28-TCRNINYMD (IDX)
                                       TO  SPA-NAI28-CRNINYMD
           MOVE    SPA-NAI28-TCRSTRYMD (IDX)
                                       TO  SPA-NAI28-CRTEKSTYMD
           MOVE    SPA-NAI28-TCRENDYMD (IDX)
                                       TO  SPA-NAI28-CRTEKEDYMD
           MOVE    SPA-NAI28-TINSKBN (IDX)
                                       TO  SPA-NAI28-INSKBN
      *
           PERFORM 41012-CAREJOTAICD-CHK-SEC
           .
       410121-CRNINTEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    要介護状態区分チェック処理
      *****************************************************************
       41012-CAREJOTAICD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CAREJOTAI =   SPACE
               MOVE    SPACE           TO  SPA-GMN28-CAREJOTAICD-G
           ELSE
      *
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-GMN28-CAREJOTAI-MAX)
                              OR  (FLG-OK  =   1                )
                   IF      SPA-GMN28-CAREJOTAI
                                       =   SPA-GMN28-CAREJOTAI-LST
                                                      (IDX) (1:2)
                       MOVE    SPA-GMN28-CAREJOTAI-LST (IDX)
                                           TO  SPA-GMN28-CAREJOTAICD-G
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "2813"          TO  SPA-ERRCD
                   MOVE    21              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CAREJOTAICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    認定日付チェック処理
      *****************************************************************
       41012-CRNINYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRNINYMD  =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRNINYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRNINYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRNINYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRNINYMD
               ELSE
                   MOVE    "2814"          TO  SPA-ERRCD
                   MOVE    22              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRNINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護開始日付チェック処理
      *****************************************************************
       41012-CRTEKSTYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRTEKSTYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRTEKSTYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRTEKSTYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRTEKSTYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRTEKSTYMD
               ELSE
                   MOVE    "2815"          TO  SPA-ERRCD
                   MOVE    23              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護終了日付チェック処理
      *****************************************************************
       41012-CRTEKEDYMD-CHK-SEC           SECTION.
      *
           IF      SPA-GMN28-CRTEKEDYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI28-CRTEKEDYMD
           ELSE
      *
               IF      SPA-GMN28-CRTEKEDYMD    =   "9999999"
                   MOVE    ALL "9"         TO  SPA-NAI28-CRTEKEDYMD
                   GO      TO      41012-CRTEKEDYMD-CHK-EXT
               END-IF
      *
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN28-CRTEKEDYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"              USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN28-CRTEKEDYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI28-CRTEKEDYMD
               ELSE
                   MOVE    "2816"          TO  SPA-ERRCD
                   MOVE    24              TO  SPA-GMN28-CUR
                   GO      TO      41012-CRTEKEDYMD-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN28-CRTEKSTYMD    NOT =   SPACE
               IF      SPA-GMN28-CRTEKEDYMD    =   SPACE
                   IF     WRK-MCP-WIDGET       =   "P028_CRTEKEDYMD"
                       MOVE    ALL "9"         TO  SPA-NAI28-CRTEKEDYMD
                       MOVE    "9999999"       TO  SPA-GMN28-CRTEKEDYMD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-NAI28-CRTEKSTYMD    NOT =   SPACE )  AND
                  (SPA-NAI28-CRTEKEDYMD    NOT =   SPACE )
      *        開始日＞終了日はエラー
               IF      SPA-NAI28-CRTEKSTYMD    >   SPA-NAI28-CRTEKEDYMD
                   MOVE    "2817"              TO  SPA-ERRCD
                   MOVE    24                  TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       41012-CRTEKEDYMD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定登録処理
      ****************************************************************
       4802-CRNIN-DELETE-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN28-CRSELNUM  =   ZERO
                   MOVE    "2824"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               ELSE
      *            削除処理
                   PERFORM 48021-CRNIN-TBLDEL-SEC
               END-IF
           END-IF
           .
       4802-CRNIN-DELETE-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者削除処理
      ****************************************************************
       48021-CRNIN-TBLDEL-SEC             SECTION.
      *
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   SPA-GMN28-CRNIN-MAX)
               IF     (SPA-GMN28-CRSELNUM  =   IDX )
                   MOVE    SPA-GMN28-CR-NINLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-NINLST-OCC(IDX)
                   INITIALIZE              SPA-GMN28-CR-NINLST-OCC
                                                              (IDX + 1)
               ELSE
                   IF      SPA-GMN28-TCRNUM (IDX)  =   ZERO
                       MOVE    SPA-GMN28-CR-NINLST-OCC (IDX + 1)
                                       TO  SPA-GMN28-CR-NINLST-OCC(IDX)
                       INITIALIZE          SPA-GMN28-CR-NINLST-OCC
                                                              (IDX + 1)
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           MOVE    ZERO                TO  SPA-GMN28-CRNIN-MAX
           PERFORM VARYING    IDX      FROM   1   BY  1
                   UNTIL     (IDX      >   50     )
               IF      SPA-NAI28-TCAREJOTAI(IDX) NOT =   SPACE
                   MOVE    IDX             TO  SPA-GMN28-TCRNUM (IDX)
                   ADD     1               TO  SPA-GMN28-CRNIN-MAX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN28-CRNIN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRSELNUM
           MOVE    20                  TO  SPA-GMN28-CUR
      *
           .
       48021-CRNIN-TBLDEL-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定登録処理
      ****************************************************************
       4801-CRNIN-TOUROKU-SEC            SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-CRNIN-TOUROKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4802-CRNIN-TOUROKU-SEC
           END-IF
           .
       4801-CRNIN-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者登録前関連チェック処理
      ****************************************************************
       4801-CRNIN-TOUROKU-CHK-SEC             SECTION.
      *
      *    必須チェック
           IF     (SPA-GMN28-CAREJOTAI =   SPACE )
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2821"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-GMN28-CRTEKSTYMD =   SPACE)
             AND  (SPA-ERRCD           =   SPACE )
               MOVE    "2818"          TO  SPA-ERRCD
               MOVE    23              TO  SPA-GMN28-CUR
           END-IF
           IF     (SPA-GMN28-CRTEKEDYMD =   SPACE)
             AND  (SPA-ERRCD            =   SPACE )
               MOVE    "2819"          TO  SPA-ERRCD
               MOVE    24              TO  SPA-GMN28-CUR
           END-IF
      *
      *    期間重複チェック
           PERFORM VARYING    IDY      FROM   1   BY  1
                   UNTIL     (IDY      >   SPA-GMN28-CRNIN-MAX)
                       OR    (SPA-ERRCD    NOT =   SPACE )
               IF     (SPA-GMN28-CRSELNUM      NOT =   IDY )
                   IF      (SPA-NAI28-TCRENDYMD (IDY)
                                               <   SPA-NAI28-CRTEKSTYMD)
                     OR    (SPA-NAI28-TCRSTRYMD (IDY)
                                               >   SPA-NAI28-CRTEKEDYMD)
                       CONTINUE
                   ELSE
                       MOVE    "2820"          TO  SPA-ERRCD
                       MOVE    23              TO  SPA-GMN28-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN28-CRSELNUM  =   ZERO  )
             AND  (SPA-ERRCD           =   SPACE )
      *        新規追加
               IF      SPA-GMN28-CRNIN-MAX >=  50
                   MOVE    "2823"          TO  SPA-ERRCD
                   MOVE    20              TO  SPA-GMN28-CUR
               END-IF
           END-IF
           .
       4801-CRNIN-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      **    介護認定者登録処理
      ****************************************************************
       4802-CRNIN-TOUROKU-SEC             SECTION.
      *
           IF      SPA-GMN28-CRSELNUM  =   ZERO
               ADD     1                   TO  SPA-GMN28-CRNIN-MAX
               MOVE    SPA-GMN28-CRNIN-MAX TO  IDX
           ELSE
               MOVE    SPA-GMN28-CRSELNUM  TO  IDX
               IF      SPA-NAI28-INSKBN    NOT =   SPACE
                   MOVE    "2"             TO  SPA-NAI28-INSKBN
               END-IF
           END-IF
      *
           INITIALIZE                  SPA-GMN28-CR-NINLST-OCC (IDX)
           MOVE    IDX                 TO  SPA-GMN28-TCRNUM (IDX)
      *
           MOVE    SPA-GMN28-CRTEKSTYMD
                                       TO  SPA-GMN28-TCRSTRYMD (IDX)
           MOVE    SPA-NAI28-CRTEKSTYMD
                                       TO  SPA-NAI28-TCRSTRYMD (IDX)
           MOVE    SPA-GMN28-CRTEKEDYMD
                                       TO  SPA-GMN28-TCRENDYMD (IDX)
           MOVE    SPA-NAI28-CRTEKEDYMD
                                       TO  SPA-NAI28-TCRENDYMD (IDX)
           MOVE    SPA-GMN28-CRNINYMD  TO  SPA-GMN28-TCRNINYMD (IDX)
           MOVE    SPA-NAI28-CRNINYMD  TO  SPA-NAI28-TCRNINYMD (IDX)
      *
           MOVE    SPA-GMN28-CAREJOTAICD-G
                                       TO  SPA-GMN28-TCAREJOTAI (IDX)
           MOVE    SPA-GMN28-CAREJOTAI TO  SPA-NAI28-TCAREJOTAI (IDX)
      *
           MOVE    SPA-NAI28-INSKBN    TO  SPA-NAI28-TINSKBN (IDX)
      *
           INITIALIZE                  SPA-GMN28-CRNIN-ING
           MOVE    ZERO                TO  SPA-GMN28-CRSELNUM
           MOVE    ZERO                TO  SPA-MAE28-CRSELNUM
           MOVE    20                  TO  SPA-GMN28-CUR
      *
           .
       4802-CRNIN-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      **    登録前関連チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
      *
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       891-HALF-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *        半角
               MOVE    ZERO                TO  FLG-ERR
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       891-HALF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINFCARE-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf_care"    TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINFCARE-REC
               MOVE    ZERO                TO  FLG-HKNJAINFCARE
           ELSE
               INITIALIZE                  HKNJAINFCARE-REC
               MOVE    1                   TO  FLG-HKNJAINFCARE
           END-IF
           .
       900-HKNJAINFCARE-READ-EXT.
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
